#!/usr/bin/env python3.6

import os
from IPython import embed

infinity = 9999999  # you know it baby, if we go this deep we will get recursion errors

# combinators

def OR(*funcs):
    def or_(p):
        for f in funcs:
            success, v, rest = f(p)
            if success:
                return success, v, rest
        return success, v, rest
    return or_

def TIMES(func, min_, max_=None):
    def times(p):
        matches = []
        for i in range(min_):
            success, v, rest = func(p)
            if not success:
                return success, None, p
            else:
                matches.append(v)
                p = rest
        if max_ is not None:
            for i in range(max_ - min_):
                success, v, rest = func(p)
                if not success:
                    return True, matches, p
                else:
                    matches.append(v)
                    p = rest
            success, v, rest = func(p)
            if success:
                return False, None, p
            else:
                success = True
                rest = p

        return success, matches, rest
    return times

def MANY(func):
    return TIMES(func, 0, infinity)

def MANY1(func):
    return TIMES(func, 1, infinity)

def ANDTHEN(func1, func2):
    def andthen(p):
        success, v1, rest = func1(p)
        if success:
            p2 = rest
            success, v2, rest = func2(p2)
            if success:
                return success, [v1, v2], rest
        return success, None, p
    return andthen

def JOINT(*funcs, join=True):
    def joint(p):
        matches = []
        rest = p
        for func in funcs:
            success, v, rest = func(rest)
            if not success:
                return success, None, p
            else:
                if join and hasattr(v, '__iter__'):
                    matches.extend(v)
                else:
                    matches.append(v)
        return success, matches, rest 
    return joint

def comp(p, val):
    lv = len(val)
    if p:
        v = p[:lv]  # warning: this will produce order dependencies when you spec the parser
    else:
        return False, None, p  # we are at the end
    return v == val, v, p[lv:]

def oper(p, func):
    if p:
        v = p[0]
    else:
        return False, None, p  # we are at the end
    return func(v), v, p[1:]

#
# units

def get_quoted_list(filename):
    with open(os.path.expanduser('~/ni/protocols/rkt/units/' + filename), 'rt') as f:
        lines = [_.split(';')[0].strip() for _ in f.readlines()]
    return [line.strip("'").strip('(').rstrip(')').split(' . ') for line in lines if line]

_SIPREFS, _SIEXPS, _SIUNITS = [get_quoted_list(_) for _ in ('si-prefixes-data.rkt', 'si-prefixes-exp-data.rkt', 'si-units-data.rkt')]
#_SIUNITS.append(['Osm', 'osmoles'])
def make_funcs(inpt):
    args = []
    for token, name in sorted(inpt, key=lambda a: -len(a[0])):  # sort to simulate right associativity (ie da recognized even if d a token)
        def fn(p, tok=token):  # late binding stupidity
            return comp(p, tok)
        args.append(fn)
    return args

siprefix = OR(*make_funcs(_SIPREFS))
siunit = OR(*make_funcs(_SIUNITS))
unit_atom = OR(JOINT(siprefix, siunit, join=False), siunit)  # have to use OR cannot use TIMES  FIXME siunit by itself needs to not be followed by another char? so NOT(siunit)  (different than kgm/s example I used before...)
def division(p): return comp(p, '/')
def multiplication(p): return comp(p, '*')
unit_op = OR(division, multiplication)
def unit(p):
    return OR(JOINT(unit_atom, unit_op, unit, join=False), unit_atom)(p)  # beware false order of operations

#siprefix = OR(*[lambda p, t=tok: comp(p, t) for tok, n in SIPREFS])  # that thing about late binding and lambdas..
#siunit = OR(*[lambda p, t=tok: comp(p, t) for tok, n in SIUNITS])  # no real closures, wew lad >_<

# basic tokens
def space(p): return comp(p, ' ')
def plus_or_minus_symbol(p): return comp(p, '±')  # NOTE range and +- are interconvertable...
def plus_or_minus_pair(p): return comp(p, '+-')  # yes that is an b'\x2d'
plus_or_minus = OR(plus_or_minus_symbol, plus_or_minus_pair)

# I hate the people who felt the need to make different type blocks for this stuff in 1673
EN_DASH = b'\xe2\x80\x93'.decode()
HYPHEN_MINUS = b'\x2d'  # yes, the thing that most keyboards have
def en_dash(p): return comp(p, EN_DASH)
def hyphen_minus(p): return comp(p, HYPHEN_MINUS)
dash_thing = OR(en_dash, hyphen_minus)  # THERE ARE TOO MANY AND THEY ALL LOOK THE SAME
double_dash_thing = TIMES(dash_thing, 2)
thing_accepted_as_a_dash = OR(dash_thing, double_dash_thing)

#explicit_range_single =
#explicit_range_pair = TIMES(explicit_range_single, 2)
#explicit_range = OR(explicit_range_pair, explicit_range_single)  # order matters since '-' is in '--'

def lt(p): return comp(p, '<')
def gt(p): return comp(p, '>')
comparison = OR(lt, gt)
def by(p): return comp(p, 'x')
def approx(p): return comp(p, '~')
#op = OR(plus_or_minus, explicit_range, lt, gt, by)
digits = [str(_) for _ in range(10)]
def digit(p): return oper(p, lambda d: d in digits)
def point(p): return comp(p,'.')
def char(p): return oper(p, lambda c: c.isalpha())
int_ = JOINT(TIMES(dash_thing, 0, 1), MANY1(digit))
float_ = JOINT(TIMES(dash_thing, 0, 1),
               OR(JOINT(MANY1(digit), point, MANY(digit)),
                  JOINT(MANY(digit), point, MANY1(digit))))
num = OR(int_, float_)
def percent(p): return comp(p, '%')
percentage = JOINT(num, MANY(space), percent, join=False)
quantity = OR(percentage, JOINT(num, MANY(space), TIMES(unit, 0, 1), join=False))
quantity_require_unit = OR(percentage, JOINT(num, MANY(space), TIMES(unit, 1), join=False))

def to(p): return comp(p, 'to')
range_indicator = OR(thing_accepted_as_a_dash, to)
range_ = JOINT(quantity, MANY(space), range_indicator, MANY(space), quantity_require_unit)

# patterns:
# num op num unit
# num unit op num unit
# unit num
# op num
# opnum
# numunit

#pat1 = JOINT(num, MANY(space), op, MANY(space), num, join=False)

degree = '°'
degree_c = degree + 'C'



def main():
    tests = ("300 mOsm", "0.5 mM", "7 mM", "0.1 Hz.", "-50 pA",
             "200–500mm", "0.3%–0.5%", "1:500", "4%", "10 U/ml",
             "–20°C", "<10 mV", "–70 ± 1 mV", "30 to 150 pA",
             "310 mosmol/l", "13–16 days old", "50 x 50 um",
             "~3.5 - 6 Mohms", "pH 7.3", "17–23 d old", "10 –100",
             "250 +- 70 um", "20±11 mm", "+- 20 degrees"
            )
    weirds = "One to 5", "100-Hz", "25 ng/ul)", "(pH 7.3", "34–36°C."
    fun = [t.split(' ')[-1] for t in tests][:5]
    test_unit_atom = [unit_atom(f) for f in fun]
    test_unit = [unit(f) for f in fun]
    embed()

if __name__ == '__main__':
    main()
    
