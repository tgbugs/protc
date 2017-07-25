#!/usr/bin/env python3.6

import os
from IPython import embed

infinity = 9999999  # you know it baby, if we go this deep we will get recursion errors

# utility

def col0(inpt): return list(zip(*inpt))[0]
def col1(inpt): return list(zip(*inpt))[1]

# combinators

def OR(*funcs):
    def or_(p):
        for f in funcs:
            success, v, rest = f(p)
            if success:
                return success, v, rest
        return False, None, p  # explicit values since different funcs will have parsed to different depths
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

def JOINT(*funcs, join=False):  # FIXME something is up with multichar tokens when join=True
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

def COMPOSE(func1, func2):
    def compose(p):
        success, v, rest = func1(p)
        if success:
            success, v, rest = func2(rest)
            if success:
                return success, v, rest

        return success, None, p
    return compose

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
# function to allow implementation of what the parser actually does/outputs

def transform_value(parser_func, func_to_apply):
    def transformed(p):
        success, value, rest = parser_func(p)
        if success:
            return success, func_to_apply(value), rest
        else:
            return success, value, rest
    return transformed

#
# units

def get_quoted_list(filename):
    with open(os.path.expanduser('~/ni/protocols/rkt/units/' + filename), 'rt') as f:
        lines = [_.split(';')[0].strip() for _ in f.readlines()]
    return [line.strip("'").strip('(').rstrip(')').split(' . ') for line in lines if line and '.' in line]

_SIPREFS, _SIEXPS, _SIUNITS, _EXTRAS = [get_quoted_list(_) for _ in ('si-prefixes-data.rkt', 'si-prefixes-exp-data.rkt', 'si-units-data.rkt', 'si-units-extras.rkt')]
_silookup = {k:v for k, v in _SIUNITS + _EXTRAS + [[v, v] for k, v in _SIUNITS] + [[v, v] for k, v in _EXTRAS]}
_siplookup = {k:v for k, v in _SIPREFS}

def make_funcs(inpt, lookuptable):
    args = []
    for token in sorted(inpt, key=lambda a: -len(a)):  # sort to simulate right associativity (ie da recognized even if d a token)
        def fn(p, tok=token):  # late binding stupidity
            return comp(p, tok)
        tvfn =  transform_value(fn , lambda v: lookuptable[v])
        args.append(tvfn)
    return args

siprefix = OR(*make_funcs(col0(_SIPREFS), _siplookup))
#siunit = OR(*make_funcs(col0(_SIUNITS) + col0(_EXTRAS)))
siunit = OR(*make_funcs(col0(_SIUNITS) + col0(_EXTRAS) +  # need both here to avoid collisions in unit_atom slower but worth it?
                             col1(_SIUNITS) + col1(_EXTRAS), _silookup))
unit_atom = OR(JOINT(siprefix, siunit, join=False), JOINT(siunit, join=False))  # have to use OR cannot use TIMES  FIXME siunit by itself needs to not be followed by another char? so NOT(siunit)  (different than kgm/s example I used before...)
def division(p): return comp(p, '/')
def multiplication(p): return comp(p, '*')
unit_op = OR(division, multiplication)
def unit(p):  # TODO cases like '5 mg mL–1' need to be carful with '5mg made to' since that would parse as mg m :/
    return OR(JOINT(unit_atom, COMPOSE(spaces, unit_op), COMPOSE(spaces, unit), join=False), unit_atom)(p)  # beware false order of operations

#siprefix = OR(*[lambda p, t=tok: comp(p, t) for tok, n in SIPREFS])  # that thing about late binding and lambdas..
#siunit = OR(*[lambda p, t=tok: comp(p, t) for tok, n in SIUNITS])  # no real closures, wew lad >_<

# abbreviations for units
def ph(p): return comp(p, 'pH')

# number words
_numlookup = {
    'zero':0,
    'one':1,
    'two':2,
    'three':3,
    'four':4,
    'five':5,
    'six':6,
    'seven':7,
    'eight':8,
    'nine':9, }
num_word_lower = OR(*make_funcs(_numlookup, _numlookup))
def num_word_cap(p): return num_word_lower(p.lower())
num_word = OR(num_word_lower, num_word_cap)

# basic tokens
def space(p): return comp(p, ' ')
spaces = MANY(space)
spaces1 = MANY1(space)
def colon(p): return comp(p, ':')
def plus_or_minus_symbol(p): return comp(p, '±')  # NOTE range and +- are interconvertable...
def plus_or_minus_pair(p): return comp(p, '+-')  # yes that is an b'\x2d'
plus_or_minus = OR(plus_or_minus_symbol, plus_or_minus_pair)

# I hate the people who felt the need to make different type blocks for this stuff in 1673
EN_DASH = b'\xe2\x80\x93'.decode()
HYPHEN_MINUS = b'\x2d'.decode()  # yes, the thing that most keyboards have
def en_dash(p): return comp(p, EN_DASH)
def hyphen_minus(p): return comp(p, HYPHEN_MINUS)
_dash_thing = OR(en_dash, hyphen_minus)  # THERE ARE TOO MANY AND THEY ALL LOOK THE SAME
dash_thing = transform_value(_dash_thing, lambda v: HYPHEN_MINUS)
double_dash_thing = TIMES(dash_thing, 2)
thing_accepted_as_a_dash = transform_value(OR(double_dash_thing, dash_thing), lambda v: HYPHEN_MINUS)

#explicit_range_single =
#explicit_range_pair = TIMES(explicit_range_single, 2)
#explicit_range = OR(explicit_range_pair, explicit_range_single)  # order matters since '-' is in '--'

def lt(p): return comp(p, '<')
def lte(p): return comp(p, '<=')
def gt(p): return comp(p, '>')
def gte(p): return comp(p, '>=')
comparison = OR(lte, gte, lt, gt)

CROSS = b'\xc3\x97'.decode()
def cross(p): return comp(p, CROSS)
def x(p): return comp(p, 'x')
by = OR(cross, x)
def approx(p): return comp(p, '~')
#op = OR(plus_or_minus, explicit_range, lt, gt, by)
digits = [str(_) for _ in range(10)]
def digit(p): return oper(p, lambda d: d in digits)
def point(p): return comp(p,'.')
def char(p): return oper(p, lambda c: c.isalpha())
_int_ = JOINT(TIMES(dash_thing, 0, 1), MANY1(digit), join=True)
int_ = transform_value(_int_, lambda i: int(''.join(i)))
_float_ = JOINT(TIMES(dash_thing, 0, 1),
                OR(JOINT(MANY1(digit), point, MANY(digit), join=True),
                   JOINT(MANY(digit), point, MANY1(digit), join=True)),
                join=True)
float_ = transform_value(_float_, lambda f: float(''.join(f)))
num = OR(float_, int_)  # float first so that int doesn't capture it
def percent(p): return comp(p, '%')
percentage = JOINT(num, COMPOSE(spaces, percent), join=False)

def plus_or_minus_thing(thing): return JOINT(plus_or_minus, COMPOSE(spaces, thing), join=False)

ph_value = JOINT(ph, COMPOSE(spaces, num), join=False)

def AT_MOST_ONE(func): return transform_value(TIMES(func, 0, 1), lambda v: v[0] if v else v)
def EXACTLY_ONE(func): return transform_value(TIMES(func, 1, 1), lambda v: v[0] if v else v)
quantity = OR(percentage, JOINT(num, COMPOSE(spaces, AT_MOST_ONE(unit)), join=False))  # FIXME annoying multiple brackets [['k Hz']]
quantity_require_unit = OR(percentage, JOINT(num, COMPOSE(spaces, EXACTLY_ONE(unit)), join=False))
quantity_with_uncertainty = JOINT(quantity, COMPOSE(spaces, plus_or_minus_thing(quantity)), join=False)  # could be error or could be a range spec, also 2nd quantity needs to require unit?? is there some way to do 'if not a then b?' or 'a unit must be in here somwhere?'


def to(p): return comp(p, 'to')
range_indicator = OR(thing_accepted_as_a_dash, to)
range_ = JOINT(quantity, COMPOSE(spaces, range_indicator), COMPOSE(spaces, quantity), join=False)
dilution_factor = JOINT(int_, colon, int_, join=False)
sq = COMPOSE(spaces, quantity)
sby = COMPOSE(spaces, by)
dimensions = OR(JOINT(quantity, sby, sq, sby, sq, join=False), JOINT(quantity, sby, sq, join=False))  # ick

fold = OR(transform_value(JOINT(by, num, join=False), lambda v: [v[1], v[0]]),
                          JOINT(num, by, join=False))

interval = JOINT(comparison, COMPOSE(spaces, quantity), join=False)

def approximate_thing(thing): return JOINT(EXACTLY_ONE(approx), COMPOSE(spaces, thing), join=False)

def _C_for_temp(p): return comp(p, 'C')

C_for_temp = transform_value(_C_for_temp, lambda v: [_silookup['~oC']])
temp_for_biology = JOINT(num, C_for_temp, join=False)

# TODO objective specifications...

def parameter_expression(p): return OR(approximate_thing(parameter_expression),
                                       dimensions,
                                       interval,
                                       range_,
                                       fold,
                                       dilution_factor,
                                       ph_value,
                                       temp_for_biology,
                                       quantity_with_uncertainty,
                                       quantity_require_unit,
                                       quantity,
                                       plus_or_minus_thing(quantity),
                                       num_word,
                                      )(p)


# patterns:
# num op num unit
# num unit op num unit
# unit num
# op num
# opnum
# numunit

#pat1 = JOINT(num, spaces, op, spaces, num, join=False)

#degree = '°'
#degree_c = degree + 'C'



def main():
    tests = ('1 daL', "300 mOsm", "0.5 mM", "7 mM", "0.1 Hz.", "-50 pA",
             "200–500mm", "0.3%–0.5%", "1:500", "4%", "10 U/ml",
             "–20°C", "<10 mV", "–70 ± 1 mV", "30 to 150 pA",
             "310 mosmol/l", "13–16 days old", "50 x 50 um",
             "~3.5 - 6 Mohms", "pH 7.3", "17–23 d old", "10 –100",
             "250 +- 70 um", "20±11 mm", "+- 20 degrees"
             '0.1 mg kg–1', '75  mg / kg', '40x', 'x100',
             '200μm×200μm×200μm', '20--29 days', '4 °C'
            )
    weirds = ("One to 5", "100-Hz", "25 ng/ul)", "(pH 7.3",
              "34–36°C.", '3*10^6 infectious particles/mL',
              '4.7 +- 0.6 x 10^7 / mm^3', '1,850',
              '4C', 'three', 'Four'
             )
    should_fail = ('~~~~1',
                  )
    fun = [t.split(' ')[-1] for t in tests][:5]
    test_unit_atom = [unit_atom(f) for f in fun]
    test_unit = [unit(f) for f in fun]
    test_quantity = [quantity(t) for t in tests]
    test_expression = [parameter_expression(t) for t in tests + weirds]
    test_fails = [parameter_expression(t) for t in tests]
    embed()

if __name__ == '__main__':
    main()
    
