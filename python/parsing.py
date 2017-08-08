#!/usr/bin/env python3.6

import os
from pyontutils.utils import coln
from IPython import embed

infinity = 9999999  # you know it baby, if we go this deep we will get recursion errors
__script_folder__ = os.path.dirname(os.path.realpath(__file__))

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
                    return True, tuple(matches), p
                else:
                    matches.append(v)
                    p = rest
            success, v, rest = func(p)
            if success:
                return False, None, p
            else:
                success = True
                rest = p

        return success, tuple(matches), rest
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
                return success, (v1, v2), rest
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
        return success, tuple(matches), rest 
    return joint

def JOINT_OR_FIRST(*funcs, join=False):  # TODO improve performance in cases where there is a base that should return true even when the rest fails
    pass

def COMPOSE(func1, func2):
    def compose(p):
        success, v, rest = func1(p)
        if success:
            success, v, rest = func2(rest)
            if success:
                return success, v, rest
        return success, None, p
    return compose

def NOT(func):
    def not_(p):
        success, v, rest = func(p)
        if success:
            return False, v, rest
        else:
            return True, v, rest
    return not_

def END(func1, func2):
    def end_(p):
        success, v, rest = func1(p)
        if not success:
            return success, v, rest
        success2, v2, rest2 = func2(rest)
        if success2:
            return success, v, rest
        else:
            return success2, v2, rest2
    return end_

def SKIP(func1, func2):
    def skip(p):
        success, v, rest = func1(p)
        if not success:
            return success, v, rest
        success2, v2, rest2 = func2(rest)
        if success2:
            return success2, v, rest2
        else:
            return success2, v2, rest2
    return skip

def comp(p, val, lv):
    if p:
        v = p[:lv]  # warning: this will produce order dependencies when you spec the parser
    else:
        return False, None, p  # we are at the end
    return v == val, v, p[lv:]

def comp1(p, val):
    if p:
        v = p[0]  # warning: this will produce order dependencies when you spec the parser
    else:
        return False, None, p  # we are at the end
    return v == val, v, p[1:]

def oper(p, func):
    if p:
        v = p[0]
    else:
        return False, None, p  # we are at the end
    return func(v), v, p[1:]

def noneof(string):
    def noneof_(p):
        if not p:
            return True, p, p
        for s in string:
            success, v, rest = comp(p, s, 1)
            if success:
                return False, v, p
        return True, p[0], p[1:]
    return noneof_

def COMP(val):
    lv = len(val)
    if lv == 1:
        def comp1_(p):
            return  comp1(p, val)
        return comp1_
    else:
        def comp_(p):
            return comp(p, val, lv)
        return comp_

#
# function to allow implementation of what the parser actually does/outputs

def BOX(v):
    return v,

def FLOP(v):
    return tuple(v[::-1])

def transform_value(parser_func, func_to_apply):
    def transformed(p):
        success, value, rest = parser_func(p)
        if success:
            return success, func_to_apply(value), rest
        else:
            return success, value, rest
    return transformed

def PVAL(prefix_name):
    def transformed_prefix(parser_func):
        return transform_value(parser_func, lambda v: ('param:' + prefix_name,) + v)
    return transformed_prefix

def STRINGIFY(func):
    return transform_value(func, lambda v: '"' + str(v).replace('"', '\\"') + '"')

def AT_MOST_ONE(func): return transform_value(TIMES(func, 0, 1), lambda v: v[0] if v else v)

def EXACTLY_ONE(func): return transform_value(TIMES(func, 1, 1), lambda v: v[0] if v else v)

#
# units

DEGREES_UNDERLINE = b'\xc2\xba'.decode()  # º sometimes pdfs misencode these
DEGREES_FEAR = b'\xe2\x97\xa6' # this thing is scary and I have no id what it is or why it wont change color ◦
def get_quoted_list(filename):
    with open(os.path.expanduser('~/ni/protocols/rkt/units/' + filename), 'rt') as f:
        lines = [_.split(';')[0].strip() for _ in f.readlines()]
    return [line.strip("'").strip('(').rstrip(')').split(' . ') for line in lines if line and '.' in line]

_SIPREFS, _SIEXPS, _SIUNITS, _EXTRAS = [get_quoted_list(_) for _ in ('si-prefixes-data.rkt', 'si-prefixes-exp-data.rkt', 'si-units-data.rkt', 'si-units-extras.rkt')]
_silookup = {k: "'" + v for k, v in _SIUNITS + _EXTRAS + [[v, v] for k, v in _SIUNITS] + [[v, v] for k, v in _EXTRAS]}
_siplookup = {k: "'" + v for k, v in _SIPREFS}

def make_funcs(inpt, lookuptable):
    args = []
    for token in sorted(inpt, key=lambda a: -len(a)):  # sort to simulate right associativity (ie da recognized even if d a token)
        #lv_ = len(token)
        #def fn(p, tok=token, lv=lv_):  # late binding stupidity
            #return comp(p, tok, lv)
        tvfn =  transform_value(COMP(token), lambda v: lookuptable[v])
        args.append(tvfn)
    return args

siprefix = OR(*make_funcs(coln(0, _SIPREFS), _siplookup))
#siunit = OR(*make_funcs(coln(0, _SIUNITS) + coln(0, _EXTRAS)))
siunit = OR(*make_funcs(list(coln(0, _SIUNITS + _EXTRAS)) + # need both here to avoid collisions in unit_atom slower but worth it?
                        list(coln(1, _SIUNITS + _EXTRAS)), _silookup))


exponent = COMP('^')
division = COMP('/')
multiplication = COMP('*')
unit_op = OR(division, multiplication)

#siprefix = OR(*[lambda p, t=tok: comp(p, t) for tok, n in SIPREFS])  # that thing about late binding and lambdas..
#siunit = OR(*[lambda p, t=tok: comp(p, t) for tok, n in SIUNITS])  # no real closures, wew lad >_<


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
space = COMP(' ')
spaces = MANY(space)
spaces1 = MANY1(space)
colon = COMP(':')
plus_or_minus_symbol = COMP('±')  # NOTE range and +- are interconvertable...
plus_or_minus_pair = COMP('+-')  # yes that is an b'\x2d'
plus_or_minus = transform_value(OR(plus_or_minus_symbol, plus_or_minus_pair), lambda v: 'plus-or-minus')

# I hate the people who felt the need to make different type blocks for this stuff in 1673
EN_DASH = b'\xe2\x80\x93'.decode()
HYPHEN_MINUS = b'\x2d'.decode()  # yes, the thing that most keyboards have
en_dash = COMP(EN_DASH)
hyphen_minus = COMP(HYPHEN_MINUS)
_dash_thing = OR(en_dash, hyphen_minus)  # THERE ARE TOO MANY AND THEY ALL LOOK THE SAME
dash_thing = transform_value(_dash_thing, lambda v: HYPHEN_MINUS)
double_dash_thing = TIMES(dash_thing, 2)
thing_accepted_as_a_dash = transform_value(OR(double_dash_thing, dash_thing), lambda v: HYPHEN_MINUS)

#explicit_range_single =
#explicit_range_pair = TIMES(explicit_range_single, 2)
#explicit_range = OR(explicit_range_pair, explicit_range_single)  # order matters since '-' is in '--'

lt = COMP('<')
lte = COMP('<=')
gt = COMP('>')
gte = COMP('>=')
comparison = OR(lte, gte, lt, gt)

CROSS = b'\xc3\x97'.decode()
cross = COMP(CROSS)
x = COMP('x')
by = OR(cross, x)
_approx = COMP('~')
approx = transform_value(_approx, lambda v: 'approximately')
digits = [str(_) for _ in range(10)]
def digit(p): return oper(p, lambda d: d in digits)
point = COMP('.')
def char(p): return oper(p, lambda c: c.isalpha())
_int_ = JOINT(TIMES(dash_thing, 0, 1), MANY1(digit), join=True)
int_ = transform_value(_int_, lambda i: int(''.join(i)))
_float_ = JOINT(TIMES(dash_thing, 0, 1),
                OR(JOINT(MANY1(digit), point, MANY(digit), join=True),
                   JOINT(MANY(digit), point, MANY1(digit), join=True)),
                join=True)
float_ = transform_value(_float_, lambda f: float(''.join(f)))
E = COMP('E')
times = COMP('*')
exponental_notation = JOINT(OR(float_, int_),  # FIXME not including as a num for now because it is sometimes used distributively across plust-or-minus infix
                            COMPOSE(spaces, OR(by, times)),
                            COMPOSE(spaces, COMP('10')),
                            exponent, int_)
_scientific_notation = JOINT(OR(_float_, _int_), E, _int_)
scientific_notation = transform_value(_scientific_notation, lambda v: float(v))
num = OR(scientific_notation, float_, int_, num_word)  # float first so that int doesn't capture it

def unit_atom(p): 
    func = OR(JOINT(siprefix, siunit, join=False),
              JOINT(siunit, join=False))  # have to use OR cannot use TIMES  FIXME siunit by itself needs to not be followed by another char? so NOT(siunit)  (different than kgm/s example I used before...)
    func = transform_value(func, FLOP)
    return func(p)

maybe_exponent = transform_value(AT_MOST_ONE(exponent), lambda v: 'exponent')  # ICK not the best way...
unit_dimension = JOINT(unit_atom, maybe_exponent, int_)
unit_base = OR(unit_dimension, unit_atom)  # FIXME this is a hilariously inefficient way to get right associativity

def unit(p):  # TODO cases like '5 mg mL–1' need to be carful with '5mg made to' since that would parse as mg m :/
    func = OR(JOINT(unit_base,
                    COMPOSE(spaces, unit_op),
                    COMPOSE(spaces, unit),
                    join=False),
              unit_base)
    return func(p)  # beware false order of operations

def plus_or_minus_thing(thing): return JOINT(plus_or_minus, COMPOSE(spaces, thing), join=False)

to = COMP('to')
range_indicator = transform_value(OR(thing_accepted_as_a_dash, to), lambda v: 'range')
def range_thing(func): return JOINT(func, COMPOSE(spaces, range_indicator), COMPOSE(spaces, func))

_ph = COMP('pH')
ph = transform_value(_ph, BOX)
P = COMP('P')
post_natal_day = transform_value(P, lambda v: BOX("'postnatal-day"))  # FIXME note that in our unit hierarchy this is a subclass of days
_fold_prefix = END(by, num)
fold_prefix = transform_value(_fold_prefix, lambda v: BOX("'fold"))

prefix_unit = PVAL('prefix-unit')(OR(ph, post_natal_day, fold_prefix))
_prefix_quantity = JOINT(prefix_unit, COMPOSE(spaces, num))  # OR(JOINT(fold, num))
prefix_quantity = transform_value(_prefix_quantity, FLOP)

_percent = COMP('%')
percent = transform_value(_percent, lambda v: BOX("'percent"))
#fold_suffix = transform_value(END(by, NOT(num)), lambda v: BOX("'fold"))  # NOT(num) required to prevent issue with dimensions
fold_suffix = transform_value(END(by, noneof('0123456789')), lambda v: BOX("'fold"))  # NOT(num) required to prevent issue with dimensions
suffix_unit = PVAL('unit')(OR(percent, unit))
suffix_quantity_no_space = JOINT(num, PVAL('unit')(EXACTLY_ONE(fold_suffix)))  # FIXME this is really bad :/ and breaks dimensions...
suffix_quantity = OR(suffix_quantity_no_space,
                     JOINT(num,
                           COMPOSE(spaces, AT_MOST_ONE(suffix_unit))))  # this catches the num by itself and leaves a blank unit
#suffix_quantity1 = OR(suffix_quantity_no_space, JOINT(num, COMPOSE(spaces, EXACTLY_ONE(suffix_unit))))

quantity = PVAL('quantity')(OR(prefix_quantity, suffix_quantity))
#quantity_require_unit = OR(prefix_quantity, suffix_quantity1)
#quantity_with_uncertainty = JOINT(quantity, COMPOSE(spaces, plus_or_minus_thing(quantity)))  # could be error or could be a range spec, also 2nd quantity needs to require unit?? is there some way to do 'if not a then b?' or 'a unit must be in here somwhere?'

dilution_factor = PVAL('dilution')(JOINT(SKIP(int_, colon), int_, join=False))
sq = COMPOSE(spaces, quantity)
sby = COMPOSE(spaces, by)
dimensions = PVAL('dimensions')(OR(JOINT(quantity, COMPOSE(sby, sq), COMPOSE(sby, sq)), JOINT(quantity, COMPOSE(sby, sq))))  # ick

#fold = OR(transform_value(JOINT(by, num, join=False), lambda v: [v[1], v[0]]),  # if we have 'force no space' in suffix/prefix can replace
                          #JOINT(num, by, join=False))

prefix_operator = OR(plus_or_minus, comparison)
infix_operator = OR(plus_or_minus, range_indicator, multiplication, division, exponent)  # colon? doesn't really operate on quantities, note that * and / do not interfere with the unit parsing because that takes precedence
prefix_expression = JOINT(prefix_operator, COMPOSE(spaces, quantity))
def infix_expression(p): return JOINT(quantity,
                                      COMPOSE(spaces, infix_operator),
                                      COMPOSE(spaces, OR(infix_expression, quantity)))(p)  # sigh, not being able to start with yourself
expression = PVAL('expression')(OR(prefix_expression, infix_expression))  # FIXME this doesn't work if you have prefix -> infix are there cases that can happen?

def approximate_thing(thing): return JOINT(EXACTLY_ONE(approx), COMPOSE(spaces, thing), join=False)

_C_for_temp = COMP('C')

C_for_temp = PVAL('unit')(transform_value(_C_for_temp, lambda v: BOX(_silookup['degrees-celcius'])))
temp_for_biology = JOINT(num, C_for_temp, join=False)

# TODO objective specifications...

def FAILURE(p):
    return PVAL('parse-failure')(lambda null: (True, tuple(), p))(p)

def parameter_expression(p): return OR(approximate_thing(parameter_expression),
                                       dimensions,
                                       dilution_factor,
                                       temp_for_biology,
                                       expression,
                                       quantity,
                                       FAILURE)(p)  # now this is some stupid shit right here

# tag docs
whitespace_atom = OR(COMP(' '), COMP('\t'), COMP('\n'))
whitespace = MANY(whitespace_atom)
whitespace1 = MANY1(whitespace_atom)  # FIXME this is broken to negation? (extremely slow)
quote_symbol = COMP('"')
_string = COMPOSE(quote_symbol, SKIP(MANY(NOT(quote_symbol)), quote_symbol))  # TODO escape
string = transform_value(_string, lambda v: ''.join(v))
symbol = OR(char, digit, COMP('-'), COMP('_'), colon, COMP('*'))  # TODO more
_quote = COMPOSE(COMP("'"), END(MANY1(symbol), whitespace_atom))
quote = transform_value(_quote, lambda v: ''.join(v))
tag_doc = SKIP(JOINT(COMPOSE(COMP('('),
                             COMPOSE(whitespace, COMP('tag-doc'))),
                     COMPOSE(whitespace1, quote),
                     COMPOSE(whitespace1, string)),
               COMP(')'))
tag_docs = MANY1(SKIP(tag_doc, whitespace))

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
             "250 +- 70 um", "20±11 mm", "+- 20 degrees",
             '0.1 mg kg–1', '75  mg / kg', '40x', 'x100',
             '200μm×200μm×200μm', '20--29 days', '4 °C', '10×10×10',
             '10 kg * mm^2 / s^2', '10 * 1.1 ^ 30 / 12'
            )
    weirds = ("One to 5", "100-Hz", "25 ng/ul)", "34–36°C.",
              '3*10^6 infectious particles/mL',
              '4.7 +- 0.6 x 10^7 / mm^3',  # FIXME this is ambigious? YES VERY also unit dimensionality...
              '1,850', '4C', 'three', 'Four', 'P28.5±2 days'
             )
    should_fail = ('~~~~1',
                   "(pH 7.3",
                  )
    q = "'"
    fun = [t.split(' ')[-1] for t in tests][:5]
    test_unit_atom = [unit_atom(f) for f in fun]
    test_unit = [unit(f) for f in fun]
    test_quantity = [quantity(t) for t in tests]
    test_expression = [parameter_expression(t) for t in tests + weirds]
    test_expression = '\n'.join(f"'{t+q:<25} -> {parameter_expression(t)[1]}" for t in tests + weirds)
    print(test_expression)
    test_fails = [parameter_expression(t) for t in tests]
    embed()

if __name__ == '__main__':
    main()
    
