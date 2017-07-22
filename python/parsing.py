#!/usr/bin/env python3.6

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
    if p:
        v = p[0]
    else:
        return False, None, p  # we are at the end
    return v == val, v, p[1:]

def oper(p, func):
    if p:
        v = p[0]
    else:
        return False, None, p  # we are at the end
    return func(v), v, p[1:]

def space(p): return comp(p, ' ')
def plus_or_minus_symbol(p): return comp(p,'±')  # NOTE range and +- are interconvertable...
def plus_or_minus_pair(p): return comp(p,'+-')
plus_or_minus = OR(plus_or_minus_symbol, plus_or_minus_pair)
def explicit_range_single(p): return comp(p,'-')
explicit_range_pair = TIMES(explicit_range_single, 2)
explicit_range = OR(explicit_range_pair, explicit_range_single)  # order matters since '-' is in '--'
def lt(p): return comp(p,'<')
def gt(p): return comp(p,'>')
comparison = OR(lt, gt)
def by(p): return comp(p,'x')
op = OR(plus_or_minus, explicit_range, lt, gt, by)
digits = [str(_) for _ in range(10)]
def digit(p): return oper(p, lambda d: d in digits)
def point(p): return comp(p,'.')
def char(p): return oper(p, lambda c: c.isalpha())

# patterns:
# num op num unit
# num unit op num unit
# unit num
# op num
# opnum
# numunit

int_ = MANY1(digit)
float_ = OR(JOINT(MANY1(digit), point, MANY(digit)),
            JOINT(MANY(digit), point, MANY1(digit)))
num = OR(int_, float_)

pat1 = JOINT(num, MANY(space), op, MANY(space), num, join=False)
unit_ = MANY1(char)

percent = '%'
degree = '°'
degree_c = degree + 'C'


def main():
    embed()

if __name__ == '__main__':
    main()
    
