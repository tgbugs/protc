import rdflib
from pyontutils import combinators as cmb
from pysercomb.pyr import units as pyru
from pyontutils.namespaces import TEMP
from pyontutils.closed_namespaces import rdf, owl, rdfs
from nifstd_tools.methods.core import prot, proc, tech, asp, dim, unit  # FIXME circular imports incoming ...

xsd = rdflib.XSD
a = rdf.type


class n3:

    _bases = {}

    @classmethod
    def cast(cls, thing):
        if isinstance(thing, cls):
            return thing
        else:
            return cls._bases[type(thing)](thing)

    @property
    def n3(self):
        return rdflib.Literal(self)


class floatn3(n3, float):
    def __truediv__(self, other):
        return floatn3(super().__truediv__(other))


class intn3(n3, int):
    def __truediv__(self, other):
        return floatn3(super().__truediv__(other))


n3._bases[float] = floatn3
n3._bases[int] = intn3


class Unit(pyru.Unit):
    def n3(self, value):
        base_unit = unit[self.unit.fullName]  # grrrr
        # FIXME this would be so much easier if i just
        # implemented everything in one place ...
        return rdflib.Literal(self.prefix.to_base(value)), base_unit


class PrefixUnit(Unit, pyru.PrefixUnit):
    pass


class Quantity(pyru.Quantity):
    def __init__(self, value, unit=None):
        value = n3.cast(value)
        super().__init__(value, unit)

    def n3(self, subject):
        if not self.unit:  # FIXME ... predicate how?
            yield subject, TEMP.hasValue, self.value.n3
            return

        value, unit = self.unit.n3(self.value)
        yield subject, TEMP.hasValue, value
        yield subject, TEMP.hasUnit, unit


class PrefixQuantity(Quantity, pyru.PrefixQuantity):  # urg
    pass


class Range(pyru.Range):
    def n3(self, subject):
        # TODO correctly done inside a restriction as well
        start = self.start.value.n3
        stop = self.stop.value.n3
        type_ = (xsd.integer if
                 isinstance(start.value, int) and
                 isinstance(stop.value, int)
                 else owl.real)
        # FIXME need the base normalized values
        if self.start.unit:
            v1, type_ = self.start.unit.n3(start.value)

        elif self.stop.unit:
            v2, type_ = self.stop.unit.n3(stop.value)

        def min_(s, p):
            o = rdflib.BNode()
            yield s, p, o
            yield o, xsd.minInclusive, start

        def max_(s, p):
            o = rdflib.BNode()
            yield s, p, o
            yield o, xsd.maxInclusive, stop

        yield subject, a, rdfs.Datatype
        yield subject, owl.onDatatype, type_
        yield from cmb.olist.serialize(subject, owl.withRestrictions, min_, max_)


class Dilution(pyru.Dilution):
    def n3(self):
        self.left
        self.right


class Dimensions(pyru.Dimensions):
    def n3(self):
        self.dims


def LoR_n3(self, subject_or_value=None):
    l = self.left
    r = self.right
    if subject_or_value is None:
        yield rdflib.Literal(f'{l}{self.op}{r}')
        return

    if isinstance(l, Quantity):
        subject = subject_or_value
        # TODO triple conv as well
        value_ = getattr(l.value, self._op)(r.value)
        unit__ = getattr(l.unit, self._op)(r.unit).n3  # huh ... would you look at that
        value, unit_ = unit__(value_)

        yield subject, TEMP.hasValue, value
        yield subject, TEMP.hasUnit, unit_

    elif isinstance(l, Unit):
        # FIXME everything except the n3 conversion should go to pyr
        value = subject_or_value
        prefix = getattr(l.prefix, self._op)(r.prefix)
        unit_ = getattr(l.unit, self._op)(r.unit)
        base_value = prefix.to_base(value)
        yield base_value
        n3 = unit_.n3()
        if isinstance(unit_, self.__class__):
            yield from n3
        else:
            yield n3

    else:
        raise ValueError(subject_or_value)


@property
def Expr_triples(self):
    yield from self.n3(rdflib.BNode())


@property
def Expr_ttl(self):
    graph = rdflib.Graph()
    [graph.add(t) for t in self.triples]
    return graph.serialize(format='nifttl')


pyru.LoR.n3 = LoR_n3  # sad that monkey patching works better here
#class Add(pyru.Add):
#class Mul(pyru.Mul):
#class Div(pyru.Div):
#class Exp(pyru.Exp):

Expr = pyru.Expr
#Expr.bindImpl(None, Add, Mul, Div, Exp)
Expr.triples = Expr_triples
Expr.ttl = Expr_ttl

ParamParser = pyru.ParamParser
ParamParser.bindImpl(None,
                     Unit,
                     PrefixUnit,
                     Quantity,
                     PrefixQuantity,
                     Range,
                     Dilution,
                     Dimensions)

UnitsParser = pyru.UnitsParser
