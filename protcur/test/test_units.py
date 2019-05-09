import unittest
import rdflib
from protcur import units


class TestUnits(unittest.TestCase):
    def test_export(self):
        a = units.UnitsParser('10 mm').asPython()
        aa = list(a.n3(rdflib.BNode()))
        b = units.UnitsParser('1 mg/kg').asPython()
        c = list(b.n3(rdflib.BNode()))
        d = units.UnitsParser('100 lm / 1000 W').asPython()
        e = list(d.n3(rdflib.BNode()))
        f = units.UnitsParser('10010.010110 g / 10mg').asPython()
        g = list(f.n3(rdflib.BNode()))
        r = units.UnitsParser('1-100T').asPython()
        s = list(r.n3(rdflib.BNode()))
        w = units.UnitsParser('9-14 weeks').asPython()
        x = list(w.n3(rdflib.BNode()))
        breakpoint()
