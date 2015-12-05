#!/usr/bin/env python3.5
from typing import Sequence, Union
from IPython import embed
# this could be handled with types itypetead and be safer
_type_concepts = {}
_type_measures = {}
_type_beings = {}
_type_steps = {}

# THESE CANNOT BE SEPARATE NAMESPACES because we will not be able to resolve a measure named 'name1' and a concept named 'name1' :/
# so these aren't namespaces, they are something else, types probably

#_type_locals can include simple parameters

def _fn_bind_concept(name, identifier):  # observe decoupling of state and impl :(
    _type_concepts[name] = identifier

def _fn_bind_being(concept):
    _type_beings[concept] = _ns_concepts[concept]  # FIXME breaks strong typing

def _fn_bind_step(name, step):
    _type_steps[name] = step

#### ok, let us try this again

class Identifier(str):
    all_ids = set()
    def __new__(cls, string: str):
        if string in cls.all_ids:
            raise ValueError('%s: identifier is not unique!' % string)
        else:
            cls.all_ids.add(string)
            return super().__new__(cls, string)

ID = Identifier

class NS1:  # TODO all classes should inherit from this so that instance.name is forced unique, not the right way to implement for real, but helps when thinking about the structure of the parsed language
    all_names = set()

namespace_one = {}

class Concept:
    def __init__(self, name: str, identifier: Identifier):
        self.name = name
        namespace_one[name] = self
        self.identifier = identifier

class Being:
    def __init__(self, concept: Concept):
        self.name = concept.name
        namespace_one[concept.name] = self  # woo preserve strong typing
        self.identifier = concept.identifier

class Measure:
    def __init__(self, name: str, unit: Concept):
        self.name = name
        namespace_one[name] = self
        self.unit = unit

class Step:  # note this is the underlying data structure, not the language itself where steps behave like functions
    def __init__(self,
                 name: str,
                 inputs: Union[Being, Sequence[Being]]=None,
                 measurements: Union[Measure, Sequence[Measure]]=None,
                 outputs: Union[Being, Sequence[Being]]=None,
                 substeps: Union['Step', Sequence['Step']]=None):  # FIXME do parameters go in here? or how to handle... parameters either come from a 'lookup' or are derived from measures on beings (inputs)
        self.name = name
        namespace_one[name] = self
        self.inputs = inputs
        self.measurements = measurements
        self.outputs = outputs
        self.substeps = substeps

#ID('eid1')
concept1 = Concept('c1', ID('eid1'))
volt = Concept('volt', ID('units:volt'))
being1 = Being(concept1)
measure1 = Measure('m1', volt)
s1 = Step('butts', being1, measure1)
embed()
