import json
from hyputils.hypothesis import HypothesisHelper
from protcur.analysis import AstGeneric, protc
from protcur.sparc import SparcMI
from protcur.core import readTagDocs, justTags


class NamespaceAstValueTranslator:
    """ Base class for defining translators that operate on the astValue of
        some other namespace. These cannot be used 
    """


class RegNVT(type):
    def __init__(self, *args, **kwargs):
        if self.source_type is not None:
            if self.namespace is not None and self.namespace != self.source_type.namespace:
                raise TypeError(f'namespaces do not match! {self.namespace} != '
                                f'{self.source_type.namespace}')
            self.namespace = self.source_type.namespace
            self.source_type.translators[self.target_type.namespace] = self
            #target_namespaces[self.target_type.namespace] = self.target_type
        self.target_type.additional_namespaces[self.namespace] = self
        super().__init__(*args, **kwargs)


class NamespaceValueTranslator:
    """ Base class used to translate naming convetions from one namespace
        into another at the raw value stage rather than the astValue stage.

        Subclasses of this class should never access the astValue of
        self.base since these classes are used to generate self.base.astValue
        and this will cause a recursion error.

        NamespaceHelper.target_type should be set to the class that implements
        the semantics that this namespace should translate into.

    """

    class BaseNamespaceMismatchError:
        pass

    source_type = None
    target_type = None
    classn  = HypothesisHelper.classn
    _value_escape = AstGeneric._value_escape
    _dispatch = AstGeneric._dispatch
    astValue = AstGeneric.astValue
    namespace = None
    _order = tuple()
    isAstNode = True
    additional_namespaces = tuple()  # intentionally not a dict because dont want this

    def __init__(self, target_instance):
        if not hasattr(self, 'prefix_ast'):
            self.prefix_ast = None if self.namespace is None else self.namespace + ':'
        self.target_instance = target_instance
        if not isinstance(self.target_instance, self.target_type):
            raise self.TargetNamespaceMismatchError('{type(self.target_instance)} is not a {self.targt_type}')

    @property
    def supported_tags(self):
        for suffix in self._order:
            yield self.prefix_ast + suffix

    @property
    def value(self):
        return self.target_instance.value

    @property
    def tags(self):
        return set(t for t in self.target_instance.tags if t.startswith(self.prefix_ast))

    @property
    def astType(self):
        tags = self.tags
        for suffix in self._order:
            tag = self.prefix_ast + suffix
            if tag in tags:
                return tag

    def _default_astValue(self):
        #type_ = self.astType
        #predicate = type_ if type_ is not None else '/'.join(self.tags)
        return self.target_instance._default_astValue()
        #return f'({predicate} {self.target_instance._default_astValue()})'
        # we don't have to set the prefix here because
        # in order to get this far the parent has to know about this
        # tag, otherwise it will abort


class order_deco:  # FIXME make it so we dont' have to init every time
    """ define functions in order to get order! """
    def __init__(self):
        self.order = tuple()

    def mark(self, cls):
        if not hasattr(cls, '_order'):
            cls._order = self.order
        else:
            cls._order += self.order

        return cls

    def __call__(self, function):
        self.order += function.__name__,
        return function


od = order_deco()
@od.mark
class protc_generic(NamespaceValueTranslator, metaclass=RegNVT):
    """ tags without namespaces """
    target_type = protc
    namespace = None
    prefix_ast = ''  # hack to fool dispatch, these are NO namespace, not empty namespace
    _order = 'TODO',
    #target_type.additional_namespaces[namespace] = protc_generic  # FIXME

    def __init__(self, generic_instance):
        self.namespace = ''  # hack so we can reuse _dispatch
        super().__init__(generic_instance)

    @property
    def tags(self):
        return set(t for t in self.target_instance.tags if ':' not in t)

    @property
    def astType(self):
        for tag in self._order:
            if tag in self.target_instance.tags:
                return ':' + tag  # hack to fool _dispatch into thinking there is an empty namespace

    #def TODO(self):
        #return '(TODO {self.target_instace._default_astValue())})'
    

od = order_deco()
@od.mark
class protc_ilxtr(NamespaceValueTranslator, metaclass=RegNVT):
    target_type = protc
    namespace = 'ilxtr'
    def __init__(self, protc_instance):
        super().__init__(protc_instance)

    @od
    def technique(self):
        # techniques lacking sections are converted to impls
        # since they are likely to involve many steps and impl
        # is the correct place holder for things with just words
        return f'(protc:impl {json.dumps(self.value)})'

    @od
    def participant(self):
        # FIXME ambiguous
        return f'(protc:input {json.dumps(self.value)})'


od = order_deco()
@od.mark
class protc_sparc(NamespaceValueTranslator, metaclass=RegNVT):
    source_type = SparcMI
    target_type = protc

    def __init__(self, protc_instance):
        super().__init__(protc_instance)

    def default(self):
        return self.target_instance.parameter()

    @property
    def astValue(self):
        prefix, suffix = self.target_instance.astType.split(':', 1)
        return getattr(self, suffix)()

    @od
    def administrationDose(self): return self.default()
    @od
    def administrationFrequency(self): return self.default()
    @od
    def animalSubjectHasWeight(self): return self.default()
    @od
    def characterisiticsOfACStimulation(self): return self.default()
    @od
    def characteristicsOfDCStimulation(self): return self.default()
    @od
    def concentrationOfChemicalInSolution(self): return self.default()
    @od
    def isOfAge(self): return self.default()
    @od
    def solutionFlowRate(self): return self.default()
    @od
    def temperatureHemostasis(self): return self.default()

    def __wat(self):
        if False:
            for tag_from_other_namespace, tag_from_this_namespace in self._order_other_tags:
                if tag_from_other_namespace in tags:
                    return tag_from_this_namespace


class RegTT(type):
    def __init__(self, *args, **kwargs):
        for target_type in self.target_types:
            target_type.tag_translators[self.namespace] = self

        super().__init__(*args, **kwargs)


class TagTranslator:
    """ Use for pre ast translation of tags """

    _tag_lookup = readTagDocs()
    order_ = tuple(t for t in justTags(_tag_lookup) if t.startswith('mo:'))
    namespace = None
    target_namespace = None

    def __init__(self, tag):
        self.tag = tag

    @property
    def translation(self):
        if self.tag in self._tag_lookup:
            tagdoc = self._tag_lookup[self.tag]
            return next(t for t in tagdoc.parents if t.startswith(self.target_namespace + ':'))
        else:
            return self.tag


class mo_to_ilxtr(TagTranslator, metaclass=RegTT):
    target_types = protc,
    namespace = 'mo'
    target_namespace = 'ilxtr'


# sparc translation


class protcur_to_technique:
    """ protocolExecution, techniqueExecution, as well as high level techinques """
    def __init__(self):
        pass


