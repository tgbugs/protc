import csv
from io import StringIO
from pathlib import Path
from datetime import datetime
from collections import defaultdict
from urllib.parse import quote
import rdflib
import ontquery as oq
from pyontutils import combinators as cmb
from pyontutils.core import OntId, simpleOnt
from pyontutils.utils import byCol, anyMembers, makeSimpleLogger
from pyontutils.config import auth
from pyontutils.sheets import get_sheet_values
from pyontutils.annotation import AnnotationMixin
from pyontutils.namespaces import TEMP, ilxtr, editorNote, definition
from pyontutils.closed_namespaces import rdf, rdfs, owl
from hyputils.hypothesis import iterclass
from protcur.analysis import AstGeneric, protc
from protcur.core import TagDoc, log as protcur_log
try:
    breakpoint
except NameError:
    from IPython import embed as breakpoint

log = protcur_log.getChild('sparc')


def oqsetup():
    from pyontutils.namespaces import PREFIXES
    paths = ('ttl/sparc-methods.ttl',  # by convention the first path is expected to define all the tags
             'ttl/methods-helper.ttl',
             'ttl/methods-core.ttl',
             'ttl/methods.ttl')
    ghq = oq.plugin.get('GitHub')('SciCrunch', 'NIF-Ontology',
                                  *paths, branch='sparc')
    pns = (
        # FIXME decl in class
        ('hyp', 'https://hyp.is/'),
        ('hlf', 'https://hypothesis-local.olympiangods.org/'),  # hlfull since extension is preserved
        ('prots-sparc', 'http://uri.interlex.org/tgbugs/uris/protocols/sparc/'),
        ('inst', 'http://uri.interlex.org/temp/uris/sparc/instances/'),
        ('bidsf', 'http://uri.interlex.org/temp/uris/sparc/bids/'),
        ('bf-sun', 'https://app.blackfynn.io/N:organization:4827d4ca-6f51-4a4e-b9c5-80c7bf8e5730/datasets/'),
        ('bf-mvp', 'https://app.blackfynn.io/N:organization:89dfbdad-a451-4941-ad97-4b8479ed3de4/datasets/'))
    [ghq.graph.bind(p, n) for p, n in pns]
    oq.OntCuries(ghq.curies)
    oq.OntCuries(PREFIXES)
    class OntTerm(oq.OntTerm):
        pass
    
    OntTerm.query_init(ghq)
    return OntTerm, ghq


class GraphOutputClass(iterclass):
    """ TODO accumulate triples as we go """

    def __init__(self, *args, **kwargs):
        self._n = -1
        super().__init__(*args, **kwargs)

    def __iter__(self):
        # FIXME this should be implemented somewhere else
        for obj in super().__iter__():
            if obj.astType:
                yield obj

    @property
    def n(self):
        self._n += 1
        return self._n

    @property
    def metadata(self):
        """ ontology metadata ala interlex """
        nowish = datetime.utcnow()  # request doesn't have this
        epoch = nowish.timestamp()
        iso = nowish.isoformat()
        ontid = TEMP['sparc/all-annotations']  # FIXME abstract this
        ver_ontid = rdflib.URIRef(ontid + f'/version/{epoch}/all-annotations')

        yield ontid, rdf.type, owl.Ontology
        yield ontid, owl.versionIRI, ver_ontid
        yield ontid, owl.versionInfo, rdflib.Literal(iso)
        yield ontid, rdfs.comment, rdflib.Literal('All annotations for SPARC metadata.')

    def ttl(self):
        return self.serialize().decode()

    def html(self):
        return self.serialize(format='htmlttl').decode()

    def _report(self, format='tsv'):
        # TODO actual format
        #obj = next(iter(self.objects.values()))
        graph = self.populate_graph()
        strio = StringIO(newline='\n')
        writer = csv.writer(strio, delimiter='\t')
        #writer.writerows()
        xc = set()
        xp = set()
        for s, p, o in sorted(graph):
            p = OntId(p)
            if p.prefix != self.namespace:
                continue
            else:
                p = p.curie
            if isinstance(s, rdflib.URIRef):
                s = OntId(s).curie
            else:
                s = ''
            if isinstance(o, rdflib.URIRef):
                o = OntId(o).curie

            xc.add(s)
            xp.add(p)
            row = [s, p, o]
            writer.writerow(row)
        
        for p in sorted(self.all_properties() - xp):
            writer.writerow(['', p, ''])
        for s in sorted(self.all_classes() - xc):
            writer.writerow([s, '', ''])

        return strio.getvalue()

    def report(self, format='tsv'):
        ptags = {t:len([p for p in v if p.isAstNode]) for t, v in self._tagIndex.items()}
        def pcount(tag):
            return ptags.get(tag, 0)

        tag_docs = self.makeTagDocs()
        skip = ('protc:', 'RRID:', 'NIFORG:', 'CHEBI:', 'SO:', 'PROCUR:', 'mo:', 'annotation-')
        atags = {t:0 for t in tag_docs}
        atags.update({t:len(v) for t, v in self._tagIndex.items()})
        _tags = [[t, d, pcount(t), ','.join(tag_docs[t].types) if t in tag_docs else '']
                 for t, d in atags.items()
                 if all(p not in t for p in skip)
        ]
        tags = sorted(_tags, key=lambda t:t[3])  # sort by type

        strio = StringIO(newline='\n')
        writer = csv.writer(strio, delimiter='\t')
        writer.writerow(['tag', 'annos', 'converted', 'type'])
        for row in tags:
            writer.writerow(row)

        return strio.getvalue()

    def populate_graph(self):
        self._n = -1
        graph = rdflib.Graph()
        for t in self.metadata:
            graph.add(t)

        if hasattr(self, 'class_extra_triples'):
            for t in self.class_extra_triples():
                graph.add(t)

        for obj in self.objects.values():
            if obj.isAstNode:
                for t in obj.triples:
                    graph.add(t)

        [graph.bind(p, n) for p, n in self.graph.namespaces()]

        if hasattr(self, 'queries'):
            for query in self.queries(graph):
                for t in query():
                    graph.add(t)

        return graph 

    def serialize(self, format='nifttl'):
        graph = self.populate_graph()
        # TODO add and remove triples on websocket update
        return graph.serialize(format=format)


def _make_sparc_domain_mapping():
    # property by domain
    oq.OntCuries({'sparc':'http://uri.interlex.org/tgbugs/uris/readable/sparc/'})
    mapping = {
        'ephys':{
            OntId('sparc:ExperimentOnLiveOrganism'),
            OntId('sparc:BioelectronicNerveBlockingModulation'),
            OntId('sparc:ElectricalAcquisition'),
            OntId('sparc:ElectricalModulation'),
            OntId('sparc:ChemicalAcquisition'),
            OntId('sparc:MechanicalAcquisition'),
            OntId('sparc:IREnergyModulation'),
            OntId('sparc:Modulation'),
            OntId('sparc:ThermalEnergyModulation'),
        },
        'microscopy':{
            OntId('sparc:MicroscopyAcquisition'),
            OntId('sparc:TissuePreparationForMicroscopy'),
            OntId('sparc:EnvironmentForTissueDerivatives'),
            OntId('sparc:ExperimentOnTissueDerivatives'),
        },
        'radiology':{
            OntId('sparc:RadiologcalAcquistion'),
            OntId('sparc:RadiologicalImagingProtocol'),
            OntId('sparc:FunctionalMRIFeatures'),
            OntId('sparc:GeneralMRISequence'),
        },
        'transcriptomics':{
            OntId('sparc:RNASeqSpecimen'),
            OntId('sparc:TranscriptomicsAcquisition'),
            OntId('sparc:TranscriptomicsExperiment'),
            OntId('sparc:BulkRNASeqSpecimen'),
            OntId('sparc:SingleCellRNASeqSpecimen'),
        },
        'optical':{
            OntId('sparc:OpticalAcquisition'),  # FIXME vs Microscopy?
        },

        # feeders that are not required but might be relevant if one of these was used upstream
        'various':{
            OntId('sparc:EngineeredTissue'),
            OntId('sparc:Extraction'),
            OntId('sparc:ExperimentOnTissueDerivatives'),
            OntId('sparc:Specimen'),
            OntId('sparc:EnvironmentForLiveOrganism'),
        },
        'cell culture':{
            OntId('sparc:CellCulture'),
            OntId('sparc:StemCells'),
            OntId('sparc:StemCellExperiment'),
            OntId('sparc:CellCultureExperiment'),
        },
        'histology':{
            OntId('sparc:IDISCO'),
            OntId('sparc:Histochemistry'),
            OntId('sparc:TissuePreservation'),
            OntId('sparc:TissuePreparationSteps'),
            OntId('sparc:TissueSample'),
            OntId('sparc:Embedding'),
            OntId('sparc:CounterStaining'),
            OntId('sparc:FreezingVsChemicalFixation'),
            OntId('sparc:Immunohistochemistry'),
            OntId('sparc:MountVsSections'),
            OntId('sparc:TissueExperiment'),
            OntId('sparc:SectionThickness'),  # FIXME really a data property not a class?
            OntId('sparc:Staining'),
            OntId('sparc:EndogenousReporters'),
            OntId('sparc:Sectioning'),
            OntId('sparc:TissueMounting'),
            OntId('sparc:TissueClearing'),
            OntId('sparc:TissueClearance'),
            OntId('sparc:SlidesVsGrid'),
            OntId('sparc:EmbeddingMedia'),
            OntId('sparc:SectioningDevice'),
        },
        'general':{
            OntId('sparc:Environment'),
            OntId('sparc:RRIDs'),
            OntId('sparc:Analysis'),
            OntId('sparc:AnatomicalLocation'),
            OntId('sparc:OrganismSubject'),
            OntId('sparc:Experiment'),
            OntId('sparc:Acquisition'),
            OntId('sparc:Anesthesia'),
            OntId('sparc:ChemicalInSolution'),
            OntId('sparc:Procedure'),
            OntId('sparc:Protocol'),
            OntId('sparc:Measurement'),
            OntId('sparc:Resource'),
            OntId('sparc:Organization'),
            OntId('sparc:Researcher'),
            OntId('sparc:Sedation'),
            OntId('sparc:AnimalExperiment'),
            OntId('sparc:AnimalSubject'),
            OntId('sparc:HumanSubject'),
            OntId('sparc:ClinicalExperiment'),
        },
    }
    return {v:k for k, vs in mapping.items() for v in vs}


def _make_sparc_range_mapping():
    oq.OntCuries({'xsd':str(rdflib.XSD)})
    mapping = {
        'ephys':{
            OntId('sparc:Modulation'),
        },
        'microscopy':{
            OntId('sparc:TissuePreparationForMicroscopy'),
            OntId('sparc:TissuePreparationSteps'),
            OntId('sparc:MicroscopyAcquisition'),
        },
        'radiology':{
            OntId('sparc:RadiologicalImagingProtocol'),
            OntId('sparc:GeneralMRISequence'),
            OntId('sparc:FunctionalMRIFeatures'),
        },
        'transcriptomics':{
        },
        'optical':{
        },
        'various':{
            OntId('sparc:Specimen'),
            OntId('sparc:TissueSample'),
            OntId('sparc:Extraction'),
        },
        'cell culture':{
            OntId('sparc:CellCulture'),
            OntId('sparc:StemCells'),
        },
        'general':{
            OntId('sparc:Analysis'),
            OntId('sparc:Anesthesia'),
            OntId('sparc:Procedure'),
            OntId('sparc:Protocol'),
            OntId('sparc:Sedation'),
            OntId('sparc:Acquisition'),
            OntId('sparc:Environment'),
            OntId('sparc:Organization'),
            OntId('sparc:Experiment'),
            OntId('sparc:Researcher'),
            OntId('sparc:Resource'),
            OntId('sparc:AnatomicalLocation'),
            OntId('sparc:ChemicalInSolution'),
            OntId('sparc:Measurement'),
        },
        None:{
            OntId('xsd:string'),
            OntId('owl:real')
        },
    }
    return {v:k for k, vs in mapping.items() for v in vs}


class SparcMI(AstGeneric, metaclass=GraphOutputClass):
    """ Class for transforming Hypothes.is annotations
        into sparc datamodel rdf"""
    namespace = 'sparc'
    prefix_skip_tags = 'PROTCUR:', 'annotation-'

    generic_tags = tuple()
    translators = {}
    tag_translators = {}

    indentDepth = 2
    objects = {}  # TODO updates
    _tagIndex = {}
    _replies = {}  # without this Hybrid replies will creep in
    _astParentIndex = {}

    graph = None  # TODO set at
    domain_mapping = _make_sparc_domain_mapping()
    range_mapping = _make_sparc_range_mapping()
    skip_lu = ('sparc:isOfFileType',
               'sparc:isOfAge',
               'sparc:firstName',
               'sparc:rnaSeqSpecimenHasSampleNumber',
               'sparc:rnaSeqSpecimenHasTotalCellNumber',
               'sparc:softwareEnvironmentForAcquisition',
               'sparc:lastName')

    dfr = 'sparc-data-file-registry'
    _bids_id = -1
    # TODO trigger add to graph off websocket
    # to do this modify HypothesisHelper so that
    # classes can define a class method for
    # add, update, and delete that will fire additional actions

    def __init__(self, *args, **kwargs):
        self._subject = None
        self.extra_triples = tuple()
        super().__init__(*args, **kwargs)
        self.ttl = self._instance_ttl
        self.html = self._instance_html
        self.serialize = self._instance_serialize
        self.n = self._instance_n

    def _instance_n(self):  # FIXME not clear why we need this
        return self.__class__.n

    @classmethod
    def _new_inst_id(cls):  # FIXME (obvs)
        return TEMP[f'sparc/instances/{cls.n}']

    @classmethod
    def _new_bids_id(cls):
        cls._bids_id += 1
        return TEMP[f'sparc/bids/{cls._bids_id}']  # FIXME (obvs)

    @classmethod
    def class_extra_triples(cls):
        """ GraphOutputClass check this function for class level extra triples """
        for t in cls.registry():
            yield t

    @classmethod
    def registry(cls):
        uris = set(a.uri for a in SparcMI if 'rcont' not in a.uri)  # rcont remove temp iris w/ bad annos

        data, grid, cells_index = get_sheet_values(cls.dfr, 'Data', False)
        ml = max(len(r) for r in data)
        normalized = [r + ([''] * (ml - len(r))) for r in data]
        prot, pgrid, pcells_index = get_sheet_values(cls.dfr, 'Protocols', False)
        pml = max(len(r) for r in prot)
        pnorm = [r + ([''] * (pml - len(r))) for r in prot]

        d = byCol(normalized)
        p = byCol(pnorm, to_index=('protocol_identifier',))
        bidsf = set(d.bids_file_name)

        cls._bids_id = -1
        for fn in sorted(bidsf):
            subject = cls._new_bids_id()
            yield subject, rdf.type, owl.NamedIndividual
            yield subject, rdf.type, ilxtr.BIDSFile
            yield subject, rdfs.label, rdflib.Literal(fn)
            for r in d:
                if r.bids_file_name == fn:
                    if r.protocol and r.protocol != 'None':
                        # FIXME hasProtocol not entirely correct
                        # data produced from process partially documented by ...
                        pr = p.searchIndex('protocol_identifier', r.protocol)
                        piri = rdflib.URIRef(r.protocol)
                        asl = rdflib.URIRef(pr.annotation_substrate_link)
                        yield subject, ilxtr.hasProtocol, piri
                        yield piri, rdf.type, owl.NamedIndividual
                        yield piri, rdf.type, ilxtr.protocolArtifact
                        yield piri, ilxtr.hasAnnotationSubstrate, asl  # NOTE this is our link to uris
                    annosubstr = rdflib.URIRef(r.annotation_substrate) if r.annotation_substrate else None
                    fileiri = rdflib.URIRef(r.file_id) if r.file_id else annosubstr
                    yield subject, ilxtr.hasFile, fileiri # XXX NOTE this flattens everything
                    yield fileiri, rdf.type, owl.NamedIndividual
                    yield fileiri, rdf.type, ilxtr.FlatFile
                    yield fileiri, rdfs.label, rdflib.Literal(r.file_name)
                    yield fileiri, ilxtr.fileType, rdflib.Literal(r.file_type)
                    if annosubstr and annosubstr != fileiri:
                        yield fileiri, ilxtr.hasAnnotationSubstrate, annosubstr
    @staticmethod
    def format_data_query(object):
        query_prefix = 'https://neuinfo.org/data/search?q='
        if isinstance(object, rdflib.URIRef):
            return rdflib.URIRef(query_prefix + OntId(object).curie)  # FIXME TODO
        elif isinstance(object, rdflib.Literal):
            return rdflib.URIRef(query_prefix + quote(object))

    @classmethod
    def queries(cls, graph):
        """ queries that should be run to expand the graph """
        # TODO chaining could be achieved by having queries return functions
        # that are generators that are called consecutively here or
        # consecutively via the metaclass

        def dodataquery(p, o):
            return (p != rdf.type
                    and (not (isinstance(o, rdflib.Literal) and
                              (o.datatype == TEMP['protc:unit'] or
                               o.isdigit()))
                         or isinstance(o, rdflib.URIRef)))

        def protocols():
            # protocols
            q = graph.query('''
                select distinct ?file ?inst where {

                ?file rdf:type ilxtr:BIDSFile .
                ?file ilxtr:hasProtocol ?prot .

                ?prot ilxtr:hasAnnotationSubstrate ?substr .
                ?substr ilxtr:hasAnnotation ?anno .

                ?blank rdf:type owl:Axiom .
                ?blank owl:annotatedSource ?inst .
                ?blank ilxtr:literatureReference ?anno .
                }''')
            for result in q.bindings:
                file = result['file']
                inst = result['inst']

                linker = rdflib.BNode()
                yield file, ilxtr.metaLocal, linker
                #yield linker, rdf.type, ilxtr.fromProt  # FIXME not quite correct
                for p, o in graph[inst]:
                    yield linker, p, o
                    if dodataquery(p, o):
                        no = cls.format_data_query(o)
                        #yield linker, ilxtr.dataQuery, no
                        yield file, ilxtr.dataQuery, no
                        if isinstance(o, rdflib.URIRef):
                            yield no, rdfs.label, next(graph[o:rdfs.label])

                if isinstance(inst, rdflib.URIRef):
                    yield file, ilxtr.metaFromProtocol, inst  # FIXME naming
                    o = next(o for o in graph[inst:rdf.type]
                            if o != owl.NamedIndividual)
                    continue
                    #yield file, ilxtr.metaFromProtocolTypes, o
                #elif isinstance(inst, rdflib.BNode):
                    #for p, o in graph[inst]:
                        #yield file, ilxtr.rawTextTODO, o
                        #yield file, p, o

        def explogs():
            # experiment logs

            q = graph.query('''
                select distinct ?file ?inst where {

                ?file rdf:type ilxtr:BIDSFile .
                ?file ilxtr:hasFile ?flat .

                { ?flat ilxtr:hasAnnotation ?anno . }
                UNION
                { ?flat ilxtr:hasAnnotationSubstrate ?substr .
                  ?substr ilxtr:hasAnnotation ?anno . }

                ?blank rdf:type owl:Axiom .
                ?blank owl:annotatedSource ?inst .
                ?blank ilxtr:literatureReference ?anno .
            }''')

            done = set()
            for result in q.bindings:
                file = result['file']
                inst = result['inst']

                # because we are flattening we only need one occurance, even if
                # the there are distinct annotations that copies were sourced from
                # the full list of instances is retained under metaFromProv
                linker = rdflib.BNode()
                new = False
                for p, o in graph[inst]:
                    if o not in done or p == rdf.type:
                        yield linker, p, o
                        done.add(o)
                        new = True
                        if dodataquery(p, o):
                            no = cls.format_data_query(o)
                            #yield linker, ilxtr.dataQuery, no
                            yield file, ilxtr.dataQuery, no
                            if isinstance(o, rdflib.URIRef):
                                yield no, rdfs.label, next(graph[o:rdfs.label])

                if new:
                    yield file, ilxtr.metaLocal, linker
                    #yield linker, rdf.type, ilxtr.fromLogs
                    #yield linker, ilxtr.instIri, inst

                if isinstance(inst, rdflib.URIRef):
                    yield file, ilxtr.metaFromProv, inst  # FIXME naming
                    #continue
                #elif isinstance(inst, rdflib.BNode):
                    #for p, o in graph[inst]:
                        #yield file, ilxtr.rawTextTODO, o
                        #yield file, p, o

        return protocols, explogs

    @classmethod
    def all_domains(cls):
        return {p:set(cls.domain_mapping[OntId(d)]
                      for d in cls._domain(cls.graph, p))
                for p in cls.all_properties()}

    @classmethod
    def all_ranges(cls):
        out = {}
        for p in cls.all_properties():
            ranges = cls._range(cls.graph, p)
            for range in ranges:
                if range is not None:
                    out[p] = cls.range_mapping[OntId(range)]
                else:
                    out[p] = 'Parent Property'

        return dict(out)

    @classmethod
    def all_modalities(cls):
        out = {}
        for p in cls.all_properties():
            domains = set(cls._domain(cls.graph, p))
            ranges = set(cls._range(cls.graph, p))
            if not ranges:
                ranges.add(None)

            modality = cls._modality(p, domains, ranges)
            out[p] = modality

        for o in cls.all_classes():
            modality = cls._modality(o, {o}, {o})
            out[o] = modality

        return dict(out)

    @classmethod
    def domain_properties(cls):
        out = defaultdict(set)
        for predicate, domains in cls.all_domains().items():
            for domain in domains:
                out[domain].add(predicate)

        return dict(out)

    @classmethod
    def modality_tags(cls):
        out = defaultdict(set)
        for predicate, modality in cls.all_modalities().items():
            out[modality].add(predicate)

        return dict(out)

    @property
    def modality(self):
        return self._modality(self.astType, self.domain, self.range)

    @classmethod
    def _modality(cls, tag, domains, ranges):
        domain = sorted(domains)[0]  # FIXME all first by accident
        range = sorted(ranges)[0]  # FIXME better logic
        if domain is None:
            if OntId(tag) in cls.all_properties():
                log.warning(f'no domain for {cls.astType}')
            return None

        dmodality = cls.domain_mapping[OntId(domain)]

        if range is None:
            return dmodality

        try:
            rmodality = cls.range_mapping[OntId(range)]
        except KeyError:
            if tag != range:
                log.warning('mapping for range', tag, range)
            return dmodality

        if rmodality is None:
            return dmodality
        elif OntId(range) == OntId('sparc:Measurement'):
            return dmodality
        elif dmodality in ('general', 'various'):
            return rmodality
        elif rmodality in ('general', 'various'):
            return dmodality
        elif rmodality != dmodality:
            log.warning(f'Modality mismatch! {dmodality} {rmodality} {domain} {range} {tag}')
            return rmodality  # assume that the range gives more specifcity
        else:
            return dmodality

    @property
    def domain(self):
        d = set(self._domain(self.graph, self.astType))
        if not d:
            d.add(None)

        return d

    @staticmethod
    def _domain(graph, property):
        for object in graph[OntId(property).u:rdfs.domain]:
            yield from graph.transitive_subjects(rdfs.subClassOf, object)

    @property
    def range(self):
        r = set(self._range(self.graph, self.astType))
        log.debug(f'aaaaaaaaaaaa {r}')
        if not r:
            r.add(None)

        return r

    @staticmethod
    def _range(graph, property):
        # TODO byPredicate? I swear I did this already ...
        yield from graph[OntId(property).u:rdfs.range]

    @property
    def only_tag(self):
        """ just in case """
        try:
            return next(t for t in self.tags if self.prefix_ast in t)
        except StopIteration:
            return None

    @property
    def complex_tags(self):
        tags = set(t for t in self.tags if self.prefix_ast in t)
        if len(tags) == 2:
            return tags
        else:
            return None
            # NOTE as implemented at the moment if there is more than 2 sparc tags
            # then only the first will be selected
            # TODO warn on > 2 tags?
            return self.only_tag

    #@property
    #def isAstNode(self):
        #bool([t for t in self.tags if self.prefix_ast in t])

    @property
    def astType(self):
        return self.only_tag

    @property
    def isClass(self):
        """ the tag used is a class and implies the start of a named individual """
        classes = self.all_classes()
        complex = self.complex_tags
        if complex is not None:
            return any(t in classes for t in complex)
        return self.only_tag in classes

    @property
    def isProperty(self):
        properties = self.all_properties()
        complex = self.complex_tags
        if complex is not None:
            return any(t in properties for t in complex)
        return self.only_tag in properties

    @classmethod
    def all_properties(cls):
        if cls.graph:
            if not hasattr(cls, '_all_properties'):
                # FIXME OntId duplicates rdflib qname
                cls._all_properties = set(OntId(s).curie for s, o in cls.graph[:rdf.type:]
                                          if #not print(s, o) and
                                          isinstance(s, rdflib.URIRef) and
                                          OntId(s).prefix == cls.namespace and
                                          o in (owl.ObjectProperty,
                                                owl.DatatypeProperty,
                                                owl.AnnotationProperty))

            return cls._all_properties
        else:
            pass

    @classmethod
    def all_classes(cls):
        if cls._done_loading:
            if not hasattr(cls, '_all_classes'):
                cls._all_classes = set(OntId(s).curie for s, o in cls.graph[:rdf.type:]
                                       if isinstance(s, rdflib.URIRef) and
                                       OntId(s).prefix == cls.namespace and
                                       o == owl.Class)
            return cls._all_classes

    @classmethod
    def all_tags(cls):
        return cls.all_classes() | cls.all_properties()

    @classmethod
    def _graph(cls):
        local_version = auth.get_path('ontology-local-repo') / 'ttl/sparc-methods.ttl'  # FIXME hardcoded
        if local_version.exists():  # on the fly updates from local
            graph = rdflib.Graph().parse(local_version.as_posix(), format='turtle')
        else:
            graph = cls.graph
        return graph

    @classmethod
    def _docs(cls, graph=None, comments=True):
        if graph is None:
            graph = cls._graph()
        mods = cls.all_modalities()
        ad = defaultdict(set)
        for s, o in graph[:rdfs.domain:]:
            ad[OntId(s).curie].add(OntId(o).curie)
        ar = defaultdict(set)
        for s, o in graph[:rdfs.range:]:
            ar[OntId(s).curie].add(OntId(o).curie)

        for tag in sorted(cls.all_classes() | cls.all_properties()):
            uri = OntId(tag).u  # FIXME inefficient
            types = sorted(OntId(_type).curie for _type in graph[uri:rdf.type])
            subThingOf = rdfs.subPropertyOf if any('Property' in t for t in types) else rdfs.subClassOf
            parents = [OntId(p).curie for p in graph[uri:subThingOf]]
            edNote = '\n'.join([o for o in graph[uri:editorNote]])
            mod = mods[tag]  # if tag in mods else ''

            try:
                _def = ' ' + next(graph[uri:definition])
            except StopIteration:
                _def = ' No definition.'

            doc = f'**{" ".join(types) if types else ""}**{_def}'
            kwargs = {'editorNote':edNote if comments else '',
                      'domain':ad[tag] if tag in ad and ad[tag] else {''},
                      'range':ar[tag] if tag in ar and ar[tag] else {''},
                      'modality':mod,}
            yield types, tag, parents, doc, kwargs

    @classmethod
    def makeTagDocs(cls, comments=True):
        if cls._done_loading:
            graph = cls._graph()
            if (not hasattr(cls, '_tag_lookup') or
                not cls._tag_lookup or
                cls.graph != graph):
                cls._tag_lookup = {tag:TagDoc(doc, parents, types, **kwargs)
                                   for types, tag, parents, doc, kwargs in
                                   cls._docs(graph, comments=comments)}

            return cls._tag_lookup
        else:
            raise BaseException('why are you erroring here?')

    @property
    def value(self):
        v = super().value
        # FIXME TODO
        if 'hyp.is' in v:
            a, b = v.split('https://hyp.is', 1)  # hyp.is comes second
            v = a
        return v 

    @property
    def subject(self):
        # TODO just get the subject from the 'children'
        # ie just paste the hypothesis link to the subject in!
        # we know what parent object to attach this to
        isClass = self.isClass  # has to be called before _subject
        domain = self.domain
        if self._subject is None:
            if isClass:
                self._subject = self._new_inst_id()
                t = self._subject, rdf.type, OntId(self.astType).u
                self.extra_triples += (t,)
                return self._subject
            for child in self.children:
                if child == self:  # cases where we want to anchor a named individual and a predicate
                    return self._subject
                elif not domain or domain and child.type_object in domain:
                    if child.isClass:
                        self._subject = child.subject
                        return self._subject
                else:
                    msg = ' '.join(('{}' for _ in range(5)))
                    args =([OntId(d).curie for d in domain],
                           child.type_object,
                           list(self.tags),
                           self.value,
                           self._repr)
                    log.error(msg.format(*args))
                    #breakpoint()

            self._subject = rdflib.BNode()

        return self._subject

    @property
    def predicate(self):
        # FIXME yield to allow multiple branches
        if self.isClass and self.isProperty:
            complex = self.complex_tags
            self.extra_triples += ((self.subject, rdf.type, owl.NamedIndividual),
                                   (self.subject, rdf.type, self.type_object))
            p = next(t for t in self.tags if t in self.all_properties())
            return OntId(p).u
        elif self.isClass:
            return rdf.type
        elif self.isProperty:
            p = next(t for t in self.tags if t in self.all_properties())  # FIXME for now are going with 1 tag
            return OntId(p).u
        else:
            return ilxtr.WHAT

    @property
    def type_object(self):
        if self.isClass:
            if self.isProperty:
                c = next(t for t in self.tags if t in self.all_classes())
                return OntId(c).u

            return OntId(self.astType).u

    @property
    def object(self):
        if self.isClass and not self.isProperty:
            return owl.NamedIndividual
        else:
            if (self.astType and 'protc' in self.translators and
                self.astType.split(':', 1)[-1] in
                self.translators['protc']._order):
                log.debug(protc.byId(self.id))
                v = protc.byId(self.id).astValue
                if '(rest' in v:
                    v, junk = v.rsplit('(rest', 1)
                    v = v.strip()
                return rdflib.Literal(v, datatype=TEMP['protc:unit'])
            id, label = self.ontLookup(self.value)
            if id is None:
                return rdflib.Literal(self.value)
            else:
                oid = OntId(id)
                if oid.prefix:
                    if self.graph.namespace_manager.store.prefix(oid.prefix) is None:
                        self.graph.bind(oid.prefix, oid.namespace)
                o = oid.u
                if label:
                    #et = self.extra_triples
                    t = o, rdfs.label, rdflib.Literal(label)
                    self.extra_triples += (t,) #(_ for _ in chain(et, (t,)))
                    #self.extra_triples = (_ for _ in chain(et, (t,)))
                return o

    @property
    def triples(self):
        t = self.subject, self.predicate, self.object
        sl = rdflib.URIRef(self.shareLink)
        po = ilxtr.literatureReference, sl
        av = (((ilxtr.annotationValue, rdflib.Literal(self.value)),)
              if self.value != self.object else tuple())
        notes = [(OntId(self.CURATOR_NOTE_TAG), rdflib.Literal(n)) for n in self.curatorNotes]
        yield t
        yield from self.extra_triples
        yield from cmb.annotation(t, po, *av, *notes)()
        yield rdflib.URIRef(self.uri), ilxtr.hasAnnotation, sl  # FIXME normalize self.uri
        # TODO any additional stuff

    def _instance_ttl(self):
        """ instance ttl """
        return self.serialize()

    def _instance_html(self):
        return self.serialize(format='htmlttl')

    def _instance_serialize(self, format='nifttl'):
        """ instance serialize """
        graph = rdflib.Graph()
        for t in self.triples:
            graph.add(t)

        if self.isClass:
            for p, o in self.graph[self.subject::]:
                if p != rdf.type:
                    t = self.subject, p, o
                    graph.add(t)
            # TODO proper transitive closure
            #for t in self.graph.transitive_objects(self.subject, None):
                #graph.add(t)

        # FIXME slooow
        [graph.bind(p, n) for p, n in self.graph.namespaces()]  # FIXME not quite right?
        # TODO add and remove triples on websocket update
        return graph.serialize(format=format)

    _repr_join = '\n\n'

    def __repr__(self, html=False, number='*'):
        """ turtle repr of class leaving prefixes implicit """
        # when its empty all you get is the anno > nice
        if html:
            text = self.html()
            sep = b'<br>\n<br>\n'
            start, rest = number.split('>', 1)
            head = start + '>### ' + rest + sep.decode()
        else:
            text = self.ttl()
            sep = b'\n\n'
            head = f'### {number}{sep.decode()}'

        return head + sep.join([s for s in text.split(sep)
                                if s.endswith(b'.')][1:]).decode()


class technique_to_sparc(AnnotationMixin):
    # we have 3 options for how to do this, neurons style, methods style, or parcellation style
    def __init__(self):
        olr = auth.get_path('git-local-base') / 'duplicates' / 'sparc-NIF-Ontology'
        g = (rdflib.Graph()
            .parse((olr / 'ttl/sparc-methods.ttl').as_posix(),
                    format='turtle')
            .parse((olr / 'ttl/methods-core.ttl').as_posix(),
                    format='turtle')
            .parse((olr / 'ttl/methods-helper.ttl').as_posix(),
                    format='turtle')
            .parse((olr / 'ttl/methods.ttl').as_posix(),
                    format='turtle'))

        rq = oq.plugin.get('rdflib')(g)
        self.rq = rq
        class OntTerm(oq.OntTerm):
            pass

        OntTerm.query_init(rq)
        oq.OntCuries({p:i for p, i in g.namespaces()})

        # if you don't set this QueryResult will switch to pyontutils and hit interlex so very slow
        sparc_ents = OntTerm.search(None, prefix='sparc')
        ontids = sorted(OntId(u) for u in
                        set(e for t in g for e in t
                            if isinstance(e, rdflib.URIRef) and 'sparc/' in e))

        {
            'protc:*measure': 'sparc:Acquisition',
            ('protc:aspect', ()): 'sparc:Anesthesia',
            ('protc:aspect',): 'sparc:Acquisition',   # not actualized probably
            None: 'sparc:perfusionProtocol',
            }
        # first pass
        # # do a first pass to have 0 or 1 on all the edges

        # collapse
        # link all parts of protocls into experiments
        breakpoint()
        self._triples = tuple()  # TODO

        protocol = list(self.protocols)
        inputs = [p for p in protocol if p.astType == 'protc:input']
        aspects = [p for p in protocol if p.astType == 'protc:aspect']
        parameters = [p for p in protocol if p.astType == 'protc:parameter*']
        measure = [p for p in protocol if p.astType == 'protc:*measure' or p.astType == 'protc:symbolic-measure']
        telos = [p for p in protocol if p.astType == 'protc:telos']

    @property
    def onts(self):
        #self.onts = rq.onts  # FIXME this is obscure and indirect sort the imports so it is clear
        yield from self.rq.onts

    @property
    def protocols(self):
        yield from (p for p in protc if '.html' in p.uri)# or (any('.html' in p for p in p.astParents if p is not None) if p.astParents is not None else False))

    @property
    def inputs(self):
        yield from (p for p in protocol if p.astType == 'protc:input')

    @property
    def triples(self):
        yield from self._triples


def sparc_mapping():
    tts = technique_to_sparc()

    protocols = list(tts.protocols)
    metadata_example = simpleOnt(filename=f'sparc-metadata-example',
                                 prefixes=oq.OntCuries._dict,  # FIXME 
                                 imports=[o for o in tts.onts if 'sparc-methods' in o],
                                 triples=tts.triples,
                                 comment='example converstion to sparc metadata',
                                 path='ttl/',
                                 branch='sparc',
                                 fail=False,
                                 _repo=True,
                                 write=False,
                                 calling__file__=__file__,)

    breakpoint()

    @property
    def protc_unit_mapping(self):
        return self.astType in self._protc_tag_mapping['protc:parameter*']

    @classmethod
    def make_protc_tag_mapping(cls):
        substrings = (
            'Weight',
            'temperature',
            'Age',
            'concentration',
            'Dose',
            'Frequency',
            'Rate',
            'CStim',   # FIXME may not work in long run
        )
        if cls.graph is not None:
            if not hasattr(cls, '_protc_tag_mapping'):
                cls._protc_tag_mapping = {}
                cls._protc_tag_mapping['protc:parameter*'] = set(t for t in cls.all_properties()
                                                                if anyMembers(t, *substrings))

            return cls._protc_tag_mapping
        else:
            return tuple()


def main():
    from hyputils.hypothesis import group, group_to_memfile, HypothesisHelper
    from protcur import namespace_mappings as nm
    from protcur.core import annoSync
    from protcur.sparc import SparcMI  # oh no ... this is so much easier than what I had been doing >_<
    from protcur.analysis import Hybrid, protc
    get_annos, annos, stream_thread, exit_loop = annoSync(group_to_memfile(group),
                                                          helpers=(HypothesisHelper, Hybrid, protc, SparcMI))
    OntTerm, ghq = oqsetup()
    SparcMI.graph = ghq.graph
    p = [protc(a, annos) for a in annos]
    _smi = [SparcMI(a, annos) for a in annos]
            #if any(t.startswith('sparc:') for t in a.tags)]
    smi = [s for s in _smi if s.isAstNode]
    s = smi[0]
    _ = [repr(s) for s in smi]
    sparc_tags_from_protc = [_ for _ in [SparcMI.translators['protc'](p).tags for p in protc] if _]
    trouble = protc.byId('tkkziO-mEei0Xze-UL2X8g')
    wat = SparcMI.byId('tkkziO-mEei0Xze-UL2X8g')
    breakpoint()
    return
    sparc_mapping()


if __name__ == '__main__':
    main()
