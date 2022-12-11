import base64, uuid
from collections import defaultdict
from urllib.parse import urlparse
import idlib
from pyontutils.utils import isoformat
from hyputils import hypothesis as hyp
from .core import log
from .analysis import extract_links_from_markdown
#from desc.prof import profile_me


class ParentDeletedError(Exception):
    """ SIGH bad design yet again """


def cleandoc(doc):
    return ' '.join(doc.split())


class Pool(hyp.AnnotationPool):

    def __init__(self, annos=None, cls=hyp.HypothesisAnnotation):
        super().__init__(annos, cls)
        # FIXME possibly subtype this to HtmlPool ??
        self._oid_index = {
            a._row['extra']['id_original']:a for a in annos
            if 'extra' in a._row and 'id_original' in a._row['extra']}

    def add(self, annos):
        super().add(annos)
        for a in annos:
            if 'extra' in a._row and 'id_original' in a._row['extra']:
                self._oid_index[a._row['extra']['id_original']] = a

    def byOriginalId(self, id_original):
        try:
            return self._oid_index[id_original]
        except KeyError:
            pass

    def bySlugTail(self, slug_tail):
        # FIXME index these
        for o in self._annos:
            if o.slug_tail == slug_tail:
                yield o

    @property
    def topological_annos(self):
        """ api default order works for refereces/replies but not parent/child
        """
        # XXX text parent cycles must be stricly forbidden otherwise it is
        # impossible to clone between groups (so far this has never happened)
        # because it is extremely unlikely that the text of an annotation would
        # ever contain a shared link pointing to a reply
        tas = toposort(self)
        return tas

    def clone_to(self, pool, h, *, test=True):
        """ clone (with modifications) to new pool posting as user to group"""
        for anno in self.topological_annos:
            new = pool.byOriginalId(anno.id)
            if new is not None:
                # TODO detect if there have been changes
                log.info(f'already cloned as {new}')

            elif anno.is_protocols_io():
                if not anno.has_data():
                    log.warning(f'skipping cases where we have no data {anno.uri_html.asStr()}')
                    continue

                try:
                    payload = anno.html_payload(h.username, h.group, pool)
                except ParentDeletedError as e:
                    log.warning(e)
                    # don't sync replies that have no parent, they will be invisible
                    continue

                try:
                    if not test:
                        resp = h.post_annotation(payload)
                        blob = resp.json()
                    else:  # XXX testing
                        blob = {k:v for k, v in payload.items()}
                        blob['id'] = base64.urlsafe_b64encode(uuid.uuid4().bytes)[:-2].decode()
                        blob['created'] = 'now'
                        blob['updated'] = 'now'

                    new = anno.__class__(blob)
                    pool.add([new])
                except Exception as e:
                    cs = list(self.replies(anno.id))
                    if cs:
                        raise e
                    raise e
                    # check if there are children

    def docs(self):
        def key(r):
            return ['' if c is None else c for c in r]

        def fib(o):
            # at some point we made these fatal by default
            try:
                return o.uri_bound
            except (idlib.exceptions.IdDoesNotExistError, idlib.exceptions.NotAuthorizedError) as e:
                log.error(e)
                return None

        report = sorted(set(
            [(o.slug_tail,
              o.private_id,
              o.uri_normalized,
              o.uri_original,
              fib(o),
              o)
             for o in self._annos]),
                      key=key)

        docs = {}
        for st, pi, un, uo, ub, o in report:
            if st not in docs:
                docs[st] = {'uri-normalized': set(),
                            'uri-original': set(),
                            'uri-bound': set(),}

            docs[st]['uri-normalized'].add(un)
            docs[st]['uri-original'].add(uo)
            if ub is not None:
                docs[st]['uri-bound'].add(ub.identifier.identifier)
            #docs[st]['annos'].append(o)

            if pi is not None:
                if 'private-id' not in docs[st]:
                    docs[st]['private-id'] = set()

                docs[st]['private-id'].add(pi)

        return docs


class Annotation(hyp.HypothesisAnnotation):

    _pio_cache = {}  # FIXME idlib should handle this transparently when data is cached

    def __repr__(self):
        return f'{self.__class__.__name__}({self.id!r})'

    @property
    def uri_original(self):
        return self.uri

    @property
    def _uri_original_parsed(self):
        if not hasattr(self, '_c_uop'):
            self._c_uop = urlparse(self.uri)

        return self._c_uop

    @property
    @idlib.utils.cache_result
    def uri_normalized(self):
        """ URI containing scheme netloc and path.
            Trailing =/abstract= also removed """
        u = self._uri_original_parsed
        path = u.path
        if path.endswith('/abstract'):
            path, _abs = path.rsplit('/', 1)

        return 'https://' + u.netloc + path

    @property
    @idlib.utils.cache_result
    def uri_bound(self):
        # FIXME confusing naming since this is the uri for the doc not anno
        if self._pio is not None:
            return self._pio.uri_human

    @property
    def _pio(self):
        if not hasattr(self, '_c_pio'):
            urin = self.uri_normalized
            if urin not in self._pio_cache:
                try:
                    self._pio_cache[urin] = idlib.Pio(self.uri_normalized)
                except idlib.exceptions.MalformedIdentifierError:
                    self._pio_cache[urin] = None

            self._c_pio = self._pio_cache[urin]

        return self._c_pio

    def is_protocols_io(self):
        return bool(self._pio)

    @property
    def private_id(self):
        # FIXME come up with some actual criteria for this
        if self.slug_tail and len(self.slug_tail) > 12:
            return self.slug_tail

    @property
    def uri_api_int(self):
        if self.is_protocols_io():
            try:
                return self._pio.uri_api_int
            except idlib.exc.RemoteError as e:
                # usually shouldn't happen if we start from a private id
                # unless the whole protocol has been deleted
                pass

    @property
    def uri_html(self):
        if self.is_protocols_io():
            try:
                return self._pio.uri_human_html
            except idlib.exc.RemoteError as e:
                # usually shouldn't happen if we start from a private id
                # unless the whole protocol has been deleted
                pass

    @property
    def uri_for_hypothesis(self):
        if self.is_protocols_io():
            try:
                # XXX protocols.io braindead link rel canonical issues
                # XXX combined with hypothes.is braindead anchoring issue
                return self._pio.uri_api_int.uri_human.asUri().replace('www.', '')
            except idlib.exc.RemoteError as e:
                # usually shouldn't happen if we start from a private id
                # unless the whole protocol has been deleted
                pass

    @property
    @idlib.utils.cache_result
    def slug_tail(self):
        """ The set of chars following the final =-= in a protocols.io URI. """

        try:
            uri_bound = self.uri_bound
        except idlib.exceptions.RemoteError:
            uri_bound = None

        if uri_bound is not None:  # FIXME uri_bound vs _pio needs col
            return uri_bound.slug_tail
        elif (self.is_protocols_io() and
              self._pio.identifier.prefix == 'pio.view'):  # FIXME account for no data separately
            return self._pio.slug_tail

    @idlib.utils.cache_result
    def is_annotation(self):
        return super().is_annotation()

    @property
    def protc_tags(self):
        return [t for t in self.tags if t.startswith('protc:')]

    def has_protc_tags(self):
        return bool(self.protc_tags)

    def has_data(self):
        """ do we have access to protocol data?
            regardless of the reason for lack of access, we need
        """
        try:
            return self.is_protocols_io() and self._pio.data()
        except idlib.exceptions.RemoteError:
            return False

    def has_protc_or_reply(self, pool):
        if self.is_reply() and self.parent(pool) is None:
            # my hatred for systems that delete the identifiers of
            # their deleted content is hard to articulate
            log.warning(f'Parent was deleted from\n{self}')
            return False

        return (self.has_protc_tags() or
                (self.is_reply() and self.parent(pool).include_for_paper(pool)))

    def include_for_paper(self, pool):
        return self.has_data() and self.has_protc_or_reply(pool)

    @property
    def html_target(self):
        # FIXME have to do this all in order so that the parents will already exist!
        ufh = self.uri_for_hypothesis
        target = {'source': ufh}
        # XXX have to use canonical because hypothes.is document iding is bad?
        selector = [s for s in self.selectors if s['type'] == 'TextQuoteSelector']  # XXX watch out for multi target case
        if selector:
            target['selector'] = selector

        return [target]

    @property
    def html_document(self):
        doc = {
            'title': [self.uri_api_int.title],  # get the latest title
        }

        try:
            doi = self.uri_api_int.doi
        except idlib.exceptions.RemoteError as e:
            log.error(e)
            doi = None

        if doi is not None:  # this might be messing up document identification?
            doc['dc'] = {
                'identifier': [
                    doi.identifier.curie,
                    doi.identifier.suffix,  # handle form
                ]}

        ufh = self.uri_for_hypothesis
        if ufh is not None:
            doc['link'] = [
                {'href': ufh,
                 'rel': 'canonical',
                 }]

        return doc

    @property
    def html_extra(self):
        return {
            'document': self.html_document,
            'id_original': self.id,
            'updated_original': self.updated,  # needed for change detection
            'uri_html': self.uri_html.asUri(),  # XXX hypothesis is braindead on document identification and anchoring
            # we don't need the group id embedded
            # nor do we want to risk leaking it
         }


    @property
    @idlib.utils.cache_result
    def text_parent_ids(self):
        text = self.text
        bads = (
            'oxvwiCmfEem9-hfQWeIrcw',  # self reference, can't fix without more processing, can't delete
            'juUahocPEem2zks3ALW0tg',  # incorrect circular reference to X5pbtIcQEemZSQsKkd_KIw
            'fyiidoPGEem38QfhKz5VLw',  # self ref
            'uXkJwC7zEemw8k_1tMnOfg',  # badly broken multiple circular, requires deep intervention
            'zlZXIHNuEem9HDcWGaQrMQ',  # bad reference from input -> aspect

        )
        if self.id in bads:
            return []
        elif 'hyp.is' in text:
            def get_id(l):
                i = l.split('/')[3]
                if i in ('journals.plos.org', 'www.protocols.io'):
                    log.debug(l)
                return i

            return [get_id(l) for l in extract_links_from_markdown(text)]
        else:
            return []

    def html_text(self, html_pool):
        text = self.text
        if 'hyp.is' in text:
            # we are safe to do this because there are no cases
            # where there are both links and other text
            def clean(l):
                old_id = l.split('/')[3]  # XXX this is not foolproof, one url is mangled
                #log.debug(old_id)
                new = html_pool.byOriginalId(old_id)  # XXX FIXME this can and will fail because not in topological order
                if new is None:
                    log.warning(f'parent annotation no longer exists on {self}: {old_id}')
                else:
                    new_id = new.id  # we want to error if this is None
                    return f'https://hyp.is/{new_id}'

            return '\n'.join([cl for link in extract_links_from_markdown(text)
                              for cl in (clean(link),) if cl is not None])
        else:
            return text

    def html_references(self, html_pool):
        parents = [html_pool.byOriginalId(ref)
                   for ref in self.references]

        for p in parents:
            if p is None:
                raise ParentDeletedError(f'reference deleted for {self}')

        return [p.id for p in parents]

    def html_payload_less_ugp(self, pool):
        # FIXME parent for replies!
        ufh = self.uri_for_hypothesis
        out = {
            'uri': ufh,
            'target': self.html_target,
            'tags': self.tags,
            'text': self.html_text(pool),
            'document': self.html_document,
            'extra': self.html_extra,
        }
        if self.references:
            out['references'] = self.html_references(pool)  # FIXME pool required

        return out

    def html_payload(self, user, group, pool):
        return {
            **self.html_payload_less_ugp(pool),
            'user': 'acct:' + user + '@hypothes.is',
            'group': group,
            'permissions': {
                "read": ['group:' + group],
                "update": ['acct:' + user + '@hypothes.is'],
                "delete": ['acct:' + user + '@hypothes.is'],
                "admin":  ['acct:' + user + '@hypothes.is'],
            }}


def toposort(pool):
    marked = set()
    temp = set()
    qq = list(pool._annos)
    out = []
    def visit(n):
        if n in marked:
            return
        if n in temp:
            import pprint
            raise Exception(f'oops you have a cycle {n}\n{pprint.pformat(n._row)}')

        temp.add(n)
        for m_id in (n.references + n.text_parent_ids):
            m = pool.byId(m_id)
            if m is not None:
                visit(m)
            else:
                log.warning(f'non-existent parent {m_id}')

        temp.remove(n)
        marked.add(n)
        qq.remove(n)
        out.append(n)

    while qq:
        n = qq[0]
        visit(n)

    return out


class AnnoCounts:
    """ number of annotations matching some criteria """

    def __init__(self, pool):
        self._pool = pool

    @idlib.utils.cache_result
    def all(self):
        """ all starting annotations """
        return self._pool._annos

    @idlib.utils.cache_result
    def all_with_protc_tags_or_reply(self):
        """ any annotations that have at least one =protc:= tag
            or are a reply to an annotation with a =protc: tag= """
        return [f for f in self.all() if f.has_protc_or_reply(self._pool)]

    @idlib.utils.cache_result
    def from_protocols_io(self):
        """ annotations on URIs with protocols.io netloc """
        return [f for f in self.all() if f.is_protocols_io()]

    @idlib.utils.cache_result
    def with_data(self):
        """ protocols.io annotations where we can retrieve data from the API """
        return [f for f in self.from_protocols_io() if f.has_data()]

    @idlib.utils.cache_result
    def with_protc_tags_or_reply(self):
        """ protocols.io annotations with data and that have at least one
            =protc:= tag or are a reply to an annotation with a =protc: tag= """
        return [f for f in self.with_data() if f.has_protc_or_reply(self._pool)]

    def with_min_annos_end(self, minimum_annotation_count=None):  # FIXME not sure if want?
        hhs = self.with_protc_tags_or_reply()
        idn = IdNormalization(hhs)
        return idn.annos_end(minimum_annotation_count)

    def document_stats(self, *args, minimum_annotation_count=None):
        if minimum_annotation_count is None:
            raise TypeError('minimum_annotation_count is a required keyword argument')

        limit = minimum_annotation_count
        rows = []
        for f in self.fs:
            hhs = f()
            idn = IdNormalization(hhs)
            if not rows:
                column_header = ('name', 'doc', *idn.column_header())
                column_description = ('doc', 'doc', *idn.column_description())
                rows.append(column_header)
                rows.append(column_description)

            columns = idn.counts(limit)
            row = (f.__name__, cleandoc(f.__doc__), *columns)
            rows.append(row)

        return rows

    @property
    def fs(self):
        return (
            self.all,
            self.from_protocols_io,
            self.with_data,
            self.with_protc_tags_or_reply,
        )

    def row_header(self):
        return [f.__name__ for f in self.fs]

    def row_description(self):
        return [cleandoc(f.__doc__) for f in self.fs]


class IdNormalization:
    """ number of unique documents for a set of annotations using certain criteria """

    def __init__(self, hypothesis_helpers):  # FIXME this should take a pool !!!
        self.hhs = hypothesis_helpers

    @idlib.utils.cache_result
    def annos_start(self):  # TODO
        """ total starting annotations for this pass """
        return self.hhs

    @idlib.utils.cache_result
    def raw_uris(self):
        """ unmodified uris """
        return set(h.uri for h in self.hhs)

    @idlib.utils.cache_result
    def normalized(self):
        """ normalized uris """
        norms = defaultdict(list)
        for anno in self.hhs:
            norms[anno.uri_normalized].append(anno)
        return dict(norms)

    @idlib.utils.cache_result
    def slug_tails(self):
        """ trailing fragment of protocols.io uri """
        slugs = defaultdict(list)
        for anno in self.hhs:  # FIXME apparently this is what is eating the memory !??!!
            slugs[anno.slug_tail].append(anno)
        return dict(slugs)

    @idlib.utils.cache_result
    def slug_streams(self):
        """ idlib.Pio streams associated with each slug """
        return {k:set(a._pio for a in v) for k, v in self.slug_tails().items()}

    @idlib.utils.cache_result
    def _uri_humans(self):
        urs = defaultdict(list)
        for anno in self.hhs:  # FIXME apparently this is what is eating the memory !??!!
            urs[anno.uri_normalized].append(anno)
        return dict(urs)

    @idlib.utils.cache_result
    def _uri_api_ints(self):
        idints = defaultdict(list)
        for anno in self.hhs:  # FIXME apparently this is what is eating the memory !??!!
            idints[anno.uri_api_int].append(anno)
        return dict(idints)

    def slug_rows(self):
        yield ('slug',
               'id',
               'uri',
               'anno count',
               'private',
               'private only',
               'doi',
               'title',
               'author count',
               'authors',
               'protocol created',
               'protocol updated',
               'protocol has versions',
               'anno date first',
               'anno date last',
               'anaesthesia',
               'microscopy',
               'close but no cigar',  # euthanasia, ephys rig
               )

        tails = self.slug_tails()
        for slug, pios in self.slug_streams().items():
            annos = tails[slug]
            spios = sorted([p for p in pios], key=lambda p: p.uri_human)
            _privates = [p for p in spios if p.identifier.is_private()]
            private = _privates[0] if _privates else None
            pio = spios[0]  # have to pick one uri if there are multiple
            hpio = pio.uri_human

            minad = min([a.created for a in annos])
            maxad = max([a.updated for a in annos])

            private_only = False

            try:
                authors = [a.name for a in hpio.authors]
                id = hpio
            except idlib.exceptions.RemoteError as e:
                if pio.identifier.is_private():
                    private_only = True
                    authors = [a.name for a in pio.authors]
                    id = pio
                else:
                    raise e

            authors_s = '|'.join(authors)

            yield (slug,
                   id.data()['id'],
                   hpio.asStr(),
                   len(annos),
                   private.asStr() if private else '',
                   private_only,
                   id.doi.asStr() if id.doi else '',
                   id.title,  # title
                   len(authors),
                   authors_s,
                   isoformat(id.created),
                   isoformat(id.updated),
                   id.hasVersions,
                   minad,
                   maxad,
                       ''
                       '',
                       '',
                       '',
                       )

    def document_rows(self):
        norms = self.normalized()
        yield ('slug',
               'uri',
               'anno count',
               'doi',
               'title',
               'author count',
               'authors',
               'protocol created',
               'protocol updated',
               'protocol has versions',
               'anno date first',
               'anno date last',
               'anaesthesia',
               'microscopy',
               'close but no cigar',  # euthanasia, ephys rig
               )

        for slug, pios in self.slug_streams().items():
            #annos = tails[slug]
            spios = sorted([p for p in pios], key=lambda p: p.uri_human)
            for pio in spios:  # have to denormalize a bit
                annos = norms[pio.asStr()]  # already normalized I think?
                minad = min([a.created for a in annos])
                maxad = max([a.updated for a in annos])

                authors = [a.name for a in pio.authors]
                authors_s = '|'.join(authors)
                yield (slug,
                       pio.asStr(),
                       len(annos),
                       pio.doi.asStr() if pio.doi else '',
                       pio.title,  # title
                       len(authors),
                       authors_s,
                       isoformat(pio.created),
                       isoformat(pio.updated),
                       pio.hasVersions,
                       minad,
                       maxad,
                       ''
                       '',
                       '',
                       '',
                       )

    def with_minimum_number_of_annoations(self, limit):
        """ slug tails with more than the minimum number of annotations """
        return {k:v for k, v in self.slug_tails().items() if len(v) > limit}

    def annos_end(self, limit):
        """ number of annotations after thresholding """
        docs = self.with_minimum_number_of_annoations(limit)
        return [a for v in docs.values() for a in v]

    def counts(self, limit):
        """ columns """
        return (
            len(self.annos_start()),
            len(self.raw_uris()),
            len(self.normalized()),
            len(self.slug_tails()),
            len(self.with_minimum_number_of_annoations(limit)),
            len(self.annos_end(limit)),
        )

    @property
    def fs(self):
        return (
            self.annos_start,
            self.raw_uris,
            self.normalized,
            self.slug_tails,
            self.with_minimum_number_of_annoations,
            self.annos_end,
        )

    def column_header(self):
        return [f.__name__ for f in self.fs]

    def column_description(self):
        return [f.__doc__ for f in self.fs]
