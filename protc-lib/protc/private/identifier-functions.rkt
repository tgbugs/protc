#lang racket/base

(require rdf/utils)
(provide (all-defined-out))

;;; identifier namespaces

; FIXME do we parameterize here or what?

(define-id-funcs
  [hyp "https://hyp.is/"]
  [hypa "https://hypothes.is/a/"]
  [hyp.api "https://hypothes.is/api/annotations/"]
  [DOI "https://doi.org/"]
  [PMID  "https://www.ncbi.nlm.nih.gov/pubmed/"]
  [researchdiets "http://uri.interlex.org/researchdiets/uris/productnumber/"]


  ;; rdf prefixes
  [xml "http://www.w3.org/XML/1998/namespace#"]
  [xsd "http://www.w3.org/2001/XMLSchema#"]
  [rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
       (type)]
  [rdfs "http://www.w3.org/2000/01/rdf-schema#"]
  [owl "http://www.w3.org/2002/07/owl#"]
  [prov "http://www.w3.org/ns/prov#"]

  [ilxtr "http://uri.interlex.org/tgbugs/uris/readable/"]

  [NIFSTD "http://uri.neuinfo.org/nif/nifstd/"]
  [BIRNLEX "http://uri.neuinfo.org/nif/nifstd/birnlex_"]
  [SAO "http://uri.neuinfo.org/nif/nifstd/sao"]

  [GO "http://purl.obolibrary.org/obo/GO_"]
  [UBERON "http://purl.obolibrary.org/obo/UBERON_"]
  [NCBITaxon "http://purl.obolibrary.org/obo/NCBITaxon_"]

  )

(module+ test
  (PMID: 1234567)
  (DOI: '10.1145/367177.367199)
  (hyp: 'yrnwoguZEeeIFfvQZd6Alg)
  (prov: "test")
  (rdfs: 'label)
  (ilxtr: 'protocol))
