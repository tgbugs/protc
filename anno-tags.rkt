;; annotation-*: tags for immutable editing of annotations
(tag-doc 'annotation-correction '(annotation-*:)
         "replace both the text and the tags of the parent annotation")

(tag-doc 'annotation-text:text '(annotation-*:)
         "text of the reply replaces the text of the parent annotation, semantics are left to the parser")
(tag-doc 'annotation-text:exact '(annotation-*:)
         "text of the reply replaces the exact of the parent annotation, usually better to use annotation-text:value unambigiously since it preserves the original exact annotation on the node for comparison")
(tag-doc 'annotation-text:value '(annotation-*:)
         "text of the reply replaces the text of the parent annotation and is the value of the resulting node")
(tag-doc 'annotation-text:children '(annotation-*:)
         "text of the reply should be a list of hyp.is links and will be used to populate the children of the current node")

(tag-doc 'annotation-tags:replace '(annotation-*:)
         "tags besides this one will replace those in the parent annotation (skips PROTCUR: and other annotation-*: tags)")
(tag-doc 'annotation-tags:add '(annotation-*:)
         "tags besides this one will be added to those in the parent annotation")
(tag-doc 'annotation-tags:delete '(annotation-*:)  ; deprecate?
         "tags besides this one will be removed from the parent annotation")
(tag-doc 'annotation-children:delete '(annotation-*:)
         "child annotations from the parent annotation will be ignored")
