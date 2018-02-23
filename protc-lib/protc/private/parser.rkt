#lang brag
; [pattern] is optional https://docs.racket-lang.org/brag/index.html?q=brag#%28part._brag-syntax%29

; @ -> a splice
; / -> a cut

protc-file : (expression)*  ; TODO at some point we may want to make use of protc-file for macro transformation... and thus not cut here...
@expression : section | section-lisp | atom | code-block  ; TODO no splice in future?
;comment : COMMENT  ; not exactly clear what to do with comments in a documentation language...

;code-open : CODE-OPEN
;code-close : CODE-CLOSE
testing : code-block ;code-open code-block code-close | code-open | code-close
;testing : symbol

@atom : literal | quote | identifier
@symbol : SYMBOL
@literal : number | string | esexp
@number : NUMBER  ; FIXME may need to not splice this
@string : STRING

identifier : bound-aspect | bound-being | <being> | symbol-or-modified  ; something that actually resolves to a thing
bound-aspect : symbol aspect 
bound-being : symbol <being>  ; for doing USES spec *my-being*
@symbol-or-modified : symbol | modified
modified : @symbol QUOTE

quote : /QUOTE (symbol | literal | section | esexp | sexp | quote)
racket-quote : /QUOTE (symbol | literal | sexp | racket-quote)
;aexp : AEXP ; OPEN-AEXP (sexp | symbol | literal) CLOSE
esexp : /OPEN-ESEXP (symbol | bound-aspect | literal | sexp | racket-quote)* /CLOSE  ; FIXME issues with greedyness...
;esexp : /OPEN-ESEXP symbol /CLOSE  ; FIXME issues with greedyness...
/sexp : /OPEN (symbol | bound-aspect | literal | sexp | racket-quote)* /CLOSE

section : section-type (aspect | <being>) [section-name] (class-message)* [/section-divider section-body] /END-SECTION
section-lisp : /OPEN section-type (aspect | <being>) [section-name] (class-message)* [/section-divider section-body] /CLOSE

section-type : symbol
class-message : @message
section-divider : /COMMA
section-body : (message)* | code-block ;code-open code-block code-close
code-block : CODE-BLOCK

@aspect : aspect-param> | <aspect-measure | aspect-symbolic
aspect-param> : ASPECT-PARAM
<aspect-measure : ASPECT-MEASURE
aspect-symbolic : ASPECT  ; NOTE implies invariant
<being> : BEING  ; reality is at the other end of the arrow, so here all we have is the name

section-name : symbol
;uses : SUBCLASSOF  ; TODO templates HRMMMMM .template?? VS allowing to use section-name as the section base...

message : message-type (message-argument)* [elipsis]

message-type : MESSAGE
message-argument : aspect-message | atom
aspect-message : identifier message-type (identifier | atom)

elipsis : /ELIPSIS

;sexp : OPEN (sexp | symbol | literal)* CLOSE
