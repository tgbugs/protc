#lang scribble/manual

@(require scribble-math)

@; @(require "test.rkt")

@title{Protc: A DSL for specifying protocols}

@author{Tom Gillespie}

Protc is... (from readme)

@section[#:tag "overview"]{Fundamental parts of a scientific protocol}

@; @racket[(*make* output inputs how)]
@; @racket[(*arrange* output inputs how)]

@racketgrammar*[
#:literals (*make* *arrange* *get* *measure parameter* lorder porder)
[statement get-statement make-statement arrange-statement measure-statement parameter-statement order-statement]
[get-statement (*get* output how)] @; implicit time input...
[make-statement (*make* output inputs how)]
[arrange-statement (*arrange* output inputs how)]
[measure-statement (*measure output-spec black-box-spec how)]
[how paramater-statement movement-statement step-statement]
[parameter-statement (parameter* thing aspect value)] @; FIXME this construction seems a bit off...
[order-statement logical-order practical-order]
[logical-order (lorder statements)] @; TODO what should be the default assumption if no order is listed for how?
[practical-order (porder statements)]
]

@section{Asterisk convention}
When naming functions in Protc we need to distinguish between 4 types of functions.
@margin-note{Note that functions from symbol->being can't actually exist, some additional semantics are implied here.}
@itemlist[
@item{Functions from being->being. Asterisks on the left and the right @racket[(*function* ...)].}
@item{Functions from being->symbol. Asterisks only on the left @racket[(*function ...)].}
@item{Functions from symbol->being. Asterisks only on the right @racket[(function* ...)].}
@item{Functions from symbol->symbol. No asterisks @racket[(function ...)].}
]
@margin-note{Functions from symbol->symbol are lisp functions.
There are also higher-order functions from functions->functions
that will be treated as symbol->symbol for now.}

In theory, and perhaps in some future reality these types could be implemented as real
function types using a type system. For the time being the underlying implementation will
use the asterisk conventions described above to denote the domain and range of functions/operations.

@($$ "\\sum_{i=0}^n x_i^3")

@section{Documentation}
@; i wonder if you can check these against the real code...
@defform[(*get* output how)]{
}
@defform[(*make* output inputs how)]{
}
@defform[(*arrange* output inputs how)]{
}
@defform[(*measure output-spec black-box-spec how)]{
}
@defform[(parameter* thing aspect value)]{
}
@defform[(lorder statements)]{
}
@defform[(porder statements)]{
}


@; @table-of-contents[]

@; ------------------------------------

@; @include-section{"introduction.scrbl"}
@; @include-section{"etc.scrbl"}
