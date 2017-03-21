#lang scribble/manual

@(require scribble-math)

@; @(require "test.rkt")

@title{Protc: A DSL for specifying protocols}

@author{Tom Gillespie}

Protc is... (from readme)

@section[#:tag "users"]{Users}

Who can use Protc? Anyone who wants to!

Who do we imagine will use Protc? We imagine that Protc will initially be used by programming literate scientists (theorists, modelers, data scientists) working in collaboration with experimentalists. This collaboration is imagined to happen prior to the publication of a paper or a data set. It would involve the programmer working directly with the experimentalist to formalize the inputs (tools, reagents, etc.) of their protocols, the outputs of any intermediate steps (e.g. a plasticized piece of brain tissue), the exact parameters they use, any restrictions that must be satisfied (e.g. time at temperature), and the quantities they are measuring (e.g a voltage).

@; In an ideal world Protc (or something similar) would be used by experimentalists during all stages of planning development, and execution of a scientific protocol. Ironically we are tackling communication between human beings first, instead of taking on the easier challenge of communicating effectively to ones own future self.

@section[#:tag "overview"]{Fundamental parts of a scientific protocol}

make (get, arrange)

measure

parameter (on inputs and on outputs, outputs also invariant/specification) (descriptive goals also fit here @racket[(objective* "looks like this")])

how

@section{Executor Semantics and Semantic Delegation}
@section{The evils and opportunities of "use"}
The word "use" occurs frequently in protocols for human consumption but it is a dead-word.
"Use" indicates that the following object should be turned into a verb. For example
"use superglue" and "superglue" are identical in meaning and presuppose that the executor
actually knows how to superglue. This is a problem since it hides the complexity that lurks
behind such statements which can sometimes unpack from @italic{"use superglue"} to @italic{"hold a razor in your dominant hand and use that razor held at an acute angle in the direction of motion to spread a drop of superglue (as produced by the viscosity of the superglue and the geometry of the container when you have cleared any blockage with a large bore hypodermic needle) in a very thin layer across the raised square in the middle of the mounting block, then place the chunk of brain stuck to the agarose (according to some other lengthy description, but really a picture would be better) on the block, and then make sure to wash the brain and the superglue gently with the cutting solution in order to get the superglue to set so that it doesn't float away and stick to the brain (WHICH CAN DAMAGE IT AND CAUSE YOU TO LOOSE A VALUABLE SAMPLE)"}.

That being said, 'use' provides us with a keyword that can implicitly verbify an input and indicate that the default executor (for this section) is expected to know how to carry out the described action (if no "how" is defined). In addition it can be used to automatically link or find other protocols that define a "how" on "use thing". This is one way to build a library of all the way one can "use" a tool.

@; @racket[(*-make* output inputs how)]
@; @racket[(*-arrange* output inputs how)]

@section[#:tag "grammar"]{Grammar}

@margin-note{@racket/form[top-level-form] covers all @racketmodfont{#lang} @racketmodname[racket] forms, see @link["https://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)"]{the Racket grammar docs}.}
@racketgrammar*[
#:literals (*-make* *-arrange* *-get* *measure *check objective* parameter* actualize lorder porder)
[statement symbol-symbol-statement being-being-statement being-symbol-statement symbol-being-statement how actualize-statement order-statement executor-spec]
[symbol-symbol-statement top-level-form] @; may change to general-top-level-form or expr
[being-being-statement get-statement make-statement arrange-statement]
[being-symbol-statement measure-statement check-statement]
[symbol-being-statement parameter-statement objective-statement] @; FIXME are parameters strictly symbol->being? yes in this case parameter* is
[get-statement (*-get* output params how)] @; implicit time input... FIXME correctness requires ...+
[make-statement (*-make* output inputs params how)]
[arrange-statement (*-arrange* output inputs params how)]
[measure-statement (*measure output-spec black-box-spec how)]
[check-statement (*check output-spec black-box-spec how)]
[parameter-statement (parameter* being aspect value)] @; FIXME this construction seems a bit off...
[objective-statement (objective* being datum-describing-or-depicting-a-black-box-state)]
[how order-statement being-being-statement delegated-statement movement-statement step-statement] @; FIXME these are mostly WHAT statements not HOW statements which require the executor semantics, FIXME does actualize-statement goes here?
[being input output]
[inputs input inputs] @; FIXME not quite right, go back and look at how to do one or more
[params symbol-being-statement params]
[actualize-statement (actualize symbol-being-statement how)] @; FIXME this should point at an identifier to a how statement? also really we just want the contract/type to be symbol-being-statement not the thing itself
[order-statement logical-order practical-order]
[logical-order (lorder statements)] @; TODO what should be the default assumption if no order is listed for how?
[practical-order (porder statements)]
]

@section{Asterisk convention}
When naming functions in Protc we need to distinguish between 5 types of functions.
@margin-note{Note that functions from symbol->being can't actually exist, some hefty semantics are implied here.
(In fact the semantics of making a symbolic representation reality are one of protc's long term goals.)}
@itemlist[
@item{Functions from being->being. Asterisks on the left and the right @racket[(*function* ...)].}
@item{Functions from being->symbol. Asterisks only on the left @racket[(*function ...)].}
@item{Functions from symbol->being. Asterisks only on the right @racket[(function* ...)].}
@item{Functions from symbol->symbol. No asterisks @racket[(function ...)].}
@item{Functions from (union being symbol)->being. An asterisk on the left follow by a dash and an asterisk right @racket[(*-function* ...)].}
]
@margin-note{Functions from symbol->symbol are lisp functions.
There are also higher-order functions from functions->functions
that will be treated as symbol->symbol for now.}
@margin-note{The @racket[*-f*] notation is provided as a convenience.
It is possible to avoid having to use @racket[*-f*] functions if you curry
the functions and use higher order functions to pass other functions around
(since we treat higher order functions as symbol->symbol despite the fact
that they have their own types).}

In theory (and perhaps in some future reality) these types could be implemented as real
function types using a type system. For the time being the underlying implementation will
use the asterisk conventions described above to denote the domain and range of functions/operations.

Examples and explanation for use.
@tabular[#:sep @hspace[2] #:style 'boxed #:row-properties '(bottom-border)
@(list
@(list @bold{*f*}  @racket[*contents*] "Use for functions that construct access black-boxes or for steps in a make chain that take no parameters (unusual but useful for expressing things quickly).")
@(list @bold{*f}   @racket[*measure] "Used for measure, or check functions.")
@(list @bold{f*}   @racket[parameter*] "Use for parameters or objectives, for numbers that lead directly to action in the world (e.g. there is another symbol in a dial on the machine that you need to use).")
@(list @bold{*-f*} @racket[*-make*] "Use for steps that create or acquire beings that also take symbolic parameters.")
)
]

@($$ "\\sum_{i=0}^n x_i^3") @; testing for formula rendering online... some weirdness

@section{Documentation}
@; i wonder if you can check these against the real code...
@defform[(*-get* output params how)]{
@racket[*-get*] reveals that we may want a way to parametrize some of these real world functions
at other times. For example we may want a generic @racket[*-get-by-rrid*] which would take a symbolic representation
and ultimately produce an aliquot of @racket[thing-with-specified-rrid].
}
@defform[(*-make* output inputs params how)]{ @; FIXME a major problem to consider here: do make 1 thing 100 times vs make 100 things one time -- make is NOT commutative with respect to the number of things (economies of scale etc..)
@racket[*-make*] denotes a transformative operation on the inputs, usually this
implies a transformation in which entropy increases.

The basic idea that drives the syntax for the current version of these forms
is the English construction "Make the named output from these inputs by executing this series of steps."
Breaking this down there is a 1:1 mapping as follows:

@tabular[#:sep @hspace[2]
@(list
@(list @bold{Protc} @bold{English})
@(list @racket[*-make*] "Make")
@(list @racket[output] "the named output")
@(list @racket[inputs] "from these inputs")
@(list @racket[how] "by executing this series of steps."))
]
}
@defform[(*-arrange* output inputs params how)]{
@racket[*-arrange*] denotes an operation that preserves the constituent parts, akin to assembling a rig.
There are some cases where some inputs are transformed and some are not, for example dissection
tools vs the subject being dissected. There is also the interesting case of resources being used
up to the point that you can run out.
}
@defform[(*measure output-spec black-box-spec how)]{
@racket[*measure] denotes an operation on a subset of reality (a black box) that produces one or more numbers. The specification of the black box can be as simple as an identifier referencing a being (i.e. a subset of reality that can be closed spatially when only considering its mass (as opposed to energy) interactions) (e.g. a @racket[mouse]).
}
@defform[(parameter* thing aspect value)]{
@racket[parameter*] denotes an operation that applies a symbolic value to something in the world. Validation of parameters either requires an accompanying @racket[*measure] or an accompanying process with proxy measures that can be shown to satisfy the parameter. Note that there are two different kinds of parameters depending on whether the quantity they place a restriction on is directly measurable. For example molarity is not directly measurable, however mass and volume can both be measured directly and converted to molarity. Parameters that are not actualized with @racket[*measure] are delegated. For example if a parameter is actualized by making the pointer on a knob point to the symbol matching the parameter value then that falls into a broad category of actualizations where another symbolic representation closes the loop. @; rewrite so that this makes more sense
}
@defform[(objective* datum-describing-or-depicting-a-black-box-state)]{ @; FIXME do we want delegating forms to require an executor to be listed as part of the arguments?
@racket[objective*] denotes an operation that applies a symbolic value to something in the world. @racket[objective*] is an executor dependent way of communicating a goal. While it is more flexible it requires semantic delegation to the executor. @racket[objective*] is the first part of an @racket[objective* *check] pair that closes a real world executor loop (observe the way the asterisks work out here).
}
@defform[(actualize param how)
         #:contracts ([how how?]
	              [param symbol-being?])]{
@racket[actualize] denotes an explicit link between a how section and the parameter or parameters that it satisfies. @; TODO in theory this should also provide a way to include/point to equations for equivalences (eg final molarity <-> the weights given the volume being made...) however this is a signifcantly more complex problem... making this section extensible is critical since no one will be able to write all their sanity check code at the same time...
}
@defform[(aspect thing name)]{
@racket[aspect] denotes a function that retrieves the value stored at the name for the thing. An aspect is a measurable quality of a black-box, however at any given point in a protc program the value at that name may be null. This is very similar to an object oriented system, for example python, where classes have properties. The difference here is that aspects need not have values and need not necessarily be defined beforehand. The aspect itself is just the name and thus other functions can set properties on that name, such as an invariant on some aspect of a thing (eg the mass of a mouse considered 'fat' in this study is an invariant function of age and their measured mass).
}
@defform[(lorder statements)]{
@racket[lorder] denotes the logical order of a sequences of steps. Logical order is the constraints on ordering of events imposed by the science or by reality. Said another way changing logical order will change the outcome of a series of steps. Statements may be any valid Protc statement.
}
@defform[(porder statements)]{
@racket[porder] denotes the practical order of a sequences of steps. Practical order is the order of events that allow the executor to most efficiently complete a sequence of steps. Said another way, changing practical order is not expected to change the outcome of a series of steps (unless doing so would mean that some invariant, usually temporal, is violated). Statements may be any valid Protc statement.
}


@; @table-of-contents[]

@; ------------------------------------

@; @include-section{"introduction.scrbl"}
@; @include-section{"etc.scrbl"}
