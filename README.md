# Protc
A language for specifying protocols.


## Description
Protc (prot see, pro tik, who cares) is a formal language for specifying protocols,
scientific or otherwise. Specifically it is intended to provide a formal way to
record the inputs, outputs, and executors (human, computer, or otherwise) of a
protocol.


## Ramblings (need to distill what goes in the lang and what goes elsewhere)
The general aim is for the language to be functional. This is particularly important
since Protc is supposed to serve as a sort of formal documentation language since in
many cases the execution of a protocol by a human executor exists outside the digital
realm. Thus, Protc needs to support the ability to generate human readable doccuments
and the ability to produce executable code for computers. Furthermore, it needs to
support highly asynchoronous execution. Since Protc is an attempt to formalize the
execution of ANY protocol it inherantly exists at an extremely high level, since many
of the protocols that it should be able to describe will never be run on a computer.

To this end Protc will need to be able to call as many other programs and call on as
many other languages as possible since in the end the hope is to use it as a way to
automate execution of arbitrary protocols. In a sense Protc ends up filling the role
of a scripting language, but sort of the scripting language to end all scripting
since its objective is to describe and document everything, in the digital and the
real world. It also needs to be considerably more formal than most scripting languages.
This formalness comes from Protc type system which will be used to cope with the
fact that many of the types that go in and out of functions don't exist in the computer.

I am also leaning toward using 'resources' to deal with monadic operations so that
it will always be VERY clear when and how types pass data from reality into the
digitial world. In a sense the universe type checks itself so most of the time we
don't have to worry that something has gone wrong (though the need for Protc does
suggest that there are issues that appear in realms beyond errors in data entry).

Since Protc aims to be both a documentation language and a programming language
we will need to think very hard about naming of functions and naming in general.
To this end we will also need to natively (stdlib style) support direct linking
of identifiers or terminologies to code objects. In the same vein the syntax of
the language will place human readability before all else (follow Python here).

Should functions that categorize things take the categories or classes as inputs?
I think they should, they need to be defined externally or better yet mapped to
and information system. I guess those would count as Values.

Can we support imperative constructs inside functions by only allowing variables
to be accessed from within their defined namespace/scope? No global variables
and no accessing variables not defined within scope. Example: could use a for
loop but could not do anything with the counter in another function unless that
variable was passed to that function. Read up on this (recursion is harder to
reason about, including our target audience, and myself).

Compiled vs interpreted? Well on the one hand it needs to be able to produce
protocols, so it needs to compile to text. On the other hand there are a huge
number of dependencies and links that need to be resolved when a protocol is
executed, including type inference from measurement quantities specified to
the tools that can measure them (how and where this functionality fits into the
language needs serious consideration). Consider a measurement step: a downgraph
step requires a number, for example, 10uL of H2O. We can infer from this that
the tool that is needed is a p20 and some pipette tips. 10uL of water could be
specified in the protocol and we will need to treat it as an input to some mixing
step. We will then infer from that input a pereviously defined measurement function
that takes some arbitrary amount of water, a p20, and some pipette tips and an
executor and can produce between 2 and 20 (? we would look up the limits) uL of
water. Thinking about how to make steps more composable for standard libs and
what the requirements on the input and output types is going to look like is
going to require some very specific examples and then attempts to apply the system
elsewhere. This may also come later.


## Conservation of mass (or how to prevent a whole bunch of typing)
Somehow this reminds me that we *may* need to do something about conservation of
energy and mass. Specifically it is going to be really akward if the p20 enterns
and never leaves that function. Maybe we can come up with a way to avoid having to
explicitily distinguish between consumables and other inputs? Tracking consumables
is also a very important feature for inventory control and compliance (eg did you
dispose of that mouse correctly? do you even have a protocol for that?). When we can
infer something about a change in the world we should, so if we know that we are
measuring 10uL of water and the observer (hrm, maybe that is what we should call
the computer that oversees the execution, assistant?) has been given access to
inventory records and can identify that water bottle Z is being used, then the
digital record for Z should be updated to reflect that change. If there is no
digitization of a being then we can just ignore it.

More on conservation of mass. One problem that we have is distinguishing between
the outputs of a function that are GENERATED vs the outputs that merely persist.
We can solve this problem by actually comparing inputs and outputs and simply
taking I and not O as consumed inputs I and O as persistent inputs, and O and
not I as generated outputs. Furthermore we can postulate that the consumed inputs
should for the most part have contributed to the generation of the new outputs.
One edge case to consider is maintainence where you have a single output and all
other inputs are consumed. Clearly unless the individual is rapidly gaining weight
there needs to be some representation that accounts for the fact that biology systems
are continually executing a protocol that takes chemical energy stored in matter and
converts it into energy which is a generated output of that protocol which then
leaves the system. Not sure we *really* want to model all of this. It would be nice
if we could just have the novel beings pop out of the function but that would seem
to violate our desire for no external state. Well, technically using a tool in
an experiment *does* modify the tool since it is a tool that has now been used
n + 1 times, which is useful for maintainance protocols. Hrm, yes, when you do a
dissection you don't get tools back from the dissection step, you get dirty tools
back! You must do something with the dirty tools! Clean them! Put them away then
you have no more tools. Yes, we need the opposite of procurement protocol. A
disposal or "done with" protocol. This is how we can create functions that have
a single return for generation purposes. We can then use the 'autodispose' type
for persistent tools as a shorthand so that people don't have to include their
cleanup procedure if they don't want to (for example). We could also infer this
automatically for functions called inside functions that return an object which
is not returned by the enclosing function.


##Alpha and Omega (generation, destruction, and friends)
Protc needs to deal very thoroughly with the coming into being and ceasing to be
of Beings. Everything in between is up to the user. We need to make it transparent
for users to deal with the complexities of generation and destruction and procurement
without getting sloppy in our accounting. It may be worth consider all the trasitions
from n=>m for various types of begets functions and figure out what we need to consider.


## Asynchrony
One of the most important aspects to consider is that some protocols descrbied in
Protc could take years to execute but might nevertheless need to be collecting data
on a daily basis. Composition of Protc functions and files should be able to manage
this without a user having to load up a specific script every time it was needed.
The execution of the protocol will thus need to be portable across different observers.
It might work out that using the 'resources' model for all digital io could make this
easier because any Protcol (Protcall? heh) would be essentially stateless and could
describe its own procedure for reloading from a long down time ('wake me when you
need me').


## Granulatiry and generalizability (composition elsewhere)
Clearly a language for describing protocols can be used to say "here is exactly
what I did" as well as to say "here is a general way to do something" in a sense
you could infer how general a result was based on how general the set of protocols
were that produced that result. Ideally we would strive for functions that were
as general as possible and could produce the same result. In fact in some domains,
for example manufacturing there might be two completely different ways to arrive
at the same result (eg a metal figure 3d printed vs subtractive sculpting). Having
a formal representation of the multiple ways to produce the same outputs is
incredibly valuable for weighing options and comparing those approaches. The level
of granularity probably will come from how exact someone is in describing their
protocol. We *should* be able to generalize a protocol based on the type, this
would need to be derived from the ontology. This seems to be deeply related to how
we will ultimately classify or categorize different protocols.


## Stability of mappings to information systems
If we are going to do type inference on the types from ontologies or vocabularies
then we are going to have to figure out how to provide a stable reference for
users who write programs that assume certain things about the type hierarchy.
Changes to the ontology could break type inference for some protocols! That would
be really really bad. I think this means that we have to approach the development
of the ontologies and their hierarchy very, very carefully OR we have to let users
maintain their own type hierarchy either in code or in some other system so that
they don't get destroyed by changes.


## License
Obviously the language specification cannot be copyrighted and will be released
under some completely open license just to make sure.

For the implementation I personally would like to use some GPL but that could
curb adoption :(.
