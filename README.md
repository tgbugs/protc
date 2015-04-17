# Protc
A language for writing protocols.

## Description
Protc (prot see, pro tik, who cares) is a formal language for describing protocols,
scientific or otherwise. Specifically it is intended to provide a formal way to
record the inputs, outputs, and executors (human, computer, or otherwise) of a
protocol.


## Ramblings
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


## License
Obviously the language specification cannot be copyrighted and will be released
under some completely open license just to make sure.

For the implementation I personally would like to use some GPL but that could
curb adoption :(.
