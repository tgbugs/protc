# Protc
A language for specifying protocols.

Where does data come from?


## Description
Protc (prot see, pro tik, who cares) is a formal language for specifying protocols,
scientific or otherwise. Specifically it is intended to provide a formal way to
record the inputs, outputs, and executors (human, computer, or otherwise) of a
protocol. What is valuable is neither the fact nor the number, but how it came to
be known.

## Dependencies
A DSL for specifying _how_ to execute a series of steps. This is fundamentally what
we are about here. The problem with laboratory science is that the practicalities of
the execution of an expreiment, protocol, or process, are often entangled with the
logical, temporal, and scientific, dependencies that will exist regardless of the
execution environment. For example, if there are two experimental groups of mice that
receive a treament, say a sham and a real surgury, then logically it does not matter
what order those surguries were conducted in and we often assume in our interpretation
that it was random. However, in reality it is often NOT random because, for example,
there is a hard limit on how long some reagent is viable and it is expensive and needs
to be prepared and used all at once within a certain time limit and thus all of one
group is run first. Indeed there are many practicalities that are vital for producing
data in science. However, those practicalities should NOT be confused with the experiemnt.
Sometimes those practicalities mean that we must assume that they do not effect our results.
It would be nice if we could clearly identify when we make those simplifying assumptions
as we develop and refine a protocol.

Thus it would be nice to have a clean way to specify different levels of dependency.
A brief list of potential dependency types:
1. Logistical, I need these reagents/tools 
2. Temporal-Logical/-Experimental, these steps MUST be done in this order or
BAD THINGS WILL HAPPEN
3. Temporal-Practical/-Arbitrary, these are things that we assret don't actually
matter but are convenient or are a product of the fact that a single human was the executor
4. Data/Knowledge, I need to know this parameter so that I can calibrate/move to next step
5. Skill/Learning, I need to be able to execute this task with a certain level of skill

Note here that one VERY useful piece of information that we might want to help people collect
is what happens when certain steps are done out of order or in a different order. In non obvious
cases (an obvious case being , 'mice must have sex before you can do experiments on their offspring')
being able to add an annotation that says "we didn't have this dependency and a blackhole ate my cat"
would help future executors understand why certain dependencies are in point of fact not arbitrary.
Knowing that certain features of a protocol have not been thoroughly explored is incredibly valuable.

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

We cannot measure or record everything. We often do not know all the variables.
However, if there is a central repository of all measurement functions that have
been used on an entity then we can reveal the gap between what is measured and
what could have been measured. Preferably we would like to have those ranked by
how commonly they are measured but we should be able to rank them along a number
of axes. The reason this is important is becase we simply can't record every last
possible detail of an experiment, we need to record the things that we think are
important with some restrictions to make it clear what data we are actually using
along the way to guide our steps. Usually the things we measure end up being the
values that we need in order to calculate how much of something is needed in a
later step. For example the reason we measure od280 for dna is that that can give
us a quantitative measurement of the concentration which we need in the next step
where we have to have a certain concentration of DNA for a transfection to work.

These are the kinds of logical dependencies that need to be recorded and made
explicit even though there are other features that could be measured or measured
using other methods. In a sense this is why it is nice to work backward from the
end result: you might discover a new and better way to get the inputs that you need
in order to perform the next step.

I think the key thing here is that protocols can be specified at varying levels
of detail. Since many parts of a protocol may have a human executor as long as
the inputs and outputs match then that is a start. Filling in the details and
linking inputs and outputs along the way (and ultimately decomposing a nested
function into a series of sequential functions) can come later or be pulled in
from a community repository based on the inputs and outputs that are listed. I
think getting people to think rigorously about the inputs and the outputs of their
protocols would be a really solid first step, even if the exact details are not
initially filled in.

Automatic decoupling could be performed by looking to see whether an input variable
is ever used in conjunction with another input variable. If it is not then it will
be possible to split the inputs into multiple functions that are independent.


## Practical considerations for readability and execution
Nothing but the interpreter. If at all possible it would be great to have the code
itself be human readable. Translation to other representations such as a step by
step list or a graph *might* be possible, but we'll see. Having looked over some
of the bpmn stuff I think I can safely say that providing syntax highlighting and
making it trivial to read and understand the raw code could be a much more productive
way to address the problem of human readability. On the other hand if we are going
to have data specialists then we can just expect them to be able to understand code.
In that context the most important thing is the ability to translate a protocol into
two different things: 1) something that can be executed by a human being or set of
human beings, 2) something that can be executed by a macine or set of machines. 3)
something that can be read and executed by a combination thereof (easy when you
break each section into parts and label it with an executor). I imagine some strange
world where the human beings are doing the addition and subtraction and the computers
are lifting heavy objects.


## There are no facts there are only measurements (re: Rene Descartes)
There are no facts. There are only measurements and assertions. Assertions and
assignments will be flagged as such. For example, suppose you want to specify a
protocol for building a car, the last step does not produce a car, the last step
produces a boolean value that evaluates to true if the putative car passes the
criteria (WHICH YOU SPECIFY and could be "quacks like a duck" -> True). This
prevents all sorts of madness and forces explicit documentation of these critria
which can the themselves be evaluated and improved. This will be REQUIRED because
in order to enter that value into the digital realm there must be a protocol for
how it is done and some resource must be specified. An under specificed classification
will stick out like a sore thumb. This is much more useful for computer systems
because computers don't know anything about the world, but if we give them the
criteria and we can see it clearly doccumented then we know what they are talking
about and can assess that. This is criticial for communication in science and
cannot be overlooked. Ontologies by their fundamental nature conflate being with
assertion. This is why the stumble so often. A language for science must enshrine
the completely circumspect nature of assertions and facts by forcing them into
the open and clearly labeling them. Sure, you can write a constant into a file
and assert "pi = 3" and try to get people to agree with you but you will likely
fail. The important thing is building a structure so that people can have the
conversation. Now, if you can specify the protocol you used to come to the
conclusion that "pi = 3" it is infinitely better because then the community and
the asserter can go back and look and determine that there was indeed a rather
nasty rounding bug in the code that was used rather than devolving into arguments
about how stupid the Indiana legislature is.

Facts do not and cannot have being. Processes can have being. So if you can
communicate how to do a process then you have much better assurance that the
result of the process is indeed about reality and not simply a hallucination.
How do you learn something? -> Read it on the internet -> How do you know the
internet is right? -> Look it up in a book -> How do you know the book is right?
-> Check with another book. -> How about that book? -> Try to reproduce their
methods IF there were methods -> Did they even specify their criteria for right?
Other options are: believe, or proceed as if it were true and see what happens.
Note that option 2 does not tell you that they were right only that if the
assertion actually applies to your case that they were right. It may not apply.
And you won't be able to find out.

At the end of the day clear doccumentation of how something was determined or
measured is far more convincing and practically useful than citation, assertion,
or even data alone.


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

Leaving scope as a way to deal with Beings that aren't needed in a protocol anymore.
Also the balance between doccumenting EVERYTHING and doccumenting what is actually
relevant for science. I mean yes, performing manual memory managment on every single
mouse carcass is ultimately needed, but surely we can do better? We do have to start
out by writing down ALL the inputs and ALL the outputs and then seeing whether
there are some that we can drop or find elegant ways to deal with so that the
only thing that needs to be written down are the relevant (heh) i/o. Of course
this presuposes that we can actually determine which elements are 'relevant.'
Using Persist types and assuming implicity garbage cans and freezers for reagents
or consumables does seem like it might be a viable.


##Alpha and Omega (generation, destruction, and friends)
Protc needs to deal very thoroughly with the coming into being and ceasing to be
of Beings. Everything in between is up to the user. We need to make it transparent
for users to deal with the complexities of generation and destruction and procurement
without getting sloppy in our accounting. It may be worth consider all the trasitions
from n=>m for various types of begets functions and figure out what we need to consider.


## Denoting Modifiction
How do we deal with modified beings? Eg a "human with stitches" or a "mouse with
a subcutaneous bladder perssure meter" or a "subject who received treatment a."
I think people have probably worked out the problem but within the language I
think we will need some ability to compose types, not for inference but simply
so that we do not force users to write them themselves, though, for outputs we
would clearly need some nice notation to indicate that a modification has occured.
Maybe a simple ```<human, substance_a>``` type could suffice basically you have
the primary entity and simply use the being that modified it or that was consumed
by it was a tag? Maybe it needs to be a tripple with ```<human, protocol, substance>```
that is what actually gets returned as a result of a consumption step. For example
an eat radiolabled cherrios protocol would bind have an ```eat(child, cherrios) ->
child: destructor(cherrios) child``` but if eat was a consumption function then
would during evlaution (not reasoning about the generation or consumption) simply
return itself as a type for future inputs and reasoning until it was unpacked and
the child was all that was left after any functions that cared about the child
being radiolabled had been executed (including the recording of that fact in a
database somehwere or something).


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
