# Protc
A language for specifying protocols.

Where does data come from?

##
Measurement the sole
arbiter of truth in
this and all worlds.

To measure is to see.
To see all that was not measured is to know.
(unfortunately this part has to be left to the information system)


## Description
Protc (prot see, pro tik, who cares) is a formal language for specifying protocols,
scientific or otherwise. Specifically it is intended to provide a formal way to
record the inputs, outputs, and executors (human, computer, or otherwise) of a
protocol. What is valuable is neither the fact nor the number, but how it came to
be known.


## Dependencies
A DSL for specifying _how_ to execute a series of steps. [SO. Turns out that what we
probably want here is actually a DSL for specifying _WHAT_ to do that makes it easier
to later express the _how_ by giving good constraints on inputs and outputs that the
_how_ implementation can follow. In fact, the _how_ obscures the logical process which
the following sentences say is what we really want to capture. Further thoughts here.
The key should be to make expressing BOTH what an how possible and make it easy to
explicity state when the _how_ is thought to matter for the outcome.] This is fundamentally
what we are about here. The problem with laboratory science is that the practicalities of
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


## Yogurt example
To demonstrate a very practical example, one of my favorite quick meals is greek
yogurt with honey, granola, and blueberries. In principle this is a very simple
concoction, however, the order in which the ingredients are added and mixed makes
and enormous difference in the outcome or vastly increases the difficulty of producing
the desired outcome. What is a concise way to document the ingredients, the order,
the actions, and the rationale (smushed blueberries)? Consider also that this does
not even touch the process of measurement and data generation, only of concisely and
correctly communicating a recipe using something other than the standard cookbook style.


## Who or what can execute this code?
What is an executor? An executor is anything that can interpert and execute a subset
of a program (or representation there of) written in protc and produce outputs that match the specified outputs
and any other criteria or restrictions on execution (e.g. time limits).
The key here though is that the executor shall be explicit in protc rather than something
that exists outside the language. In a sense this is rather like the shebang #! of a shell
script (it cannot escape the fact that if the a protc program is to be run on a computer
we do need a valid interpreter or compiler). Since protc programs should be executable
by anything capable of interpreting them one key feature of protc is that it must have
and extensive collection of 'hello world' style setups that make it possible to gurantee
sane execution of a block of code that has been annotated to be runnable by some executor.
One could think of these like ebuilds for anything, or a replacement for autotools/make.
A code block that should be executable by an english speaking human being should simply
print the code itself and show it to the human. Any values returned by that block come in
as interrupts from the RealWorld. Note here that when I say code block I mean function
because (unless the human has a device that can manually flip bits) blocks of code that
are to be interpreted by a human being cannot possibly modify global state of the program
and are thus inhereantly functional (the RealWorld monad). Here is some potential syntax:
```
#!/usr/bin/env protc   # this kills the repl ;_; some day we won't need the sheban
[executor 20year] ; the first line of every protc file should list the expected executor and runtime upper bound
;(RealWorld human yogurt honey blueberries granola bowl spoon) ; FIXME prefer defreal?
(defreal human yogurt honey blueberries granola bowl spoon) ; this dumps names into our namespace without assertions
(defreal something-not-specced-by-identifier-is-fine-too)
(map-identifiers
  (id1 id2 id3 id4 i5 i6 i7) ; or should this be (id1 human) (id2 yogur) etc?
  ; note here that to make this lang useful we need to infer 'fridge' for yogurt
  ; and spoon making process for the spoon, etc. However, this will be handled by
  ; the identifier system, and anything referenced by identifier here should have
  ; relevant type information imported if that type has an accompanying protc deftype
  ; that deftype is where ALL the possible measurements would probably live for reals
  (human yogurt honey blueberries granola bowl spoon))
(executor human (RealWorld))   ; need to look up how to do RealWorld monad
(executor python3 ("/usr/bin/env python3"))  ; insufficiently general, also, shells... :/
(executor sh (defun sh_implemented_using_lisp_right_here_in_this_file)) ; you could do it if you were a madman
[human (interval 1 20)year](defun learn_english (executor))

[(learn_english human) 20min](defmon RTFM (executor)
  ("This is the manual that you should read. It is plain
   english and there is nothing else that needs to happen in here.
   Hell, with a reasonable type system for the [executor, duration]
   capture we wouldn't even need to have the qutoes because they would
   be implied by the type of the executor??"))

[python3 2sec](defmon SomePythonCode (executor)
  ; when we pass executor we assert that anything inside is valid for that executor
  (import antigravity)
  (read-executor some_pipe_connected_to_the_running_program)) ; in this case there would be no return
  ; NOTE: we REALLY do not want to try to reinvent pipes here stdout/stdin linking should be easy
  ; ON THE OTHER HAND if you need stdout/stdin linking WTF are you using protc for!? just write some sh

[sh 2sec](defmon why_not_bash_question_mark (executor) ; maybe we drop the (executor) here since we have defmon?
  (cat /tmp/my_test_file | grep that\ was\ easy | sed 's/^.+$/dont try to recreate the wheel kids!/')
  (read-executor)) ; read-executor should default to read from stdout of the executor and should be implict if not specced

[human 1sec](defmon press-a-key (executor)
  ("Yo dude, hit a key on your keyboard! This is your only job.")
  (read-char))

[human 1min](defmon mix (executor thing-to-mix-with &rest things-to-be-mixed)
  "this is an underspecified mix function for a computer but human's got it!"
  (use thing-to-mix-with to mix/stir/beat/whisk/agitate things-to-be-mixed)
  (deftype (thing-to-mix-with 'things-to-be-mixed))) ; OOOOH does this return back into the interpreter or does it return the output of the mixing!?
  ; FIXME since this is a language for documentation I think we really do need to have the output be the actual product
  ; a nice default of course would be to simply return the verbed noun or simply (verb noun) as a type
  ; THE USER SHOULD NOT HAVE TO INTERACT TO GO TO THE NEXT STEP UNLESS THEY ARE TO INPUT A VALUE

[human 10sec](defmon put (executor thing-to-put thing-to-put-in)
  (put/place thing-to-put in thing-to-put-in)
  (deftype (thing-to-put thing-to-put-in))) ; does this even work!? NOTE the last value is what is actually returned

[human 3min](defmon make-delicious-yogurt (executor yogurt honey granola blueberries bowl spoon)
  "Notice that all of these things exist at the type level and the specificty is largely determined
  by the identifier system, so the more general the protocol the more general the identifiers should be.
  We do not handle this within protc"
  (put yogurt bowl) ; TODO how do sequential... iirc lisp actually has a nice way to do this with (put honey (put yogurt bowl))?
  (create indentation-in-yogurt) ; could use create as a way to create local names for use later in a sequence for human interp
  (let indentation-in-yogurt) ; pretty sure let actually does something like creating a new name, but seems a bit off
  ; create could be used as a logical place holder until a (defmon make-indentation (tool object)) was written
  ; TODO I really need to review lisp naming/scope because I think I would need 
  (put honey indentation-in-yogurt)
  (mix executor spoon things-in-bowl) ; FIXME I want executor to be implicit for single executor functions, multi executor notation will be more complicated :/
  (put granola bowl)
  (mix spoon things-in-bowl)
  (put blueberries bowl)
  (mix-carefully spoon things-in-bowl)
  (return-from make-delicious-yogurt delicious-yogurt)) ; last s-exp is returned and possibly should be ided using map-identifiers

```
ARGH, yes we have 'colored' functions, fortunately as long as the executor matches we should be ok.
Multiexecutor functions/defmons are going to have to accept a list object as the first argument.
Since we fully specify the execution environment and all executor defined functions cannot
-touch- access global state (unless they twiddle bits in memory directly) functions run by
different executors are entirely isolated from eachother and may be freely composed at the
level of their inputs and their outputs. Since they are monadic functions in protc (WITH
specified return values!!!) we need to specify how we will handle linking return values.
The stdlib will effectively be the implementation of various executors and doing the linkings.
The simplest version of course is to convert everything to human and tell them to "run this
code in bash" (for example). Having played with this a bit I'm not entirely sure we even
need the capture syntax for this, we just need a way to... wait, no, we need to be able to
write down the inputs and the outputs and make RealWorld things explict inputs to functions.
Therefore we _must_ include the human as an input when we _call_ the function, that is why the
(executor) is there, DUH, do you even lisp bro?

This needs a bunch of work because we need to be able to have return values as well
from functions that are executed by humans. If numbers are the output of a process that a
human executes we would like to be able to get them and advance the state of the interpreter.
Yes, all RealWorld monad transations are blocking on that thread. Do we need `executor`
to be a reserved keyword for referencing the first element of the capture, or should we
go all in on the types of the executor and allow ANYTHING inside the parens? No, we need a
sane way to name things even if the function is going to be executed by something other than
a computer (and there is no input). Another way to think of these things are as named comments
or blocks of code that are conditionally commented out based on whether the current executor
matches the one specified by the capture. No, that is not quite right. It is conditional on
whether the specified executor is know to the interpreter and has a hello world that can be
loaded in the computer. Unspecified blocks can simply be treated as regular old s expressions
and default to the interpreter itself.


## Function notation and execution order vs unordered applications
`(college (highschool human))` works fine for most cases because there is an unwritten rule that
you must complete highschool before going to college, expressed as the idea that
`(defrw (college (highschool human)))`. However, we often don't care about the order in which
certain things were done to an object. Now, to be fair, many of these cases are rare,
especially in science where (heat (mix thing)) is in no way equivalent to (mix (heat thing)).
The issue here is one of how we communicate what features of the ordering are in fact relevant
at the time of declaration -- NOT at the time of evaluation. It would be trivial to implement
that we don't care about the order of application by writing
`(defrw (college (unordered (highschool (middleschool human)))))` but then we start inserting
`(unordered <expression>)` all over the place which is no fun at all. So we want a way to assert
that only the application but not the order matters while also preserving function notation so
that we can communicate the order in which things _were actually done_. A good example of this
would be that we need a mouse that has had two types of injections `(inj-a (inj-b mouse))` but
we assert that the order does not matter. BUT we also want a way to record what was actually done
so that after the fact we can go back and actually check whether the order mattered.

Maybe the correct way to do things here is to borrow a bit from the ontology world and use an
additional assertion system that adds a note in the type checker? For example:
`(type-assertion (order-equiv (inj-b (inj-a mouse)) (inj-a (inj-b mouse))))` or something.
Obviously pitfalls around the diversity within `mouse` and the need to enable people to record
what mice we actually have data on and what we can measure/know about mice that might covary
with any differences, but that is a different issue. Another though is how to handle assertions
that regardless of the being being functioned whether such an assertion should hold, eg
`(type-assertion (order-equiv (inj-b (inj-a human)) (inj-a (inj-b human))))` might actually
turn out to be false (note here to include the ability to make it easy to collect all assertions
so that you can see them and see whether they are true or untested). However in some other case
we might like to be able to say `(type-assertion (order-equiv rw-func-1 rw-func-2))` which would
mean that anywhere those two functions were called in either sequence the output types should
be considered equivalent. I wish there was an easier way to embed this information in the protocol
but written documents are linear, and maybe it is ok to put the assertions after the fact.

Unfortunately this does not get around the problem that from a human readability standpoint
(a (b (c d))) looks like things should in fact be done a b c  in temporal order. Alternate
way to articulate this might be `(apply-any-order '(a b c) d)`, the question is what the
resulting object would look like and whether we could infer equivalences between them
without having to declare them directly. This would mean that the apply-any-order call
would have to modify global state behind the scenes to update the type information so
that `(eq (a (b (c d))) (b (c (a d)))) -> #t`. This solution also doesn't resolve the
issue of whether they are equivalent only under application to d or whether they are
equivalent under any application. Nice extension of the apply-all is that it could
provide a consistent way to talk about orders, eg
`(apply-exec <executor> (practical-ordering a b c) d)` alternate syntax where apply
function is defined per executor spec:
`[<executor>](apply-all (practical-ordering a b c) d)`

Note that this is not an issue when dealing with concurrent/parallel functions that operate on
discrete subsets of the universal state. For example `(verb1 (verb2 global-state))` where
verb1 and verb2 act on different parts of the global state are NOT good ways to write
protocols because they obscure function I/O. One complaint of course is that verb1 and verb2
must be done sequentially by a single operator, but that is down to the executor spec
not the nature of the functions. As a side note we will however need some notation to
indicate that two functions can be run simultaneously by default with a 'preferred order'
that can be specified into the scheduler, otherwise the order will be more or less aribrary.


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

Sometimes you just want to be able to specify that you need/want a thing
that meets some specific criteria. For example, I don't care what antibody
you use, but it needs to 1) bind the thing 2) not bind other stuff 3) not
have been raised in the target organism, and here is the method to validate
those requirements. THEN as a proxy, a reference to a thing that has perviously
been validated according to those criteria can be used but we might want some
way to point to the evidence either as a paper or as the output of the prior
protocol. (This seems to fit with the 'how do you know you already have the
reagents you need' problem)


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


## Use cases
Landon mentions how annoying it is when dealing with various drugs and not
knowing what solvent to use, or people just not reporting it. This is a
great use case since it is a set of simple things where the verb 'mix' is
very consistent the inpubs ought to be in chebi, and it would make people's
lives much easier. Could potentially add a restriction or comment that the
output was intended for injection into mice, interesting question of how to
handle that kind of information since it is not strictly relevant to the
protocol itself but is a piece of the larger context that might be useful
when choosing between two protocols.

Russ Poldrack doi:10.1016/j.neuroimage.2007.11.048 for a list of things that
the fMRI folks might want to be able to model.


## Play
One of the very powerful things about modern programming environments is that
they allow users to play around and test whether something works as expected.
It also enables them to do it in a very tight loop and iterate quickly.
One of the features of the implementation is that it needs to support some
level of simulation beyond syntax checking an input/output consistency checking.
Adding the ability to natively specify a function/parameters for the range of the
expected results (or infer it directly based on the unit of the measure) could
help here. The ability to add implementation to steps transparently without
actually bumping the version... also seems like it helps here, since a (mix a b)
step can be pretty darned generic. I wonder how this interacts with being able
to use first class functions.


## Mutually exclusive measurements
When making measurements on the universe there are certain measurements which
are suspected to be mutually exclusive (eg position/momentum of electrons).
Others are practically mutually exclusive. More precisely, there are some
measurement implementations that are mutually exclusive, there might be a way
to measure more than one thing at a time using a different (perhaps unknown)
implementation. I don't think this goes in protc, but it fits within the larger
information system that needs to enumerate known/possible measurements on defined
subsets of the universe's state.


## Black boxes
Protc needs to support the ability to flag methods as 'black boxes' to facilitate
communication about the fact that at this level of abstraction a measurement
method which has a more detailed explaination is being treated as if it magically
produces numbers of certain units. The syntax for this needs to make it trivial
to unbox the black box when the implementation is added (ie without having to
reindent or anything stupid like that). This example come from reading with wiki
page on thermocouples which measure temperature based on the principle of the
thermoelectric effect which requires quite a bit more explaination than 'this
machine produces numbers with units of degrees celcius.' Black box annotations
(like all measurement methods?) should probably require units so that inference
can be made about the types of the underlying being measurements. Note: we really
need to avoid the black box being 'there is a device driver that puts bits into
this register that correspond to the number Z.' Black box methods could function
similarly to headers, where they specify the inputs and outputs of experiments
with out specifying the exact implementation so that it is possible to cite at
the black box level as well as the implementation level. This is a bit weird
since black box methods are also like types, but also provide natural levels of
encapuslation for steps and the like because they are guranteed functional.


## Semantics for when nouns can verb (RE: NEVER)
One interesting possibility for supporting more... normal? language like
representations is to allow people to use 'nouns' as verbs. However what
this means is that you have a strange duality and require a modifier to be
attached so that verb forms of a thing can be passed to a function as a verb
instead of as the default noun (this will only be needed if you are trying to
pass a verb as an argument to a function so that you can modify it. For example
```
(acquire knife) ; bind knife as a verb in the local namespace, def left blank
(knife bob) ; this kills the bob
(give knife bob) ; no error noun form
((quickly knife) bob) ; ERROR because quickly modifies the verb form
((quickly #V'knife) bob) ; pass the verb form using #V' sugar for (verbify noun)
```
This kind of sucks though. Consider shovel. Please use dig, or hit, or other
explicit verbs, it increases clarity. This confuses people in English, it seems
clever and therefore is axed because it hides very important semantic differences.


## License
Obviously the language specification cannot be copyrighted and will be released
under some completely open license just to make sure.

For the implementation I personally would like to use some GPL but that could
curb adoption :(.
