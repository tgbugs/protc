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


## License
Obviously the language specification cannot be copyrighted and will be released
under some completely open license just to make sure.

For the implementation I personally would like to use some GPL but that could
curb adoption :(.
