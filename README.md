# Protc
A language for specifying protocols.

## Description
Protc (pro tik) is a formal language for specifying protocols,
scientific or otherwise. Its primary focus is on measurement
and the processes leading up to it. Protc is intended to provide
a formal way to record the specification and implementation of a protocol.

## WIP
Protc is a work in progress.
At the moment it is not ready for outside use.
Many parts of the language will change substantially.

## Development installation
Protc is built using [Racket](https://racket-lang.org/).
I suggest using either DrRacket or emacs with racket-mode for developing and writing Protc.
``` bash
raco pkg install protc*/

# building scribble docs requires
raco pkg install scribble-math
```

## Additional setup requirements
For some experimental features you will need [rkdf](https://github.com/tgbugs/rkdf)
and the [NIF-Ontology](https://github.com/SciCrunch/NIF-Ontology) repo.
```
git clone https://github.com/tgbugs/rkdf.git
git clone https://github.com/SciCrunch/NIF-Ontology.git
pushd NIF-Ontology
git checkout dev
../rkdf/bin/rkdf-convert-all
popd
raco pkg install NIF-Ontology/
```

## Licensing
This repository holds a number of different projects that have different licenses.
For the time being the Racket code is licensed under [AGLP-3](). Explicitly NOT 3+.
Please see individual subfolders for license information.
This licensing may be revisited in the future, such that the core Protc implementation
bears a more permissive license, while the Protc core libraries bear a copyleft license.

Scribble documentation is licensed under the license for the code in its containing
subtree or if copying only the text portions of the compiled docs then CC-BY-4.0.

Files in `thoughts/` are explicitly not licensed and all rights are reserved
(though various parts will eventually be incorporated into documents that
will be permissively licensed).

Language specifications cannot be copyrighted.
Just in case, when complete, the Protc spec will be released under CC0.
