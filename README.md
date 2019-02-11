# ulf2english
Maps ULFs to English sentences.

## Dependencies
- Quicklisp
- TTT (get a copy at http://cs.rochester.edu/u/gkim21/data/ttt.tar.gz)
- https://github.com/genelkim/ulf-lib
- https://github.com/genelkim/cl-util
- cl-strings (loaded automatically via quicklisp)
- lisp-unit (loaded automatically via quicklisp)

## Installation
1. Install quicklisp by following instructions at https://www.quicklisp.org/beta/
2. Then place the other depenedencies listed above in a folder accessible to Quicklisp or ASDF (which underlies quicklisp).  How to do this in a couple ways is described by the following Stack Overflow answer https://stackoverflow.com/a/11265601.

## Running the Code
This is really meant to be a library, but to check the basic functionality of any of the functions, you can load the file load.lisp and enter the package :ulf2english.  For example,
```
$ acl
$ (load "load.lisp")
$ ...[loading messages]...
$ (in-package :ulf2english)
$ (ulf2english '(This.pro ((pres be.v) (= (a.d sentence.n)))))
"this be a sentence"
$
```

## Running Tests
The following steps are for running the blocks world question answering unit tests.  You can substitute the file and entered package to select the appropriate tests.

```
$ acl
$ (load "load.lisp")
...[loading messages]...
$ (load "test/blocks-world-qa.lisp")
...[loading messages]...
$ (in-package :ulf2english)
#<The ULF2ENGLISH package>
$ (run-tests)
ULF2ENGLISH(4): (run-tests)
 | Failed Form: (ULF2ENGLISH REDUCED-ULF)
 | Expected "Is the Nvidia block to the left of the Texaco block?" but saw "be the Nvidia block to the left of the Texaco block"
 | EXPECTED => "Is the Nvidia block to the left of the Texaco block?"
 | (ULF2ENGLISH REDUCED-ULF) => "be the Nvidia block to the left of the Texaco block"
 | REDUCED-ULF => (((PRES BE.V) (THE.D (|Nvidia| BLOCK.N)) (TO_THE_LEFT_OF.P (THE.D (|Texaco| BLOCK.N)))) ?)
 |
 | Failed Form: (ULF2ENGLISH ULF)
 | Expected "Is the Nvidia block to the left of the Texaco block?" but saw "be the Nvidia block to the left of the Texaco block"
 | EXPECTED => "Is the Nvidia block to the left of the Texaco block?"
 | (ULF2ENGLISH ULF) => "be the Nvidia block to the left of the Texaco block"
 | ULF => (((PRES BE.V) (THE.D (|Nvidia| BLOCK.N)) (TO.P (THE.D ((ADV-A LEFT.A) (OF.P (THE.D |Texaco| BLOCK.N)))))) ?)
 |
BWQA-1: 0 assertions passed, 2 failed.

Unit Test Summary
 | 2 assertions total
 | 0 passed
 | 2 failed
 | 0 execution errors
 | 0 missing tests

#<TEST-RESULTS-DB Total(2) Passed(0) Failed(2) Errors(0)>
$
```

## After making changes

- Delete fasl files in project (e.g. `rm *.fasl`)
- Delete fasl files in dependency projects if the dependency has changed (e.g. pulling recent version)
- Run `(ql:quickload :dependency-name)`

