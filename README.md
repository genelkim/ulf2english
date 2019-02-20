# ulf2english
Maps ULFs to English sentences.

## Dependencies
- Quicklisp
- [ASDF version 3 or above](https://common-lisp.net/project/asdf/archives/asdf.lisp)
- TTT (get a copy at http://cs.rochester.edu/u/gkim21/data/ttt.tar.gz)
- [ulf-lib](https://github.com/genelkim/ulf-lib)
- [cl-util](https://github.com/genelkim/cl-util)
- cl-strings (loaded automatically via quicklisp)
- lisp-unit (loaded automatically via quicklisp)
- [pattern.en](https://www.clips.uantwerpen.be/pattern), installed through install-pattern-en.sh
- drakma (loaded automatically via quicklisp)
- cl-json (loaded automatically via quicklisp)

## Installation
1. Install quicklisp by following instructions at https://www.quicklisp.org/beta/
2. Download the latest asdf.lisp file and include it in your lisp start-up script (e.g. `.clinit.cl`)
3. Then place the other depenedencies listed above in a folder accessible to Quicklisp or ASDF (which underlies quicklisp).  How to do this in a couple ways is described by the following Stack Overflow answer https://stackoverflow.com/a/11265601.
4. If you need a Python virtual environment, get [virtualenv](https://virtualenv.pypa.io/en/latest/#) (`cs.rochester.edu` already has it) and start a virtual environment. I also recommend [pyenv](https://github.com/pyenv/pyenv) and [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv) for better management of virtual environments.
5. Run `install.sh` and check that it didn't fail

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
### Starting the pattern.en server

In order to get good performance with ulf2english you'll want to use the server option of accessing pattern.en. In order to do this, make sure to have pattern.en installed and then run 
```
python python-repl-server.py 8080 "g:g"
```
This will start the server on localhost, port 8080 with authentication "g:g" (which is the default in the code).

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



## Loading for Express Version of Allegro Common Lisp

When loading the project (say via `load.lisp` or `runtest.lisp`) on the Express version of Allegro Common Lisp, you will likely come across this error:
```
;   Fast loading /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/inferior-shell-20160929-git/macros.fasl
;   Fast loading /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/inferior-shell-20160929-git/host.fasl
;   Fast loading /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/inferior-shell-20160929-git/run.fasl
;   Fast loading /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/puri-20180228-git/src.fasl
;   Fast loading /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/cl-base64-20150923-git/package.fasl
Error: about to bind CHAR to NIL, which is not of type CHARACTER.
  [condition type: TYPE-ERROR]

Restart actions (select using :continue):
  0: store the value anyway.
  1: retry the load of /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/cl-base64-20150923-git/package.fasl
  2: skip loading /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/cl-base64-20150923-git/package.fasl
  3: recompile /home/vax8/u60/gkim21/quicklisp/dists/quicklisp/software/cl-base64-20150923-git/package.lisp
  4: Recompile package and try loading it again
  5: Retry loading FASL for #<CL-SOURCE-FILE "cl-base64" "package">.
  6: Continue, treating loading FASL for #<CL-SOURCE-FILE "cl-base64" "package"> as having been successful.
  7: Retry ASDF operation.
  8: Retry ASDF operation after resetting the configuration.
  9: Retry ASDF operation.
 10: Retry ASDF operation after resetting the configuration.
 11: retry the load of load
 12: skip loading load
 13: recompile /home/vax8/u60/gkim21/research/ulf2english/load.lisp
 14: Return to Top Level (an "abort" restart).
 15: Abort entirely from this (lisp) process.

[changing package from "COMMON-LISP-USER" to "CL-BASE64"]
[1] BASE64(2):
```
When you encounter this, please select the restart action 0 by typing
`:continue 0`. I'm pretty sure this error comes from the fact that the library
being loaded is for 64-bit common lisp where as the Express Edition of ACL is
32-bit.

## TODO

- [ ] Add a pattern-en server start-up function in ulf2english
- [ ] Generalize the socket interface to pattern-en to be useable more generally

