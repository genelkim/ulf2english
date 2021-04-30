ULF2English
=======

[![Build Status](https://travis-ci.com/genelkim/ulf2english.svg?branch=master)](https://travis-ci.com/genelkim/ulf2english)
[![Coverage Status](https://coveralls.io/repos/github/genelkim/ulf2english/badge.svg?branch=master)](https://coveralls.io/github/genelkim/ulf2english?branch=master)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Maps ULFs to English sentences.

## Dependencies
- Quicklisp
- [ASDF version 3 or above](https://common-lisp.net/project/asdf/archives/asdf.lisp)
- [TTT](https://github.com/genelkim/ttt)
- [ulf-lib](https://github.com/genelkim/ulf-lib)
- [gute](https://github.com/genelkim/gute)
- [pattern](https://github.com/clips/pattern), If using Python 3, install with `pip install pattern`. If using Python 2, install with `script/install-pattern-en.sh`
- cl-strings (loaded automatically via quicklisp)
- lisp-unit (loaded automatically via quicklisp)
- drakma (loaded automatically via quicklisp)
- cl-json (loaded automatically via quicklisp)
- py4cl (loaded automatically via quicklisp)

The current version of the code has only been tested on SBCL.

## Installation
1. Install quicklisp by following instructions at https://www.quicklisp.org/beta/
2. Download the latest [asdf.lisp](https://common-lisp.net/project/asdf/#downloads) file and include it in your lisp start-up script (e.g. `.sbclrc`). I recommend also overwriting `quicklisp/asdf.lisp` to eliminate the possibility of accidentally loading the out-of-date copy of `asdf.lisp` that comes with Quicklisp be default.
3. Then place the other depenedencies listed above in a folder accessible to Quicklisp or ASDF (which underlies quicklisp).  How to do this in a couple ways is described by the following Stack Overflow answer https://stackoverflow.com/a/11265601.
4. If you need a Python virtual environment, get [virtualenv](https://virtualenv.pypa.io/en/latest/#) (`cs.rochester.edu` already has it) and start a virtual environment. I also recommend [pyenv](https://github.com/pyenv/pyenv) and [pyenv-virtualenv](https://github.com/pyenv/pyenv-virtualenv) for better management of virtual environments. Alternatively, you can use [Anaconda](https://www.anaconda.com/products/individual).
5. Run `script/install-pattern-en.sh` and check that it didn't fail (NB: the version we install requires Python 2). For Python 3 use `pip install pattern`.

## Running the Code
This is really meant to be a library, but to check the basic functionality of any of the functions, you can load the file load.lisp and enter the package :ulf2english.  For example,
```
$ sbcl
$ (load "load.lisp")
$ ...[loading messages]...
$ (in-package :ulf2english)
$ (ulf2english '(This.pro ((pres be.v) (= (a.d sentence.n)))))
"this be a sentence"
$
```
### Starting the pattern.en server

By default, ulf2english uses `py4cl` to interface with pattern.en, but if you choose to use the server option instead make sure to start up the server using the following command.
```
python python-repl-server.py 8080 "g:g"
```
This will start the server on localhost, port 8080 with authentication "g:g" (which is the default in the code). You can change the port, username, and password that ulf2english uses through parameters (`*python-server-url*`,`*python-server-username*`, `*python-server-password*`)  defined in `pattern-en.lisp`.

## Running Tests
The following steps are for running the blocks world question answering unit tests.  You can substitute the file and entered package to select the appropriate tests.

```
> sbcl
* (load "load.lisp")
...[loading messages]...
* (load "test/blocks-world-qa.lisp")
...[loading messages]...
* (in-package :ulf2english)
#<PACKAGE "ULF2ENGLISH">
* (run-tests)

BWQA-GK1: 1 assertions passed, 0 failed.

BWQA-GK2: 1 assertions passed, 0 failed.

...[more test results]...

BWQA-GK13: 1 assertions passed, 0 failed.

 | Failed Form: (ULF2ENGLISH ULF)
 | Expected "Are the Nvidia and the SRI blocks in the same stack?"
 | but saw "Are the Nvidia and SRI blocks in the same stack?"
 | SENTENCE => "Are the Nvidia and the SRI blocks in the same stack?"
 | (ULF2ENGLISH ULF) => "Are the Nvidia and SRI blocks in the same stack?"
 | ULF => (((PRES BE.V) (THE.D ((| Nvidia| AND.CC | SRI|) (PLUR BLOCK.N)))
            (IN.P (THE.D (SAME.A STACK.N))))
           ?)
 |
BWQA-GK14: 0 assertions passed, 1 failed.

...[more test results]...

BWQA-GP91: 1 assertions passed, 0 failed.

Unit Test Summary
 | 99 assertions total
 | 98 passed
 | 1 failed
 | 0 execution errors
 | 0 missing tests

#<TEST-RESULTS-DB Total(99) Passed(98) Failed(1) Errors(0)>
*
```

Alternatively, run all the default tests by loading `runtest.lisp`.

```
> sbcl
* (load "runtest.lisp")
...[loading messages]...
BWQA-GK1: 1 assertions passed, 0 failed.

BWQA-GK2: 1 assertions passed, 0 failed.

...[more test results]...

SCOPED-PS: 3 assertions passed, 0 failed.

FLAT-PS: 2 assertions passed, 0 failed.

Unit Test Summary
 | 187 assertions total
 | 154 passed
 | 33 failed
 | 0 execution errors
 | 0 missing tests

T
*
```

## After making changes

- Delete fasl files in project (e.g. `rm *.fasl`)
- Delete fasl files in dependency projects if the dependency has changed (e.g. pulling recent version)
- Run `(ql:quickload :dependency-name)`

