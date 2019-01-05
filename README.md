# ulf2english
Maps ULFs to English sentences.

## Dependencies
- Quicklisp
- TTT (get a copy at http://cs.rochester.edu/u/gkim21/data/ttt.tar.gz)
- https://github.com/genelkim/ulf-lib
- https://github.com/genelkim/cl-util
- cl-strings (loaded automatically via quicklisp)

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

