#!/usr/bin/env bash

set -e

git clone https://github.com/genelkim/ttt.git ~/quicklisp/local-projects/ttt
git clone https://github.com/genelkim/cl-util.git ~/quicklisp/local-projects/cl-util
git clone https://github.com/genelkim/ulf-lib.git ~/quicklisp/local-projects/ulf-lib
cp -r cl-coveralls ~/quicklisp/local-projects/cl-coveralls

CURDIR=$(pwd)
cd ~/quicklisp/local-projects/ttt/src
cl -e '(load "load")'
cd $CURDIR

## Update quicklisp asdf version.
#ASDF_URL="https://common-lisp.net/project/asdf/archives/asdf.lisp"
#wget $ASDF_URL
#rm ~/quicklisp/asdf.lisp
#mv asdf.lisp ~/quicklisp/asdf.lisp

