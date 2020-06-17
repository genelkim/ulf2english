#!/usr/bin/env bash

set -e

git clone https://github.com/genelkim/ttt.git ~/quicklisp/local-projects/ttt
git clone https://github.com/genelkim/cl-util.git ~/quicklisp/local-projects/cl-util
git clone https://github.com/genelkim/ulf-lib.git ~/quicklisp/local-projects/ulf-lib

# Pre-load TTT to compile it.
CURDIR=$(pwd)
cd ~/quicklisp/local-projects/ttt/src
cl -e '(load "load")'
cd $CURDIR

