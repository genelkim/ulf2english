#!/usr/bin/env bash

set -e

git clone https://github.com/genelkim/ttt.git ~/quicklisp/local-projects/
git clone https://github.com/genelkim/cl-util.git ~/quicklisp/local-projects/
git clone https://github.com/genelkim/ulf-lib.git ~/quicklisp/local-projects/

CURDIR=$(pwd)
cd ~/quicklisp/local-projects/ttt/src
cl -e '(load "load")'
cd $CURDIR
