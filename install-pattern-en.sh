#!/bin/bash

#
# Install script.
#
# If you're running this from the cs.rochester.edu network, make sure to use a
# python virtual environment so the installation won't fail. See the README for
# links to those tools.

# Pattern library from CLIPS.  Mainly used for word conjugating.
wget https://www.clips.uantwerpen.be/media/pattern-2.6.zip
unzip pattern-2.6.zip
cd pattern-2.6
python setup.py install
cd ..
rm -rf pattern-2.6
rm pattern-2.6.zip

