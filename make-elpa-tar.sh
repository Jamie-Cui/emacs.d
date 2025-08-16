#!/bin/bash

set -e
set -o

pushd $HOME/.emacs.d
find . -name "*.elc" -type f -delete
tar czf elpa.tar.gz elpa
popd

mv $HOME/.emacs.d/elpa.tar.gz .
