#!/bin/bash

set -e
set -o

pushd $HOME/.emacs.d
find . -name "*.elc" -type f -delete
popd
