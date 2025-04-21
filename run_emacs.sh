#!/bin/bash

set -e
set -o

emacs -q --load "${HOME}"/Desktop/emacs.d/init.el --debug-init
