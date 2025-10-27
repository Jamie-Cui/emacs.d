#!/bin/bash

set -e
set -o

SRC_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
emacs -q --load $SRC_DIR/init.el --debug-init
