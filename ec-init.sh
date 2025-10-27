#!/bin/bash

set -e
set -o

TARGET="$HOME/.emacs.d/init.el"

if [ -f "$TARGET" ]; then
  echo "Target file exists."
else
  cp templates/init.el $TARGET
fi
