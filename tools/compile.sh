#!/usr/bin/env bash
# Byte-compile the configuration to a temporary directory.
#
# Loads the configuration with a throwaway HOME and a copied ELPA tree (so
# startup side effects cannot write to the real ~/.emacs.d), then byte-compiles
# each core/module/lang file individually, writing every .elc to a throwaway
# directory so the source tree stays clean.  Reports warnings/errors; exits
# non-zero on a hard failure.
#
# Like startup, modules are NOT placed on `load-path' (several basenames shadow
# built-in libraries); they are compiled by absolute path after the config has
# loaded, matching how `+emacs/load-modules' loads them at runtime.
set -euo pipefail

REPO_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)"
REAL_ELPA="${HOME}/.emacs.d/elpa"

if [ ! -d "$REAL_ELPA" ]; then
    echo "compile: no ELPA found at $REAL_ELPA; run 'make download' first." >&2
    exit 1
fi

TMP_ROOT="$(mktemp -d)"
OUT_DIR="$TMP_ROOT/elc"
COMPILE_HOME="$TMP_ROOT/home"
trap 'rm -rf "$TMP_ROOT"' EXIT

mkdir -p "$OUT_DIR" "$COMPILE_HOME/.emacs.d"
cp -a "$REAL_ELPA" "$COMPILE_HOME/.emacs.d/elpa"

echo "compile: loading config and byte-compiling to $OUT_DIR"
HOME="$COMPILE_HOME" emacs -q --batch \
    --eval "(setq byte-compile-dest-file-function
                  (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \"$OUT_DIR\")))" \
    --load "$REPO_DIR/init.el" \
    --eval "(let ((files (append
                          (directory-files (expand-file-name \"lisp/core\" \"$REPO_DIR\") t \"\\\\.el\\\\'\")
                          (directory-files (expand-file-name \"lisp/modules\" \"$REPO_DIR\") t \"\\\\.el\\\\'\")
                          (directory-files (expand-file-name \"lisp/modules/lang\" \"$REPO_DIR\") t \"\\\\.el\\\\'\")
                          (directory-files (expand-file-name \"lisp\" \"$REPO_DIR\") t \"init-config-.*\\\\.el\\\\'\"))))
              (dolist (f files) (byte-compile-file f)))" \
    --eval "(message \"compile: done (artifacts discarded)\")"
