# AGENTS.md

This file provides guidance to the AI agent when working with code in this repository.

## Bootstrap & Loading Chain

- `templates/init.el` is the user-facing bootstrap copied to `~/.emacs.d/init.el` by `make init`. It sets `+emacs/repo-directory` and loads the root `init.el`.
- The root `init.el` is the main entry point: startup tuning, package setup, then explicit `(require 'init-*)` calls to load modules from `lisp/`.
- Adding a new module requires adding a `(require 'init-foo)` in the root `init.el`.
- `lisp/init-config-*.el` files are sub-modules loaded by their parent `init-*.el` modules, not directly by the root `init.el`.

## Build Commands

- `make init` — install `templates/init.el` to `~/.emacs.d/init.el` and point it at this checkout.
- `make debug` — run Emacs with `--debug-init` for startup failures.
- Most Makefile targets (`clean`, `elpa`, `standalone`) operate on `$HOME/.emacs.d`, not the repo directory.
- Focused startup check: `emacs -q --load /path/to/repo/init.el --debug-init`.

## Coding Conventions

- All Elisp files use lexical binding: `;;; file.el --- summary -*- lexical-binding: t -*-`.
- Use `use-package` for external packages. Keep local reusable code in `site-lisp/`.
- Custom functions and variables use `+module/name` prefix (e.g., `+emacs/proxy`, `+emacs/repo-directory`).
- Advice helpers use `-a` suffix; hook helpers use `-h` suffix.
- Targets Emacs 30.1+.

## Commits

Conventional Commit style with scope: `feat(magit): ...`, `fix(org): ...`, `refactor(llm): ...`, `chore: ...`. Imperative, scoped subjects.

## Gotchas

- `site-lisp/` contains local custom/forked packages (e.g., `org-project.el`, `magit-gptel.el`, `dashboard-elfeed.el`), not third-party ELPA packages.
- User-customizable settings (`+emacs/proxy`, `+emacs/org-root-dir`) belong in `templates/init.el` or the installed `~/.emacs.d/init.el`, not hard-coded in shared modules.
- Do not commit secrets, auth tokens, private org data, or machine-specific paths.
