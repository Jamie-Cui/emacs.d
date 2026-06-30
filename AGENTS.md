# AGENTS.md

This file provides guidance to the AI agent when working with code in this repository.

## Bootstrap & Loading Chain

The configuration loads in three layers (see README.org "Architecture" for the
full picture and the module-boundary table):

1. **Bootstrap** — repo `early-init.el` (GC, `package-enable-at-startup`, early UI
   chrome), then repo `init.el`, which is a manifest only.
2. **Core** (`lisp/core/`) — `core-vars`, `core-paths`, `core-startup`,
   `core-package`, `core-loader`, `core-util`, loaded with `require` in that fixed
   order. File names match their feature (`core-vars.el` provides `core-vars`).
3. **Modules** (`lisp/modules/`) — one file per domain, each `(provide 'init-<module>)`.
   Language modules live in `lisp/modules/lang/` and are named path-like (`lang/cmake`).

`init.el` calls `(+emacs/load-modules '(...))` with the module list in load order.
`keys` is always last (so module commands are defined before binding).

- **Adding a module**: create `lisp/modules/foo.el` with `(provide 'init-foo)` and add
  `"foo"` to the manifest list in `init.el` at the right position.
- **Adding a language**: create `lisp/modules/lang/foo.el` with `(provide 'init-lang-foo)`
  and add `"lang/foo"` to the manifest.
- `lisp/modules/` is **not** on `load-path` (basenames like `project`, `vc`, `org`,
  `files` would shadow built-in libraries); the loader loads modules by absolute path.
- `lisp/init-config-*.el` are helper sub-files for local/forked `site-lisp/` packages,
  `require`d by the owning module; they stay in `lisp/` (which is on `load-path`).

## Build Commands

- `make init` — install `templates/{init,early-init}.el` to `~/.emacs.d` (skips existing files).
- `make init-force` — reinstall the templates, overwriting existing files.
- `make smoke` — offline startup smoke test of both the repo-init and installed-init chains.
- `make compile` — byte-compile core + modules to a temp dir (does not touch the source tree).
- `make debug` — run Emacs with `--debug-init`.
- `clean`, `elpa`, `standalone` operate on `$HOME/.emacs.d` and/or the repo.
- Focused startup check: `emacs -q --load /path/to/repo/init.el --debug-init`.

## Coding Conventions

- All Elisp files use lexical binding: `;;; file.el --- summary -*- lexical-binding: t -*-`.
- Use `use-package` for external packages. Keep local reusable code in `site-lisp/`.
- Custom functions and variables use `+module/name` prefix (e.g., `+emacs/proxy`, `+ui/...`).
- Advice helpers use `-a` suffix; hook helpers use `-h` suffix.
- Targets Emacs 30.1+.

## Commits

Conventional Commit style with scope: `feat(magit): ...`, `fix(org): ...`, `refactor(llm): ...`, `chore: ...`. Imperative, scoped subjects.

## Gotchas

- `site-lisp/` contains local custom/forked packages (e.g., `org-project.el`, `magit-gptel.el`, `dashboard-elfeed.el`), not third-party ELPA packages.
- User-customizable settings (`+emacs/proxy`, `+emacs/org-root-dir`, `+emacs/disabled-modules`) belong in `templates/init.el` or the installed `~/.emacs.d/init.el`, not hard-coded in shared modules.
- `package-archives` and `(load-theme ...)` live in the template / installed `init.el`, not in the repo.
- Do not commit secrets, auth tokens, private org data, or machine-specific paths.
