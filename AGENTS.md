# Repository Guidelines

## Project Structure & Module Organization

This repository is a GNU Emacs 30.1+ configuration. `templates/init.el` is the user-facing bootstrap copied by `make init`; it sets `+emacs/repo-directory` and loads the root `init.el`. The root `init.el` handles startup tuning, package setup, and module loading.

Core modules live in `lisp/` as `init-*.el` and `init-config-*.el`. Local packages and helper libraries live in `site-lisp/`. YASnippet templates are grouped by mode in `snippets/`, font profiles are in `cnfonts/`, gptel-agent org files are in `agents/`, and miscellaneous templates are in `templates/`.

## Build, Test, and Development Commands

- `make help`: list available maintenance targets.
- `make init`: install `templates/init.el` to `~/.emacs.d/init.el` and point it at this checkout.
- `make run`: start Emacs with this configuration.
- `make debug`: start Emacs with `--debug-init` for startup failures.
- `make clean`: remove generated `.elc`, `.eln`, and archive files.
- `make download`: refresh package metadata and install configured packages in batch mode.
- `make elpa` / `make standalone`: build package or full standalone archives.

For a focused startup check, run `emacs -q --load /path/to/this/init.el --debug-init`.

## Coding Style & Naming Conventions

All Elisp files should use lexical binding, usually via `;;; file.el --- summary -*- lexical-binding: t -*-`. Prefer `use-package` for external packages and keep local reusable code in `site-lisp/`. Custom functions and variables use the `+module/name` prefix, for example `+emacs/proxy`. Advice helpers use `-a` suffixes and hook helpers use `-h` suffixes. Keep comments short and only where they clarify non-obvious behavior.

## Testing Guidelines

There is no dedicated repository-wide test suite yet. Validate changes by byte-compiling the touched Elisp file when practical and by running `make debug` for startup-impacting edits. For local packages, add ERT tests if behavior becomes complex or shared; name tests after the feature or function under test.

## Commit & Pull Request Guidelines

Recent history mostly follows concise Conventional Commit style, such as `feat(magit): ...`, `fix(org): ...`, `refactor(llm): ...`, and `chore: ...`. Use that pattern when possible and keep subjects imperative and scoped. Pull requests should describe the user-visible behavior, list manual validation commands, and include screenshots only for UI-facing changes such as dashboard, font, or display tweaks.

## Security & Configuration Tips

Do not commit secrets, auth tokens, private org data, or machine-specific paths beyond documented defaults. Keep proxy and local directory customizations in the installed user init file rather than hard-coding them into shared modules.
