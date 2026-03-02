# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

Modern Emacs configuration targeting Emacs 30.1+ with immediate module loading and startup performance tuning.

### Two-file Entry Point

- **`templates/init.el`** → installed to `~/.emacs.d/init.el` via `make init`. This is the **user-facing config** where `+emacs/*` variables, package archives, theme, proxy, and keyboard layout are customized. Its final action is `(load (concat +emacs/repo-directory "/init.el"))`.
- **`init.el`** (repo root) → the actual configuration entry point. Sets up performance tuning, custom variables, package management, environment, loads all modules via `require`, and defines additional inline packages (treesit-auto, elfeed, magent, etc.).

### Custom Variables (set in ~/.emacs.d/init.el before loading)

| Variable | Default | Purpose |
|----------|---------|---------|
| `+emacs/repo-directory` | `~/.emacs.d` | Root of this config repository (used pervasively: cnfonts, snippets, agents, site-lisp, dashboard) |
| `+emacs/org-root-dir` | `~/opt/org-root` | Root for org files (auto-creates `roam/`, `journal/`, `deft/` subdirs) |
| `+emacs/proxy` | `127.0.0.1:10808` | HTTP/HTTPS proxy (used by gptel, url-proxy-services, eshell) |

### Module Loading Order

All modules are loaded immediately via `require`. Individual packages within modules use `use-package` with `:defer t` where appropriate.

```
init.el
  ├─ init-utils       (must be first — provides macros and utilities used by all other modules)
  ├─ init-evil         (uses +point-in-comment-p from init-utils without explicit require)
  ├─ init-kbd          (requires init-evil; defines +my-leader-def used later in init.el)
  ├─ init-completion   (no explicit requires)
  ├─ init-core         (requires init-evil; references init-kbd functions via deferred #' symbols)
  ├─ init-misc         (requires init-utils)
  ├─ init-os           (no explicit requires)
  ├─ init-llm          (no explicit requires)
  ├─ init-org          (requires init-utils)
  └─ init-latex        (requires init-org; GUI only, guarded by display-graphic-p)

  Inline in init.el after modules:
    nael, protobuf-mode, meson-mode, themes, speed-type, keyfreq,
    flycheck-google-cpplint, apheleia, treesit-auto, eldoc-cmake,
    bazel, markdown-mode, elfeed, magent (conditional), edraw (conditional)
```

### Package Management

- Uses built-in `package.el` + `use-package` (no straight.el or elpaca)
- **Four archives**: gnu, nongnu, org, melpa (configured in `templates/init.el`)
- Package-quickstart enabled (Emacs 30+)
- Native compilation enabled (`package-native-compile t`)
- Signature checking disabled

### Special Directories

| Directory | Purpose |
|-----------|---------|
| `lisp/` | Core module files (`init-*.el`) |
| `site-lisp/` | Local packages: persp-projectile, org-remoteimg, org-imgtog, consult-citre, agent-review |
| `site-lisp/magent/` | AI coding agent (has own `CLAUDE.md` and `Makefile`) |
| `site-lisp/el-easydraw/` | SVG drawing editor for org-mode (optional, loaded conditionally) |
| `agents/` | gptel-agent tool configurations (org-mode files with MCP tool definitions) |
| `snippets/` | YASnippet templates (org-mode, c++-ts-mode, LaTeX-mode) |
| `templates/` | User-facing templates (`init.el`, `.authinfo`) |

## Development Commands

### Makefile Targets
```bash
make help          # Show available targets
make clean         # Clean .elc, .eln files and .tar.gz archives
make debug         # Run Emacs with --debug-init
make run           # Run Emacs
make init          # Install templates/init.el → ~/.emacs.d/init.el (sets repo path via sed)
make download      # Batch download/update all ELPA packages
make elpa          # Create elpa.tar.gz archive of packages
make standalone    # Create standalone .emacs.d archive (config + packages, no .git)
```

### Magent Development
```bash
cd site-lisp/magent
make compile    # Byte-compile all magent Elisp files
make test       # Run magent tests
make clean      # Remove .elc files
```

See `site-lisp/magent/CLAUDE.md` for full magent documentation.

### Manual Testing
```bash
emacs -q --load /path/to/this/init.el --debug-init
```

## Key Patterns and Conventions

### Naming Convention

Custom functions/variables use `+module/name` prefix (e.g., `+evil/smart-insert`, `+emacs/proxy`).

**Name suffixes have meaning:**
- `-a` suffix = **advice** function (e.g., `+evil-join-a`, `+evil/window-split-a`)
- `-h` suffix = **hook** function (e.g., `+enable-delete-trailing-whitespace-h`)

### The `+use-package-when-dir-exists` Macro (init-utils.el)

Compile-time conditional `use-package` wrapper. Evaluates the directory path at macro-expansion time and silently omits the entire `use-package` form if the directory doesn't exist. Used at the bottom of `init.el` to conditionally load `magent` and `edraw` from `site-lisp/`:

```elisp
(+use-package-when-dir-exists magent
    (concat +emacs/repo-directory "/site-lisp/magent/lisp")
  :after gptel
  :demand t
  ...)
```

### Keybinding System (init-kbd.el)

All keybindings go through `general.el`. The leader key definer is:

```elisp
(defconst my-leader "SPC")
(general-create-definer +my-leader-def :prefix my-leader)
```

To add new leader bindings (from any module or init.el):
```elisp
(+my-leader-def
  :states '(normal visual motion)
  :keymaps 'override
  "xx" #'some-function)
```

Override keybindings (`:keymaps 'override`) are used for bindings that must not be shadowed by any mode. C-i and TAB are separated in GUI via `input-decode-map`.

### TRAMP Memoization (init-misc.el)

Remote path operations are memoized via `+tramp--memoize` to avoid repeated slow calls. Applied as advice around `magit-toplevel`, `project-current`, and `vc-git-root`.

### Evil Mode is Foundational

The entire keybinding system assumes evil states. Many utility functions are evil-aware:
- `+region-active-p` checks both `use-region-p` and `evil-visual-state-p`
- `+region-beginning`/`+region-end` use evil visual markers
- `+backward-kill-to-bol-and-indent` delegates to `evil-delete` when available

## Module Reference

### init-utils.el
Foundation module. Key utilities used across the codebase:
- `+syntax-ppss`: Memoized `syntax-ppss` with auto-reset on buffer changes
- `+point-in-comment-p`, `+point-in-string-p`, `+point-in-string-or-comment-p`: Syntax state detection
- `+thing-at-point-or-region`: Smart text grabber (tries selection → thing-at-point → xref → prompt)
- `+bol-bot-eot-eol`: Computes four positions per line (BOL, beginning-of-text, end-of-text before comments, EOL)
- `+backward-to-bol-or-indent`, `+forward-to-last-non-comment-or-eol`: Smart Home/End cycling
- `+dumb-indent`/`+dumb-dedent`: Literal space insertion respecting `tab-width` alignment
- `+use-package-when-dir-exists`: Conditional package loading macro (see above)

### init-evil.el
- `+evil/smart-insert`: Context-aware insert (used as advice in init-org.el and init-latex.el)
- `+evil-join-a`: Advice that removes comment delimiters when joining commented lines
- `+evil/window-split-a`/`+evil/window-vsplit-a`: Fix window focus history after splits
- `evil-escape-key-sequence` set to `"jk"`; `forward-evil-word` aliased to `forward-evil-symbol`

### init-kbd.el
- Leader `SPC` with hierarchy: `G` gptel, `a` embark, `w` window, `b` buffer, `p` project, `n` notes, `c` code, `f` find, `m` magent
- Mac-like overrides: `M-s` save, `M-c` copy, `M-v` paste, `M-a` select-all, `M-/` comment
- `C-h`/`C-l` perspective prev/next, `M-p`/`M-P` compile

### init-completion.el
Vertico + Corfu + Consult + Orderless + Marginalia. `+vertico/file-search` and `+vertico/project-search` build custom ripgrep commands via `consult--grep`. Requires external `ripgrep`.

### init-core.el
- `+flycheck/eldoc-function`: Shows flycheck errors at point via eldoc with severity-colored faces
- `citre` appended to `evil-goto-definition-functions` so `gd` falls back to ctags
- `persp-projectile` (from site-lisp): Creates a perspective per project when switching
- Magit auto-commit transient with message "chore: stale - work still in progress"

### init-misc.el
- Custom `mode-line-format` with buffer name, position, selection size, line count
- Compilation mode: custom Rust error/warning/panic regexp patterns
- Eshell proxy management: `eshell/set-proxy`, `eshell/unset-proxy`, `eshell/show-proxy`
- TRAMP memoization for `magit-toplevel`, `project-current`, `vc-git-root`

### init-llm.el
- gptel backends: Gemini (auth-source), Aliyun/Dashscope (multiple models), Zhipu
- `gptel-magit--format-commit-message` overridden to handle `(reasoning . text)` cons cells
- gptel-agent loads tools from `agents/` directory
- Org-mode context prefixes: `=@Jamie=` and `=@AI=`

### init-org.el
- org-remoteimg (site-lisp): Inline display of HTTP/HTTPS images with caching
- TODO keywords: `TODO(t)` | `NEXT(n)` | `WAIT(w)` || `DONE(d)` | `KILL(k)`
- Custom faces: `+org-todo-active`, `+org-todo-project`, `+org-todo-onhold`, `+org-todo-cancel`
- org-gtd with org-edna for task dependencies
- LaTeX preview: xelatex + dvisvgm; xenops for enhanced math rendering (non-Windows)

### init-latex.el
GUI-only, non-Windows. AUCTeX with LatexMk, cdlatex, citar + ebib for bibliography.

## Important Notes

- **Emacs version**: 30.1+ required
- **Lexical binding**: All files must use `;;; -*- lexical-binding: t; -*-`
- **Font**: Maple Mono NL NF CN via cnfonts. `default` profile for coding, `reading` profile (CMU Serif + FandolKai) for documents.
- **External tools needed**: ripgrep, PlantUML, universal-ctags, direnv
- **Startup performance**: GC threshold set to `most-positive-fixnum` during init (reset to 16MB/0.1%), `file-name-handler-alist` unset during init, `read-process-output-max` at 1MB
- **Server mode**: `server-start` called at end of init
