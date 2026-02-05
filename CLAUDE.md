# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

Modern Emacs configuration targeting Emacs 30.1+ with two-level deferred loading for fast startup.

### Two-file Entry Point

The configuration uses a two-file entry system:
- **`templates/init.el`** → installed to `~/.emacs.d/init.el` via `make init`. This is the **user-facing config** where `+emacs/*` variables, package archives, theme, proxy, and keyboard layout are customized. It loads the main `init.el` via `(load ...)`.
- **`init.el`** (repo root) → the actual configuration entry point. Sets up performance tuning, custom variables, package management, environment, and loads all modules.

### Custom Variables (set in ~/.emacs.d/init.el before loading)

| Variable | Default | Purpose |
|----------|---------|---------|
| `+emacs/repo-directory` | `~/.emacs.d` | Root of this config repository |
| `+emacs/org-root-dir` | `~/opt/org-root` | Root for org files (auto-creates `roam/`, `journal/`, `deft/` subdirs) |
| `+emacs/proxy` | `127.0.0.1:10808` | HTTP/HTTPS proxy (used by gptel and url-proxy-services) |

### Module Loading Order

```
init.el
  ├─ init-utils       (immediate)
  ├─ init-evil         (immediate)
  ├─ init-kbd          (immediate, requires init-evil)
  ├─ init-completion   (immediate)
  │
  ├─ with-eval-after-load 'init-utils:
  │   ├─ init-core     (deferred)
  │   ├─ init-misc     (deferred)
  │   └─ init-os       (deferred)
  │
  ├─ with-eval-after-load 'init-core:
  │   ├─ init-llm      (deferred)
  │   └─ init-org      (deferred)
  │
  └─ when GUI + with-eval-after-load 'init-org:
      └─ init-latex    (GUI only, deferred)

  Additional packages defined inline in init.el:
    nael, protobuf-mode, meson-mode, themes, speed-type,
    keyfreq, flycheck-google-cpplint, apheleia, treesit-auto,
    eldoc-cmake, bazel, markdown-mode, elfeed
```

### Package Management

- **Four archives**: gnu, nongnu, org, melpa (configured in `templates/init.el`)
- Package-quickstart enabled (Emacs 30+)
- Native compilation enabled (`package-native-compile t`)
- Built-in package upgrades enabled (`package-install-upgrade-built-in t`)
- Signature checking disabled

### Special Directories

| Directory | Purpose |
|-----------|---------|
| `lisp/` | Core module files |
| `site-lisp/` | Local packages (org-remoteimg, org-imgtog, persp-projectile, consult-citre, agent-review) |
| `agents/` | gptel-agent tool configurations |
| `snippets/` | YASnippet templates (org-mode, c++-ts-mode, LaTeX-mode) |
| `templates/` | User-facing templates (`init.el`, `.authinfo`) |
| `cnfonts/` | cnfonts profiles (`default`, `reading`) and config |
| `data/` | Dashboard banner files |

## Development Commands

### Makefile Targets
```bash
make help          # Show available targets
make clean         # Clean .elc, .eln files and .tar.gz archives
make elpa          # Create elpa.tar.gz archive of packages
make standalone    # Create standalone .emacs.d archive (config + packages, no .git)
make debug         # Run Emacs with --debug-init
make run           # Run Emacs
make init          # Install templates/init.el → ~/.emacs.d/init.el (sets repo path via sed)
make download      # Batch download/update all ELPA packages
```

### Testing
```bash
emacs -q --load /path/to/this/init.el --debug-init
```

## Module Reference

### init-utils.el
- `+syntax-ppss`: Memoized `syntax-ppss`
- `+surrounded-by-pairs-p`: Brace delimiter detection

### init-evil.el
Evil mode with: evil-collection, evil-mc (multiple cursors), evil-multiedit, evil-surround, evil-args, evil-nerd-commenter, evil-escape, undo-tree, expand-region. Custom `+evil/smart-insert` for context-aware insertion.

### init-kbd.el
- Leader: `SPC` via `general.el`, local leader: `SPC m`
- C-i and TAB separated in GUI via `input-decode-map`
- `[[`/`]]` in motion state: flycheck prev/next error
- Override bindings on `M-` and `C-` prefixes (M-s save, M-c copy, M-v paste, M-/ comment, etc.)

### init-completion.el
Vertico (with posframe) + Corfu + Consult + Orderless + Marginalia. Custom functions: `+vertico/project-search`, `+vertico/file-search`. Requires `ripgrep`.

### init-core.el
- **Dired**: dired-du, dired-subtree, diredfl, dired-sidebar
- **Editing**: hl-todo, diff-hl, pangu-spacing, smartparens, cnfonts, embark
- **Programming**: flycheck (with eldoc integration), flycheck-eglot, eglot (with harper-ls for text-mode), citre, eldoc-box
- **Workspaces**: perspective + projectile (with persp-projectile from site-lisp)
- **Tools**: magit (with auto-commit transient), eat terminal, dashboard, docker, envrc

### init-misc.el
Pixel scrolling, backup/autosave directories, TRAMP settings, fringe, trailing whitespace cleanup.

### init-os.el
OS-specific settings (keyboard modifiers, clipboard, etc.).

### init-llm.el
- **gptel**: Gemini (via auth-source), Aliyun/Dashscope (qwen3-max, qwen-plus, deepseek-r1, qwen3-coder-plus), Zhipu (glm-4.7)
- **gptel-agent**: Loads tools from `agents/` directory
- **gptel-magit**: AI commit messages with reasoning model formatting
- Org-mode context prefixes: `=@Jamie=` and `=@AI=`

### init-org.el
- HTTP/HTTPS inline images via org-remoteimg (site-lisp)
- Org babel: C, shell, LaTeX, PlantUML
- LaTeX preview: xelatex + dvisvgm (xdvisvgm)
- Org-roam, org-journal, deft (with file type display), toc-org, org-appear
- xenops for enhanced math rendering (non-Windows only)

### init-latex.el
GUI-only, non-Windows. AUCTeX with LatexMk, cdlatex, citar for bibliography.

## Important Notes

- **Emacs version**: 30.1+ required
- **Lexical binding**: All files use `;;; -*- lexical-binding: t; -*-`
- **Naming convention**: Custom functions/variables use `+module/name` prefix (e.g., `+evil/smart-insert`, `+emacs/proxy`)
- **Font**: Maple Mono NF CN (configured via cnfonts, profiles stored in `cnfonts/`). `default` profile for coding, `reading` profile (CMU Serif + FandolKai) for documents. Per-file profile switching via Local Variables.
- **External tools needed**: ripgrep, PlantUML, universal-ctags, direnv
- **Startup performance**: GC threshold set to `most-positive-fixnum` during init (reset to 16MB/0.1%), file-name-handler-alist unset during init, read-process-output-max at 1MB
