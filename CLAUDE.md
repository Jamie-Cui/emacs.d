# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

Modern Emacs configuration targeting Emacs 30.1+ with immediate module loading and startup performance tuning.

### Two-file Entry Point

- **`templates/init.el`** → installed to `~/.emacs.d/init.el` via `make init`. This is the **user-facing config** where `+emacs/*` variables, package archives, theme, proxy, and keyboard layout are customized. It loads the main `init.el` via `(load ...)`.
- **`init.el`** (repo root) → the actual configuration entry point. Sets up performance tuning, custom variables, package management, environment, and loads all modules.

### Custom Variables (set in ~/.emacs.d/init.el before loading)

| Variable | Default | Purpose |
|----------|---------|---------|
| `+emacs/repo-directory` | `~/.emacs.d` | Root of this config repository |
| `+emacs/org-root-dir` | `~/opt/org-root` | Root for org files (auto-creates `roam/`, `journal/`, `deft/` subdirs) |
| `+emacs/proxy` | `127.0.0.1:10808` | HTTP/HTTPS proxy (used by gptel and url-proxy-services) |

### Module Loading Order

All modules are loaded immediately via `require`. Individual packages within modules use `use-package` with `:defer t` where appropriate.

```
init.el
  ├─ init-utils       (immediate, must be first)
  ├─ init-evil         (immediate)
  ├─ init-kbd          (immediate, requires init-evil)
  ├─ init-completion   (immediate)
  ├─ init-core         (immediate)
  ├─ init-misc         (immediate)
  ├─ init-os           (immediate)
  ├─ init-llm          (immediate)
  ├─ init-org          (immediate)
  └─ init-latex        (immediate, GUI only)

  Additional packages defined inline in init.el:
    nael, protobuf-mode, meson-mode, themes, speed-type,
    keyfreq, flycheck-google-cpplint, apheleia, treesit-auto,
    eldoc-cmake, bazel, markdown-mode, elfeed
```

### Package Management

- Uses built-in `package.el` + `use-package` (no straight.el or elpaca)
- **Four archives**: gnu, nongnu, org, melpa (configured in `templates/init.el`)
- Package-quickstart enabled (Emacs 30+)
- Native compilation enabled (`package-native-compile t`)
- Built-in package upgrades enabled (`package-install-upgrade-built-in t`)
- Signature checking disabled

### Special Directories

| Directory | Purpose |
|-----------|---------|
| `lisp/` | Core module files (`init-*.el`) |
| `site-lisp/` | Local packages and development projects |
| `site-lisp/magent/` | AI coding agent for Emacs (see Magent section below) |
| `agents/` | gptel-agent tool configurations (org-mode files loaded via MCP) |
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

No automated test infrastructure. Manual testing via:
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
- **gptel-agent**: Loads tools from `agents/` directory (org-mode files with MCP tool definitions)
- **gptel-magit**: AI commit messages with reasoning model formatting
- **magent**: Multi-agent AI coding assistant loaded from `site-lisp/magent/` with keybindings `M-m` and `SPC m*`
- **agent-shell**: Interactive AI shell interface
- Org-mode context prefixes: `=@Jamie=` and `=@AI=`

### init-org.el
- HTTP/HTTPS inline images via org-remoteimg (site-lisp)
- Org babel: C, shell, LaTeX, PlantUML
- LaTeX preview: xelatex + dvisvgm (xdvisvgm)
- Org-roam, org-journal, deft (with file type display), toc-org, org-appear
- org-gtd with org-edna for task dependencies
- xenops for enhanced math rendering (non-Windows only)

### init-latex.el
GUI-only, non-Windows. AUCTeX with LatexMk, cdlatex, citar for bibliography.

## Magent - AI Coding Agent

Magent is a local development project in `site-lisp/magent/` providing multi-agent AI assistance within Emacs.

### Architecture
- **Multi-agent system**: `build` (default), `plan`, `explore`, `general`, plus custom agents via `.magent/agent/*.md`
- **Permission-based tools**: Per-agent control over `read_file`, `write_file`, `grep`, `glob`, `bash`
- **gptel integration**: All LLM communication delegated to gptel (supports Claude, OpenAI, local models)
- **Session persistence**: Conversation history saved to `~/.emacs.d/magent-sessions/`

### Keybindings
Global override: `M-m` → `magent-prompt`

Leader keybindings (`SPC m`):
- `SPC mp` → prompt, `SPC mr` → prompt with region, `SPC ma` → ask at point
- `SPC mc` → clear session, `SPC ms` → show session, `SPC ml` → show log
- `SPC mA` → select agent, `SPC mi` → show current agent, `SPC mv` → list agents

### Development
Magent has its own `Makefile` in `site-lisp/magent/`:
```bash
cd site-lisp/magent
make compile    # Byte-compile all magent Elisp files
make test       # Run magent tests
make clean      # Remove .elc files
```

See `site-lisp/magent/README.md` and `site-lisp/magent/CLAUDE.md` for full documentation.

## Important Notes

- **Emacs version**: 30.1+ required
- **Lexical binding**: All files use `;;; -*- lexical-binding: t; -*-`
- **Naming convention**: Custom functions/variables use `+module/name` prefix (e.g., `+evil/smart-insert`, `+emacs/proxy`)
- **Font**: Maple Mono NL NF CN (configured via cnfonts, profiles stored in `cnfonts/`). `default` profile for coding, `reading` profile (CMU Serif + FandolKai) for documents. Per-file profile switching via Local Variables.
- **External tools needed**: ripgrep, PlantUML, universal-ctags, direnv
- **Startup performance**: GC threshold set to `most-positive-fixnum` during init (reset to 16MB/0.1%), file-name-handler-alist unset during init, read-process-output-max at 1MB
- **Server mode**: Emacs server started automatically at end of init (`server-start`)
