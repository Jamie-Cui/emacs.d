# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

This is a modern Emacs configuration targeting Emacs 30.1+ with optimized startup performance and modular structure. The configuration uses a modular architecture with deferred loading for better startup times.

### Core Architecture

- **Main entry point**: `init.el` - Sets up package management, environment, and loads modules
- **Modular structure**: Configuration split into focused modules in `lisp/` directory
- **Deferred loading**: Uses `with-eval-after-load` and `:defer t` for faster startup
- **Native compilation**: Enabled for better performance on Emacs 30+
- **Tree-sitter**: Uses built-in tree-sitter with `treesit-auto` for modern syntax highlighting

### Module Loading Order

**Core utilities** (loaded first):
- `init-utils.el` - Utility functions and macros (memoization, syntax parsing, etc.)

**Essential modules** (loaded immediately):
- `init-evil.el` - Evil mode and Vim emulation setup
- `init-kbd.el` - Keybindings and leader key configuration (depends on init-evil)
- `init-completion.el` - Vertico, Corfu, Consult, Orderless, Marginalia

**Core functionality** (deferred via `with-eval-after-load`):
- `init-core.el` - Core functionality (dired, file management, projectile, etc.)
- `init-misc.el` - Miscellaneous configurations (projectile, treemacs, yasnippet, etc.)
- `init-os.el` - OS-specific settings

**Advanced features** (deferred, depends on init-core):
- `init-llm.el` - LLM/AI tool configurations (gptel, agent-shell, gptel-magit)
- `init-org.el` - Org-mode setup with inline image support for HTTP/HTTPS

**LaTeX support** (GUI only, deferred, depends on init-org):
- `init-latex.el` - LaTeX support

## Development Commands

### Makefile Targets
```bash
make help          # Show available targets
make clean         # Clean all .elc files, .eln files, and .tar.gz archives
make elpa          # Create elpa.tar.gz archive of packages
make standalone    # Create standalone configuration archive (.emacs.d with packages)
make debug         # Run Emacs with debug init
make run           # Run Emacs
make init          # Initialize configuration in ~/.emacs.d/ (from templates/init.el)
make download      # Download/update all ELPA packages
```

### Testing Configuration
```bash
# Test configuration with debug output
emacs -q --load /path/to/this/init.el --debug-init

# Test with specific modules
emacs --eval "(load-file \"/path/to/init.el\")"

# Toggle tree-sitter auto mode
M-x +treesit-auto/toggle
```

## Key Configuration Details

### Package Management
- Uses MELPA exclusively
- Package-quickstart enabled for faster startup
- Parallel package initialization
- Native compilation enabled with `auto-compile-on-load-mode` and `auto-compile-on-save-mode`
- Auto-compilation via `auto-compile` package

### Keybinding System
- Leader key: SPC (space)
- Local leader: SPC m
- Uses `general.el` for keybinding management
- Evil mode with comprehensive Vim emulation
- C-i and TAB are separated in GUI Emacs via input-decode-map

### Completion System
- **Vertico**: Vertical completion UI with posframe support
- **Corfu**: In-text completion (disabled in eshell)
- **Consult**: Enhanced search and navigation with ripgrep integration
  - Custom function: `+vertico/project-search` for project-wide search
  - Custom function: `+vertico/file-search` for flexible file searching
- **Orderless**: Fuzzy matching for completion
- **Marginalia**: Completion candidate annotations
- **Dependencies**: Requires `ripgrep` (rg) for consult-ripgrep

### Tree-sitter Configuration
- Auto-installs tree-sitter grammars with `treesit-auto`
- Custom mode associations:
  - `.h/.hpp/.cc/.cpp` → `c++-ts-mode`
  - `CMakeLists.txt` → `cmake-ts-mode`
- Syntax table fixes for `cmake-ts-mode` via `+cmake-ts-mode/fix-syntax-table`
- Toggle via `+treesit-auto/toggle` function

### Development Tools Integration
- **LSP**: Eglot for language server support
- **C/C++**: clangd, clang-format, clang-tidy, cpplint (via flycheck-google-cpplint)
- **CMake**: cmake-format, cmake-ts-mode with syntax table fixes
- **Code quality**: flycheck with cpplint integration (filtered rules: excludes whitespace, build/include_order, build/header_guard, runtime/reference)
- **Formatting**: apheleia with remote algorithm set to local
- **Version control**: Built-in magit support with gptel-magit integration
- **Project management**: Projectile for project navigation
- **File tree**: Treemacs for project overview
- **Snippets**: YASnippet for code templates

### LLM/AI Integration
- **gptel**: Primary LLM interface with support for:
  - Gemini backend (via auth-source)
  - Aliyun (Dashscope) backend with qwen models (qwen3-max, qwen-plus, deepseek-r1, qwen3-coder-plus)
  - Zhipu backend with glm models (glm-4.7)
  - Custom context prefixes for org-mode (`=@Jamie=` and `=@AI=`)
  - Automatic heading demotion in responses
- **gptel-agent**: Agent system with tools from `agents/` directory
- **agent-shell**: Alternative agent interface
- **gptel-magit**: AI-assisted commit message generation with custom formatting for reasoning models

### Special Directories
- Configuration root: `+emacs/repo-directory` (customizable, set in ~/.emacs.d/init.el)
- Org files: `+emacs/org-root-dir` (default: `~/opt/org-root`)
  - Subdirectories: `roam/`, `journal/`, `deft/` (auto-created on startup)
- Package user directory: Standard `package-user-dir`
- Agent directory: `agents/` for gptel-agent configurations
- Site-lisp: `site-lisp/` for local packages (e.g., org-yt)
- Undo tree history: `undo-tree-hist/` in user-emacs-directory
- Elfeed data: `elfeed/db/` and `elfeed/enclosure/`

### Performance Optimizations
- **Startup**: GC threshold set to `most-positive-fixnum` during init, reset to 16MB after startup
- **File I/O**: File name handler unset during init for faster I/O
- **Process output**: `read-process-output-max` increased to 1MB for faster LSP responses
- **Loading**: `load-prefer-newer` set to t to prefer newer .el files over .elc
- **Directory creation**: Deferred to `emacs-startup-hook` for faster init
- **Memoization**: Utility functions in `init-utils.el` use memoization (e.g., `+syntax-ppss`)

## Important Notes

- **Emacs version**: Requires Emacs 30.1+
- **Lexical binding**: Used throughout (`-*- lexical-binding: t; -*-`)
- **GUI-only features**: LaTeX support conditionally loaded with `(display-graphic-p)`
- **Proxy configuration**: Available via `+emacs/proxy` custom variable (used by gptel and url-proxy-services)
- **Environment setup**: Uses `exec-path-from-shell` for non-Windows systems to inherit shell environment
- **Font recommendation**: Maple Mono NF CN (no ligature)
- **PlantUML**: Required for UML diagrams in org-mode
- **Auto-compilation**: Enabled via `auto-compile` package

## Module-Specific Notes

### init-utils.el
Provides core utility functions:
- `+syntax-ppss`: Memoized version of `syntax-ppss` for performance
- `+surrounded-by-pairs-p`: Checks if point is surrounded by brace delimiters
- Various helper functions used across modules

### init-org.el
- **HTTP/HTTPS inline images**: Custom `org-http-image-data-fn` for async image loading
- **YouTube thumbnails**: Integrated via `org-yt` package in site-lisp
- **LaTeX preview**: Uses xdvisvgm with xelatex and dvisvgm
- **Org roam**: Enabled with database in `+emacs/org-root-dir/roam`
- **Org journal**: Enabled with files in `+emacs/org-root-dir/journal`

### init-kbd.el
- **Leader key**: SPC (space) using `general.el`
- **Local leader**: SPC m for mode-specific bindings
- **C-i vs TAB**: Separated in GUI Emacs via input-decode-map
- **Minibuffer**: Enhanced navigation with C-n/C-p for history
- **Flycheck navigation**: [[ and ]] for previous/next errors in motion state

### init-evil.el
Comprehensive Vim emulation including:
- expand-region, undo-tree
- evil-collection, evil-args, evil-nerd-commenter
- evil-mc (multiple cursors), evil-multiedit
- evil-surround, evil-escape
- evil-terminal-cursor-changer (for terminal)

### init-core.el
- **Dired**: Enhanced with dired-du, dired-subtree, diredfl, dired-sidebar
- **Projectile**: Project management and navigation
- **Treemacs**: File tree visualization
- **Eglot**: LSP client configuration
- **Flycheck**: Syntax checking with eglot integration
