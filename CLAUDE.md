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

### Module Structure

**Core modules (loaded immediately)**:
- `init-kbd.el` - Keybindings and leader key configuration
- `init-evil.el` - Evil mode and Vim emulation setup

**Deferred modules (loaded after dependencies)**:
- `init-core.el` - Core functionality (dired, file management, etc.)
- `init-misc.el` - Miscellaneous configurations
- `init-os.el` - OS-specific settings
- `init-llm.el` - LLM/AI tool configurations
- `init-org.el` - Org-mode setup (loaded after LLM module)
- `init-latex.el` - LaTeX support (GUI only, loaded after org)

**Utility modules**:
- `init-utils.el` - Utility functions and macros (used by other modules)

## Development Commands

### Makefile Targets
```bash
make help          # Show available targets
make clean         # Clean all .elc files and archives
make elpa          # Create elpa.tar.gz archive of packages
make standalone    # Create standalone configuration archive
make run-debug     # Run Emacs with debug init
make init          # Initialize configuration in ~/.emacs.d/
```

### Testing Configuration
```bash
# Test configuration with debug output
emacs -q --load /path/to/this/init.el --debug-init

# Test with specific modules
emacs --eval "(load-file \"/path/to/init.el\")"
```

## Key Configuration Details

### Package Management
- Uses MELPA exclusively
- Package-quickstart enabled for faster startup
- Parallel package initialization
- Native compilation enabled

### Keybinding System
- Leader key: SPC (space)
- Local leader: SPC m
- Uses `general.el` for keybinding management
- Evil mode with comprehensive Vim emulation

### Tree-sitter Configuration
- Auto-installs tree-sitter grammars with `treesit-auto`
- Custom mode associations:
  - `.h/.hpp/.cc/.cpp` → `c++-ts-mode`
  - `CMakeLists.txt` → `cmake-ts-mode`
- Syntax table fixes for `cmake-ts-mode`

### Development Tools Integration
- **LSP**: Eglot for language server support
- **C/C++**: clangd, clang-format, clang-tidy
- **Code quality**: flycheck with cpplint integration
- **Formatting**: apheleia with eglot-format
- **Version control**: Built-in magit support

### Special Directories
- Configuration root: `+emacs/repo-directory` (customizable)
- Org files: `+emacs/org-root-dir` (default: `~/org-root`)
- Package user directory: Standard `package-user-dir`

### Performance Optimizations
- Deferred loading of non-essential packages
- Native compilation where available
- Package-quickstart for faster startup
- Memoized utility functions in `init-utils.el`

## Important Notes

- Requires Emacs 30.1+
- Uses lexical binding throughout
- GUI-only features (like LaTeX) are conditionally loaded
- Proxy configuration available via `+emacs/proxy`
- Environment setup via `exec-path-from-shell` for non-Windows systems
