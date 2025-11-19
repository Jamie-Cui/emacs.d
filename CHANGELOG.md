# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-11-19

### Added
- Comprehensive Emacs configuration for version 30.1
- LaTeX workflow with citation management (citar, ebib)
- C++ development support with tree-sitter
- Eshell and Eat mode integration
- Keybinding system with search/replace functionality
- Compilation buffer configuration
- Wgrep package for enhanced grep functionality
- Dired sidebar support
- HTTP request support in org-mode
- Code snippets for C++ and other languages
- Embark integration for citation management
- Ubuntu installation script
- Maple Mono NF CN font configuration
- PlantUML integration for UML diagrams

### Changed
- Reorganized package configurations with DONE headers
- Migrated from shell-based initialization to Makefile
- Restructured keybinding system
- Enhanced evil mode configuration with new packages
- Updated font configuration to use Maple Mono NF CN (no ligature)
- Improved C++ file extensions support in auto-mode-alist
- Reorganized search keybindings and enabled posframe

### Fixed
- Corrected citar-embark package initialization
- Fixed backspace keybinding in evil mode
- Corrected font name syntax in cnfonts configuration
- Disabled org-return-follows-link behavior
- Fixed indentation issues in keybinding configurations
- Commented out problematic default bib file configuration
- Fixed compilation command to use selected region

### Refactored
- Moved citar-open binding from note to find menu
- Replaced +eshell/new with eshell for ot binding
- Renamed search keybindings to find
- Consolidated package configurations into logical sections

### Documentation
- Added comprehensive README with installation instructions
- Updated font download link to no-ligature version
- Added prerequisites and development tools setup
- Included Ubuntu installation script for dependencies

### Development Tools
- Added support for clangd, clang-format, clang-tidy
- Integrated cpplint and cmake-format
- Configured ctags for code navigation
- Set up complete C/C++ development environment

### Configuration
- Enabled native compilation support
- Configured tree-sitter for syntax highlighting
- Set up SQLite3 support for org-roam
- Configured compilation buffer to use current window
- Added internal border width configuration

### Dependencies
- Added libgccjit for native compilation
- Added libsqlite3 for org-roam database
- Added libtree-sitter for enhanced syntax analysis
- Included texlive for LaTeX support

### Performance
- Optimized package loading with use-package
- Enabled tree-sitter for improved syntax highlighting
- Configured native compilation for better performance
