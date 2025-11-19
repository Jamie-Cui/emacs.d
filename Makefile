# Makefile for Emacs configuration management

.PHONY: clean elpa standalone run-debug init help

# Default target
help:
	@echo "Available targets:"
	@echo "  clean          - Clean all .elc files"
	@echo "  elpa           - Create elpa.tar.gz archive"
	@echo "  standalone     - Create standalone .emacs.d.tar.gz"
	@echo "  run-debug      - Run Emacs with debug init"
	@echo "  init           - Initialize Emacs configuration"
	@echo "  help           - Show this help message"

# Clean all .elc files and tar.gz archives
clean:
	@echo "Cleaning all .elc files..."
	@cd $$HOME/.emacs.d && find . -name "*.elc" -type f -delete
	@echo "Cleaning all .tar.gz files in current directory..."
	@rm -f *.tar.gz

# Create elpa.tar.gz archive
elpa:
	@echo "Creating elpa.tar.gz archive..."
	@cd $$HOME/.emacs.d && find . -name "*.elc" -type f -delete
	@cd $$HOME/.emacs.d && tar czf elpa.tar.gz elpa
	@mv $$HOME/.emacs.d/elpa.tar.gz .

# Create standalone .emacs.d.tar.gz
standalone:
	@echo "Creating standalone .emacs.d.tar.gz..."
	@SCRIPT_DIR="$$(cd -- "$$(dirname -- "$${BASH_SOURCE[0]}")" &> /dev/null && pwd -P)"; \
	TEMP_DIR=$$(mktemp -d); \
	echo "Script directory: $$SCRIPT_DIR"; \
	echo "Temp directory: $$TEMP_DIR"; \
	cp -r $$SCRIPT_DIR $$TEMP_DIR/.emacs.d; \
	rm -rf $$TEMP_DIR/.emacs.d/.git; \
	rm -rf $$TEMP_DIR/.emacs.d/*.tar.gz; \
	cd $$HOME/.emacs.d && find . -name "*.elc" -type f -delete; \
	cp -r elpa $$TEMP_DIR/.emacs.d; \
	cd $$TEMP_DIR && tar czf .emacs.d.tar.gz .emacs.d; \
	mv $$TEMP_DIR/.emacs.d.tar.gz $$SCRIPT_DIR; \
	rm -rf $$TEMP_DIR

# Run Emacs with debug init
run-debug:
	@echo "Running Emacs with debug init..."
	@SRC_DIR="$$( cd "$$( dirname "$${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"; \
	emacs -q --load $$SRC_DIR/init.el --debug-init

# Initialize Emacs configuration
init:
	@echo "Initializing Emacs configuration..."
	@SCRIPT_DIR="$$(cd -- "$$(dirname -- "$${BASH_SOURCE[0]}")" &> /dev/null && pwd -P)"; \
	TARGET="$$HOME/.emacs.d/init.el"; \
	if [ -f "$$TARGET" ]; then \
		echo "Target file exists."; \
	else \
		cp templates/init.el $$TARGET; \
		sed -i "7 i\(setopt +emacs/repo-directory (expand-file-name \"$$SCRIPT_DIR\"))" $$TARGET; \
	fi