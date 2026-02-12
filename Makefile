# Configuration
CONFIG_DIR := $(shell pwd)
BUILD_DIR := $(CONFIG_DIR)/_build

# Colors for status output
GREEN := \033[0;32m
ORANGE := \033[0;33m
RED := \033[0;31m
NC := \033[0m

# File mappings and lists
SOURCE_FILES := clang-format/clang-format emacs/emacs git/gitconfig git/gitignore shell/bashrc shell/zshrc tmux/tmux
XMONAD_FILES := xmonad/xmonad.hs
VALGRIND_FILES := valgrind/valgrindrc
BUILD_DIRS := emacs.d valgrind xmonad config
LINK_DIRS := emacs.d xmonad
LINK_FILES := bashrc clang-format emacs emacs.elc gitconfig gitignore tmux.conf valgrindrc zshrc
CONFIG_ITEMS := gtk-3.0 terminator warp-terminal
ALL_DOTFILES := $(LINK_FILES) $(LINK_DIRS)

# Default target when running 'make' without arguments
.DEFAULT_GOAL := usage

.PHONY: usage help
usage help:
	@echo "Config files Management"
	@echo ""
	@echo "Usage:"
	@echo "  make build     - Build and install config files"
	@echo "  make clean     - Remove installed config files"
	@echo "  make status    - Show current config files status"
	@echo ""
	@echo "Configuration:"
	@echo "  CONFIG_DIR = $(CONFIG_DIR)"
	@echo "  BUILD_DIR  = $(BUILD_DIR)"

.PHONY: build
build:
	@echo "Building config files..."
	@mkdir -p $(BUILD_DIR)
	@$(foreach dir,$(BUILD_DIRS),mkdir -p $(BUILD_DIR)/$(dir);)

	# Copy source files
	@$(foreach file,$(SOURCE_FILES), \
		if [ -f "src/$(file)" ]; then \
			cp "src/$(file)" "$(BUILD_DIR)/$(notdir $(file))"; \
		fi;)

	@$(foreach file,$(XMONAD_FILES), \
		if [ -f "src/$(file)" ]; then \
			cp "src/$(file)" "$(BUILD_DIR)/$(file)"; \
		fi;)

	@$(foreach file,$(VALGRIND_FILES), \
		if [ -f "src/$(file)" ]; then \
			cp "src/$(file)" "$(BUILD_DIR)/$(notdir $(file))"; \
		fi;)

	# Handle tmux naming
	@if [ -f "$(BUILD_DIR)/tmux" ]; then \
		mv "$(BUILD_DIR)/tmux" "$(BUILD_DIR)/tmux.conf"; \
	fi

	# Copy valgrind directory if it exists
	@if [ -d "src/valgrind/valgrind" ]; then \
		cp -r "src/valgrind/valgrind" "$(BUILD_DIR)/"; \
	fi

	# Copy config directory structures
	@if [ -d "src/config" ]; then \
		$(foreach item,$(CONFIG_ITEMS), \
			if [ -e "src/config/$(item)" ]; then \
				cp -r "src/config/$(item)" "$(BUILD_DIR)/config"; \
			fi;) \
	fi

	# Link files
	@$(foreach file,$(LINK_FILES), \
		if [ -f "$(BUILD_DIR)/$(file)" ]; then \
			ln -sf "$(BUILD_DIR)/$(file)" "$(HOME)/.$(file)"; \
		fi;)

	# Link directories
	@$(foreach dir,$(LINK_DIRS), \
		if [ -d "$(BUILD_DIR)/$(dir)" ]; then \
			ln -sfn "$(BUILD_DIR)/$(dir)" "$(HOME)/.$(dir)"; \
		fi;)

	# Link config items
	@$(foreach item,$(CONFIG_ITEMS), \
		if [ -e "$(BUILD_DIR)/config/$(item)" ]; then \
			ln -sfn "$(BUILD_DIR)/config/$(item)" "$(HOME)/.config/$(item)"; \
		fi;)

	# Compile Emacs configuration
	@if [ -f "$(BUILD_DIR)/emacs" ] && command -v emacs >/dev/null 2>&1; then \
		cd "$(BUILD_DIR)" && rm -f "emacs.elc" && emacs --batch --eval "(byte-compile-file \"$(BUILD_DIR)/emacs\")" 2>/dev/null || echo "Warning: Emacs compilation failed"; \
	else \
		echo "Warning: emacs not found, skipping compilation"; \
	fi

	# Compile XMonad configuration
	@if [ -f "$(BUILD_DIR)/xmonad/xmonad.hs" ] && command -v xmonad >/dev/null 2>&1; then \
		cd "$(BUILD_DIR)/xmonad" && xmonad --recompile 2>/dev/null || echo "Warning: XMonad compilation failed"; \
	else \
		echo "Warning: xmonad not found, skipping compilation"; \
	fi

	# Setup git and gsettings if this is a fresh build
	@if [ ! -d "$(BUILD_DIR)/.git" ]; then \
		echo "First-time setup: creating .git directory..."; \
		git init "$(BUILD_DIR)"; \
		echo "bin" > "$(BUILD_DIR)/.gitignore"; \
		echo "emacs.elc" >> "$(BUILD_DIR)/.gitignore"; \
		echo "emacs.d" >> "$(BUILD_DIR)/.gitignore"; \
		echo "xmonad/xmonad.errors" >> "$(BUILD_DIR)/.gitignore"; \
		echo "xmonad/xmonad.hi" >> "$(BUILD_DIR)/.gitignore"; \
		echo "xmonad/xmonad.o" >> "$(BUILD_DIR)/.gitignore"; \
		echo "xmonad/xmonad.state" >> "$(BUILD_DIR)/.gitignore"; \
		echo "xmonad/xmonad-x86_64-linux" >> "$(BUILD_DIR)/.gitignore"; \
		if [ ! -f "$(BUILD_DIR)/gitconfig.private" ]; then \
			echo "Creating gitconfig.private file..."; \
			read -p "Enter your Git email: " git_email; \
			read -p "Enter your Git name: " git_name; \
			echo "[user]" > "$(BUILD_DIR)/gitconfig.private"; \
			echo "	email = $$git_email" >> "$(BUILD_DIR)/gitconfig.private"; \
			echo "	name = $$git_name" >> "$(BUILD_DIR)/gitconfig.private"; \
		fi; \
		echo "Configuring gsettings..."; \
		cd "$(BUILD_DIR)"; \
		git add .; \
		git commit -m "Initial commit"; \
		cd ..; \
		gsettings set org.mate.session.required-components windowmanager xmonad || echo "Warning: gsettings 1 failed"; \
		gsettings set org.mate.session required-components-list "['windowmanager', 'panel']" || echo "Warning: gsettings 2 failed"; \
		gsettings set org.mate.mate-menu hot-key '' || echo "Warning: gsettings 3 failed"; \
		gsettings set com.solus-project.brisk-menu hot-key '' || echo "Warning: gsettings 4 failed"; \
	fi

	@echo "Build complete!"

.PHONY: status
status:
	@echo "Config symlinks status:"
	@$(foreach file,$(ALL_DOTFILES), \
		if [ -L "$(HOME)/.$(file)" ]; then \
			echo "  $(GREEN)✓$(NC) .$(file) -> $$(readlink $(HOME)/.$(file))"; \
		elif [ -e "$(HOME)/.$(file)" ]; then \
			echo "  $(ORANGE)⚠$(NC) .$(file) (exists but not a symlink)"; \
		else \
			echo "  $(RED)✗$(NC) .$(file) (missing)"; \
		fi;)
	@echo ""
	@echo "Config symlinks:"
	@$(foreach item,$(CONFIG_ITEMS), \
		if [ -L "$(HOME)/.config/$(item)" ]; then \
			echo "  $(GREEN)✓$(NC) .config/$(item) -> $$(readlink $(HOME)/.config/$(item))"; \
		elif [ -e "$(HOME)/.config/$(item)" ]; then \
			echo "  $(ORANGE)⚠$(NC) .config/$(item) (exists but not a symlink)"; \
		else \
			echo "  $(RED)✗$(NC) .config/$(item) (missing)"; \
		fi;)

.PHONY: clean
clean:
	@echo "Removing config files..."
	@$(foreach file,$(ALL_DOTFILES), \
		if [ -L "$(HOME)/.$(file)" ]; then \
			rm -f "$(HOME)/.$(file)"; \
			echo "Removed link: .$(file)"; \
		elif [ -e "$(HOME)/.$(file)" ]; then \
			echo "Warning: $(HOME)/.$(file) exists but is not a symlink - skipping"; \
		fi;)

	@$(foreach item,$(CONFIG_ITEMS), \
		if [ -L "$(HOME)/.config/$(item)" ]; then \
			rm -f "$(HOME)/.config/$(item)"; \
			echo "Removed link: .config/$(item)"; \
		elif [ -e "$(HOME)/.config/$(item)" ]; then \
			echo "Warning: $(HOME)/.config/$(item) exists but is not a symlink - skipping"; \
		fi;)

	@if [ -d "$(BUILD_DIR)" ]; then \
		rm -rf "$(BUILD_DIR)"; \
		echo "Removed build directory"; \
	fi
	@echo "Clean complete"
