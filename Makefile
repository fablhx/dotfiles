# =============================================================================
#                        Config files Management
# =============================================================================
# src/ is the tracked source. `make build` copies it into _build/ and symlinks
# the results into $HOME. Build artifacts (compiled emacs/xmonad, installed
# packages) live in _build/ and are never tracked.
#
# Recipes run under bash with `set -e` and .ONESHELL, so a failing step aborts
# the target instead of being silently swallowed by a chain of `if`s.
# =============================================================================

SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
.ONESHELL:

CONFIG_DIR := $(CURDIR)
BUILD_DIR  := $(CONFIG_DIR)/_build

# Colors for status output
GREEN  := \033[0;32m
ORANGE := \033[0;33m
RED    := \033[0;31m
NC     := \033[0m

# src/<path> -> _build/<basename>, symlinked to ~/.<basename>
SOURCE_FILES  := clang-format/clang-format emacs/emacs git/gitconfig git/gitignore \
                 shell/bashrc tmux/tmux.conf
XMONAD_FILES  := xmonad/xmonad.hs

BUILD_DIRS := emacs.d xmonad config
LINK_DIRS  := emacs.d xmonad
# emacs.elc is linked by the compile step, not this list, so a failed
# compilation cannot leave a dangling ~/.emacs.elc behind.
LINK_FILES := bashrc clang-format emacs gitconfig gitignore tmux.conf
CONFIG_ITEMS := warp-terminal
ALL_DOTFILES := $(LINK_FILES) emacs.elc $(LINK_DIRS)

# Personal overrides. Untracked by *this* repo, but committed to the git repo
# that lives inside _build - that is the point: they get history without ever
# being pushed here.
#
# bashrc sources its own straight out of _build, resolving its location through
# the symlink. gitconfig cannot do that: git resolves a relative include.path
# against the directory of the file it *read*, which is ~/.gitconfig, not the
# _build/gitconfig that symlink points at - so a relative include would look in
# $HOME and silently find nothing. Git does follow a symlinked include target,
# so gitconfig.private gets one.
PRIVATE_FILES := gitconfig.private bashrc.private
PRIVATE_LINKS := gitconfig.private

.DEFAULT_GOAL := usage

.PHONY: usage help
usage help:
	@echo "Config files Management"
	@echo ""
	@echo "Usage:"
	@echo "  make build     - Build and install config files"
	@echo "  make status    - Show current config files status"
	@echo "  make check     - Report src/ vs _build/ drift"
	@echo "  make lint      - Static-check src/ (shellcheck, byte-compile, ghc, tmux)"
	@echo "  make clean     - Remove installed symlinks (keeps _build)"
	@echo "  make distclean - clean + delete _build (emacs packages, compiled xmonad)"
	@echo ""
	@echo "Private files, versioned inside $(BUILD_DIR) but never pushed:"
	@echo "  $(PRIVATE_FILES)"
	@echo ""
	@echo "Configuration:"
	@echo "  CONFIG_DIR = $(CONFIG_DIR)"
	@echo "  BUILD_DIR  = $(BUILD_DIR)"

# -----------------------------------------------------------------------------
# Shell helper emitted into any recipe that creates symlinks.
#
# `ln -sfn` only replaces a destination that is already a symlink. Against a
# real directory it silently creates the link *inside* it - which is how
# ~/.config/gtk-3.0/gtk-3.0 and ~/.config/warp-terminal/warp-terminal came to
# exist while the actual configs were never deployed, with `make status`
# reporting them as "exists but not a symlink" the whole time.
#
# A skipped link must not abort the build, so this is a function that returns
# rather than a macro that exits.
# -----------------------------------------------------------------------------
define LINK_FN
link_path() {
  local target="$$1" link="$$2"
  if [ -L "$$link" ]; then
    rm -f "$$link"
  elif [ -d "$$link" ] || [ -e "$$link" ]; then
    echo -e "  $(ORANGE)skip$(NC)    $$link exists and is not a symlink"
    echo    "          move it aside and re-run: mv '$$link' '$$link.bak'"
    return 0
  fi
  ln -s "$$target" "$$link"
  echo -e "  $(GREEN)linked$(NC)  $$link"
  return 0
}
endef

.PHONY: build
build: $(BUILD_DIR)
	@echo "Building config files..."
	mkdir -p "$(BUILD_DIR)"
	for d in $(BUILD_DIRS); do mkdir -p "$(BUILD_DIR)/$$d"; done

	# --- Copy sources -------------------------------------------------------
	for f in $(SOURCE_FILES); do
	  if [ -f "src/$$f" ]; then
	    cp "src/$$f" "$(BUILD_DIR)/$$(basename "$$f")"
	  else
	    echo "Warning: missing source src/$$f"
	  fi
	done

	for f in $(XMONAD_FILES); do
	  if [ -f "src/$$f" ]; then cp "src/$$f" "$(BUILD_DIR)/$$f"; fi
	done

	# Copy tracked files in, but never wipe the directory: these are live
	# application config dirs, and the app writes its own runtime state
	# alongside our files (Warp keeps a ~200KB user_preferences.json there).
	# An `rm -rf` here destroys that state on every rebuild.
	for item in $(CONFIG_ITEMS); do
	  if [ -d "src/config/$$item" ]; then
	    mkdir -p "$(BUILD_DIR)/config/$$item"
	    find "src/config/$$item" -mindepth 1 -maxdepth 1 -exec \
	      cp -r {} "$(BUILD_DIR)/config/$$item/" \;
	  fi
	done

	# --- Link ---------------------------------------------------------------
	echo "Linking into $(HOME)..."
	$(LINK_FN)
	for f in $(LINK_FILES); do
	  if [ -f "$(BUILD_DIR)/$$f" ]; then
	    link_path "$(BUILD_DIR)/$$f" "$(HOME)/.$$f"
	  fi
	done

	for d in $(LINK_DIRS); do
	  if [ -d "$(BUILD_DIR)/$$d" ]; then
	    link_path "$(BUILD_DIR)/$$d" "$(HOME)/.$$d"
	  fi
	done

	# Only gitconfig.private needs this; see the PRIVATE_LINKS comment above.
	for p in $(PRIVATE_LINKS); do
	  if [ -f "$(BUILD_DIR)/$$p" ]; then
	    link_path "$(BUILD_DIR)/$$p" "$(HOME)/.$$p"
	  fi
	done

	mkdir -p "$(HOME)/.config"
	for item in $(CONFIG_ITEMS); do
	  if [ -e "$(BUILD_DIR)/config/$$item" ]; then
	    link_path "$(BUILD_DIR)/config/$$item" "$(HOME)/.config/$$item"
	  fi
	done

	# --- Compile ------------------------------------------------------------
	# -f package-initialize matters: batch Emacs does not activate packages
	# (startup.el gates that on user-init-file), so without it the compiler
	# cannot see any installed package and use-package's :ensure tries to
	# reinstall them mid-compile.
	if [ -f "$(BUILD_DIR)/emacs" ] && command -v emacs >/dev/null 2>&1; then
	  echo "Byte-compiling emacs config..."
	  rm -f "$(BUILD_DIR)/emacs.elc" "$(HOME)/.emacs.elc"
	  # batch-byte-compile, not --eval '(byte-compile-file ...)': the latter
	  # returns nil on failure but still exits 0, so the warning below could
	  # never fire and a broken config looked like a successful build.
	  emacs --batch -f package-initialize \
	        -f batch-byte-compile "$(BUILD_DIR)/emacs" \
	    || echo "Warning: Emacs compilation failed (falling back to source config)"
	  if [ -f "$(BUILD_DIR)/emacs.elc" ]; then
	    link_path "$(BUILD_DIR)/emacs.elc" "$(HOME)/.emacs.elc"
	  fi
	else
	  echo "Warning: emacs not found, skipping compilation"
	fi

	if [ -f "$(BUILD_DIR)/xmonad/xmonad.hs" ] && command -v xmonad >/dev/null 2>&1; then
	  echo "Compiling xmonad config..."
	  cd "$(BUILD_DIR)/xmonad" && xmonad --recompile \
	    || echo "Warning: XMonad compilation failed"
	else
	  echo "Warning: xmonad not found, skipping compilation"
	fi

	@echo -e "$(GREEN)Build complete.$(NC)"

# First-time setup: private files, build-tree git repo, MATE session wiring.
$(BUILD_DIR):
	@echo "First-time setup..."
	mkdir -p "$(BUILD_DIR)"
	printf '%s\n' bin emacs.elc emacs.d config/warp-terminal \
	  'xmonad/xmonad.errors' 'xmonad/xmonad.hi' 'xmonad/xmonad.o' \
	  'xmonad/xmonad.state' 'xmonad/build-*' 'xmonad/xmonad-*-linux' \
	  > "$(BUILD_DIR)/.gitignore"

	# Adopt private files stashed in $HOME back into _build. This is the return
	# leg of what `distclean` does before it removes the build tree, and it also
	# picks up files left in $HOME by the interim layout that kept them there.
	for p in $(PRIVATE_FILES); do
	  if [ -f "$(HOME)/.$$p" ] && [ ! -L "$(HOME)/.$$p" ] && [ ! -f "$(BUILD_DIR)/$$p" ]; then
	    echo "Adopting \$$HOME/.$$p into $(BUILD_DIR)/$$p"
	    mv "$(HOME)/.$$p" "$(BUILD_DIR)/$$p"
	  fi
	done

	if [ ! -f "$(BUILD_DIR)/gitconfig.private" ]; then
	  echo "Creating $(BUILD_DIR)/gitconfig.private"
	  read -r -p "Enter your Git email: " git_email
	  read -r -p "Enter your Git name: " git_name
	  printf '[user]\n\temail = %s\n\tname = %s\n' "$$git_email" "$$git_name" \
	    > "$(BUILD_DIR)/gitconfig.private"
	fi

	# bashrc sources this only if present, so an empty stub keeps the
	# "edit this file" story obvious rather than leaving nothing to find.
	if [ ! -f "$(BUILD_DIR)/bashrc.private" ]; then
	  printf '# Machine-local bashrc overrides. Versioned in this build tree only.\n' \
	    > "$(BUILD_DIR)/bashrc.private"
	fi

	if [ ! -d "$(BUILD_DIR)/.git" ] && command -v git >/dev/null 2>&1; then
	  git init -q "$(BUILD_DIR)"
	fi

	if command -v gsettings >/dev/null 2>&1; then
	  echo "Configuring MATE session for XMonad..."
	  gsettings set org.mate.session.required-components windowmanager xmonad || echo "  warning: gsettings windowmanager failed"
	  gsettings set org.mate.session required-components-list "['windowmanager', 'panel']" || echo "  warning: gsettings components-list failed"
	  gsettings set org.mate.mate-menu hot-key '' || echo "  warning: gsettings mate-menu failed"
	  gsettings set com.solus-project.brisk-menu hot-key '' || echo "  warning: gsettings brisk-menu failed"
	fi

.PHONY: status
status:
	@echo "Dotfile symlinks:"
	for f in $(ALL_DOTFILES); do
	  link="$(HOME)/.$$f"
	  if [ -L "$$link" ]; then
	    tgt="$$(readlink "$$link")"
	    if [ -e "$$link" ]; then
	      echo -e "  $(GREEN)ok$(NC)      .$$f -> $$tgt"
	    else
	      echo -e "  $(RED)dangling$(NC) .$$f -> $$tgt"
	    fi
	  elif [ -e "$$link" ]; then
	    echo -e "  $(ORANGE)blocked$(NC)  .$$f (exists, not a symlink)"
	  else
	    echo -e "  $(RED)missing$(NC)  .$$f"
	  fi
	done

	@echo ""
	@echo "~/.config symlinks:"
	for item in $(CONFIG_ITEMS); do
	  link="$(HOME)/.config/$$item"
	  if [ -L "$$link" ]; then
	    tgt="$$(readlink "$$link")"
	    if [ -e "$$link" ]; then
	      echo -e "  $(GREEN)ok$(NC)      .config/$$item -> $$tgt"
	    else
	      echo -e "  $(RED)dangling$(NC) .config/$$item -> $$tgt"
	    fi
	  elif [ -e "$$link" ]; then
	    echo -e "  $(ORANGE)blocked$(NC)  .config/$$item (exists, not a symlink)"
	  else
	    echo -e "  $(RED)missing$(NC)  .config/$$item"
	  fi
	done

	@echo ""
	@echo "Private files (in $(BUILD_DIR), versioned by its own git repo):"
	for p in $(PRIVATE_FILES); do
	  if [ -f "$(BUILD_DIR)/$$p" ]; then
	    echo -e "  $(GREEN)ok$(NC)      $$p"
	  else
	    echo -e "  $(ORANGE)absent$(NC)   $$p"
	  fi
	done
	for p in $(PRIVATE_LINKS); do
	  link="$(HOME)/.$$p"
	  if [ -L "$$link" ] && [ -e "$$link" ]; then
	    echo -e "  $(GREEN)ok$(NC)      .$$p -> $$(readlink "$$link")"
	  elif [ -L "$$link" ]; then
	    echo -e "  $(RED)dangling$(NC) .$$p -> $$(readlink "$$link")"
	  elif [ -e "$$link" ]; then
	    echo -e "  $(ORANGE)blocked$(NC)  .$$p (exists, not a symlink)"
	  else
	    echo -e "  $(RED)missing$(NC)  .$$p (git include will find nothing)"
	  fi
	done
	if [ -d "$(BUILD_DIR)/.git" ]; then
	  uncommitted=$$(cd "$(BUILD_DIR)" && git status --porcelain -- $(PRIVATE_FILES) 2>/dev/null)
	  if [ -n "$$uncommitted" ]; then
	    echo -e "  $(ORANGE)uncommitted$(NC) changes in $(BUILD_DIR): $$uncommitted"
	  fi
	fi

	@$(MAKE) --no-print-directory check

# Catches the failure mode where src/ was edited but `make build` never ran,
# so the deployed (and byte-compiled) config silently lags behind.
.PHONY: check
check:
	@echo ""
	@echo "src/ vs _build/ drift:"
	drift=0
	for f in $(SOURCE_FILES); do
	  src="src/$$f"
	  built="$(BUILD_DIR)/$$(basename "$$f")"
	  if [ -f "$$src" ] && [ -f "$$built" ] && ! cmp -s "$$src" "$$built"; then
	    echo -e "  $(ORANGE)stale$(NC)   $$built"
	    drift=1
	  fi
	done
	for f in $(XMONAD_FILES); do
	  if [ -f "src/$$f" ] && [ -f "$(BUILD_DIR)/$$f" ] && ! cmp -s "src/$$f" "$(BUILD_DIR)/$$f"; then
	    echo -e "  $(ORANGE)stale$(NC)   $(BUILD_DIR)/$$f"
	    drift=1
	  fi
	done
	if [ -f "$(BUILD_DIR)/emacs" ] && [ -f "$(BUILD_DIR)/emacs.elc" ] \
	   && [ "$(BUILD_DIR)/emacs" -nt "$(BUILD_DIR)/emacs.elc" ]; then
	  echo -e "  $(ORANGE)stale$(NC)   emacs.elc is older than emacs (Emacs loads the .elc)"
	  drift=1
	fi
	if [ "$$drift" -eq 0 ]; then
	  echo -e "  $(GREEN)up to date$(NC)"
	else
	  echo    "  run 'make build'"
	fi

# Static checks over src/, in a scratch directory so nothing is deployed and
# no .elc lands next to the sources. Distinct from `check`, which only compares
# src/ against the deployed copy: that notices a change was never built, this
# notices the change was wrong. Every tool is optional and reports skip when
# absent, so this stays runnable on a machine that has only some of them.
#
# Byte-compilation fails the target on errors only. Warnings are printed and
# counted but tolerated, because a package update can introduce one that is not
# ours to fix; add byte-compile-error-on-warn to tighten that.
.PHONY: lint
lint:
	@failed=0
	tmp=$$(mktemp -d)
	trap 'rm -rf "$$tmp"' EXIT

	echo "bashrc:"
	if command -v shellcheck >/dev/null 2>&1; then
	  # SC1091 only reports that bash-completion and bashrc.private are not
	  # readable from here; both exist at runtime.
	  if shellcheck -s bash -e SC1091 src/shell/bashrc; then
	    echo -e "  $(GREEN)ok$(NC)      shellcheck"
	  else
	    echo -e "  $(RED)fail$(NC)    shellcheck"; failed=1
	  fi
	else
	  echo -e "  $(ORANGE)skip$(NC)    shellcheck not installed"
	fi
	if bash -n src/shell/bashrc; then
	  echo -e "  $(GREEN)ok$(NC)      bash -n"
	else
	  echo -e "  $(RED)fail$(NC)    bash -n"; failed=1
	fi

	echo "emacs:"
	if command -v emacs >/dev/null 2>&1; then
	  cp src/emacs/emacs "$$tmp/init.el"
	  if out=$$(emacs --batch -f package-initialize \
	                  -f batch-byte-compile "$$tmp/init.el" 2>&1); then
	    n=$$(printf '%s' "$$out" | grep -c 'Warning:' || true)
	    if [ "$$n" -eq 0 ]; then
	      echo -e "  $(GREEN)ok$(NC)      byte-compile"
	    else
	      printf '%s\n' "$$out" | grep 'Warning:' | sed 's|^.*/init.el:|  src/emacs/emacs:|'
	      echo -e "  $(ORANGE)warn$(NC)    byte-compile: $$n warning(s)"
	    fi
	  else
	    printf '%s\n' "$$out"
	    echo -e "  $(RED)fail$(NC)    byte-compile"; failed=1
	  fi
	else
	  echo -e "  $(ORANGE)skip$(NC)    emacs not installed"
	fi

	echo "xmonad:"
	if command -v ghc >/dev/null 2>&1; then
	  cp src/xmonad/xmonad.hs "$$tmp/"
	  if (cd "$$tmp" && ghc --make xmonad.hs -fno-code -outputdir out \
	        -Wunused-imports -Werror=unused-imports >/dev/null); then
	    echo -e "  $(GREEN)ok$(NC)      ghc type-check"
	  else
	    echo -e "  $(RED)fail$(NC)    ghc type-check"; failed=1
	  fi
	else
	  echo -e "  $(ORANGE)skip$(NC)    ghc not installed"
	fi

	echo "tmux:"
	if command -v tmux >/dev/null 2>&1; then
	  # source-file against an already running server is the only form that
	  # reports a bad option to the caller: `tmux -f bad.conf new-session`
	  # shows the error inside the new session and still exits 0.
	  tmux -f /dev/null -S "$$tmp/tmux.sock" new-session -d -s lint
	  if out=$$(tmux -S "$$tmp/tmux.sock" source-file src/tmux/tmux.conf 2>&1); then
	    echo -e "  $(GREEN)ok$(NC)      config loads"
	  else
	    printf '%s\n' "$$out" | sed 's/^/  /'
	    echo -e "  $(RED)fail$(NC)    config loads"; failed=1
	  fi
	  tmux -S "$$tmp/tmux.sock" kill-server 2>/dev/null || true
	else
	  echo -e "  $(ORANGE)skip$(NC)    tmux not installed"
	fi

	echo "git:"
	if git config --list --file src/git/gitconfig >/dev/null 2>&1; then
	  echo -e "  $(GREEN)ok$(NC)      gitconfig parses"
	else
	  # || true: this rerun exists only to surface the parse error, and under
	  # set -e its non-zero status would abort the target before the summary.
	  git config --list --file src/git/gitconfig 2>&1 >/dev/null | sed 's/^/  /' || true
	  echo -e "  $(RED)fail$(NC)    gitconfig parses"; failed=1
	fi

	echo ""
	if [ "$$failed" -eq 0 ]; then
	  echo -e "$(GREEN)Lint passed.$(NC)"
	else
	  echo -e "$(RED)Lint failed.$(NC)"
	  exit 1
	fi

# Removes the symlinks only. _build (emacs packages, compiled xmonad, the
# build-tree git history) survives; use distclean to remove that too.
.PHONY: clean
clean:
	@echo "Removing config symlinks..."
	for f in $(ALL_DOTFILES); do
	  link="$(HOME)/.$$f"
	  if [ -L "$$link" ]; then
	    rm -f "$$link"; echo "  removed .$$f"
	  elif [ -e "$$link" ]; then
	    echo "  skipped .$$f (not a symlink)"
	  fi
	done
	for item in $(CONFIG_ITEMS); do
	  link="$(HOME)/.config/$$item"
	  if [ -L "$$link" ]; then
	    rm -f "$$link"; echo "  removed .config/$$item"
	  elif [ -e "$$link" ]; then
	    echo "  skipped .config/$$item (not a symlink)"
	  fi
	done
	# Only the link is removed; the file itself stays in _build.
	for p in $(PRIVATE_LINKS); do
	  if [ -L "$(HOME)/.$$p" ]; then
	    rm -f "$(HOME)/.$$p"; echo "  removed .$$p"
	  fi
	done
	@echo "Clean complete (_build kept; run 'make distclean' to remove it)."

.PHONY: distclean
distclean: clean
	@if [ -d "$(BUILD_DIR)" ]; then
	  echo ""
	  echo "This deletes $(BUILD_DIR), including:"
	  echo "  - every installed Emacs package (emacs.d)"
	  echo "  - the compiled xmonad binary"
	  echo "  - the build-tree git history, including every past revision of"
	  echo "    the private files"
	  echo ""
	  echo "The current contents of the private files are moved to \$$HOME first"
	  echo "and adopted back by the next 'make build', so only their history is"
	  echo "lost - but that history is the only copy. Back up $(BUILD_DIR)/.git"
	  echo "first if you want to keep it."
	  read -r -p "Type 'yes' to continue: " confirm
	  if [ "$$confirm" = "yes" ]; then
	    for p in $(PRIVATE_FILES); do
	      if [ -f "$(BUILD_DIR)/$$p" ]; then
	        rm -f "$(HOME)/.$$p"
	        mv "$(BUILD_DIR)/$$p" "$(HOME)/.$$p"
	        echo "  preserved $$p as \$$HOME/.$$p"
	      fi
	    done
	    rm -rf "$(BUILD_DIR)"
	    echo "Removed $(BUILD_DIR)"
	  else
	    echo "Aborted."
	  fi
	fi
