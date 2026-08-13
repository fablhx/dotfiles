# =============================================================================
#                        Config files Management
# =============================================================================
# src/ is the deployed configuration, symlinked straight into $HOME. There is
# no staging copy: the file Emacs or bash reads *is* the file in this
# repository, so "edited but never rebuilt" cannot happen and `make build` is
# only ever creating links and recompiling xmonad.
#
# Every entry below links a FILE, never a directory. Applications own their
# own directories (~/.config/warp-terminal, ~/.xmonad) and write runtime state
# into them; replacing one with a symlink either destroys that state or, when
# the directory already exists, silently creates the link *inside* it.
#
# Recipes run under bash with `set -e` and .ONESHELL, so a failing step aborts
# the target instead of being swallowed by a chain of `if`s.
# =============================================================================

SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
.ONESHELL:

CONFIG_DIR := $(CURDIR)

# Colors for status output
GREEN  := \033[0;32m
ORANGE := \033[0;33m
RED    := \033[0;31m
NC     := \033[0m

# <path under src/>:<path under $HOME>
LINKS := \
  shell/bashrc:.bashrc \
  clang-format/clang-format:.clang-format \
  emacs/emacs:.emacs \
  git/gitconfig:.gitconfig \
  git/gitignore:.gitignore \
  tmux/tmux.conf:.tmux.conf \
  xmonad/xmonad.hs:.xmonad/xmonad.hs \
  config/warp-terminal/keybindings.yaml:.config/warp-terminal/keybindings.yaml \
  config/warp-terminal/settings.toml:.config/warp-terminal/settings.toml

# Machine-local, never tracked here. Plain files in $HOME: they are read
# through no indirection, survive every target below, and are the only part of
# the setup this repository cannot recreate.
PRIVATE_FILES := .bashrc.private .gitconfig.private

.DEFAULT_GOAL := usage

.PHONY: usage help build status lint clean mate-session

usage help:
	@echo "Config files Management"
	@echo ""
	@echo "Usage:"
	@echo "  make build        - Symlink src/ into \$$HOME and recompile xmonad"
	@echo "  make status       - Show every symlink and private file"
	@echo "  make lint         - Static-check src/ (shellcheck, emacs, ghc, tmux, git)"
	@echo "  make clean        - Remove the symlinks this repository installed"
	@echo "  make mate-session - Point the MATE session at XMonad (gsettings)"
	@echo ""
	@echo "Private files, kept in \$$HOME and never tracked:"
	@echo "  $(PRIVATE_FILES)"

# -----------------------------------------------------------------------------
# Shell helpers emitted into the recipes that need them.
#
# link_path refuses to touch anything that is not already a symlink: `ln -sfn`
# against a real directory creates the link inside it instead of replacing it.
# A skipped link must not abort the build, so this returns rather than exits.
#
# prune_foreign removes symlinks that point into this repository but are not in
# LINKS. That is what a file deleted from src/ leaves behind, and without it a
# removed tool stays deployed forever - ~/.zshrc and ~/.valgrindrc outlived
# their sources by weeks that way.
# -----------------------------------------------------------------------------
define SHELL_HELPERS
link_path() {
  local target="$$1" link="$$2"
  if [ -L "$$link" ]; then
    rm -f "$$link"
  elif [ -e "$$link" ]; then
    echo -e "  $(ORANGE)skip$(NC)    $$link exists and is not a symlink"
    echo    "          move it aside and re-run: mv '$$link' '$$link.bak'"
    return 0
  fi
  mkdir -p "$$(dirname "$$link")"
  ln -s "$$target" "$$link"
  echo -e "  $(GREEN)linked$(NC)  $$link"
  return 0
}

managed_destinations() {
  for pair in $(LINKS); do echo "$$HOME/$${pair#*:}"; done
}

prune_foreign() {
  local managed link resolved
  managed="$$(managed_destinations)"
  shopt -s nullglob
  for link in "$$HOME"/.[!.]* "$$HOME"/.config/* "$$HOME"/.config/*/*; do
    [ -L "$$link" ] || continue
    resolved="$$(readlink -m "$$link")"
    case "$$resolved" in
      "$(CONFIG_DIR)"/*) ;;
      *) continue ;;
    esac
    if ! grep -Fxq "$$link" <<< "$$managed"; then
      rm -f "$$link"
      echo -e "  $(ORANGE)pruned$(NC)  $$link (no longer in src/)"
    fi
  done
  shopt -u nullglob
}
endef

build:
	@echo "Linking src/ into $(HOME)..."
	$(SHELL_HELPERS)
	for pair in $(LINKS); do
	  src="src/$${pair%%:*}"
	  dest="$(HOME)/$${pair#*:}"
	  if [ -f "$$src" ]; then
	    link_path "$(CONFIG_DIR)/$$src" "$$dest"
	  else
	    echo -e "  $(RED)missing$(NC) $$src"
	  fi
	done
	prune_foreign

	# Created on demand rather than by a one-shot first-run target, which
	# ossifies: any later change to it never reaches an existing install.
	if [ ! -f "$(HOME)/.gitconfig.private" ]; then
	  if [ -t 0 ]; then
	    echo "Creating $(HOME)/.gitconfig.private"
	    read -r -p "  Git email: " git_email
	    read -r -p "  Git name:  " git_name
	    printf '[user]\n\temail = %s\n\tname = %s\n' "$$git_email" "$$git_name" \
	      > "$(HOME)/.gitconfig.private"
	  else
	    echo -e "  $(ORANGE)skip$(NC)    .gitconfig.private (not a terminal)"
	  fi
	fi
	if [ ! -f "$(HOME)/.bashrc.private" ]; then
	  printf '# Machine-local bashrc overrides, sourced last by ~/.bashrc.\n' \
	    > "$(HOME)/.bashrc.private"
	  echo -e "  $(GREEN)created$(NC) $(HOME)/.bashrc.private"
	fi

	# xmonad reads ~/.xmonad/xmonad.hs (a symlink into src/) and writes its
	# build output beside it, which is why that directory is not itself linked.
	if command -v xmonad >/dev/null 2>&1; then
	  echo "Compiling xmonad config..."
	  xmonad --recompile || echo -e "  $(RED)Warning$(NC): xmonad recompile failed"
	else
	  echo -e "  $(ORANGE)skip$(NC)    xmonad not installed"
	fi

	@echo -e "$(GREEN)Build complete.$(NC)"

status:
	@echo "Symlinks:"
	for pair in $(LINKS); do
	  src="$(CONFIG_DIR)/src/$${pair%%:*}"
	  dest="$(HOME)/$${pair#*:}"
	  name="$${pair#*:}"
	  if [ -L "$$dest" ]; then
	    tgt="$$(readlink "$$dest")"
	    if [ ! -e "$$dest" ]; then
	      echo -e "  $(RED)dangling$(NC) $$name -> $$tgt"
	    elif [ "$$tgt" != "$$src" ]; then
	      echo -e "  $(ORANGE)foreign$(NC)  $$name -> $$tgt"
	    else
	      echo -e "  $(GREEN)ok$(NC)       $$name"
	    fi
	  elif [ -e "$$dest" ]; then
	    echo -e "  $(ORANGE)blocked$(NC)  $$name (exists, not a symlink)"
	  else
	    echo -e "  $(RED)missing$(NC)  $$name"
	  fi
	done

	@echo ""
	@echo "Private files (in \$$HOME, untracked):"
	for p in $(PRIVATE_FILES); do
	  if [ -L "$(HOME)/$$p" ]; then
	    echo -e "  $(ORANGE)symlink$(NC)  $$p (expected a real file)"
	  elif [ -f "$(HOME)/$$p" ]; then
	    echo -e "  $(GREEN)ok$(NC)       $$p ($$(wc -c < "$(HOME)/$$p") bytes)"
	  else
	    echo -e "  $(RED)missing$(NC)  $$p"
	  fi
	done

# Static checks over src/, in a scratch directory so nothing is deployed and no
# .elc lands next to the sources. Emacs is not byte-compiled during build any
# more, but compiling here still catches errors before they reach a live init.
#
# Byte-compilation fails the target on errors only. Warnings are printed and
# counted but tolerated, because a package update can introduce one that is not
# ours to fix; add byte-compile-error-on-warn to tighten that.
lint:
	@failed=0
	tmp=$$(mktemp -d)
	trap 'rm -rf "$$tmp"' EXIT

	echo "bashrc:"
	if command -v shellcheck >/dev/null 2>&1; then
	  # SC1091 only reports that bash-completion and .bashrc.private are not
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

# Removes only what this repository installed. The private files in $HOME, the
# Emacs packages in ~/.emacs.d and the compiled xmonad in ~/.xmonad are not
# ours to delete and are left alone.
clean:
	@echo "Removing symlinks..."
	$(SHELL_HELPERS)
	for pair in $(LINKS); do
	  dest="$(HOME)/$${pair#*:}"
	  if [ -L "$$dest" ]; then
	    rm -f "$$dest"; echo "  removed $${pair#*:}"
	  elif [ -e "$$dest" ]; then
	    echo "  skipped $${pair#*:} (not a symlink)"
	  fi
	done
	prune_foreign
	@echo "Clean complete. Private files, ~/.emacs.d and ~/.xmonad were left alone."

mate-session:
	@if ! command -v gsettings >/dev/null 2>&1; then
	  echo "gsettings not found"; exit 1
	fi
	gsettings set org.mate.session.required-components windowmanager xmonad
	gsettings set org.mate.session required-components-list "['windowmanager', 'panel']"
	gsettings set org.mate.mate-menu hot-key '' || echo "  warning: mate-menu schema absent"
	gsettings set com.solus-project.brisk-menu hot-key '' || echo "  warning: brisk-menu schema absent"
	@echo -e "$(GREEN)MATE session pointed at XMonad.$(NC)"
