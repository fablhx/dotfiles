# =============================================================================
#                        Config files Management
# =============================================================================
# src/ is symlinked straight into $HOME; there is no staging copy.
#
# Every entry links a FILE, never a directory. Applications own their own
# directories and write state into them, so linking one either destroys that
# state or, if it already exists, creates the link inside it.
# =============================================================================

SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
.ONESHELL:

CONFIG_DIR := $(CURDIR)

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

# Machine-local, never tracked, untouched by every target below. The only part
# of the setup this repository cannot recreate.
PRIVATE_FILES := .bashrc.private .gitconfig.private

.DEFAULT_GOAL := usage
.PHONY: usage help build status lint clean mate-session

# Shared shell prelude. `say` is the single place that decides how a status
# word is coloured and aligned, so every target reports the same way.
define PRELUDE
say() {
  local kind="$$1"; shift
  local color
  case "$$kind" in
    ok|linked|created)   color=32 ;;
    skip|warn|pruned)    color=33 ;;
    *)                   color=31 ;;
  esac
  printf '  \033[0;%sm%-8s\033[0m %s\n' "$$color" "$$kind" "$$*"
}

have() {
  command -v "$$1" >/dev/null 2>&1 && return 0
  say skip "$$1 not installed"
  return 1
}

# Refuses anything that is not already a symlink, since `ln -sfn` against a
# real directory links inside it. Returns rather than exits, so one skipped
# link does not abort the build.
link_path() {
  local target="$$1" link="$$2"
  if [ -L "$$link" ]; then
    rm -f "$$link"
  elif [ -e "$$link" ]; then
    say skip "$$link exists and is not a symlink; move it aside and re-run"
    return 0
  fi
  mkdir -p "$$(dirname "$$link")"
  ln -s "$$target" "$$link"
  say linked "$$link"
}

# Removes links into this repository that LINKS no longer claims, which is what
# deleting a file from src/ leaves behind.
prune_foreign() {
  local managed link
  managed=$$(for pair in $(LINKS); do echo "$$HOME/$${pair#*:}"; done)
  shopt -s nullglob
  for link in "$$HOME"/.[!.]* "$$HOME"/.config/* "$$HOME"/.config/*/*; do
    [ -L "$$link" ] || continue
    [[ "$$(readlink -m "$$link")" == "$(CONFIG_DIR)"/* ]] || continue
    if ! grep -Fxq "$$link" <<< "$$managed"; then
      rm -f "$$link"
      say pruned "$$link (no longer in src/)"
    fi
  done
  shopt -u nullglob
}
endef

usage help:
	@echo "Config files Management"
	@echo ""
	@echo "  make build        - Symlink src/ into \$$HOME and recompile xmonad"
	@echo "  make status       - Show every symlink and private file"
	@echo "  make lint         - Static-check src/ (shellcheck, emacs, ghc, tmux, git)"
	@echo "  make clean        - Remove the symlinks this repository installed"
	@echo "  make mate-session - Point the MATE session at XMonad (gsettings)"
	@echo ""
	@echo "Private files, kept in \$$HOME and never tracked:"
	@echo "  $(PRIVATE_FILES)"

build:
	@$(PRELUDE)
	echo "Linking src/ into $(HOME)..."
	for pair in $(LINKS); do
	  src="$(CONFIG_DIR)/src/$${pair%%:*}"
	  if [ -f "$$src" ]; then
	    link_path "$$src" "$(HOME)/$${pair#*:}"
	  else
	    say missing "src/$${pair%%:*}"
	  fi
	done
	prune_foreign

	# On demand, not from a first-run target: that ossifies, and later changes
	# to it never reach an existing install.
	if [ ! -f "$(HOME)/.gitconfig.private" ] && [ -t 0 ]; then
	  read -r -p "  Git email: " email
	  read -r -p "  Git name:  " name
	  printf '[user]\n\temail = %s\n\tname = %s\n' "$$email" "$$name" \
	    > "$(HOME)/.gitconfig.private"
	  say created "$(HOME)/.gitconfig.private"
	fi
	if [ ! -f "$(HOME)/.bashrc.private" ]; then
	  echo '# Machine-local bashrc overrides, sourced last by ~/.bashrc.' \
	    > "$(HOME)/.bashrc.private"
	  say created "$(HOME)/.bashrc.private"
	fi

	# xmonad writes its build output beside xmonad.hs, which is why ~/.xmonad
	# is a real directory rather than a link.
	if have xmonad; then
	  xmonad --recompile >/dev/null && say ok "xmonad recompiled" \
	    || say fail "xmonad recompile"
	fi
	printf '\033[0;32mBuild complete.\033[0m\n'

status:
	@$(PRELUDE)
	echo "Symlinks:"
	for pair in $(LINKS); do
	  want="$(CONFIG_DIR)/src/$${pair%%:*}"
	  dest="$(HOME)/$${pair#*:}"
	  name="$${pair#*:}"
	  if [ ! -e "$$dest" ] && [ ! -L "$$dest" ]; then
	    say missing "$$name"
	  elif [ ! -L "$$dest" ]; then
	    say blocked "$$name (exists, not a symlink)"
	  elif [ "$$(readlink "$$dest")" != "$$want" ]; then
	    say wrong "$$name -> $$(readlink "$$dest")"
	  else
	    say ok "$$name"
	  fi
	done

	echo ""
	echo "Private files (in \$$HOME, untracked):"
	for p in $(PRIVATE_FILES); do
	  if [ -f "$(HOME)/$$p" ] && [ ! -L "$(HOME)/$$p" ]; then
	    say ok "$$p ($$(wc -c < "$(HOME)/$$p") bytes)"
	  else
	    say missing "$$p"
	  fi
	done

# Static checks over src/, in a scratch directory so no .elc lands beside the
# sources. Byte-compilation fails on errors only: warnings are counted but
# tolerated, since a package update can introduce one that is not ours to fix.
# Add byte-compile-error-on-warn to tighten that.
lint:
	@$(PRELUDE)
	failed=0
	tmp=$$(mktemp -d)
	trap 'rm -rf "$$tmp"' EXIT

	# Runs a command, reporting pass or fail and showing output only on failure.
	try() {
	  local label="$$1" out; shift
	  if out=$$("$$@" 2>&1); then
	    say ok "$$label"
	  else
	    if [ -n "$$out" ]; then printf '%s\n' "$$out" | sed 's/^/          /'; fi
	    say fail "$$label"
	    failed=1
	  fi
	}

	echo "bashrc:"
	# SC1091 only reports sourced files missing at lint time; both exist at runtime.
	if have shellcheck; then
	  try shellcheck shellcheck -s bash -e SC1091 src/shell/bashrc
	fi
	try "bash -n" bash -n src/shell/bashrc

	echo "emacs:"
	if have emacs; then
	  cp src/emacs/emacs "$$tmp/init.el"
	  if out=$$(emacs --batch -f package-initialize \
	                  -f batch-byte-compile "$$tmp/init.el" 2>&1); then
	    warnings=$$(printf '%s' "$$out" | grep -c 'Warning:' || true)
	    if [ "$$warnings" -eq 0 ]; then
	      say ok "byte-compile"
	    else
	      printf '%s\n' "$$out" | grep 'Warning:' | sed 's|^.*/init.el:|  src/emacs/emacs:|'
	      say warn "byte-compile: $$warnings warning(s)"
	    fi
	  else
	    printf '%s\n' "$$out"
	    say fail "byte-compile"
	    failed=1
	  fi
	fi

	echo "xmonad:"
	if have ghc; then
	  cp src/xmonad/xmonad.hs "$$tmp/"
	  try "ghc type-check" env -C "$$tmp" ghc --make xmonad.hs -fno-code \
	    -outputdir out -Wunused-imports -Werror=unused-imports
	fi

	echo "tmux:"
	# source-file against a running server is the only form that reports a bad
	# option to the caller; `tmux -f bad.conf new-session` still exits 0.
	if have tmux; then
	  tmux -f /dev/null -S "$$tmp/sock" new-session -d -s lint
	  try "config loads" tmux -S "$$tmp/sock" source-file src/tmux/tmux.conf
	  tmux -S "$$tmp/sock" kill-server 2>/dev/null || true
	fi

	echo "git:"
	try "gitconfig parses" git config --list --file src/git/gitconfig

	echo ""
	if [ "$$failed" -eq 0 ]; then
	  printf '\033[0;32mLint passed.\033[0m\n'
	else
	  printf '\033[0;31mLint failed.\033[0m\n'
	  exit 1
	fi

# Removes only what this repository installed; ~/.emacs.d, ~/.xmonad and the
# private files are not ours to delete.
clean:
	@$(PRELUDE)
	echo "Removing symlinks..."
	for pair in $(LINKS); do
	  dest="$(HOME)/$${pair#*:}"
	  if [ -L "$$dest" ]; then
	    rm -f "$$dest"
	    say removed "$${pair#*:}"
	  fi
	done
	prune_foreign
	echo "Private files, ~/.emacs.d and ~/.xmonad were left alone."

mate-session:
	@$(PRELUDE)
	have gsettings || exit 1
	gsettings set org.mate.session.required-components windowmanager xmonad
	gsettings set org.mate.session required-components-list "['windowmanager', 'panel']"
	gsettings set org.mate.mate-menu hot-key '' || say warn "mate-menu schema absent"
	gsettings set com.solus-project.brisk-menu hot-key '' || say warn "brisk-menu schema absent"
	say ok "MATE session pointed at XMonad"
