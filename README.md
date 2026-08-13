# Configuration Management

Personal dotfiles for Ubuntu MATE 24.04.

`src/` is the tracked source. `make build` copies it into `_build/`, symlinks
the results into `$HOME`, and compiles the Emacs and XMonad configurations.
Build artifacts and installed Emacs packages live in `_build/` and are not
tracked.

```
src/<tool>/<file>  ──copy──►  _build/<file>  ──symlink──►  ~/.<file>
```

## Installation

### 1. Required packages

```bash
sudo apt update && sudo apt install \
  bat eza fd-find git git-lfs ripgrep tree xclip xdg-utils \
  emacs hunspell hunspell-en-us hunspell-fr hunspell-fr-classical \
  clang-format clangd \
  tmux \
  xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev
```

`libghc-xmonad-contrib-dev` is not optional — `src/xmonad/xmonad.hs` imports
from `XMonad.Layout.*`, `XMonad.Actions.*` and `XMonad.Config.Mate`, and the
build fails without it.

### 2. Deploy

```bash
git clone git@github.com:fablhx/dotfiles.git "${HOME}/config"
cd "${HOME}/config"
make build
```

`make build` is idempotent — re-run it after every change to `src/`.

> **Before the first build**, move aside any of these that already exist as
> real files or directories:
> `~/.bashrc ~/.emacs ~/.gitconfig ~/.gitignore ~/.tmux.conf`
> `~/.clang-format ~/.emacs.d ~/.xmonad ~/.config/warp-terminal`
>
> The build refuses to replace a non-symlink and prints `skip` for it, so
> nothing is destroyed — but that config is then simply not deployed. Run
> `make status` afterwards; anything marked `blocked` is not live.

### 3. Post-install (once)

Install the tree-sitter grammars used for Rust, YAML and TypeScript. This needs
`git` and a C compiler and takes a while, which is why it is not done during
startup:

```
M-x my-install-treesit-grammars
```

Install the Nerd Font glyphs used by the modeline:

```
M-x nerd-icons-install-fonts
```

Optional language servers (Emacs picks these up from `PATH` automatically):

```bash
rustup component add rust-analyzer      # Rust
opam install ocaml-lsp-server           # OCaml (otherwise merlin is used)
npm i -g bash-language-server           # Bash
```

If XMonad compilation fails during the build:

```bash
cd "${HOME}/config/_build/xmonad" && xmonad --recompile
```

### 4. MATE session

`make build` applies these on first run; they are listed here for reference:

```bash
gsettings set org.mate.session.required-components windowmanager xmonad
gsettings set org.mate.session required-components-list "['windowmanager', 'panel']"
gsettings set org.mate.mate-menu hot-key ''
gsettings set com.solus-project.brisk-menu hot-key ''
```

To offer "MATE + XMonad" at the login screen, install the desktop entries
(not handled by `make build`, since they are written outside `$HOME`):

```bash
sudo cp src/xmonad/sessions/mate-xmonad.desktop     /usr/share/xsessions/
sudo cp src/xmonad/applications/mate-xmonad.desktop /usr/share/applications/
```

## Usage

| Target | Effect |
| --- | --- |
| `make build` | Copy `src/` → `_build/`, symlink into `$HOME`, compile Emacs + XMonad |
| `make status` | Show every symlink, private file, and any `src/` ↔ `_build/` drift |
| `make check` | Drift only — warns when `src/` was edited but not rebuilt |
| `make lint` | Static-check `src/` before building: shellcheck + `bash -n`, Emacs byte-compilation, `ghc` type-check with unused imports fatal, tmux config load, gitconfig parse. Each tool is skipped when not installed |
| `make clean` | Remove the installed symlinks. **Keeps** `_build/` |
| `make distclean` | `clean`, then delete `_build/` after an explicit `yes` |

`make distclean` destroys every installed Emacs package, the compiled XMonad
binary, and the `_build/` git history. `make clean` does not — use it for
ordinary uninstalls.

## Private / machine-local configuration

These live in `_build/`, which `make build` initialises as its own local git
repository. That gives them version history while keeping them out of this
repository and off its remote — nothing under `_build/` is tracked here.

| File | Purpose |
| --- | --- |
| `_build/gitconfig.private` | `[user]` name and email; included last by `gitconfig`, so it can override anything |
| `_build/bashrc.private` | Sourced at the end of `bashrc` |

`bashrc` resolves its own location through the `$HOME` symlink and sources its
file directly. `gitconfig` cannot do the same: git resolves a relative
`include.path` against the directory of the file it *read* — `~/.gitconfig` —
rather than the `_build/gitconfig` that symlink points at, so a relative
include would look in `$HOME` and silently find nothing. Git does follow a
symlinked include target, so `make build` links `~/.gitconfig.private` to the
real file.

Commit them from inside the build tree:

```bash
cd _build && git add -A && git commit -m "Update private config"
```

`make build` creates `gitconfig.private` interactively on first run, stubs out
`bashrc.private`, and adopts either left in `$HOME` by an earlier layout.
`make clean` never touches them. `make distclean` moves them to `$HOME`
before removing `_build/` — the contents survive and the next `make build`
takes them back, but their history does not, so back up `_build/.git` first if
you care about it.

## Components

| Area | Files | Notes |
| --- | --- | --- |
| Shell | `src/shell/bashrc` | Includes the `gi` git-status helper |
| Editor | `src/emacs/emacs` | Requires Emacs 29.1+; byte-compiled to `emacs.elc`, which Emacs loads in preference to the source |
| Terminal | `src/config/warp-terminal/`, `src/tmux/tmux.conf` | |
| Window manager | `src/xmonad/` | XMonad + MATE; `Win+H` shows the key bindings |
| Version control | `src/git/{gitconfig,gitignore}` | `gitignore` is the global ignore file; `core.excludesfile` points at it and thereby overrides git's XDG default, so all global rules belong there |
| Formatting | `src/clang-format/clang-format` | |

## Development tools

```bash
sudo apt install shellcheck                       # shell linting
sudo apt install texlive-latex-base texlive-fonts-recommended \
                 texlive-fonts-extra texlive-latex-extra   # LaTeX
```
