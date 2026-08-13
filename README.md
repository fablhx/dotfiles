# Configuration Management

Personal dotfiles for Ubuntu MATE 24.04.

`src/` **is** the deployed configuration. `make build` symlinks it straight into
`$HOME`, so the file bash or Emacs reads is the file in this repository —
editing `~/.bashrc` edits `src/shell/bashrc`, and there is no rebuild step to
forget.

```
src/<tool>/<file>  ──symlink──►  ~/.<file>
```

Only files are linked, never directories. Applications own their own
directories and write runtime state into them, so `~/.config/warp-terminal` and
`~/.xmonad` stay real directories with individual files linked inside.

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

`make build` is idempotent. It creates the symlinks, removes any left behind by
files that have since been deleted from `src/`, creates the private files below
if they are missing, and recompiles xmonad.

> **On a machine with existing dotfiles**, move aside anything that is already a
> real file: `~/.bashrc ~/.emacs ~/.gitconfig ~/.gitignore ~/.tmux.conf`
> `~/.clang-format`, and inside `~/.config/warp-terminal`, `keybindings.yaml`
> and `settings.toml`.
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

If xmonad compilation fails during the build:

```bash
cd ~/.xmonad && xmonad --recompile
```

### 4. MATE session

```bash
make mate-session
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
| `make build` | Symlink `src/` into `$HOME`, prune stale links, recompile xmonad |
| `make status` | Show every symlink and private file |
| `make lint` | Static-check `src/`: shellcheck + `bash -n`, Emacs byte-compilation, `ghc` type-check with unused imports fatal, tmux config load, gitconfig parse. Each tool is skipped when not installed |
| `make clean` | Remove the symlinks this repository installed |
| `make mate-session` | Point the MATE session at XMonad |

`make clean` removes links only. `~/.emacs.d`, `~/.xmonad` and the private files
are not this repository's to delete and are left alone.

## Private / machine-local configuration

Two plain files in `$HOME`, untracked here and untouched by every target:

| File | Purpose |
| --- | --- |
| `~/.gitconfig.private` | `[user]` name and email; included last by `gitconfig`, so it can override anything |
| `~/.bashrc.private` | Sourced last by `bashrc` |

`make build` prompts for the git identity the first time and stubs out
`~/.bashrc.private`. They hold nothing this repository can regenerate, so they
are the only part of the setup worth backing up.

## Components

| Area | Files | Notes |
| --- | --- | --- |
| Shell | `src/shell/bashrc` | Includes the `gi` git-status helper |
| Editor | `src/emacs/emacs` | Requires Emacs 29.1+; loaded as source, not byte-compiled |
| Terminal | `src/config/warp-terminal/`, `src/tmux/tmux.conf` | Warp rewrites `settings.toml` itself, so changes made in its UI show up as edits here |
| Window manager | `src/xmonad/` | XMonad + MATE; `Win+H` shows the key bindings. Build output stays in `~/.xmonad` |
| Version control | `src/git/{gitconfig,gitignore}` | `gitignore` is the global ignore file; `core.excludesfile` points at it and thereby overrides git's XDG default, so all global rules belong there |
| Formatting | `src/clang-format/clang-format` | |

## Development tools

```bash
sudo apt install shellcheck                       # shell linting
sudo apt install texlive-latex-base texlive-fonts-recommended \
                 texlive-fonts-extra texlive-latex-extra   # LaTeX
```
