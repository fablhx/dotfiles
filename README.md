# Configuration Management

Personal dotfiles for Ubuntu MATE 24.04.

## Installation

### 1. Required packages

```bash
sudo apt update && sudo apt install \
  bat eza fd-find git git-lfs jq ripgrep tree xclip xdg-utils \
  emacs hunspell hunspell-en-us hunspell-fr hunspell-fr-classical \
  clang-format clangd \
  tmux \
  xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev
```

### 2. Deploy

```bash
git clone git@github.com:fablhx/dotfiles.git "${HOME}/config"
cd "${HOME}/config"
make build
```

> **On a machine with existing dotfiles**, move aside anything already present
> as a real file: `~/.bashrc ~/.emacs ~/.gitconfig ~/.gitignore ~/.tmux.conf`
> `~/.clang-format`, plus `early-init.el` under `~/.emacs.d`, `keybindings.yaml`
> and `settings.toml` under `~/.config/warp-terminal`, and `CLAUDE.md`,
> `settings.json` and `statusline-command.sh` under `~/.claude`.
>
> The build prints `skip` rather than replacing a non-symlink, so nothing is
> destroyed — but that config is then not deployed. Anything `make status`
> marks `blocked` is not live.

### 3. Post-install (once)

Tree-sitter grammars for Rust, YAML and TypeScript. Needs `git` and a C
compiler, and takes a while, which is why it is not done at startup:

```
M-x my-install-treesit-grammars
```

Nerd Font glyphs for the modeline:

```
M-x nerd-icons-install-fonts
```

One preloaded file of package autoloads, in place of one per package. From
here on package.el rewrites it whenever a package is installed or deleted:

```
M-x package-quickstart-refresh
```

Optional language servers, picked up from `PATH`:

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

To offer "MATE + XMonad" at the login screen, install the desktop entries.
`make build` does not, since they live outside `$HOME`:

```bash
sudo cp src/xmonad/sessions/mate-xmonad.desktop     /usr/share/xsessions/
sudo cp src/xmonad/applications/mate-xmonad.desktop /usr/share/applications/
```

## Usage

| Target | Effect |
| --- | --- |
| `make build` | Symlink `src/` into `$HOME`, prune stale links, recompile xmonad |
| `make status` | Show every symlink and private file |
| `make lint` | Static-check `src/` with shellcheck, `bash -n`, Emacs byte-compilation, `ghc`, tmux and git. Missing tools are skipped |
| `make clean` | Remove the symlinks this repository installed |
| `make mate-session` | Point the MATE session at XMonad |

`make clean` removes links only; `~/.emacs.d`, `~/.xmonad` and the private files
are left alone.

## Private / machine-local configuration

Two plain files in `$HOME`, untracked and untouched by every target. `make build`
prompts for the git identity the first time and stubs out the other. Nothing
here can regenerate them, so they are the only part worth backing up.

| File | Purpose |
| --- | --- |
| `~/.gitconfig.private` | `[user]` name and email; included last, so it overrides anything |
| `~/.bashrc.private` | Sourced last by `bashrc` |

## Development tools

```bash
sudo apt install shellcheck                       # shell linting
sudo apt install texlive-latex-base texlive-fonts-recommended \
                 texlive-fonts-extra texlive-latex-extra   # LaTeX
```
