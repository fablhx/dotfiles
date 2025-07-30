# Configuration Management

Personal dotfiles and configuration management for Ubuntu Mate 24.04.

## Installation

### Prerequisites

Install required packages:

```bash
sudo apt update && sudo apt install afl++ bat bear btop cloc exa emacs git htop hunspell hunspell-en-us hunspell-fr hunspell-fr-classical hyperfine icdiff ncdu parallel ripgrep silversearcher-ag ssh tree xclip xmonad zsh
```

### Additional Tools (TODO)

Consider installing these additional tools:

- [broot](https://github.com/Canop/broot) - Interactive directory tree
- [tokei](https://github.com/XAMPRocky/tokei) - Code statistics tool

### Configuration Deployment

Clone and deploy the configuration:

```bash
git clone https://github.com/fablhx/dotfiles.git "${HOME}/config"
cd "${HOME}/config"
make clean
make build
```

The build process will automatically:
- Copy configuration files to `_build/` directory
- Create symlinks in your home directory
- Compile Emacs and XMonad configurations
- Set up git repository in `_build/`
- Configure gsettings for XMonad

### Manual XMonad Setup

If XMonad compilation fails during build:

```bash
cd "${HOME}/config/_build/xmonad"
xmonad --recompile
```

### MATE Session Configuration

Configure MATE to use XMonad:

```bash
gsettings set org.mate.session.required-components windowmanager xmonad
gsettings set org.mate.session required-components-list "['windowmanager', 'panel']"
gsettings set org.mate.mate-menu hot-key ''
gsettings set com.solus-project.brisk-menu hot-key ''
```

### Development Tools

#### ShellCheck Installation

Install ShellCheck for shell script linting:

```bash
sudo apt install cabal-install
cabal update
mkdir -p "${HOME}/work/git"
pushd "${HOME}/work/git"
git clone https://github.com/koalaman/shellcheck.git
pushd shellcheck
cabal install
popd
popd
```

#### LaTeX Installation

Install LaTeX packages for document processing:

```bash
sudo apt-get install texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra
```

## Usage

After installation, use these make targets:

- `make build` - Build and install configuration files
- `make clean` - Remove installed configuration files
- `make status` - Show current configuration status

## Configuration Components

- **Shell**: Bash and Zsh with custom prompts and Git integration
- **Editor**: Emacs with extensive customization
- **Terminal**: Tmux configuration with custom keybindings
- **Window Manager**: XMonad with MATE integration
- **Version Control**: Git with aliases and enhanced diff tools
- **Development**: Clang-format, Python, OCaml tooling
