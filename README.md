# Installation

Latest try on Ubuntu Mate 24.04


```bash
sudo apt update && sudo apt install afl++ bear btop cloc emacs git htop hunspell hunspell-en-us hunspell-fr hunspell-fr-classical icdiff ncdu parallel ripgrep silversearcher-ag ssh tree xclip xmonad zsh
```

```bash
wget --output-document=oh-my-zsh-install.sh https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh
sed -i'' 's/exec zsh -l/#exec zsh -l/g' oh-my-zsh-install.sh
sh oh-my-zsh-install.sh
pushd "${HOME}/.oh-my-zsh/custom/plugins/"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
popd
```

```bash
git clone https://github.com/fablhx/dotfiles.git "${HOME}/config"
cd "${HOME}/config"
make clean
make build
cat > "${HOME}/config/_build/.gitignore" <<EOF
emacs.d
xmonad/xmonad.errors
xmonad/xmonad.hi
xmonad/xmonad.o
xmonad/xmonad.state
xmonad/xmonad-x86_64-linux
EOF

cd "${HOME}/config/_build/xmonad"
xmonad --recompile

gsettings set org.mate.session.required-components windowmanager xmonad
gsettings set org.mate.session required-components-list "['windowmanager', 'panel']"
gsettings set org.mate.mate-menu hot-key ''
gsettings set com.solus-project.brisk-menu hot-key ''
```

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

```bash
sudo apt-get install texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra
```
