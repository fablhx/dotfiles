#!/usr/bin/env bash

set -Eeuo pipefail

APT_PKG=(
  afl++ # ubuntu 20.04
  bear
  cabal-install
  cloc
  docker.io
  emacs
  git
  gnome-session-xmonad
  google-chrome-stable
  htop
  hunspell
  hunspell-en-us
  hunspell-fr
  hunspell-fr-classical
  #hyperfine # TODO
  icdiff
  nginx
  openssh-client
  openssh-server
  pylint3
  python3-pip
  ripgrep
  silversearcher-ag
  ssh
  terminator
  tmux
  tree
  xclip
  xmobar
  xmonad
  zsh
)

echo -e "\033[36mInstall dependencies? [y,N]\033[0m"
read -re answer
if [[ $answer =~ ^([yY][eE][sS]|[yY])$ ]]; then
  sudo sh -c 'echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list'
  wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -

  sudo add-apt-repository ppa:mattrose/terminator

  sudo add-apt-repository ppa:gekkio/xmonad

  sudo apt update

  sudo apt install "${APT_PKG[@]}"

  cabal update

  mkdir -p "${HOME}/work/git/extern"
  pushd "${HOME}/work/git/extern"
  git clone https://github.com/koalaman/shellcheck.git
  pushd shellcheck
  cabal install
  popd
  git clone https://github.com/cyrus-and/gdb-dashboard.git
  popd

  wget --output-document=oh-my-zsh-install.sh \
       https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh
  sed -i'' 's/exec zsh -l/#exec zsh -l/g' oh-my-zsh-install.sh
  sh oh-my-zsh-install.sh
  pushd "${HOME}/.oh-my-zsh/custom/plugins/"
  git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
  popd
fi

echo -e "\033[36mInstall my config? [y,N]\033[0m"
read -re answer
if [[ $answer =~ ^([yY][eE][sS]|[yY])$ ]]; then
  git clone git@github.com:fablhx/dotfiles.git "${HOME}/config"
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
  cd "${HOME}"
  #source "${HOME}/.zshrc"

  #TODO
  # create the files to run xmonad:
  #/usr/share/applications/xmonad.desktop
  #/usr/share/gnome-session/sessions/xmonad.session
  #/usr/share/xsessions/xmonad.desktop
  # see also: https://github.com/Gekkio/gnome-session-xmonad

  echo -e "\033[36m[Info]\033[0m Remaining manual things to do:"
  echo "	- Start a git repo in ${HOME}/config/_build"
  echo "	- Create the file ${HOME}/config/_build/gitconfig.private"
  echo "	- Start emacs to download the packages"
fi

echo -e "\033[36mRemove Raspberry-Pi unnecessary packages? [y,N]\033[0m"
read -re answer
if [[ $answer =~ ^([yY][eE][sS]|[yY])$ ]]; then
  sudo apt-get purge \
       geany* \
       idle-python* \
       libreoffice* \
       minecraft-pi \
       python-pygame \
       python3-thonny \
       scratch* \
       sonic-pi

  rm -rf "${HOME}/python_games"
fi

sudo apt autoremove
