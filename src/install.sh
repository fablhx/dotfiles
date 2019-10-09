#!/usr/bin/env bash

set -euo pipefail

echo -e "\033[36mInstall minimal dependencies? [y,N]\033[0m"
read -re answer
if [[ $answer =~ ^([yY][eE][sS]|[yY])$ ]]; then
    sudo apt install \
         emacs \
         git \
         htop \
         icdiff \
         openssh-client \
         openssh-server \
         silversearcher-ag \
         ssh \
         tmux \
         xmobar \
         xmonad \
         zsh
fi

echo -e "\033[36mInstall full dependencies? [y,N]\033[0m"
read -re answer
if [[ $answer =~ ^([yY][eE][sS]|[yY])$ ]]; then
    sudo sh -c 'echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list'
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -

    sudo add-apt-repository ppa:gnome-terminator/nightly-gtk3

    sudo apt-get update

    sudo apt install \
         afl \
         bear \
         cabal-install \
         cloc \
         docker.io \
         google-chrome-stable \
         hunspell \
         hunspell-en-us \
         hunspell-fr \
         hunspell-fr-classical \
         nginx \
         python3-pip \
         pylint3 \
         terminator \
         tree \
         xclip

    sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${HOME}/.oh-my-zsh/custom/plugins/"

    cabal update

    mkdir -p /home/fabien/work/git/extern
    git clone https://github.com/koalaman/shellcheck.git /home/fabien/work/git/extern/
    pushd /home/fabien/work/git/extern/shellcheck
    cabal install
    popd
fi

echo -e "\033[36mInstall my config? [y,N]\033[0m"
read -re answer
if [[ $answer =~ ^([yY][eE][sS]|[yY])$ ]]; then
    sed -i 's/^XDG_DESKTOP_DIR/#XDG_DESKTOP_DIR/g; s/^XDG_TEMPLATES_DIR/#XDG_TEMPLATES_DIR/g; s/^XDG_PUBLICSHARE_DIR/#XDG_PUBLICSHARE_DIR/g; s/^XDG_MUSIC_DIR/#XDG_MUSIC_DIR/g; s/^XDG_PICTURES_DIR/#XDG_PICTURES_DIR/g; s/^XDG_VIDEOS_DIR/#XDG_VIDEOS_DIR/g' "$HOME"/.config/user-dirs.dirs
    echo 'enable=False' >> "$HOME"/.config/user-dirs.dirs

    rm -f "${HOME}/.zshrc"

    git clone git@github.com:fablhx/dotfiles.git "${HOME}/config"
    cd "${HOME}/config"
    make build
    echo "emacs.d" > "${HOME}/config/_build/.gitignore"
    cd "${HOME}"
    source "${HOME}/.zshrc"
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
