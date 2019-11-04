#!/usr/bin/env bash

set -Eeuo pipefail

APT_PKG=(
    afl
    bear
    cabal-install
    cloc
    docker.io
    emacs
    git
    google-chrome-stable
    htop
    hunspell
    hunspell-en-us
    hunspell-fr
    hunspell-fr-classical
    icdiff
    nginx
    openssh-client
    openssh-server
    pylint3
    python3-pip
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

    sudo add-apt-repository ppa:gnome-terminator/nightly-gtk3

    sudo apt-get update

    sudo apt install "${APT_PKG[@]}"

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
