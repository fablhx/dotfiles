.PHONY: usage
usage:
	@echo 'Run `make build` to generate the files and the links in {HOME}'

.PHONY: build
build: SHELL := /bin/bash
build:
	mkdir -p ${HOME}/config/_build
	mkdir -p ${HOME}/config/_build/emacs.d
	cp ${HOME}/config/src/emacs/emacs ${HOME}/config/_build
	cp ${HOME}/config/src/git/gitconfig ${HOME}/config/_build
	cp ${HOME}/config/src/git/gitignore ${HOME}/config/_build
	cp ${HOME}/config/src/shell/bashrc ${HOME}/config/_build
	cp ${HOME}/config/src/shell/zshrc ${HOME}/config/_build
	cp ${HOME}/config/src/tmux/tmux ${HOME}/config/_build
	cp ${HOME}/config/src/valgrind/valgrindrc ${HOME}/config/_build
	cp -R ${HOME}/config/src/valgrind/valgrind ${HOME}/config/_build
	cp -R ${HOME}/config/src/xmonad ${HOME}/config/_build
	ln -sf ${HOME}/config/_build/bashrc ${HOME}/.bashrc
	ln -sf ${HOME}/config/_build/emacs ${HOME}/.emacs
	if [ ! -L ${HOME}/.emacs.d ]; then ln -s ${HOME}/config/_build/emacs.d ${HOME}/.emacs.d; fi
	ln -sf ${HOME}/work/git/extern/gdb-dashboard/.gdbinit ${HOME}/.gdbinit
	ln -sf ${HOME}/config/_build/gitconfig ${HOME}/.gitconfig
	ln -sf ${HOME}/config/_build/gitignore ${HOME}/.gitignore
	ln -sf ${HOME}/config/_build/tmux ${HOME}/.tmux.conf
	ln -sf ${HOME}/config/_build/valgrindrc ${HOME}/.valgrindrc
	ln -sf ${HOME}/config/_build/zshrc ${HOME}/.zshrc
	ln -sf ${HOME}/config/_build/xmonad/xmonad.hs ${HOME}/.xmonad.hs
	ln -sf ${HOME}/config/_build/xmonad/xmobarrc ${HOME}/.xmobarrc
	if [ ! -L ${HOME}/.xmonad ]; then ln -s ${HOME}/config/_build/xmonad ${HOME}/.xmonad; fi

.PHONY: clean
clean: SHELL := /bin/bash
clean:
	rm -f ${HOME}/.bashrc
	rm -f ${HOME}/.emacs
	rm -rf ${HOME}/.emacs.d
	rm -f ${HOME}/.gdbinit
	rm -f ${HOME}/.gitconfig
	rm -f ${HOME}/.gitignore
	rm -f ${HOME}/.tmux.conf
	rm -f ${HOME}/.valgrindrc
	rm -f ${HOME}/.zshrc
	rm -f ${HOME}/.xmonad.hs
	rm -f ${HOME}/.xmobarrc
	rm -rf ${HOME}/.xmonad
