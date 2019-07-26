.PHONY: usage
usage:
	@echo 'Run `make build` to generate the files and the links in {HOME}'

.PHONY: build
build: SHELL := /bin/bash
build:
	mkdir -p ${HOME}/config/_build
	mkdir -p ${HOME}/config/_build/emacs.d
	cp ${HOME}/config/src/emacs/emacs ${HOME}/config/_build
	cp ${HOME}/config/src/gdb/gdbinit ${HOME}/config/_build
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
	ln -sf ${HOME}/config/_build/gdbinit ${HOME}/.gdbinit
	ln -sf ${HOME}/config/_build/gitconfig ${HOME}/.gitconfig
	ln -sf ${HOME}/config/_build/gitignore ${HOME}/.gitignore
	ln -sf ${HOME}/config/_build/tmux ${HOME}/.tmux.conf
	ln -sf ${HOME}/config/_build/valgrindrc ${HOME}/.valgrindrc
	ln -sf ${HOME}/config/_build/zshrc ${HOME}/.zshrc
	ln -sf ${HOME}/config/_build/xmonad/xmonad.hs ${HOME}/.xmonad.hs
