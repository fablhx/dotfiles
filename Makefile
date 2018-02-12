.PHONY: usage
usage:
	@echo 'Run `make build` to generate the files'
	@echo 'Run `make links` to generate the links in {HOME}'

define generate_file =
  gen_filename=$1 \
  config_dir=$2\
  fname=${gen_filename##*/} \
  fname=${fname%.*} \
  concat_file=$fname.concat \
  cp $gen_filename $config_dir/$fname \
  if [ -e "$config_dir/private/$concat_file" ]; then \
    cat $config_dir/private/$concat_file >> $config_dir/$fname \
  fi \
endef

generate-file: ; $(value generate_file)

.ONESHELL:


.PHONY: build
build:
	generate-file ${HOME}/config/src/emacs/emacs ${HOME}/config/_build
	generate-file ${HOME}/config/src/gdb/gdbinit ${HOME}/config/_build
	generate-file ${HOME}/config/src/git/gitconfig ${HOME}/config/_build
	generate-file ${HOME}/config/src/git/gitignore ${HOME}/config/_build
	generate-file ${HOME}/config/src/shell/aliases ${HOME}/config/_build
	generate-file ${HOME}/config/src/shell/export ${HOME}/config/_build
	generate-file ${HOME}/config/src/shell/functions ${HOME}/config/_build
	generate-file ${HOME}/config/src/shell/gitstatus.py ${HOME}/config/_build
	generate-file ${HOME}/config/src/shell/prompt ${HOME}/config/_build
	generate-file ${HOME}/config/src/shell/zshrc ${HOME}/config/_build
	generate-file ${HOME}/config/src/valgrind/valgrindrc ${HOME}/config/_build
	generate-file ${HOME}/config/src/valgrind/valgrind ${HOME}/config/_build

.PHONY: links
links:
	ln -sf ${HOME}/config/_build/aliases ${HOME}/.aliases
	ln -sf ${HOME}/config/_build/emacs ${HOME}/.emacs
	ln -sf ${HOME}/config/_build/emacs.d ${HOME}/.emacs.d
	ln -sf ${HOME}/config/_build/export ${HOME}/.export
	ln -sf ${HOME}/config/_build/functions ${HOME}/.functions
	ln -sf ${HOME}/config/_build/gdbinit ${HOME}/.gdbinit
	ln -sf ${HOME}/config/_build/gitconfig ${HOME}/.gitconfig
	ln -sf ${HOME}/config/_build/gitignore ${HOME}/.gitignore
	ln -sf ${HOME}/config/_build/gitstatus.py ${HOME}/.gitstatus.py
	ln -sf ${HOME}/config/_build/prompt ${HOME}/.prompt
	ln -sf ${HOME}/config/_build/valgrindrc ${HOME}/.valgrindrc
	ln -sf ${HOME}/config/_build/zshrc ${HOME}/.zshrc
