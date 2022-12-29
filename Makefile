.PHONY: test

test: dist
	emacs -batch -l ert \
	-l heex-ts-mode.el \
	-l elixir-ts-mode.el \
	-l ./test/elixir-ts-tests.el \
	--eval "(add-to-list 'treesit-extra-load-path \"./dist\")" \
	-f ert-run-tests-batch-and-exit

dist: build.sh
	mkdir -p dist
	./build.sh elixir
	./build.sh heex

build.sh:
	wget https://raw.githubusercontent.com/casouri/tree-sitter-module/master/build.sh
	chmod +x build.sh

clean:
	rm -rf build.sh dist

version:
	emacs --version

