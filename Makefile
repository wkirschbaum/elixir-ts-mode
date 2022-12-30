.PHONY: test

dist: build.sh
	chmod +x build.sh
	mkdir -p dist
	./build.sh elixir
	./build.sh heex

build.sh:
	curl -s https://raw.githubusercontent.com/casouri/tree-sitter-module/master/build.sh -o build.sh

test: dist
	emacs -batch -l ert \
	-l heex-ts-mode.el \
	-l elixir-ts-mode.el \
	-l ./test/elixir-ts-tests.el \
	--eval "(add-to-list 'treesit-extra-load-path \"./dist\")" \
	-f ert-run-tests-batch-and-exit

clean:
	rm -rf build.sh dist

version:
	emacs --version

