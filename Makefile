.PHONY: test

dist: build.sh
	chmod +x build.sh
	./build.sh elixir

build.sh: ./dist/heex-ts-mode.el
	curl -s -N https://raw.githubusercontent.com/casouri/tree-sitter-module/master/build.sh -o build.sh

./dist/heex-ts-mode.el:
	mkdir -p dist
	curl -s -N https://raw.githubusercontent.com/wkirschbaum/heex-ts-mode/main/heex-ts-mode.el -o ./dist/heex-ts-mode.el
  
test: dist
	emacs -batch -l ert \
	-l ./dist/heex-ts-mode.el \
	-l elixir-ts-mode.el \
	-l ./test/elixir-ts-mode-tests.el \
	--eval "(add-to-list 'treesit-extra-load-path \"./dist\"))" \
	-f ert-run-tests-batch-and-exit

clean:
	rm -rf build.sh dist

version:
	emacs --version

