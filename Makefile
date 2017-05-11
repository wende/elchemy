dev:
	elm-make Main.elm --output=example/elm.js
	open -g example/index.html


release:
	elm-make Main.elm --output=example/elm.js
	mkdir -p stable
	cp -r example/ stable/

compile:
	elm-make Main.elm --output compiled.js
	sed 's/var Elm = {}/&; \
	var fs = require(\"fs\"); \
	var a = fs.readFileSync(process.argv[2]).toString(); \
	console.log(_user$$project$$Compiler$$tree(a))/' compiled.js > elmchemy.js
	rm compiled.js

compile-watch:
	find . -name "*.elm" | grep -v "elm-stuff" | grep -v .# | entr make compile

test:
	elm test

test-std:
	cd elmchemy-core/elixir-stuff/elmchemy && mix test

compile-std:
	rm -rf elmchemy-core/elixir-stuff/elmchemy/lib/Elmchemy/*
	cd elmchemy-core && ../elmchemy compile src/Elmchemy/ elixir-stuff/elmchemy/lib/Elmchemy/

compile-std-watch:
	find elmchemy-core -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr make compile-std

compile-std-tests-watch:
	find elmchemy-core -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile-std && sleep 2 && make test-std"

tests-watch:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr elm-test

compile-demo:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && node elmchemy.js src/Example.elm  > elixir-stuff/elmchemy/lib/example.ex"

install-sysconf:
	git clone "https://github.com/obmarg/libsysconfcpus.git"
	cd libsysconfcpus && ./configure && make && make install
	cd .. && rm -rf libsysconfcpus

bump-patch:
	npm ls | grep -o '[0-9][0-9]*\.[0-9][0-9]*\.[0-9]*' | xargs -I '{}' sed -i  "s/[0-9][0-9]*\.[0-9][0-9]*\.[0-9]*/{}/g" src/Compiler.elm
