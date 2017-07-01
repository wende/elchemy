dev:
	pkill -f http-server &
	echo "Make sure to install http-server with npm i -g http-server"
	elm-make Main.elm --output=example/elm.js
	(http-server ./example/ -p 8081 -c-1 &) && open "http://127.0.0.1:8081"

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
	cd elmchemy-core/ && mix test

compile-std:
	make compile
	rm -rf elmchemy-core/lib/Elmchemy/*
	cd elmchemy-core && ../elmchemy compile elm/ lib/

compile-std-watch:
	find elmchemy-core -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr make compile-std

compile-std-tests-watch:
	find elmchemy-core -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && make compile-std && make test-std"

tests-watch:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr elm-test

compile-demo:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && node elmchemy.js src/Example.elm  > elixir-stuff/elmchemy/lib/example.ex"

install-sysconf:
	git clone "https://github.com/obmarg/libsysconfcpus.git"
	cd libsysconfcpus && ./configure && make && make install
	cd .. && rm -rf libsysconfcpus
