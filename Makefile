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
	elm-make Main.elm --yes --output compiled.js
	sed 's/var Elm = {}/&; \
	var fs = require(\"fs\"); \
	var a = fs.readFileSync(process.argv[2]).toString(); \
  var output = _user$$project$$Compiler$$tree(a); \
	fs.writeFileSync(process.argv[3], output);/' compiled.js > elchemy.js
	rm compiled.js

compile-watch:
	find . -name "*.elm" | grep -v "elm-stuff" | grep -v .# | entr make compile

test:
	elm test

test-std:
	cd elchemy-core/ && mix test

compile-std:
	make compile
	rm -rf elchemy-core/lib/Elchemy/*
	cd elchemy-core && ../elchemy compile elm/ lib/

compile-std-watch:
	find elchemy-core -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr make compile-std

compile-std-tests-watch:
	find elchemy-core \( -name "*.elm" -or -name '*.ex' \) | grep -v "elchemy.ex" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && make compile-std && make test-std"

tests-watch:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr elm-test

compile-demo:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && node elchemy.js src/Example.elm  > elixir-stuff/elchemy/lib/example.ex"

install-sysconf:
	git clone "https://github.com/obmarg/libsysconfcpus.git"
	cd libsysconfcpus && ./configure && make && make install
	cd .. && rm -rf libsysconfcpus
