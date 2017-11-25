dev:
	pkill -f http-server &
	echo "Make sure to install http-server with npm i -g http-server"
	elm-make Main.elm --output=example/elm.js --debug
	(http-server ./example/ -p 8081 -c-1 &) && open "http://127.0.0.1:8081"

release:
	elm-make Main.elm --output=example/elm.js
	mkdir -p docs/stable
	cp -r example/ docs/stable/

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
	./node_modules/.bin/elm-test

test-std:
	cd elchemy-core/ && mix test

compile-std:
	make compile
	rm -rf elchemy-core/lib/Elchemy/*
	cd elchemy-core && ../elchemy compile elm lib

compile-std-watch:
	find elchemy-core -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr make compile-std

compile-std-tests-watch:
	find elchemy-core \( -name "*.elm" -or -name '*.ex' \) | grep -v "elchemy.ex" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && make compile-std && make test-std"

tests-watch:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr ./node_modules/.bin/elm-test

compile-demo:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && node elchemy.js src/Example.elm  > elixir-stuff/elchemy/lib/example.ex"

install-sysconf:
	git clone "https://github.com/obmarg/libsysconfcpus.git"
	cd libsysconfcpus && ./configure && make && make install
	cd .. && rm -rf libsysconfcpus

compile-elixir:
	make compile
	rm -rf elchemy_ex/elm-deps
	cd elchemy_ex && ../elchemy compile ../src lib | ts %H:%M:%.S

compile-test-elixir:
	make compile-elixir
	rm -rf elchemy_ex/elm-deps/Bogdanp/elm-ast/8.0.7/example
	cd elchemy_ex && mix compile

build-docs:
	cd ../elchemy-page && git checkout master && git pull && elm install && yarn && yarn build
	rm -rf docs/*
	cp -r ../elchemy-page/dist/* docs/
