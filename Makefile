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
	require(".\/elchemy_node.js").execute(_user$$project$$Compiler$$tree, _user$$project$$Compiler$$fullTree, _user$$project$$Compiler$$treeAndCommons)/' compiled.js > elchemy.js
	rm compiled.js

compile-watch:
	find . -name "*.elm" | grep -v "elm-stuff" | grep -v .# | entr make compile

test:
	./node_modules/.bin/elm-test

test-all:
	make test
	make test-std
	make compile-elixir # Change to compile-elixir-and-test when let..in fixed completely
	make test-project

test-project:
	rm -rf test_project
	mix new test_project
	cd test_project ; \
		(yes | ../elchemy init) ;\
		../elchemy compile elm lib  ;\
		cp -r ../elchemy-core/lib lib/elm-deps ;\
		cp -r ../elchemy-core/elm lib/elm-deps-elixir-files ;\
		mix test
 
test-std:
	cd elchemy-core/ && mix test

compile-std:
	cd elchemy-core && rm -rf .elchemy
	make compile-std-incremental

compile-std-incremental:
	make compile
	cd elchemy-core && ../elchemy compile elm lib

compile-std-watch:
	find elchemy-core -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr make compile-std

compile-std-tests-watch:
	find elchemy-core \( -name "*.elm" -or -name '*.ex*' \) | grep -v "elchemy.ex" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && make compile-std && make test-std"

compile-incremental-std-tests-watch:
	find elchemy-core \( -name "*.elm" -or -name '*.ex*' \) | grep -v "elchemy.ex" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c "make compile && make compile-std-incremental && make test-std"

tests-watch:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr ./node_modules/.bin/elm-test

install-sysconf:
	git clone "https://github.com/obmarg/libsysconfcpus.git"
	cd libsysconfcpus && ./configure && make && make install
	cd .. && rm -rf libsysconfcpus

compile-elixir:
	make compile
	rm -rf elchemy_ex/elm-deps
	rm -rf elchemy_ex/.elchemy
	cd elchemy_ex && ../elchemy compile ../src lib

compile-elixir-and-run:
	make compile-elixir
	cd elchemy_ex && mix compile

compile-elixir-and-test:
	make compile-elixir
	cd elchemy_ex && mix test

build-docs:
	cd ../elchemy-page && git checkout master && git pull && elm install && yarn && yarn build
	rm -rf docs/*
	cp -r ../elchemy-page/dist/* docs/
