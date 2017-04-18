dev:
	elm-make Main.elm
	open -g index.html

release:
	elm-make Main.elm
	mkdir -p stable
	mv index.html stable/index.html

compile:
	elm-make Main.elm --output compiled.js
	sed 's/var Elm = {}/&; \
	var fs = require(\"fs\"); \
	var a = fs.readFileSync(process.argv[2]).toString(); \
	console.log(_user$$project$$Compiler$$tree(a))/' compiled.js > elmchemy.js
	rm compiled.js


compile-watch:
	find . -name "*.elm" | grep -v "elm-stuff" | grep -v .# | entr make compile

compile-std-watch:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr bash -c \
		"make compile && ./elmchemy compile src/elixir_std output"

tests-watch:
	find . -name "*.elm" | grep -v ".#" | grep -v "elm-stuff" | entr elm-test

compile-demo:
	find . | entr bash -c "make compile && node elmchemy.js src/Example.elm  > elixir-stuff/elmchemy/lib/example.ex"
