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
	console.warn(\"Compiling \" + process.argv[2]); \
	var a = fs.readFileSync(process.argv[2]).toString(); \
	console.log(_user$$project$$Compiler$$tree(a))/' compiled.js > elmchemy.js
	rm compiled.js
	elm-make ./src/MyList.elm && node elmchemy.js ./src/MyList.elm > output.ex

compile-watch:
	find . | grep -v .# | entr make compile
