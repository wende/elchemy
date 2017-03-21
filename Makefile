dev:
	elm-make Main.elm
	open -g index.html

compile:
	elm-make Main.elm --output compiled.js
	sed 's/var Elm = {}/&; \
	var fs = require(\"fs\"); \
	console.warn(\"Compiling \" + process.argv[2]); \
	var a = fs.readFileSync(process.argv[2]).toString(); \
	console.log(_user$$project$$Compiler$$tree(a))/' compiled.js > elmchemy.js
	elm-make example.elm && node elmchemy.js example.elm > output.ex
