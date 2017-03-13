dev:
	elm-make Main.elm
	open -g index.html

compile:
	elm-make Compiler.elm --output compiled.js
	sed 's/var Elm = {}/&; \
	var fs = require(\"fs\"); \
	var a = fs.readFileSync(\"example.elm\").toString(); \
	console.log(_user$$project$$Main$$tree(a).children[0].text)/' compiled.js > _compiled.js
	node _compiled.js > output.ex
