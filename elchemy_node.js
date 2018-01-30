var path = require("path")
var fs = require("fs");

var SOURCE_DIR = process.argv[2]
var TARGET_DIR = process.argv[3]
var CACHE_PATH = process.argv[4]

function execute(tree, fullTree, treeAndCommons) {
  var cacheExists = CACHE_PATH && fs.existsSync(CACHE_PATH)

  // If cache is provided and cache doesn't exist the compiler doesn't expect cache, and will produce cache after compilation
  if(CACHE_PATH && !cacheExists) {
    var compiledSource = fs.readFileSync(SOURCE_DIR).toString();
    var compiledOutput = treeAndCommons(compiledSource);
    console.log(compiledOutput)
    var compiledCode = compiledOutput._0
    var compiledCache = JSON.stringify(compiledOutput._1)
    fs.writeFileSync(TARGET_DIR, compiledCode);
    fs.writeFileSync(CACHE_PATH, compiledCache);
  }
  // If cache ISN'T provided the compiler DOESN'T expect cache.
  else if(!CACHE_PATH){
  	var compiledSource = fs.readFileSync(SOURCE_DIR).toString();
    var compiledCode = tree(compiledSource);
  	fs.writeFileSync(TARGET_DIR, compiledCode);
  }
  // If found it will use the cached ExContext.commons and compile target using it
  else {
    var cachedCommons = JSON.parse(fs.readFileSync(CACHE_PATH).toString())
    var compiledSource = fs.readFileSync(SOURCE_DIR).toString();
    var compiledOutput = fullTree(compiledSource, cachedCommons);
    var compiledCode = compiledOutput._0
    fs.writeFileSync(TARGET_DIR, compiledCode);
  }
}


module.exports = {
  execute: execute
}
