const init = `
module Stack exposing (..)

import Elmchemy exposing (..)


`;

var app = Elm.Main.embed(document.getElementById('root'), init);

var codeMirror = CodeMirror(document.getElementById('editor'), {
  value: init,
  lineNumbers: true,
  mode: 'elm',
  theme: 'solarized'
});

codeMirror.on('change', function(cm, change) {
  var data = cm.getValue();
  app.ports.updateInput.send(data);
});
