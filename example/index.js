const init = `
module FizzBuzz exposing (fizzbuzz)

import List exposing (map, range)

{-| Fizzes the buzzes, and buzzfizzes the fizz out of buzz
    fizzbuzz 1 7 == "1 2 Fizz 4 Buzz Fizz 7"
-}
fizzbuzz : Int -> Int -> String
fizzbuzz from to =
  let fizzBuzz n = case (n % 3, n % 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    _      -> toString n
  in List.range from to |> map (fizzBuzz >> toString) |> joinWords


joinWords : List String -> String
joinWords a = String.join " " a`;

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
