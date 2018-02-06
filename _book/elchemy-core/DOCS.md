# Modules
- [Elchemy.XString](#elchemyxstring)
- [Elchemy.XList](#elchemyxlist)
- [Elchemy.XDebug](#elchemyxdebug)
- [Elchemy.XResult](#elchemyxresult)
- [Elchemy.XChar](#elchemyxchar)
- [Elchemy.XBasics](#elchemyxbasics)
- [Elchemy.XMaybe](#elchemyxmaybe)
- [Elchemy.XTuple](#elchemyxtuple)
- [Elchemy](#elchemy)

# Elchemy.XString
- [isEmpty](#isempty)
- [length](#length)
- [reverse](#reverse)
- [repeat](#repeat)
- [cons](#cons)
- [uncons](#uncons)
- [fromChar](#fromchar)
- [append](#append)
- [concat](#concat)
- [split](#split)
- [join](#join)
- [words](#words)
- [lines](#lines)
- [slice](#slice)
- [left](#left)
- [right](#right)
- [dropLeft](#dropleft)
- [dropRight](#dropright)
- [contains](#contains)
- [startsWith](#startswith)
- [endsWith](#endswith)
- [indexes](#indexes)
- [indices](#indices)
- [toInt](#toint)
- [toFloat](#tofloat)
- [toList](#tolist)
- [fromList](#fromlist)
- [toUpper](#toupper)
- [toLower](#tolower)
- [pad](#pad)
- [padLeft](#padleft)
- [padRight](#padright)
- [trim](#trim)
- [trimLeft](#trimleft)
- [trimRight](#trimright)
- [map](#map)
- [filter](#filter)
- [foldl](#foldl)
- [foldr](#foldr)
- [any](#any)
- [all](#all)

 A built-in representation for efficient string manipulation. String literals
are enclosed in `"double quotes"`. Strings are *not* lists of characters.
## Basics

### `isEmpty`
```elm
isEmpty : String -> Bool
```
 Determine if a string is empty.
```elm
isEmpty "" == True
isEmpty "the world" == False
```


---

### `length`
```elm
length : String -> Int
```
 Get the length of a string.
```elm
length "innumerable" == 11
length "" == 0
```


---

### `reverse`
```elm
reverse : String -> String
```
 Reverse a string.
```elm
reverse "stressed" == "desserts"
```


---

### `repeat`
```elm
repeat : Int -> String -> String
```
 Repeat a string *n* times.
```elm
repeat 3 "ha" == "hahaha"
```


---

## Building and Splitting

### `cons`
```elm
cons : Char -> String -> String
```
 Add a character to the beginning of a string.
```elm
XString.cons 'T' "he truth is out there" == "The truth is out there"
```


---

### `uncons`
```elm
uncons : String -> Maybe.Maybe ( Char, String )
```
 Split a non-empty string into its head and tail. This lets you
pattern match on strings exactly as you would with lists.
```elm
uncons "abc" == Just ('a',"bc")
uncons ""    == Nothing
```


---

### `fromChar`
```elm
fromChar : Char -> String
```
 Create a string from a given character.
```elm
fromChar 'a' == "a"
```


---

### `append`
```elm
append : String -> String -> String
```
 Append two strings. You can also use [the `(++)` operator](Basics#++)
to do this.
```elm
append "butter" "fly" == "butterfly"
```


---

### `concat`
```elm
concat : List String -> String
```
 Concatenate many strings into one.
```elm
concat ["never","the","less"] == "nevertheless"
```


---

### `split`
```elm
split : String -> String -> List String
```
 Split a string using a given separator.
```elm
split "," "cat,dog,cow"        == ["cat","dog","cow"]
split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]
```

Use [`Regex.split`](Regex#split) if you need something more flexible.


---

### `join`
```elm
join : String -> List String -> String
```
 Put many strings together with a given separator.
```elm
join "a" ["H","w","ii","n"]        == "Hawaiian"
join " " ["cat","dog","cow"]       == "cat dog cow"
join "/" ["home","evan","Desktop"] == "home/evan/Desktop"
```


---

### `words`
```elm
words : String -> List String
```
 Break a string into words, splitting on chunks of whitespace.
```elm
words "How are \t you? \n Good?" == ["How","are","you?","Good?"]
```


---

### `lines`
```elm
lines : String -> List String
```
 Break a string into lines, splitting on newlines.
```elm
lines "How are you?\nGood?" == ["How are you?", "Good?"]
```


---

## Get Substrings

### `slice`
```elm
slice : Int -> Int -> String -> String
```
 Take a substring given a start and end index. Negative indexes
are taken starting from the *end* of the list.
```elm
slice  7  9 "snakes on a plane!" == "on"
slice  0  6 "snakes on a plane!" == "snakes"
slice  0 -7 "snakes on a plane!" == "snakes on a"
slice -6 -1 "snakes on a plane!" == "plane"
```


---

### `left`
```elm
left : Int -> String -> String
```
 Take *n* characters from the left side of a string.
```elm
left 2 "Mulder" == "Mu"
```


---

### `right`
```elm
right : Int -> String -> String
```
 Take *n* characters from the right side of a string.
```elm
right 2 "Scully" == "ly"
```


---

### `dropLeft`
```elm
dropLeft : Int -> String -> String
```
 Drop *n* characters from the left side of a string.
```elm
dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"
```


---

### `dropRight`
```elm
dropRight : Int -> String -> String
```
 Drop *n* characters from the right side of a string.
```elm
dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"
```


---

## Check for Substrings

### `contains`
```elm
contains : String -> String -> Bool
```
 See if the second string contains the first one.
```elm
contains "the" "theory" == True
contains "hat" "theory" == False
contains "THE" "theory" == False
```

Use [`Regex.contains`](Regex#contains) if you need something more flexible.


---

### `startsWith`
```elm
startsWith : String -> String -> Bool
```
 See if the second string starts with the first one.
```elm
startsWith "the" "theory" == True
startsWith "ory" "theory" == False
```


---

### `endsWith`
```elm
endsWith : String -> String -> Bool
```
 See if the second string ends with the first one.
```elm
endsWith "the" "theory" == False
endsWith "ory" "theory" == True
```


---

### `indexes`
```elm
indexes : String -> String -> List Int
```
 Get all of the indexes for a substring in another string.
```elm
indexes "i" "Mississippi"   == [1,4,7,10]
indexes "ss" "Mississippi"  == [2,5]
indexes "needle" "haystack" == []
```


---

### `indices`
```elm
indices : String -> String -> List Int
```
 Alias for `indexes`.

---

## Conversions

### `toInt`
```elm
toInt : String -> Result.Result String Int
```
 Try to convert a string into an int, failing on improperly formatted strings.
```elm
XString.toInt "123" == Ok 123
XString.toInt "-42" == Ok -42
XString.toInt "3.1" == Err "could not convert string '3.1' to an Int"
XString.toInt "31a" == Err "could not convert string '31a' to an Int"
```

If you are extracting a number from some raw user input, you will typically
want to use [`Result.withDefault`](Result#withDefault) to handle bad data:
```elm
XResult.withDefault 0 (XString.toInt "42") == 42
XResult.withDefault 0 (XString.toInt "ab") == 0
```


---

### `toFloat`
```elm
toFloat : String -> Result.Result String Float
```
 Try to convert a string into a float, failing on improperly formatted strings.
```elm
XString.toFloat "123" == Ok 123.0
XString.toFloat "-42" == Ok -42.0
XString.toFloat "3.1" == Ok 3.1
XString.toFloat "31a" == Err "could not convert string '31a' to a Float"
```

If you are extracting a number from some raw user input, you will typically
want to use [`Result.withDefault`](Result#withDefault) to handle bad data:
```elm
XResult.withDefault 0 (XString.toFloat "42.5") == 42.5
XResult.withDefault 0 (XString.toFloat "cats") == 0
```


---

### `toList`
```elm
toList : String -> List Char
```
 Convert a string to a list of characters.
```elm
toList "abc" == ['a','b','c']
```


---

### `fromList`
```elm
fromList : List Char -> String
```
 Convert a list of characters into a String. Can be useful if you
want to create a string primarily by consing, perhaps for decoding
something.
```elm
fromList ['a','b','c'] == "abc"
```


---

## Formatting
Cosmetic operations such as padding with extra characters or trimming whitespace.

### `toUpper`
```elm
toUpper : String -> String
```
 Convert a string to all upper case. Useful for case-insensitive comparisons
and VIRTUAL YELLING.
```elm
toUpper "skinner" == "SKINNER"
```


---

### `toLower`
```elm
toLower : String -> String
```
 Convert a string to all lower case. Useful for case-insensitive comparisons.
```elm
toLower "X-FILES" == "x-files"
```


---

### `pad`
```elm
pad : Int -> Char -> String -> String
```
 Pad a string on both sides until it has a given length.
```elm
pad 5 ' ' "1"   == "  1  "
pad 5 ' ' "11"  == "  11 "
pad 5 ' ' "121" == " 121 "
```


---

### `padLeft`
```elm
padLeft : Int -> Char -> String -> String
```
 Pad a string on the left until it has a given length.
```elm
padLeft 5 '.' "1"   == "....1"
padLeft 5 '.' "11"  == "...11"
padLeft 5 '.' "121" == "..121"
```


---

### `padRight`
```elm
padRight : Int -> Char -> String -> String
```
 Pad a string on the right until it has a given length.
```elm
padRight 5 '.' "1"   == "1...."
padRight 5 '.' "11"  == "11..."
padRight 5 '.' "121" == "121.."
```


---

### `trim`
```elm
trim : String -> String
```
 Get rid of whitespace on both sides of a string.
```elm
trim "  hats  \n" == "hats"
```


---

### `trimLeft`
```elm
trimLeft : String -> String
```
 Get rid of whitespace on the left of a string.
```elm
trimLeft "  hats  \n" == "hats  \n"
```


---

### `trimRight`
```elm
trimRight : String -> String
```
 Get rid of whitespace on the right of a string.
```elm
trimRight "  hats  \n" == "  hats"
```


---

## Higher-Order Functions

### `map`
```elm
map : (Char -> Char) -> String -> String
```
 Transform every character in a string
```elm
map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"
```


---

### `filter`
```elm
filter : (Char -> Bool) -> String -> String
```
 Keep only the characters that satisfy the predicate.
```elm
filter ((==) '2') "R2-D2" == "22"
```


---

### `foldl`
```elm
foldl : (Char -> b -> b) -> b -> String -> b
```
 Reduce a string from the left.
```elm
foldl XString.cons "" "time" == "emit"
```


---

### `foldr`
```elm
foldr : (Char -> b -> b) -> b -> String -> b
```
 Reduce a string from the right.
```elm
foldr XString.cons "" "time" == "time"
```


---

### `any`
```elm
any : (Char -> Bool) -> String -> Bool
```
 Determine whether *any* characters satisfy a predicate.
```elm
any XChar.isDigit "90210" == True
any XChar.isDigit "R2-D2" == True
any XChar.isDigit "heart" == False
```


---

### `all`
```elm
all : (Char -> Bool) -> String -> Bool
```
 Determine whether *all* characters satisfy a predicate.
```elm
all XChar.isDigit "90210" == True
all XChar.isDigit "R2-D2" == False
all XChar.isDigit "heart" == False
```


---



# Elchemy.XList
- [isEmpty](#isempty-1)
- [length](#length-1)
- [reverse](#reverse-1)
- [member](#member)
- [head](#head)
- [tail](#tail)
- [filter](#filter-1)
- [take](#take)
- [drop](#drop)
- [singleton](#singleton)
- [repeat](#repeat-1)
- [range](#range)
- [cons](#cons-1)
- [::](#::)
- [append](#append-1)
- [concat](#concat-1)
- [intersperse](#intersperse)
- [partition](#partition)
- [unzip](#unzip)
- [map](#map-1)
- [map2](#map2)
- [filterMap](#filtermap)
- [concatMap](#concatmap)
- [indexedMap](#indexedmap)
- [foldr](#foldr-1)
- [foldl](#foldl-1)
- [sum](#sum)
- [product](#product)
- [maximum](#maximum)
- [minimum](#minimum)
- [all](#all-1)
- [any](#any-1)
- [scanl](#scanl)
- [sort](#sort)
- [sortBy](#sortby)
- [sortWith](#sortwith)

 A library for manipulating lists of values. Every value in a
list must have the same type.
## Basics

### `isEmpty`
```elm
isEmpty : List a -> Bool
```
 Determine if a list is empty.
```elm
isEmpty [] == True
```


---

### `length`
```elm
length : List a -> Int
```
 Determine the length of a list.
```elm
length [1,2,3] == 3
```


---

### `reverse`
```elm
reverse : List a -> List a
```
 Reverse a list.
```elm
reverse [1,2,3,4] == [4,3,2,1]
```


---

### `member`
```elm
member : a -> List a -> Bool
```
 Figure out whether a list contains a value.
```elm
member 9 [1,2,3,4] == False
member 4 [1,2,3,4] == True
```


---

## Sub-lists

### `head`
```elm
head : List a -> Maybe.Maybe a
```
 Extract the first element of a list.
```elm
head [1,2,3] == Just 1
head [] == Nothing
```


---

### `tail`
```elm
tail : List a -> Maybe.Maybe (List a)
```
 Extract the rest of the list.
```elm
tail [1,2,3] == Just [2,3]
tail [] == Nothing
```


---

### `filter`
```elm
filter : (a -> Bool) -> List a -> List a
```
 Keep only elements that satisfy the predicate.
```elm
filter (flip (%) 2 >> (==) 0) [1,2,3,4,5,6] == [2,4,6]
```


---

### `take`
```elm
take : Int -> List a -> List a
```
 Take the first *n* members of a list.
```elm
take 2 [1,2,3,4] == [1,2]
```


---

### `drop`
```elm
drop : Int -> List a -> List a
```
 Drop the first *n* members of a list.
```elm
drop 2 [1,2,3,4] == [3,4]
```


---

## Putting Lists Together

### `singleton`
```elm
singleton : a -> List a
```
 Create a list with only one element:
```elm
singleton 1234 == [1234]
singleton "hi" == ["hi"]
```


---

### `repeat`
```elm
repeat : Int -> a -> List a
```
 Create a list with *n* copies of a value:
```elm
repeat 3 0 == [0, 0, 0]
```


---

### `range`
```elm
range : Int -> Int -> List Int
```
 Create a list of numbers, every element increasing by one.
You give the lowest and highest number that should be in the list.
```elm
range 3 6 == [3, 4, 5, 6]
range 3 3 == [3]
range 6 3 == []
```


---

### `cons`
```elm
cons : a -> List a -> List a
```
 Add an element to the front of a list. Pronounced *cons*.
```elm
cons 1 [2,3] == [1,2,3]
cons 1 [] == [1]
```


---

### `::`
```elm
:: : a -> List a -> List a
```
 Add an element to the front of a list. Pronounced *cons*.
```elm
1 :: [2,3] == [1,2,3]
1 :: [] == [1]
```


---

### `append`
```elm
append : List a -> List a -> List a
```
 Put two lists together.
```elm
append [1,1,2] [3,5,8] == [1,1,2,3,5,8]
append ['a','b'] ['c'] == ['a','b','c']
```

You can also use [the `(++)` operator](Basics#++) to append lists.


---

### `concat`
```elm
concat : List (List a) -> List a
```
 Concatenate a bunch of lists into a single list:
```elm
concat [[1,2],[3],[4,5]] == [1,2,3,4,5]
```


---

### `intersperse`
```elm
intersperse : a -> List a -> List a
```
 Places the given value between all members of the given list.
```elm
intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]
```


---

## Taking Lists Apart

### `partition`
```elm
partition : (a -> Bool) -> List a -> ( List a, List a )
```
 Partition a list based on a predicate. The first list contains all values
that satisfy the predicate, and the second list contains all the value that do
not.
```elm
partition (\x -> x < 3)      [0,1,2,3,4,5] == ([0,1,2], [3,4,5])
partition (\a -> a % 2 == 0) [0,1,2,3,4,5] == ([0,2,4], [1,3,5])
```


---

### `unzip`
```elm
unzip : List ( a, b ) -> ( List a, List b )
```
 Decompose a list of tuples into a tuple of lists.
```elm
unzip (repeat 3 (0, True)) == ([0,0,0], [True,True,True])
```


---

## Mapping

### `map`
```elm
map : (a -> b) -> List a -> List b
```
 Apply a function to every element of a list.
```elm
map sqrt [1,4,9] == [1.0,2.0,3.0]
map not [True,False,True] == [False,True,False]
```


---

### `map2`
```elm
map2 : (a -> b -> result) -> List a -> List b -> List result
```
 Combine two lists, combining them with the given function.
If one list is longer, the extra elements are dropped.
```elm
map2 (+) [1,2,3] [1,2,3,4] == [2,4,6]
map2 (,) [1,2,3] ['a','b'] == [ (1,'a'), (2,'b') ]
```


---

If you can think of a legitimate use of `mapN` where `N` is 6 or more, please
let us know on [the list](https://groups.google.com/forum/#!forum/elm-discuss).
The current sentiment is that it is already quite error prone once you get to
4 and possibly should be approached another way.
## Special Maps

### `filterMap`
```elm
filterMap : (a -> Maybe.Maybe b) -> List a -> List b
```
 Apply a function that may succeed to all values in the list, but only keep
the successes.
```elm
filterMap (\a -> if a >= 18 then Just a else Nothing) [3, 15, 12, 18, 24] == [18, 24]
```


---

### `concatMap`
```elm
concatMap : (a -> List b) -> List a -> List b
```
 Map a given function onto a list and flatten the resulting lists.
```elm
concatMap (range 2) [1] == concat (map (range 2) [1]) == True
```


---

### `indexedMap`
```elm
indexedMap : (Int -> a -> b) -> List a -> List b
```
 Same as `map` but the function is also applied to the index of each
element (starting at zero).
```elm
indexedMap (,) ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
```


---

## Folding

### `foldr`
```elm
foldr : (a -> b -> b) -> b -> List a -> b
```
 Reduce a list from the right.
```elm
foldr (+) 0 [1,2,3] == 6
```


---

### `foldl`
```elm
foldl : (a -> b -> b) -> b -> List a -> b
```
 Reduce a list from the left.
```elm
foldl (::) [] [1,2,3] == [3,2,1]
```


---

## Special Folds

### `sum`
```elm
sum : List number -> number
```
 Get the sum of the list elements.
```elm
sum [1,2,3,4] == 10
```


---

### `product`
```elm
product : List number -> number
```
 Get the product of the list elements.
```elm
product [1,2,3,4] == 24
```


---

### `maximum`
```elm
maximum : List comparable -> Maybe.Maybe comparable
```
 Find the maximum element in a non-empty list.
```elm
maximum [1,4,2] == Just 4
maximum []      == Nothing
```


---

### `minimum`
```elm
minimum : List comparable -> Maybe.Maybe comparable
```
 Find the minimum element in a non-empty list.
```elm
minimum [3,2,1] == Just 1
minimum []      == Nothing
```


---

### `all`
```elm
all : (a -> Bool) -> List a -> Bool
```
 Determine if all elements satisfy the predicate.
```elm
all (\a -> a % 2 == 0) [2,4] == True
all (\a -> a % 2 == 0) [2,3] == False
all (\a -> a % 2 == 0) [] == True
```


---

### `any`
```elm
any : (a -> Bool) -> List a -> Bool
```
 Determine if any elements satisfy the predicate.
```elm
any (\a -> a % 2 == 0) [2,3] == True
any (\a -> a % 2 == 0) [1,3] == False
any (\a -> a % 2 == 0) [] == False
```


---

### `scanl`
```elm
scanl : (a -> b -> b) -> b -> List a -> List b
```
 Reduce a list from the left, building up all of the intermediate results into a list.
```elm
scanl (+) 0 [1,2,3,4] == [0,1,3,6,10]
```


---

## Sorting

### `sort`
```elm
sort : List comparable -> List comparable
```
 Sort values from lowest to highest
```elm
sort [3,1,5] == [1,3,5]
```


---

### `sortBy`
```elm
sortBy : (a -> comparable) -> List a -> List a
```
 Sort values by a derived property. To be replaced
```elm
sortBy (\(i, _) -> i)  [(1, "mouse"),(0, "cat")] == [(0, "cat"), (1, "mouse")]
```


---

### `sortWith`
```elm
sortWith : (a -> a -> Basics.Order) -> List a -> List a
```
 Sort values with a custom comparison function.
```elm
sortWith (flip compare) [1,2,3,4,5] == [5,4,3,2,1]
```

This is also the most general sort function, allowing you
to define any other: `sort == sortWith compare`
f


---



# Elchemy.XDebug
- [log](#log)
- [crash](#crash)

 Module with helper functions for debugging
## Debug

### `log`
```elm
log : String -> a -> a
```
 Log to console in `title: object` format

---

### `crash`
```elm
crash : String -> a
```
 Raise an exception to crash the runtime. Should be avoided at all
costs. Helpful for crashing at not yet implelented functionality

---



# Elchemy.XResult
- [Result](#result)
- [map](#map-2)
- [map2](#map2-1)
- [map3](#map3)
- [map4](#map4)
- [map5](#map5)
- [andThen](#andthen)
- [withDefault](#withdefault)
- [toMaybe](#tomaybe)
- [fromMaybe](#frommaybe)
- [mapError](#maperror)

 A `Result` is the result of a computation that may fail. This is a great
way to manage errors in Elm.
## Type and Constructors

### `Result`
```elm
type Result error value
    = Ok value
    | Err error
```
 A `Result` is either `Ok` meaning the computation succeeded, or it is an
`Err` meaning that there was some failure.

---

## Mapping

### `map`
```elm
map : (a -> value) -> Result x a -> Result x value
```
 Apply a function to a result. If the result is `Ok`, it will be converted.
If the result is an `Err`, the same error value will propagate through.
```elm
map sqrt (Ok 4.0)          == Ok 2.0
map sqrt (Err "bad input") == Err "bad input"
```


---

### `map2`
```elm
map2 : (a -> b -> value) -> Result x a -> Result x b -> Result x value
```
 Apply a function to two results, if both results are `Ok`. If not,
the first argument which is an `Err` will propagate through.
```elm
map2 (+) (XString.toInt "1") (XString.toInt "2") == Ok 3
map2 (+) (XString.toInt "1") (XString.toInt "y") == Err "could not convert string 'y' to an Int"
map2 (+) (XString.toInt "x") (XString.toInt "y") == Err "could not convert string 'x' to an Int"
```


---

### `map3`
```elm
map3 : (a -> b -> c -> value) -> Result x a -> Result x b -> Result x c -> Result x value
```
 
---

### `map4`
```elm
map4 : (a -> b -> c -> d -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x value
```
 
---

### `map5`
```elm
map5 : (a -> b -> c -> d -> e -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x value
```
 
---

## Chaining

### `andThen`
```elm
andThen : (a -> Result x b) -> Result x a -> Result x b
```
 Chain together a sequence of computations that may fail. It is helpful
to see its definition:

This means we only continue with the callback if things are going well. For
example, say you need to use (`toInt : String -> Result String Int`) to parse
a month and make sure it is between 1 and 12:

This allows us to come out of a chain of operations with quite a specific error
message. It is often best to create a custom type that explicitly represents
the exact ways your computation may fail. This way it is easy to handle in your
code.


---

## Handling Errors

### `withDefault`
```elm
withDefault : a -> Result x a -> a
```
 If the result is `Ok` return the value, but if the result is an `Err` then
return a given default value. The following examples try to parse integers.
```elm
withDefault 0 (XString.toInt "123") == 123
withDefault 0 (XString.toInt "abc") == 0
```


---

### `toMaybe`
```elm
toMaybe : Result x a -> Maybe.Maybe a
```
 Convert to a simpler `Maybe` if the actual error message is not needed or
you need to interact with some code that primarily uses maybes.

---

### `fromMaybe`
```elm
fromMaybe : x -> Maybe.Maybe a -> Result x a
```
 Convert from a simple `Maybe` to interact with some code that primarily
uses `Results`.

---

### `mapError`
```elm
mapError : (x -> y) -> Result x a -> Result y a
```
 Transform an `Err` value. For example, say the errors we get have too much
information:
```elm
mapError XTuple.first (Ok (123, 1)) == Ok (123, 1)
mapError XTuple.second (Err ("nothing", "important")) == Err "important"
```


---



# Elchemy.XChar
- [isUpper](#isupper)
- [isLower](#islower)
- [isDigit](#isdigit)
- [isOctDigit](#isoctdigit)
- [isHexDigit](#ishexdigit)
- [toUpper](#toupper-1)
- [toLower](#tolower-1)
- [KeyCode](#keycode)
- [toCode](#tocode)
- [fromCode](#fromcode)

 Functions for working with characters. Character literals are enclosed in
`'a'` pair of single quotes.
## Classification

### `isUpper`
```elm
isUpper : Char -> Bool
```
 True for upper case ASCII letters.
```elm
isUpper 'D' == True
isUpper 'A' == True
isUpper 'x' == False
```


---

### `isLower`
```elm
isLower : Char -> Bool
```
 True for lower case ASCII letters.
```elm
isLower 'd' == True
isLower 'a' == True
isLower 'X' == False
```


---

### `isDigit`
```elm
isDigit : Char -> Bool
```
 True for ASCII digits `[0-9]`.
```elm
isDigit '1' == True
isDigit '9' == True
isDigit 'a' == False
```


---

### `isOctDigit`
```elm
isOctDigit : Char -> Bool
```
 True for ASCII octal digits `[0-7]`.
```elm
isOctDigit '7' == True
isOctDigit '5' == True
isOctDigit '9' == False
```


---

### `isHexDigit`
```elm
isHexDigit : Char -> Bool
```
 True for ASCII hexadecimal digits `[0-9a-fA-F]`.
```elm
isHexDigit 'd' == True
isHexDigit 'D' == True
isHexDigit 'x' == False
```


---

## Conversion

### `toUpper`
```elm
toUpper : Char -> Char
```
 Convert to upper case.
```elm
toUpper 'a' == 'A'
```


---

### `toLower`
```elm
toLower : Char -> Char
```
 Convert to lower case.
```elm
toLower 'A' == 'a'
```


---

## Key Codes

### `KeyCode`
```elm
type alias KeyCode  =
    Int
```
 Keyboard keys can be represented as integers. These are called *key codes*.
You can use [`toCode`](#toCode) and [`fromCode`](#fromCode) to convert between
key codes and characters.

---

### `toCode`
```elm
toCode : Char -> KeyCode
```
 Convert to key code.
```elm
toCode 'a' == 97
```


---

### `fromCode`
```elm
fromCode : KeyCode -> Char
```
 Convert from key code.
```elm
fromCode 97 == 'a'
```


---



# Elchemy.XBasics
- [compare](#compare-1)
- [xor](#xor-1)
- [sqrt](#sqrt-1)
- [clamp](#clamp)
- [compare](#compare-1)
- [xor](#xor-1)
- [negate](#negate)
- [sqrt](#sqrt-1)
- [logBase](#logbase)
- [e](#e)
- [pi](#pi)
- [cos](#cos)
- [sin](#sin)
- [tan](#tan)
- [acos](#acos)
- [asin](#asin)
- [atan](#atan)
- [atan2](#atan2)
- [round](#round)
- [floor](#floor)
- [ceiling](#ceiling)
- [truncate](#truncate)
- [toFloat](#tofloat-1)
- [toString](#tostring)
- [++](#++)
- [identity](#identity)
- [always](#always)
- [flip](#flip)
- [tuple2](#tuple2)
- [tuple3](#tuple3)
- [tuple4](#tuple4)
- [tuple5](#tuple5)
- [Order](#order)

 Tons of useful functions that get imported by default.

### `compare`
```elm
compare : comparable -> comparable -> Order
```
 Basic compare function


### Example
```elm
compare 1 2 == LT
```


---

### `xor`
```elm
xor : Bool -> Bool -> Bool
```
 The exclusive-or operator. `True` if exactly one input is `True`.

---

### `sqrt`
```elm
sqrt : number -> Float
```
 Take the square root of a number.

---

### `clamp`
```elm
clamp : comparable -> comparable -> comparable -> comparable
```
 Clamps a number within a given range. With the expression
`clamp 100 200 x` the results are as follows:
100 if x < 100
x if 100 <= x < 200
200 if 200 <= x

---

### `compare`
```elm
compare : comparable -> comparable -> Order
```
 Basic compare function


### Example
```elm
compare 1 2 == LT
```


---

### `xor`
```elm
xor : Bool -> Bool -> Bool
```
 The exclusive-or operator. `True` if exactly one input is `True`.

---

### `negate`
```elm
negate : number -> number
```
 Negate a number.
```elm
negate 42 == -42
negate -42 == 42
negate 0 == 0
```


---

### `sqrt`
```elm
sqrt : number -> Float
```
 Take the square root of a number.

---

### `logBase`
```elm
logBase : Float -> Float -> Float
```
 
---

### `e`
```elm
e : Float
```
 
---

### `pi`
```elm
pi : Float
```
 
---

### `cos`
```elm
cos : Float -> Float
```
 
---

### `sin`
```elm
sin : Float -> Float
```
 
---

### `tan`
```elm
tan : Float -> Float
```
 
---

### `acos`
```elm
acos : Float -> Float
```
 
---

### `asin`
```elm
asin : Float -> Float
```
 
---

### `atan`
```elm
atan : Float -> Float
```
 
---

### `atan2`
```elm
atan2 : Float -> Float -> Float
```
 
---

### `round`
```elm
round : Float -> Int
```
 
---

### `floor`
```elm
floor : Float -> Int
```
 
---

### `ceiling`
```elm
ceiling : Float -> Int
```
 
---

### `truncate`
```elm
truncate : Float -> Int
```
 Truncate a number, rounding towards zero.

---

### `toFloat`
```elm
toFloat : Int -> Float
```
 Convert an integer into a float.

---

### `toString`
```elm
toString : a -> String
```
 Turn any kind of value into a string. When you view the resulting string
with `Text.fromString` it should look just like the value it came from.
```elm
toString 42 == "42"
toString [1,2] == "[1, 2]"
```


---

### `++`
```elm
++ : appendable -> appendable -> appendable
```
 Put two appendable things together. This includes strings, lists, and text.
```elm
"hello" ++ "world" == "helloworld"
[1,1,2] ++ [3,5,8] == [1,1,2,3,5,8]
```


---

### `identity`
```elm
identity : a -> a
```
 Given a value, returns exactly the same value. This is called
[the identity function](http://en.wikipedia.org/wiki/Identity_function).

---

### `always`
```elm
always : a -> a -> a
```
 Create a function that *always* returns the same value. Useful with
functions like `map`:
```elm
List.map (always 0) [1,2,3,4,5] == [0,0,0,0,0]
List.map (\_ -> 0) [1,2,3,4,5] == [0,0,0,0,0]
```


---

### `flip`
```elm
flip : (a -> b -> c) -> b -> a -> c
```
 Flip the order of the first two arguments to a function.

---

### `tuple2`
```elm
tuple2 : a -> b -> ( a, b )
```
 
---

### `tuple3`
```elm
tuple3 : a -> b -> c -> ( a, b, c )
```
 
---

### `tuple4`
```elm
tuple4 : a -> b -> c -> d -> ( a, b, c, d )
```
 
---

### `tuple5`
```elm
tuple5 : a -> b -> c -> d -> e -> ( a, b, c, d, e )
```
 
---


### `Order`
```elm
type Order 
    = LT 
    | EQ 
    | GT 
```
 Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.

---



# Elchemy.XMaybe
- [Maybe](#maybe)
- [withDefault](#withdefault-1)
- [map](#map-3)
- [map2](#map2-2)
- [map3](#map3-1)
- [map4](#map4-1)
- [map5](#map5-1)
- [andThen](#andthen-1)

 This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.
## Definition

### `Maybe`
```elm
type Maybe a
    = Just a
    | Nothing 
```
 Represent values that may or may not exist. It can be useful if you have a
record field that is only filled in sometimes. Or if a function takes a value
sometimes, but does not absolutely need it.

---

## Common Helpers

### `withDefault`
```elm
withDefault : a -> Maybe a -> a
```
 Provide a default value, turning an optional value into a normal
value. This comes in handy when paired with functions like
[`Dict.get`](Dict#get) which gives back a `Maybe`.
```elm
withDefault 100 (Just 42)   == 42
withDefault 100 Nothing     == 100
```


---

### `map`
```elm
map : (a -> b) -> Maybe a -> Maybe b
```
 Transform a `Maybe` value with a given function:
```elm
map ((+) 2) (Just 9) == Just 11
map ((+) 2) Nothing == Nothing
```


---

### `map2`
```elm
map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
```
 Apply a function if all the arguments are `Just` a value.
```elm
map2 (+) (Just 3) (Just 4) == Just 7
map2 (+) (Just 3) Nothing == Nothing
map2 (+) Nothing (Just 4) == Nothing
```


---

### `map3`
```elm
map3 : (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
```
 
---

### `map4`
```elm
map4 : (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
```
 
---

### `map5`
```elm
map5 : (a -> b -> c -> d -> e -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe value
```
 
---

## Chaining Maybes

### `andThen`
```elm
andThen : (a -> Maybe b) -> Maybe a -> Maybe b
```
 Chain together many computations that may fail. It is helpful to see its
definition:

This means we only continue with the callback if things are going well. For
example, say you need to use (`head : List Int -> Maybe Int`) to get the
first month from a `List` and then make sure it is between 1 and 12:

If `head` fails and results in `Nothing` (because the `List` was `empty`),
this entire chain of operations will short-circuit and result in `Nothing`.
If `toValidMonth` results in `Nothing`, again the chain of computations
will result in `Nothing`.


---



# Elchemy.XTuple
- [first](#first)
- [second](#second)
- [mapFirst](#mapfirst)
- [mapSecond](#mapsecond)

Module for tuple manipulation

### `first`
```elm
first : ( a, b ) -> a
```
 Extract the first value from a tuple.
```elm
first (3, 4) == 3
first ("john", "doe") == "john"
```
 
---

### `second`
```elm
second : ( a, b ) -> b
```
 Extract the second value from a tuple.
```elm
second (3, 4) == 4
second ("john", "doe") == "doe"
```
 
---

### `mapFirst`
```elm
mapFirst : (a -> a1) -> ( a, b ) -> ( a1, b )
```
 Transform the first value in a tuple.
```elm
mapFirst String.reverse ("stressed", 16) == ("desserts", 16)
mapFirst String.length  ("stressed", 16) == (8, 16)
```
 
---

### `mapSecond`
```elm
mapSecond : (b -> b1) -> ( a, b ) -> ( a, b1 )
```
 Transform the second value in a tuple.
```elm
mapSecond sqrt          ("stressed", 16) == ("stressed", 4.0)
mapSecond (\x -> x + 1) ("stressed", 16) == ("stressed", 17)
```
 
---



# Elchemy
- [lffi](#lffi)
- [ffi](#ffi)
- [tryFfi](#tryffi)
- [flambda](#flambda)
- [tryCatch](#trycatch)

 Module to help express some Elixir related types and calls that wouldn't otherwise be possible

### `lffi`
```elm
lffi : String -> a
```
 *Deprecated since Elchemy 0.3.0*
Function to make a direct Elixir local call. Takes a local function name
and a tuple or single value of arguments
```elm
lffi "to_string"
```


---

### `ffi`
```elm
ffi : String -> String -> a
```
 Function to make a direct Elixir remote call. Takes a module name and a function name
Ffi functions must be the only call in the entire function and the type declaration
is mandatory. For example
```elm
mySum : List number -> number
mySum =
    ffi "Enum" "sum"
```

That function will call `Enum.sum(list)` on our parameter, and also it will generate
a type verification macro, that makes sure, that `Enum.sum/1` actually returns
our type (List number -> number).
To actiate the typecheck put
```elm
typetest MyModule
```

inside of your test suite


---

### `tryFfi`
```elm
tryFfi : String -> String -> a
```
 Function to make a direct Elixir remote call. Takes a module name, a function name
and a tuple or single value of arguments. tryFfi verifies no types at compile time
but it makes sure the value you're requesting is what you're expecting wrappend
in Result.Ok Type, or otherwise you get a Result.Error String
```elm
myToFloat : String -> Result Float
myToFloat =
    ffi "Kernel" "to_float"
```


---

### `flambda`
```elm
flambda : Int -> a -> b
```
 *Deprecated since Elchemy 0.3.0*

Produce multiple argument anonymous function out of regular elm function.
```elm
flambda 2 fun --> fn x1, x2 -> fun.(x1).(x2) end
```


---

### `tryCatch`
```elm
tryCatch : (() -> a) -> Result.Result String a
```
 *Deprecated since Elchemy 0.3.0*

Wrap a function call in try catch returning Result based on wether the function throwed an error


---


> Generated with elm-make: 0.18.0 and elm-docs: 0.2.2
