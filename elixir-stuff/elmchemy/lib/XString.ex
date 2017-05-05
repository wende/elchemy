# Compiled using Elmchemy v0.0.22
defmodule XString do
  use Elmchemy

  import Elmchemy
  #  isEmpty
  @spec is_empty(String.t) :: boolean
  @spec is_empty() :: (String.t -> boolean)
  curry is_empty/1
  def is_empty(str) do
    "" == str
  end

  # cons
  @spec cons(:char, String.t) :: String.t
  @spec cons() :: (:char -> (String.t -> String.t))
  curry cons/2
  def cons(c, str) do
    Kernel.to_string(c) ++ str
  end

  #  fromChar
  #  uncons
  @spec uncons(String.t) :: {:char, String.t} | nil
  @spec uncons() :: (String.t -> {:char, String.t} | nil)
  curry uncons/1
  def uncons(str) do
    nil
  end

  #  append : should be <> instead of ++
  @spec append(String.t, String.t) :: String.t
  @spec append() :: (String.t -> (String.t -> String.t))
  curry append/2
  def append(a, b) do
    a ++ b
  end

  #  concat
  @spec concat(list(String.t)) :: String.t
  @spec concat() :: (list(String.t) -> String.t)
  curry concat/1
  def concat(list) do
    ""
  end

  #  length
  @spec length(String.t) :: integer
  @spec length() :: (String.t -> integer)
  curry length/1
  def length(str) do
    String.length(str)
  end

  #  map
  #  filter
  #  reverse
  #  foldl
  #  foldr
  #  split
  #  join
  #  slice
  #  left
  #  right
  #  dropLeft
  #  dropRight
  #  pad
  #  padLeft
  #  padRight
  #  trim
  #  trimLeft
  #  trimRight
  #  words
  @spec words(String.t) :: list(String.t)
  @spec words() :: (String.t -> list(String.t))
  curry words/1
  def words(str) do
    String.split(str)
  end

  #  lines
  @spec lines(String.t) :: list(String.t)
  @spec lines() :: (String.t -> list(String.t))
  curry lines/1
  def lines(str) do
    String.split(str, ["\\x0D\\n", "\\x0D", "\\n"])
  end

  #  toUpper
  @spec to_upper(String.t) :: String.t
  @spec to_upper() :: (String.t -> String.t)
  curry to_upper/1
  def to_upper(str) do
    String.upcase(str)
  end

  #  toLower
  @spec to_lower(String.t) :: String.t
  @spec to_lower() :: (String.t -> String.t)
  curry to_lower/1
  def to_lower(str) do
    String.downcase(str)
  end

  #  any
  #  all
  #  contains
  @spec contains(String.t, String.t) :: boolean
  @spec contains() :: (String.t -> (String.t -> boolean))
  curry contains/2
  def contains(contents, string) do
    String.contains?(string, contents)
  end

  #  startWith
  @spec starts_with(String.t, String.t) :: boolean
  @spec starts_with() :: (String.t -> (String.t -> boolean))
  curry starts_with/2
  def starts_with(prefix, str) do
    String.start_with?(str, prefix)
  end

  #  endsWith
  @spec ends_with(String.t, String.t) :: boolean
  @spec ends_with() :: (String.t -> (String.t -> boolean))
  curry ends_with/2
  def ends_with(suffix, str) do
    String.ends_with?(str, suffix)
  end

  #  indexes
  #  indices
  #  toInt
  #  toFloat
  #  toList
  #  fromList
end
