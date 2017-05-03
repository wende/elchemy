# Compiled using Elmchemy v0.0.17
defmodule Elmchemy.XBasics do
  use Elmchemy

  import Elmchemy
  alias XList

  @type order :: :lt | :eq | :gt

  #  Operators

  curry ==/2
  curry !=/2
  curry </2
  curry >/2
  curry <=/2
  curry >=/2
  curry max/2
  curry min/2

  curry &&/2
  curry ||/2

  curry +/2
  curry -/2
  curry */2
  curry //2
  curry div/2
  curry rem/2
  # curry abs/2


  @doc """
  this is basic compare function

  ### Example

  ```js
  content
  ```

  """
  @spec compare(any, any) :: order
  curry compare/2
  def compare(a, b) do
    cond do
      a > b -> :gt
      a < b -> :lt
      true -> :eq
    end
  end

  # >> is replaced with >>> by the compiler
  def l >>> r do
    fn x -> l.(r.(x)) end
  end


  #  not/1 is inlined by the compiler
  @spec xor(boolean, boolean) :: boolean
  curry xor/2
  def xor(a, b) do
    a && Elmchemy.!().(b) || Elmchemy.!().(a) && b
  end

  @spec negate(number) :: number
  curry negate/1
  def negate(x) do
    -(x)
  end


  @spec sqrt(float) :: float
  curry sqrt/1
  def sqrt(x) do
    :math.sqrt(x)
  end

  @spec clamp(any, any, any) :: any
  curry clamp/3
  def clamp(x, bottom, top) do
    x |> min.(bottom) |> max.(top)
  end

  @spec log_base(float, float) :: float
  curry log_base/2
  def log_base(a, b) do
    not_implemented
  end

  @spec e :: float
  def e() do
    2.71828
  end

  @spec pi :: float
  def pi() do
    apply(":math", "pi", [])
  end

  @spec cos(float) :: float
  curry cos/1
  def cos(x) do
    :math.cos(x)
  end

  @spec sin(float) :: float
  curry sin/1
  def sin(x) do
    :math.sin(x)
  end

  @spec tan(float) :: float
  curry tan/1
  def tan(x) do
    :math.tan(x)
  end

  @spec acos(float) :: float
  curry acos/1
  def acos(x) do
    :math.acos(x)
  end

  @spec asin(float) :: float
  curry asin/1
  def asin(x) do
    :math.asin(x)
  end

  @spec atan(float) :: float
  curry atan/1
  def atan(x) do
    :math.atan(x)
  end

  @spec atan2(float, float) :: float
  curry atan2/2
  def atan2(x, y) do
    :math.atan2(x)
  end

  @spec round(float) :: integer
  curry round/1
  def round(x) do
    round(x)
  end

  @spec floor(float) :: integer
  curry floor/1
  def floor(x) do
    not_implemented
  end

  @spec ceiling(float) :: integer
  curry ceiling/1
  def ceiling(x) do
    not_implemented
  end

  @spec truncate(float) :: integer
  curry truncate/1
  def truncate(x) do
    not_implemented
  end

  @spec to_float(integer) :: float
  curry to_float/1
  def to_float(x) do
    Elmchemy.*(x, 1)
  end

  @spec to_string(any) :: String.t
  curry to_string/1
  def to_string(x) do
    to_string(x)
  end

  curry ++/2
  def a ++ b do
    cond do
      is_string(a) && is_string(b) -> Elmchemy.<>(a, b)
      true -> Elmchemy.++(a, b)
    end
  end

  @spec indentity(any) :: any
  curry indentity/1
  def indentity(a) do
    a
  end

  @spec id(any) :: any
  def id() do
    identity
  end

  @spec always(any, any) :: any
  curry always/1
  def always(a) do
    fn(_) -> a end
  end

  curry |>/2
  def left |> fun do
    Kernel.|>(left, fun)
  end

  #  TODO Will be fixed with #34
  @spec curried(({any, any} -> any)) :: ((any -> any) -> any)
  curry curried/1
  def curried(fun) do
    fn fst -> fn snd -> fun.({fst, snd}) end end
  end

  @spec uncurried(((any -> any) -> any)) :: ({any, any} -> any)
  curry uncurried/1
  def uncurried(fun) do
    fn {fst, snd} -> fun.(fst).(snd) end
  end


  #  We don't care for Never type
  @spec not_implemented :: any
  def not_implemented() do
    throw("Not implemented")
  end

end
