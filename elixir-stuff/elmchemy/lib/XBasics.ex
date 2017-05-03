# Compiled using Elmchemy v0.0.17
defmodule Elmchemy.XBasics do
  use Elmchemy

  import Elmchemy
  alias XList
  @type order :: :l_t | :e_q | :g_t
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
      a > b -> :g_t
      a < b -> :l_t
      true -> :e_q
    end
  end

  #  Cant implement >> or << yet
  #  typespec don't work for such types yet

  def l >>> r do
  fn x -> l.(r.(x)) end
  end


  #  not : Bool -> Bool
  #  not x =
  #      ffi "Kernel" "not" x
  #  xor : Bool -> Bool -> Bool
  #  xor a b =
  #      (a && not b) || (not a && b)
  @spec negate(number) :: number
  curry negate/1
  def negate(x) do
    -(x)
  end

  @spec sqrt(:float) :: :float
  curry sqrt/1
  def sqrt(x) do
    :math.sqrt(x)
  end

  @spec clamp(any, any, any) :: any
  curry clamp/3
  def clamp(x, bottom, top) do
    x |> min.(bottom) |> max.(top)
  end

end
