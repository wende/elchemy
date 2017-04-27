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


  @spec compare(any, any) :: order
  curry compare/2
  def compare(a, b) do
    if a > b do
      :g_t
    else if a < b do
      :l_t
    else
      :e_q
    end
  end

  #  Cant implement >> or << yet
  #  typespec don't work for such types yet

  def l >> r do
    fn x -> l.(r.(x)) end
  end


  @spec not(boolean) :: boolean
  curry not/1
  def not(x) do
    not(x)
  end

  @spec xor(boolean, boolean) :: boolean
  curry xor/2
  def xor(a, b) do
    a && not.(b) || not.(a) && b
  end

  @spec negate(number) :: number
  curry negate/1
  def negate(x) do
    -(x)
  end

end
