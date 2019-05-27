defmodule Times do
  def double(n), do: n * 2
  #ModulesAndFunctions-1
  def triple(n), do: n * 3
  #ModulesAndFunctions-3
  def quadruple(n), do: double(double(n))
end

defmodule RecursiveSum do
  def sum(0), do: 0
  def sum(n), do: n + sum(n-1)
end

defmodule Maths do
  def gcd(x, 0), do: x
  def gcd(x, y), do: gcd(y, rem(x,y))
end

defmodule Chop do
  def guess(n, range = bottom..top) do
    half_range = div(bottom + top, 2)
    IO.puts "Is it #{half_range}?"
    try_guess(n, half_range, range)
  end

  defp try_guess(n, n, _) do
    IO.puts "Hurray! It is #{n}"
  end

  defp try_guess(n, trial, _bottom..top)
    when trial < n do
      guess(n, trial + 1..top)
  end

  defp try_guess(n, trial, bottom.._top)
    when trial > n do
      guess(n, bottom..trial-1)
  end
end
