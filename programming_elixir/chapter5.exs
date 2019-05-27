# Functions-1
list_concat = fn list1, list2 -> list1 ++ list2 end
sum = fn a, b, c -> a + b + c end
pair_tuple_to_list = fn {a, b} -> [a, b] end

IO.puts "Functions-1"
IO.inspect list_concat.([1,2,3], [4,5,6])
IO.inspect sum.(1, 2, 3)
IO.inspect pair_tuple_to_list.( { 8, 7 } )
IO.puts "\n"



# Functions-2
fizz_buzz = fn
    0, 0, _ -> "FizzBuzz"
    0, _, _ -> "Fizz"
    _, 0, _ -> "Buzz"
    _, _, a -> a
end

IO.puts "Functions-2"
IO.inspect fizz_buzz.(0, 0, :hey)
IO.inspect fizz_buzz.(0, :hey, :hey)
IO.inspect fizz_buzz.(:hey, 0, :hey)
IO.inspect fizz_buzz.(:ignored, :ignored, :printed!)
IO.puts "\n"



# Functions-3
fizz_buzz_remainders = fn n -> fizz_buzz.(
    rem(n,3),
    rem(n,5),
    n
) end

IO.puts "Functions-3"

fizz_buzz_remainders_result = for n <- 10..16 do
    fizz_buzz_remainders.(n)
end

IO.inspect fizz_buzz_remainders_result
IO.puts "\n"



# Functions-4
prefix = fn prefixed -> fn postfixed -> "#{prefixed} #{postfixed}" end end

IO.puts "Functions-4"
mrs = prefix.("Mrs")
IO.inspect mrs
IO.inspect mrs.("Smith")
IO.inspect prefix.("Elixir").("Rocks")
IO.puts "\n"



# Functions-5
IO.puts "Functions-5"
IO.inspect Enum.map [1,2,3,4], &(&1 + 2)
IO.inspect Enum.each [1,2,3,4], &(IO.puts &1)
IO.puts "\n"

