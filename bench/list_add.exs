mod = AddTwoIntVec
Charms.JIT.init(mod)

Benchee.run(
  %{
    "Enum.zip_reduce" => fn {a, b} ->
      Enum.zip_reduce(a, b, [], fn x, y, acc -> [x + y | acc] end) |> Enum.reverse()
    end,
    "AddTwoIntVec.add" => fn {a, b} -> AddTwoIntVec.add(a, b, :err_msg) end,
    "AddTwoIntVec.dummy_load_no_make" => fn {a, b} -> AddTwoIntVec.dummy_load(a, b, :err_msg) end,
    "AddTwoIntVec.dummy_return" => fn {a, b} -> AddTwoIntVec.dummy_return(a, b, :err_msg) end
  },
  inputs: %{
    "array size 8" => 8
  },
  before_scenario: fn i ->
    a = Enum.to_list(1..i) |> Enum.shuffle()
    b = Enum.to_list(1..i) |> Enum.shuffle()
    {a, b}
  end
)

Charms.JIT.destroy(mod)
