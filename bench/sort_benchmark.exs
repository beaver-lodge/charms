Charms.JIT.init(ENIFQuickSort)
Charms.JIT.init([ENIFTimSort, ENIFMergeSort])

arr = Enum.to_list(1..10000) |> Enum.shuffle()
ENIFQuickSort.sort(arr, :arg_err)
ENIFMergeSort.sort(arr, :arg_err)
ENIFTimSort.sort(arr, :arg_err)

Benchee.run(
  %{
    "Enum.sort" => &Enum.sort/1,
    "enif_quick_sort" => &ENIFQuickSort.sort(&1, :arg_err),
    "enif_merge_sort" => &ENIFMergeSort.sort(&1, :arg_err),
    "enif_tim_sort" => &ENIFTimSort.sort(&1, :arg_err)
  },
  inputs: %{
    "array size 10" => 10,
    "array size 100" => 100,
    "array size 1000" => 1000,
    "array size 10000" => 10000
  },
  before_scenario: fn i ->
    Enum.to_list(1..i) |> Enum.shuffle()
  end
)

Charms.JIT.destroy(ENIFMergeSort)
Charms.JIT.destroy(ENIFQuickSort)
Charms.JIT.destroy(ENIFTimSort)
