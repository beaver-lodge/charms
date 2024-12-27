arr = Enum.to_list(1..10000) |> Enum.shuffle()
ENIFQuickSort.sort(arr)
ENIFMergeSort.sort(arr)
ENIFTimSort.sort(arr)

Benchee.run(
  %{
    "Enum.sort" => &Enum.sort/1,
    "enif_quick_sort" => &ENIFQuickSort.sort(&1)
  },
  parallel: 2,
  warmup: 1,
  time: 3,
  inputs: [10, 1000, 100_000, 1_000_000] |> Enum.map(&{"array size #{&1}", &1}) |> Enum.into(%{}),
  before_scenario: fn i ->
    Enum.to_list(1..i) |> Enum.shuffle()
  end
)
