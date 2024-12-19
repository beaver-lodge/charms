arr = Enum.to_list(1..10000) |> Enum.shuffle()
ENIFQuickSort.sort(arr)
ENIFMergeSort.sort(arr)
ENIFTimSort.sort(arr)

Benchee.run(
  %{
    "Enum.sort" => &Enum.sort/1,
    "enif_quick_sort" => &ENIFQuickSort.sort(&1),
    "enif_merge_sort" => &ENIFMergeSort.sort(&1),
    "enif_tim_sort" => &ENIFTimSort.sort(&1)
  },
  parallel: 2,
  inputs: [10, 100, 1000, 10000] |> Enum.map(&{"array size #{&1}", &1}) |> Enum.into(%{}),
  before_scenario: fn i ->
    Enum.to_list(1..i) |> Enum.shuffle()
  end
)
