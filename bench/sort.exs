Charms.JIT.init(ENIFQuickSort)
Charms.JIT.init([ENIFTimSort, ENIFMergeSort])

Benchee.run(
  %{
    "Enum.sort" => fn arr -> Enum.sort(arr) end,
    "enif_quick_sort" => fn arr -> ENIFQuickSort.sort(arr, :arg_err) end,
    "enif_merge_sort" => fn arr -> ENIFMergeSort.sort(arr, :arg_err) end,
    "enif_tim_sort" => fn arr -> ENIFTimSort.sort(arr, :arg_err) end
  },
  inputs:
    %{
      "array size 10" => 10,
      "array size 100" => 100,
      "array size 1000" => 1000
    }
    |> then(fn m ->
      # for some reason, it segfaults on linux with large array size
      if :os.type() == {:unix, :darwin} do
        Map.merge(m, %{"array size 65535" => 65535})
      else
        m
      end
    end),
  before_scenario: fn i ->
    Enum.to_list(1..i) |> Enum.shuffle()
  end
)

Charms.JIT.destroy(ENIFMergeSort)
Charms.JIT.destroy(ENIFQuickSort)
Charms.JIT.destroy(ENIFTimSort)
