defmodule SortUtil do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term}

  defm copy_terms(env, movable_list_ptr :: Pointer.t(Term.t()), arr :: Pointer.t(Term.t())) do
    head = Pointer.allocate(Term.t())
    zero = const 0 :: i32()
    i_ptr = Pointer.allocate(i32())
    set! i_ptr[0], zero

    while(
      enif_get_list_cell(
        env,
        movable_list_ptr[0],
        head,
        movable_list_ptr
      ) > 0
    ) do
      head_val = head[0]
      i = i_ptr[0]
      set! arr[i], head_val
      set! i_ptr[0], i + 1
    end
  end

  defm merge(arr :: Pointer.t(Term.t()), l :: i32(), m :: i32(), r :: i32()) do
    n1 = m - l + 1
    n2 = r - m

    left_temp = Pointer.allocate(Term.t(), n1)
    right_temp = Pointer.allocate(Term.t(), n2)

    for_loop {element, i} <- {arr + l, n1} do
      set! left_temp[i], element
    end

    for_loop {element, j} <- {arr + m + 1, n2} do
      set! right_temp[j], element
    end

    i_ptr = Pointer.allocate(i32())
    j_ptr = Pointer.allocate(i32())
    k_ptr = Pointer.allocate(i32())

    zero = const 0 :: i32()
    set! i_ptr[0], zero
    set! j_ptr[0], zero
    set! k_ptr[0], l

    while i_ptr[0] < n1 && j_ptr[0] < n2 do
      i = i_ptr[0]
      j = j_ptr[0]
      k = k_ptr[0]

      left_term = left_temp[i]
      right_term = right_temp[j]

      if enif_compare(left_term, right_term) <= 0 do
        set! arr[k], left_temp[i]
        set! i_ptr[0], i + 1
      else
        set! arr[k], right_temp[j]
        set! j_ptr[0], j + 1
      end

      set! k_ptr[0], k + 1
    end

    while i_ptr[0] < n1 do
      i = i_ptr[0]
      k = k_ptr[0]
      set! arr[k], left_temp[i]
      set! i_ptr[0], i + 1
      set! k_ptr[0], k + 1
    end

    while j_ptr[0] < n2 do
      j = j_ptr[0]
      k = k_ptr[0]
      set! arr[k], right_temp[j]
      set! j_ptr[0], j + 1
      set! k_ptr[0], k + 1
    end
  end
end
