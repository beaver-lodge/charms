defmodule DifferentCalls do
  @moduledoc false
  use Charms
  alias Charms.Term

  defm return_type_annotation(env, i) :: Term.t() do
    call RefereeMod.term_roundtrip1(i) :: Term.t()
  end

  defm no_return_type_annotation(env, i) :: Term.t() do
    call RefereeMod.term_roundtrip1(i)
  end

  defm without_call_macro(env, i) :: Term.t() do
    RefereeMod.term_roundtrip1(i)
  end

  defm forward_call(i) :: Term.t() do
    call defined_later(i)
  end

  defm forward_call_with_type(i) :: Term.t() do
    call defined_later(i) :: Term.t()
  end

  defm without_call_macro_forward_call(i) :: Term.t() do
    defined_later(i)
  end

  defm defined_later(i) :: Term.t() do
    func.return(i)
  end
end
