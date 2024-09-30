defmodule ChildMod2 do
  use Charms
  alias Charms.Term

  defm term_roundtrip(env, i) :: Term.t() do
    i = call ChildMod.term_roundtrip(env, i) :: Term.t()
    func.return(i)
  end
end
