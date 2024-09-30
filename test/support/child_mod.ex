defmodule ChildMod do
  use Charms
  alias Charms.Term

  defm term_roundtrip(env, i) :: Term.t() do
    func.return(i)
  end
end
