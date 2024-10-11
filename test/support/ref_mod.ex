defmodule RefereeMod do
  use Charms
  alias Charms.Term

  defm term_roundtrip0(env, i) :: Term.t() do
    func.return(i)
  end

  defm term_roundtrip1(i) :: Term.t() do
    func.return(i)
  end
end

defmodule ReferrerMod do
  use Charms
  alias Charms.Term

  defm term_roundtrip(env, i) :: Term.t() do
    i = call RefereeMod.term_roundtrip0(env, i) :: Term.t()
    i = call RefereeMod.term_roundtrip1(i) :: Term.t()
    func.return(i)
  end
end
