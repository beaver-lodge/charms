defmodule RefereeMod do
  @moduledoc false
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
  @moduledoc false
  use Charms
  alias Charms.Term

  defm term_roundtrip(env, i) :: Term.t() do
    i = RefereeMod.term_roundtrip0(env, i)
    i = RefereeMod.term_roundtrip1(i)
    func.return(i)
  end
end
