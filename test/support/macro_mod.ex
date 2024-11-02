defmodule MacroMod do
  @moduledoc false
  defmacro return(i) do
    quote do
      func.return(unquote(i))
    end
  end
end

defmodule CallMacroMod do
  @moduledoc false
  use Charms
  alias Charms.Term

  defm term_roundtrip1(env, i) :: Term.t() do
    require MacroMod
    MacroMod.return(i)
  end

  defm term_roundtrip2(env, i) :: Term.t() do
    import MacroMod
    return(i)
  end
end
