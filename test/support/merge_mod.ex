defmodule SubMod0 do
  use Charms
  alias Charms.Term

  defm get_term(env, i) :: Term.t() do
    func.return(i)
  end

  @a __MODULE__
  defm get_attr(env) :: Term.t() do
    func.return(@a)
  end

  @b :some_attr
  defm get_attr_dup(env) :: Term.t() do
    func.return(@b)
  end
end

defmodule SubMod1 do
  use Charms
  alias Charms.Term

  defm get_term(env, i) :: Term.t() do
    i = call SubMod0.get_term(env, i)
    func.return(i)
  end

  @a __MODULE__
  defm get_attr(env) :: Term.t() do
    func.return(@a)
  end

  @b :some_attr
  defm get_attr_dup(env) :: Term.t() do
    func.return(@b)
  end
end
