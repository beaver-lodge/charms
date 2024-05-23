defmodule Charms.Term do
  use Beaver

  def handle_intrinsic(:t, [], opts) do
    Beaver.ENIF.Type.term(opts)
  end
end
