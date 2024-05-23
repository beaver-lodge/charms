defmodule Charms.Env do
  use Beaver

  def handle_intrinsic(:t, [], opts) do
    Beaver.ENIF.Type.env(opts)
  end
end
