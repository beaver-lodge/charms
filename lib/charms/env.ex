defmodule Charms.Env do
  @moduledoc """
  Intrinsic module for BEAM environment's type.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts

  defintr t() do
    %Opts{ctx: ctx} = __IR__
    Beaver.ENIF.Type.env(ctx: ctx)
  end
end
