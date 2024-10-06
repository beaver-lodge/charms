defmodule Charms.Intrinsic do
  @moduledoc """
  Behaviour to define intrinsic functions.
  """
  alias Beaver
  @type opt :: {:ctx, MLIR.Context.t()} | {:block, MLIR.Block.t()}
  @type opts :: [opt | {atom(), term()}]
  @callback handle_intrinsic(atom(), [term()], opts()) :: term()

  defmacro __using__(_) do
    quote do
      @behaviour Charms.Intrinsic
      use Beaver
    end
  end
end
