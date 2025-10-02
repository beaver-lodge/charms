defmodule Charms.Application do
  @moduledoc false
  alias Beaver.MLIR

  def start(_args, _type) do
    children = [
      Charms.JIT.LockedCache,
      Charms.ContextPool.specs()
    ]

    if Charms.Debug.global_debug_enabled?() do
      MLIR.Debug.enable_global_debug(true)
      MLIR.Debug.set_debug_type(~w[pass-manager dialect-conversion])
    end

    Supervisor.start_link(children, name: __MODULE__, strategy: :one_for_one)
  end
end
