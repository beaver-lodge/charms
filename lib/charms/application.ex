defmodule Charms.Application do
  @moduledoc false

  def start(_args, _type) do
    children = [
      Charms.JIT.LockedCache,
      Charms.ContextPool.specs()
    ]

    if Charms.Debug.global_debug_enabled?() do
      Beaver.MLIR.CAPI.mlirEnableGlobalDebug(true)
    end

    Supervisor.start_link(children, name: __MODULE__, strategy: :one_for_one)
  end
end
