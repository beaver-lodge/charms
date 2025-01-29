defmodule Charms.ContextPool do
  @moduledoc false
  @behaviour NimblePool

  alias Beaver.MLIR
  @impl NimblePool
  def init_worker(pool_state) do
    {:ok, MLIR.Context.create(), pool_state}
  end

  @impl NimblePool
  def handle_checkout(:checkout, _, ctx, pool_state) do
    {:ok, ctx, ctx, pool_state}
  end

  @impl NimblePool
  def terminate_worker(_reason, ctx, pool_state) do
    :ok = MLIR.Context.destroy(ctx)
    {:ok, pool_state}
  end

  def specs() do
    {NimblePool, worker: {__MODULE__, :dummy_pool_state}, name: __MODULE__}
  end
end
