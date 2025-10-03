defmodule Charms.Transform do
  @moduledoc """
  Charms.Transform provides MLIR transform operations for Charms compilation.
  """
  use Beaver

  @gpu_transform "lib/charms/transforms/gpu.mlir"
  @external_resource @gpu_transform
  @gpu File.read!(@gpu_transform)
  def put_gpu_transforms(%MLIR.Module{} = module) do
    m = ~m{#{@gpu}}.(MLIR.context(module)) |> MLIR.Operation.from_module()
    MLIR.Block.append(MLIR.Module.body(module), m)
  end
end
