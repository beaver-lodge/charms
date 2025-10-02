defmodule Charms.Transform do
  use Beaver
  alias MLIR.Dialect.Transform

  def put_jit_transforms(%MLIR.Module{} = module) do
    mlir ctx: MLIR.context(module) do
      m =
        module "transform.with_named_sequence": MLIR.Attribute.unit() do
          Transform.named_sequence sym_name: MLIR.Attribute.string("__transform_main"),
                                   function_type: MLIR.Type.function([~t{!transform.any_op}], []) do
            region do
              block _(_input >>> ~t{!transform.any_op}) do
                Transform.yield() >>> []
              end
            end
          end >>> []
        end

      m = MLIR.Operation.from_module(m)
      MLIR.Block.append(MLIR.Module.body(module), m)
    end
  end

  @gpu_transform "lib/charms/transforms/gpu.mlir"
  @external_resource @gpu_transform
  @gpu File.read!(@gpu_transform)
  def put_gpu_transforms(%MLIR.Module{} = module) do
    m = ~m{#{@gpu}}.(MLIR.context(module)) |> MLIR.Operation.from_module()
    MLIR.Block.append(MLIR.Module.body(module), m)
  end
end
