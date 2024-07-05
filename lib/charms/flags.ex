defmodule Charms.Flag do
  alias Beaver.MLIR

  def print_ir_pass(op) do
    if System.get_env("DEFM_PRINT_IR") == "1" do
      MLIR.Transforms.print_ir(op)
    else
      op
    end
  end

  def step_print?() do
    System.get_env("DEFM_PRINT_IR") == "step"
  end
end
