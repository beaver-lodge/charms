defmodule Charms.Intrinsic do
  @moduledoc """
  Behaviour to define intrinsic functions.
  """
  alias Beaver
  @type opt :: {:ctx, MLIR.Context.t()} | {:block, MLIR.Block.t() | {:loc, MLIR.Location.t()}}
  @type opts :: [opt | {atom(), term()}]
  @type ir_return :: MLIR.Value.t() | MLIR.Operation.t()
  @type intrinsic_return :: ir_return() | (any() -> ir_return())
  @doc """
  Callback to implement an intrinsic.

  Having different return types, there are two kinds of intrinsic functions:
  - Regular: returns a MLIR value or operation.
  - Higher-order: returns a function that returns a MLIR value or operation.

  ## More on higher-order intrinsic
  Higher-order intrinsic function can be variadic, which means it a list will be passed as arguments.
  """
  @callback handle_intrinsic(atom(), [Macro.t()], [term()], opts()) :: intrinsic_return()
  Module.register_attribute(__MODULE__, :defintrinsic, accumulate: true)

  @doc false
  def collect_intrinsics(nil) do
    raise ArgumentError, "no intrinsic functions defined"
  end

  def collect_intrinsics(attr_list) when length(attr_list) > 0 do
    attr_list |> Enum.reverse() |> List.flatten() |> Enum.uniq()
  end

  defmacro __using__(_) do
    quote do
      @behaviour Charms.Intrinsic
      use Beaver
      @before_compile Charms.Intrinsic
      import Charms.Intrinsic, only: :macros
    end
  end

  defmacro defintrinsic(intrinsic_list) do
    quote do
      @defintrinsic unquote(intrinsic_list)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      @defintrinsic_list @defintrinsic |> Charms.Intrinsic.collect_intrinsics()
      def __intrinsics__() do
        @defintrinsic_list
      end
    end
  end
end
