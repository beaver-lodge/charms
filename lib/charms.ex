defmodule Charms do
  @moduledoc """
  Documentation for `Charms`.
  """

  defmacro __using__(_opts) do
    quote do
      import Charms.Defm
      use Beaver
      require Beaver.MLIR.Dialect.Func
      alias Beaver.MLIR.Dialect.{Func, Arith, LLVM, CF}
      alias Beaver.MLIR.{Type, Attribute}
      import Type

      @before_compile Charms
      Module.register_attribute(__MODULE__, :defm, accumulate: true)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      @ir @defm |> Enum.reverse() |> Charms.Defm.compile_definitions()
      def __ir__ do
        @ir
      end
    end
  end
end
