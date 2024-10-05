defmodule Charms do
  @moduledoc """
  Documentation for `Charms`.
  """

  defmacro __using__(opts) do
    quote do
      import Charms.Defm
      use Beaver
      require Beaver.MLIR.Dialect.Func
      alias Beaver.MLIR.Dialect.{Func, Arith, LLVM, CF}
      alias Beaver.MLIR.{Type, Attribute}
      import Type

      @before_compile Charms
      Module.register_attribute(__MODULE__, :defm, accumulate: true)
      Module.register_attribute(__MODULE__, :init_at_fun_call, persist: true)
      @init_at_fun_call Keyword.get(unquote(opts), :init, true)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      {ir, referenced_modules} = @defm |> Enum.reverse() |> Charms.Defm.compile_definitions()
      @ir ir
      @referenced_modules referenced_modules

      def __ir__ do
        @ir
      end

      def __referenced_modules__ do
        @referenced_modules
      end
    end
  end

  def child_spec(mod, opts \\ [])

  def child_spec(mods, opts) when is_list(mods) do
    %{id: Module.concat(mods), start: {Charms.JIT, :init, [mods, opts]}}
  end

  def child_spec(mod, opts) do
    %{id: mod, start: {Charms.JIT, :init, [mod, opts]}}
  end
end
