defmodule Charms do
  @moduledoc """
  Documentation for `Charms`.

  ## `defm` and intrinsic
  There are two ways to define a function with `defm/2` or implement callbacks of `Charms.Intrinsic` behavior. The `defm/2` is a macro that generates a function definition in Charm. The intrinsic is a behavior that generates a function definition in MLIR.

  The intrinsic is more flexible than `defm` because:
  - Intrinsic can be variadic and its argument can be anything
  - Intrinsic is suitable for the cases where directly writing or generating MLIR is more ideal
  - An intrinsic should be responsible for its type check while the Charm’s type system is responsible for function call’s type check

  The `defm` is more suitable for simple functions because it is designed to be as close to vanilla Elixir as possible. As a rule of thumb, use `defm` for simple functions and intrinsic for complex functions or higher-order(generic) function with type as argument.

  ## `defm`'s differences from `Beaver.>>>/2` op expressions
  - In `Beaver.>>>/2`, MLIR code are expected to mixed with regular Elixir code. While in `defm/2`, there is only Elixir code (a subset of Elixir, to be more precise).
  - In `defm/2`, the extension of the compiler happens at the function level (define your intrinsics or `defm/2`s), while in `Beaver.>>>/2`, the extension happens at the op level (define your op expression).
  - In `Beaver.>>>/2` the management of MLIR context and other resources are done by the user, while in `defm/2`, the management of resources are done by the `Charms` compiler.
  - In `defm/2`, there is expected to be extra verifications built-in to the `Charms` compiler (both syntax and types), while in `Beaver.>>>/2`, there is none.

  ## Caveats and limitations

  - We need a explicit `call` in function call because the `::` special form has a parser priority  that is too low so a `call` macro is introduced to ensure proper scope.
  - Being variadic, intrinsic must be called with the module name. `import` doesn't work with intrinsic functions while `alias` is supported.

  ## Glossary of modules

  - `Charms`: the top level macros `defm` and `use Charms`
  - `Charms.Defm`: the `defm` DSL syntax and special forms
  - `Charms.Defm.Definition`: functions to define and compile `defm` functions to MLIR
  - `Charms.Intrinsic`: the behavior and define and compile intrinsic functions
  """

  defmacro __using__(opts) do
    quote do
      import Charms
      use Beaver
      import Beaver.MLIR.Type
      @doc false
      def __use_ir__, do: nil
      @before_compile Charms
      Module.register_attribute(__MODULE__, :defm, accumulate: true)
      Module.register_attribute(__MODULE__, :init_at_fun_call, persist: true)
      @init_at_fun_call Keyword.get(unquote(opts), :init, true)
    end
  end

  defmacro __before_compile__(env) do
    defm_decls = Module.get_attribute(env.module, :defm) || []
    {ir, referenced_modules} = defm_decls |> Enum.reverse() |> Charms.Defm.Definition.compile()

    # create uses in Elixir, to disallow loop reference
    r =
      for r <- referenced_modules, r != env.module do
        quote do
          unquote(r).__use_ir__
        end
      end

    quote do
      @ir unquote(ir)
      @referenced_modules unquote(referenced_modules)
      unquote_splicing(r)

      @ir_hash [
        :erlang.phash2(@ir)
        | for r <- @referenced_modules, r != __MODULE__ do
            r.__ir__hash__()
          end
      ]

      @doc false
      def __ir__ do
        @ir
      end

      @doc false
      def __ir__hash__ do
        @ir_hash
      end

      @doc false
      def referenced_modules do
        @referenced_modules
      end

      defoverridable referenced_modules: 0
    end
  end

  @doc """
  define a function that can be JIT compiled
  """
  defmacro defm(call, body \\ []) do
    Charms.Defm.Definition.declare(__CALLER__, call, body)
  end
end
