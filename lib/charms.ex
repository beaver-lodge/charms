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
  """

  defmacro __using__(opts) do
    quote do
      import Charms
      use Beaver
      import Beaver.MLIR.Type

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

      @doc false
      def __ir__ do
        @ir
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
    {call, ret_types} = Charms.Defm.decompose_call_with_return_type(call)

    call = Charms.Defm.normalize_call(call)
    {name, args} = Macro.decompose_call(call)

    {:ok, env} =
      __CALLER__ |> Macro.Env.define_import([], Charms.Defm, warn: false, only: :macros)

    [_enif_env | invoke_args] = args

    invoke_args =
      for {:"::", _, [a, _t]} <- invoke_args do
        a
      end

    quote do
      @defm unquote(Macro.escape({env, {call, ret_types, body}}))
      def unquote(name)(unquote_splicing(invoke_args)) do
        mfa = {unquote(env.module), unquote(name), unquote(invoke_args)}

        cond do
          @init_at_fun_call ->
            {_, %Charms.JIT{engine: engine} = jit} = Charms.JIT.init(__MODULE__)
            Charms.JIT.invoke(engine, mfa)

          (engine = Charms.JIT.engine(__MODULE__)) != nil ->
            Charms.JIT.invoke(engine, mfa)

          true ->
            &Charms.JIT.invoke(&1, mfa)
        end
      end
    end
  end
end
