defmodule Charms do
  @moduledoc """
  Documentation for `Charms`.

  ## `defm` and intrinsic
  There are two ways to define a function with `defm/2` or implement callbacks of `Charms.Intrinsic` behavior. The `defm/2` is a macro that generates a function definition in Charm. The intrinsic is a behavior that generates a function definition in MLIR.

  The intrinsic is more flexible than `defm` because:
  - Intrinsic is suitable for the cases where directly writing or generating MLIR is more ideal
  - An intrinsic should be responsible for its type check while the Charm's type system is responsible for function call's type check
  - It is possible for an intrinsic to return a MLIR type, while `defm` can only return value.
  - Intrinsic function is always inline.

  The `defm` is more suitable for simple functions because it is designed to be as close to vanilla Elixir as possible. As a rule of thumb, use `defm` for simple functions and intrinsic for complex functions or function with type as argument.

  ## `defm`'s differences from `Beaver.>>>/2` op expressions
  - In `Beaver.>>>/2`, MLIR code are expected to mixed with regular Elixir code. While in `defm/2`, there is only Elixir code (a subset of Elixir, to be more precise).
  - In `defm/2`, the extension of the compiler happens at the function level (define your intrinsics or `defm/2`s), while in `Beaver.>>>/2`, the extension happens at the op level (define your op expression).
  - In `Beaver.>>>/2` the management of MLIR context and other resources are done by the user, while in `defm/2`, the management of resources are done by the `Charms` compiler.
  - In `defm/2`, there is expected to be extra verifications built-in to the `Charms` compiler (both syntax and types), while in `Beaver.>>>/2`, there is none.

  ## Macros in `Charms` module and `Charms.Defm.Expander`

  `Charms.Defm.Expander` will do the job of expanding the macros like `defm/2` to its corespondent MLIR entity.
  While the macro definitions in `Charms` module will generate the code to integrate with Elixir's compiler and module system.
  For example, macro `defm/2` will generate the code to call the JIT function.

  ## Caveats and limitations

  - We need explicit `value` or `op` macro, because the `::` special form has a parser priority that is too low so an extra macro is introduced to ensure proper scope.

  ## Glossary of modules

  - `Charms`: the top level macros `defm`, `defk` and `use Charms`
  - `Charms.Defm`: the `defm` DSL syntax and special forms
  - `Charms.Definition`: functions to define and compile `defm` functions to MLIR
  - `Charms.Intrinsic`: the behavior used to define and compile intrinsic functions
  """

  defmacro __using__(opts) do
    quote do
      import Charms
      use Beaver
      import Charms.Prelude
      import Charms.GPU
      @doc false
      def __use_ir__, do: nil
      @before_compile Charms
      Module.register_attribute(__MODULE__, :__charm_function__, accumulate: true)
      Module.register_attribute(__MODULE__, :__charm_struct__, accumulate: false)
      Module.register_attribute(__MODULE__, :init_at_fun_call, persist: true)
      @init_at_fun_call Keyword.get(unquote(opts), :init, true)

      @spec dynamic_libraries() :: [String.t()]
      def dynamic_libraries() do
        []
      end

      @doc false
      def expand_struct(_mlir_expander) do
        nil
      end

      defoverridable dynamic_libraries: 0, expand_struct: 1
    end
  end

  defp compile_module_attributes(env) do
    defm_definitions =
      (Module.get_attribute(env.module, :__charm_function__) || []) |> Enum.reverse()

    defmstruct_definition = Module.get_attribute(env.module, :__charm_struct__)
    Charms.Definition.compile(defm_definitions, defmstruct_definition)
  end

  defmacro __before_compile__(env) do
    {ir, referenced_modules, required_intrinsic_modules, exports} = compile_module_attributes(env)
    use_ir = generate_use_ir_quotes(referenced_modules, env.module)
    intrinsic = generate_use_intrinsic_quotes(required_intrinsic_modules, env.module)
    ir_exports = generate_ir_exports_quotes()

    quote do
      @ir unquote(ir)
      @referenced_modules unquote(referenced_modules)
      unquote_splicing(use_ir)
      unquote_splicing(intrinsic)
      @all_exports unquote(Macro.escape(exports))
      unquote(ir_exports)
      def __ir_exports__(_, _), do: nil
      unquote(generate_helper_functions())
      @doc false
      def infer_argument_type(_fa, _index, _state), do: nil
      def infer_return_type(_fa, _state), do: nil
    end
  end

  defp generate_use_ir_quotes(referenced_modules, current_module) do
    for r <- referenced_modules, r != current_module do
      quote do
        unquote(r).__use_ir__
      end
    end
  end

  defp generate_use_intrinsic_quotes(required_intrinsic_modules, current_module) do
    for r <- required_intrinsic_modules, r != current_module do
      quote do
        unquote(r).__use_intrinsic__
      end
    end
  end

  defp generate_ir_exports_quotes() do
    quote bind_quoted: [] do
      for {name, arity, func_name} <- @all_exports do
        def __ir_exports__(unquote(name), unquote(arity)) do
          unquote(func_name)
        end
      end
    end
  end

  defp generate_helper_functions() do
    quote do
      @doc false
      def __ir__ do
        @ir
      end

      @ir_hash [
                 :erlang.phash2(@ir)
                 | for r <- @referenced_modules, r != __MODULE__ do
                     r.__ir_digest__()
                   end
               ]
               |> List.flatten()
      @doc false
      def __ir_digest__ do
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
  defmacro defm(call, body) do
    Charms.Definition.declare(__CALLER__, call, body)
  end

  defmacro defk(call, body) do
    Charms.Definition.declare(__CALLER__, call, body, convention: :defk)
  end

  @doc """
  Define a Charms function that binds to a C function in a shared library.

  ## Example

      defbind sqrt_c(x: f64) :: f64, c_name: "sqrt_wrapper", lib: "test/support/libsqrt.so"

  """
  defmacro defbind(call) do
    Charms.Definition.declare(__CALLER__, call,
      do:
        quote do
        end
    )
  end

  @doc """
  define a native struct

  define a native struct with the given fields. The fields are defined as a list of tuples, where the first element is the field name and the second element is the field type.

  > #### Native struct's fields are ordered, unlike Elixir's struct fields. {: .info}
  >
  > Note that the order of the fields is important for memory layout. An LLVM struct type will be created with the same order as the fields in the list.
  """
  defmacro defmstruct(fields) do
    quote do
      @__charm_struct__ unquote(
                          Macro.escape(Charms.Defmstruct.Definition.new(__CALLER__, fields))
                        )
      use Charms.Intrinsic
      alias Charms.Intrinsic.Opts

      defintr t() do
        %Opts{ctx: ctx} = __IR__

        %Charms.Defmstruct.Definition{
          fields: fields,
          env: env
        } = @__charm_struct__

        # Expands to defmstruct/2, which is then handled by
        # Charms.Defm.Expander.expand_macro
        {quote(do: defmstruct_impl(unquote(__MODULE__), unquote(fields))), []}
      end
    end
  end

  @doc false
  defmacro defmstruct_impl(_mod, _fields) do
    raise "should only use inside 'defm'"
  end
end
