defmodule Charms.Defm.Expander.Std do
  @moduledoc """
  Expand standard library functions in Charms defm.
  """
  use Beaver
  alias Charms.Defm.Expander
  alias Charms.Defm.Expander.MLIRImporter
  alias MLIR.Dialect.{SCF, MemRef, Index}

  def expand(Enum, :reduce, args, state, env) do
    while =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        [l, init, f] = args
        {l, state, env} = Expander.expand(l, state, env)
        {init, state, env} = Expander.expand(init, state, env)
        result_t = MLIR.Value.type(init)

        {{tail_ptr, head_ptr}, _state, _env} =
          quote do
            tail_ptr = Charms.Pointer.allocate(Term.t())
            Charms.Pointer.store(l, tail_ptr)
            head_ptr = Charms.Pointer.allocate(Term.t())
            {tail_ptr, head_ptr}
          end
          |> Expander.expand_with_bindings(state, env, l: l)

        # we compile the Enum.reduce/3 to a scf.while in MLIR
        SCF.while [init] do
          region do
            block _(acc >>> result_t) do
              state = put_in(state.mlir.blk, Beaver.Env.block())

              # getting the BEAM env, assuming it is a regular defm with env as the first argument
              env_ptr = Charms.Defm.Expander.Call.beam_env_from_defm!(env, state)

              # the condition of the while loop, consuming the list with enif_get_list_cell
              {condition, _state, _env} =
                quote do
                  enif_get_list_cell(
                    env_ptr,
                    Pointer.load(Term.t(), tail_ptr),
                    head_ptr,
                    tail_ptr
                  ) > 0
                end
                |> Expander.expand_with_bindings(state, env,
                  tail_ptr: tail_ptr,
                  head_ptr: head_ptr,
                  env_ptr: env_ptr
                )

              SCF.condition(condition, acc) >>> []
            end
          end

          # the body of the while loop, compiled from the reducer which is an anonymous function
          region do
            block _(acc >>> result_t) do
              state = put_in(state.mlir.blk, Beaver.Env.block())
              {:fn, _, [{:->, _, [[arg_element, arg_acc], body]}]} = f

              # inject head and acc before expanding the body
              state = update_in(state.mlir, &MLIRImporter.put_var(&1, arg_acc, acc))

              {head_val, state, env} =
                quote(do: Charms.Pointer.load(Charms.Term.t(), head_ptr))
                |> Expander.expand_with_bindings(state, env, head_ptr: head_ptr)

              state = update_in(state.mlir, &MLIRImporter.put_var(&1, arg_element, head_val))
              # expand the body
              {body, _state, _env} = Expander.expand(body, state, env)
              SCF.yield(body) >>> []
            end
          end
        end >>> result_t
      end

    {while, state, env}
  end

  def expand(String, :length, [string], state, env) do
    {string, state, env} = Expander.expand(string, state, env)

    len =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
        MemRef.dim(string, zero) >>> :infer
      end

    {len, state, env}
  end
end
