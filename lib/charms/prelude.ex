defmodule Charms.Prelude do
  @moduledoc """
  Intrinsic module to define essential functions provided by Charms.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.{Func, CF, Arith}
  @enif_functions Beaver.ENIF.functions()

  defp literal_to_constant(v, t, %Opts{ctx: ctx, blk: blk, loc: loc})
       when is_integer(v) or is_float(v) do
    mlir ctx: ctx, blk: blk do
      Charms.Constant.from_literal(v, t, ctx, blk, loc)
    end
  end

  defp literal_to_constant(v, _, _) do
    v
  end

  defintr result_at(%MLIR.Operation{} = op, index) do
    num_results = Beaver.Walker.results(op) |> Enum.count()

    if index < num_results do
      Beaver.Walker.results(op)[index]
    else
      raise ArgumentError,
            "Index #{index} is out of bounds for operation results, num results: #{num_results}"
    end
  end

  @doc """
  Get the MLIR type of the given value.
  """
  defintr type_of(value) do
    MLIR.Value.type(value)
  end

  defintr const(ast) do
    {:"::", _type_meta, [value, type]} = ast
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      Charms.Constant.from_literal(value, type, ctx, blk, loc)
    end
  end

  @doc """
  Dump the MLIR entity at compile time with `IO.puts/1`
  """
  defintr dump(entity) do
    entity |> tap(&IO.puts(MLIR.to_string(&1)))
  end

  @doc """
  Syntactic sugar for `Charms.Pointer.allocate/1`.
  """
  defintr ptr!(t) do
    {
      quote do
        Charms.Pointer.allocate(t)
      end,
      t: t
    }
  end

  @doc """
  Syntactic sugar for `Charms.Pointer.allocate/2`.
  """
  defintr ptr!(t, n) do
    {
      quote do
        Charms.Pointer.allocate(t, n)
      end,
      t: t, n: n
    }
  end

  @doc """
  Syntactic sugar for `Charms.Pointer.free/1`.
  """
  defintr free!(ptr) do
    {
      quote do
        Charms.Pointer.free(ptr)
      end,
      ptr: ptr
    }
  end

  defintr unreachable!() do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      f = Arith.constant(false) >>> Type.i1()
      loc = Beaver.Deferred.create(loc, ctx)
      msg = MLIR.Attribute.string("Unreachable code reached. #{to_string(loc)}")
      CF.assert(f, loc: loc, msg: msg) >>> []
    end
  end

  defp extract_raw_pointer(arg, arg_type, %Opts{ctx: ctx} = opts) do
    if MLIR.equal?(~t{!llvm.ptr}.(ctx), arg_type) and Charms.Pointer.memref_ptr?(arg) do
      Charms.Pointer.extract_raw_pointer(arg, opts)
    else
      arg
    end
  end

  defp preprocess_args(args, arg_types, []) do
    for {{arg, arg_type}, i} <- args |> Enum.zip(arg_types) |> Enum.with_index() do
      if not MLIR.equal?(MLIR.Value.type(arg), arg_type) do
        raise ArgumentError,
              "Expected arg##{i} of type #{MLIR.to_string(arg_type)}, got #{MLIR.to_string(MLIR.Value.type(arg))}"
      end

      arg
    end
  end

  defp preprocess_args(args, arg_types, [preprocessor | tail]) do
    for {arg, arg_type} <- args |> Enum.zip(arg_types) do
      preprocessor.(arg, arg_type)
    end
    |> preprocess_args(arg_types, tail)
  end

  defp call_enif(name, args, %Opts{ctx: ctx, blk: blk, loc: loc} = opts) do
    {arg_types, ret_types} = Beaver.ENIF.signature(ctx, name)

    args =
      preprocess_args(args, arg_types, [
        &literal_to_constant(&1, &2, opts),
        &extract_raw_pointer(&1, &2, opts)
      ])

    mlir ctx: ctx, blk: blk do
      Func.call(args, callee: Attribute.flat_symbol_ref(name), loc: loc) >>> ret_types
    end
  end

  signature_ctx = MLIR.Context.create()

  for name <- @enif_functions do
    arity = Beaver.ENIF.signature(signature_ctx, name) |> elem(0) |> length()
    args = Macro.generate_arguments(arity, __MODULE__)

    defintr unquote(name)(unquote_splicing(args)) do
      call_enif(unquote(name), unquote(args), __IR__)
    end

    if String.starts_with?(Atom.to_string(name), "enif_get_") or name == :enif_send do
      defintr unquote(String.to_atom("#{name}!"))(unquote_splicing(args)) do
        {
          quote do
            if ret == 0 do
              unreachable!()
            else
              ret
            end
          end,
          ret: call_enif(unquote(name), unquote(args), __IR__)
        }
      end
    end
  end

  MLIR.Context.destroy(signature_ctx)

  @doc false
  def intrinsics() do
    @enif_functions ++ [:result_at]
  end

  defintr i32() do
    MLIR.Type.i32(ctx: __IR__.ctx)
  end

  defintr i64() do
    MLIR.Type.i64(ctx: __IR__.ctx)
  end

  defintr f32() do
    MLIR.Type.f32(ctx: __IR__.ctx)
  end

  defintr f64() do
    MLIR.Type.f64(ctx: __IR__.ctx)
  end

  defintr index() do
    MLIR.Type.index(ctx: __IR__.ctx)
  end

  defintr bool() do
    MLIR.Type.i1(ctx: __IR__.ctx)
  end

  defintr unranked_tensor(elem_t) do
    MLIR.Type.unranked_tensor!(elem_t)
  end
end
