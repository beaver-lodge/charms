defmodule Charms.Struct do
  @moduledoc false
  alias Beaver.MLIR
  alias MLIR.Dialect.LLVM
  use Beaver
  import Charms.Diagnostic

  def llvm_struct_type_identified!(name, field_types, opts) do
    Beaver.Deferred.from_opts(
      opts,
      fn ctx ->
        field_types_array = Beaver.Native.array(field_types, MLIR.Type)
        num_fields = length(field_types)

        # Create opaque identified struct first

        struct_type =
          MLIR.CAPI.mlirLLVMStructTypeIdentifiedGet(
            ctx,
            MLIR.StringRef.create(name)
          )

        if MLIR.null?(struct_type) do
          raise "Failed to create struct type"
        end

        # Set the struct body with our fields
        result =
          MLIR.CAPI.mlirLLVMStructTypeSetBody(
            struct_type,
            num_fields,
            field_types_array,
            false
          )

        unless MLIR.LogicalResult.success?(result) do
          raise "Failed to set struct body"
        end

        struct_type
      end
    )
  end

  # serialize the struct type as a module attribute
  @defmstruct_type "chm.defmstruct_type"
  @defmstruct_fields "chm.defmstruct_fields"
  def save_as_module_attribute(ctx, module, struct_type, field_names) do
    field_names =
      field_names |> Enum.map(&MLIR.Attribute.string/1) |> MLIR.Attribute.array(ctx: ctx)

    module = module |> MLIR.Operation.from_module()
    _ = put_in(module[@defmstruct_fields], field_names)
    put_in(module[@defmstruct_type], MLIR.Attribute.type(struct_type))
  end

  def retrieve_struct_type(%m{} = module) when m in [Beaver.MLIR.Module, Beaver.MLIR.Operation] do
    Beaver.Walker.attributes(module)[@defmstruct_type]
    |> then(fn
      %MLIR.Attribute{} = struct_type_attr ->
        MLIR.Attribute.unwrap(struct_type_attr)

      nil ->
        nil
    end)
  end

  def retrieve_struct_type(_), do: nil

  defp retrieve_field_names(module) do
    Beaver.Walker.attributes(module)[@defmstruct_fields]
    |> Stream.map(fn field_name ->
      field_name |> MLIR.Attribute.unwrap() |> MLIR.to_string() |> String.to_atom()
    end)
  end

  def position_of_field!(env, module, field_name) do
    field_names = retrieve_field_names(module)

    position =
      Enum.find_index(field_names, &(&1 == field_name)) ||
        raise_compile_error(
          env,
          "field #{inspect(field_name)} not found, available fields: #{inspect(Enum.to_list(field_names))}"
        )

    MLIR.Attribute.dense_array([position], Beaver.Native.I64, ctx: MLIR.context(module))
  end

  @doc false
  def expand_new_struct(module, mod, fields, state, env) do
    loc = MLIR.Location.from_env(env)

    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      {dependence, state} =
        if module == mod do
          {state.mlir.mod, state}
        else
          fetch_dependence_module(mod, state)
        end

      struct = LLVM.mlir_undef(loc: loc) >>> retrieve_struct_type(dependence)
      insert_struct_fields(struct, fields, dependence, state, env, loc)
    end
  end

  @doc false
  def expand_update_struct(module, mod, struct, fields, state, env) do
    loc = MLIR.Location.from_env(env)

    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      {dependence, state} =
        if module == mod do
          {state.mlir.mod, state}
        else
          fetch_dependence_module(mod, state)
        end

      insert_struct_fields(struct, fields, dependence, state, env, loc)
    end
  end

  defp insert_struct_fields(struct, fields, dependence, state, env, loc) do
    struct_type = retrieve_struct_type(dependence)

    struct =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        for {k, v} <- fields, reduce: struct do
          struct ->
            position = position_of_field!(env, dependence, k)
            struct = LLVM.insertvalue(struct, v, position: position, loc: loc) >>> struct_type
            struct
        end
      end

    {struct, state, env}
  end

  @doc false
  def expand_extract_field(struct, fun, state, env) do
    loc = MLIR.Location.from_env(env)

    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      struct_type = MLIR.Value.type(struct)

      unless MLIR.Type.llvm_struct?(struct_type) do
        raise_compile_error(
          env,
          "Expected a struct type, got: #{MLIR.to_string(struct_type)}"
        )
      end

      defining_module =
        MLIR.CAPI.mlirLLVMStructTypeGetIdentifier(struct_type)
        |> MLIR.to_string()
        |> String.to_atom()

      {dependence, state} =
        if defining_module == env.module do
          {state.mlir.mod, state}
        else
          fetch_dependence_module(defining_module, state)
        end

      position = position_of_field!(env, dependence, fun)
      elem_t = MLIR.CAPI.mlirLLVMStructTypeGetElementType(struct_type, position[0])
      field_value = LLVM.extractvalue(struct, position: position, loc: loc) >>> elem_t
      {field_value, state, env}
    end
  end

  defp fetch_dependence_module(module, state) do
    update_in(
      state.mlir.dependence_modules,
      &Map.put_new_lazy(&1, module, fn ->
        if function_exported?(module, :__ir__, 0) do
          MLIR.Module.create!(module.__ir__(), ctx: state.mlir.ctx)
          |> MLIR.Operation.from_module()
        else
          nil
        end
      end)
    )
    |> then(&{&1.mlir.dependence_modules[module], &1})
  end
end
