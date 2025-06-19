defmodule Charms.Struct do
  @moduledoc false
  alias Beaver.MLIR
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
end
