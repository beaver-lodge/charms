defmodule Charms.Diagnostic do
  @moduledoc false
  @doc false
  alias Beaver.MLIR

  def meta_from_loc(%MLIR.Location{} = loc) do
    c = Regex.named_captures(~r/(?<file>.+):(?<line>\d+):(?<column>\d+)/, MLIR.to_string(loc))
    [file: c["file"], line: c["line"] || 0]
  end

  def compile_error_message(%Beaver.MLIR.Diagnostic{} = d) do
    txt = to_string(d)

    case txt do
      "" ->
        {:error, "No diagnostic message"}

      note ->
        {:ok, meta_from_loc(MLIR.location(d)) ++ [description: note]}
    end
  end

  defmacro raise_compile_error(env, diagnostic) do
    quote do
      raise CompileError,
            Charms.Diagnostic.compile_error_message_from_env(unquote(env), unquote(diagnostic))
    end
  end

  def compile_error_message_from_env(%Macro.Env{file: file, line: line}, description) do
    [file: file, line: line, description: description]
  end
end
