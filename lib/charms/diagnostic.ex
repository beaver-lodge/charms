defmodule Charms.Diagnostic do
  @moduledoc false
  @doc false
  alias Beaver.MLIR
  def compile_error_message(%Beaver.MLIR.Diagnostic{} = d) do
    loc = to_string(MLIR.location(d))
    txt = to_string(d)
    case txt do
      "" ->
        {:error, "No diagnostic message"}

      note ->
        c =
          Regex.named_captures(
            ~r/(?<file>.+):(?<line>\d+):(?<column>\d+)/,
            loc
          )

        {:ok, [file: c["file"], line: c["line"] || 0, description: note]}
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
