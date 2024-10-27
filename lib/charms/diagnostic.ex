defmodule Charms.Diagnostic do
  @moduledoc false
  @doc false
  def compile_error_message(diagnostic_server) when is_pid(diagnostic_server) do
    case txt = Beaver.Diagnostic.Server.flush(diagnostic_server) do
      "[Beaver] [Diagnostic] [" <> _suffix ->
        c =
          Regex.named_captures(
            ~r/\[Beaver\] \[Diagnostic\] \[(?<file>.+):(?<line>\d+):(?<column>\d+)\] (?<note>.*)/,
            txt
          )

        {:ok, [file: c["file"], line: c["line"] || 0, description: c["note"] || txt]}

      "" ->
        {:error, "No diagnostic message"}
    end
  end

  defmacro raise_compile_error(env, description) do
    quote do
      raise CompileError,
            Charms.Diagnostic.compile_error_message(unquote(env), unquote(description))
    end
  end

  def compile_error_message(%Macro.Env{file: file, line: line}, description) do
    [file: file, line: line, description: description]
  end
end
