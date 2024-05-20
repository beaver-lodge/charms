defmodule Charm.MixProject do
  use Mix.Project

  def project do
    [
      app: :charm,
      version: "0.1.0",
      elixir: "~> 1.17-dev",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package()
    ]
  end

  def package do
    [
      description: "Elixir compiler for native targets",
      licenses: ["Apache-2.0", "MIT"],
      links: %{"GitHub" => "https://github.com/beaver-lodge/charm"}
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end
end
