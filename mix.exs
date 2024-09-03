defmodule Charms.MixProject do
  use Mix.Project

  def project do
    [
      app: :charms,
      version: "0.1.1-dev",
      elixir: "~> 1.17-dev",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      package: package()
    ]
  end

  def package do
    [
      description: "Elixir compiler for native targets",
      licenses: ["Apache-2.0", "MIT"],
      links: %{"GitHub" => "https://github.com/beaver-lodge/charms"}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "bench"]
  defp elixirc_paths(:dev), do: ["lib", "bench"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:beaver, "~> 0.3.10"},
      {:benchee, "~> 1.0", only: :dev}
    ]
  end
end
