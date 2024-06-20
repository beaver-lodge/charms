# Charms
[![Package](https://img.shields.io/badge/-Package-important)](https://hex.pm/packages/charms) [![Documentation](https://img.shields.io/badge/-Documentation-blueviolet)](https://hexdocs.pm/charms)

Elixir compiler to compile a subset of Elixir to native targets

- [x] `defm` to define native functions, and here are the [examples](/bench/)

- [ ] SIMD support

- [ ] SIMT support

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `charms` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:charms, "~> 0.1.0"}
  ]
end
```

## Development

- run the benchmarks of sorting algorithms
  ```sh
  mix run bench/sort.exs
  ```
