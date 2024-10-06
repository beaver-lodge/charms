# Charms
[![Package](https://img.shields.io/badge/-Package-important)](https://hex.pm/packages/charms) [![Documentation](https://img.shields.io/badge/-Documentation-blueviolet)](https://hexdocs.pm/charms)

Elixir compiler to compile a subset of Elixir to native targets

- [x] `defm` to define native functions, some examples
  - [quick sort](/bench/enif_quick_sort.ex)
  - [vector add](/bench/vec_add_int_list.ex)

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

To run the benchmarks:
```sh
mix run bench/sort_benchmark.exs
mix run bench/list_add_benchmark.exs
```
