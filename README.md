[![Run in Livebook](https://livebook.dev/badge/v1/pink.svg)](https://livebook.dev/run?url=https%3A%2F%2Fhexdocs.pm%2Fcharms%2F0.1.1%2Fprogramming-with-charms.livemd)

# Charms
[![Package](https://img.shields.io/badge/-Package-important)](https://hex.pm/packages/charms) [![Documentation](https://img.shields.io/badge/-Documentation-blueviolet)](https://hexdocs.pm/charms)

Charms is an Elixir compiler that compiles a subset of Elixir to optimized native code using MLIR. It enables writing performance-critical Elixir code that gets compiled to efficient native binaries while maintaining Elixir's syntax and development workflow.

# Features
- [x] `defm` to define native functions, some examples
  - [quick sort](/bench/enif_quick_sort.ex)
  - [vector add](/bench/vec_add_int_list.ex)
- [x] Multi-threaded compilation, built upon Elixir processes and MLIR's multi-threaded capabilities
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
