# Used by "mix format"
locals_without_parens = [
  op: 1,
  value: 1,
  call: 1,
  const: 1,
  defm: 2,
  defk: 2,
  defbind: 1,
  set!: 2,
  ptr!: 1,
  ptr!: 2,
  defmstruct: 1,
  launch!: 3
]

[
  locals_without_parens: locals_without_parens,
  inputs: ["{mix,.formatter}.exs", "{config,lib,test,scripts,bench}/**/*.{ex,exs}"],
  export: [
    locals_without_parens: locals_without_parens
  ]
]
