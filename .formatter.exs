# Used by "mix format"
locals_without_parens = [
  op: 1,
  value: 1,
  const: 1,
  defm: 2,
  defk: 2,
  defbind: 1,
  set!: 2,
  tmp!: 1,
  tmp!: 2,
  new!: 1,
  new!: 2,
  launch!: 3,
  defer: 1,
  free!: 1,
  defmstruct: 1
]

[
  locals_without_parens: locals_without_parens,
  inputs: ["{mix,.formatter}.exs", "{config,lib,test,scripts,bench}/**/*.{ex,exs}"],
  export: [
    locals_without_parens: locals_without_parens
  ]
]
