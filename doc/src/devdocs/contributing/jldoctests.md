# [Writing jldoctests](@id writing-jldoctests)

This page describes how to write and maintain `jldoctest` blocks in the documentation. Following these guidelines helps keep doctests reliable and easy to read.

## Filters

Use `filter =` whenever output contains text that might vary across runs.
The documentation relies on several recurring patterns:

- `r"int.jl:\\d+"` — remove line numbers from introspection macros.
- `r"Stacktrace:(\\n \\[0-9]+\\].*)*"` — hide stack traces when illustrating
  errors.
- `r"Closest candidates.*\\n  .*"` — skip the method suggestions printed by
  `MethodError`.
- `r"@ .*"` — strip file locations from the output of `methods` or
  `@which`.
- `r"\\@world\\(MyStruct, \\d+:\\d+\\)"` — filter world age numbers.
- `r"with \\d+ methods"` — ignore method counts when redefining functions.
- `r"[0-9\\.]+ seconds \\(.*?\\)"` — remove timing output with memory
  information.
- `r"[0-9\\.]+ seconds"` — remove simple timing results.
- `r"[0-9\\.]+"` — filter digits from names such as anonymous functions.
- `r"([A-B] [0-5])"` and `r"[A-B] [X-Z] [0-5]"` — account for non-deterministic
  process output.
- `r"(world\\nhello|hello\\nworld)"` — allow either ordering of interleaved
  output.

If none of these match your situation, craft a regular expression that
removes the varying text. Using filters keeps doctests stable across
platforms and Julia versions.

## Setup code

Small setup expressions may be placed inline using the `setup =` option:

````
```jldoctest; setup = :(using InteractiveUtils)
...
```
````

For longer setup code or if multiple blocks require the same environment, use the
`DocTestSetup` meta block:

````
```@meta
DocTestSetup = :(import Random; Random.seed!(1234))
```
````

and disable it afterwards with

````
```@meta
DocTestSetup = nothing
```
````

## Maintaining state between snippets

Related doctest blocks can share state by giving them the same label after the
`jldoctest` keyword. The manual uses this pattern to demonstrate mutation:

````
```jldoctest mutation_vs_rebind
julia> a = [1,2,3]
...
```
````

and later

````
```jldoctest mutation_vs_rebind
julia> a[1] = 42
...
```
````

Blocks with the same name execute sequentially during doctesting, so variables
created in the first block remain available in the following ones.

When a snippet needs to preserve its result for later examples, give it a label
and reuse that label. This avoids repeating setup code and mirrors a REPL
session more closely.

## Further reading
For a complete reference of doctest syntax, see the [corresponding Documenter.jl docs](https://documenter.juliadocs.org/stable/man/doctests/).
