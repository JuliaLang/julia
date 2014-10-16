# Compat Package for Julia

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)

The **Compat** package is designed to ease interoperability between
older and newer versions of the [Julia
language](http://julialang.org/).  In particular, in cases where it is
impossible to write code that works with both the latest Julia
`master` branch and older Julia versions, or impossible to write code
that doesn't generate a deprecation warning in some Julia version, the
Compat package provides a macro that lets you use the *latest syntax*
in a backwards-compatible way.

This is primarily intended for use by other [Julia
packages](http://docs.julialang.org/en/latest/manual/packages/), where
it is important to maintain cross-version compatibility.

## Usage

To use Compat in your Julia package, add a line `Compat` to the
`REQUIRE` file in your package directory.  Then, in your package,
after a `using Compat` statement to load Compat, simply use:

```
@compat ...Julia master syntax...
```

wherever you want to use syntax that differs in the latest Julia
`master` (the development version of Julia).

## Supported syntax

Currently, the `@compat` macro supports the following syntaxes:

* `@compat Dict(foo => bar, baz => qux)` - type-inferred `Dict` construction.

* `@compat Dict{Foo,Bar}(foo => bar, baz => qux)` - type-declared `Dict` construction.

* `@compat split(str, splitter; keywords...)` - the Julia 0.4-style keyword-based `split` function
