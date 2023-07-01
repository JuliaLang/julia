# JuliaSyntax

[![Build Status](https://github.com/c42f/JuliaSyntax.jl/workflows/CI/badge.svg)](https://github.com/c42f/JuliaSyntax.jl/actions)
[![codecov.io](http://codecov.io/github/JuliaLang/JuliaSyntax.jl/coverage.svg?branch=main)](http://codecov.io/github/JuliaLang/JuliaSyntax.jl?branch=main)

A Julia compiler frontend, written in Julia.

Read the [documentation](https://JuliaLang.github.io/JuliaSyntax.jl/dev) for
more information.

### Status

JuliaSyntax.jl is used as the new default Julia parser in Julia 1.10.
It's highly compatible with Julia's older
[femtolisp-based parser](https://github.com/JuliaLang/julia/blob/master/src/julia-parser.scm) -
It parses all of Base, the standard libraries and General registry. Some minor
difference remain where we've decided to fix bugs or strange behaviors in the
reference parser.

The AST and tree data structures are usable but their APIs will evolve as we
try out various use cases. Parsing to the standard `Expr` AST is always
possible and will be stable.

The intention is to extend this library over time to cover more of the Julia
compiler frontend.

# Getting involved

For people who want to help improve Julia's error messages by contributing to
JuliaSyntax, I'd suggest looking through the issue list at
https://github.com/JuliaLang/JuliaSyntax.jl/issues and choosing a small issue
or two to work on to familiarize yourself with the code. Anything marked with
the labels `intro issue` or `bug` might be a good place to start.

Also watching the [2022 JuliaCon talk](https://www.youtube.com/watch?v=CIiGng9Brrk)
and reading the [design](https://julialang.github.io/JuliaSyntax.jl/dev/design/) and
[reference](https://julialang.github.io/JuliaSyntax.jl/dev/reference/)
documentation should be good for an overview.

As of May 2023, we've got really good positional tracking within the source,
but JuliaSyntax really needs a better system for parser recovery before the
errors are really nice. This requires some research. For example, you could
read up on how rust-analyzer does recovery, or rslint - both these are
event-based recursive decent parsers with similar structure to JuliaSyntax
(though in Rust). I also want to investigate whether we can do data-driven
parser recovery using an ML technique. But again, this is a research project.
