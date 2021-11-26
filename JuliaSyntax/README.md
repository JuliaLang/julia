# JuliaSyntax

[![Build Status](https://github.com/c42f/JuliaSyntax.jl/workflows/CI/badge.svg)](https://github.com/c42f/JuliaSyntax.jl/actions)

Yet another Julia frontend, written in Julia.

Goals:
* Parse Julia code with precise source mapping (concrete syntax trees)
* Avoid worrying about how much work this will be 😅

Nice to have:
* Speedy enough for interactive editing
* Production quality error recovery and reporting
* "Compilation as an API" to support all sorts of tooling
* Go further than parsing - macro expansion, syntax desugaring and scope analysis
* Code which is correct, fast and understandable

## Design

The datastructure design here is hard:
- There's many useful ways to augment a syntax tree depending on use case.
- Analysis algorithms should be able to act on any tree type, ignoring
  but carrying augmentations which they don't know about.

Let's tackle it by prototyping identifying several important work flows:

1. Precise error reporting in lowering
  - Syntax desugaring `[a, b] = (c, d)` should report "invalid assignment
    location `[a, b]`". But at a precise source location.
  - Try something several layers deeper inside lowering. For example "macro
    definition not allowed inside a local scope"

2. Refactoring
  - A pass to rename local variables

3. Incremental reparsing
  - Reparse a source file, given a byte range replacement

4. Formatting
  - Re-indent a file

