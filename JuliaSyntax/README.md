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

The tree datastructure design here is hard:

1. The symbolic part of compilation (the compiler frontend) incrementally
   abstracts the source text, but errors along the way should refer back to the
   source.
  - The tree must be a lossless representation of the source text
  - Some aspects of the source text (comments, most whitespace) are irrelevant
    to parsing.
  - More aspects of the source text are irrelevant after we have an abstract
    syntax tree of the surface syntax. Some good examples here are the
    parentheses in `2*(x + y)` and the explicit vs implicit multiplication
    symbol in `2*x` vs `2x`.

2. There's various type of *analyses* 
- There's many useful ways to augment, a syntax tree depending on use case.
- Analysis algorithms should be able to act on any tree type, ignoring
  but carrying augmentations which they don't know about.

Let's tackle it by prototyping several important work flows:

* Syntax transformations
  - Choose some macros to implement. This is a basic test of mixing source
    trees from different files while preserving precise source locations.
* Formatting
  - Re-indent a file. This tests the handling of syntax trivia.
* Refactoring
  - A pass to rename local variables. This tests how information from further
    down the compilation pipeline can be attached to the syntax tree and used
    to modify the source code.
* Precise error reporting in lowering
  - Syntax desugaring `[a, b] = (c, d)` should report "invalid assignment
    location `[a, b]`". But at a precise source location.
  - Try something several layers deeper inside lowering? For example "macro
    definition not allowed inside a local scope"
* Incremental reparsing
  - Reparse a source file, given a byte range replacement


## Tree design

Raw syntax tree (RST / "Green tree")

We want RawSyntaxNode to be
* *structurally minimal* — For efficiency and generality
* *immutable*            — For efficiency (& thread safety?)
* *complete*             — To preserve parser knowledge

```
for i = 1:10
    a + 2
    # hi
    c
    #= hey
       ho =#
end
```

The simplest idea possible is to have:
* Leaf nodes are a single token
* Children are in source order

```
- - trivia
I - identifier
L - literal

[for]
    - "for"
    - " "
    [=]
        I "i"
        - " "
        - "="
        - " "
        [call]
            I "1"
            - ":"
            L "10"
    - "\n    "
    [call]
        I "a"
        - " "
        I "+"
        - " "
        L "2"
    - "\n    "
    - "# hi"
    - "\n    "
    I "c"
    - "\n    "
    - #= hey\n       ho =#'
    - "\n"
    - "end"
```

Call represents a challange for the AST vs RST in terms of node placement /
iteration for infix operators vs normal prefix function calls.

- The normal problem of `a + 1` vs `+(a, 1)`
- Or even worse, `a + 1 + 2` vs `+(a, 1, 2)`

Clearly in the AST's *interface* we need to abstract over this placement. For
example with something like the normal Julia AST. But in the RST we only need
to distinguish between infix and prefix.



## Fun research questions

* Given the raw tree (the green tree, in Roslyn terminology) can we regress a
  model of indentiation? Such that formatting rules for new code is defined
  implicitly by a software project's existing style?

# Resources

* [Persistence, façades and Roslyn’s red-green trees](https://ericlippert.com/2012/06/08/red-green-trees/)
  - [Roslyn optimization overview](https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees)
  - [Literate C# Usage Example](https://johtela.github.io/LiterateCS/LiterateCS/BlockBuilder.html)
* Andy Chu (the author of the OIL shell) has written some things about this
  - Collected links about lossless syntax in [a wiki page](https://github.com/oilshell/oil/wiki/Lossless-Syntax-Tree-Pattern)
  - A blog post [From AST to Lossless Syntax Tree](https://www.oilshell.org/blog/2017/02/11.html)
