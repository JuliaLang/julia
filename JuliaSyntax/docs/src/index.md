# JuliaSyntax.jl

A Julia compiler frontend, written in Julia.

A [talk from JuliaCon 2022](https://youtu.be/CIiGng9Brrk) covered some aspects
of this package.

## Examples

Here's what parsing of a small piece of code currently looks like in various
forms. We'll use the `JuliaSyntax.parsestmt` function to demonstrate, there's also
`JuliaSyntax.parse!` offering more fine-grained control.

First, a source-ordered AST with `SyntaxNode` (`call-i` in the dump here means
the `call` has the infix `-i` flag):

```julia
julia> using JuliaSyntax

julia> parsestmt(SyntaxNode, "(x + y)*z", filename="foo.jl")
line:col│ tree                                   │ file_name
   1:1  │[call-i]                                │foo.jl
   1:1  │  [parens]
   1:2  │    [call-i]
   1:2  │      x
   1:4  │      +
   1:6  │      y
   1:8  │  *
   1:9  │  z
```

Internally this has a full representation of all syntax trivia (whitespace and
comments) as can be seen with the more raw ["green tree"](#raw-syntax-tree--green-tree)
representation with `GreenNode`. Here ranges on the left are byte ranges, and
`✔` flags nontrivia tokens. Note that the parentheses are trivia in the tree
representation, despite being important for parsing.

```julia
julia> text = "(x + y)*z"
       greentree = parsestmt(JuliaSyntax.GreenNode, text)
     1:9      │[call]
     1:7      │  [parens]
     1:1      │    (
     2:6      │    [call]
     2:2      │      Identifier         ✔
     3:3      │      Whitespace
     4:4      │      +                  ✔
     5:5      │      Whitespace
     6:6      │      Identifier         ✔
     7:7      │    )
     8:8      │  *                      ✔
     9:9      │  Identifier             ✔
```

`GreenNode` stores only byte ranges, but the token strings can be shown by
supplying the source text string:

```julia
julia> show(stdout, MIME"text/plain"(), greentree, text)
     1:9      │[call]
     1:7      │  [parens]
     1:1      │    (                        "("
     2:6      │    [call]
     2:2      │      Identifier         ✔   "x"
     3:3      │      Whitespace             " "
     4:4      │      +                  ✔   "+"
     5:5      │      Whitespace             " "
     6:6      │      Identifier         ✔   "y"
     7:7      │    )                        ")"
     8:8      │  *                      ✔   "*"
     9:9      │  Identifier             ✔   "z"
```

Julia `Expr` can also be produced:

```julia
julia> JuliaSyntax.parsestmt(Expr, "(x + y)*z")
:((x + y) * z)
```

