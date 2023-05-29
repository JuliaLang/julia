# Syntax Trees

This section describes the syntax trees produced by JuliaSyntax, mainly in
terms of their similarities and differences with the `Expr` tree data
structures used since Julia 0.1.

## JuliaSyntax trees vs `Expr`

The tree structure of `GreenNode`/`SyntaxNode` is similar to Julia's `Expr`
data structure but there are various differences:

### Source ordered children

The children of our trees are strictly in source order. This has many
consequences in places where `Expr` reorders child expressions.

* Infix and postfix operator calls have the operator name in the *second* child position. `a + b` is parsed as `(call-i a + b)` - where the infix `-i` flag indicates infix child position - rather than `Expr(:call, :+, :a, :b)`.
* Generators are represented in source order as a single node rather than multiple nested flatten and generator expressions.

### No `LineNumberNode`s

Our syntax nodes inherently stores source position, so there's no need for the
`LineNumberNode`s used by `Expr`.

### More consistent / less redundant `block`s

Sometimes `Expr` needs redundant block constructs to store `LineNumberNode`s,
but we don't need these. Also in cases which do use blocks we try to use them
consistently.

* No block is used on the right hand side of short form function syntax
* No block is used for the conditional in `elseif`
* No block is used for the body of anonymous functions after the `->`
* `let` argument lists always use a block regardless of number or form of bindings

### Faithful representation of the source text / avoid premature lowering

Some cases of "premature lowering" have been removed, preferring to represent
the source text more closely.

* `K"macrocall"` - allow users to easily distinguish macrocalls with parentheses from those without them (#218)
* Grouping parentheses are represented with a node of kind `K"parens"` (#222)
* The right hand side of `x where {T}` retains the `K"braces"` node around the `T` to distinguish it from `x where T`.
* Ternary syntax is not immediately lowered to an `if` node: `a ? b : c` parses as `(? a b c)` rather than `Expr(:if, :a, :b, :c)` (#85)
* `global const` and `const global` are not normalized by the parser. This is done in `Expr` conversion (#130)
* The AST for `do` is flatter and not lowered to a lambda by the parser: `f(x) do y ; body end` is parsed as `(do (call f x) (tuple y) (block body))` (#98)
* `@.` is not lowered to `@__dot__` inside the parser (#146)
* Docstrings use the `K"doc"` kind, and are not lowered to `Core.@doc` until later (#217)
* Juxtaposition uses the `K"juxtapose"` kind rather than lowering immediately to `*` (#220)
* `return` without a value has zero children, rather than lowering to `return nothing` (#220)

### Containers for string-like constructs

String-like constructs always come within a container node, not as a single
token. These are useful for tooling which works with the tokens of the source
text. Also separating the delimiters from the text they delimit removes a whole
class of tokenization errors and lets the parser deal with them.

* string always use `K"string"` to wrap strings, even when they only contain a single string chunk (#94)
* char literals are wrapped in the `K"char"` kind, containing the character literal string along with their delimiters (#121)
* backticks use the `K"cmdstring"` kind
* `var""` syntax uses `K"var"` as the head (#127)
* The parser splits triple quoted strings into string chunks interspersed with whitespace trivia

### Improvements for AST inconsistencies

* Dotted call syntax like `f.(a,b)` and `a .+ b` has been made consistent with the `K"dotcall"` head (#90)
* Standalone dotted operators are always parsed as `(. op)`. For example `.*(x,y)` is parsed as `(call (. *) x y)` (#240)
* The `K"="` kind is used for keyword syntax rather than `kw`, to avoid various inconsistencies and ambiguities (#103)
* Unadorned postfix adjoint is parsed as `call` rather than as a syntactic operator for consistency with suffixed versions like `x'ᵀ` (#124)

### Improvements to awkward AST forms

* Frakentuples with multiple parameter blocks like `(a=1, b=2; c=3; d=4)` are flattened into the parent tuple instead of using nested `K"parameters"` nodes (#133)
* Using `try catch else finally end` is parsed with `K"catch"` `K"else"` and `K"finally"` children to avoid the awkwardness of the optional child nodes in the `Expr` representation (#234)
* The dotted import path syntax as in `import A.b.c` is parsed with a `K"importpath"` kind rather than `K"."`, because a bare `A.b.c` has a very different nested/quoted expression representation (#244)
* We use flags rather than child nodes to represent the difference between `struct` and `mutable struct`, `module` and `baremodule` (#220)
* Multiple iterations within the header of a `for`, as in `for a=as, b=bs body end` are represented with a `cartesian_iterator` head rather than a `block`, as these lists of iterators are neither semantically nor syntactically a sequence of statements. Unlike other uses of `block` (see also generators).


## More detail on tree differences

### Generators

Flattened generators are uniquely problematic because the Julia AST doesn't
respect a key rule we normally expect: that the children of an AST node are a
*contiguous* range in the source text. For example, the `for`s in
`[xy for x in xs for y in ys]` are parsed in the normal order of a for loop to
mean

```
for x in xs
for y in ys
  push!(xy, collection)
```

so the `xy` prefix is in the *body* of the innermost for loop. Following this,
the standard Julia AST is like so:

```
(flatten
  (generator
    (generator
      xy
      (= y ys))
    (= x xs)))
```

however, note that if this tree were flattened, the order would be
`(xy) (y in ys) (x in xs)` and the `x` and `y` iterations are *opposite* of the
source order.

However, our green tree is strictly source-ordered, so we must deviate from the
Julia AST. We deal with this by grouping cartesian products of iterators
(separated by commas) within `cartesian_iterator` blocks as in `for` loops, and
use the presence of multiple iterator blocks rather than the `flatten` head to
distinguish flattened iterators. The nested flattens and generators of `Expr`
forms are reconstructed later. In this form the tree structure resembles the
source much more closely. For example, `(xy for x in xs for y in ys)` is parsed as

```
(generator
  xy
  (= x xs)
  (= y ys))
```

And the cartesian iteration `(xy for x in xs, y in ys)` is parsed as

```
(generator
  xy
  (cartesian_iterator
    (= x xs)
    (= y ys)))
```

### Whitespace trivia inside strings

For triple quoted strings, the indentation isn't part of the string data so
should also be excluded from the string content within the green tree. That is,
it should be treated as separate whitespace trivia tokens. With this separation
things like formatting should be much easier. The same reasoning goes for
escaping newlines and following whitespace with backslashes in normal strings.

Detecting string trivia during parsing means that string content is split over
several tokens. Here we wrap these in the K"string" kind (as is already used
for interpolations). The individual chunks can then be reassembled during Expr
construction. (A possible alternative might be to reuse the K"String" and
K"CmdString" kinds for groups of string chunks (without interpolation).)

Take as an example the following Julia fragment.

```julia
x = """
    $a
    b"""
```

Here this is parsed as `(= x (string-s a "\n" "b"))` (the `-s` flag in
`string-s` means "triple quoted string")

Looking at the green tree, we see the indentation before the `$a` and `b` are
marked as trivia:

```
julia> text = "x = \"\"\"\n    \$a\n    b\"\"\""
       show(stdout, MIME"text/plain"(), parseall(GreenNode, text, rule=:statement), text)
     1:23     │[=]
     1:1      │  Identifier             ✔   "x"
     2:2      │  Whitespace                 " "
     3:3      │  =                          "="
     4:4      │  Whitespace                 " "
     5:23     │  [string]
     5:7      │    """                      "\"\"\""
     8:8      │    String                   "\n"
     9:12     │    Whitespace               "    "
    13:13     │    $                        "\$"
    14:14     │    Identifier           ✔   "a"
    15:15     │    String               ✔   "\n"
    16:19     │    Whitespace               "    "
    20:20     │    String               ✔   "b"
    21:23     │    """                      "\"\"\""
```

### String nodes always wrapped in `K"string"` or `K"cmdstring"`

All strings are surrounded by a node of kind `K"string"`, even non-interpolated
literals, so `"x"` parses as `(string "x")`. This makes string handling simpler
and more systematic because interpolations and triple strings with embedded
trivia don't need to be treated differently. It also gives a container in which
to attach the delimiting quotes.

The same goes for command strings which are always wrapped in `K"cmdstring"`
regardless of whether they have multiple pieces (due to triple-quoted
dedenting) or otherwise.

### No desugaring of the closure in do blocks

The reference parser represents `do` syntax with a closure for the second
argument. That is,

```julia
f(x) do y
    body
end
```

becomes `(do (call f x) (-> (tuple y) (block body)))` in the reference parser.

However, the nested closure with `->` head is implied here rather than present
in the surface syntax, which suggests this is a premature desugaring step.
Instead we emit the flatter structure `(do (call f x) (tuple y) (block body))`.


## Tree structure reference

This section may eventually contain a full description of the Julia AST. For
now, we describe a few of the more subtle features.

### Concatenation syntax

Concatenation syntax comes in two syntax forms:
* The traditional `hcat`/`vcat`/`row` which deal with concatenation or matrix
  construction along dimensions one and two.
* The new `ncat`/`nrow` syntax which deals with concatenation or array
  construction along arbitrary dimensions.

We write `ncat-3` for concatenation along the third dimension. (The `3` is
stored in the head flags for `SyntaxNode` trees, and in the first `arg` for
`Expr` trees.) Semantically the new syntax can work like the old:
* `ncat-1` is the same as `vcat`
* `ncat-2` is the same as `hcat`
* `row` is the same as `nrow-2`

#### Vertical concatenation (dimension 1)

Vertical concatenation along dimension 1 can be done with semicolons or newlines

```julia
julia> print_tree(:([a
                     b]))
Expr(:vcat)
├─ :a
└─ :b

julia> print_tree(:([a ; b]))
Expr(:vcat)
├─ :a
└─ :b
```

#### Horizontal concatenation (dimension 2)

For horizontal concatenation along dimension 2, use spaces or double semicolons

```julia
julia> print_tree(:([a b]))
Expr(:hcat)
├─ :a
└─ :b

julia> print_tree(:([a ;; b]))
Expr(:ncat)
├─ 2
├─ :a
└─ :b
```

#### Mixed concatenation

Concatenation along dimensions 1 and 2 can be done with spaces and single
semicolons or newlines, producing a mixture of `vcat` and `row` expressions:

```julia
julia> print_tree(:([a b
                     c d]))
# OR
julia> print_tree(:([a b ; c d]))
Expr(:vcat)
├─ Expr(:row)
│  ├─ :a
│  └─ :b
└─ Expr(:row)
   ├─ :c
   └─ :d
```

General n-dimensional concatenation results in nested `ncat` and `nrow`, for
example

```julia
julia> print_tree(:([a ; b ;; c ; d ;;; x]))
Expr(:ncat)
├─ 3
├─ Expr(:nrow)
│  ├─ 2
│  ├─ Expr(:nrow)
│  │  ├─ 1
│  │  ├─ :a
│  │  └─ :b
│  └─ Expr(:nrow)
│     ├─ 1
│     ├─ :c
│     └─ :d
└─ :x
```

