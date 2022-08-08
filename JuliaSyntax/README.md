# JuliaSyntax

[![Build Status](https://github.com/c42f/JuliaSyntax.jl/workflows/CI/badge.svg)](https://github.com/c42f/JuliaSyntax.jl/actions)

A Julia frontend, written in Julia.

## Goals

* Lossless parsing of Julia code with precise source mapping
* Production quality error recovery, reporting and unit testing
* Parser structure as similar as possible to Julia's flisp-based parser
* Speedy enough for interactive editing
* "Compilation as an API" to support all sorts of tooling
* Grow to encompass the rest of the compiler frontend: macro expansion,
  desugaring and other lowering steps.
* Once mature, replace Julia's flisp-based reference frontend in `Core`

### Design Opinions

* Parser implementation should be independent from tree data structures. So
  we have the `ParseStream` interface.
* Tree data structures should be *layered* to balance losslessness with
  abstraction and generality. So we have `SyntaxNode` (an AST) layered on top
  of `GreenNode` (a lossless parse tree). We might need other tree types later.
* Fancy parser generators still seem marginal for production compilers. We use
  a boring but flexible recursive descent parser.

### Status

The library is in pre-0.1 stage, but parses all of Base correctly with only a
handful of failures remaining in the Base tests and standard library.
The tree data structures should be somewhat usable but will evolve as we try
out various use cases.

A talk from JuliaCon 2022 covered some aspects of this package.

[![Youtube video thumbnail](https://img.youtube.com/vi/CIiGng9Brrk/mqdefault.jpg)](https://youtu.be/CIiGng9Brrk)

# Examples

Here's what parsing of a small piece of code currently looks like in various
forms.  We'll use the `parseall` convenience function to demonstrate, but
there's also a more flexible parsing interface with `JuliaSyntax.parse()`.

First, a source-ordered AST with `SyntaxNode` (`call-i` in the dump here means
the `call` has the infix `-i` flag):

```julia
julia> parseall(SyntaxNode, "(x + y)*z", filename="foo.jl")
line:col│ byte_range  │ tree                                   │ file_name
   1:1  │     1:9     │[toplevel]                              │foo.jl
   1:1  │     1:9     │  [call-i]
   1:2  │     2:6     │    [call-i]
   1:2  │     2:2     │      x
   1:4  │     4:4     │      +
   1:6  │     6:6     │      y
   1:8  │     8:8     │    *
   1:9  │     9:9     │    z
```

Internally this has a full representation of all syntax trivia (whitespace and
comments) as can be seen with the more raw "green tree" representation with
`GreenNode`. Here ranges on the left are byte ranges, and `✔` flags nontrivia
tokens. Note that the parentheses are trivia in the tree representation,
despite being important for parsing.

```julia
julia> text = "(x + y)*z"
       greentree = parseall(GreenNode, text)
     1:9      │[toplevel]
     1:9      │  [call]
     1:1      │    (
     2:6      │    [call]
     2:2      │      Identifier         ✔
     3:3      │      Whitespace
     4:4      │      +                  ✔
     5:5      │      Whitespace
     6:6      │      Identifier         ✔
     7:7      │    )
     8:8      │    *                    ✔
     9:9      │    Identifier           ✔
```

`GreenNode` stores only byte ranges, but the token strings can be shown by
supplying the source text string:

```julia
julia> show(stdout, MIME"text/plain"(), greentree, text)
     1:9      │[toplevel]
     1:9      │  [call]
     1:1      │    (                        "("
     2:6      │    [call]
     2:2      │      Identifier         ✔   "x"
     3:3      │      Whitespace             " "
     4:4      │      +                  ✔   "+"
     5:5      │      Whitespace             " "
     6:6      │      Identifier         ✔   "y"
     7:7      │    )                        ")"
     8:8      │    *                    ✔   "*"
     9:9      │    Identifier           ✔   "z"
```

Julia `Expr` can also be produced:

```julia
julia> parseall(Expr, "(x + y)*z")
:($(Expr(:toplevel, :((x + y) * z))))
```

# Using JuliaSyntax as the default parser

To use JuliaSyntax as the default Julia parser to `include()` files,
parse code with `Meta.parse()`, etc, call

```
julia> JuliaSyntax.enable_in_core!()
```

This causes some startup latency, so to reduce that you can create a custom
system image by running the code in `./sysimage/compile.jl` as a Julia script
(or directly using the shell, on unix). Then use `julia -J $resulting_sysimage`.

Using a custom sysimage has the advantage that package precompilation will also
go through the JuliaSyntax parser.

# Parser implementation

Our goal is to losslessly represent the source text with a tree; this may be
called a "lossless syntax tree". (This is sometimes called a "concrete syntax
tree", but that term has also been used for the parse tree of the full formal
grammar for a language including any grammar hacks required to solve
ambiguities, etc. So we avoid this term.)

`JuliaSyntax` uses a mostly recursive descent parser which closely
follows the high level structure of the flisp reference parser. This makes the
code familiar and reduces porting bugs. It also gives a lot of flexibility for
designing the diagnostics, tree data structures, compatibility with different
Julia versions, etc. I didn't choose a parser generator as they still seem
marginal for production compilers — for the parsing itself they don't seem
*greatly* more expressive and they can be less flexible for the important
"auxiliary" code which needs to be written in either case.

### Lexing

We use a version of [Tokenize.jl](https://github.com/JuliaLang/Tokenize.jl)
which has been modified to better match the needs of parsing:
* Newline-containing whitespace is emitted as a separate kind
* Tokens inside string interpolations are emitted separately from the string
* Strings delimiters are separate tokens and the actual string always has the
  `String` kind
* Additional contextual keywords (`as`, `var`, `doc`) have been added and
  moved to a subcategory of keywords.
* Nonterminal kinds were added (though these should probably be factored out again)
* Various bugs fixed and additions for newer Julia versions

This copy of Tokenize lives in the `JuliaSyntax` source tree due to the volume
of changes required but once the churn settles down it would be good to figure
out how to un-fork the lexer in some way or other.

### Parsing with ParseStream

The main parser innovation is the `ParseStream` interface which provides a
stream-like I/O interface for writing the parser. The parser does not
depend on or produce any concrete tree data structure as part of the parsing
phase but the output spans can be post-processed into various tree data
structures as required. This is like the design of rust-analyzer though with a
simpler implementation.

Parsing proceeds by recursive descent;

* The parser consumes a flat list of lexed tokens as *input* using `peek()` to
  examine tokens and `bump()` to consume them.
* The parser produces a flat list of text spans as *output* using `bump()` to
  transfer tokens to the output and `position()`/`emit()` for nonterminal ranges.
* Diagnostics are emitted as separate text spans
* Whitespace and comments are automatically `bump()`ed and don't need to be
  handled explicitly. The exception is syntactically relevant newlines in space
  sensitive mode.
* Parser modes are passed down the call tree using `ParseState`.

The output spans track the byte range, a syntax "kind" stored as an integer
tag, and some flags. The kind tag makes the spans a [sum
type](https://blog.waleedkhan.name/union-vs-sum-types/) but where the type is
tracked explicitly outside of Julia's type system.

For lossless parsing the output spans must cover the entire input text. Using
`bump()`, `position()` and `emit()` in a natural way also ensures that:
* Spans are cleanly nested with children contained entirely within their parents
* Siblings spans are emitted in source order
* Parent spans are emitted after all their children.

These properties make the output spans naturally isomorphic to a
["green tree"](https://ericlippert.com/2012/06/08/red-green-trees/)
in the terminology of C#'s Roslyn compiler.

### Tree construction

The `build_tree` function performs a depth-first traversal of the `ParseStream`
output spans allowing it to be assembled into a concrete tree data structure,
for example using the `GreenNode` data type. We further build on top of this to
define `build_tree` for the AST type `SyntaxNode` and for normal Julia `Expr`.

### Error recovery

The goal of the parser is to produce well-formed hierarchical structure from
the source text. For interactive tools we need this to work even when the
source text contains errors; it's the job of the parser to include the recovery
heuristics to make this work.

Concretely, the parser in `JuliaSyntax` should always produce a green tree
which is *well formed* in the sense that `GreenNode`s of a given `Kind` have
well-defined layout of children. This means the `GreenNode` to `SyntaxNode`
transformation is deterministic and tools can assume they're working with a
"mostly valid" AST.

What does "mostly valid" mean? We allow the tree to contain the following types
of error nodes:

* Missing tokens or nodes may be **added** as placeholders when they're needed
  to complete a piece of syntax. For example, we could parse `a + (b *` as
  `(call-i a + (call-i * b XXX))` where `XXX` is a placeholder error node.
* A sequence of unexpected tokens may be **removed** by collecting
  them as children of an error node and treating them as syntax trivia during
  AST construction. For example, `a + b end * c` could be parsed as the green
  tree `(call-i a + b (error-t end * c))`, and turned into the AST `(call + a b)`.

We want to encode both these cases in a way which is simplest for downstream
tools to use. This is an open question, but for now we use `K"error"` as the
kind, with the `TRIVIA_FLAG` set for unexpected syntax.

# Syntax trees

Julia's `Expr` abstract syntax tree can't store precise source locations or
deal with syntax trivia like whitespace or comments. So we need some new tree
types in `JuliaSyntax`.

JuliaSyntax currently deals in three types of trees:
* `GreenNode` is a minimal *lossless syntax tree* where
  - Nodes store a kind and length in bytes, but no text
  - Syntax trivia are included in the list of children
  - Children are strictly in source order
* `SyntaxNode` is an *abstract syntax tree* which has
  - An absolute position and pointer to the source text
  - Children strictly in source order
  - Leaf nodes store values, not text
  - Trivia are ignored, but there is a 1:1 mapping of non-trivia nodes to the
    associated `GreenTree` nodes.
* `Expr` is used as a conversion target for compatibility

## Julia AST structures

In this section we describe some features of Julia's AST structures.

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

## Tree differences between GreenNode and Expr

Wherever possible, the tree structure of `GreenNode`/`SyntaxNode` is 1:1 with
`Expr`. There are, however, some exceptions. First, `GreenNode` inherently
stores source position, so there's no need for the `LineNumberNode`s used by
`Expr`. There's also a small number of other differences

### Flattened generators

Flattened generators are uniquely problematic because the Julia AST doesn't
respect a key rule we normally expect: that the children of an AST node are a
*contiguous* range in the source text. This is because the `for`s in
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
Julia AST. The natural representation seems to be to remove the generators and
use a flattened structure:

```
(flatten
  xy
  (= x xs)
  (= y ys))
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

## More about syntax kinds

We generally track the type of syntax nodes with a syntax "kind", stored
explicitly in each node an integer tag. This effectively makes the node type a
[sum type](https://blog.waleedkhan.name/union-vs-sum-types/) in the type system
sense, but with the type tracked explicitly outside of Julia's type system.

Managing the type explicitly brings a few benefits:
* Code and data structures for manipulating syntax nodes is always concretely
  typed from the point of view of the compiler.
* We control the data layout and can pack the kind into very few bits along
  with other flags bits, as desired.
* Predicates such as `is_operator` can be extremely efficient, given that we
  know the meaning of the kind's bits.
* The kind can be applied to several different tree data structures, or
  manipulated by itself.
* Pattern matching code is efficient when the full set of kinds is closed and
  known during compilation.

There's arguably a few downsides:
* Normal Julia dispatch can't express dispatch over syntax kind. Luckily,
  a pattern matching macro can provide a very elegant way of expressing such
  algorithms over a non-extensible set of kinds, so this is not a big problem.
* Different node kinds could come with different data fields, but a syntax
  tree must have generic fields to cater for all kinds. (Consider as an analogy
  the normal Julia AST `QuoteNode` with a single field vs `Expr` with generic
  `head` and `args` fields.) This could be a disadvantage for code which
  processes one specific kind but for generic code processing many kinds
  having a generic but *concrete* data layout should be faster.


# Differences from the flisp parser

Practically the flisp parser is not quite a classic [recursive descent
parser](https://en.wikipedia.org/wiki/Recursive_descent_parser), because it
often looks back and modifies the output tree it has already produced. We've
tried to eliminate this pattern it favor of lookahead where possible because

* It works poorly when the parser is emitting a stream of node spans with
  strict source ordering constraints.
* It's confusing to reason about this kind of code

However, on occasion it seems to solve genuine ambiguities where Julia code
can't be parsed top-down with finite lookahead. Eg for the `kw` vs `=`
ambiguity within parentheses. In these cases we put up with using the
functions `look_behind` and `reset_node!()`.

## Code structure

Large structural changes were generally avoided while porting. In particular,
nearly all function names for parsing productions are the same with `-`
replaced by `_` and predicates prefixed by `is_`.

Some notable differences:

* `parse-arglist` and a parts of `parse-paren-` have been combined into a
  general function `parse_brackets`. This function deals with all the odd
  corner cases of how the AST is emitted when mixing `,` and `;` within
  parentheses. In particular regard to:
  - Determining whether `;` are block syntax separators or keyword parameters
  - Determining whether to emit `parameter` sections based on context
  - Emitting key-value pairs either as `kw` or `=` depending on context
* The way that `parse-resword` is entered has been rearranged to avoid parsing
  reserved words with `parse-atom` inside `parse-unary-prefix`. Instead, we
  detect reserved words and enter `parse_resword` earlier.

## Flisp parser bugs

Here's some behaviors which seem to be bugs. (Some of these we replicate in the
name of compatibility, perhaps with a warning.)

* Macro module paths allow calls which gives weird stateful semantics!
  ```
  b() = rand() > 0.5 ? Base : Core
  b().@info "hi"
  ```
* Misplaced `@` in macro module paths like `A.@B.x` is parsed as odd
  broken-looking AST like `(macrocall (. A (quote (. B @x))))`.  It should
  probably be rejected.
* Operator prefix call syntax doesn't work in the cases like `+(a;b,c)` where
  keyword parameters are separated by commas. A tuple is produced instead.
* `const` and `global` allow chained assignment, but the right hand side is not
  constant. `a` const here but not `b`.
  ```
  const a = b = 1
  ```
* Parsing the `ncat` array concatenation syntax within braces gives
  strange AST: `{a ;; b}` parses to `(bracescat 2 a b)` which is the same as
  `{2 ; a ; b}`, but should probably be `(bracescat (nrow 2 a b))` in analogy
  to how `{a b}` produces `(bracescat (row a b))`.
* `export a, \n $b` is rejected, but `export a, \n b` parses fine.
* In try-catch-finally, the `finally` clause is allowed before the `catch`, but
  always executes afterward. (Presumably was this a mistake? It seems pretty awful!)
* When parsing `"[x \n\n ]"` the flisp parser gets confused, but `"[x \n ]"` is
  correctly parsed as `Expr(:vect)` (maybe fixed in 1.7?)
* `f(x for x in in xs)` is accepted, and parsed very strangely.
* Octal escape sequences saturate rather than being reported as errors. Eg,
  `"\777"` results in `"\xff"`.  This is inconsistent with
  `Base.parse(::Type{Int}, ...)`
* Leading dots in import paths with operator-named modules are parsed into
  dotted operators rather than a relative path. Ie, we have `import .⋆` parsing
  to `(import (. .⋆))` whereas it should be `(import (. . ⋆))` for consistency
  with the parsing of `import .A`.
* Looking back on the output disregards grouping parentheses which can lead to
  odd results in some cases. For example, `f(((((x=1)))))` parses as a keyword
  call to function `f` with the keyword `x=1`, but arguably it should be an
  assignment.
* Hexfloat literals can have a trailing `f` for example, `0x1p1f`
  but this doesn't do anything. In the `flisp` C code such cases are treated as
  Float32 literals and this was intentional https://github.com/JuliaLang/julia/pull/2925
  but this has never been officially supported in Julia. It seems this bug
  arises from `(set! pred char-hex?)` in `parse-number` accepting hex exponent
  digits, all of which are detected as invalid except for a trailing `f` when
  processed by `isnumtok_base`.

## Parsing / AST oddities and warts

### Questionable allowed forms

There's various allowed syntaxes which are fairly easily detected in the
parser, but which will be rejected later during lowering. To allow building
DSLs this is fine and good but some such allowed syntaxes don't seem very
useful, even for DSLs:

* `macro (x) end` is allowed but there are no anonymous macros.
* `abstract type A < B end` and other subtype comparisons are allowed, but
  only `A <: B` makes sense.
* `x where {S T}` produces `(where x (bracescat (row S T)))`. This seems pretty weird!
* `[x for outer x in xs]` parses, but `outer` makes no real sense in this
  context (and using this form is a lowering error)

### `kw` and `=` inconsistencies

There's many apparent inconsistencies between how `kw` and `=` are used when
parsing `key=val` pairs inside parentheses.

* Inconsistent parsing of tuple keyword args inside vs outside of dot calls
  ```julia
  (a=1,)           # (tuple (= a 1))
  f.(a=1)          # (tuple (kw a 1))
  ```
* Mixtures of `,` and `;` in calls give nested parameter AST which parses
  strangely, and is kind-of-horrible to use.
  ```julia
  # (tuple (parameters (parameters e f) c d) a b)
  (a,b; c,d; e,f)
  ```
* Long-form anonymous functions have argument lists which are parsed
  as tuples (or blocks!) rather than argument lists and this mess appears to be
  papered over as part of lowering. For example, in `function (a;b) end` the
  `(a;b)` is parsed as a block! This leads to more inconsistency in the use of
  `kw` for keywords.


### Other oddities

* Operators with suffices don't seem to always be parsed consistently as the
  same operator without a suffix. Unclear whether this is by design or mistake.
  For example, `[x +y] ==> (hcat x (+ y))`, but `[x +₁y] ==> (hcat (call +₁ x y))`

* `global const x=1` is normalized by the parser into `(const (global (= x 1)))`.
  I suppose this is somewhat useful for AST consumers, but reversing the source
  order is pretty weird and inconvenient when moving to a lossless parser.

* `let` bindings might be stored in a block, or they might not be, depending on
  special cases:
   ```
   # Special cases not in a block
   let x=1 ; end   ==>  (let (= x 1) (block))
   let x::1 ; end  ==>  (let (:: x 1) (block))
   let x ; end     ==>  (let x (block))

   # In a block
   let x=1,y=2 ; end  ==>  (let (block (= x 1) (= y 2) (block)))
   let x+=1 ; end     ==>  (let (block (+= x 1)) (block))
   ```

* The `elseif` condition is always in a block but not the `if` condition.
  Presumably because of the need to add a line number node in the flisp parser
  `if a xx elseif b yy end   ==>  (if a (block xx) (elseif (block b) (block yy)))`

* Spaces are allowed between import dots — `import . .A` is allowed, and
  parsed the same as `import ..A`

* `import A..` produces `(import (. A .))` which is arguably nonsensical, as `.`
  can't be a normal identifier.

* The raw string escaping rules are *super* confusing for backslashes near
  the end of the string: `raw"\\\\ "` contains four backslashes, whereas
  `raw"\\\\"` contains only two. However this was an intentional feature to
  allow all strings to be represented and it's unclear whether the situation
  can be improved.

* In braces after macrocall, `@S{a b}` is invalid but both `@S{a,b}` and
  `@S {a b}` parse. Conversely, `@S[a b]` parses.

* Macro names and invocations are post-processed from the output of
  `parse-atom` / `parse-call`, which leads to some surprising and questionable
  constructs which "work":
  - Absurdities like `@(((((a))))) x ==> (macrocall @a x)`
  - Infix macros!? `@(x + y)  ==>  (macrocall @+ x y)` (ok, kinda cute and has
    some weird logic to it... but what?)
  - Similarly additional parentheses are allowed `@(f(x)) ==> (macrocall @f x)`

* Allowing `@` first in macro module paths (eg `@A.B.x` instead of `A.B.@x`)
  seems like unnecessary variation in syntax. It makes parsing valid macro
  module paths more complex and leads to oddities like `@$.x y ==> (macrocall
  ($ (quote x)) y` where the `$` is first parsed as a macro name, but turns out
  to be the module name after the `.` is parsed. But `$` can never be a valid
  module name in normal Julia code so this makes no sense.

* Triple quoted `var"""##"""` identifiers are allowed. But it's not clear these
  are required or desired given that they come with the complex triple-quoted
  string deindentation rules.

* Deindentation of triple quoted strings with mismatched whitespace is weird
  when there's nothing but whitespace. For example, we have
  `"\"\"\"\n  \n \n  \"\"\"" ==> "\n \n"` so the middle line of whitespace
  here isn't dedented but the other two longer lines are?? Here it seems more
  consistent that either (a) the middle line should be deindented completely,
  or (b) all lines should be dedented only one character, as that's the
  matching prefix.

* Parsing of anonymous function arguments is somewhat inconsistent.
  `function (xs...) \n body end` parses the argument list as `(... xs)`, whereas
  `function (x) \n body end` parses the argument list as `(tuple x)`.

* The difference between multidimensional vs flattened iterators is subtle, and
  perhaps too syntactically permissive.  For example,
  - `[(x,y) for x * in 1:10, y in 1:10]` is a multidimensional iterator
  - `[(x,y) for x * in 1:10 for y in 1:10]` is a flattened iterator
  - `[(x,y) for x in 1:10, y in 1:10 if y < x]` is a flattened iterator

  It's this last case which seems problematic (why not *require* the second
  form as a more explicit way to indicate flattening?). It's not even pretty
  printed correctly:
  ```
  julia> :([(x,y) for x in 1:10, y in 1:10 if y < x])
  :([(x, y) for $(Expr(:filter, :(y < x), :(x = 1:10), :(y = 1:10)))])
  ```

# Comparisons to other packages

### Official Julia compiler

The official Julia compiler frontend lives in the Julia source tree. It's
mostly contained in just a few files:
* The parser in [src/julia-parser.scm](https://github.com/JuliaLang/julia/blob/9c4b75d7f63d01d12b67aaf7ce8bb4a078825b52/src/julia-parser.scm)
* Macro expansion in [src/ast.c](https://github.com/JuliaLang/julia/blob/9c4b75d7f63d01d12b67aaf7ce8bb4a078825b52/src/ast.c) and [src/macroexpand.scm](https://github.com/JuliaLang/julia/blob/9c4b75d7f63d01d12b67aaf7ce8bb4a078825b52/src/macroexpand.scm)
* Syntax lowering in [src/julia-syntax.scm](https://github.com/JuliaLang/julia/blob/9c4b75d7f63d01d12b67aaf7ce8bb4a078825b52/src/julia-syntax.scm)
* The flisp runtime and C extensions for Julia in [src/flisp](https://github.com/JuliaLang/julia/tree/master/src/flisp)
* Supporting utility functions in a few other `.scm` and `.c` files.

There's two issues with the official reference frontend which suggest a rewrite.

First, there's no support for precise source locations and the existing data
structures (bare flisp lists) can't easily be extended to add these. Fixing
this would require changes to nearly all of the code.

Second, it's written in flisp: an aestheically pleasing, minimal but obscure
implementation of Scheme. Learning Scheme is actually a good way to appreciate
some of Julia's design inspiration, but it's quite a barrier for developers of
Julia language tooling. (Flisp has no user-level documentation but non-schemers
can refer to the [Racket documentation](https://docs.racket-lang.org) which is
quite compatible for basic things.) In addition to the social factors, having
the embedded flisp interpreter and runtime with its own separate data
structures and FFI is complex and inefficient.

### JuliaParser.jl

[JuliaParser.jl](https://github.com/JuliaLang/JuliaParser.jl)
was a direct port of Julia's flisp reference parser but was abandoned around
Julia 0.5 or so. However it doesn't support lossless parsing and doing so would
amount to a full rewrite. Given the divergence with the flisp reference parser
since Julia-0.5, it seemed better just to start with the reference parser
instead.

### Tokenize.jl

[Tokenize.jl](https://github.com/JuliaLang/Tokenize.jl)
is a fast lexer for Julia code. The code from Tokenize has been
imported and used in JuliaSyntax, with some major modifications as discussed in
the lexer implementation section.

### CSTParser.jl

[CSTParser.jl](https://github.com/julia-vscode/CSTParser.jl)
is a ([mostly?](https://github.com/domluna/JuliaFormatter.jl/issues/52#issuecomment-529945126))
lossless parser with goals quite similar to JuliaParser and used extensively in
the VSCode / LanguageServer / JuliaFormatter ecosystem. CSTParser is very useful
but I do find the implementation hard to understand and I wanted to try a fresh
approach with a focus on:

* "Production readyness": Good docs, tests, diagnostics and maximum similarity
  with the flisp parser, with the goal of getting the new parser into `Core`.
* Learning from the latest ideas about composable parsing and data structures
  from outside Julia. In particular the implementation of `rust-analyzer` is
  very clean, well documented, and a great source of inspiration.
* Composability of tree data structures — I feel like the trees should be
  layered somehow with a really lightweight green tree at the most basic level,
  similar to Roslyn or rust-analyzer. In comparison CSTParser uses a more heavy
  weight non-layered data structure. Alternatively or additionally, have a
  common tree API with many concrete task-specific implementations.

A big benefit of the JuliaSyntax parser is that it separates the parser code
from the tree data structures entirely which should give a lot of flexibility
in experimenting with various tree representations.

I also want JuliaSyntax to tackle macro expansion and other lowering steps, and
provide APIs for this which can be used by both the core language and the
editor tooling.

### tree-sitter-julia

Using a modern production-ready parser generator like `tree-sitter` is an
interesting option and some progress has already been made in
[tree-sitter-julia](https://github.com/tree-sitter/tree-sitter-julia).
But I feel like the grammars for parser generators are only marginally more
expressive than writing the parser by hand after accounting for the effort
spent on the weird edge cases of a real language and writing the parser's tests
and "supporting code".

On the other hand a hand-written parser is completely flexible and can be
mutually understood with the reference implementation so I chose that approach
for JuliaSyntax.

# Resources

## Julia issues

Here's a few links to relevant Julia issues.

#### Macro expansion

* Automatic hygiene for macros https://github.com/JuliaLang/julia/pull/6910 —
  would be interesting to implement this in a new frontend.

#### Lowering

* A partial implementation of lowering in Julia https://github.com/JuliaLang/julia/pull/32201 —
  some of this should be ported.
* The closure capture problem https://github.com/JuliaLang/julia/issues/15276 —
  would be interesting to see whether we can tackle some of the harder cases in
  a new implementation.

## C# Roslyn

[Persistence, façades and Roslyn’s red-green trees](https://ericlippert.com/2012/06/08/red-green-trees/)
* [Roslyn optimization overview](https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees)
* [Literate C# Usage Example](https://johtela.github.io/LiterateCS/LiterateCS/BlockBuilder.html)


## Rust-analyzer

`rust-analyzer` seems to be very close to what I'm building here, and has come
to the same conclusions on green tree layout with explicit trivia nodes.  Their
document on internals
[here](https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/syntax.md)
is great. Points of note:

* They have *three* trees!
  1. Green trees exactly like mine (pretty much all the same design
     decisions, including trivia storage). Though note that the team are still
     [toying with](https://github.com/rust-analyzer/rust-analyzer/issues/6584)
     the idea of using the Roslyn model of trivia.
  2. Untyped red syntax trees somewhat like mine, but much more minimal. For
     example, these don't attempt to reorder children.
  3. A typed AST layer with a type for each expression head. The AST searches
     for children by dynamically traversing the child list each time, rather
     than having a single canonical ordering or remembering the placement of
     children which the parser knew.
* "Parser does not see whitespace nodes. Instead, they are attached to the
  tree in the TreeSink layer." This may be relevant to us - it's a pain to
  attach whitespace to otherwise significant tokens, and inefficient to
  allocate and pass around a dynamic list of whitespace trivia.
* "In practice, incremental reparsing doesn't actually matter much for IDE
  use-cases, parsing from scratch seems to be fast enough." (I wonder why
  they've implemented incremental parsing then?)
* There's various comments about macros... Rust macro expansion seems quite
  different from Julia (it appears it may be interleaved with parsing??)

In general I think it's unclear whether we want typed ASTs in Julia and we
particularly need to deal with the fact that `Expr` is the existing public
interface. Could we have `Expr2` wrap `SyntaxNode`?

* A related very useful set of blog posts which discuss using the rust syntax
  tree library (rowan) for representing of a non-rust toy language is here
  https://dev.to/cad97/lossless-syntax-trees-280c

Not all the design decisions in `rust-analyzer` are finalized but the
[architecture document](https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/architecture.md)
is a fantastic source of design inspiration.

Highlights:
* "The parser is independent of the particular tree structure and particular
  representation of the tokens. It transforms one flat stream of events into
  another flat stream of events."  This seems great, let's adopt it!
* TODO

## RSLint

[RSLint](https://rslint.org/dev) is a linter for javascript, built in Rust. It
uses the same parsing infrastructure and green tree libraries `rust-analyzer`.
There's an excellent and friendly high level overview of how all this works in
the rslint [parsing devdocs](https://rslint.org/dev/parsing.html).

Points of note:

* Backtracking and restarting the parser on error is actually quite simple in
  the architecture we (mostly) share with `rust-analyzer`:
  > ... events allow us to cheaply backtrack the parser by simply draining
  > the events and resetting the token source cursor back to some place.

* The section on [error
  recovery](https://rslint.org/dev/parsing.html#error-recovery) is interesting;
  they talk about various error recovery strategies.

## Diagnostics

The paper [P2429 - Concepts Error Messages for
Humans](https://wg21.tartanllama.xyz/P2429%20-%20Concepts%20Error%20Messages%20for%20Humans.pdf)
is C++ centric, but has a nice review of quality error reporting in various
compilers including Elm, ReasonML, Flow, D and Rust.

Some Rust-specific resources:
* [rustc_errors::Diagnostic](https://doc.rust-lang.org/stable/nightly-rustc/rustc_errors/struct.Diagnostic.html)
* The source of the Rust compiler's diagnostics system:
  - The [`println!` macro](https://github.com/rust-lang/rust/blob/0b6f079e4987ded15c13a15b734e7cfb8176839f/compiler/rustc_builtin_macros/src/format.rs)
    shows how these can be emitted from macros
  - The parser's [diagnostics.rs](https://github.com/rust-lang/rust/blob/0b6f079e4987ded15c13a15b734e7cfb8176839f/compiler/rustc_parse/src/parser/diagnostics.rs)

## General resources about parsing

* [Modern parser generator](https://matklad.github.io/2018/06/06/modern-parser-generator.html)
  has a lot of practical notes on writing parsers. Highlights:
  - Encourages writing tests for handwritten parsers as inline comments
  - Mentions Pratt parsers for simple operator precedence parsing. Good articles:
    - [From Aleksey Kladov (matklad - the main rust-analyzer author, etc)](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
    - [From Bob Nystrom (munificent - one of the Dart devs, etc](http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/)
  - Some discussion of error recovery

* Some notes about stateful lexers for parsing shell-like string interpolations:
  http://www.oilshell.org/blog/2017/12/17.html


# Design notes

The following are some fairly disorganized design notes covering a mixture of
things which have already been done and musings about further work.

## Prototyping approach

The tree datastructure design here is tricky:

1. The symbolic part of compilation (the compiler frontend) incrementally
   abstracts and transforms the source text, but errors along the way should
   refer back to the source.
  - The tree must be a lossless representation of the source text
  - Some aspects of the source text (comments, most whitespace) are irrelevant
    to parsing.
  - More aspects of the source text are irrelevant after we have an abstract
    syntax tree of the surface syntax. Some good examples here are the
    parentheses in `2*(x + y)` and the explicit vs implicit multiplication
    symbol in `2*x` vs `2x`.

2. There's various type of *analyses*
- There's many useful ways to augment a syntax tree depending on use case.
- Analysis algorithms should be able to act on any tree type, ignoring
  but carrying augmentations which they don't know about.

Having so many use cases suggests it might be best to have several different
tree types with a common interface rather than one main abstract syntax tree
type. But it seems useful to figure this out by prototyping several important
work flows:

* Syntax transformations
  - Choose some macros to implement. This is a basic test of mixing source
    trees from different files while preserving precise source locations.
    (Done in <test/syntax_interpolation.jl>.)
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

### Raw syntax tree / Green tree

Raw syntax tree (or "Green tree" in the terminology from Roslyn)

We want GreenNode to be
* *structurally minimal* — For efficiency and generality
* *immutable*            — For efficiency (& thread safety)
* *complete*             — To preserve parser knowledge
* *token agnostic*       — To allow use with any source language

The simplest idea possible is to have:
* Leaf nodes are a single token
* Children are in source order

Call represents a challenge for the AST vs Green tree in terms of node
placement / iteration for infix operators vs normal prefix function calls.

- The normal problem of `a + 1` vs `+(a, 1)`
- Or worse, `a + 1 + 2` vs `+(a, 1, 2)`

Clearly in the AST's *interface* we need to abstract over this placement. For
example with something like the normal Julia AST's iteration order.

### Abstract syntax tree

By pointing to green tree nodes, AST nodes become traceable back to the original
source.

Unlike most languages, designing a new AST is tricky because the existing
`Expr` is a very public API used in every macro expansion. User-defined
macro expansions interpose between the source text and lowering, and using
`Expr` looses source information in many ways.

There seems to be a few ways forward:
* Maybe we can give `Expr` some new semi-hidden fields to point back to the
  green tree nodes that the `Expr` or its `args` list came from?
* We can use the existing `Expr` during macro expansion and try to recover
  source information after macro expansion using heuristics. Likely the
  presence of correct hygiene can help with this.
* Introducing a new AST would be possible if it were opt-in for some
  hypothetical "new-style macros" only. Fixing hygiene should go along with
  this. Design challenge: How do we make manipulating expressions reasonable
  when literals need to carry source location?

One option which may help bridge between locationless ASTs and something new
may be to have wrappers for the small number of literal types we need to cover.
For example:

```julia
SourceSymbol <: AbstractSymbol
SourceInt    <: Integer
SourceString <: AbstractString
```

Having source location attached to symbols would potentially solve most of the
hygiene problem. There's still the problem of macro helper functions which use
symbol literals; we can't very well be changing the meaning of `:x`! Perhaps
the trick there is to try capturing the current module at the location of the
interpolation syntax. Eg, if you do `:(y + $x)`, lowering expands this to
`Core._expr(:call, :+, :y, x)`, but it could expand it to something like
`Core._expr(:call, :+, :y, _add_source_symbol(_module_we_are_lowering_into, x))`?

## Parsing

### Error recovery

Some disorganized musings about error recovery

Different types of errors seem to occur...

* Disallowed syntax (such as lack of spaces in conditional expressions)
  where we can reasonably just continue parsing and emit the node with an error
  flag which is otherwise fully formed. In some cases like parsing infix
  expressions with a missing tail, emitting a zero width error token can lead
  to a fully formed parse tree without the productions up the stack needing to
  participate in recovery.
* A token which is disallowed in current context. Eg, `=` in parse_atom, or a
  closing token inside an infix expression. Here we can emit a `K"error"`, but
  we can't descend further into the parse tree; we must pop several recursive
  frames off. Seems tricky!

A typical structure is as follows:

```julia
function parse_foo(ps)
    mark = position(ps)
    parse_bar(ps)  # What if this fails?
    if peek(ps) == K"some-token"
        bump(ps)
        parse_baz(ps)  # What if this fails?
        emit(ps, mark, K"foo")
    end
end
```

Emitting plain error tokens are good in unfinished infix expressions:

```julia
begin
    a = x +
end
```

The "missing end" problem is tricky, as the intermediate syntax is valid; the
problem is often only obvious until we get to EOF.

Missing end
```julia
function f()
    begin
        a = 10
end

# <-- Indentation would be wrong if g() was an inner function of f.
function g()
end
```

It seems like ideal error recovery would need to backtrack in this case. For
example:

- Pop back to the frame which was parsing `f()`
- Backtrack through the parse events until we find a function with indentation
  mismatched to the nesting of the parent.
- Reset ParseStream to a parsing checkpoint before `g()` was called
- Emit error and exit the function parsing `f()`
- Restart parsing
- Somehow make sure all of this can't result in infinite recursion 😅

Missing commas or closing brackets in nested structures also present the
existing parser with a problem.

```julia
f(a,
  g(b,
    c    # -- missing comma?
    d),
  e)
```

Again the local indentation might tell a story

```julia
f(a,
  g(b,
    c    # -- missing closing `)` ?
  d)
```

But not always!

```julia
f(a,
  g(b,
    c    # -- missing closing `,` ?
  d))
```

Another particularly difficult problem for diagnostics in the current system is
broken parentheses or double quotes in string interpolations, especially when
nested.

# Fun research questions

### Parser Recovery

Can we learn fast and reasonably accurate recovery heuristics for when the
parser encounters broken syntax, rather than hand-coding these? How would we
set the parser up so that training works and injecting the model is
nonintrusive? If the model is embedded in and works together with the parser,
can it be made compact enough that training is fast and the model itself is
tiny?

### Formatting

Given source and syntax tree, can we regress/learn a generative model of
indentation from the syntax tree?  Source formatting involves a big pile of
heuristics to get something which "looks nice"... and ML systems have become
very good at heuristics. Also, we've got huge piles of training data — just
choose some high quality, tastefully hand-formatted libraries.
