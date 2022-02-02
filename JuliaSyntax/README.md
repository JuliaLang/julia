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

### Design Opinions

* Parser implementation should be independent from tree data structures. So
  we have the `ParseStream` interface.
* Tree data structures should be *layered* to balance losslessness with
  abstraction and generality. So we have `SyntaxNode` (an AST) layered on top
  of `GreenNode` (a lossless parse tree). We might need other tree types later.
* Fancy parser generators still seem marginal for production compilers. We use
  a boring but flexible recursive descent parser.

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

# Parser implementation

Our goal is to losslessly represent the source text with a tree; this may be
called a "lossless syntax tree". (This is sometimes called a "concrete syntax
tree", but that term has also been used for the parse tree of the full formal
grammar for a language including any grammar hacks required to solve
ambiguities, etc. So we avoid this term.)

`JuliaSyntax` uses use a mostly recursive descent parser which closely
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


### More about syntax kinds

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
* `abstract type A < B end` and other subtypes comparisons are allowed, but
  only `A <: B` makes sense.
* `x where {S T}` produces `(where x (bracescat (row S T)))`

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


### Flattened generators

Flattened generators are uniquely problematic because the Julia AST doesn't
respect a key rule we normally expect: that the children of an AST node are a
*contiguous* range in the source text. This is because the `for`s in
`[xy for x in xs for y in ys]` are parsed in the normal order of a for loop as

```
for x in xs
for y in ys
  push!(xy, collection)
```

and the standard Julia AST is like this:

```
(flatten
  (generator
    (generator
      xy
      (= y ys))
    (= x xs)))
```

however, note that if this tree were flattened, the order of tokens would be
`(xy) (y in ys) (x in xs)` which is *not* the source order.  So in this case
our green tree must deviate from the Julia AST. The natural representation
seems to be to flatten the generators:

```
(flatten
  xy
  (= x xs)
  (= y ys))
```

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

* The raw string escaping rules are *super* confusing for backslashes near vs
  at the end of the string: `raw"\\\\ "` contains four backslashes, whereas
  `raw"\\\\"` contains only two. It's unclear whether anything can be done
  about this, however.

* In braces after macrocall, `@S{a b}` is invalid but both `@S{a,b}` and
  `@S {a b}` parse. Conversely, `@S[a b]` parses.

# Resources

## Julia issues

Here's a few links to relevant Julia issues. No doubt there's many more.

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

Rust is renowned for having great compiler diagnostics, so it's probably a good
place to get inspiration from.

Some resources:
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
* Introducing a new AST would be possible if it were opt-in for new-style
  macros only. Fixing hygiene should go along with this. Design challenge: How
  do we make manipulating expressions reasonable when literals need to carry
  source location?

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
  where we can reasonably just continue parsing the production and emit the
  node with an error flag which is otherwise fully formed. In some cases like
  parsing infix expressions with a missing tail, emitting a zero width error
  token can lead to a fully formed parse tree without the productions up the
  stack needing to participate in recovery.
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

For this kind of recovery it sure would be good if we could reify the program
stack into a parser state object...

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
    c    # -- missing closing `)` ?
  d)
```

# Fun research questions

* Given source and syntax tree, can we regress/learn a generative model of
  indentation from the syntax tree?  Source formatting involves a big pile of
  heuristics to get something which "looks nice"... and ML systems have become
  very good at heuristics.  Also, we've got huge piles of training data — just
  choose some high quality, tastefully hand-formatted libraries.

* Similarly, can we learn fast and reasonably accurate recovery heuristics for
  when the parser encounters broken syntax rather than hand-coding these? How
  do we set the parser up so that training works and inference is nonintrusive?
