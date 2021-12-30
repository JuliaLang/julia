# JuliaSyntax

[![Build Status](https://github.com/c42f/JuliaSyntax.jl/workflows/CI/badge.svg)](https://github.com/c42f/JuliaSyntax.jl/actions)

Yet another Julia frontend, written in Julia.

Goals:
* Parse Julia code with precise source mapping
* Avoid worrying about how much work this will be 😅

Nice to have:
* Speedy enough for interactive editing
* Production quality error recovery and reporting
* "Compilation as an API" to support all sorts of tooling
* Make the code easy to maintain in parallel with Julia's flisp frontend
* Go further than parsing - macro expansion, syntax desugaring and scope analysis

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

### Raw syntax tree / Green tree

Raw syntax tree (RST, or "Green tree" in the terminology from Roslyn)

We want GreenNode to be
* *structurally minimal* — For efficiency and generality
* *immutable*            — For efficiency (& thread safety?)
* *complete*             — To preserve parser knowledge
* *token agnostic*       — To allow use with any source language

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
- Or worse, `a + 1 + 2` vs `+(a, 1, 2)`

Clearly in the AST's *interface* we need to abstract over this placement. For
example with something like the normal Julia AST's iteration order.

### Abstract syntax tree

By pointing to green tree nodes, AST nodes become tracable back to the original
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
hygine problem. There's still the problem of macro helper functions which use
symbol literals; we can't very well be changing the meaning of `:x`! Perhaps
the trick there is to try capturing the current module at the location of the
interpolation syntax. Eg, if you do `:(y + $x)`, lowering expands this to
`Core._expr(:call, :+, :y, x)`, but it could expand it to something like
`Core._expr(:call, :+, :y, _add_source_symbol(_module_we_are_lowering_into, x))`?

## Parsing

The goal of the parser is to produce well-formed heirarchical structure from
the source text. For interactive tools we need this to work even when the
source text contains errors, so it's the job of the parser to include the
recovery heuristics necessary to make this work.

Concretely, the parser in `JuliaSyntax` should always produce a green tree
which is *well formed* in the sense that `GreenNode`s of a given `Kind` have
well-defined layout of children. This means the `GreenNode` to `SyntaxNode`
transformation is deterministic and tools can assume they're working with a
"mostly valid" AST.

What does "mostly valid" mean? We allow the tree to contain the following types
of error nodes:

* Missing tokens or nodes may be **added** as placeholders when they're needed
  to complete a piece of syntax. For example, we could parse `a + (b *` as
  `(call-i a + (call-i * b XXX))` where `XXX` is a placeholder.
* A sequence of unexpected tokens may be **removed** by collecting
  them as children of an error node and treating them as syntax trivia during
  AST construction. For example, `a + b end * c` could be parsed as the green
  tree `(call-i a + b (error end * c))`, and turned into the AST `(call + a b)`.

We want to encode both these cases in a way which is simplest for downstream
tools to use. This is an open question, but for now we use `K"error"` as the
token head, with the `TRIVIA_FLAG` set for unexpected syntax.

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

It seems like ideal error recorvery would need to backtrack in this case. For
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

## Fun research questions

* Given source and syntax tree, can we regress/learn a generative model of
  indentiation from the syntax tree?  Source formatting involves a big pile of
  heuristics to get something which "looks nice"... and ML systems have become
  very good at heuristics.  Also, we've got huge piles of traininig data — just
  choose some high quality, tastefully hand-formatted libraries.

* Similarly, can we learn fast and reasonably accurate recovery heuristics?

# Resources

## C# Roslyn

[Persistence, façades and Roslyn’s red-green trees](https://ericlippert.com/2012/06/08/red-green-trees/)
* [Roslyn optimization overview](https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees)
* [Literate C# Usage Example](https://johtela.github.io/LiterateCS/LiterateCS/BlockBuilder.html)


## Rust-analyzer

`rust-analyzer` seems to be very close to what I'm buildin here, and has come
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

# Parser devdocs

# Differences from the flisp parser

## Make parsing decisions earlier

The flisp-based parser has many places where it parses an expression and then
optionally rearranges the resulting AST, modifying heads of expressions etc.

This parser tries hard to avoid that pattern becase
* It works poorly when the parser is emitting a stream of node spans rather
  than eagerly creating a tree data structure.
* It's confusing to re-make parsing decisions

Often the information required to avoid postprocessing the parse tree is
available early with a bit of restructuring and we make use of this wherever
possible.

## Function names

Large structural changes were generally avoided while porting. In particular,
nearly all function names for parsing productions are the same with `-`
replaced by `_` and predicates prefixed by `is_`.

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

Here's some behaviors which seem to be bugs:

* Macro module paths allow calls which gives weird stateful semantics!
  ```
  b() = rand() > 0.5 ? Base : Core
  b().@info "hi"
  ```
* Misplaced `@` in macro module paths like `A.@B.x` is parsed as odd
  broken-looking AST like `(macrocall (. A (quote (. B @x))))`.  It should
  probably be rejected.
* Operator prefix call syntax doesn't work in the cases like `+(a;b,c)` where
  parameters are separated by commas. A tuple is produced instead. 
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
  parsed as `Expr(:vect)`

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
  as tuples rather than argument lists. This leads to more inconsistency in the
  use of `kw` for keywords.


### Flattened generators

Flattened generators are hard because the Julia AST doesn't respect a key
rule we normally expect: that the children of an AST node are a contiguous
range in the source text. This is because the `for`s in
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
(= x xs))
```

however, note that if this tree were flattened, the order of tokens would be
`(xy) (y in ys) (x in xs)` which is *not* the source order.  So in this case
our tree needs to deviate from the Julia AST. The natural representation seems
to be to flatten the generators:

```
(flatten
xy
(= x xs)
(= y ys))
```

### Other oddities

* `global const x=1` is normalized by the parser into `(const (global (= x 1)))`.
  I suppose this is somewhat useful for AST consumers, but it seems a bit weird
  and unnecessary.

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

* `import . .A` is allowed, and parsed the same as `import ..A`

* `import A..` produces `(import (. A .))` which is arguably nonsensical, as `.`
  can't be a normal identifier.

