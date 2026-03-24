# API Reference

## Parsing

```@docs
JuliaSyntax.parsestmt
JuliaSyntax.parseall
JuliaSyntax.parseatom
```

### Low level parsing API

The `ParseStream` interface which provides a low-level stream-like I/O
interface for writing the parser. The parser does not depend on or produce any
concrete tree data structure as part of the parsing phase but the output spans
can be post-processed into various tree data structures as required using
[`JuliaSyntax.build_tree`](@ref).

```@docs
JuliaSyntax.parse!
JuliaSyntax.ParseStream
JuliaSyntax.build_tree
```

## Tokenization

```@docs
JuliaSyntax.tokenize
JuliaSyntax.untokenize
JuliaSyntax.Token
```

## Source code handling

This section describes the generic functions for source text, source location
computation and formatting functions.

Contiguous syntax objects like nodes in the syntax tree should implement the
following where possible:

```@docs
JuliaSyntax.sourcefile
JuliaSyntax.byte_range
```

This will provide implementations of the following which include range
information, line numbers, and fancy highlighting of source ranges:

```@docs
JuliaSyntax.first_byte
JuliaSyntax.last_byte
JuliaSyntax.filename
JuliaSyntax.source_line
JuliaSyntax.source_location
JuliaSyntax.char_range
JuliaSyntax.sourcetext
JuliaSyntax.highlight
```

`SourceFile`-specific functions:

```@docs
JuliaSyntax.SourceFile
JuliaSyntax.source_line_range
```

## Expression predicates, kinds and flags

Expressions are tagged with a kind - like a type, but represented as an integer
tag rather than a full Julia type for efficiency. (Very like the tag of a "sum
type".) `Kind`s are constructed with the `@K_str` macro.

```@docs
JuliaSyntax.@K_str
JuliaSyntax.Kind
```

The kind of an expression `ex` in a tree should be accessed with `kind(ex)`

```@docs
JuliaSyntax.kind
```

In addition to the `kind`, a small integer set of "flags" is included to
further distinguish details of each expression, accessed with the `flags`
function. The kind and flags can be wrapped into a `SyntaxHead` which is
accessed with the `head` function.

```@docs
JuliaSyntax.flags
JuliaSyntax.SyntaxHead
JuliaSyntax.head
```

Details about the flags may be extracted using various predicates:

```@docs
JuliaSyntax.is_trivia
JuliaSyntax.is_prefix_call
JuliaSyntax.is_infix_op_call
JuliaSyntax.is_prefix_op_call
JuliaSyntax.is_postfix_op_call
JuliaSyntax.is_dotted
JuliaSyntax.is_suffixed
JuliaSyntax.is_decorated
JuliaSyntax.numeric_flags
```

Some of the more unusual predicates are accessed merely with `has_flags(x,
flag_bits)`, where any of the following uppercase constants may be used for
`flag_bits` after checking that the `kind` is correct.

```@docs
JuliaSyntax.has_flags
JuliaSyntax.TRIPLE_STRING_FLAG
JuliaSyntax.RAW_STRING_FLAG
JuliaSyntax.PARENS_FLAG
JuliaSyntax.TRAILING_COMMA_FLAG
JuliaSyntax.COLON_QUOTE
JuliaSyntax.TOPLEVEL_SEMICOLONS_FLAG
JuliaSyntax.MUTABLE_FLAG
JuliaSyntax.BARE_MODULE_FLAG
JuliaSyntax.SHORT_FORM_FUNCTION_FLAG
```

## Syntax trees

Access to the children of a tree node is provided by the functions

```@docs
JuliaSyntax.is_leaf
JuliaSyntax.numchildren
JuliaSyntax.children
```

For convenient access to the children, we also provide `node[i]`, `node[i:j]`
and `node[begin:end]` by implementing `Base.getindex()`, `Base.firstindex()` and
`Base.lastindex()`. We choose to return a view from `node[i:j]` to make it
non-allocating.

Tree traversal is supported by using these functions along with the predicates
such as [`kind`](@ref) listed above.

### Trees referencing the source

```@docs
JuliaSyntax.SyntaxNode
```

Functions applicable to `SyntaxNode` include everything in the sections on
heads/kinds as well as the accessor functions in the source code handling
section.

### Relocatable syntax trees

[`GreenNode`](@ref) is a special low level syntax tree: it's "relocatable" in
the sense that it doesn't carry an absolute position in the source code or even
a reference to the source text. This allows it to be reused for incremental
parsing, but does make it a pain to work with directly!

```@docs
JuliaSyntax.GreenNode
```

Green nodes only have a relative position so implement `span()` instead of
`byte_range()`:

```@docs
JuliaSyntax.span
```
