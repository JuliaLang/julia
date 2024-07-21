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
JuliaSyntax.sourcetext
JuliaSyntax.highlight
```

`SourceFile`-specific functions:

```@docs
JuliaSyntax.SourceFile
JuliaSyntax.source_line_range
```

## Expression heads/kinds

```@docs
JuliaSyntax.Kind
JuliaSyntax.SyntaxHead
JuliaSyntax.@K_str
JuliaSyntax.kind
JuliaSyntax.head
JuliaSyntax.flags
```

see also predicates related to `flags`.

## Syntax trees

Syntax tree types:

```@docs
JuliaSyntax.SyntaxNode
JuliaSyntax.GreenNode
```

Functions applicable to syntax trees include everything in the sections on
heads/kinds as well as the accessor functions in the source code handling
section.
