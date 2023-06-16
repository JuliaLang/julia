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

## Source file handling

```@docs
JuliaSyntax.SourceFile
JuliaSyntax.highlight
JuliaSyntax.sourcetext
JuliaSyntax.source_line
JuliaSyntax.source_location
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

## Syntax tree types

```@docs
JuliaSyntax.SyntaxNode
JuliaSyntax.GreenNode
```
