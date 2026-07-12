# UnifiedIR

UnifiedIR is one IR data structure for the Julia compiler and for external
compilers: a flat statement table with hybrid regions, explicit layout
states, a namespaced kind registry, extension columns, and exactly two
renaming points. The same substrate stores syntax trees (JuliaSyntax's
`SyntaxGraph` runs on it), lowered and optimized function bodies
(`Compiler.Unified`), and external compilers' IRs (dataflow dialects in
the floating layout).

The normative specification is [`design.md`](design.md). These pages are the
practical documentation:

- [**Tour**](tour.md) — hands-on: get IR for real code with `@code_unified`,
  read the listing, build IR by hand, round-trip the text format, run the
  reference interpreter.
- [**Concepts**](concepts.md) — statements and tagged operands, regions,
  layout states and the two renaming points, kinds and dialects, extension
  columns, cells.
- [**Text format**](textformat.md) — the printed/parsed syntax, statement by
  statement.
- [**Writing passes**](passes.md) — mutation APIs per layout state, the
  stale-id discipline, verification, and a pass checklist.

Planned pages (not yet written): trees and provenance (the generic porcelain
that `SyntaxTree` is an alias of), registering dialects (worked example:
a small dataflow dialect), and the `Compiler.Unified` interop surface (converters,
queries, activation).

## Where things live

| | |
|---|---|
| `UnifiedIR/` | this package: the substrate, core dialect, verifier, text format, tree porcelain, test dialect + reference interpreter |
| `Compiler/src/unified/` | `Compiler.Unified` (load with `Compiler.load_unified!()`): CodeInfo/IRCode converters, native inference and optimizer, queries, activation |
| `JuliaSyntax/` | the parser; its `SyntaxGraph`/`SyntaxTree` are the substrate + tree conventions |
| `JuliaLowering/src/unified/` | `UnifiedBackend`: direct structured lowering to UnifiedIR with per-statement provenance |
| `UnifiedIR/demo/provenance_demo.jl` | the one-stack demo: parse → lower → infer/optimize → source-highlighted diagnostics → AST GC |

## Thirty seconds of it

```julia
pushfirst!(LOAD_PATH, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))
import Compiler
const UC = Compiler.load_unified!()

julia> UC.@code_unified gcd(10, 20)
func @gcd(%1::Const(gcd), %2::Int64, %3::Int64) -> Int64 {
  %4 = call global Base.===, %2, const 0 :: Bool !flag(0x0000000f)
  %5 = if %4 {
    ...
  } else {
    ...
  } :: Int64
  return %5
}
```

Structured control flow is first-class (`if`/`loop`/`try` own regions;
there is no goto in sight unless the body genuinely needs a `cfg` island),
every statement is typed, and the printed form round-trips through
`parse_ir`.
