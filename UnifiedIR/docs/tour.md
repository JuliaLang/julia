# Tour

## Getting IR for real code

Load the compiler port and use `@code_unified`, the `@code_typed` of the
unified pipeline:

```julia
pushfirst!(LOAD_PATH, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))
import Compiler
const UC = Compiler.load_unified!()

mysum(n) = begin s = 0; i = 1; while i <= n; s += i; i += 1; end; s end

julia> ir = UC.@code_unified mysum(10)
func @mysum(%1::Const(mysum), %2::Int64) -> Int64 {
  %3 = loop (init %4::Int64 = const 1, init %5::Int64 = const 0) {
    %6 = call global Base.sle_int, %4, %2 :: Bool !flag(0x0000000f)
    %7 = if %6 {
      %8 = call global Base.add_int, %5, %4 :: Int64 !flag(0x0000000f)
      %9 = call global Base.add_int, %4, const 1 :: Int64 !flag(0x0000000f)
      continue %3 if const true (%9, %8)
    } else {
      break %3 (%5)
    } :: Union{}
    unreachable
  } :: Int64
  return %3
}
```

Arguments may be values (their types are taken; nothing is called) or bare
type annotations; `optimize=false` stops after inference:

```julia
UC.@code_unified optimize=false sort(::Vector{Int})
UC.typed_ir(sin, Any[Float64])                    # the functional form
UC.infer_return(hypot, Any[Float64, Float64])     # just the return type
```

The REPL display prints the full listing. To truncate long ones, opt in
with `UnifiedIR.display_maxlines!(40)` (global) or
`IOContext(io, :ir_maxlines => 40)` (per stream).

## Reading a listing

- `%3 = loop (init %4::Int64 = const 1, ...) { ... } :: Int64` — a
  region-owning statement. The parenthesized list declares the loop's
  carried values (`region_arg`s) and their initial operands; the trailing
  `:: Int64` on the closing brace is the type of the statement's result.
- `continue %3 if const true (%9, %8)` — terminators name the region-owning
  statement they exit to and carry the next iteration's values; `break %3
  (%5)` delivers the loop's result.
- `call global Base.add_int, %5, %4` — operands are SSA references (`%n`),
  interned constants (`const 1`), globals (`global M.x`), or static
  parameters. `!flag(0x...)` is the per-statement effects/flags word.
- An `if` whose result is unused prints without the `%n =` prefix.
- When a body has genuinely unstructured control flow, it appears as an
  explicit `cfg { ^bb1: ...; goto (^bb2) }` island with block arguments —
  structured ops and islands compose in one body.

## Building IR by hand

The builder is append-only; regions are opened and closed explicitly (or
via the `build_if!`/`build_loop!` block helpers), and `finish!` seals to
the dense layout and verifies. Sum of `1..n`, from the package's own smoke
test:

```julia
using UnifiedIR

b = Builder(name = :sumn)
n = append_stmt!(b, K"region_arg"; type = Int64)              # the argument
r = build_loop!(b, 0, 1; type = Tuple{Int64,Int64},           # carried (s, j)
                argtypes = Any[Int64, Int64]) do b, args
    s, j = args
    s2  = append_stmt!(b, K"test.add", s, j; type = Int64)
    j2  = append_stmt!(b, K"test.add", j, 1; type = Int64)
    cnd = append_stmt!(b, K"test.icmp", :sle, j2, n; type = Bool)
    body = UnifiedIR.current_region(b)
    append_stmt!(b, K"continue", op_region(body), op_stmt(cnd),
                 op_stmt(s2), op_stmt(j2))                    # backedge…
end                                                           # …falls out = loop result
sum_ = append_stmt!(b, K"extract", op_stmt(r), op_inline(1); type = Int64)
append_stmt!(b, K"return", sum_)
ir = finish!(b)
```

Value arguments are converted automatically (`StmtId` → SSA reference,
small integers → inline immediates, other values → interned constants);
pass `Operand`s (`op_stmt`, `op_region`, `op_inline`, …) for full control.
Multi-value results (here the loop's `(s, j)` tuple) are destructured with
`extract` — 1-based, exactly `getfield`'s index. The `test` dialect used
here ships with the package so the substrate is exercisable without Julia
semantics; real Julia bodies come from the converters or the lowering
backend.

## Text round-trip and the reference interpreter

```julia
txt = print_ir(ir)                # the listing as a String
ir2 = parse_ir(txt)               # parse it back
struct_eq(ir, ir2)                # structural equality

interpret(ir, 10)                 # reference semantics for the test dialect
verify_ir(ir; level = 1)          # structural (L0) + dataflow/visibility (L1)
```

`print_ir`/`parse_ir` round-trip the portable subset — this is what test
cases are written in, and what you paste into an issue.

## Where the IR comes from and goes

```
JuliaSyntax parse ─▶ JuliaLowering front half ─▶ UnifiedBackend  ─▶ UnifiedIR
                                                                      │
CodeInfo ────────────── codeinfo_to_ir ──────────────────────────────▶│
                                                                      ▼
                                              infer_ir! / optimize_ir!│
                                                                      ▼
                    ir_to_codeinfo / typed IRCode exit  ◀─────────────┘
```

`with_unified_compiler() do ... end` runs real code through the pipeline
via the ordinary compiler-plugin mechanism; `activate!()` installs it as
the global inference entry.
