# The text format

`print_ir(io, ir)` and `parse_ir(text)` round-trip the portable subset of
the IR. Tests are written in it; paste it into issues. The grammar below is
the printer's output form (the parser accepts exactly this).

## Function header

```
func @name(%1::T1, %2::T2) -> RT {
  ...body statements...
}
```

Root `region_arg`s print in the header. A floating-layout body prints as
`node @name(...)  layout=floating { eq %n = ... }` with unordered `eq`
lines instead of a statement sequence.

## Statements

```
%5 = call global Base.add_int, %3, const 1 :: Int64 !flag(0x0000000f)
```

- `%n = ` — present iff the statement produces a used result.
- kind — unqualified for the core dialect (`call`, `extract`, `cell_set`),
  dialect-qualified otherwise (`test.add`, `test.icmp`).
- operands, comma-separated (see below).
- ` :: T` — the statement's type.
- ` !flag(0x…)` — the flags word (effects bits; omitted when zero).

## Operands

| form | meaning |
|---|---|
| `%17` | SSA reference to statement 17 |
| `const 1`, `const "s"`, `const type Int64`, `const #<opaque T>` | interned constant (opaque = unprintable value; not portable) |
| `global Base.sin` | global reference |
| `sparam 2` | static parameter |
| `:sle` | inline immediate (symbols, small integers, bools) |
| `^r3` / `^bb2` | region / island-block reference (terminator targets) |

## Region-owning statements

The owned regions print as brace blocks. **The statement's result-type
annotation sits on the final closing brace** — inline annotations belong to
operands, so a type between the introducer and `{` would read as an
assertion about the condition or the init tuple:

```
%7 = if %6 {
  ...then arm...
} else {
  ...else arm...
} :: Float64

%3 = loop (init %4::Int64 = const 1, init %5::Int64 = const 0) {
  ...body; region_args %4, %5 are the carried values...
} :: Int64

%9 = try {
  ...body...
} catch (%exc::Any) {
  ...handler; %exc is the handler's leading region_arg...
} :: Union{Int64, Nothing}
```

`loop`'s parenthesized list pairs each carried `region_arg` with its init
operand. An unused-result owner prints without `%n = `.

## Terminators

```
result %21            # feed the owning statement's value (arity may be >1)
break %3 (%5)         # exit loop %3, delivering its result(s)
continue %3 if %6 (%9, %8)   # backedge to loop %3 when %6, carrying values
return %13
unreachable
goto (^bb4)           # island-internal edge
```

`break`/`continue`/`return` name their target statement explicitly —
multi-level exits are spelled, not implied. Parenthesized lists after an
exit are its carried values (tuple results destructure via `extract`,
1-based).

## `cfg` islands

```
%27 = cfg (%2) {
  ^bb1(%28::Int64):
    ...
    br_if %30 (^bb2) (^bb3)
  ^bb2():
    ...
    result %31
} :: Bool
```

Blocks are labeled `^bbN` with block arguments; island terminators are
`goto`, `br_if`, and the sealed exits (`result`/`break`/`continue`/
`return`) whose value lists are the only channel out of the island.

## Cells

```
%4 = cell const type Int64 :: Any
cell_new %4                    # (re)declare undef at this point
cell_set %4, %9
%12 = cell_get %4 :: Int64
%13 = cell_isdefined %4 :: Bool
throw_undef_if_not %13, const :x
```

## Display options

The REPL shows the full listing by default. `display_maxlines!(n)` sets a
global line budget (`nothing` restores full); `IOContext(io, :ir_maxlines
=> n)` overrides per stream. `IOContext(io, :stmt_annotate => (ir, s) ->
"…")` prints a `// …` line above each statement — the provenance demo uses
it for source excerpts.
