# Writing passes

## The shape of a pass

A pass picks the layout state whose mutation vocabulary it needs, works,
and leaves the body verifiable. The states and their APIs:

**Dense** — analysis and in-place rewriting. Iterate with `each_stmt(ir)` /
`region_stmts(ir, r)`; read with `stmt_kind`/`stmt_type`/`stmt_flag`/
`nops`/`getop`/`operands`; write types and flags through `set_type!`/
`set_flag!`/`add_flag!` (they bump the analysis-cache epochs). Structural
mutation is deliberately narrow:

- `replace_stmt!(ir, s, kind, ops...; type)` — same identity, same
  footprint (result arity, terminator-ness, region ownership preserved).
- `delete_stmt!(ir, s)` — tombstone; plain statements only (never owners,
  terminators, `region_arg`s). Uses must be gone by verify time.
- `replace_uses!(ir, old => new)` — queued; `flush_renames!` applies all of
  them in one O(n) sweep. Renames compose; reads during the pass observe
  pre-flush operands.
- `comes_before(ir, a, b)` — integer compare, dense only.

**Editable** (`editable(ir)`) — insertion and surgery: `insert_before!`/
`insert_after!`/`push_stmt!`, and the structured operations `wrap_in_if!`,
`wrap_in_loop!`, `inline_region!`, `splice_body!` (inlining another body).
Statement ids remain stable; positions are list links.

**Floating** (`float!(ir)`) — order is discarded (legal only when every
statement is reorderable or covered by dialect ordering rules);
`schedule!(ir; strategy)` re-linearizes.

## The stale-id discipline

Ids are stable except at exactly two renaming points — `compact!` (dense
repack) and `schedule!` (floating → dense). Both return a `RemapSet`:

```julia
ir, remap = compact!(ir)
new_id = remap[old_id]        # 0 ⇒ the statement was dropped
```

Anything holding `StmtId`s across a renaming point — worklists, side
tables, caches, cursors — must translate through the remap or re-derive.
Extension columns are handled for you (each column class participates in
compaction through uniform hooks); `Annotation`-class columns are rekeyed
but their *values* are never rewritten (that's what makes cross-namespace
references like provenance cursors safe to store).

## Visibility, not block dominance

When moving or forwarding a value, legality is the §5.5 visibility
relation, not CFG dominance: earlier in the same region; in an enclosing
region before the owner; or island-internal dominance for `cfg` blocks.
`visible(ir, def, use)` answers it; the L1 verifier enforces it globally.
The practical consequences:

- forwarding a value into a region is fine (enclosing defs are visible);
  hoisting one *out* of a region is a code-motion decision that must check
  reorderability (`FLAG_REMOVABLE`: effect-free ∧ nothrow ∧ terminates —
  plain effect-freedom is not enough to move something).
- values leave regions only through terminators; values leave islands only
  through sealed exit-value lists. A pass that needs a value across one of
  those boundaries threads it (grow the exit's value list / the owner's
  results) — see the cell-promotion passes for the worked pattern.

## Effects and flags

Per-statement flags start from the kind's registered default and are
refined by inference/optimizer passes (`FLAG_CONSISTENT`,
`FLAG_EFFECT_FREE`, `FLAG_NOTHROW`, `FLAG_TERMINATES`, plus `REMOVABLE` and
`PURE` masks). DCE removes an unused statement iff `REMOVABLE`; anything
weaker must stay.

## Verify early, verify often

`verify_ir(ir; level = 0)` checks structure (region nesting, arg
placement, terminator arities); `level = 1` adds dataflow: visibility of
every operand, use counts, exit arities, gc-preserve pairing. Passes should
verify in tests after every transformation they perform; the fuzz harnesses
(`test/fuzz.jl`, `Compiler/test/unified/cellfuzz.jl`) show the pattern of
pairing verification with semantic differentials through the reference
interpreter — a pattern that has repeatedly caught order-dependence,
staleness, and termination bugs that unit tests missed.

## Checklist

1. Right layout state; transition explicitly (`editable`/`float!`) and
   seal what you open.
2. No raw id arithmetic; ids through `RemapSet` at renaming points.
3. Deletions: `REMOVABLE` (or dialect rules) + no remaining uses.
4. Code motion: visibility + reorderability, never just effect-freedom.
5. Region/island boundaries: values cross through results/exit lists only.
6. Columns you add: pick the right class (`Semantic`/`Annotation`/
   `Derived`); wrong class = silently stale or needlessly invalidated data.
7. Tests: golden text via `print_ir`/`parse_ir`, `verify_ir(level=1)`
   after each step, interpreter differential where semantics could change.
