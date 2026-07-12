# Concepts

The full rationale for everything here is in [`design.md`](design.md);
section references below point into it.

## Statements and operands (§3.1, §3.2)

A function body is a flat table of statements. Each statement is a row:
a `Kind` (16-bit, from the namespaced registry), a packed operand word, and
core columns (`type`, `flag`, `debug`, `region`). A statement produces zero
or one result; a `StmtId` is both the statement's identity and — in the
dense layout — its position. Multi-value operations produce one tuple-typed
value destructured by `extract` (1-based; `getfield` semantics).

Operands live in one shared **tagged pool**: an operand word is an SSA
reference (`STMT`), an interned constant (`CONST` — egal-deduplicated,
`===`-keyed), a small inline immediate (`INLINE`), a global (`GLOBAL`), a
static parameter (`SPARAM`), or a structural reference (`REGION`, `BLOCK`).
Statements with ≤2 operands of the right shape use an inline encoding and
cost zero pool words — `extract` is the motivating case.

The same row/pool storage core (`AttrGraph`) also stores syntax trees: a
tree is the substrate viewed through the node-reference projection of the
operand list (§3.7). `SyntaxTree` is an alias of the generic cursor.

## Regions (§3.3, §5)

Control structure is expressed by **regions**: contiguous, properly nested
statement spans owned by region-owning statements (`if`, `loop`, `try`,
`cfg`). Region inputs are leading `region_arg` statements; region outputs
flow through terminators *inside* the region — `result` (feeds the owner's
value), `continue`/`break` (loop backedge/exit, carrying values), `return`,
and cross-island `goto`. Terminators name their target region explicitly,
so multi-level exits are first-class rather than encoded in jump spaghetti.

Unstructured control flow is not outlawed — it is contained: a `cfg`
statement owns an island of basic blocks with block arguments, and its
exits are **sealed** (§5.9): the only way values leave is through declared
exit-value lists. Structured ops and islands compose freely in one body;
passes that only understand one of the two can treat the other as opaque.

**Visibility** (§5.5) replaces block-local dominance: a use may see a def
if it is earlier in the same region, in an enclosing region before the
owner, or — inside an island — dominating within the island's own blocks.
The verifier (`verify_ir(ir; level = 1)`) checks it.

## Layout states and the two renaming points (§2.2, §4)

A body is always in exactly one layout state:

| state | order | for |
|---|---|---|
| **dense** | position = order | analysis, pattern-matching, column writes, tombstone deletion, queued use-rewrites |
| **editable** | linked list | insertion/removal, structural surgery (`wrap_in_if!`, `inline_region!`, `splice_body!`) |
| **floating** | none (operand edges + dialect ordering rules) | dataflow dialects, scheduling; an AST is a floating body |

`StmtId`s are stable through all of that — renaming happens at exactly two
points: **`compact!`** (dense repack: drops tombstones, renumbers) and
**`schedule!`** (floating → dense: topological order assignment ending in a
compaction). Both return a `RemapSet`; everything that holds statement ids
across a renaming point must translate through it or re-derive. There is no
third place where ids change.

## Kinds and dialects (§3.4)

`Kind` is one 16-bit type shared from the tokenizer to the optimizer. The
registry splits it into dialect id | opcode; dialects claim contiguous
opcode blocks at registration (`register_dialect!`, `register_kind!`).
Identity is symbolic (`dialect.opname`) — numeric kinds are session-local,
except for the statically reserved bootstrap dialects (`core`,
`JuliaSyntax`, `JuliaLowering`, formatter) whose `K"..."` literals are
compile-time constants. Kind registration declares result arity, operand
schema (named fields, optional tails, varargs), default effects, and
temporal identity (`is_delay`: never CSE'd, never duplicated). Syntax
`K"call"` and core `K"call"` are distinct kinds sharing one numbering
space.

## Extension columns (§3.5)

Consumers attach their own per-statement data as columns: dense
(`DenseCol`), sparse (Dict-shaped), or an open `Dict{Symbol,Any}` universe
(what syntax trees use). Columns declare a semantic class — `Semantic`
(part of the IR's meaning), `Annotation` (rekeyed at renaming points,
values never rewritten — provenance is this), `Derived` (invalidated
conservatively on mutation) — and participate in `compact!`/`schedule!`
through uniform hooks.

## Cells (§6)

Mutable locals that resist SSA form are explicit **cells** (`cell`,
`cell_set`, `cell_get`, `cell_isdefined`), with undef expressed in the
vocabulary (`throw_undef_if_not`) rather than implicit slot state. The
optimizer promotes cells to SSA at the region tree's join points — plain
defs, if-results (sibling-arm stores), loop region args and break values
(loop-carried state, including values threaded through sealed island
exits), block arguments inside islands, and definedness-as-data for
maybe-undef locals. The §6 completeness argument and its verification
harness (stock-oracle, structured fuzzing, and the dominance-frontier
correspondence check) document why those join points are jointly complete.
A residual cell can exist for exactly the machine-verified v1
representation choices — handler-crossing state (pending the
PhiC/Upsilon-equivalent), gc-preserve tokens (pending the pairing-rule
extension), and closure captures that genuinely must share
(`cell_shared` cells `promote_capture_cells!` refuses: written inside a
lambda, written after a creation site, multi-shot backedge hazards,
maybe-undef) — anything else is, by definition, a bug.
