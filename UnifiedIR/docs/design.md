# UnifiedIR: One IR Data Structure for the Julia Compiler and External Compilers

*Status: implementation specification, July 2026. The immediate target is the
P0 vertical slice of §12; §13 records decision status and open questions.
"v1" throughout means the initial implementation scope, not a document
revision. UnifiedIR is intended to eventually subsume `CodeInfo`, `IRCode`,
SynchCompiler's `DNode`/`INode`/`IFunction`, and IRStructurizer's
`StructuredIRCode` as forms of one data structure (design lineage:
DAECompiler's `design/predicated.md`, Appendix A).*

---

## 1. Motivation and goals

### 1.1 The problem inventory

Julia's compiler IR data structures work well for what Base needs, but poorly as
general compiler infrastructure. The evidence, with receipts:

**Position-encoded SSA.** In `IRCode`, a statement's number is its array index.
Identity, execution order, and storage location are the same integer, so any
insertion renumbers everything downstream. `IncrementalCompact` exists to defer
that renumbering, at the cost of three SSA namespaces (`SSAValue`,
`OldSSAValue`, `NewSSAValue`) whose meaning depends on sign and compaction phase
(the `NewSSAValue` docstring at `Compiler/src/ssair/ir.jl:287` calls this out as
refactor debt), three separate pending-node buffers, a `late_fixup` queue, and
refcount bookkeeping with a debug "oracle" checker. Known consequences: node
insertion "does not maintain ordering among the new nodes" (`ir.jl:1109` TODO),
re-iterating an `IncrementalCompact` silently corrupts IR (#46945), `compact!`
emits invalid IR when blocks are out of dominance order (#53011/#53013), and
dead `Union{}` phis survive cleanup (#53640).

**Second-class CFG mutation.** There is no split-block or insert-block API. Edge
helpers (`cfg_insert_edge!`/`cfg_delete_edge!`) do not maintain terminators,
phis, or dominance. CFG transforms are gated behind
`allow_cfg_transforms=true`. Implicit fallthrough terminators make every
end-of-block insertion a special case (#41476). The verifier openly concedes
defeat at `verify.jl:246`: extra statements are tolerated in `:enter` blocks
"until we can do proper CFG manipulations during compaction". Better DCE has
been blocked for years on "a better representation of the CFG and the ability
to update the domtree" (#27547).

**Phi nodes.** `PhiNode` is the one place in the IR where a use appears
lexically before its def, which complicates def-use reasoning everywhere;
#31603 proposes moving phi uses into predecessors (basic-block arguments) and
notes this could subsume the `Argument` type. Redundant `PiNode`s and dead
blocks accumulate from union splitting (#54762); SROA gives inserted phis
declared field types instead of recomputed joins (#50285).

**Exception handling.** The `PhiCNode`/`UpsilonNode`/`EnterNode`/`:leave`/
`:pop_exception` encoding — with an *invisible* CFG edge into catch blocks — is
the most intricate part of the IR. Because catch blocks have no dominating
defs, every value live across the try boundary needs an Upsilon store, even in
functions with no SSA values at all (#34229).

**Two IRs for one pipeline.** Slot-based `CodeInfo` (lowering, inference) and
SSA `IRCode` (optimization) require conversion machinery (`convert_to_ircode!`,
`slot2ssa`, `inflate_ir`), and `slot2ssa` re-implements compaction-shaped logic
separately. The C runtime defines the node types (`jltypes.c`), so extending
the vocabulary means touching C.

**Closed schema.** `InstructionStream` has exactly five columns
(`stmt`/`type`/`info`/`line`/`flag`). External consumers can attach data only by
piggy-backing on `CallInfo` or by maintaining parallel side tables that must be
manually re-indexed after every compaction — DAECompiler does this across
~106 literal `insert_node!`/`compact!` call sites (400+ mentions of the
family).

**External-consumer instability.** The `Compiler` stdlib ships as a loadable
package but exports no stable surface; consumers reach in by qualified name.
SynchCompiler carries dozens of `@static if VERSION` branches tracking churn in
`abstract_call_known`, `matching_cache_argtypes`, `run_passes_ipo_safe`, the
`:invoke` operand shape, and more. DAECompiler pirate-forwards `Base.iterate`
etc. onto compiler types (`compiler_reexports.jl`: "This really needs to go
into a uniform compiler stdlib"). Ordinary package method definitions
invalidate swaths of compiler code (#61510); defining a custom
`AbstractInterpreter` does too (#58000). Even naming is inconsistent
(`stmt`/`inst`/`node`, #49057).

**What external compilers actually need** (from SynchJulia/SynchCompiler and
DAECompiler):

1. Typed, statically-known statement kinds — not `head::Symbol` +
   `args::Vector{Any}` with a hand-rolled field-name table.
2. One substrate spanning equation/dataflow form *and* structured imperative
   form (SynchCompiler today maintains two unrelated tree IRs plus validators
   policing which heads may appear where).
3. Structured control flow with block/region arguments as a native form —
   SynchCompiler depends on the external IRStructurizer package to reconstruct
   structure from goto+phi IR, and its phi→block-argument matching is its
   documented sore spot.
4. First-class opaque intrinsics that survive optimization. SynchCompiler's
   custom `AbstractInterpreter` exists mostly to keep chosen calls visible
   (`NoCallInfo`), isolate a cache, force inlining, and disable semi-concrete
   eval — all workarounds for missing IR-level extension points.
5. Extensible per-statement metadata (type, clock lattice terms, initialization
   lattice, provenance) that rides through stock passes.
6. Structured control flow as a *codegen target* (C and Julia source emission,
   not only LLVM).
7. A state/memory-cell notion (SynchJulia's `getstate`/`setstate`, `pre`).

### 1.2 Goals

1. **One IR, many forms.** Lowering output, inference subject, optimizer
   subject, equation systems, and structured codegen input are *states and
   dialects of one data structure*, with explicit, verified transitions. We
   want to simplify — the end state deletes more IR formats than it adds.
2. **External reusability as a design constraint, not a hope.** A compiler like
   SynchCompiler or DAECompiler builds on the surface with zero
   version-conditional code and zero bespoke `AbstractInterpreter` boilerplate
   for the common cases.
3. **Testability without Julia semantics.** A trivial test dialect plus a
   round-trippable textual format make the data structure and every generic
   pass testable in isolation, LLVM-style.
4. **Keep what works.** The flat, dense, struct-of-arrays statement list is a
   strength of the current design and remains the default representation. So
   does the two-phase mutate-then-compact discipline — reformed, not replaced.
5. **Existence precedes stability.** Nothing here is gated on API freezes:
   Julia has lived with unstable compiler APIs for twenty years, and UnifiedIR
   may remain explicitly unstable for a while even after it merges. Tier
   labels (§8.4) record *intended eventual* stability surfaces so churn is
   deliberate, not promised.

### 1.3 Non-goals (for now)

- Replacing the runtime's `CodeInfo`-based execution/serialization boundary.
  Conversion at the edges is the contract until very late phases (§10, §12).
- Changing Julia language semantics. The IR must express exactly today's
  semantics (including `@goto` — in *and out* of any construct where it is
  legal today, see §5.9 — exceptions, world ages).
- Multi-threaded mutation of a single function's IR.
- Promoting exception-crossing values to pure SSA (§6): v1 deliberately leaves
  them in memory form, as today's compiler effectively does.

---

## 2. Design overview

### 2.1 The one-tree observation (stated precisely)

Four tree-shaped structures that today live in four different places share one
skeleton:

1. the **predicate tree** of predicated.md (the parent of a predication is the
   predication of its introducing condition);
2. the **region nesting** of structured control flow (which `if`-arm/`loop`
   body a statement belongs to);
3. the **dominance scaffold** of structured code;
4. SynchJulia's **clock hierarchy** (`CBase` at the root, each
   `COn(parent, cond, polarity)` a child).

UnifiedIR stores this skeleton once — a `region` column on a flat statement
table plus a small region table — and derives the rest, with the following
precise relationships (they are projections, not identities):

- **Dominance** is *region-ancestry + order* (§5.1), read off the structure
  with no computed domtree for structured code. It **under-approximates** full
  dominance: after an `if` whose else-arm always diverges, today's domtree
  knows then-arm defs dominate the continuation; the region rule requires
  threading them through the `if`'s results. The gap is exactly
  (LCSSA + closed-arm SSA), is sound by construction, and is largely
  recoverable by an *arm-continuation canonicalization* (when one arm
  diverges, hoist the surviving arm's tail into the parent). Post-dominance is
  a genuine analysis, not a tree read-off (§11.1).
- **Clock identity vs region identity**: in floating state the identification
  is exact — `assign_regions!` canonicalizes, one region per canonical clock.
  In dense state region identity *refines* clock identity: two disjoint
  same-guard blocks are distinct regions (correctly — dense mode carries
  side-effect ordering), projecting onto the same clock in the guard view.
- **Activation frames**: the tree reading of "enclosing loop/handler" applies
  only within one activation. Structural role and activation are orthogonal
  region dimensions (§3.3); `deferred` regions (closure bodies) and
  edge-defined `resume` subgraphs (continuations) are *activation boundaries*
  across which exit-targeting and handler-attachment do not reach (§5.1).
- **The predicated form** of predicated.md is a bijective *view* of structured
  code (print each statement with its guard path) and the *native* form of the
  effect-free equation dialect.

Where predicated.md ran into trouble — loops as self-referential ω-nodes — the
region model supplies the fix: cyclic constructs become explicit ops with
carried values, so the use-def graph is acyclic everywhere (§5.3).

### 2.2 Layout states

The IR is one logical structure with several layout states, and every API
declares which state it needs.

- **Dense** (default): flat statement vector; *position is the order*; id =
  position; zero per-statement ordering overhead. What today's well-behaved
  passes do without `IncrementalCompact` — iterate, pattern-match, replace in
  footprint, tombstone, batched renames — works here, fast. A fresh function
  is constructed in an *unsealed builder* sub-state (append is the primitive,
  regions are explicitly opened/closed); `finish!` seals it and runs L0.
- **Editable**: an explicit `editable(ir)` expansion materializes an ordering
  structure, enabling insert-anywhere and region surgery. `compact!`
  transitions back to dense, returning a `RemapSet`.
- **Floating**: no order at all. The layout state of the equation/dataflow
  dialect. Scheduling is the floating→dense transition.

There are exactly **two renaming points** in the system: `compact!`, and
`schedule!` — which is *defined* as order assignment followed by a compaction
and likewise returns its `RemapSet`. No pass ever renames anything.

Precedent: Cranelift separates instruction data (arena keyed by stable id) from
`Layout` (a separate order structure); IRCode + IncrementalCompact is already a
two-representation design — its mistake is that the second representation is a
*streaming transition* whose intermediate state leaks (tri-namespace,
re-iteration corruption) rather than a real state with stable names.

### 2.3 The identity principle

**Ids are names; position is layout.** Dense state is the special case where
they coincide. Pass-facing APIs (`comes_before`, `replace_uses!`, iteration)
are written once against ids and work in every state. Ids are wrapped in
zero-cost typed structs at every API boundary (`StmtId`, `RegionId` — never
bare integers, so references cannot be confused with immediates or each
other); debug builds carry and check a generation tag on handles. Production
builds do not pay for per-dereference checking — stale handles after a
renaming point are a contract violation (§11.1), caught by debug builds and
compaction fuzzing, the same position Cranelift and LLVM take.

---

## 3. Core data structure

### 3.1 The statement table

```julia
struct StmtId;   id::Int32; end   # zero-cost wrapper; = position in Dense state
struct RegionId; id::Int32; end   # index into the region table; 1 = function body
const  Value = StmtId             # an SSA reference. Statements have zero or one
                                  # result (kinds declare which); references to a
                                  # zero-result statement are rejected at L0.
                                  # Multi-value ops produce one tuple-typed value
                                  # (§5.1); a future representational variant may
                                  # add native multi-result behind the accessors.
const  Kind = UInt16              # namespaced: dialect id bits | opcode bits
                                  # (split is a registry parameter, §3.4)

mutable struct IRBody{Cols}
    len::Int32
    # ---- core columns, StmtId-indexed (struct of arrays) ----
    kind::Vector{Kind}              # K"deleted" marks a tombstone
    ops::Vector{UInt64}             # two-mode word (§3.2): pool range
                                    #   (offset:39, len:24) or inline operands
                                    #   (mode bit | imm:24 | stmt:32)
    type::Vector{Any}               # lattice element (tuple type for multi-value ops)
    flag::Vector{UInt32}            # effect/analysis bits (IR_FLAG_* successor)
    debug::Vector{NTuple{3,Int32}}  # Core.DebugInfo codeloc triple per stmt
    region::Vector{RegionId}
    # ---- shared pools (append-only within a generation) ----
    operands::Vector{Operand}       # tagged 64-bit words, §3.2
    constants::Vector{Any}          # constant pool (egal-interned, §13.8)
    globals::Vector{GlobalRef}      # deduped (Module, Symbol) table (GLOBAL tag)
    # ---- open extension columns, §3.5 ----
    cols::Cols                      # the universe's column set (§3.5): a static
                                    #   NamedTuple parameter fixed per consumer;
                                    #   a Dict-backed dynamic container is one
                                    #   legal parameter value (JuliaLowering)
end
```

The core column vectors are internal: all mutation goes through the API (§4),
which is what lets epochs (§11.1), footprint checks (§4.1), and the reference
protocol (§3.2) actually hold.

**The handle model**: an `IR` value is a thin
handle around one shared **body owner** that holds the layout-state tag, the
generation, and mutation ownership. Every public operation checks the state
tag — one O(1) compare per *operation*, not per dereference — so a stale dense
alias fails deterministically once an `editable` session opens, and pass-local
column views carry the same owner token. Debug builds use a fattened row
cursor (`StmtRef` = owner + generation + index) behind the same accessors and
check the generation on dereference; release builds erase it. A violated
release contract may silently alias — that is the accepted, documented risk
(§2.3, question #10).

The containing `IR` type adds: the region table (§3.3), `argtypes`, `sptypes`,
`debuginfo` (compressed `DebugInfo` stream), an optional provenance graph
(§3.6), `valid_worlds`, the analysis cache (§11), a `generation::UInt32`
(bumped at each renaming point; checked against handles in debug builds), and
the layout-state tag (§4).

### 3.2 Operands: tagged words into a shared pool

```
struct Operand; bits::UInt64; end        # 4-bit tag | 60-bit payload

STMT    payload = StmtId                 # SSA use — the common case
BLOCK   payload = RegionId of a block    # branch target; only inside `cfg`
REGION  payload = RegionId               # general region reference; role fixed by
                                         #   the op schema (exit target for break/
                                         #   continue/return/goto; base-clock ref
                                         #   for synch.instantiate; …)
CONST   payload = constant-pool index    # any boxed Julia value
INLINE  payload = small immediate (Int/Bool/UInt8/…)
GLOBAL  payload = globals-table index
SPARAM  payload = static-parameter index
NONE    undef slot
```

(There is deliberately no `ARG` tag: function parameters are region 1's
`region_arg` statements, referenced via `STMT` like everything else — one
identity path.)

A statement is `(kind, type, flag, operands…)`. Rationale for flattening
(vs. keeping boxed `Expr`-like nodes):

- **Uniform operand iteration.** `ir.jl`'s ~180-line `UseRef`/`userefs`
  machinery exists because operands live in five heterogeneous shapes and
  immutable nodes must be *reconstructed* to replace one operand. Here operand
  iteration is a loop over machine words and operand replacement is an integer
  store.
- **Renaming is mechanical.** `compact!` and rename tables rewrite operands
  without knowing kinds — **provided all references live in the protocol**
  (next paragraph).
- **Allocation profile.** No per-statement `Expr` + `args` vector; constants
  boxed once in the pool.
- **Def-use for free.** A use site is a small tagged sum, `UseSite`:
  `StmtOperand(user::StmtId, opidx::Int32)` — the overwhelmingly common case,
  porting the compressed `TwoPhaseDefUseMap` layout directly — or
  `GuardCondition(region::RegionId)` for ownerless guard regions (§3.3), the
  one non-statement use site in the system.

**The reference protocol.** Every semantic reference in the whole structure —
operand words, the region table's fields, extension columns whose values embed
ids, escape-hatch constants — participates in one mandatory
`visit_refs`/`remap_refs` protocol, and **every visited reference carries a
role**: `ssa_use` (operands; ownerless-guard conditions), `owner_link`
(region→owner), `arg_def` (region→its region_arg statements), `control_target`
(exit/branch targets), `layout_anchor` (region spans). Remapping consumes all
roles; use counts, liveness, and DCE consume exactly the `ssa_use` role — the
role tags are what let one generic visitor serve both without conflating "is
referenced by the structure" with "is used as a value".
Renaming points produce a `RemapSet` covering *all* compactable namespaces
(statements, regions, constants, globals), and everything reachable through
`visit_refs` is rewritten. The `K"value"` escape kind is restricted to
**reference-free leaf objects** — a payload that embeds statement/region
references requires a registered codec implementing `visit_refs`, and fails L0
verification without one.

**Inline operand encoding for high-volume small kinds.** The per-statement
`ops` word is a two-mode union discriminated by its top bit: pool mode
`[0 | offset:39 | len:24]` (a range into the operand pool, the general case),
or inline mode `[1 | spare:7 | imm:24 | stmt:32]` — the operands stored in the
word itself. A kind may declare inline encoding iff its schema is **exactly
one `STMT` operand plus at most one raw immediate** — never a `CONST`,
`REGION`, `BLOCK`, or `GLOBAL`, whose indices belong to other remap namespaces
and would force those remappers to consult kinds. The fixed reference shape is
what keeps the mechanical renamer kind-agnostic: it checks one bit and either
walks the pool range or remaps the low 32 bits, never touching `imm`.
Declared in `@stmtkind`, checked at registration and L0; invisible behind the
generated accessors. Core kinds using it: **`extract`** (the motivating case —
loop/if destructuring produces many, and Julia-dialect
`getfield(tuple, const i)` canonicalizes to it), `cell_get`,
`cell_isdefined`, `gc_preserve_end`. Each inline `extract` saves two pool
words and an indirection, and its rewrites can never orphan pool slots.

**Variable-length successor bundles** (§5.5) have a defined physical encoding:
length-prefixed groups in the operand range — `BLOCK dest, INLINE argc,
args…` per bundle; `switch` is `INLINE ncases`, then per-case `CONST caseval`
+ bundle, then the default bundle. Lengths and case values are
`INLINE`/`CONST` words, so the mechanical renamer remains a pure tagged-word
rewrite, and `@stmtkind`'s generated schema drives iteration, arity checking,
and printing.

Mitigations for the flattening-ergonomics cost: generated per-kind accessors
and a destructuring macro (§3.4), an `@stmt` builder macro, lossless
`to_expr`/`from_expr!` at the `CodeInfo` boundary, and the (restricted)
`K"value"` escape kind so anything not yet modeled still round-trips.

Same-or-smaller-arity in-place rewrites reuse the operand range; growth appends
a fresh range and orphans the old slots (reclaimed by `compact!`, counted
toward the compaction trigger).

### 3.3 The region table

```julia
# Structural role and activation are ORTHOGONAL dimensions (a single enum
# cannot express, e.g., a resumption entry that is also a cfg `block`, or a
# handler executing inside a deferred activation).
@enum RegionKind body arm guard loop_body handler block
@enum Activation immediate deferred resume

struct Region
    kind::RegionKind       # `guard` = ownerless activation-condition region
                           #   (floating dialect, §7): no owner op, no
                           #   terminator; `arm` = an owned, terminated arm
    activation::Activation # activation boundary property (§5.1); `immediate`
                           #   for everything in the v1 core — `deferred`
                           #   enters with closures (P3); for cfg-form await,
                           #   activation is a property of the RESUME EDGE and
                           #   its reachable subgraph (§5.6), not a lexical
                           #   region bit
    owner::StmtId          # owning op; 0 for region 1 and for `guard` regions
    parent::RegionId       # the tree edge: guard/clock/dominance scaffold
    args::Vector{StmtId}   # region-argument stmts (loop-carried, exception, block args)
    cond::Value            # STORED ONLY for `guard` regions (owner == 0),
    negated::Bool          #   where it is authoritative and a real `ssa_use`.
                           #   Owned arms have NO stored condition — the guard
                           #   is derived from the owner op's operand, so there
                           #   is exactly one source of truth and a footprint-
                           #   preserving operand rewrite cannot desync it.
    first::StmtId          # dense state: contiguous span [first, last]
    last::StmtId           #   (editable state: list head/tail instead)
end
```

All id-bearing fields participate in the §3.2 reference protocol with their
roles: `cond` is an `ssa_use` (guard regions only); `owner` is an
`owner_link`; `args` are `arg_def`s; spans are `layout_anchor`s.

**Activation boundaries** are the fix for the deepest v1 review finding:
lexical nesting alone cannot express that a closure body or continuation
executes in a *different activation*. `immediate` regions execute within
their owner's activation; **`deferred`** (closure bodies) and **`resume`**
(continuation entry, edge-defined for cfg `await`) are *activation boundaries*
with these rules (verified at L1):

- Exit terminators (`break`/`continue`/`return`/`goto`) may not target across
  an activation boundary — a `break` inside a closure cannot target a
  lexically enclosing loop.
- Throw edges bind to the dynamically enclosing handler of the *executing*
  activation: a deferred body does not attach to the creation-site `try`.
- Effect composition for DCE: an op's removability counts the effects of its
  immediate-mode regions; deferred-mode region effects do not count at the
  creation site (they surface at call sites). This resolves the asymmetry
  that an unused closure with an effectful body is deletable while an
  unused-result `if` with an effectful arm is not.
- References from inside a deferred/resume activation to outside values are
  *captures*, not ordinary uses (§5.1).

**v1 seals region ownership**: only core kinds (`if`, `loop`, `try`, `cfg`,
and — the implemented v1 slice, §5.7 — `closure`) own regions. (`await` is *not* a region
owner: its cfg form is a block terminator, §5.6.) External dialects
may declare `guard`-kind regions via registration — which is all SynchJulia's
`:clocked` needs — but may not introduce new region-owning ops with novel
execution semantics. A descriptor mechanism for foreign region owners
(activation, capture, exit legality, result feeding, effect composition,
traversal) is future work; until it exists, unknown ops never own regions, so
the question of passes mis-traversing them cannot arise.

**Verifier applicability is keyed by (layout state, region kind)**: the
one-terminator rule is an L0 rule applying to *ordered* regions of kinds
`body`/`arm`/`loop_body`/`handler`/`block`; `guard` regions (floating) have no
terminator and no order (§11.2).

Region arguments are ordinary statements (`K"region_arg"`) occupying the
leading positions of their region's span (an L0 rule), so **everything an
operand can reference is a statement**. Function parameters are region 1's
args — the `Argument` type disappears, completing #31603's observation.

### 3.4 Kinds: a namespaced registry

`Kind` reuses the JuliaSyntax registration *mechanism* — dialects claim
opcode ranges at load time (`register_dialect!`) — with two corrections
from review:

- **Numeric kinds are session-local; identity is symbolic.** A dialect has a
  stable name (and UUID); the dialect-id bits are assigned per session in
  registration order. Every *persistent* form — pkgimages, native
  serialization (P4), the textual format — records symbolic
  `dialect.opname` identities and relocates to session numbering on load.
  Raw `Kind` integers never escape a session. (JuliaSyntax today hardcodes
  its three module ids; that does not scale to an open dialect ecosystem.)
- **The bit split is a registry parameter.** JuliaSyntax's 6/10 split allows
  64 modules — far too few if syntax and IR dialects ever share a numbering
  space (§3.7). The registry owns the split and can widen it without
  representation changes elsewhere; capacity exhaustion is a defined error.
- **Kind literals in precompiled code**: symbolic
  serialization is not enough if `s.kind === K"synch.delay"` bakes a session
  number into a pkgimage. Mechanism: for the **sealed core dialect**, `K"…"`
  is a true compile-time constant (fixed dialect id 0 — the hot paths pay
  nothing). For **external dialects**, `K"…"` in package code lowers to a read
  of a per-kind registry binding populated at `register_dialect!` time — a
  load instead of an immediate, constant-foldable only within a session; a
  deterministic-numbering optimization can come later if the load ever shows
  up in profiles. The registry is published as an immutable snapshot per
  compilation (no torn reads under concurrent package loading), and an IR
  instance records the snapshot it was built against.

The core dialect is sealed; `K"synch.delay"`, `K"dae.equation"`, GPU ops etc.
are ordinary registered kinds. Each kind declares its result arity (0, 1, or —
for region-owning core ops — instance-determined 0/1: present iff exits feed
values), operand schema, and effects.

```julia
@stmtkind K"call"      (callee, args...)
@stmtkind K"invoke"    (codeinst::CONST, callee, args...)
@stmtkind K"br_if"     (cond, then_edge::EDGE, else_edge::EDGE)   # cfg only; §5.5
@stmtkind K"synch.delay" (expr, init; reset)        # external registration
```

`@stmtkind` generates: typed accessors (`callee(ir, s)` / `s.cond`), arity and
tag checks used by the structural verifier, printer/parser hooks, and a
`@match_stmt` destructuring case. This subsumes SynchJulia's
`irexpr_fields` head→field table (`src/irbase.jl`) — the same idea, typed and
shared. (Whether accessor generation introduces invalidation-relevant methods
is measured, not assumed — §8.1's CI gate.)

### 3.5 Extension columns: static universes + dispatch

Extension across consumers does not require dynamism within a consumer — and
Julia already has a mechanism for attaching behavior to types. Hence:

**The column set is a static type parameter — a *universe* — declared once
per consumer at package top level:**

```julia
# Each consumer fixes its column set up front. Concrete NamedTuple of column
# containers; every pass in that consumer's pipeline takes this type.
const CoreIR  = IR{@cols(callinfo::SparseCol{CallInfo})}
const SynchIR = IR{@cols(clock::DenseCol{ClockTerm},
                         init::DenseCol{InitLattice})}
const DAEIR   = IR{@cols(incidence::DenseCol{Incidence})}

function clock_inference!(ir::SynchIR)
    for s in stmts(ir)
        s[:clock] = ...        # direct, fully concrete column access
    end
end
```

The set is fixed for a pipeline: there is no mid-flight column addition, no
column views. This makes column desync unrepresentable — there is exactly one
type per pipeline, so mutation machinery cannot run against a handle that is
missing columns — and
`compact!`/`splice_body!` iterate the NamedTuple statically: fully unrolled,
type-stable, faster than any runtime registry. It is also `InstructionStream`'s
virtue kept (a fixed, known, fast column set) with the fix external consumers
actually needed: the fixed set is *per consumer*, not universal. And it is
SyntaxGraph's actual design — Dict-mode and frozen-NamedTuple-mode already
exist there; a **dynamic, Dict-backed column container is simply one legal
parameter value**, used by JuliaLowering, whose macro-expansion-era attributes
are genuinely open (passes invent attributes as they go).

**Column semantics attach by multiple dispatch on the column type** — Julia's
extension idiom, not a parallel descriptor system:

```julia
UnifiedIR.hasrefs(::Type{IncidenceCol}) = true            # values embed StmtIds
UnifiedIR.remap_refs!(col::IncidenceCol, rs::RemapSet) = ...
UnifiedIR.semclass(::Type{CallInfoCol}) = Derived()
UnifiedIR.on_splice!(col::ProvenanceCol, ...) = ...       # opt-in refinement
```

with safe defaults so trivial columns define nothing: `hasrefs = false` (no-op
remap — a column whose values embed ids *must* override both, a contract the
compaction fuzzers' id-perturbation exists to catch), `semclass = Derived()`,
and class-driven conservative behavior. The classes: `Semantic()` (part of
program meaning — a dialect's clock column), `Annotation()` (durable,
meaning-preserving — provenance), `Derived()` (analysis results — `CallInfo`,
incidence). **The conservative default is column-wide invalidation** of
`Derived` columns on any relevant statement/type/flag/region/layout event
(analyses are nonlocal — one operand change can stale facts on many unchanged
rows); row-scoped or event-refined behavior is opt-in via more methods. Row
survival is never meaning survival: a `replace_stmt!` that kept a stale
`CallInfo` on a different call was the original unsoundness. What *is*
automatic for all classes: structural bookkeeping — permutation at compaction,
defaults on insertion, reference remapping through `hasrefs`/`remap_refs!`.

**Why dispatch is safe here** (and where it wouldn't be): the invalidation
hazard behind #61510/#58000 is *abstractly-typed dispatch sites in precompiled
code whose method tables grow with the ecosystem* — not method definition per
se. Universes make every hook call in a specialized pass **concrete**:
monomorphized at precompile time, no dynamic dispatch, and a new column type
elsewhere cannot intersect `remap_refs!(::IncidenceCol, ::RemapSet)`, so
nothing invalidates — a new universe produces fresh specializations in the
*consumer's* pkgimage and touches nothing in the sysimage. The deliberately
dynamic case — the Dict-backed container's hook calls — is cold by design and
its hook generic functions carry `@max_methods 1`-style protection so
world-splitting over a growing method table cannot invalidate their callers.
(§8.1 states the resulting rule.)

Consequences of the universe model:

- **Naming/namespacing is ordinary Julia namespacing** — a universe is a type
  owned by a module; no schema registry is needed.
- **Universes are declared at package top level and precompiled** — dynamic
  universe creation post-sysimage would force runtime pass compilation; the
  discipline rule makes specialization a precompile-time event.
- **Cross-universe operations are explicit**: `convert_universe(TargetU, ir)`
  maps, defaults, or — only when explicitly listed — drops columns, checked at
  compile time; `splice_body!` across universes **requires** it (a hard error
  otherwise — silent column dropping is a quiet-staleness bug class this
  design forbids).
- **Conditional metadata declares the superset**: a sparse column that is
  usually empty costs approximately nothing, so e.g. provenance can be
  declared universe-wide and populated only when lowering ran with it.
- **Pass-local scratch is not a column**: transient per-stmt state lives in
  ordinary side tables keyed by `StmtId` (with `RemapSet` subscription if it
  outlives a renaming point). Columns are for metadata that crosses passes.
- **Post-hoc annotation of an IR you don't own** (tooling attaching remarks to
  core-produced IR) is a side table or a `convert_universe` — not a column
  injection into someone else's universe.

`CallInfo` is an ordinary `Derived()`-class column of the *core universe* —
the core eats its own dog food; there is nothing special about Base's metadata
versus SynchJulia's `clock` or DAECompiler's incidence lattice, except which
universe declares it.

### 3.6 Provenance and debug info

Two tiers, both columns:

- `debug` keeps today's compressed `DebugInfo` codeloc encoding —
  the C runtime consumers and `Core.DebugInfo` are untouched. `splice_body!`
  performs the inlined-at chaining today done by `ir_inline_linetable!`
  (this is the *debug* tier's merge, distinct from the provenance tier).
- An optional sparse `provenance` column (`:annotation` class) holds
  **graph-qualified** node references — `(GraphId, NodeId)`, since inlining
  combines provenance from more than one source graph — pointing into the
  graph(s) JuliaLowering produced, giving expression-level provenance
  (#31162) — `sourceref`, macro-expansion chains — on IR statements with no
  conversion. Statement builders accept `src = <stmt|SyntaxTree>` to inherit
  or set both tiers. The cross-graph lifetime contract — whether referenced
  syntax graphs are frozen, rooted, or independently compacted with a
  node-remap subscription — is part of the Level-2 decision (question #9;
  P3-gated).

### 3.7 The AST is the same substrate: SyntaxGraph and Level 2

JuliaSyntax's `SyntaxGraph` is

```julia
mutable struct SyntaxGraph{Attrs}
    const edge_ranges::Vector{UnitRange{Int}}   # per-node range into `edges`
    const edges::Vector{NodeId}                  # shared child pool
    const attributes::Attrs                      # open columns (Dict | frozen NamedTuple)
end
```

— structurally isomorphic to the UnifiedIR statement table: node table +
per-node range into a shared edge pool + open attribute columns, with the same
`Kind` registry mechanism. The differences: AST edges are untagged `NodeId`s
(a plain node-reference tag case of the pool word in the shared substrate),
`kind` is an attribute rather than a core column, and there are no
`region`/`type`/`flag` columns.

**Level 1 — one node model, one porcelain.** One `AttrGraph` foundation (row
table: kind column + packed operand words over one tagged pool + parametric
column sets (§3.5)): SyntaxGraph = AttrGraph + tree conventions; UnifiedIR =
AttrGraph + core columns + regions + layout states. Storage sharing alone,
however, just produces bolted-together twins. The unifying principle is:

> A node is `kind + tagged operands + attribute columns`. A **tree is not a
> second data structure** — it is the substrate viewed through the
> node-reference projection of the operand list: `children(g, id)` = the
> node-tagged operands. A tree is the special case of IR where every value
> has exactly one use and containment coincides with use — i.e. exactly the
> floating layout state before scheduling. An AST *is* a floating body in a
> dialect where that special case always holds (every operand a node ref, no
> regions, no order); lowering is the progressive introduction of ordering
> and regions into the same rows.

Everything either consumer does with trees is therefore written **once**, at
the substrate, against the projection:

- **cursor** `Tree{G}` = (graph, id): child indexing, kind, attributes as
  properties — `SyntaxTree{Attrs}` is an alias of it;
- **lists** `NodeList{G,V}` (= `SyntaxList`);
- **construction**: `newleaf`/`newnode` (fresh node, provenance recorded) and
  the copy-on-write `mkleaf`/`mknode` (attribute copy, provenance = the old
  node) — the primitives every lowering transformation bottoms out in;
- **traversal & rewriting**: `mapchildren` (alloc-free when unchanged),
  recursive cross-graph `copy_ast`, structural `≈`, generic tree printing;
- **provenance**: the `:source` chain (`provenance`/`prov_end`/`sourceref`
  walk node-id links to a terminal source reference) — the identical chain
  works over IR rows carrying a `:source` column, which is how lowering→IR
  provenance gets zero seams (a Level 2 corollary pulled forward);
- **GC**: `compact!`-as-GC over a root set — something lowering
  independently wants: macro expansion and desugaring grow the graph
  monotonically, and dead intermediate nodes otherwise require ad-hoc
  pruning.

Explicitly *not* unified at Level 1: the kind registries (two numbering
spaces until §3.4-namespaced unification at Level 2 — generic porcelain
treats kind as opaque), source-text machinery (`SourceRef`), and the leaf
payload convention (the AST dialect keeps literal values in columns
(`:value`/`:name_val`); IR interns them CONST-tagged in the constant pool —
both are non-node payload the projection ignores).

**Level 2 — one structure from source to IR.** A single flat arena holding
both syntax nodes and IR statements is ruled out: it cannot coexist with dense
`StmtId == position` (syntax nodes would either consume statement positions or
force gaps). Level 2 instead means:
**shared substrate, two node namespaces** (`NodeId` and `StmtId`, never
unified), graph-qualified provenance joining them, and lowering structured as
in-graph rewriting whose final emission into a dense function body is an
*extraction* — the same machinery as `schedule!`/`compact!` (build unordered,
extract dense). The attractive corollaries survive: provenance with zero
seams, late passes (closure materialization) reading surface structure by
graph walk, the scope-tree→region-tree refinement hypothesis, one
printer/verifier/test stack.

**GO decided 2026-07-10 (review).** With Level 1 + the shared porcelain in
place, Level 2 is a bounded increment — the region-structured lowering
emitter (§10.1 port) already embodies the extraction architecture (the
lowering passes rewrite in-graph on the shared substrate; the emitter walks
the lowered tree and extracts a region-structured dense body). What remains,
in order:

1. **One kind registry.** The §3.4 registry becomes THE registry.
   JuliaSyntax's `_register_kinds!` re-plumbs into `register_dialect!`: its
   kind modules (JuliaSyntax, JuliaLowering, formatter extensions) become
   dialects claiming contiguous opcode blocks through the shared instance, so
   `parentmodule`/range predicates (`is_literal` etc.) keep their contiguity
   assumptions per dialect. One `Kind` type end to end; one `@K_str` with
   per-package default-dialect search paths (syntax `K"call"` and core
   `K"call"` remain *distinct kinds sharing one numbering space* — the
   emitter's dialect mapping stays explicit).
2. **Graph-qualified provenance.** The lowering emitter records, per emitted
   statement, a `:source` column holding a *graph-qualified* node reference
   (a tree cursor: graph + id — self-qualifying, and what keeps the AST
   alive); the `debug` line-info column is derived from it at the boundary.
   The generic `provenance` walk then crosses seamlessly: IR statement →
   syntax node → `:source` chain → `SourceRef`, giving IR diagnostics with
   surface-text highlighting for free. IR-to-IR renaming points never rewrite
   `:source` (it is Annotation-class payload into a *different* namespace).
3. **AST lifetime.** The `CodeInstance`-finalization policy prototyped:
   `compact_graph!` of the lowering graph rooted at the union of live IR
   `:source` references (plus module-level roots) — collect syntax nodes no
   surviving provenance mentions.
4. **One printer/verifier/test stack.** Kinds print dialect-qualified from
   the one registry on both syntax nodes and IR statements; the tree printer
   and `print_ir` share the porcelain.

Frictions (design question #9): SyntaxGraph internals would change (tagged
pool words, `Int32` ids, kind as a core column) behind a preserved
`SyntaxTree` view API — release engineering for an independently shipped
package; syntax and IR kinds sharing one registry numbering space (mechanism
now specified, §3.4); AST lifetime under shared provenance (the compact!/prune
policy at `CodeInstance` finalization must collect syntax nodes unreferenced
by surviving provenance).

---

## 4. Layout states and the mutation API

### 4.1 Dense (default)

Position is order; `StmtId` = index; regions occupy contiguous, properly nested
spans, with each region's `region_arg`s in its leading positions (both L0
rules — verifiable in one linear walk, unviolatable in sealed dense state
since it has no insertion). Allowed operations:

| Operation | Notes |
|---|---|
| iterate / pattern-match | sequential array scan |
| `replace_stmt!(ir, id, new)` | same identity, **same footprint**: result arity, terminator-ness, and region-ownership must be preserved (a non-owner cannot become an `if`); structural changes go through editable surgery. Column class behavior fires (§3.5: `Derived` columns conservatively invalidated). |
| column writes (`type`, `flag`, extensions) | through the API; bumps the relevant epoch |
| `delete_stmt!(ir, id)` | tombstone (`K"deleted"`); the moral `nothing`. **Restricted to plain statements**: non-owner, non-terminator, non-`region_arg`, not a guard-region condition — so dense deletion cannot orphan a region or unterminate a body (structural deletion is editable surgery that removes or rewires the owned structure). Uses must be gone by verify time. |
| `replace_uses!(ir, old => new)` | queued rename; flushed in one O(n) sweep at pass end. Semantics: renames compose (targets resolved through the pending map at flush; chains collapse; cycles are an error); reads during the pass observe pre-flush operands. |
| `comes_before(ir, a, b)` | integer compare |

Construction happens in the **unsealed builder** sub-state: `append_stmt!`,
`open_region!`/`close_region!`, then `finish!` (seals, runs L0). A sealed
dense IR has no append (there is no well-defined "trailing open region" in a
completed function).

**Strong exception guarantee**: every multi-component
mutation — replacement and deletion with their column event callbacks,
surgery, `compact!`, `splice_body!` — either completes or leaves the IR
logically unchanged. Concretely: column/event callbacks that can throw run
against a staged delta (or before any core mutation), and `compact!`/surgery
build and verify their replacement before atomically publishing it, so a
failing external callback cannot leave a body with mixed generations. The
acceptance tests assert this postcondition, not merely "doesn't crash".

This covers the numeric majority of passes: canonicalization, refinement, DCE
marking, flag propagation, rewrites. It is exactly today's fast path, kept.

### 4.2 Editable

```julia
eir = editable(ir)          # O(n); consumes the dense handle (epoch-checked)
...mutations...
ir′, remap = compact!(eir)  # O(n); produces a RemapSet over all namespaces
```

`editable` materializes per-region intrusive doubly-linked lists (`next`/`prev`
columns); ids stay exactly what they were — no translation at the boundary.
Additional operations:

- `insert_before!(eir, at, stmt)` / `insert_after!` / `push_stmt!(eir, region,
  stmt)` — anywhere, O(1), no cursor discipline;
- region surgery: `wrap_in_if!(eir, first, last, cond)`, `split_region!`,
  `new_region!`, `wrap_in_loop!` — each a verified, transactional operation
  that keeps the region table, terminators, and result feeding consistent
  (surgery builds and verifies its delta before publishing, so a failure
  cannot leave mixed state). `wrap_in_if!`'s precondition: defs escaping the
  wrapped run require either a diverging `else_arm` or caller-supplied
  else-values — otherwise there is nothing sound to produce (see §6 on undef).
- `splice_body!(eir, at, callee::IR; argmap)` — the library-owned inlining
  primitive: bulk-copies the callee in one remap pass; relocates constants,
  globals, and static parameters; **intersects world-validity ranges**;
  applies column class behavior (§3.5); across universes it **requires** an
  explicit `convert_universe` (hard error otherwise, §3.5); chains both
  provenance tiers (§3.6). Replaces the nested-`IncrementalCompact` trick and
  the block surgery DAECompiler hand-rolls.

**Order comparison in editable state**: source of truth is the list. An
optional order-key accelerator column (list-labeling: midpoint insertion,
local relabel on gap exhaustion — amortized O(log n) against adversarial
insertion, near-O(1) in practice since `compact!` re-spreads keys) provides
fast `comes_before`; keys are never exported or stored in any reference, and
debug builds may perturb them at `compact!` to flush out violators. Honest
complexity note: the fallback (per-pass position snapshots) is O(1) only for
statement pairs that existed at snapshot time; order-query-heavy passes should
either run their query phases on dense state or accept the key column. A
per-state complexity table ships with the API docs.

`compact!` drops tombstones and orphaned operand/constant/global slots,
renumbers densely in region order (restoring contiguity), rewrites every
reference reachable through the §3.2 protocol — operand words, region table,
all registered columns — bumps the generation, and returns the `RemapSet`.

**Iteration-under-mutation contract** (specified, verifier-checkable): deleting
the cursor statement is safe; statements inserted after the cursor are
visited; statements inserted before it are not.

**The paved path for mostly-scan passes** is an `InsertionPlan`: collect
planned insertions/rewrites against the dense IR (positions validated eagerly),
then apply in one `editable → apply-all → compact!` step. This is the honest
version of today's `insert_node!`-then-`compact!` workflow, which is genuinely
good and is kept.

### 4.3 Floating

No order; the `region` column is purely a guard. The verifier replaces
def-before-use with acyclicity-modulo-`delay`. Transitions, with the
review-tightened legality rules:

- `dense → floating` (`float(ir)`): legal iff every statement is
  **reorderable** — `EFFECT_FREE ∧ NOTHROW ∧ TERMINATES` (the `REMOVABLE`
  mask; plain effect-freedom is insufficient — pure operations may throw or
  diverge, and reordering those is observable) — or covered by
  dialect-declared ordering rules; and control structure is guard-only (loops
  must already be recurrences through `delay`).
- `floating → dense` (`schedule!(ir; strategy)`): **layout only** — a
  topological sort over operand edges *plus dialect-registered ordering
  constraints* (how Synch's `invoke_node`, stateful across steps but
  instantaneous within, gets its causality edges), deterministic tie-breaking,
  a diagnostic for every unbreakable cycle (= causality error), ending in a
  compaction that returns the `RemapSet` (scheduling assigns positions, so it
  renames — §2.2). Semantic rewrites — guard reification into `if` ops,
  `merge` into if-results, `delay` into loop-carried state or cells — are
  **separate, explicitly lossy legalization passes** (§7), not part of
  `schedule!`; keeping them separate is deliberate.
- Re-floating a scheduled-but-not-legalized graph is legal under the same
  reorderability precondition — order is discarded and derivable again.
  Legalization is not undone by re-floating.

**Temporal identity**: `delay`-like kinds denote state resources. They are
never CSE-able (two syntactically identical delays are distinct cells) and
never duplicable — encoded in their registered effects, checked by L2.
"Acyclic modulo `delay`" cuts exactly the *delayed data operand* edge; `init`
and `reset` operands are instantaneous dependencies and are **not** cut.

### 4.4 Worked example 1: strength reduction, dense state

```julia
function strength_reduce!(ir::IR)
    for s in stmts(ir)
        s.kind === K"call" || continue
        is_known(ir, s.callee, Base.:*) || continue
        nargs(s) == 2 || continue
        x, c = s.args[1], s.args[2]
        is_const(ir, c, 2) || continue      # second operand is the constant 2
        replace_stmt!(ir, s.id, @stmt K"call"(Base.:+, x, x) type=s[:type] src=s)
        # footprint preserved (1 result, non-owner, non-terminator);
        # Derived-class columns (e.g. callinfo) conservatively invalidated (§3.5).
    end
end
```

No compaction, no renaming, no cursor discipline; identity is preserved, so
every use of the value is untouched. Compare today: the same pass runs inside
`IncrementalCompact`, addressing the value as `OldSSAValue` before the cursor
and `SSAValue` after.

### 4.5 Worked example 2: insert a bounds check, editable state

```julia
function insert_boundscheck!(ir::IR, at::StmtId, arr::Value, i::Value)
    eir = editable(ir)
    len = insert_before!(eir, at, @stmt K"call"(arraylen, arr)   type=Int  src=at)
    ok  = insert_before!(eir, at, @stmt K"call"(sle_int, i, len) type=Bool src=at)
    wrap_in_if!(eir, at, at, ok; else_arm = err -> begin
        push_stmt!(eir, err, @stmt K"call"(throw_boundserror, arr, i) type=Union{} src=at)
        push_stmt!(eir, err, @stmt K"unreachable"())
    end)
    ir′, _remap = compact!(eir)
    return ir′
end
```

Everything is local: no terminator bookkeeping, no phi repair, no renumbering
visible to the pass. `wrap_in_if!` rewrites the region column for the wrapped
run, adds two region-table rows, and — per its contract — rewires visibility:
any value defined in the wrapped run and used *after* the `if` becomes an
if-result (a `result` terminator in the then-arm, uses rewritten to the result). Here the
else-arm terminates in `unreachable`, so the precondition is met and the join
is trivial. Today this operation is effectively impossible mid-pass.

---

## 5. Semantic model: regions and the core dialect

### 5.1 Visibility and value flow

`%a` is usable at `%s` iff

1. `region(%a)` is an ancestor-or-self of `region(%s)`;
2. `%a` comes before `%s` (dense: id compare; editable: `comes_before`;
   floating: condition dropped — replaced by the acyclicity check); and
3. **`%s` does not lie within any region owned by `%a`.** A region-owning
   op's value does not exist until its regions have completed; without this
   clause, preorder layout would make an `if`'s result visible inside its own
   arms — admitting an arm that produces the enclosing `if`'s result, a loop
   body that bypasses its carried arguments by reading the loop result, and
   accidental closure recursion. (This was the sharpest soundness hole found
   in review.) Self-reference exists only through explicit binders: loop
   carried args, a future closure `rec` arg, `await`'s continuation value.

Clauses 1–3 define visibility **within one activation**. A `deferred` region
or an edge-defined `resume` subgraph (§3.3, §5.6) is an activation boundary: a
reference from inside it to an outside value is a *capture*, governed by the
owning construct's semantics (§5.6/§5.7 — by-value snapshot or shared cell),
implying no execution-order relationship; and no exit terminator or throw edge crosses the
boundary (L1).

Dominance for structured code is read off this rule — no domtree is computed;
classical dominance is computed only locally inside `cfg` islands (§5.5).
As §2.1 states precisely, this *under-approximates* full dominance across
diverging arms, recovered by canonicalization.

**Value flow across region boundaries** — one mechanism, symmetric:

- **In**: region arguments only — loop-carried values, handler `%exc`,
  resumption args, closure params, `cfg` block args, function parameters
  (region 1's args).
- **Out**: the owning op's results only, fed by exit terminators that are
  themselves *inside* the region — `result` (if-arms, try, cfg), `break` and
  fall-out (`continue` with false condition) for loops, `return` (function and
  closure bodies).
- **Neither**: cells — the deliberate memory-shaped path for values that
  cannot flow through results (across the throw edge, un-promoted variables,
  mutable closure captures) (§6).

No value is ever referenced outside the region that defines it. This is what
dissolves predicated.md's "special ω visibility": the ω *is* the op result and
its inputs are `result` operands in ordinary dominance position. It is also why
loop-closed SSA is structural — code after a loop can only name the loop's
results.

Precise rules (verified at L1):

1. **Feeding**: every exit terminator either feeds its owner's results with
   matching arity, or exits a strict ancestor op within the same activation
   (`return`, multi-level `break`, cross-island `goto` — running the
   structural leave/pop actions of every `try` crossed).
2. **Typing is a recomputed join** of what the feeding terminators actually
   produce — never a declared type (the #50285 lesson as a rule). Arms ending in
   `break`/`return`/`unreachable` contribute nothing to the join. Loop
   carried-argument types are the (widened) join of the init values and the
   backedge (`continue`) values — computed separately from the loop's
   exit-result join.
3. **Statements have zero or one result** (declared per kind: terminators,
   `cell_set`, and friends have zero; references to zero-result statements are
   rejected). An op with multiple exit values produces *one* value of tuple
   type: feeding terminators produce a tuple, consumers destructure via the
   explicit core kind `extract(value, index)` with a constant `INLINE` index
   (**1-based** — this is Julia; the index is exactly `getfield`'s) —
   a kind check, not call-pattern matching (there will be a lot of these,
   which is why `extract` uses the inline operand encoding of §3.2 and costs
   zero pool words; in Julia semantics `extract` maps to `getfield`, and SROA
   eliminates it routinely). If tuple churn shows up in inference profiles
   (which walk the
   pre-SROA form — an acknowledged cost, §13.1), a future representational
   variant can add native multi-result transparently behind the accessor API.
4. **Surgery owns the rewiring**: `wrap_in_if!` threads outward-used defs
   through fresh `result` terminators (§4.5); region inlining (folded conditions,
   restructuring) replaces result uses with the unique feeding `result` terminator's
   operands; `splice_body!` maps callee `return`s onto splice-point results.
5. **DCE**: unused results do not keep an op alive; an op survives only on the
   `REMOVABLE`-relevant properties of itself and its immediate-mode regions —
   effects, may-throw, **and termination** (an effect-free infinite `loop` is
   not removable; this aligns with §8.2's mask — the v1 rule omitted
   termination). Deferred-mode regions are excluded (§3.3).
6. **Floating dialect**: no *exit-fed structured-op results* exist (equations
   are ordinary single-result statements) — `merge` joins, guards scope; the
   question materializes only at legalization, when guards reify into `if` ops
   and merges into their results.

### 5.2 `if`: ω as a region result

predicated.md's ω-node survives as the result of an `if` op whose arms end in
`result` — the "special visibility into previous predication" rule made
structural (each `result` terminator sits inside its arm, in ordinary dominance position),
and #31603's move-phi-uses-into-predecessors done one level up:

```
%c = call >(%a, 0.0)
%z = if %c {
    result 1
} else {
    %y = call g(%a)
    result %y
} :: Int64
```

The **predicated view** — a bijective pretty-print for structured code, and the
form passes may "think in" (DAECompiler tearing, clock reasoning):

```
      %c = call >(%a, 0.0)
(!%c) %y = call g(%a)
      %z = ω(%c, 1, %y)
```

`select` (a mux whose operands are visible *at* the merge point) is a distinct
op from an if-result (whose operands are visible only *under their guards*) —
preserving the distinction SynchJulia is careful about (`:if` vs `:clocked`).

### 5.3 `loop`: carried values, not self-referential ω

Where predicated.md's loop encoding (`%l = ω(%l, %c, true)`) had holes — cyclic
use-def, forward references for every carried value, an implicit
parallel-move-group-at-head convention (phi-at-block-top in new clothes), the
non-local contiguity rule, and break-as-whole-loop-rewrite — the region form
fixes each structurally:

```
%r = loop (init %s = 0, %j = 1) {
    %f = call getindex(%found, %j)
    if %f { break (%s, %j) }                # local edit
    %s2 = call +(%s, %j)
    %j2 = call +(%j, 1)
    %c  = call <=(%j2, %n)
    continue %c (%s2, %j2)                  # repeat if %c, else results = (%s2,%j2)
} :: Tuple{Int64, Int64}
%sum = extract %r, 1
%i   = extract %r, 2
```

- Carried values are region args: defs at body entry, the parallel move
  explicit in the arg list. Next-iteration values are operands of `continue`:
  uses at body end. **The use-def graph is acyclic everywhere** (including
  clause 3 of §5.1: the body cannot read `%r`).
- `break (vals…)` / `continue cond (vals…)` are ordinary terminator ops; all
  exits produce the same arity, and the loop's results are the op's results — so
  loop-closed SSA is a structural fact, not a maintained invariant.
- Cross-iteration flow *must* go through the owning loop's args; inner regions
  see outer values by ordinary ancestor visibility. Nested-loop binding is
  explicit, not by-convention.
- Side-effect contiguity is region membership — insertion by id cannot violate
  it.
- (The parenthesized exit-value lists are grammar for tuple results; a printer
  sugar `%sum, %i = loop …` may render the result-plus-extracts form.)

### 5.4 `try`: see §6.

### 5.5 The `cfg` escape hatch

Julia has `@goto`, so irreducible control flow must be expressible. Rather than
restricting the IR (predicated.md's concession) or forking a legacy format,
irreducibility is quarantined in a `cfg` op: its region contains `block`
sub-regions with block arguments and explicit terminators. **Every successor
is an edge bundle** `(destination block, arguments…)` feeding the destination's
block args (the v1 `br_if(cond, dest)` schema could not express its own CFG —
one destination, no edge arguments):

- `goto (^bb, args…)`
- `br_if %cond (^bb_true, args…) (^bb_false, args…)`
- `switch %val [case c₁ → (^bb₁, args…), …, default → (^bbₙ, args…)]`
- `result vals…` — exit the `cfg` op with values
- `unreachable`

L1 verifies edge arity/types against destination block args. Rules:

- This is the **only** place block targets exist for *forward* control flow.
  One exception crosses outward: `goto` targeting a block of an **ancestor**
  `cfg` island is a sealed cross-region exit (§5.9) — required because
  `@goto` *out of* a `try`/`catch` is legal Julia (JuliaLowering emits
  `K"leave"` for it; only jumping *into* a try errors,
  `linear_ir.jl:209`), and a `try` inside an island is a structured op whose
  body is not a block. Such a `goto` runs the structural leave/pop actions of
  every `try` it crosses, exactly like `break`. Islands are otherwise closed;
  passes computing locally inside an island treat ops containing outward
  gotos via their exit summaries.
- May-throwing statements inside an island transfer to the dynamically
  enclosing handler exactly as anywhere else — the island does not change
  exception semantics.
- Passes that don't understand a `cfg` see one opaque op with summarized
  effects and exits. Inference and codegen run classical algorithms inside
  it; the classical dominator tree is computed locally, per island.
- Frontends emit structure directly (JuliaLowering sees structured source);
  only genuine `@goto` tangles produce a `cfg`. SynchJulia/DAECompiler never
  see one. Explicit terminators everywhere resolve #41476; `switch` removes
  the GotoIfNot asymmetry. An optional restructuring pass
  (Relooper/node-splitting) can eliminate `cfg` ops where profitable.

### 5.6 Delimited continuations: `await` (julia#58532)

Julia is gaining a delimited-continuation primitive
([JuliaLang/julia#58532](https://github.com/JuliaLang/julia/pull/58532)):
capture the current frame and return an `OpaqueClosure` that resumes execution
at a label; multi-shot; delimited by `return`. Its **central design property**
is that the decision of which values enter the capture list is deferred to the
last possible moment (LLVM coro-style lowering), so DCE, SROA, and AD
transforms apply across the suspension boundary.

**v1 core: the `cfg` form.** A structured `await` — whose continuation would
be "the remainder of the enclosing region tree" — would introduce a mid-region
second entry point that the data structure deliberately does not model (a
program point plus a stack of pending join obligations); it is future work
(question #5). UnifiedIR therefore represents `await` in `cfg` form, mirroring
the upstream `AwaitNode` exactly and inheriting its well-definedness:

```
cfg {
  ^entry:
    ...
    await [flags] (normal ^cont, resume ^resume)   # a BLOCK TERMINATOR with
                                                   #   two edge bundles (§5.5)
  ^cont(%C::OpaqueClosure{Tuple{T...},R}):
    ...              # normal path: the continuation is DELIVERED AS A BLOCK
                     #   ARGUMENT of the normal successor
  ^resume(%a::T...):
    ...              # entered per invocation of %C, with an independent frame
                     #   snapshot; %a are the invocation arguments (argt)
}
```

`await` is a *terminator*, so every CFG successor comes from a terminator (a
value-producing non-terminator with a successor would violate the
explicit-terminator rule). This resolves several questions at the
representation level:

- **Successor enumeration**: both edges are ordinary CFG successors; the
  resume edge is a real predecessor edge for **dominance** (defs before the
  await dominate `^resume` through it) but has distinct **liveness**
  semantics: live-in across it is the captured frame snapshot, which is
  exactly the capture-set computation.
- **Activation is edge-defined**: the resume edge and its reachable subgraph
  execute in a `resume` activation (§3.3) — a property of the edge, not a
  lexical region bit, since blocks reachable from `^resume` are ordinary CFG
  siblings.
- **`%C` is not defined on the resume path**: it flows only along the normal
  edge, as a block argument — settling at the representation level what the
  upstream PR leaves open about the continuation's own value (question #5:
  visible on resumption only if explicitly threaded).
- **DCE precondition, structural**: if `^cont`'s `%C` argument is unused and
  the resume subgraph is reachable only via the resume edge, rewrite
  `await → goto (^cont)` and the subgraph dies — the PR's "`AwaitNode` may be
  DCE'd", with its precondition spelled out.

- **Late capture is still the visibility/liveness rule**: no capture list
  appears in the IR; the continuation's environment is the live set at the
  await, computed classically inside the island, shrinking under DCE/SROA —
  the PR's key property, preserved.
- **Multi-shot ⇒ frame snapshot per invocation**, covering frame-class cells
  (§6) but *not* shared-class cells — matching the PR's actual model (slots
  copied, heap boxes shared). Cell promotion treats `await` as a frame
  boundary.
- Single-shot and other `flags` are an `INLINE` operand; the `argt`
  representation in resumption block args tracks the PR (design question #5).

A structured `await` (suspension expressible without an island) remains
desirable and is future work gated on an operational semantics for
resumption program points; the earlier sketch is preserved in design question
#5 as a starting point, explicitly **not** part of the v1 semantic core.

The PR frames `await` as "an alternative representation for
`:new_opaque_closure` that is friendlier to optimization passes" — the same
late-capture principle serves closure capture generally (§5.7). Its intended
consumers are precisely this design's audience: `Task` and the Compiler
futures mechanism, reverse-mode AD residuals (Diffractor/Enzyme), carried
state between torn partitions in DAECompiler, and ResumableFunctions.jl.

### 5.7 Late capture generalized: closures

The same late-capture principle serves lowering's **generic closure
conversion**. Today `JuliaLowering/src/closure_conversion.jl` +
`binding_analysis.jl` decide captures and `Core.Box` insertion *syntactically*,
before inference — the root cause of the #15276 boxing-pessimism class. On
UnifiedIR, a closure is a region op (the RVSDG lambda analog), owning a
**deferred-mode** region (§3.3):

```
%f = closure (%x::T) -> R {     # deferred region: entered by call, not fall-through
    %v = call +(%x, %a)         # %a: capture via visibility into ancestor regions
    return %v
}
```

- **No capture list in the IR.** The environment is the body region's free
  values, derived by the visibility rule, shrinking under DCE/SROA. Creation
  is pure; body effects are deferred to call time (mode-aware DCE, §3.3).
- **Mutable captures are shared-class cells** (§6) — the one semantic axis
  distinguishing closures from `await` (frame snapshot). `Core.Box` is
  subsumed by the shared cell class; boxing becomes cell promotion +
  typed-cell materialization *after* inference.
- **Closure inlining is `splice_body!`** where creation dominates the call
  (free values visible by construction — sound and conservative); the general
  case (closure escaping through an if-result, called where creation does not
  dominate) requires environment-substitution inlining, a defined future
  capability, not a soundness hole.
- **Materialization by escape class**: `OpaqueClosure`/continuations get fully
  late capture (structural, signature-only types — julia#58532's plan);
  non-escaping generic closures never materialize a type at all; *escaping*
  generic closures have their capture **set** fixed once per method definition
  by a late materialization pass (IR free-value analysis replacing the
  syntactic scheme), while the **layout** stays type-parameterized so typed
  cells (`RefValue{T}`-style fields) replace untyped `Box` per specialization.
  World-age discipline: the nominal type still comes into existence at
  definition time (dispatch identity, serialization); materialization is split
  between a definition-time part (type + field set) and a specialization-time
  part (inlining away, typed-cell instantiation).
- Recursive and multi-method local functions need a self-reference binder (a
  `rec` region arg — note §5.1 clause 3 makes accidental self-reference
  impossible, so recursion is opt-in by construction) or an eager fallback —
  design question #5.

**Scoping note: the v1 `closure`-op slice is IMPLEMENTED; the full P3
lowering port keeps its frame.** The op carries no capture list (the
environment is `closure_environment(ir, s)`'s derived free values — ordered
SSA values and cells referenced from the deferred subtree but defined
outside it, shared by the interpreter and the materializer), owns exactly
one `ACT_DEFERRED` `REGION_BODY` region whose leading `region_arg`s are the
parameters, and takes at most one INLINE flags operand (bit 1 = isva; the
trailing arg packs a varargs tuple). The L1 activation rules, implemented:

- **Result-feeding class per owner kind**: a `result` whose region is owned
  by a `closure` is an error — the body never feeds its owner (the closure
  IS the op's value); body exits are `return`/`unreachable`.
- **Cell-class boundary rule**: a cell op reaching a frame `cell` across an
  `ACT_DEFERRED` boundary is an error — `cell_shared` is the crossing class
  (§6). Keyed to `ACT_DEFERRED`: an `ACT_RESUME` boundary COPIES frame
  cells into resume snapshots, edge-defined for cfg `await` (§5.6).
- Exit terminators and cfg edge bundles never cross an activation boundary,
  and only `closure` owns deferred regions in v1.
- **Mode-aware effect composition** for DCE: an unused closure is removable
  on its creation-site flags alone — deferred-region effects surface at
  call sites (`dce!`; `adce_region_ops!` skips deferred regions).
- Throw edges bind to the executing activation's handler; the reference
  interpreter gets this free (a `UClosure` call unwinds to the CALL site's
  `try`), and executes closures as creation-time snapshots — values by
  value, `CellBox` cells by reference, so `cell_shared` sharing and
  multi-shot loop snapshots fall out.

**Capture decisions are structural.** `promote_capture_cells!`
(`UnifiedIR/src/promote.jl`, inside `promote_fixpoint!`) implements the
exact criterion — for a variable `v` and closure-creation site `C`, **value
capture is legal iff (a)** no write to `v`'s cell inside any deferred
region, **(b)** no write can execute after any home-frame site whose
subtree reads it (same-activation forward order with sibling-`if`-arm
exclusivity, plus multi-shot loop backedges, where the backedge hazard is
cancelled only when the CELL ITSELF is declared inside the shared loop — a
fresh shared box per iteration, the structural form of per-iteration
rebinding), **and (c)** a single defined value reaches each `C`, joins
included — judged by the standard fixpoint itself on a scratch copy,
without definedness-as-data. Maybe-undef captures keep the shared cell,
preserving use-time `UndefVarError`. `JuliaLowering.analyze_captures_precise!`
(`JuliaLowering/src/unified/capture_analysis.jl`) emits throwaway IR whose
creation sites are real closure regions holding the capture FOOTPRINT and
reads the per-variable verdict off which `cell_shared` cells survive; the
UnifiedBackend's default lowering path (`lower_to_ir`) emits full closure
regions and MATERIALIZES the residuals (`unified/materialize.jl`): capture
set := `closure_environment`, closure types via the existing
`eval_closure_type` machinery — value fields type-parameterized, surviving
shares `Core.Box` or the lowering-provable typed `Base.RefValue{T}` — and
the deferred region extracted as a standalone method IR. The worked
examples (the julia#15276 zoo) are in `docs/closures.md`;
`demo/capture_zoo.jl` is the runnable differential.

Still eager/fallback in v1 (per-construct, to `convert_closures`):
recursive self-capture keeps the shared-cell fallback (matching stock's
Box'd self-reference; the `rec` binder is design question #5), multi-method
local functions, kwargs closures, sparam'd (`where`) local methods, varargs
local functions, declared return types on local functions, and opaque
closures. `await` shares the activation-generic pieces when its cfg form
lands: `closure_environment` and the boundary-aware promotion rule are
keyed by a `boundary` parameter whose `:resume` variant replaces the
temporal no-write-after-creation criterion (b) with the live-at-suspension
frame snapshot (§5.6).

### 5.8 Core dialect inventory

| Group | Kinds (result arity noted where 0) |
|---|---|
| values/structure | `region_arg`, `extract` (const-index tuple extraction — `getfield` in Julia semantics; §5.1), `refine` (Pi successor: an ordinary, canonicalizable statement — fixes the #54762 accumulation class), `value` (escape hatch; reference-free leaves only, §3.2) |
| computation | `call`, `invoke`, `intrinsic`, `foreigncall`, `new`, `splatnew`, `globalref` |
| structured CF | `if`, `loop`, `try`, `select`; terminators (0-result) `result`, `continue`, `break`, `return`, `unreachable` (`break`/`continue`/`return` carry a `REGION` target for multi-level exit within the activation) |
| suspension/capture | `await` (§5.6; v1 in `cfg` form), `closure` (§5.7; v1 slice implemented — one ACT_DEFERRED body region, optional INLINE isva flags) |
| unstructured island | `cfg`; block terminators (0-result) `goto`, `br_if`, `switch` with edge bundles (§5.5) |
| memory cells | `cell` (frame-class), `cell_shared` (heap-class), `cell_get`, `cell_set` (0), `cell_new` (0; re-undefines — the `NewvarNode` successor), `cell_isdefined`, `throw_undef_if_not` (0) — §6 |
| lowering/runtime vocabulary | `gc_preserve_begin`/`gc_preserve_end` (0; pairing is an L1 rule; a preserve-*region* variant that makes pairing structural is under consideration), `boundscheck`, `latestworld` (0; world barrier — inference must split states across it), `meta` payloads as flags/columns (`:inline`/`:noinline`), coverage effect (0; effectful-inert, position-pinned), `loopinfo` as an *op-attached column* on `loop` (not an adjacency-encoded statement — surgery preserves columns, not adjacency), `copyast`, `cfunction`, `:method`/top-level forms (P3 lowering-port appendix will enumerate exhaustively) |

**Gone**: `PhiNode`, `PhiCNode`, `UpsilonNode`, `EnterNode`, `:leave`,
`:pop_exception`, `GotoIfNot`, implicit fallthrough, `SlotNumber`/`NewvarNode`
(→ cells), `Argument` (→ region args), `ReturnNode`-as-struct — **from the
pass-facing mid-end representation**; the boundary converters synthesize
phi/PhiC forms until direct codegen consumption (§10).

### 5.9 Early exits: precedent and design rules

Every structured IR confronts `break`/`continue`/nested-`return`; the
precedent landscape, and where this design sits in it:

- **MLIR `scf`** has no early exits; frontends pay for it divergently —
  Polygeist flag-encodes (`i1` flags + guarding all subsequent code), Flang
  bails to the unstructured `cf` dialect for any loop containing `EXIT`, and
  RVSDG absorbs exits into theta-node predicates. All three are the
  whole-body-rewrite cost predicated.md's loop encoding had.
- **The active MLIR RFC** ([region-based control flow with early
  exits](https://discourse.llvm.org/t/rfc-region-based-control-flow-with-early-exits-in-mlir/76998),
  2024–2026, `scf.loop`/`scf.break`/`scf.continue`) surfaced the canonical
  objections: interface-defined exits let unregistered ops silently change
  control-flow semantics (region CF must remain a *core concept*); mid-block
  exits violate "only terminators leave blocks" and break post-dominance;
  regions lack predecessor tracking (finding a loop's exits = IR walk); and
  the "effect at a distance" mental-model problem (do intermediate ops
  re-throw exits by default?). Value-carrying and multi-level exits were left
  unresolved. Consensus direction: a formal region kind + a block-vs-region
  terminator distinction, in core; an exception-unwinding framing (ancestors
  catch, intermediates re-throw) as the semantic model.
- **ClangIR** is the production existence proof: `cir.break`/`cir.continue`/
  `cir.return` legal in nested regions, one late FlattenCFG pass to blocks.
- **WebAssembly** is the strongest precedent for this design specifically:
  multi-level `br N` with **typed labels** — the branch carries values that
  feed the target's results, validated by a stack walk, deployed at scale for
  a decade. That is exactly §5.1's feeding rule.

Design rules adopted from this history:

1. **Exit kinds are sealed core**: `break`, `continue`, `return`, and `goto`
   (the last targeting a block of an ancestor `cfg` island — required because
   `@goto` out of `try`/`catch` is legal Julia, §5.5). External dialects
   cannot register new inter-region control-transfer ops (they get calls,
   closures, `cfg`, and effects — never new exit kinds). The may-early-exit
   summary is therefore derivable by scanning for core exit kinds, even
   through foreign guard regions; the unregistered-op hazard cannot arise.
2. **Exits are region terminators by construction.** `if %found { break }`
   makes the `break` the arm's terminator; L0's one-terminator-per-ordered-
   region invariant (floating `guard` regions exempt — §3.3) means there is no
   mid-block exit and no block/region-terminator ambiguity to retrofit.
3. **Explicit targets → maintained reverse index.** Exits carry a `REGION`
   (or, for cross-island `goto`, `BLOCK`) target operand, so "all feeders of
   this op's results" is an O(n)-buildable, surgery-maintained analysis-cache
   entry — the generalization of the may-early-exit bit.
4. **Exits never cross activation boundaries** (§3.3, §5.1) — the unwinding
   model is scoped to one activation.
5. **Exit-normalization is a pass, not a representation.** Because
   flag-encoding is local and mechanical here (rewrite exits into flags +
   guards), exit-free form is available on demand for passes or backends that
   want it; the floating dialect is exit-free by construction, so
   SynchJulia/DAECompiler never encounter exits at all.
6. **Unwinding vocabulary unified with `try`**: an early exit is an unwind
   without an exception object; `try` is the one op that handles rather than
   re-throws (§6).

---

## 6. Exception handling and cells

The devdocs describe PhiC/Upsilon with the metaphor "a store-many, read-once
slot". UnifiedIR promotes the metaphor to the representation — with the
claim stated precisely: **the PhiC/Upsilon *information* does not
vanish; what changes is that it stops pervading the representation and every
pass.** It is concentrated in exactly two localized places: the promotion
pass's refusal rules, and the boundary converter's synthesis (§10).

**Cells come in two storage classes** (one construct cannot serve both
copy-on-suspend and share-by-reference semantics):

- `cell T` — **frame-class**: per-activation storage. Subsumes slots and
  Upsilon/PhiC targets. Copied into each `await` frame snapshot (§5.6).
- `cell_shared T` — **heap-class**: shared by reference. Subsumes `Core.Box`
  and SynchJulia's reified state; the class closures use for mutable captures
  (§5.7). *Not* copied by `await` — matching julia#58532 (slots copied,
  boxes shared).

Undef is explicit vocabulary: `cell_new` re-undefines a cell (the
`NewvarNode` successor — per-iteration fresh locals), `cell_isdefined` tests
(`@isdefined`), `throw_undef_if_not` raises `UndefVarError`; `cell_get` on a
never-stored cell is a checked error.

```
%buf = cell IOBuffer
%r = try {
    %b = call open_buffer(%a)        # may throw BEFORE %buf is stored
    cell_set %buf, %b
    %s = call may_throw(%b)          # may throw AFTER %buf is stored
    result %s
} catch (%exc) {
    %d  = cell_isdefined %buf
    throw_undef_if_not %d, :buf
    %b′ = cell_get %buf
    %m  = call recover(%exc, %b′)
    result %m
} :: String
```

- **The invisible catch edge becomes a region property**: "any **may-throw**
  statement in the body may transfer to the handler" — the predicate is the
  `NOTHROW` effect bit, *not* effectfulness (a pure `sqrt` throws; an
  effectful store can be nothrow; the v1 wording had this backwards). This is
  a fact about the `try` op, not n·m CFG edges (the edge explosion `ssair.md`
  cites as the reason Julia didn't adopt LLVM-style invokes). The handler
  observes, for each cell, the last store on the throwing prefix — as the
  example shows, possibly none.
- **The handler takes `%exc` as a region argument** — catch blocks finally
  have a dominating def for the exception. `current_exception()`/`rethrow()`
  in nested handlers resolve to the nearest enclosing handler's `%exc`
  (no exception-stack tokens in IR; the runtime stack stays a runtime
  concern).
- **v1 promotion policy** (deliberate, cheap, honest): the cell-promotion
  pass (mem2reg over the region tree — straightforward for structured code,
  classical inside `cfg` islands) promotes **frame-class cells whose every
  use is reached by an unconditional dominating store within the same
  activation, never across a throw edge and never across an `await`.**
  Conditionally-assigned variables and exception-crossing values simply stay
  cells through the mid-end — which is what PhiC/Upsilon *lower to* in
  codegen anyway (stack slots), and today's optimizer does almost nothing
  clever with them either. Promotion across throw edges (true exceptional
  SSA) is a future optimization with its own spec (design question #15), not
  a prerequisite. This is what replaces `slot2ssa` for the cases where
  `slot2ssa` is simple, and *declines* to replace it where `slot2ssa` is
  hairy (its `undef_ssaval`/definedness machinery) — those cases keep the
  memory form that codegen gives them anyway.
- **Boundary synthesis** (§10) — with the "mechanical 1:1" claim scoped
  precisely: Upsilon/PhiC model *exceptional
  store-many/read-once* transfer specifically; a general residual cell may
  also have normal-path reads and writes, multiple reads, `cell_new`,
  definedness tests, and loops. The exit converter's specified strategy:
  **P0–P2 restrict the feature matrix to cells promotable before exit**
  (declared in the converter's matrix, an error otherwise); **P3 implements
  full synthesis** — ordinary phis for normal-path flow, Upsilon/PhiC for the
  exceptional transfers, definedness bits where needed. That is `slot2ssa`'s
  algorithm relocated to the boundary and run once, rather than a pipeline
  stage every pass sees — which is the honest form of the deletion claim.
  Shared-class cells lower to `Box`-equivalents, reproducing today's forms.
- `:leave`/`:pop_exception` bookkeeping becomes structural: exiting k regions
  is leaving k handlers. The `verify.jl:246` concession has nothing left to
  concede. The `tryfinally` dynamic-scope operand (`@with`) is an optional
  operand of `try`.
- `finally` stays frontend-lowered with the tag scheme JuliaLowering already
  implements; a core `finally` region op was considered and rejected (buys
  little over tags, complicates every region-aware pass).
- **Early exits** (`return`, `break`/`continue`, cross-island `goto` crossing
  a `try`) are explicit ops whose semantics include the structural leave/pop
  actions — an unwind without an exception object, with `try` the one op that
  handles rather than re-throws; each region caches a *may-early-exit*
  summary (precedent and rules in §5.9; residual pass-tax risk in design
  question #2).

Since spurious-PhiC cases like #34229 (empty try/catch forcing a PhiC in a
function with no SSA values) simply have no cells to insert, they cost nothing
here.

### 6.x Join-point completeness (mem2reg over the region tree)

**The claim.** For slot-class (frame, non-escaping) cells, the hybrid IR's
join vocabulary — `if` results, loop region args, loop break-values, and
island block-args — is jointly COMPLETE: the promotion pipeline places a
join value at exactly the region points that project onto the iterated
dominance frontier (IDF) of the cell's store set in the flattened CFG, and
therefore reaches the same SSA form classical slot2ssa reaches through
dominance frontiers, up to the documented exception classes below.

**The passes and the projection.** Promotion is a joint fixpoint of five
passes (`optimize_ir!` interleaves them; `promotion_fixpoint!` in
`Compiler/src/unified/completeness.jl` runs them in isolation):

- `promote_cells!` — the dominating case: every read reached by one
  unconditional dominating store; no join value needed (empty IDF after
  liveness pruning).
- `promote_block_cells!` — the single-region case: all uses direct members
  of one region with reads store-preceded in region order; sequential
  execution makes the last preceding store the reaching definition on every
  path (empty pruned IDF).
- `promote_arm_cells!` — the if-join case, by STORE SINKING: conditional
  sibling-arm stores become per-arm `result` values plus ONE unconditional
  `cell_set` of the `if`'s (possibly tupled) result immediately after the
  join. An arm that does not store contributes the incoming value (a
  `cell_get` materialized before the `if`, inserted only under definite
  assignment); arms that exit (break/continue/return/unreachable) contribute
  nothing, and their stores legitimately flow out through memory. The
  placement projects onto the IDF block of the diamond's merge point.
- `promote_island_cells!` — the island case: full liveness-pruned
  IDF-placed SSA construction over one cfg op's block graph. Phis are block
  region-args; incoming values ride the edge bundles of the block
  terminators AND of sealed exits of nested islands that land on our blocks
  mid-block (§5.5) — a mid-block edge exports the reaching definition at
  the sealed sub-op's member position (well-defined even when the exit
  leaves from a nested handler, because candidate stores are all direct
  block members and so cannot execute mid-`try`). Placement here IS the
  classical algorithm, so the projection is the identity.
- `promote_loop_cells!` — the loop case: body stores become carried region
  args (the IDF block of the loop header — the backedge join) and
  break-values (the IDF block of the loop exit when distinct definitions
  merge there).

The composition argument is local: arm sinking strictly simplifies the store
structure (conditional → unconditional at the join) without touching reads,
which is precisely the input shape the dominating, island, and loop cases
consume (an if fully inside one island block sinks to a block-level store
that the island pass then routes); inside-out processing over nested ifs
makes an inner join's post-join store a direct arm store of the enclosing
arm. Each pass preserves the §6 inviolables — cells observable across throw
edges stay memory-form (the sink also may not move a store past a
potentially-throwing point that a handler could observe, hence the
handler-read refusal), maybe-undef-at-read cells keep their `isdefined`
vocabulary, escaping/token cells never promote.

**Sealed-exit threading and the backedge-staleness rule** (island
soundness): when a loop body lies strictly between a cfg op and a cell's
declaration, the cell's memory is carried ACROSS iterations through the
island's sealed exits (`continue` terminators leave the island directly; the
next iteration re-reads the cell before re-entering it). When the entry
value is OBSERVED, the value THREADS: the loop grows a carried arg (init =
the store reaching the loop, refused unless it re-executes per enclosing
iteration or no other store shares an enclosing backedge — the init
backedge hazard), every `continue` targeting the loop appends the reaching
definition at its exit point (the island dataflow's per-block values, also
through mid-block sealed exits), island reads take the arg, and post-loop
reads keep memory fed by one unconditional store of the loop's exit value.
An UNOBSERVED entry value needs no threading at all — every read is
iteration-local and deleting the stores is already sound. The loop pass
composes the same way at structured level: post-loop reads that cannot
anchor for direct exit threading SINK one unconditional store of the exit
value right after the loop (pre-loop stores are then kept for the paths
that skip it), which the next fixpoint round consumes at the enclosing
level. Cases that cannot legally thread keep the refusal — the staleness
sentinel: in particular DOUBLY-carried cells (the value crosses two
backedge levels, so the inner carried init would need the outer carried
value) stay classified memory.

**Residual classes** (machine-checkable; `classify_residual_cells`): the
taxonomy is TWO-CATEGORY. Cells surviving promotion must carry one of the
v1 REPRESENTATION choices, each naming its successor mechanism:

| reason | meaning | successor |
|---|---|---|
| `:handler_crossing` | observed across a throw edge (handler reads/queries; try-crossing uses with a joining handler) | exception SSA (PhiC/Upsilon equivalent) |
| `:gc_token` | gc_preserve token cells | pairing verifier tracks values |
| `:box_capture` | shared/boxed captures (`cell_shared`) | closure conversion (no producer today) |

EVERYTHING else is a bug, verified at UNCLASSIFIED severity by the harness:
the diagnostic classes `:escape` (converter-path escape), `:island`,
`:refused_multilevel_exit`, and `:maybe_undef_read` exist to aid debugging
but any occurrence fails the acceptance run. Ordinary slot residuals are
ZERO corpus-wide.

**Definedness as data** (`promote_undef_cells!`): maybe-undef cells rewrite
into a definitely-assigned value cell (dummy-initialized at declaration)
plus a parallel `Bool` definedness cell — stores set it, `cell_new` clears
it, `cell_isdefined` reads it, and undominated reads acquire a guard
(`if !def; throw(UndefVarError); unreachable; end`). Both cells then
dissolve through the ordinary join machinery: the Bool joins land on
exactly the same dominance-frontier points as the value joins, and guards
constant-fold wherever definedness is provable. This is stock slot2ssa's
maybe-undef handling expressed in region vocabulary — no cell is left
behind for being possibly-undefined.

**Nested carried args**: when a carried init is stale across an enclosing
backedge (the init hazard), the plan switches to MEMORY-INIT — the init
becomes a `cell_get` immediately before the loop and the exit store keeps
that memory current — so the enclosing loop's next fixpoint round consumes
the get/store pair as ordinary body traffic. Doubly-carried cells
(countlines' line counter) dissolve one nesting level per round.

**The harness** (`Compiler/src/unified/completeness.jl`, tests in
`Compiler/test/unified/completeness.jl` + `cellfuzz.jl`, full-scale runs in
`Compiler/bench/unified_completeness.jl`):

1. residual classifier + STOCK ORACLE over real Base/stdlib bodies: where
   stock `code_typed` of the same instance fully mem2reg'd — slot-free,
   Box-free, and free of runtime undef guards (`throw_undef_if_not`, stock's
   own admission of a maybe-undef slot) — our residual set must be empty
   apart from exception-SSA classes;
2. structured fuzzing (random nested if/loop/try with random cell
   placement, the gcd swap shape, multi-level exits, guarded maybe-undef
   reads, handler interactions) with a semantic differential — identical
   values AND identical thrown errors before/after promotion — and residual
   classification totality;
3. DF correspondence: flatten the pre-promotion body through the exit
   converter, compute STOCK slot2ssa's liveness-pruned IDF
   (`Compiler.iterated_dominance_frontier`) per cell, and check our
   placements land exactly on those blocks (if-join → merge block, carried
   args → header block, break values → exit block, island phi → its block
   label). Statically dead code (flattened as unreachable blocks, where
   dominance is undefined) is excluded on both sides. Extra placements
   beyond pruned IDF occur (arm sinking is unpruned, and the region form
   must thread values out through enclosing arms — `:if_thread` in the
   trace — where flat SSA reads a dominating phi directly); they are
   harmless-but-suboptimal and reported. MISSING placements are completeness
   bugs; the acceptance target (and measured result) is zero.

---|---|---|
| `:escape_or_token` | value-used/escaping cell, `cell_shared`, gc-preserve token | `Core.Box` / token slots |
| `:throw_edge_handler` | read or queried in a handler, or used across a `try` boundary | PhiC/Upsilon exception SSA |
| `:maybe_undef_read` | some read/query no store dominates (definite assignment fails) | slots with undef tracking |
| `:island` | uses inside a residual cfg island (irreducible control flow) | goto-land phis |
| `:refused_multilevel_exit` | loop-boundary refusals: values through multi-level exits / ambiguous body reaching | phis stock places via IDF in goto-land |
| `:UNCLASSIFIED` | none of the above — a completeness BUG | — |

**The harness** (`Compiler/src/unified/completeness.jl`, tests in
`Compiler/test/unified/completeness.jl` + `cellfuzz.jl`, full-scale runs in
`Compiler/bench/unified_completeness.jl`):

1. residual classifier + STOCK ORACLE over real Base/stdlib bodies: where
   stock `code_typed` of the same instance is slot-free and Box-free, our
   residual set must be empty apart from exception-SSA classes;
2. structured fuzzing (random nested if/loop/try with random cell
   placement, the gcd swap shape, multi-level exits, guarded maybe-undef
   reads, handler interactions) with a semantic differential — identical
   values AND identical thrown errors before/after promotion — and residual
   classification totality;
3. DF correspondence: flatten the pre-promotion body through the exit
   converter, compute STOCK slot2ssa's liveness-pruned IDF
   (`Compiler.iterated_dominance_frontier`) per cell, and check our
   placements land exactly on those blocks (if-join → merge block, carried
   args → header block, break values → exit block). Extra placements beyond
   pruned IDF occur (arm sinking is unpruned, and the region form must
   thread values out through enclosing arms — `:if_thread` in the trace —
   where flat SSA reads a dominating phi directly); they are
   harmless-but-suboptimal and reported. MISSING placements are completeness
   bugs; the acceptance target (and measured result) is zero.

---

## 7. The dataflow dialect (SynchJulia / DAECompiler)

**Clock = guard = region**, with the precision from §2.1: exact in floating
state (one region per canonical clock after `assign_regions!`), refined by
region identity in dense state, and — the review's key addition — **scoped per
instantiated node frame** (see modular clocks below).

- `CBase` (base clock) is region 1 *of a node instance*. `COn(parent, cond,
  polarity)` is a child region of kind `guard` (owner 0 while floating).
  The clock lattice's canonical path structure *is* the region tree's path
  structure.
- **`when(clk, e)` is not a node kind**: sampling is guarded placement —
  putting `e` in the region guarded by `clk`. (SynchCompiler may keep a `when`
  view for surface fidelity.)
- **`merge(clk, a, b)` is ω**: an if-result whose operands are visible only
  under their guards. The mux (`select`) remains distinct, as SynchJulia
  requires.
- **`pre(x; init)` is `synch.delay(x, init[, reset])`** — the only legal way
  to close a cycle in floating state (the causality rule; only the delayed
  *data* edge is cut — `init`/`reset` are instantaneous, §4.3). Legalization
  rewrites it into a carried arg of the step `loop`, or into a cell when the
  state struct is reified. Delays carry temporal identity: never CSE'd or
  duplicated (§4.3).
- SynchCompiler's `DMeta`/`IMeta` (`ty`, `clock`, `init`, `prov`) map 1:1 onto
  columns; `DDecl` onto named region args/results. Clock inference is
  **two-phase**: during unification the clock *column* is authoritative
  (statements sit in a pending region; `CVar`s live in the column; the region
  table is never touched), then one `assign_regions!` pass reifies the solved
  canonical clock terms into the region tree — regions are an *output* of
  clock inference, never mutated mid-flight (resolved question #7).
- **Scheduling = assigning positions** (floating→dense, §4.3 — layout only,
  ending in compaction); **legalization** (guards→`if`s, merges→if-results,
  delays→state) is a separate pass sequence. Causality errors are the cycles
  scheduling cannot break. The DNode → INode → IFunction pipeline becomes
  column writes, one scheduling transition, and explicit legalizations in one
  structure.
- **Modular compilation & clock polymorphism**: SynchCompiler's
  `ClockInstantiation` witnesses (mirroring Vélus's `WellInstantiated`) make a
  callee's entire clock tree *parametric* in the caller's per-call-site base
  clock, with a sampler-rename map. UnifiedIR represents node invocation as a
  registered kind `synch.instantiate(callee, base_clock::REGION,
  rename_map::CONST)` whose witness rides as an operand — clock inference
  substitutes through it exactly as today (the unification machinery lives in
  columns and is unchanged); per-call-site **instance state** maps onto
  distinct cells/state fields per instantiation site at legalization. The
  clock=region identification holds within each instantiated frame; the
  witness is the bridge between frames.
- **C/Julia source codegen walks the region tree** — SynchCompiler's
  `IFunction` heads (`:if`/`:while`/`:for`/`:break`/`:continue`/`:return`) map
  1:1 onto region ops. **IRStructurizer's role is absorbed** (§10): structure
  is native once frontends emit it, and the entry converter's structurization
  pass covers goto-form input, including its `source_location` scope-stack
  query via the provenance column.

Worked example — the counter node through the pipeline. Floating (as lowered):

```
node @counter(%reset::Bool) -> (%out::Int64)  layout=floating {
  eq %prev = synch.delay(%out, init const 0, reset %reset)   # cycle legal through delay
  eq %out  = call +(%prev, 1)
}
```

After scheduling + state legalization (delay reified into a state field;
cells have become ordinary mutable-struct fields, exactly as SynchCompiler's
`lower_inode!` does today):

```
func @counter.step(%reset::Bool, %state::CounterState) -> Int64 {
  %old = call getfield(%state, :prev)                          :: Int64
  %p0  = if %reset { result const 0 } else { result %old } :: Int64
  %out = call +(%p0, 1)
  call setfield!(%state, :prev, %out)
  return %out
}
```

A clocked equation, showing clock-as-region literally (`x^clk = when(clk, a)`;
note operands are values, not nested expressions — the init expression is its
own equation, whose init-time clocking is a Synch-dialect analysis concern):

```
node @test(%clk::Bool, %a::Int64, %b::Int64) -> (%z::Int64^clk)  layout=floating {
  region ^on_clk = arm(^base, cond %clk)        # = COn(CBase, [:clk], false); owner 0
  eq %x  @^on_clk = %a                          # when(clk, a): guarded placement
  eq %i0 @^on_clk = call +(%b, const 2)
  eq %d  @^on_clk = synch.delay(%x, init %i0)
  eq %z  @^on_clk = call +(%d, const 1)
}
```

**DAECompiler scope**: the equation frontend
sits exactly where predicated.md aimed — equations as floating effect-free
statements, `variable`/`equation!`/`ddt` as `dae.*` kinds, predicate inference
as region assignment, the `Incidence` lattice as an extension column. But the
*majority* of DAECompiler's compiler surface is Diffractor-style forward-mode
AD and dominance-based tearing over goto-form IRCode
(`index_lowering_ad.jl`, `tearing_schedule*.jl`). Migrating that means either
porting the AD transforms to region IR — plausibly *easier* than on CFGs
(forward-mode is local and region-preserving; cf. Enzyme/MLIR experience) but
unbudgeted research — or round-tripping through the boundary converters per
derivative. This is called out as its own P3-adjacent line item, not assumed
away.

---

## 8. Extensibility and external reusability

### 8.1 Extension mechanics: tables for kinds, dispatch for types

The precise invalidation lesson of #61510/#58000 is *not* "avoid methods" —
it is: **precompiled compiler code must not contain abstractly-typed dispatch
sites whose method tables grow with the ecosystem** (inference world-splits or
devirtualizes over the loaded methods; backedges on abstract signatures fire
when the next package adds a method). UnifiedIR's rules, drawn from that:

1. **Per-kind data lives in tables keyed by integer kind** — effects mask,
   result arity, operand schema, printer names, optional transfer callbacks —
   populated at `register_dialect!` time. This is not dispatch-avoidance on
   principle: kinds are integers *by design* (no per-kind Julia types, no
   C-side `jl_*_type`s), the runtime key is an integer, and hot loops want
   `effects[kind]` to be an array index, not a `Val`-dispatch constructed from
   a runtime value. Loading a dialect adds rows and invalidates nothing.
2. **Type-carried extension behavior extends by multiple dispatch** — columns
   (§3.5), constant codecs, analyses, lattices: define a type, add methods.
   That is how Julia does things, and the universe model is what makes it
   safe: hook calls inside a pass specialized for a universe are **concrete**
   — monomorphized at precompile time, invalidation-immune (a new column type
   cannot intersect a concrete signature), producing fresh specializations in
   the consumer's pkgimage and touching nothing in the sysimage. The
   discipline that remains is a **call-site typing rule**: precompiled core
   code calls extension hooks either at concrete signatures (guaranteed by
   universe monomorphization) or through deliberately-dynamic barriers whose
   hook functions carry `@max_methods 1`-style protection (the Dict-backed
   dynamic-column mode) so a growing method table cannot invalidate callers
   via world-splitting.
3. UnifiedIR owns all `Base` interface methods (`iterate`, `getindex`, `show`,
   …) on its own types — removing the piracy pressure that produced
   DAECompiler's `compiler_reexports.jl`.
4. CI gate: load a synthetic dialect package *and* a synthetic universe,
   assert zero invalidations in UnifiedIR + Compiler pkgimages (this
   *measures* the accessor-generation question from §3.4 and the call-site
   discipline of rule 2, rather than assuming them).

### 8.2 The opacity contract

A stock pass encountering an unknown kind may consult **only** (a) its
operands/results and (b) its declared effect bits. Today's
`IR_FLAGS_REMOVABLE = EFFECT_FREE | NOTHROW | TERMINATES` rule generalizes
verbatim: DCE deletes unused removable ops; CSE requires `CONSISTENT`; code
motion consults the memory bits; anything else is a barrier. Unknown ops never
own regions in v1 (§3.3), so there is no unknown-traversal problem; declared
guard regions compose with the effects of their contents by the mode rules.
Effect declarations are trusted exactly as `@assume_effects` is, with verifier
lints. External dialects additionally carry a **legalization contract**: a
final target-legality check runs before conversion to `CodeInfo` or another
backend, so an op with no registered lowering is a verifier error, not a
codegen surprise. This deletes the reasons SynchCompiler's
`AbstractInterpreter` exists to protect intrinsics (`NoCallInfo` visibility
hack, tfunc/efunc plumbing, forced inlining policy).

### 8.3 Packaging and layering

An in-repo, pure-Julia package using the dual sysimage/stdlib-package
mechanism proven by `Compiler/src/Compiler.jl` (stub on old Julia,
pkgimage-reuse fast path, UUID pinning), **layered below Compiler**, split in
two (a zero-dependency package cannot itself know `IRCode` or drive
inference):

- **`UnifiedIR` (core)** — zero dependencies: the data model, layout states,
  region semantics, verifier, printer, test dialect, the kind registry.
  Knows nothing of `IRCode`, inference, or method tables. Compiler imports it;
  it never imports Compiler — the MLIR:LLVM relationship.
- **Interop** — the `CodeInfo`/`IRCode` converters and the `Queries` API
  (§8.5) live *with the compiler side* (in Compiler, or a bridge package/
  extension), registered through a provider mechanism (the existing
  `Base.REFLECTION_COMPILER`-style indirection). With no provider loaded, the
  queries throw a defined error; the core remains fully usable (test dialect,
  external dialects, printing, verification).

Because kinds are integers and statements are columns, **no new C-side
`jl_*_type`s are needed**; `CodeInfo` remains the execution and serialization
boundary until the last roadmap phase. The graph substrate is intended to be
shared with JuliaSyntax/JuliaLowering (§3.7): a zero-dep `AttrGraph` bottom
layer both build on — neither package depends on the other.

### 8.4 API tiers and (non-)stabilization

- **Tier 1 (intended-stable surface)**: IR/`StmtId`/`RegionId`/Kind types,
  builders, iteration, `comes_before`, layout-state transitions,
  dialect/column registration, `verify`, printing.
- **Tier 2 (intended-stable-for-compilers)**: the editable session,
  `splice_body!`, effect-bit definitions, the analysis-cache and remap
  protocols, boundary converters (provider-side).
- **Tier 3 (`UnifiedIR.Internals`)**: explicitly none.

**No stability milestone gates anything.** Julia has lived with unstable
compiler APIs for twenty years; UnifiedIR's job is first to *exist* and be
right. The tiers record intent so churn is deliberate; no formal commitment
attaches to any tier until real ports (P2–P3) have validated the
representation, and the API may remain explicitly unstable for a while even
after merging. The test-dialect grammar acquires de-facto stability through
its own test suite, not through a freeze.

Naming discipline settles #49057 up front: the row is a **stmt**, everywhere;
"instruction" and "node" do not appear in the API (a "node" exists only in
AST/SyntaxGraph contexts).

### 8.5 Inference-as-a-service: `Queries` (provider-side)

SynchCompiler hand-drives `InferenceState`/`typeinf` (~50 lines × 3 sites, with
version branches) just to ask Const-aware questions. The queries wrap
whichever compiler is active, via the provider registration of §8.3:

```julia
infer_return(f, argtypes; config)                 # Const-aware; argtypes may contain Const(v)
typed_ir(f, argtypes; config, optimize_until)     # const-seeded, returns UnifiedIR.IR
effects_of(f, argtypes; config)
InferenceConfig(; method_table, cache_token, inline_all, semi_concrete, world)
```

`InferenceConfig` declaratively covers the remaining legitimate
`SynchInterpreter` reasons: overlay tables (subsuming CompilerCaching's
`StackedMethodTable`), cache isolation, inlining policy, the
semi-concrete-with-overlays workaround (julia#47349). Success metric: a
consumer on core + Queries carries **zero** `@static if VERSION` branches.
Genuinely custom lattices (DAECompiler's `DAELattice`) still warrant a real
`AbstractInterpreter` — that's appropriate, not an entry fee.

---

## 9. Test dialect and textual format

The IR must be fully exercisable **without Julia semantics** — no method
tables, no inference, no `Core` types. LLVM's great strength; a hard
requirement here.

**The `test` dialect**: `test.iconst`, `test.add`, `test.mul`, `test.icmp`,
`test.print` (an effectful op), `test.opaque` (declared-effects black box),
`test.delay` (a unit delay, so floating state and acyclicity-modulo-delay are
exercisable without Synch semantics) — enough, with the core structural ops
(`if`/`loop`/`try`/`cfg`/cells), to exercise every feature of the data
structure: layout transitions, region surgery, splicing, promotion,
verification, compaction, printing.

**Textual format** (MLIR-flavored, versioned grammar) — with the round-trip
guarantee scoped precisely. The full in-memory IR contains values
with no portable textual form (mutable identity-bearing constants, modules,
`CodeInstance`s, lattice elements, arbitrary column values); a universal
`parse(print(ir)) == ir` is impossible. Instead:

- A **portable subset** is defined by file-level dialect and column schema
  declarations plus registered, non-evaluating per-column and per-constant
  codecs. Within the subset, round-trip is guaranteed against **structural
  equality** (defined separately from runtime object identity) and enforced
  in CI. The test dialect is 100% inside the subset; the Julia dialect
  guarantees a documented constant subset.
- Outside the subset, the printer emits opaque markers that the parser
  **rejects cleanly** — a debug printer covers everything without promising
  parseability.

```
func @f(%a::Int64) -> Int64 {
  %c = test.icmp sgt, %a, const 0        :: Bool     !dbg(f.jl:3)
  %z = if %c {
    result const 1
  } else {
    %y = test.mul %a, %a                 :: Int64
    result %y
  } :: Int64
  return %z
}

node @counter(%reset::Bool) -> (%out::Int64)  layout=floating {
  eq %prev = test.delay(%out, init const 0, reset %reset)
  eq %out  = test.add %prev, const 1
}
```

`%N` are stmt ids (dense post-compaction), `::T` the type column, `[flags]`
effect bits, `!name(…)` extension columns via their codec methods (§3.5). The
printer and grammar spec live in core (verifier errors and reflection display
need them anyway); the parser, `@ir_str`, and FileCheck-style helpers live in
a tooling package. Grammar stability is de-facto (per §8.4), earned by the
test suites that depend on it.

---

## 10. The end-state pipeline and its boundaries

The point is not to add an IR; it is to end with fewer. End state:

1. **JuliaLowering emits UnifiedIR directly.** Structured source → regions
   (no goto detour; `cfg` only for `@goto` tangles); variables → cells (with
   classes and undef vocabulary, §6); local functions → `closure` ops (§5.7)
   with conversion deferred to late materialization; expression provenance →
   the provenance column. The full lowering output vocabulary (§5.8 last row)
   is enumerated in a P3 appendix.
2. **Cell promotion is the first pipeline pass** — mem2reg over the region
   tree under the §6 v1 policy. Replaces `slot2ssa` for the common cases and
   deliberately leaves the hairy ones (conditional assignment, throw-edge
   crossing) in memory form.
3. **Inference runs on UnifiedIR.** Types written into the `type` column in
   place; `CallInfo` is a column; irinterp/semi-concrete on the same
   structure. The three genuinely hard mappings — the real migration cost of
   the inference port — are:
   (a) *diverging-arm refinement*: when one arm of an `if` diverges, the
   parent-region continuation state is the surviving arm's exit state, and
   `refine` placement at the join reproduces today's Pi/`Conditional`
   machinery (it does not disappear; it relocates);
   (b) *irinterp edge-killing* becomes constrained region surgery during
   inference (arm deletion = region inlining, a defined editable operation);
   (c) *backedge refinement*: carried-arg states join with refinement from
   the `continue` condition, or loop-precision regresses vs today's per-edge
   states.
4. **The optimizer is ported.** Simple passes on dense state; SROA/inlining
   via editable sessions and `splice_body!`. The pass pipeline becomes data
   (named passes with `requires`/`preserves` on layout states and dialects),
   replacing `optimize_until` string matching.
5. **The boundary converters are first-class, specified passes**:
   - **Entry** (`IRCode`/`CodeInfo` → UnifiedIR): two modes. *cfg-wrap* —
     the whole body becomes one island (always available, trivially correct,
     the P0 mode); and *structurization* — recover regions from reducible
     goto+phi form (this **absorbs IRStructurizer**; the claim "structure is
     never lost" is true only of native frontends, and until the P3 lowering
     port, everything arrives through this converter).
   - **Exit** (UnifiedIR → `CodeInfo` for codegen/caching): synthesizes
     header/merge `PhiNode`s from loop/if region args and results — de-tupling
     the single-result encoding **for extract-only uses**; a tuple result that
     escapes as a first-class value (passed, stored) is materialized —
     lowers residual cells per the §6 strategy
     (restricted feature matrix through P2; full ordinary+exceptional SSA
     synthesis incl. definedness at P3), and flattens regions to goto form.
     Runs the §8.2 target-legality check. A real pass with its own verifier —
     the "Gone" list (§5.8) is true of the mid-end only until direct codegen
     consumption.
   Both converters publish a feature matrix and exact round-trip properties;
   the differential harness (§12 P0) runs old and new pipelines side by side.

**What gets deleted, eventually** (with review-corrected wording):

| Deleted | Replaced by |
|---|---|
| `slot2ssa.jl` (incl. its `NewSSAValue` reuse) | cell promotion (common cases) + cells staying memory (hairy cases) |
| `IncrementalCompact`, `OldSSAValue`/`NewSSAValue`, pending buffers, refcount oracle | editable state + `compact!` |
| `UseRef`/`userefs` machinery | flat operand words + reference protocol |
| `PhiNode`/`PhiCNode`/`UpsilonNode`/`EnterNode`/`:leave`/`:pop_exception` | region args, `try` regions, cells — in the mid-end; synthesized at the exit boundary until direct codegen |
| eager closure conversion + syntactic `Core.Box` insertion | `closure` ops + shared cells + late materialization (§5.7; from P3) |
| CFG bookkeeping in every pass; `allow_cfg_transforms` | region surgery ops; construction-enforced invariants |
| IRStructurizer (external) | absorbed as the entry converter's structurization mode + native structure from frontends |
| SynchCompiler's `DNode`/`INode`/`IFunction` trees + head validators | dialects + columns + layout states of one IR |
| SynchCompiler's hand-driven inference, most of `SynchInterpreter` | `Queries` + registered kinds + effect tables |
| DAECompiler's side-table fixups, `compiler_reexports.jl` piracy | extension columns; owned Base methods |

Every pain point from §1.1 maps to a section above.

---

## 11. Analyses and verification

### 11.1 Analysis cache

```julia
mutable struct AnalysisCache
    entries::Dict{AnalysisKey,AnalysisEntry}   # key = (analysis type, config)
    stmt_epoch::UInt64      # bumped by stmt insert/delete/replace
    region_epoch::UInt64    # bumped by region/edge mutation
    type_epoch::UInt64      # bumped by type-column writes
    flag_epoch::UInt64      # effects/flags writes
    layout_epoch::UInt64    # state transitions, rename flushes
    # per-column epochs live with the universe's columns (§3.5)
end
```

Two further dependencies are properties, not epochs, and are stated as such:
`valid_worlds` is an IR field only narrowed by defined operations (splice), so
world-sensitive analyses key on it directly; the kind-registry snapshot an IR
was built against is immutable for that IR's lifetime (§3.4), so no registry
epoch is needed.

All mutation flows through the API (§3.1), so epochs cannot be bypassed; cache
queries are illegal during an uncommitted surgery transaction; batch surgery
publishes one event set after invariants are restored. Each analysis declares
which epochs it depends on and optionally `update!(a, ir, event)::Bool` —
absorb the mutation or be invalidated (unblocking the #27547 class).

Standard entries: `LoopInfo` (free from regions), `DomTree` (needed only
inside `cfg` islands), **`PostDomTree`** (finalizer
insertion and irinterp's `visit_conditional_successors` need post-dominance
today, and with early exits the region tree is *not* a post-dominator tree; it
is computed from the region tree + exit summaries), `ExitIndex` (the §5.9
reverse index), `UseCounts` (hook-maintained), `DefUses` (CSR port of
`TwoPhaseDefUseMap`; lazily built, explicitly invalidated, optionally
hook-maintained), `Reachability`. External analyses use the same slots.

**Contract**: analyses and external side tables survive `editable` expansion
(ids unchanged) and must consume the `RemapSet` or be dropped at every
renaming point — no third option; per-statement external state should be a
column (auto-remapped); id-keyed side tables are the discouraged path and
must subscribe to the remap callback. Debug builds tag handles with the
generation (checked on deref) and perturb order keys at `compact!`.

### 11.2 Verification layers

- **L0 structural, O(n), always cheap**: column lengths; kinds registered;
  result-arity discipline (no references to zero-result statements); operand
  tags/pool indices in range and arity-correct per `@stmtkind`; edge-bundle
  encoding well-formed (§3.2); escape constants reference-free or codec'd
  (§3.2); region parent links acyclic; **each ordered region ends in exactly
  one terminator** (floating `guard` regions exempt — §3.3); dense state:
  region spans contiguous and properly nested, `region_arg`s leading their
  span; editable state: lists consistent, keys (if present) increasing along
  links; floating state: no order accessed.
- **L1 SSA/regions semantic**: the visibility rule (all three clauses) for
  every `ssa_use`-role reference, including `GuardCondition` use sites (§3.2);
  activation-boundary rules (no exits or handler-attachment across `deferred`/
  `resume` activations, including edge-defined resume subgraphs, §5.6);
  region-arg counts match `continue`/`break`/`result` arities and `cfg` edge
  bundles match destination block args; no uses of tombstoned defs;
  `gc_preserve_begin`/`end` pairing not split across region boundaries by
  surgery; floating: acyclicity modulo the delayed edge.
- **L2 typing/effects, opt-in, lattice-parameterized**: type column
  consistency with operand types; flag soundness (incl. temporal-identity
  rules for delay-like kinds, §4.3); world validity across `latestworld`
  splits. External dialects plug their lattices in (Synch's clock/init rules
  become L2 rules over its columns).

A key design KPI: the table of invariants made **unrepresentable by
construction** (SoA length skew, non-contiguous blocks, terminator/succ
mismatch, out-of-range pending SSA refs, phis-not-at-top, use-before-def via
phis) versus merely verified — every row moved into the first column is a
class of pass bugs deleted.

---

## 12. Implementation roadmap

Ordered so the riskiest assumptions are tested first, in a
thin end-to-end slice, before the substrate is generalized. Nothing below is
a stabilization milestone (§8.4).

- **P0 — thin vertical differential slice.** Minimal core schema (`if`,
  `loop`, cells, `cfg`, `extract`, the test dialect incl. `test.delay`);
  typed id wrappers; dense (builder + sealed) and editable states with a
  minimal `compact!`/`RemapSet`; **entry converter in cfg-wrap mode** from
  `IRCode`; **exit converter** back to `CodeInfo` for the subset; one scan
  pass and one insertion-shaped pass; a differential execution harness (old
  pipeline vs converted round-trip) on a function corpus; first compile-time
  and memory measurements; L0 **plus a minimal L1 subset** (visibility clause
  3, terminator discipline, edge-bundle arity, the dense structural-deletion
  restriction) so the harness never executes semantically malformed IR.
  *Exit criterion: differential correctness on the corpus + honest first
  numbers against the §13.1 gates.*
- **P1 — structured core, proven.** Full region surgery (transactional);
  **structurization mode** of the entry converter (absorbing IRStructurizer);
  the **exceptions & exits mini-spec** (§6 §5.9: undef vocabulary, promotion
  policy, boundary phi/PhiC synthesis, cross-island `goto`) with adversarial
  tests (partial initialization, nested handlers, loops, rethrow, early
  exits, islands); cell promotion; `splice_body!` (world intersection, pool
  relocation); **compaction/remap fuzzing** over registered columns, deleted
  ids, surgery, callback failure, and stale views (the #10 protocol); L1;
  parser + `@ir_str` tooling. *Exit criterion:
  bounds-check-insertion and inline-splice tests in the pure test dialect;
  round-trip CI on the portable subset; fuzzers quiet.*
- **P2 — external ports.** The Julia core dialect; Synch port (incl. modular
  clocks/`synch.instantiate`, scheduling + legalization split); DAECompiler
  equation frontend; `Queries` via the provider mechanism. Falsify the API
  while breaking changes are free (§8.4). *Exit criterion: SynchCompiler
  test suite green on UnifiedIR without IRStructurizer; consumer code carries
  zero version branches.*
- **P3 — end-state exploration** (the project's center of gravity): a JuliaLowering backend
  targeting UnifiedIR (regions + cells + provenance; closures enter the core
  here; decides Level-2, #9; enumerates the full lowering vocabulary); an
  inference port implementing the three §10.3 mappings; optimizer ports —
  **SROA first, against the #13.1 gate criteria; the make-or-break
  measurement** — then DCE, then inlining via `splice_body!`; DAECompiler AD
  feasibility study (§7).
- **P4 — integration.** Upstream entry (post-inlining segment first, behind
  the gates), reflection routing (`@code_typed`-style display of region IR is
  its own small design), direct codegen consumption, symbolic-identity native
  serialization (§3.4).

---

## 13. Design questions: status and residual risk

1. **(OPEN — gated)** **Hot-path performance.** Dense-scan parity of the
   *access pattern* is by construction, but net cost depends on statement-
   count deltas (result-terminator/extract insertions from diverging-arm threading, §2.1; tuple
   types in the pre-SROA form inference walks), type-erased registry callbacks
   (§8.1), and `type::Vector{Any}` boxing. Concrete gate at P3: (a) ported
   SROA within ~5% of `IncrementalCompact` wall-clock and allocations on a
   compile-time corpus (stdlib inference workload); (b) an inference-shaped
   kernel (kind/operand/type walk) within noise of the `CodeInfo` walk;
   (c) `editable → compact!` round trip on inlining-heavy IR ≤ current
   nested-compaction cost; (d) a real memory tabulation (core columns +
   operand/constant pools vs boxed `Expr`s) favoring UnifiedIR. First numbers
   arrive at P0 (§12). Reserve levers: fused `compact!(f, ir)` rewrite mode,
   INLINE immediates, dense-prefix iteration, arm-continuation
   canonicalization.
2. **(OPEN — evaluate in practice)** **Early exits crossing regions.**
   Representation settled (§5.9, now incl. cross-island `goto`); the residual
   risk is the *pass tax* — code motion and effect summarization consulting
   exit summaries correctly. Needs the worked verifier plus two nontrivial
   ported passes (SROA, DCE) as proof, and a decision on when
   exit-normalization is worth its code-quality cost.
3. **(RESOLVED)** **Multi-result plumbing** — by elimination: statements have
   zero-or-one results; multi-value ops produce one tuple-typed value
   destructured by `extract` (§5.1). A future representational variant may
   add native multi-result transparently behind the accessors.
4. **(SPECIFY in P1 — prerequisite)** **`try` semantics
   completeness.** Structural answers recorded (§6: may-throw predicate,
   nearest-`%exc` for `current_exception`/`rethrow`, cfg-inside-try,
   dynscope operand, undef vocabulary, v1 promotion policy, boundary
   synthesis); the P1 mini-spec + adversarial test suite is the deliverable.
   Residual caution: this is where hidden IRCode behavior most likely lurks.
5. **(OPEN — tracks julia#58532; structured form quarantined)**
   **`await`/continuation and closure semantics.** v1 carries `await` in
   `cfg` form only (§5.6); the structured form needs an operational semantics
   for mid-region resumption points (pending-join context) before it can
   enter the core. The cfg encoding is settled: `await` is a
   block terminator with normal/resume edge bundles, `%C` delivered as the
   normal successor's block argument (hence not visible on the resume path
   unless threaded — settled representationally), activation defined on the
   resume edge. Still tracked with the PR: try interaction, task-bound state
   (dynamic scope, locks, RNG), single- vs multi-shot effects, `argt`
   evolution. For closures
   (§5.7, enters core at P3): the `rec` binder; the definition-time/
   specialization-time split of escaping-closure materialization (type
   identity, world age, serialization).
6. **(RESOLVED)** **Floating↔dense transition rules** — reorderability
   precondition (`REMOVABLE` mask or dialect rules), layout-only `schedule!`
   ending in compaction, separate legalization passes, delay temporal
   identity (§4.3). Verified in practice by the P2 Synch port.
7. **(RESOLVED)** **Provisional region assignment during clock inference** —
   two-phase: column-authoritative unification, then one `assign_regions!`
   reification (§7).
8. **(RESOLVED)** **Constant pool identity** — intern by `===` only
   (`IdDict`-keyed): egal dedup is unconditionally semantics-preserving, and
   egal *is* identity for mutables. Never dedup by `==`. Small bits values go
   `INLINE`; `compact!` collects unreferenced entries.
9. **(GO — decided 2026-07-10)** **SyntaxGraph unification depth (§3.7).**
   Level 1 (`AttrGraph` substrate + the shared tree porcelain over the
   node-reference projection) built and verified against the vendored
   JuliaSyntax/JuliaLowering suites. Level 2 — redefined after
   review as *shared substrate, two namespaces, extraction-based emission*
   (the flat shared arena is dead) — pulled forward from P3 by review: the
   region-structured lowering emitter already embodies the extraction
   architecture, so Level 2 proceeds now as the §3.7 four-step increment
   (one registry, graph-qualified provenance, AST-lifetime GC, one
   printer/verifier stack). The Level-2 gate still explicitly
   includes the cross-graph provenance lifetime contract (§3.6): frozen vs
   rooted vs independently-compacted source graphs, and node-remap
   subscription if the latter.
10. **(SPECIFIED — P1 fuzzing)** **Stale-id discipline at renaming
    points.** `compact!`/`schedule!` return a `RemapSet` over all namespaces;
    cache entries consume it or are dropped; columns over side tables; typed
    wrappers at API boundaries; the concrete handle model of §3.1 (one shared
    body owner carrying state tag + generation, checked per *operation*;
    debug-fattened row cursors checked per dereference; key perturbation);
    production relies on the contract (no per-dereference production checks;
    a violated release contract may silently alias — a deliberate decision). P1's fuzzers must show no leaks and must assert the
    strong exception guarantee (§4.1) on callback failure — this is where a
    #46945-class bug would reappear, one level up.
11. **(RESOLVED)** **Naming: `UnifiedIR`.** Vocabulary per #49057, fixed
    once: the row is a `stmt`, scoping is a `region`, an SSA reference is a
    `Value` (= `StmtId`); "instruction" and "node" do not appear in the API.
12. **(SPECIFIED — v1 sealed)** **Region kinds and activation boundaries**
    (§3.3, §5.1): structural role and activation are orthogonal dimensions
    (`RegionKind` × `Activation` — a single enum cannot express a
    resume-entry block or a handler inside a deferred body); resume
    activation is edge-defined for cfg `await`; rules for
    exits, handler attachment, capture, and DCE composition; region ownership
    sealed to core kinds (`await` is not an owner), guard regions
    registrable. Full foreign-region-owner descriptors are future work.
13. **(SPECIFIED)** **Kind-registry identity and serialization** (§3.4):
    session-local numerics, symbolic persistence with load-time relocation,
    parameterized bit split, defined collision/capacity behavior; precompiled
    external-dialect kind literals read a registration-populated binding
    (core-dialect literals stay true constants); immutable registry snapshot
    per compilation.
14. **(OPEN — P3)** **The inference mapping** (§10.3): diverging-arm
    refinement/`refine` placement, irinterp-as-surgery, backedge refinement
    precision. The named migration cost of the inference port.
15. **(OPEN — future optimization)** **Exceptional-SSA promotion** beyond the
    v1 policy (§6): promoting cells across throw edges requires
    per-throwing-prefix join machinery (the PhiC information, formalized). Not
    load-bearing; the v1 policy plus boundary synthesis reproduces today's
    behavior.
16. **(SPECIFIED — staged)** **Residual-cell exit lowering** (§6, §10.5):
    general residual cells are not a 1:1 Upsilon/PhiC
    map (normal-path reads/writes, multiple reads, `cell_new`, definedness).
    Strategy: P0–P2 restrict the exit converter's feature matrix to cells
    promotable before exit (explicit, erroring); P3 implements full synthesis
    of ordinary + exceptional SSA with definedness — `slot2ssa`'s algorithm
    relocated to the boundary and run once. Escaping tuple results are
    materialized; de-tupling applies to extract-only uses.

---

## Appendix A: file references

- predicated.md: `git -C OldDAECompiler.jl show da5858f:design/predicated.md`
- IRCode/IncrementalCompact: `julia/Compiler/src/ssair/ir.jl` (esp. `:287`,
  `:1109`), `verify.jl:246`, `legacy.jl`; slot2ssa undef machinery:
  `julia/Compiler/src/ssair/slot2ssa.jl:541-651`
- Dual-mode packaging: `julia/Compiler/src/Compiler.jl`; effect bits:
  `julia/Compiler/src/optimize.jl` (`IR_FLAG_*`)
- ECS columns: `julia/JuliaSyntax/src/porcelain/syntax_graph.jl`; kind bit
  split: `julia/JuliaSyntax/src/julia/kinds.jl`;
  `julia/JuliaLowering/src/ast.jl`; goto/try lowering:
  `julia/JuliaLowering/src/linear_ir.jl` (jump-into-try error at `:209`;
  `compile_leave_handler` emitting `K"leave"` for outward jumps)
- SynchJulia: `SynchJulia.jl/src/irbase.jl`, `src/dataflow.jl`,
  `src/dataflow/clocks.jl` (`ClockInstantiation` at `:203`),
  `SynchCompiler/src/{interpreter.jl,imperative/from_julia.jl,dataflow/types.jl}`
- DAECompiler AD/tearing surface:
  `OldDAECompiler.jl/src/analysis/index_lowering_ad.jl`,
  `src/transform/tearing_schedule*.jl`
- Issues: #41476, #31603, #27547, #46945, #53011/#53013, #53640, #34229,
  #54762, #50285, #31162, #61510, #58000, #49057, #48818, #47349, #15276
- PRs: julia#58532 (`await`/`AwaitNode` delimited continuations — §5.6)
- Precedents: Cranelift `DataFlowGraph`/`Layout` split; MLIR regions/dialects;
  RVSDG (arXiv 1912.05036); MLIR early-exit RFC (discourse.llvm.org/t/76998);
  ClangIR structured CF + FlattenCFG (llvm.github.io/clangir); WebAssembly
  typed multi-level `br`; Polygeist flag-encoding; Flang
  structured/unstructured lowering split

## Appendix B: acceptance tests for the prototype


- An `if`, `loop`, or `closure` body cannot use its owner's result without an
  explicit legal binder (visibility clause 3 negative tests).
- Guard conditions appear in def-use, scheduling, liveness, and every
  `RemapSet`.
- Stale statement/region handles fail deterministically after a renaming
  point *in debug builds*; no old integer silently aliases a new entity
  through the typed public API. (Production: contract + fuzzing, §2.3.)
- Compaction fuzzing covers arbitrary registered columns, deleted ids, region
  surgery, callback failure, and aliased stale views — and asserts the strong
  exception guarantee: after an injected callback failure, the IR is
  logically unchanged (not merely "no crash").
- Dense `delete_stmt!` refuses terminators, region owners, `region_arg`s, and
  guard conditions.
- `await`'s normal and resume edges enumerate as CFG successors with the
  specified dominance/liveness split; `%C` is defined only along the normal
  edge.
- Replacing a call invalidates `CallInfo` and other `:derived` columns unless
  the pass explicitly transfers them.
- `br_if` and `switch` round-trip two or more successor edge bundles with
  block arguments and no implicit fallthrough.
- `@goto` out of a `try` — including out of a `catch` — round-trips through
  the boundary converters with correct leave/pop behavior.
- Floating conversion rejects pure-but-throwing and potentially nonterminating
  operations; independent delays retain distinct temporal identities.
- Exception tests distinguish throws before and after a cell store
  (`cell_isdefined` in handlers) and match the existing boundary IR after
  promotion + exit synthesis.
- Deferred closure effects and exits do not leak into the creation site's
  handler or loop (activation-boundary L1 tests).
- `await` in `cfg` form: frame-class cells copied per resumption,
  shared-class cells shared; DCE removes an unused continuation and its
  resumption block.
- Inlining intersects world validity and relocates constants, globals, static
  parameters, columns, and both provenance tiers.
- Text parsing rejects unsupported identity-bearing constants cleanly rather
  than claiming a false round trip.
- A differential end-to-end benchmark (P0 harness) reaches the §13.1 gates
  before the P4 integration step.
