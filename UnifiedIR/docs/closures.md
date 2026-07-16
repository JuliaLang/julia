# Closure captures: the precise decision, worked

Julia closures capture the **variable**, not its value. Stock (flisp)
lowering decides value-vs-shared capture with a syntactic rule — essentially
"assigned more than once ⇒ untyped `Core.Box`" — which is the root cause of
the julia#15276 boxing-pessimism class: provably-safe captures get an
`Any`-typed heap cell and inference gives up.

JuliaLowering now computes the precise answer with the **same mem2reg
machinery** the UnifiedIR optimizer uses (`UnifiedIR/src/promote.jl`, §6 of
the design): `analyze_captures_precise!`
(`JuliaLowering/src/unified/capture_analysis.jl`) lowers each enclosing body
to throwaway UnifiedIR — captured variables as `cell_shared` cells, each
closure-creation site as a real §5.7 `closure` region op whose deferred
body is the site's capture footprint — and runs
`UnifiedIR.promote_fixpoint!`, the identical pass pipeline
`Compiler.Unified` runs inside `optimize_ir!` (now including
`promote_capture_cells!`). Shared functions, not parallel logic.

## The criterion

For variable `v` and closure-creation site `C`, **value capture is legal**
iff:

- **(a)** no lambda capturing `v` stores to it — a tree fact from scope
  analysis;
- **(b)** no store to `v` can execute after `C`: forward order in the region
  tree (`comes_before` + `_may_reach`; stores in the *sibling arm* of an
  `if` are mutually exclusive with `C` and don't count), **plus** the
  multi-shot rule — a store sharing a loop with `C` executes again on the
  next iteration and *is* observable, unless `v` is re-declared inside that
  loop (a fresh binding per iteration, `local x` in the loop body — at the
  IR level, the `cell_shared` statement itself sits inside the loop, so
  each iteration works a fresh shared box);
- **(c)** a single defined value reaches `C` — *joins are fine; that is the
  point*. This is exactly cell promotion: the verdict is "did the fixpoint
  resolve the site's `cell_get` to a reaching definition". Arm joins come
  from `promote_arm_cells!`, loop-carried values from `promote_loop_cells!`,
  exception joins from `promote_try_cells!`.

**The representation: real `closure` regions, decided by
`promote_capture_cells!`.** Creation sites are §5.7 `closure` ops — the
captured reads live *inside* the deferred body, behind the activation
boundary — and the capture-promotion pass (`UnifiedIR/src/promote.jl`,
part of `promote_fixpoint!`) implements the criterion directly on that
form: (a) and (b) are checked on IR positions (the backedge cancellation
is the cell's own declaration sitting inside the shared loop — a fresh
shared box per iteration, which is also how the emitter renders
per-iteration `local`), and (c) is judged by the standard fixpoint on a
scratch copy: the candidate is speculatively demoted to a frame cell with
one probe `cell_get` per creation site, and a probe the join passes
resolve proves definite assignment and names the reaching value. The
commit rewrites the in-deferred reads to the (home-frame) probe — a legal
by-visibility capture — and the cell dissolves through ordinary
promotion. In `analyze_captures_precise!`'s throwaway IR the closure
bodies are the capture FOOTPRINT (one `cell_get` per captured variable,
plus a synthetic store for lambda-written ones, making criterion (a)
structural); the per-variable verdict is whether the `cell_shared`
survived with an in-deferred use. The UnifiedBackend's default lowering
path emits full closure regions the same way and MATERIALIZES the
residuals back to runtime closure types (`unified/materialize.jl`).

Maybe-undef captures never become values: the analysis runs the fixpoint
**without** `promote_undef_cells!` (definedness-as-data would manufacture a
value where the program has undefined memory), so an unresolved read keeps
the shared cell and `UndefVarError` stays a *use-time* error.

**The envelope: what lowering may optimize.** Keno's rule, verbatim:

> Here's the optimizations lowering is allowed to do: isdefined->Bool,
> sinking closure definitions (as long as they haven't been used yet), and
> anything else that's visible structurally. It's not allowed to do
> anything that would need to do inference (because it's not allowed to
> read the method or binding table).

The capture criterion above is squarely inside that envelope — it is
structural mem2reg on positions and joins, never on types. When sharing is
required, lowering never *types* the container (resolving a declared type
reads the binding table; joining store types is inference). Typed-cell
materialization for unavoidable shares belongs to the compiler's
late-materialization pipeline (the §5.7 definition/specialization split),
after inference.

**The share representation: closures as mutable structs.** The container
CHOICE for what remains shared is separately authorized, verbatim:

> For better performance, it is fine to lower closures to mutable struct
> where mutably captured values become mutable fields and everything else
> `const`. That eliminates `Core.Box` while preserving semantics. It's not
> possible to type the closure field, because any later assignment in the
> closure body could read an updated world table that we know nothing
> about, so we don't know what the type is gonna be.

So a variable that must stay shared and is mutably captured by **exactly
one** closure merges its container INTO that closure:
`analyze_merged_captures!` (`JuliaLowering/src/closure_conversion.jl`, run
at the end of scope resolution, after sinking and the capture analysis)
marks it, the closure type becomes a `mutable struct` whose value captures
stay `const` fields (type-parameterized as always) while each merged
variable becomes an **untyped mutable field** — never type-annotated, per
the world argument above. One allocation and one indirection instead of two
(`Core.Box` + closure). Accesses split around the creation: inside the
closure they are `getfield`/`setfield!` on `#self#`; home-frame accesses
*after* the creation go through the closure's binding (`getfield(f, :x)` /
`setfield!(f, :x, v)` — sound because applicability requires the creation
to dominate them); accesses *before* it use the plain local slot, whose
value initializes the field at `new`. Declared-type semantics are
unchanged: stores keep funneling through `convert` + `typeassert` into the
field (an `InexactError` surfaces exactly as stock), and — as with the Box
— reads are NOT type-asserted (stock flisp asserts reads of declared-type
boxed locals, a deliberate parity gap recorded below: runtime behavior is
identical because every store converts; read-side precision is the typed
compiler pipeline's job). Maybe-undef variables merge too, as the
*uninitialized trailing field* flavor: such fields are ordered last so the
partial `new` is legal, creation runs `isdefined(x) && setfield!(f, :x, x)`,
reads keep the #20016 named-variable guard (so `UndefVarError` still names
`x` at use time), and `@isdefined x` maps to `isdefined(f, :x)`.

The v1 applicability is purely structural; **any failure keeps the
classical `Core.Box`**:

- exactly one closure binding captures the variable (checked on the
  closure-bindings table AND on the tree: no occurrence under any foreign
  lambda — cross-closure sharing keeps the Box; a shared *frame struct* for
  closure groups is future work);
- the variable is not itself a closure binding (recursion/self-capture
  keeps the Box: the container must exist before the instance does);
- the closure binding is a plain local assigned only by its unique
  `function_decl` (home reads through it must see THE instance);
- every home occurrence classifies as before-the-creation (an earlier
  sibling subtree at the path divergence) or dominated-after (a later
  sibling of a block whose chain down to the `function_decl` is
  completion-transparent); exclusive `if`-arms, handler positions, and
  signature material refuse;
- the nearest enclosing loop of the variable's declaration is the nearest
  enclosing loop of the creation (one activation ↔ at most one instance;
  this is what keeps the multi-shot loop shapes on `Core.Box`);
- no `@label`/`@goto` in the frame.

Both lowering paths apply the same verdicts: the tree path emits the
mutable struct directly (`eval_closure_type` field kinds; `new` leaves
maybe-undef fields uninitialized), and the region path's materializer
(`unified/materialize.jl`) turns a merged residual `cell_shared` into the
mutable field — in-body cell ops become field ops on `#self#`, the `new`
takes the value the cell holds at creation, later home cell ops become
field ops on the instance, and the leftover init-phase cell is demoted and
dissolved by a final promotion fixpoint (no container allocation remains).
The region path is more conservative on one point: maybe-undef variables
keep the Box there (the conditional field initialization needs an `if`
region planted mid-surgery; the tree path handles the flavor fully).

One reflection-visible consequence is sanctioned by the authorization
itself: `ismutable` on such a closure returns `true` where stock says
`false` (and `===` on separately-created instances is unaffected — the
multi-instance shapes are exactly what the loop rule refuses).

**Sinking: the authorized code motion.** Before the capture analysis runs
— and before either lowering path reads the tree —
`sink_closure_definitions!` (`JuliaLowering/src/closure_conversion.jl`)
moves each pure closure-creation statement down its enclosing block to
just before the first statement that mentions any binding it assigns.
Creation is pure and nothing has observed it yet, so the motion is
unobservable — and a store that sat between creation and first use now
lands *before* the sunk creation, where criterion (b) holds: `x = 1;
f = () -> x; x = 2; f()` becomes a VALUE capture of `2`. The soundness
coupling is by construction: the capture decision and the emitted creation
position cannot disagree, because every consumer — the capture-analysis
IR, `convert_closures`, and the region emitter — reads the same
already-sunk statement order; the position rule has exactly one
implementation. v1 is deliberately conservative: whole statements only,
same block only, method bodies only (never toplevel thunks, where
closure-type definition and world-age effects pin the position). Every
occurrence of the created bindings outside the statement must sit in a
later statement of the same block (a capture by another closure,
`@isdefined f`, an alias `g = f` — all uses; declaration markers are not);
skipped statements may not mention them anywhere in their subtree (nested
lambdas included), may not carry `@label`/`@goto` (a label could admit
control between the old and new positions), and a skipped `function_decl`
counts as mentioning everything its closure *captures* — the instance
materializes at the decl and reads each captured binding there, a mention
outside the decl's subtree when the methods sit in separate `method_defs`
statements (kwargs closures split this way). Skipping statements that may
throw or exit early is fine: on such a path control leaves the block and,
by confinement, nothing that can observe the binding is ever reached — a
loop re-enters the block from the top and re-runs the sunk creation before
any use, preserving per-iteration lifetimes.

## The zoo

Run `demo/capture_zoo.jl` under the built binary for the live table,
execution differential, and microbenchmarks. Decisions:

| # | shape | stock | ours | why |
|---|-------|-------|------|-----|
| 1 | `if c; x=1 else x=2 end; ()->x` | Box | **value** | (c) arm join; no store after the site |
| 2 | `try x=f() catch; x=g() end; ()->x` | Box | **value** | (c) try join (`promote_try_cells!`); defined on both paths |
| 3 | `x=1; f=()->x; x=2; f()` | Box | **value** | the creation sinks past the store; (b) holds at the sunk position — the snapshot sees `2` |
| 3b | `x=1; f=()->x; f(); x=2` | Box | **shared, mutable field** | the use blocks the sink; (b) fails — the second `f()` must see `2`; single capturer ⇒ the share merges (`setfield!(f, :x, 2)` at home) |
| 4 | closure in loop, `x=i` later in body | Box | **shared, `Core.Box`** | (b) fails via the backedge — multi-shot closures see later iterations; the merge is refused too (x scoped outside the repeating creation: all instances must alias ONE location) |
| 5 | counter `()->(x+=1)` | Box | **shared, mutable field** | (a) fails — the closure writes its capture; single capturer ⇒ merged (declared-type variant keeps the convert funnel through `setfield!`) |
| 6 | `local x; if c; x=1 end; ()->x` | Box | **shared, mutable field (maybe-undef flavor)** | (c) fails (maybe-undef); the uninitialized trailing field preserves use-time `UndefVarError` naming `x` |
| 7 | conditional assignment feeding a comprehension | Box | **value** | same as 1 through the generator closure |

Cases 3b, 4, 6 are seeded as regression sentinels
(`JuliaLowering/test/capture_analysis.jl`): a wrong-direction decision there
is a silent miscompile, so they assert both the decision *and* the observable
semantics (mutation visibility, multi-shot reads, undef errors). Case 3 is
the sinking sentinel in the other direction: its differential proves the
sunk snapshot sees the post-store value (2, never the stale 1 that a
decision-at-sunk/emission-at-original mismatch would produce), and seeded
source-level fuzz batteries generate the store-after-creation class AND the
merged mutable-field class (write captures, maybe-undef writes,
cross-closure sharing, per-iteration loop mutation, post-creation home
traffic) heavily against stock execution.

The payoff shows in inference: case 1's return type goes `Any → Int64`,
case 2 `Any → Float64`, case 7 `Vector → Vector{Int64}`. Escaping-closure
microbenchmarks run ~14–30× faster; the merged mutable field removes the
Box allocation and one indirection on every shared-capture closure (see
`bench_mutcap` in the demo).

## Honest limits (what still boxes, and why)

- **Unavoidable shares are untyped**, whatever the stores look like —
  merged mutable fields deliberately so (the world argument above): typing
  the location is inference-time materialization — the §5.7 `closure`-op
  plan, where the capture set is fixed at definition but the layout stays
  type-parameterized. Lowering only picks the SET of shares precisely and
  the best untyped REPRESENTATION; the compiler pipeline types them.
- **Cross-closure shares keep `Core.Box`**: two closures over one variable
  need a location both can alias — v1 has no shared frame struct (future
  work, noted in the TODO).
- **Multi-instance loop shares keep `Core.Box`**: a variable scoped outside
  a loop that re-creates the closure inside it (zoo 4) — every instance
  must alias one location. The per-iteration `local` shape (variable and
  creation under the same loop) merges fine.
- **Maybe-undef shares** stay *observably undefined* by design: merged as
  uninitialized trailing fields in the tree path (guarded reads keep the
  use-time `UndefVarError` with the right name), `Core.Box` in the region
  path (v1 conservatism).
- **Captures read inside `catch` blocks** and other handler-observed cells
  keep memory form (§6 throw-edge rule); a creation inside a `try` with
  accesses in the handler or beyond refuses the merge the same way.
- **Recursive/mutually-recursive local functions** self-capture before their
  own definition completes: shared by construction, and the self-share keeps
  `Core.Box` — the container must exist before the instance (the future
  `rec` binder, §5.7 design question #5).
- Forms the analysis emitter does not model (`@goto`, crossing-`finally`
  bodies, …) bail per-lambda to the stock syntactic verdicts —
  `UnsupportedForm` is a legitimate exit, never a mis-lowering. The merge
  analysis likewise refuses whole frames containing `@label`/`@goto`.

## The late pipeline: inferring what lowering leaves untyped

Runtime containers stay untyped by design. Typing a generic closure's field
is not possible: **"any later assignment in the closure body could read an
updated world table that we know nothing about, so we don't know what the
type is gonna be. That kind of optimization only works on the
late-generated closures from the await mechanism."** A nominal closure type
is world-persistent — a typed field would bake an inference-world fact into
it, and a semantically legal later-world store would throw instead of
proceeding. Typed layouts are therefore reserved for late-generated
closures (`await`'s per-specialization continuations, §5.6), which never
outlive their inference world.

What CAN be typed, soundly, is the in-world dataflow.
`Compiler.Unified.typed_region_ir!` (the experimental late pipeline)
consumes the backend's PRE-materialization region IR — real §5.7 `closure`
ops and residual `cell_shared` cells, after the capture-decision fixpoint —
and runs unified inference extended with four pieces (no λ-lattice
element):

- **Deferred descent.** A closure body is typed "as if", inside the
  enclosing frame's fixpoint: value captures are SSA (fixed values), cell
  captures read the content join, params come from the join of argtypes
  over visible call sites (declared/`Any` for escapees). The walk runs
  behind a refinement barrier (creation-time flow facts are invalid at call
  time) with rettype/effects isolated (a body `return` binds to the
  closure; deferred effects surface at call sites, not the creation site —
  §3.3).
- **Shared-cell content fixpoint.** Every store to the variable is
  syntactically present in the enclosing IR — only the method body can name
  the local — so the content type is a join over ALL `cell_set` sites, home
  and deferred alike, in the same monotone accumulator (and under the same
  widening escalator) the engine already uses for frame cells.
  Self-referential stores (`x = x + 1`) iterate to a fixpoint. Maybe-undef
  affects definedness, not the type: the `cell_isdefined` /
  `throw_undef_if_not` guards pass through untouched.
- **Call-site result refinement.** A call whose callee operand's def is a
  visible `closure` op takes the body's inferred return-type join and the
  body's effects mask. Arity-incompatible calls of a visible closure are
  `Union{}` (guaranteed throw — dispatch arity is fixed at definition, and
  later-added methods are invisible without a world barrier).
- **The escape/world discipline** — what keeps the in-world answer sound:
  - a closure value that flows anywhere but the callee position of a call
    ESCAPES: its params degrade to declared/`Any`, and every cell it
    captures is *poisoned* — the materialized closure's untyped mutable
    capture fields are settable by any holder, so poisoned-cell reads are
    `Any` even when every visible store is monomorphic (the joins are still
    computed, for diagnostics and the future await consumer — they are just
    never used for refinement);
  - a `latestworld` barrier that may execute after a closure's creation
    (same position machinery as capture criterion (b): forward order +
    reach, shared loops always hazardous) makes the closure *world-shifted*:
    no refinement of any kind — its body may run against a newer
    method/binding table (§5.8). Redefinition within a world stays covered
    by the ordinary invalidation contract, like all inference.

The payoff is the class lowering cannot type: zoo5b's undeclared counter
(`x = 0; inc = () -> (x = x + 1); inc(); inc(); inc(); x`) — `Core.Box` at
runtime, `Any` in stock inference — infers `Int64` end-to-end (the cell
join converges `Const(0) ∨ Int64 → Int64`, `inc()` refines to `Int64`, the
home read follows). The declared-type counter (zoo5) infers the same way,
with the declared type arriving structurally through the emitted
`convert`/`typeassert` store funnels rather than through any lowering-side
type fact.

Honest limits (v1): a never-called, non-escaping closure's stores still
join (walk-everything conservatism); splatted closure calls
(`Core._apply_iterate`) count as escapes; closure values crossing `cfg`
block boundaries travel through cells/block args and get no callee
refinement; recursive local functions self-capture through a shared cell
whose content is the closure value (`Any`) — calls through it are opaque;
`isa`/typeassert refinements never attach to shared cells (a visible call
in between may store); the engine-wide `latestworld` coarseness for
straight-line home code predates this pipeline and is unchanged. No
materialization happens in the pipeline: differentials interpret the
region IR itself (closure ops execute natively as `UClosure`s).
