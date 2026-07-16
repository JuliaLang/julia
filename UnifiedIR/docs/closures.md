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
required, the container is the untyped `Core.Box`, exactly as stock:
lowering never types a container (resolving a declared type reads the
binding table; joining store types is inference). Typed-cell
materialization for unavoidable shares belongs to the compiler's
late-materialization pipeline (the §5.7 definition/specialization split),
after inference.

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
| 3b | `x=1; f=()->x; f(); x=2` | Box | **shared, `Core.Box`** | the use blocks the sink; (b) fails — the second `f()` must see `2` |
| 4 | closure in loop, `x=i` later in body | Box | **shared, `Core.Box`** | (b) fails via the backedge — multi-shot closures see later iterations (creation feeds `push!` in the same statement: nothing to sink) |
| 5 | counter `()->(x+=1)` | Box | **shared, `Core.Box`** | (a) fails — the closure writes its capture |
| 6 | `local x; if c; x=1 end; ()->x` | Box | **shared, `Core.Box`** | (c) fails (maybe-undef); Box preserves use-time `UndefVarError` |
| 7 | conditional assignment feeding a comprehension | Box | **value** | same as 1 through the generator closure |

Cases 3b, 4, 6 are seeded as regression sentinels
(`JuliaLowering/test/capture_analysis.jl`): a wrong-direction decision there
is a silent miscompile, so they assert both the decision *and* the observable
semantics (mutation visibility, multi-shot reads, undef errors). Case 3 is
the sinking sentinel in the other direction: its differential proves the
sunk snapshot sees the post-store value (2, never the stale 1 that a
decision-at-sunk/emission-at-original mismatch would produce), and a seeded
source-level fuzz battery generates the store-after-creation class heavily
against stock execution.

The payoff shows in inference: case 1's return type goes `Any → Int64`,
case 2 `Any → Float64`, case 7 `Vector → Vector{Int64}`. Escaping-closure
microbenchmarks run ~14–30× faster (see the demo).

## Honest limits (what still boxes, and why)

- **Unavoidable shares are untyped** (`Core.Box`), whatever the stores look
  like: typing the cell is inference-time materialization — the §5.7
  `closure`-op plan, where the capture set is fixed at definition but the
  layout stays type-parameterized. Lowering only picks the SET of shares
  precisely; the compiler pipeline types them.
- **Maybe-undef shares** stay `Core.Box` by design (undef must be
  observable).
- **Captures read inside `catch` blocks** and other handler-observed cells
  keep memory form (§6 throw-edge rule).
- **Recursive/mutually-recursive local functions** self-capture before their
  own definition completes: shared by construction (the future `rec` binder,
  §5.7 design question #5).
- Forms the analysis emitter does not model (`@goto`, crossing-`finally`
  bodies, …) bail per-lambda to the stock syntactic verdicts —
  `UnsupportedForm` is a legitimate exit, never a mis-lowering.
