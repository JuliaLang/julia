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
to throwaway UnifiedIR — captured variables as frame cells, each
closure-creation site as a marker call holding one `cell_get` per captured
variable — and runs `UnifiedIR.promote_fixpoint!`, the identical pass
pipeline `Compiler.Unified` runs inside `optimize_ir!`. Shared functions,
not parallel logic.

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
  loop (a fresh binding per iteration, `local x` in the loop body);
- **(c)** a single defined value reaches `C` — *joins are fine; that is the
  point*. This is exactly cell promotion: the verdict is "did the fixpoint
  resolve the site's `cell_get` to a reaching definition". Arm joins come
  from `promote_arm_cells!`, loop-carried values from `promote_loop_cells!`,
  exception joins from `promote_try_cells!`.

Maybe-undef captures never become values: the analysis runs the fixpoint
**without** `promote_undef_cells!` (definedness-as-data would manufacture a
value where the program has undefined memory), so an unresolved read keeps
the shared cell and `UndefVarError` stays a *use-time* error.

When sharing is required, the container is **typed** where lowering can
prove the value type: a declared type that resolves to a constant (stores
are already funneled through `convert`), or all store right-hand sides are
literals of one concrete type. The variable must additionally be
*undef-safe* (every read and every capture store-dominated — the shared
`dominates_for_cell` checker — since e.g. `RefValue{Int}`'s empty state is
unobservable). Then the closure field is `Base.RefValue{T}` instead of
`Core.Box`.

## The zoo

Run `demo/capture_zoo.jl` under the built binary for the live table,
execution differential, and microbenchmarks. Decisions:

| # | shape | stock | ours | why |
|---|-------|-------|------|-----|
| 1 | `if c; x=1 else x=2 end; ()->x` | Box | **value** | (c) arm join; no store after the site |
| 2 | `try x=f() catch; x=g() end; ()->x` | Box | **value** | (c) try join (`promote_try_cells!`); defined on both paths |
| 3 | `x=1; f=()->x; x=2` | Box | **shared, `RefValue{Int}`** | (b) fails — `f()` must see `2`; literal join types the container |
| 4 | closure in loop, `x=i` later in body | Box | **shared**, `RefValue{Int}` with `local x::Int` | (b) fails via the backedge — multi-shot closures see later iterations |
| 5 | counter `()->(x+=1)` | Box | **shared**; `RefValue{Int}` when declared `local x::Int` | (a) fails; `x+1` is not a literal, so only a declared type can type the container |
| 6 | `local x; if c; x=1 end; ()->x` | Box | **shared, `Core.Box`** | (c) fails (maybe-undef); Box preserves use-time `UndefVarError` |
| 7 | conditional assignment feeding a comprehension | Box | **value** | same as 1 through the generator closure |

Cases 3, 4, 6 are seeded as regression sentinels
(`JuliaLowering/test/capture_analysis.jl`): a wrong-direction decision there
is a silent miscompile, so they assert both the decision *and* the observable
semantics (mutation visibility, multi-shot reads, undef errors).

The payoff shows in inference: case 1's return type goes `Any → Int64`,
case 2 `Any → Float64`, case 7 `Vector → Vector{Int64}`; a declared-type
counter (case 5) infers `Int` end to end. Escaping-closure microbenchmarks
run ~14–30× faster (see the demo).

## Honest limits (what still boxes, and why)

- **Stores with non-literal types on undeclared variables** (`x = x + 1`,
  `x = f(y)`): no type is provable at lowering time, so the container stays
  `Core.Box`. Typing these needs inference-time materialization — the §5.7
  `closure`-op plan, where the capture set is fixed at definition but the
  layout stays type-parameterized.
- **Maybe-undef shares** stay `Core.Box` by design (undef must be
  observable).
- **Captures read inside `catch` blocks** and other handler-observed cells
  keep memory form (§6 throw-edge rule).
- **Recursive/mutually-recursive local functions** self-capture before their
  own definition completes: shared by construction (the future `rec` binder,
  §5.7 design question #5).
- **Arguments** never get typed containers (their entry value's type is not
  a lowering-time fact); precision (value capture) still applies.
- Forms the analysis emitter does not model (`@goto`, crossing-`finally`
  bodies, …) bail per-lambda to the stock syntactic verdicts —
  `UnsupportedForm` is a legitimate exit, never a mis-lowering.
