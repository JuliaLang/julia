# `EscapeAnalysis`

`Compiler.EscapeAnalysis` is a compiler utility module that aims to analyze
escape information of [Julia's SSA-form IR](@ref Julia-SSA-form-IR) a.k.a. `IRCode`.

The current implementation is a local, flow-sensitive analysis. Its algorithmic
shape is inspired by prior work on partial escape analysis[^PEA], but adapted to
Julia's SSA IR and compiler infrastructure. It aims to:

- leverage Julia-level IR semantics when reasoning about object allocation,
  field contents, aliasing, calls, and exception edges;
- provide precise enough local facts for memory-related optimizations such as
  alias-aware SROA, load forwarding, allocation sinking, early `finalize`
  insertion, and stack allocation preconditions.

Inter-procedural alias analysis is also a long-term target, following the same
broad motivation as partial escape analysis, but it is not implemented by the
local analysis described here. See
[Calls and Inter-Procedural Summaries](@ref EA-Inter-Procedural-Summaries) for
the current call-boundary behavior and future direction.

## Try it out!

You can try the escape analysis by loading the `EAUtils.jl` utility script. It
provides the convenience entries `code_escapes` and `@code_escapes` for testing
and debugging:

```@repl EAUtils
let JULIA_DIR = normpath(Sys.BINDIR, Base.DATAROOTDIR, "julia")
    include(normpath(JULIA_DIR, "Compiler", "test", "EAUtils.jl"))
    using .EAUtils
end

result = code_escapes((Base.RefValue{String},String,String,String)) do r1, s2, s3, s4
    r2 = Ref(s2)
    r3 = Ref{String}()
    length(s3) > 0 && (r3[] = s3)
    r4 = Ref(s4) # return escape
    try
        s1 = r1[]
        ret = sizeof(s1)
    catch err
        global GV = err # `r1` may escape
    end
    s2 = r2[] # no throw
    s3 = r3[] # may throw
    s4 = r4[] # no throw
    return s2, s3, s4, r4
end
```

The symbols printed next to call arguments and SSA statements represent the
following escape states:

- `◌` (plain): this value is not analyzed because its escape information is not
  needed, for example because the value is an identity-free bits value
- `✓` (green or cyan): this value does not escape according to
  `has_no_escape(result[x])`, colored cyan when it is also an argument visible
  from the caller according to `has_arg_escape(result[x])`
- `↑` (blue or yellow): this value may escape to the caller via return according
  to `has_return_escape(result[x])`, colored yellow when it may also escape via
  an unhandled thrown edge according to `has_thrown_escape(result[x])`
- `X` (red): this value may escape somewhere the analysis cannot model, for
  example global memory, according to `has_all_escape(result[x])`
- `*` (bold): this value is between return escape and all escape in the partial
  order of [`EscapeInfo`](@ref Base.Compiler.EscapeAnalysis.EscapeInfo), colored
  yellow when it may also escape through an unhandled thrown edge
- `′`: this value has tracked per-field memory-content facts in its
  `ObjectInfo`

Escape information for an argument or SSA value can also be inspected
programmatically:

```@repl EAUtils
result[Core.Argument(2)] # escape info of `r1`
result[Core.SSAValue(2)] # escape info of `SSAValue(2)` (i.e. `r3`)
```

## Analysis Design

### Lattice Design

`EscapeAnalysis` is implemented as a data-flow analysis over
[`EscapeInfo`](@ref Base.Compiler.EscapeAnalysis.EscapeInfo). Each tracked
argument or SSA value has an `EscapeInfo` value with four independent
components:

- `ReturnEscape::Bool`: whether the value can escape to the caller via return
- `ThrownEscape::Bool`: whether the value may be thrown as an exception and
  escape through an exception edge
- `ObjectInfo::ObjectInfo`: memory-content facts associated with fields of the
  value, when such fields can be tracked precisely
- `Liveness::Liveness`: statement numbers at which the value must remain live,
  including the special program counter `0` that marks call arguments as visible
  from the caller

The object-memory component has three main cases:

- `NoMemoryContents`, the bottom element, represents the absence of
  field-content facts
- `HasIndexableFields(fields)` stores a vector of per-field
  [`MemoryInfo`](@ref Base.Compiler.EscapeAnalysis.MemoryInfo) values
- `UnknownMemoryContents`, the top element, represents untrackable or
  unanalyzable memory contents

The `fields` vector in `HasIndexableFields` is prefix-ordered: missing trailing
fields mean that no facts are associated with those fields, not that their
contents are unknown. If memory contents cannot be tracked precisely, the object
is raised to `UnknownMemoryContents` instead.

Per-field `MemoryInfo` tracks field contents, not statement-level forwarding
results. It is either:

- `UninitializedMemory()`, for a definitely uninitialized field; or
- `AliasedMemory(alias, maybeundef)`, where `alias` is either a single alias
  value or a proper may-alias set with at least two entries, and `maybeundef`
  records whether the field may also be uninitialized.

Statement-level load-forwarding results are recorded separately in
`EscapeResult.ssamemoryinfo`. For a load-like statement, this table may record a
forwardable value, a definite `UninitializedMemory()`, a definite `Bool` for
`isdefined`, or a marker such as `ConflictedMemory()` / `UnknownMemory()` when
forwarding is not valid.

### Forward Flow-Sensitive Propagation

The analysis follows the same broad direction as partial escape analysis: it is
flow-sensitive, propagates facts forward along the control-flow graph, and uses
a basic-block worklist to converge to a fixed point. This is similar in spirit
to Julia's native type-inference engine, but the lattice stores escape, alias,
and field-content facts instead of type information.

The analysis maintains a current `BlockEscapeState`, applies statement-local
changes to that state, and propagates the resulting state to successor blocks.
When a successor already has a state, the incoming state is joined into the
successor state; changed successors are placed on a basic-block worklist until a
fixed point is reached.

During analysis, missing SSA entries in a `BlockEscapeState` default to the
bottom escape information. Once analysis finishes, the returned
[`EscapeResult`](@ref Base.Compiler.EscapeAnalysis.EscapeResult) seals the block
states: missing entries in the result mean that a value was not tracked, and are
read back as `nothing` rather than as bottom.

Each statement is handled by a local transfer function that emits changes to the
current block state. Those changes are applied before the state is propagated to
successor blocks, so later statements and successor blocks observe the updated
escape, alias, liveness, and field-content facts.

### [Field and Alias Analysis](@id EA-Alias-Analysis)

`EscapeAnalysis` tracks field contents of freshly allocated objects and other
precisely analyzable objects using `ObjectInfo`. For example, constructing a
mutable object records its constructor arguments as per-field `MemoryInfo` facts.
A precise `setfield!` performs a strong update of the corresponding field fact,
and a precise `getfield` uses the field fact to alias the loaded SSA value with
its stored value.

```@repl EAUtils
code_escapes((String,)) do s
    obj = Ref("init")
    obj[] = s
    v = obj[]
    return v
end
```

In the example above, the final `getfield` can be forwarded to `s`. The return
escape imposed on the loaded value is propagated to the stored value, not to the
container object itself. If the field cannot be identified precisely, or if the
object's memory contents cannot be tracked, the analysis raises the object to
`UnknownMemoryContents` and propagates escapes conservatively.[^Dynamism]

This field analysis is complemented by a method-local alias set. Some IR nodes
return the same object as an operand, such as `PiNode`, `typeassert`, and some
ϕ-node patterns. The analysis records such must-alias relationships in a
disjoint set, and whenever escape information for one member changes, the
information is equalized across the alias set.

This alias set is also important for mutations through different SSA values that
can denote the same object:

```@repl EAUtils
code_escapes((Bool, String,)) do cond, x
    if cond
        ϕ2 = ϕ1 = Ref("foo")
    else
        ϕ2 = ϕ1 = Ref("bar")
    end
    ϕ2[] = x
    y = ϕ1[]
    return y
end
```

The analysis must recognize that `ϕ1` and `ϕ2` can alias, so that the field
update through `ϕ2` is visible to the later field load through `ϕ1`.

### [Calls and Inter-Procedural Summaries](@id EA-Inter-Procedural-Summaries)

For inlined calls, the local analysis sees the callee IR directly. For
non-inlined calls, `analyze_escapes` accepts a `get_escape_cache` callback. The
callback may return:

- `true`, meaning the callee is simple enough that its arguments need not be
  escaped conservatively, although arguments may still alias the return value
- `false`, meaning no usable summary is available and the call should be handled
  conservatively, or
- an `ArgEscapeCache`, which summarizes argument escape bits, argument aliases,
  and return escape information for the callee.

This call-boundary support is intentionally limited. The current cache does not
carry detailed local `ObjectInfo` field-content facts across method boundaries,
and non-inlined calls often still fall back to conservative handling.

A future inter-procedural design should use an IPO-safe memory summary rather
than exporting the full local `ObjectInfo` lattice. Such a summary would record
only compact facts that remain valid across optimization boundaries, such as how
arguments, returned values, and selected argument memory locations may alias or
escape. In that design, abstract interpretation could produce and consume these
summaries, while the local escape analysis would use them at call boundaries and
continue to perform optimizer-local transformations such as load forwarding,
alias-aware SROA, and allocation sinking. This future memory-summary layer is a
separate design direction from the local EA refactoring documented here.

### [Exception Handling](@id EA-Exception-Handling)

`EscapeAnalysis` also needs to account for escapes through exceptions. A value
can be marked with `ThrownEscape` when it may be thrown by an operation that is
not known to be `nothrow`.

When a statement is inside a `try` region, thrown values must be propagated to
the exception handler. Julia exposes exception objects through mechanisms such
as `Base.current_exceptions`, `rethrow`, and `Expr(:the_exception)`, so the
current implementation conservatively widens values that may be thrown into a
handler to the top escape information in the exception-handler state.

```@repl EAUtils
const GR = Ref{Any}()
@noinline function rethrow_escape!()
    try
        rethrow()
    catch err
        GR[] = err
    end
end;
get′(x) = isassigned(x) ? x[] : throw(x);

code_escapes() do
    r = Ref{String}()
    local t
    try
        t = get′(r)
    catch err
        t = typeof(err)   # `err` does not escape here
        rethrow_escape!() # but `r` escapes here
    end
    return t
end
```

This conservative treatment is intentional. More precise exception reasoning
would require proving that the handler cannot expose the exception object via
any of Julia's exception interfaces, which is usually not worthwhile for cold
error paths.

## Analysis Usage

[`analyze_escapes`](@ref Base.Compiler.EscapeAnalysis.analyze_escapes) is the
entry point for escape analysis of SSA IR elements. It can analyze `IRCode` at
multiple optimization stages, but the detailed `ObjectInfo` / `MemoryInfo` facts
are local to the IR frame being analyzed.

Most memory optimizations, such as SROA, are most effective after inlining has
exposed allocation and field operations in the same IR. In this mode,
`analyze_escapes` computes locally valid escape, alias, field-content, and
load-forwarding facts that can be consumed by later optimizer passes.

There are internal hooks for consuming compact call-boundary summaries, but the
inter-procedural design is incomplete and should not be used as a public
interface. See
[Calls and Inter-Procedural Summaries](@ref EA-Inter-Procedural-Summaries) for
the current limitations and intended future direction.

```@docs
Base.Compiler.EscapeAnalysis.analyze_escapes
Base.Compiler.EscapeAnalysis.EscapeResult
Base.Compiler.EscapeAnalysis.EscapeInfo
Base.Compiler.EscapeAnalysis.MemoryInfo
Base.Compiler.EscapeAnalysis.ObjectInfo
```

--------------------------------------------------------------------------------------------

[^PEA]: Lukas Stadler, Thomas Würthinger, and Hanspeter Mössenböck. 2014.
    Partial Escape Analysis and Scalar Replacement for Java. In _Proceedings of
    the Annual IEEE/ACM International Symposium on Code Generation and
    Optimization_ (CGO '14), 165–174.
    <https://doi.org/10.1145/2544137.2544157>.

[^Dynamism]: In some cases, object fields cannot be analyzed precisely. For
    example, an object may escape to code whose memory effects the analysis
    cannot model, or the object's concrete field layout may be unavailable. In
    such cases `ObjectInfo` is raised to `UnknownMemoryContents`, and field
    analysis propagates escapes conservatively.
