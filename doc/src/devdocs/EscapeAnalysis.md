# `EscapeAnalysis`

`Core.Compiler.EscapeAnalysis` is a compiler utility module that aims to analyze
escape information of [Julia's SSA-form IR](@ref Julia-SSA-form-IR) a.k.a. `IRCode`.

This escape analysis aims to:
- leverage Julia's high-level semantics, especially reason about escapes and aliasing via
  inter-procedural calls
- be versatile enough to be used for various optimizations including
  [alias-aware SROA](https://github.com/JuliaLang/julia/pull/43888),
  [early `finalize` insertion](https://github.com/JuliaLang/julia/pull/44056),
  [copy-free `ImmutableArray` construction](https://github.com/JuliaLang/julia/pull/42465),
  stack allocation of mutable objects,
  and so on.
- achieve a simple implementation based on a fully backward data-flow analysis implementation
  as well as a new lattice design that combines orthogonal lattice properties

## Try it out!

You can give a try to the escape analysis by loading the `EAUtils.jl` utility script that
define the convenience entries `code_escapes` and `@code_escapes` for testing and debugging purposes:
```@repl EAUtils
include(normpath(Sys.BINDIR, "..", "share", "julia", "test", "compiler", "EscapeAnalysis", "EAUtils.jl")); using .EAUtils

mutable struct SafeRef{T}
    x::T
end
Base.getindex(x::SafeRef) = x.x;
Base.setindex!(x::SafeRef, v) = x.x = v;
Base.isassigned(x::SafeRef) = true;
get′(x) = isassigned(x) ? x[] : throw(x);

result = code_escapes((String,String,String,String)) do s1, s2, s3, s4
    r1 = Ref(s1)
    r2 = Ref(s2)
    r3 = SafeRef(s3)
    try
        s1 = get′(r1)
        ret = sizeof(s1)
    catch err
        global GV = err # will definitely escape `r1`
    end
    s2 = get′(r2)       # still `r2` doesn't escape fully
    s3 = get′(r3)       # still `r3` doesn't escape fully
    s4 = sizeof(s4)     # the argument `s4` doesn't escape here
    return s2, s3, s4
end
```

The symbols in the side of each call argument and SSA statements represents the following meaning:
- `◌` (plain): this value is not analyzed because escape information of it won't be used anyway (when the object is `isbitstype` for example)
- `✓` (green or cyan): this value never escapes (`has_no_escape(result.state[x])` holds), colored blue if it has arg escape also (`has_arg_escape(result.state[x])` holds)
- `↑` (blue or yellow): this value can escape to the caller via return (`has_return_escape(result.state[x])` holds), colored yellow if it has unhandled thrown escape also (`has_thrown_escape(result.state[x])` holds)
- `X` (red): this value can escape to somewhere the escape analysis can't reason about like escapes to a global memory (`has_all_escape(result.state[x])` holds)
- `*` (bold): this value's escape state is between the `ReturnEscape` and `AllEscape` in the partial order of [`EscapeInfo`](@ref Core.Compiler.EscapeAnalysis.EscapeInfo), colored yellow if it has unhandled thrown escape also (`has_thrown_escape(result.state[x])` holds)
- `′`: this value has additional object field / array element information in its `AliasInfo` property

Escape information of each call argument and SSA value can be inspected programmatically as like:
```@repl EAUtils
result.state[Core.Argument(3)] # get EscapeInfo of `s2`

result.state[Core.SSAValue(3)] # get EscapeInfo of `r3`
```

## Analysis Design

### Lattice Design

`EscapeAnalysis` is implemented as a [data-flow analysis](https://en.wikipedia.org/wiki/Data-flow_analysis)
that works on a lattice of [`x::EscapeInfo`](@ref Core.Compiler.EscapeAnalysis.EscapeInfo),
which is composed of the following properties:
- `x.Analyzed::Bool`: not formally part of the lattice, only indicates `x` has not been analyzed or not
- `x.ReturnEscape::BitSet`: records SSA statements where `x` can escape to the caller via return
- `x.ThrownEscape::BitSet`: records SSA statements where `x` can be thrown as exception
  (used for the [exception handling](@ref EA-Exception-Handling) described below)
- `x.AliasInfo`: maintains all possible values that can be aliased to fields or array elements of `x`
  (used for the [alias analysis](@ref EA-Alias-Analysis) described below)
- `x.ArgEscape::Int` (not implemented yet): indicates it will escape to the caller through
  `setfield!` on argument(s)

These attributes can be combined to create a partial lattice that has a finite height, given
the invariant that an input program has a finite number of statements, which is assured by Julia's semantics.
The clever part of this lattice design is that it enables a simpler implementation of
lattice operations by allowing them to handle each lattice property separately[^LatticeDesign].

### Backward Escape Propagation

This escape analysis implementation is based on the data-flow algorithm described in the paper[^MM02].
The analysis works on the lattice of `EscapeInfo` and transitions lattice elements from the
bottom to the top until every lattice element gets converged to a fixed point by maintaining
a (conceptual) working set that contains program counters corresponding to remaining SSA
statements to be analyzed. The analysis manages a single global state that tracks
`EscapeInfo` of each argument and SSA statement, but also note that some flow-sensitivity
is encoded as program counters recorded in `EscapeInfo`'s `ReturnEscape` property,
which can be combined with domination analysis later to reason about flow-sensitivity if necessary.

One distinctive design of this escape analysis is that it is fully _backward_,
i.e. escape information flows _from usages to definitions_.
For example, in the code snippet below, EA first analyzes the statement `return %1` and
imposes `ReturnEscape` on `%1` (corresponding to `obj`), and then it analyzes
`%1 = %new(Base.RefValue{String, _2}))` and propagates the `ReturnEscape` imposed on `%1`
to the call argument `_2` (corresponding to `s`):
```@repl EAUtils
code_escapes((String,)) do s
    obj = Ref(s)
    return obj
end
```

The key observation here is that this backward analysis allows escape information to flow
naturally along the use-def chain rather than control-flow[^BackandForth].
As a result this scheme enables a simple implementation of escape analysis,
e.g. `PhiNode` for example can be handled simply by propagating escape information
imposed on a `PhiNode` to its predecessor values:
```@repl EAUtils
code_escapes((Bool, String, String)) do cnd, s, t
    if cnd
        obj = Ref(s)
    else
        obj = Ref(t)
    end
    return obj
end
```

### [Alias Analysis](@id EA-Alias-Analysis)

`EscapeAnalysis` implements a backward field analysis in order to reason about escapes
imposed on object fields with certain accuracy,
and `x::EscapeInfo`'s `x.AliasInfo` property exists for this purpose.
It records all possible values that can be aliased to fields of `x` at "usage" sites,
and then the escape information of that recorded values are propagated to the actual field values later at "definition" sites.
More specifically, the analysis records a value that may be aliased to a field of object by analyzing `getfield` call,
and then it propagates its escape information to the field when analyzing `%new(...)` expression or `setfield!` call[^Dynamism].
```@repl EAUtils
code_escapes((String,)) do s
    obj = SafeRef("init")
    obj[] = s
    v = obj[]
    return v
end
```
In the example above, `ReturnEscape` imposed on `%3` (corresponding to `v`) is _not_ directly
propagated to `%1` (corresponding to `obj`) but rather that `ReturnEscape` is only propagated
to `_2` (corresponding to `s`). Here `%3` is recorded in `%1`'s `AliasInfo` property as
it can be aliased to the first field of `%1`, and then when analyzing `Base.setfield!(%1, :x, _2)::String`,
that escape information is propagated to `_2` but not to `%1`.

So `EscapeAnalysis` tracks which IR elements can be aliased across a `getfield`-`%new`/`setfield!` chain
in order to analyze escapes of object fields, but actually this alias analysis needs to be
generalized to handle other IR elements as well. This is because in Julia IR the same
object is sometimes represented by different IR elements and so we should make sure that those
different IR elements that actually can represent the same object share the same escape information.
IR elements that return the same object as their operand(s), such as `PiNode` and `typeassert`,
can cause that IR-level aliasing and thus requires escape information imposed on any of such
aliased values to be shared between them.
More interestingly, it is also needed for correctly reasoning about mutations on `PhiNode`.
Let's consider the following example:
```@repl EAUtils
code_escapes((Bool, String,)) do cond, x
    if cond
        ϕ2 = ϕ1 = SafeRef("foo")
    else
        ϕ2 = ϕ1 = SafeRef("bar")
    end
    ϕ2[] = x
    y = ϕ1[]
    return y
end
```
`ϕ1 = %5` and `ϕ2 = %6` are aliased and thus `ReturnEscape` imposed on `%8 = Base.getfield(%6, :x)::String` (corresponding to `y = ϕ1[]`)
needs to be propagated to `Base.setfield!(%5, :x, _3)::String` (corresponding to `ϕ2[] = x`).
In order for such escape information to be propagated correctly, the analysis should recognize that
the _predecessors_ of `ϕ1` and `ϕ2` can be aliased as well and equalize their escape information.

One interesting property of such aliasing information is that it is not known at "usage" site
but can only be derived at "definition" site (as aliasing is conceptually equivalent to assignment),
and thus it doesn't naturally fit in a backward analysis. In order to efficiently propagate escape
information between related values, EscapeAnalysis.jl uses an approach inspired by the escape
analysis algorithm explained in an old JVM paper[^JVM05]. That is, in addition to managing
escape lattice elements, the analysis also maintains an "equi"-alias set, a disjoint set of
aliased arguments and SSA statements. The alias set manages values that can be aliased to
each other and allows escape information imposed on any of such aliased values to be equalized
between them.

### [Array Analysis](@id EA-Array-Analysis)

The alias analysis for object fields described above can also be generalized to analyze array operations.
`EscapeAnalysis` implements handlings for various primitive array operations so that it can propagate
escapes via `arrayref`-`arrayset` use-def chain and does not escape allocated arrays too conservatively:
```@repl EAUtils
code_escapes((String,)) do s
    ary = Any[]
    push!(ary, SafeRef(s))
    return ary[1], length(ary)
end
```
In the above example `EscapeAnalysis` understands that `%20` and `%2` (corresponding to the allocated object `SafeRef(s)`)
are aliased via the `arrayset`-`arrayref` chain and imposes `ReturnEscape` on them,
but not impose it on the allocated array `%1` (corresponding to `ary`).
`EscapeAnalysis` still imposes `ThrownEscape` on `ary` since it also needs to account for
potential escapes via `BoundsError`, but also note that such unhandled `ThrownEscape` can
often be ignored when optimizing the `ary` allocation.

Furthermore, in cases when array index information as well as array dimensions can be known _precisely_,
`EscapeAnalysis` is able to even reason about "per-element" aliasing via `arrayref`-`arrayset` chain,
as `EscapeAnalysis` does "per-field" alias analysis for objects:
```@repl EAUtils
code_escapes((String,String)) do s, t
    ary = Vector{Any}(undef, 2)
    ary[1] = SafeRef(s)
    ary[2] = SafeRef(t)
    return ary[1], length(ary)
end
```
Note that `ReturnEscape` is only imposed on `%2` (corresponding to `SafeRef(s)`) but not on `%4` (corresponding to `SafeRef(t)`).
This is because the allocated array's dimension and indices involved with all `arrayref`/`arrayset`
operations are available as constant information and `EscapeAnalysis` can understand that
`%6` is aliased to `%2` but never be aliased to `%4`.
In this kind of case, the succeeding optimization passes will be able to
replace `Base.arrayref(true, %1, 1)::Any` with `%2` (a.k.a. "load-forwarding") and
eventually eliminate the allocation of array `%1` entirely (a.k.a. "scalar-replacement").

When compared to object field analysis, where an access to object field can be analyzed trivially
using type information derived by inference, array dimension isn't encoded as type information
and so we need an additional analysis to derive that information. `EscapeAnalysis` at this moment
first does an additional simple linear scan to analyze dimensions of allocated arrays before
firing up the main analysis routine so that the succeeding escape analysis can precisely
analyze operations on those arrays.

However, such precise "per-element" alias analysis is often hard.
Essentially, the main difficulty inherit to array is that array dimension and index are often non-constant:
- loop often produces loop-variant, non-constant array indices
- (specific to vectors) array resizing changes array dimension and invalidates its constant-ness

Let's discuss those difficulties with concrete examples.

In the following example, `EscapeAnalysis` fails the precise alias analysis since the index
at the `Base.arrayset(false, %4, %8, %6)::Vector{Any}` is not (trivially) constant.
Especially `Any[nothing, nothing]` forms a loop and calls that `arrayset` operation in a loop,
where `%6` is represented as a ϕ-node value (whose value is control-flow dependent).
As a result, `ReturnEscape` ends up imposed on both `%23` (corresponding to `SafeRef(s)`) and
`%25` (corresponding to `SafeRef(t)`), although ideally we want it to be imposed only on `%23` but not on `%25`:
```@repl EAUtils
code_escapes((String,String)) do s, t
    ary = Any[nothing, nothing]
    ary[1] = SafeRef(s)
    ary[2] = SafeRef(t)
    return ary[1], length(ary)
end
```

The next example illustrates how vector resizing makes precise alias analysis hard.
The essential difficulty is that the dimension of allocated array `%1` is first initialized as `0`,
but it changes by the two `:jl_array_grow_end` calls afterwards.
`EscapeAnalysis` currently simply gives up precise alias analysis whenever it encounters any
array resizing operations and so `ReturnEscape` is imposed on both `%2` (corresponding to `SafeRef(s)`)
and `%20` (corresponding to `SafeRef(t)`):
```@repl EAUtils
code_escapes((String,String)) do s, t
    ary = Any[]
    push!(ary, SafeRef(s))
    push!(ary, SafeRef(t))
    ary[1], length(ary)
end
```

In order to address these difficulties, we need inference to be aware of array dimensions
and propagate array dimensions in a flow-sensitive way[^ArrayDimension], as well as come
up with nice representation of loop-variant values.

`EscapeAnalysis` at this moment quickly switches to the more imprecise analysis that doesn't
track precise index information in cases when array dimensions or indices are trivially non
constant. The switch can naturally be implemented as a lattice join operation of
`EscapeInfo.AliasInfo` property in the data-flow analysis framework.

### [Exception Handling](@id EA-Exception-Handling)

It would be also worth noting how `EscapeAnalysis` handles possible escapes via exceptions.
Naively it seems enough to propagate escape information imposed on `:the_exception` object to
all values that may be thrown in a corresponding `try` block.
But there are actually several other ways to access to the exception object in Julia,
such as `Base.current_exceptions` and `rethrow`.
For example, escape analysis needs to account for potential escape of `r` in the example below:
```@repl EAUtils
const GR = Ref{Any}();
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
        t = typeof(err)   # `err` (which `r` aliases to) doesn't escape here
        rethrow_escape!() # but `r` escapes here
    end
    return t
end
```

It requires a global analysis in order to correctly reason about all possible escapes via
existing exception interfaces. For now we always propagate the topmost escape information to
all potentially thrown objects conservatively, since such an additional analysis might not be
worthwhile to do given that exception handling and error path usually don't need to be
very performance sensitive, and also optimizations of error paths might be very ineffective anyway
since they are often even "unoptimized" intentionally for latency reasons.

`x::EscapeInfo`'s `x.ThrownEscape` property records SSA statements where `x` can be thrown as an exception.
Using this information `EscapeAnalysis` can propagate possible escapes via exceptions limitedly
to only those may be thrown in each `try` region:
```@repl EAUtils
result = code_escapes((String,String)) do s1, s2
    r1 = Ref(s1)
    r2 = Ref(s2)
    local ret
    try
        s1 = get′(r1)
        ret = sizeof(s1)
    catch err
        global GV = err # will definitely escape `r1`
    end
    s2 = get′(r2)       # still `r2` doesn't escape fully
    return s2
end
```

## Analysis Usage

`analyze_escapes` is the entry point to analyze escape information of SSA-IR elements.

Most optimizations like SROA (`sroa_pass!`) are more effective when applied to
an optimized source that the inlining pass (`ssa_inlining_pass!`) has simplified
by resolving inter-procedural calls and expanding callee sources.
Accordingly, `analyze_escapes` is also able to analyze post-inlining IR and collect
escape information that is useful for certain memory-related optimizations.

However, since certain optimization passes like inlining can change control flows and eliminate dead code,
they can break the inter-procedural validity of escape information. In particularity,
in order to collect inter-procedurally valid escape information, we need to analyze a pre-inlining IR.

Because of this reason, `analyze_escapes` can analyze `IRCode` at any Julia-level optimization stage,
and especially, it is supposed to be used at the following two stages:
- `IPO EA`: analyze pre-inlining IR to generate IPO-valid escape information cache
- `Local EA`: analyze post-inlining IR to collect locally-valid escape information

Escape information derived by `IPO EA` is transformed to the `ArgEscapeCache` data structure and cached globally.
By passing an appropriate `get_escape_cache` callback to `analyze_escapes`,
the escape analysis can improve analysis accuracy by utilizing cached inter-procedural information of
non-inlined callees that has been derived by previous `IPO EA`.
More interestingly, it is also valid to use `IPO EA` escape information for type inference,
e.g., inference accuracy can be improved by forming `Const`/`PartialStruct`/`MustAlias` of mutable object.

Since the computational cost of `analyze_escapes` is not that cheap,
both `IPO EA` and `Local EA` are better to run only when there is any profitability.
Currently `EscapeAnalysis` provides the `is_ipo_profitable` heuristic to check a profitability of `IPO EA`.
```@docs
Core.Compiler.EscapeAnalysis.analyze_escapes
Core.Compiler.EscapeAnalysis.EscapeState
Core.Compiler.EscapeAnalysis.EscapeInfo
Core.Compiler.EscapeAnalysis.is_ipo_profitable
```

--------------------------------------------------------------------------------------------

[^LatticeDesign]: Our type inference implementation takes the alternative approach,
    where each lattice property is represented by a special lattice element type object.
    It turns out that it started to complicate implementations of the lattice operations
    mainly because it often requires conversion rules between each lattice element type object.
    And we are working on [overhauling our type inference lattice implementation](https://github.com/JuliaLang/julia/pull/42596)
    with `EscapeInfo`-like lattice design.

[^MM02]: _A Graph-Free approach to Data-Flow Analysis_.
         Markas Mohnen, 2002, April.
         <https://api.semanticscholar.org/CorpusID:28519618>.

[^BackandForth]: Our type inference algorithm in contrast is implemented as a forward analysis,
    because type information usually flows from "definition" to "usage" and it is more
    natural and effective to propagate such information in a forward way.

[^Dynamism]: In some cases, however, object fields can't be analyzed precisely.
    For example, object may escape to somewhere `EscapeAnalysis` can't account for possible memory effects on it,
    or fields of the objects simply can't be known because of the lack of type information.
    In such cases `AliasInfo` property is raised to the topmost element within its own lattice order,
    and it causes succeeding field analysis to be conservative and escape information imposed on
    fields of an unanalyzable object to be propagated to the object itself.

[^JVM05]: _Escape Analysis in the Context of Dynamic Compilation and Deoptimization_.
          Thomas Kotzmann and Hanspeter Mössenböck, 2005, June.
          <https://dl.acm.org/doi/10.1145/1064979.1064996>.

[^ArrayDimension]: Otherwise we will need yet another forward data-flow analysis on top of the escape analysis.
