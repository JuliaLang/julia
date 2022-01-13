[![CI](https://github.com/aviatesk/EscapeAnalysis.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/aviatesk/EscapeAnalysis.jl/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/aviatesk/EscapeAnalysis.jl/branch/master/graph/badge.svg?token=ADEKPZRUJH)](https://codecov.io/gh/aviatesk/EscapeAnalysis.jl)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://aviatesk.github.io/EscapeAnalysis.jl/dev/)

`EscapeAnalysis` is a simple module that collects escape information in
[Julia's SSA optimization IR](@ref Julia-SSA-form-IR) a.k.a. `IRCode`.

You can give a try to the escape analysis with the convenience entries that
`EscapeAnalysis` exports for testing and debugging purposes:
```@docs
Base.code_escapes
InteractiveUtils.@code_escapes
```

## Analysis Design

### Lattice Design

`EscapeAnalysis` is implemented as a [data-flow analysis](https://en.wikipedia.org/wiki/Data-flow_analysis)
that works on a lattice of `x::EscapeInfo`, which is composed of the following properties:
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
```julia
julia> code_escapes((String,)) do s
           obj = Ref(s)
           return obj
       end
#1(↑ _2::String) in Main at REPL[2]:2
2 ↑  1 ─ %1 = %new(Base.RefValue{String}, _2)::Base.RefValue{String}                │╻╷╷ Ref
3 ◌  └──      return %1                                                             │
```

The key observation here is that this backward analysis allows escape information to flow
naturally along the use-def chain rather than control-flow[^BackandForth].
As a result this scheme enables a simple implementation of escape analysis,
e.g. `PhiNode` for example can be handled simply by propagating escape information
imposed on a `PhiNode` to its predecessor values:
```julia
julia> code_escapes((Bool, String, String)) do cnd, s, t
           if cnd
               obj = Ref(s)
           else
               obj = Ref(t)
           end
           return obj
       end
  #3(↑ _2::Bool, ↑ _3::String, ↑ _4::String) in Main at REPL[3]:2
2 ◌  1 ─      goto #3 if not _2                                                     │
3 ↑  2 ─ %2 = %new(Base.RefValue{String}, _3)::Base.RefValue{String}                │╻╷╷ Ref
  ◌  └──      goto #4                                                               │
5 ↑  3 ─ %4 = %new(Base.RefValue{String}, _4)::Base.RefValue{String}                │╻╷╷ Ref
7 ↑  4 ┄ %5 = φ (#2 => %2, #3 => %4)::Base.RefValue{String}                         │
  ◌  └──      return %5                                                             │
```

### [Alias Analysis](@id EA-Alias-Analysis)

`EscapeAnalysis` implements a backward field analysis in order to reason about escapes
imposed on object fields with certain accuracy,
and `x::EscapeInfo`'s `x.AliasInfo` property exists for this purpose.
It records all possible values that can be aliased to fields of `x` at "usage" sites,
and then the escape information of that recorded values are propagated to the actual field values later at "definition" sites.
More specifically, the analysis records a value that may be aliased to a field of object by analyzing `getfield` call,
and then it propagates its escape information to the field when analyzing `%new(...)` expression or `setfield!` call[^Dynamism].
```julia
julia> mutable struct SafeRef{T}
           x::T
       end

julia> Base.getindex(x::SafeRef) = x.x;

julia> Base.setindex!(x::SafeRef, v) = x.x = v;

julia> code_escapes((String,)) do s
           obj = SafeRef("init")
           obj[] = s
           v = obj[]
           return v
       end
#5(↑ _2::String) in Main at REPL[7]:2
2 ✓′ 1 ─ %1 = %new(SafeRef{String}, "init")::SafeRef{String}                   │╻╷ SafeRef
3 ◌  │        Base.setfield!(%1, :x, _2)::String                               │╻╷ setindex!
4 ↑  │   %3 = Base.getfield(%1, :x)::String                                    │╻╷ getindex
5 ◌  └──      return %3                                                        │
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
```julia
julia> code_escapes((Bool, String,)) do cond, x
           if cond
               ϕ2 = ϕ1 = SafeRef("foo")
           else
               ϕ2 = ϕ1 = SafeRef("bar")
           end
           ϕ2[] = x
           y = ϕ1[]
           return y
       end
#7(↑ _2::Bool, ↑ _3::String) in Main at REPL[8]:2
2 ◌  1 ─      goto #3 if not _2                                                │
3 ✓′ 2 ─ %2 = %new(SafeRef{String}, "foo")::SafeRef{String}                    │╻╷ SafeRef
  ◌  └──      goto #4                                                          │
5 ✓′ 3 ─ %4 = %new(SafeRef{String}, "bar")::SafeRef{String}                    │╻╷ SafeRef
7 ✓′ 4 ┄ %5 = φ (#2 => %2, #3 => %4)::SafeRef{String}                          │
  ✓′ │   %6 = φ (#2 => %2, #3 => %4)::SafeRef{String}                          │
  ◌  │        Base.setfield!(%5, :x, _3)::String                               │╻  setindex!
8 ↑  │   %8 = Base.getfield(%6, :x)::String                                    │╻╷ getindex
9 ◌  └──      return %8                                                        │
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

Lastly, this scheme of alias/field analysis can also be generalized to analyze array operations.
`EscapeAnalysis` currently reasons about escapes imposed on array elements using
an imprecise version of the field analysis described above, where `AliasInfo` doesn't
try to track precise array index but rather simply records all possible values that can be
aliased any elements of the array.

### [Exception Handling](@id EA-Exception-Handling)

It would be also worth noting how `EscapeAnalysis` handles possible escapes via exceptions.
Naively it seems enough to propagate escape information imposed on `:the_exception` object to
all values that may be thrown in a corresponding `try` block.
But there are actually several other ways to access to the exception object in Julia,
such as `Base.current_exceptions` and manual catch of `rethrow`n object.
For example, escape analysis needs to account for potential escape of `r` in the example below:
```julia
julia> const Gx = Ref{Any}();

julia> @noinline function rethrow_escape!()
           try
               rethrow()
           catch err
               Gx[] = err
           end
       end;

julia> get′(x) = isassigned(x) ? x[] : throw(x);

julia> code_escapes() do
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
#9() in Main at REPL[12]:2
2  X  1 ── %1  = %new(Base.RefValue{String})::Base.RefValue{String}            │╻╷ Ref
4  ◌  2 ── %2  = $(Expr(:enter, #8))                                           │
5  ◌  3 ── %3  = Base.isdefined(%1, :x)::Bool                                  │╻╷ get′
   ◌  └───       goto #5 if not %3                                             ││
   ↑  4 ── %5  = Base.getfield(%1, :x)::String                                 ││╻  getindex
   ◌  └───       goto #6                                                       ││
   ◌  5 ──       Main.throw(%1)::Union{}                                       ││
   ◌  └───       unreachable                                                   ││
   ◌  6 ──       $(Expr(:leave, 1))                                            │
   ◌  7 ──       goto #10                                                      │
   ◌  8 ──       $(Expr(:leave, 1))                                            │
   ◌  9 ── %12 = $(Expr(:the_exception))::Any                                  │
7  ↑  │    %13 = Main.typeof(%12)::DataType                                    │
8  ◌  │          invoke Main.rethrow_escape!()::Any                            │
   ◌  └───       $(Expr(:pop_exception, :(%2)))::Any                           │
10 ↑  10 ┄ %16 = φ (#7 => %5, #9 => %13)::Union{DataType, String}              │
   ◌  └───       return %16                                                    │
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
```julia
julia> result = code_escapes((String,String)) do s1, s2
           r1 = Ref(s1)
           r2 = Ref(s2)
           local ret
           try
               s1 = get′(r1)
               ret = sizeof(s1)
           catch err
               global g = err # will definitely escape `r1`
           end
           s2 = get′(r2)      # still `r2` doesn't escape fully
           return s2
       end
#11(X _2::String, ↑ _3::String) in Main at REPL[13]:2
2  X  1 ── %1  = %new(Base.RefValue{String}, _2)::Base.RefValue{String}   │╻╷╷ Ref
3  *′ └─── %2  = %new(Base.RefValue{String}, _3)::Base.RefValue{String}   │╻╷╷ Ref
5  ◌  2 ── %3  = $(Expr(:enter, #8))                                      │
   *′ └─── %4  = ϒ (%2)::Base.RefValue{String}                            │
6  ◌  3 ── %5  = Base.isdefined(%1, :x)::Bool                             │╻╷  get′
   ◌  └───       goto #5 if not %5                                        ││
   X  4 ──       Base.getfield(%1, :x)::String                            ││╻   getindex
   ◌  └───       goto #6                                                  ││
   ◌  5 ──       Main.throw(%1)::Union{}                                  ││
   ◌  └───       unreachable                                              ││
7  ◌  6 ──       nothing::typeof(Core.sizeof)                             │╻   sizeof
   ◌  │          nothing::Int64                                           ││
   ◌  └───       $(Expr(:leave, 1))                                       │
   ◌  7 ──       goto #10                                                 │
   *′ 8 ── %15 = φᶜ (%4)::Base.RefValue{String}                           │
   ◌  └───       $(Expr(:leave, 1))                                       │
   X  9 ── %17 = $(Expr(:the_exception))::Any                             │
9  ◌  │          (Main.g = %17)::Any                                      │
   ◌  └───       $(Expr(:pop_exception, :(%3)))::Any                      │
11 *′ 10 ┄ %20 = φ (#7 => %2, #9 => %15)::Base.RefValue{String}           │
   ◌  │    %21 = Base.isdefined(%20, :x)::Bool                            ││╻   isassigned
   ◌  └───       goto #12 if not %21                                      ││
   ↑  11 ─ %23 = Base.getfield(%20, :x)::String                           │││╻   getproperty
   ◌  └───       goto #13                                                 ││
   ◌  12 ─       Main.throw(%20)::Union{}                                 ││
   ◌  └───       unreachable                                              ││
12 ◌  13 ─       return %23                                               │
```

## Analysis Usage

When using `EscapeAnalysis` in Julia's high-level compilation pipeline, we can run
`analyze_escapes(ir::IRCode) -> estate::EscapeState` to analyze escape information of each SSA-IR element in `ir`.

Note that it should be most effective if `analyze_escapes` runs after inlining,
as `EscapeAnalysis`'s interprocedural escape information handling is limited at this moment.

Since the computational cost of `analyze_escapes` is not that cheap,
it is more ideal if it runs once and succeeding optimization passes incrementally update
    the escape information upon IR transformation.

```@docs
Core.Compiler.EscapeAnalysis.analyze_escapes
Core.Compiler.EscapeAnalysis.EscapeState
Core.Compiler.EscapeAnalysis.EscapeInfo
Core.Compiler.EscapeAnalysis.cache_escapes!
```

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
