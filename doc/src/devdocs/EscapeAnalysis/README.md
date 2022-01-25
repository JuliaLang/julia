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
↑  1 ─ %1 = %new(Base.RefValue{String}, _2)::Base.RefValue{String}
◌  └──      return %1
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
#3(✓ _2::Bool, ↑ _3::String, ↑ _4::String) in Main at REPL[3]:2
◌  1 ─      goto #3 if not _2
↑  2 ─ %2 = %new(Base.RefValue{String}, _3)::Base.RefValue{String}
◌  └──      goto #4
↑  3 ─ %4 = %new(Base.RefValue{String}, _4)::Base.RefValue{String}
↑  4 ┄ %5 = φ (#2 => %2, #3 => %4)::Base.RefValue{String}
◌  └──      return %5
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
✓′ 1 ─ %1 = %new(SafeRef{String}, "init")::SafeRef{String}
◌  │        Base.setfield!(%1, :x, _2)::String
↑  │   %3 = Base.getfield(%1, :x)::String
◌  └──      return %3
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
#7(✓ _2::Bool, ↑ _3::String) in Main at REPL[8]:2
◌  1 ─      goto #3 if not _2
✓′ 2 ─ %2 = %new(SafeRef{String}, "foo")::SafeRef{String}
◌  └──      goto #4
✓′ 3 ─ %4 = %new(SafeRef{String}, "bar")::SafeRef{String}
✓′ 4 ┄ %5 = φ (#2 => %2, #3 => %4)::SafeRef{String}
✓′ │   %6 = φ (#2 => %2, #3 => %4)::SafeRef{String}
◌  │        Base.setfield!(%5, :x, _3)::String
↑  │   %8 = Base.getfield(%6, :x)::String
◌  └──      return %8
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
```julia
julia> code_escapes((String,)) do s
           ary = Any[]
           push!(ary, SafeRef(s))
           return ary[1], length(ary)
       end
#9(↑ _2::String) in Main at REPL[9]:2
*′ 1 ── %1  = $(Expr(:foreigncall, :(:jl_alloc_array_1d), Vector{Any}, svec(Any, Int64), 0, :(:ccall), Vector{Any}, 0, 0))::Vector{Any}
↑  │    %2  = %new(SafeRef{String}, _2)::SafeRef{String}
◌  │    %3  = Core.lshr_int(1, 63)::Int64
◌  │    %4  = Core.trunc_int(Core.UInt8, %3)::UInt8
◌  │    %5  = Core.eq_int(%4, 0x01)::Bool
◌  └───       goto #3 if not %5
◌  2 ──       invoke Core.throw_inexacterror(:check_top_bit::Symbol, UInt64::Type{UInt64}, 1::Int64)::Union{}
◌  └───       unreachable
◌  3 ──       goto #4
◌  4 ── %10 = Core.bitcast(Core.UInt64, 1)::UInt64
◌  └───       goto #5
◌  5 ──       goto #6
◌  6 ──       goto #7
◌  7 ──       goto #8
◌  8 ──       $(Expr(:foreigncall, :(:jl_array_grow_end), Nothing, svec(Any, UInt64), 0, :(:ccall), :(%1), :(%10), :(%10)))::Nothing
◌  └───       goto #9
◌  9 ── %17 = Base.arraylen(%1)::Int64
◌  │          Base.arrayset(true, %1, %2, %17)::Vector{Any}
◌  └───       goto #10
↑  10 ─ %20 = Base.arrayref(true, %1, 1)::Any
◌  │    %21 = Base.arraylen(%1)::Int64
↑  │    %22 = Core.tuple(%20, %21)::Tuple{Any, Int64}
◌  └───       return %22
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
```julia
julia> code_escapes((String,String)) do s, t
           ary = Vector{Any}(undef, 2)
           ary[1] = SafeRef(s)
           ary[2] = SafeRef(t)
           return ary[1], length(ary)
       end
#11(↑ _2::String, * _3::String) in Main at REPL[10]:2
*′ 1 ─ %1 = $(Expr(:foreigncall, :(:jl_alloc_array_1d), Vector{Any}, svec(Any, Int64), 0, :(:ccall), Vector{Any}, 2, 2))::Vector{Any}
↑  │   %2 = %new(SafeRef{String}, _2)::SafeRef{String}
◌  │        Base.arrayset(true, %1, %2, 1)::Vector{Any}
*  │   %4 = %new(SafeRef{String}, _3)::SafeRef{String}
◌  │        Base.arrayset(true, %1, %4, 2)::Vector{Any}
↑  │   %6 = Base.arrayref(true, %1, 1)::Any
◌  │   %7 = Base.arraylen(%1)::Int64
↑  │   %8 = Core.tuple(%6, %7)::Tuple{Any, Int64}
◌  └──      return %8
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

However, such precise "per-element" alias analyis is often hard.
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
```julia
julia> code_escapes((String,String)) do s, t
           ary = Any[nothing, nothing]
           ary[1] = SafeRef(s)
           ary[2] = SafeRef(t)
           return ary[1], length(ary)
       end
#13(↑ _2::String, ↑ _3::String) in Main at REPL[11]:2
◌  1 ─ %1  = Main.nothing::Core.Const(nothing)
◌  │   %2  = Main.nothing::Core.Const(nothing)
◌  │   %3  = Core.tuple(%1, %2)::Core.Const((nothing, nothing))
*′ │   %4  = $(Expr(:foreigncall, :(:jl_alloc_array_1d), Vector{Any}, svec(Any, Int64), 0, :(:ccall), Vector{Any}, 2, 2))::Vector{Any}
◌  └──       goto #7 if not true
◌  2 ┄ %6  = φ (#1 => 1, #6 => %16)::Int64
◌  │   %7  = φ (#1 => 1, #6 => %17)::Int64
↑  │   %8  = Base.getfield(%3, %6, false)::Nothing
◌  │         Base.arrayset(false, %4, %8, %6)::Vector{Any}
◌  │   %10 = (%7 === 2)::Bool
◌  └──       goto #4 if not %10
◌  3 ─       Base.nothing::Nothing
◌  └──       goto #5
◌  4 ─ %14 = Base.add_int(%7, 1)::Int64
◌  └──       goto #5
◌  5 ┄ %16 = φ (#4 => %14)::Int64
◌  │   %17 = φ (#4 => %14)::Int64
◌  │   %18 = φ (#3 => true, #4 => false)::Bool
◌  │   %19 = Base.not_int(%18)::Bool
◌  └──       goto #7 if not %19
◌  6 ─       goto #2
◌  7 ┄       goto #8
↑  8 ─ %23 = %new(SafeRef{String}, _2)::SafeRef{String}
◌  │         Base.arrayset(true, %4, %23, 1)::Vector{Any}
↑  │   %25 = %new(SafeRef{String}, _3)::SafeRef{String}
◌  │         Base.arrayset(true, %4, %25, 2)::Vector{Any}
↑  │   %27 = Base.arrayref(true, %4, 1)::Any
◌  │   %28 = Base.arraylen(%4)::Int64
↑  │   %29 = Core.tuple(%27, %28)::Tuple{Any, Int64}
◌  └──       return %29
```

The next example illustrates how vector resizing makes precise alias analysis hard.
The essential difficulty is that the dimension of allocated array `%1` is first initialized as `0`,
but it changes by the two `:jl_array_grow_end` calls afterwards.
`EscapeAnalysis` currently simply gives up precise alias analysis whenever it encounters any
array resizing operations and so `ReturnEscape` is imposed on both `%2` (corresponding to `SafeRef(s)`)
and `%20` (corresponding to `SafeRef(t)`):
```julia
julia> code_escapes((String,String)) do s, t
           ary = Any[]
           push!(ary, SafeRef(s))
           push!(ary, SafeRef(t))
           ary[1], length(ary)
       end
#15(↑ _2::String, ↑ _3::String) in Main at REPL[12]:2
*′ 1 ── %1  = $(Expr(:foreigncall, :(:jl_alloc_array_1d), Vector{Any}, svec(Any, Int64), 0, :(:ccall), Vector{Any}, 0, 0))::Vector{Any}
↑  │    %2  = %new(SafeRef{String}, _2)::SafeRef{String}
◌  │    %3  = Core.lshr_int(1, 63)::Int64
◌  │    %4  = Core.trunc_int(Core.UInt8, %3)::UInt8
◌  │    %5  = Core.eq_int(%4, 0x01)::Bool
◌  └───       goto #3 if not %5
◌  2 ──       invoke Core.throw_inexacterror(:check_top_bit::Symbol, UInt64::Type{UInt64}, 1::Int64)::Union{}
◌  └───       unreachable
◌  3 ──       goto #4
◌  4 ── %10 = Core.bitcast(Core.UInt64, 1)::UInt64
◌  └───       goto #5
◌  5 ──       goto #6
◌  6 ──       goto #7
◌  7 ──       goto #8
◌  8 ──       $(Expr(:foreigncall, :(:jl_array_grow_end), Nothing, svec(Any, UInt64), 0, :(:ccall), :(%1), :(%10), :(%10)))::Nothing
◌  └───       goto #9
◌  9 ── %17 = Base.arraylen(%1)::Int64
◌  │          Base.arrayset(true, %1, %2, %17)::Vector{Any}
◌  └───       goto #10
↑  10 ─ %20 = %new(SafeRef{String}, _3)::SafeRef{String}
◌  │    %21 = Core.lshr_int(1, 63)::Int64
◌  │    %22 = Core.trunc_int(Core.UInt8, %21)::UInt8
◌  │    %23 = Core.eq_int(%22, 0x01)::Bool
◌  └───       goto #12 if not %23
◌  11 ─       invoke Core.throw_inexacterror(:check_top_bit::Symbol, UInt64::Type{UInt64}, 1::Int64)::Union{}
◌  └───       unreachable
◌  12 ─       goto #13
◌  13 ─ %28 = Core.bitcast(Core.UInt64, 1)::UInt64
◌  └───       goto #14
◌  14 ─       goto #15
◌  15 ─       goto #16
◌  16 ─       goto #17
◌  17 ─       $(Expr(:foreigncall, :(:jl_array_grow_end), Nothing, svec(Any, UInt64), 0, :(:ccall), :(%1), :(%28), :(%28)))::Nothing
◌  └───       goto #18
◌  18 ─ %35 = Base.arraylen(%1)::Int64
◌  │          Base.arrayset(true, %1, %20, %35)::Vector{Any}
◌  └───       goto #19
↑  19 ─ %38 = Base.arrayref(true, %1, 1)::Any
◌  │    %39 = Base.arraylen(%1)::Int64
↑  │    %40 = Core.tuple(%38, %39)::Tuple{Any, Int64}
◌  └───       return %40
```

In order to address these difficulties, we need inference to be aware of array dimensions
and propagate array dimenstions in a flow-sensitive way[^ArrayDimension], as well as come
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
#17() in Main at REPL[16]:2
X  1 ── %1  = %new(Base.RefValue{String})::Base.RefValue{String}
◌  2 ── %2  = $(Expr(:enter, #8))
◌  3 ── %3  = Base.isdefined(%1, :x)::Bool
◌  └───       goto #5 if not %3
↑  4 ── %5  = Base.getfield(%1, :x)::String
◌  └───       goto #6
◌  5 ──       Main.throw(%1)::Union{}
◌  └───       unreachable
◌  6 ──       $(Expr(:leave, 1))
◌  7 ──       goto #10
◌  8 ──       $(Expr(:leave, 1))
✓  9 ── %12 = $(Expr(:the_exception))::Any
↑  │    %13 = Main.typeof(%12)::DataType
◌  │          invoke Main.rethrow_escape!()::Any
◌  └───       $(Expr(:pop_exception, :(%2)))::Any
↑  10 ┄ %16 = φ (#7 => %5, #9 => %13)::Union{DataType, String}
◌  └───       return %16
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
#19(X _2::String, ↑ _3::String) in Main at REPL[17]:2
X  1 ── %1  = %new(Base.RefValue{String}, _2)::Base.RefValue{String}
*′ └─── %2  = %new(Base.RefValue{String}, _3)::Base.RefValue{String}
◌  2 ── %3  = $(Expr(:enter, #8))
*′ └─── %4  = ϒ (%2)::Base.RefValue{String}
◌  3 ── %5  = Base.isdefined(%1, :x)::Bool
◌  └───       goto #5 if not %5
X  4 ──       Base.getfield(%1, :x)::String
◌  └───       goto #6
◌  5 ──       Main.throw(%1)::Union{}
◌  └───       unreachable
◌  6 ──       nothing::typeof(Core.sizeof)
◌  │          nothing::Int64
◌  └───       $(Expr(:leave, 1))
◌  7 ──       goto #10
*′ 8 ── %15 = φᶜ (%4)::Base.RefValue{String}
◌  └───       $(Expr(:leave, 1))
X  9 ── %17 = $(Expr(:the_exception))::Any
◌  │          (Main.g = %17)::Any
◌  └───       $(Expr(:pop_exception, :(%3)))::Any
*′ 10 ┄ %20 = φ (#7 => %2, #9 => %15)::Base.RefValue{String}
◌  │    %21 = Base.isdefined(%20, :x)::Bool
◌  └───       goto #12 if not %21
↑  11 ─ %23 = Base.getfield(%20, :x)::String
◌  └───       goto #13
◌  12 ─       Main.throw(%20)::Union{}
◌  └───       unreachable
◌  13 ─       return %23
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

[^ArrayDimension]: Otherwise we will need yet another forward data-flow analysis on top of the escape analysis.
