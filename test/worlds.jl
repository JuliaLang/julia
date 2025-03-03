# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for accurate updating of method tables

using Base: get_world_counter, tls_world_age
@test typemax(UInt) > get_world_counter() == tls_world_age() > 0

# test simple method replacement
begin
    g265a() = f265a(0)
    f265a(x::Any) = 1
    @test g265a() == 1
    @test Base.return_types(g265a, ()) == Any[Int]
    @test Core.Compiler.return_type(g265a, Tuple{}) == Int

    f265a(x::Any) = 2.0
    @test g265a() == 2.0

    @test Base.return_types(g265a, ()) == Any[Float64]
    @test Core.Compiler.return_type(g265a, Tuple{}) == Float64
end

# test signature widening
begin
    f265b(x::Int) = 1
    let ty = Any[1, 2.0e0]
        global g265b(i::Int) = f265b(ty[i])
    end
    @test g265b(1) == 1
    @test Base.return_types(g265b, (Int,)) == Any[Int]
    @test Core.Compiler.return_type(g265b, Tuple{Int,}) == Int

    f265b(x::Any) = 2.0
    @test g265b(1) == 1
    @test g265b(2) == 2.0
    @test Base.return_types(g265b, (Int,)) == Any[Union{Int, Float64}]
    @test Core.Compiler.return_type(g265b, Tuple{Int,}) == Union{Int, Float64}
end

# test signature narrowing
begin
    g265c() = f265c(0)
    f265c(x::Any) = 1
    @test g265c() == 1
    @test Base.return_types(g265c, ()) == Any[Int]
    @test Core.Compiler.return_type(g265c, Tuple{}) == Int

    f265c(x::Int) = 2.0
    @test g265c() == 2.0

    @test Base.return_types(g265c, ()) == Any[Float64]
    @test Core.Compiler.return_type(g265c, Tuple{}) == Float64
end

# test constructor narrowing
mutable struct A265{T}
    field1::T
end
A265_() = A265(1)
@test (A265_()::A265{Int}).field1 === 1
A265(fld::Int) = A265(Float64(fld))
@test (A265_()::A265{Float64}).field1 === 1.0e0

# test constructor widening
mutable struct B265{T}
    field1::T
    # dummy arg is present to prevent (::Type{T}){T}(arg) from matching the test calls
    B265{T}(field1::Any, dummy::Nothing) where {T} = new(field1) # prevent generation of outer ctor
end
  # define some constructors
B265(x::Int, dummy::Nothing) = B265{Int}(x, dummy)
let ty = Any[1, 2.0e0, 3.0f0]
    global B265_(i::Int) = B265(ty[i], nothing)
end
  # test for correct answers
@test (B265_(1)::B265{Int}).field1 === 1
@test_throws MethodError B265_(2)
@test_throws MethodError B265_(3)
@test Base.return_types(B265_, (Int,)) == Any[B265{Int}]
@test Core.Compiler.return_type(B265_, Tuple{Int,}) == B265{Int}

  # add new constructors
B265(x::Float64, dummy::Nothing) = B265{Float64}(x, dummy)
B265(x::Any, dummy::Nothing) = B265{UInt8}(x, dummy)

  # make sure answers are updated
@test (B265_(1)::B265{Int}).field1 === 1
@test (B265_(2)::B265{Float64}).field1 === 2.0e0
@test (B265_(3)::B265{UInt8}).field1 === 0x03

@test B265{UInt8} <: only(Base.return_types(B265_, (Int,))) <: B265
@test B265{UInt8} <: Core.Compiler.return_type(B265_, Tuple{Int,}) <: B265


# test oldworld call / inference
function wfunc(c1,c2)
    while true
        (f, args) = take!(c1)
        put!(c2, f(args...))
    end
end
function put_n_take!(v...)
    put!(chnls[1], v)
    take!(chnls[2])
end

g265() = [f265(x) for x in 1:3.]
wc265 = get_world_counter()
wc265_41332a = Task(tls_world_age)
@test tls_world_age() == wc265 + 1
(function ()
    global wc265_41332b = Task(tls_world_age)
    @eval f265(::Any) = 1.0
    global wc265_41332c = Base.invokelatest(Task, tls_world_age)
    global wc265_41332d = Task(tls_world_age)
    nothing
end)()
@test wc265 + 12 == get_world_counter() == tls_world_age()
schedule(wc265_41332a)
schedule(wc265_41332b)
schedule(wc265_41332c)
schedule(wc265_41332d)
@test wc265 + 1 == fetch(wc265_41332a)
@test wc265 + 10 == fetch(wc265_41332b)
@test wc265 + 12 == fetch(wc265_41332c)
@test wc265 + 10 == fetch(wc265_41332d)
chnls, tasks = Base.channeled_tasks(2, wfunc)
t265 = tasks[1]

wc265 = get_world_counter()
@test put_n_take!(get_world_counter, ()) == wc265
@test put_n_take!(tls_world_age, ()) + 3 == wc265
f265(::Int) = 1
@test put_n_take!(get_world_counter, ()) == wc265 + 1 == get_world_counter() == tls_world_age()
@test put_n_take!(tls_world_age, ()) + 3 == wc265

@test g265() == Int[1, 1, 1]
@test Core.Compiler.return_type(f265, Tuple{Any,}) == Union{Float64, Int}
@test Core.Compiler.return_type(f265, Tuple{Int,}) == Int
@test Core.Compiler.return_type(f265, Tuple{Float64,}) == Float64

@test put_n_take!(g265, ()) == Float64[1.0, 1.0, 1.0]
@test put_n_take!(Core.Compiler.return_type, (f265, Tuple{Any,})) == Float64
@test put_n_take!(Core.Compiler.return_type, (f265, Tuple{Int,})) == Float64
@test put_n_take!(Core.Compiler.return_type, (f265, Tuple{Float64,})) == Float64
@test put_n_take!(Core.Compiler.return_type, (f265, Tuple{Float64,})) == Float64

# test that reflection ignores worlds
@test Base.return_types(f265, (Any,)) == Any[Int, Float64]
@test put_n_take!(Base.return_types, (f265, (Any,))) == Any[Int, Float64]

# test for method errors
h265() = true
file = @__FILE__
Base.stacktrace_contract_userdir() && (file = Base.contractuser(file))
loc_h265 = "@ $(@__MODULE__) $file:$(@__LINE__() - 3)"
@test h265()
@test_throws TaskFailedException(t265) put_n_take!(h265, ())
@test_throws TaskFailedException(t265) fetch(t265)
@test istaskdone(t265)
let ex = t265.exception
    @test ex isa MethodError
    @test ex.f == h265
    @test ex.args == ()
    @test ex.world == wc265-3
    str = sprint(showerror, ex)
    wc = get_world_counter()
    cmps = """
        MethodError: no method matching h265()
        The applicable method may be too new: running in world age $(wc265-3), while current world is $wc."""
    @test startswith(str, cmps)
    cmps = "\n  h265() (method too new to be called from this world context.)\n   $loc_h265"
    @test occursin(cmps, str)
end

# test for generated function correctness
# and min/max world computation validity of cache_method
f_gen265(x) = 1
@generated g_gen265(x) = f_gen265(x)
@generated h_gen265(x) = :(f_gen265(x))
f_gen265(x::Int) = 2
f_gen265(x::Type{Int}) = 3
@generated g_gen265b(x) = f_gen265(x)
@test h_gen265(0) == 2
@test g_gen265(0) == 1
@test f_gen265(Int) == 3
@test g_gen265b(0) == 3

# Test that old, invalidated specializations don't get revived for
# intermediate worlds by later additions to the method table that
# would have capped those specializations if they were still valid
f26506(@nospecialize(x)) = 1
g26506(x) = Base.inferencebarrier(f26506)(x[1])
z = Any["ABC"]
f26506(x::Int) = 2
g26506(z) # Places an entry for f26506(::String) in mt.name.cache
f26506(x::String) = 3
let cache = typeof(f26506).name.mt.cache
    # The entry we created above should have been truncated
    @test cache.min_world == cache.max_world
end
c26506_1, c26506_2 = Condition(), Condition()
# Captures the world age
result26506 = Any[]
t = Task(()->begin
    wait(c26506_1)
    push!(result26506, g26506(z))
    notify(c26506_2)
end)
yield(t)
f26506(x::Float64) = 4
let cache = typeof(f26506).name.mt.cache
    # The entry we created above should have been truncated
    @test cache.min_world == cache.max_world
end
notify(c26506_1)
wait(c26506_2)
@test result26506[1] == 3

# issue #38435
f38435(::Int, ::Any) = 1
f38435(::Any, ::Int) = 2
g38435(x) = f38435(x, x)
@test_throws MethodError(f38435, (1, 1), Base.get_world_counter()) g38435(1)
f38435(::Int, ::Int) = 3.0
@test g38435(1) === 3.0

# Invalidation
# ============

function method_instance(f, types=Base.default_tt(f))
    m = which(f, types)
    inst = nothing
    tt = Base.signature_type(f, types)
    for mi in Base.specializations(m)
        if mi.specTypes <: tt && tt <: mi.specTypes
            inst = mi
            break
        end
    end
    return inst
end

function worlds(mi::Core.MethodInstance)
    w = Tuple{UInt,UInt}[]
    if isdefined(mi, :cache)
        ci = mi.cache
        push!(w, (ci.min_world, ci.max_world))
        while isdefined(ci, :next)
            ci = ci.next
            push!(w, (ci.min_world, ci.max_world))
        end
    end
    return w
end

# avoid adding this to Base
function equal(ci1::Core.CodeInfo, ci2::Core.CodeInfo)
    return ci1.code == ci2.code &&
           ci1.debuginfo == ci2.debuginfo &&
           ci1.ssavaluetypes == ci2.ssavaluetypes &&
           ci1.ssaflags == ci2.ssaflags &&
           ci1.method_for_inference_limit_heuristics == ci2.method_for_inference_limit_heuristics &&
           ci1.slotnames == ci2.slotnames &&
           ci1.slotflags == ci2.slotflags &&
           ci1.slottypes == ci2.slottypes
end
equal(p1::Pair, p2::Pair) = p1.second == p2.second && equal(p1.first, p2.first)

## Union-splitting based on state-of-the-world: check that each invalidation corresponds to new code
applyf35855(c) = f35855(c[1])
f35855(::Int) = 1
f35855(::Float64) = 2
applyf35855([1])
applyf35855([1.0])
applyf35855(Any[1])
wint   = worlds(method_instance(applyf35855, (Vector{Int},)))
wfloat = worlds(method_instance(applyf35855, (Vector{Float64},)))
wany2  = worlds(method_instance(applyf35855, (Vector{Any},)))
src2 = code_typed(applyf35855, (Vector{Any},))[1]
f35855(::String) = 3
applyf35855(Any[1])
@test worlds(method_instance(applyf35855, (Vector{Int},))) == wint
@test worlds(method_instance(applyf35855, (Vector{Float64},))) == wfloat
wany3 = worlds(method_instance(applyf35855, (Vector{Any},)))
src3 = code_typed(applyf35855, (Vector{Any},))[1]
@test !(wany3 == wany2) || equal(src3, src2) # code doesn't change unless you invalidate
f35855(::AbstractVector) = 4
applyf35855(Any[1])
wany4 = worlds(method_instance(applyf35855, (Vector{Any},)))
src4 = code_typed(applyf35855, (Vector{Any},))[1]
@test !(wany4 == wany3) || equal(src4, src3) # code doesn't change unless you invalidate
f35855(::Dict) = 5
applyf35855(Any[1])
wany5 = worlds(method_instance(applyf35855, (Vector{Any},)))
src5 = code_typed(applyf35855, (Vector{Any},))[1]
@test (wany5 == wany4) == equal(src5, src4)
f35855(::Set) = 6    # with current settings, this shouldn't invalidate
applyf35855(Any[1])
wany6 = worlds(method_instance(applyf35855, (Vector{Any},)))
src6 = code_typed(applyf35855, (Vector{Any},))[1]
@test wany6 == wany5
@test equal(src6, src5)

applyf35855_2(c) = f35855_2(c[1])
f35855_2(::Int) = 1
f35855_2(::Float64) = 2
applyf35855_2(Any[1])
wany3 = worlds(method_instance(applyf35855_2, (Vector{Any},)))
src3 = code_typed(applyf35855_2, (Vector{Any},))[1]
f35855_2(::AbstractVector) = 4
applyf35855_2(Any[1])
wany4 = worlds(method_instance(applyf35855_2, (Vector{Any},)))
src4 = code_typed(applyf35855_2, (Vector{Any},))[1]
@test !(wany4 == wany3) || equal(src4, src3) # code doesn't change unless you invalidate

## ambiguities do not trigger invalidation
m = which(+, (Char, UInt8))
mi = Core.Compiler.specialize_method(m, Tuple{typeof(+), AbstractChar, UInt8}, Core.svec())
w = worlds(mi)

abstract type FixedPoint35855{T <: Integer} <: Real end
struct Normed35855 <: FixedPoint35855{UInt8}
    i::UInt8
    Normed35855(i::Integer, _) = new(i % UInt8)
end
(::Type{X})(x::Real) where {T, X<:FixedPoint35855{T}} = X(round(T, typemax(T)*x), 0)
@test worlds(mi) == w

mi = method_instance(convert, (Type{Nothing}, String))
w = worlds(mi)
abstract type Colorant35855 end
Base.convert(::Type{C}, c) where {C<:Colorant35855} = false
@test worlds(mi) == w

## NamedTuple and extensions of eltype
outer(anyc) = inner(anyc[])
inner(s::Union{Vector,Dict}; kw=false) = inneri(s, kwi=maximum(s), kwb=kw)
inneri(s, args...; kwargs...) = inneri(IOBuffer(), s, args...; kwargs...)
inneri(io::IO, s::Union{Vector,Dict}; kwi=0, kwb=false) = (print(io, first(s), " "^kwi, kwb); String(take!(io)))
@test outer(Ref{Any}([1,2,3])) == "1   false"
mi = method_instance(Core.kwcall, (NamedTuple{(:kwi,:kwb),TT} where TT<:Tuple{Any,Bool}, typeof(inneri), Vector{T} where T))
w = worlds(mi)
abstract type Container{T} end
Base.eltype(::Type{C}) where {T,C<:Container{T}} = T
@test worlds(mi) == w

## invoke call

_invoke46741(a::Int) = a > 0 ? :int : println(a)
_invoke46741(a::Integer) = a > 0 ? :integer : println(a)
invoke46741(a) = @invoke _invoke46741(a::Integer)
@test invoke46741(42) === :integer
invoke46741_world = worlds(method_instance(invoke46741, (Int,)))
_invoke46741(a::Int) = a > 0 ? :int2 : println(a)
@test invoke46741(42) === :integer
@test worlds(method_instance(invoke46741, (Int,))) == invoke46741_world
_invoke46741(a::UInt) = a > 0 ? :uint2 : println(a)
@test invoke46741(42) === :integer
@test worlds(method_instance(invoke46741, (Int,))) == invoke46741_world
_invoke46741(a::Integer) = a > 0 ? :integer2 : println(a)
@test invoke46741(42) === :integer2
@test worlds(method_instance(invoke46741, (Int,))) ≠ invoke46741_world

# const-prop'ed call
_invoke46741(a::Int) = a > 0 ? :int : println(a)
_invoke46741(a::Integer) = a > 0 ? :integer : println(a)
invoke46741() = @invoke _invoke46741(42::Integer)
@test invoke46741() === :integer
invoke46741_world = worlds(method_instance(invoke46741, ()))
_invoke46741(a::Int) = a > 0 ? :int2 : println(a)
@test invoke46741() === :integer
@test worlds(method_instance(invoke46741, ())) == invoke46741_world
_invoke46741(a::UInt) = a > 0 ? :uint2 : println(a)
@test invoke46741() === :integer
@test worlds(method_instance(invoke46741, ())) == invoke46741_world
_invoke46741(a::Integer) = a > 0 ? :integer2 : println(a)
@test invoke46741() === :integer2
@test worlds(method_instance(invoke46741, ())) ≠ invoke46741_world

# invoke_in_world
# ===============

f_inworld(x) = "world one; x=$x"
g_inworld(x; y) = "world one; x=$x, y=$y"
wc_aiw1 = get_world_counter()
# redefine f_inworld, g_inworld, and check that we can invoke both versions
f_inworld(x) = "world two; x=$x"
g_inworld(x; y) = "world two; x=$x, y=$y"
wc_aiw2 = get_world_counter()
@test Base.invoke_in_world(wc_aiw1, f_inworld, 2) == "world one; x=2"
@test Base.invoke_in_world(wc_aiw2, f_inworld, 2) == "world two; x=2"
@test Base.invoke_in_world(wc_aiw1, g_inworld, 2, y=3) == "world one; x=2, y=3"
@test Base.invoke_in_world(wc_aiw2, g_inworld, 2, y=3) == "world two; x=2, y=3"

# logging
mc48954(x, y) = false
mc48954(x::Int, y::Int) = x == y
mc48954(x::Symbol, y::Symbol) = x == y
function mcc48954(container, y)
    x = container[1]
    return mc48954(x, y)
end

mcc48954(Any[1], 1)
mc48954i = method_instance(mc48954, (Any, Int))
mcc48954i = method_instance(mcc48954, (Vector{Any}, Int))
list48954 = ccall(:jl_debug_method_invalidation, Any, (Cint,), 1)
mc48954(x::AbstractFloat, y::Int) = x == y
ccall(:jl_debug_method_invalidation, Any, (Cint,), 0)
@test list48954 == [
    mcc48954i,
    1,
    mc48954i,
    "jl_method_table_insert",
    which(mc48954, (AbstractFloat, Int)),
    "jl_method_table_insert"
]

# issue #50091 -- missing invoke edge affecting nospecialized dispatch
module ExceptionUnwrapping
@nospecialize
unwrap_exception(@nospecialize(e)) = e
unwrap_exception(e::Base.TaskFailedException) = e.task.exception
@noinline function _summarize_task_exceptions(io::IO, exc, prefix = nothing)
    _summarize_exception((;prefix,), io, exc)
    nothing
end
@noinline function _summarize_exception(kws, io::IO, e::TaskFailedException)
    _summarize_task_exceptions(io, e.task, kws.prefix)
end
# This is the overload that prints the actual exception that occurred.
result = Bool[]
@noinline function _summarize_exception(kws, io::IO, @nospecialize(exc))
    global result
    push!(result, unwrap_exception(exc) === exc)
    if unwrap_exception(exc) !== exc # something uninferrable
        return _summarize_exception(kws, io, unwrap_exception(exc))
    end
end
struct X; x; end
end
let e = ExceptionUnwrapping.X(nothing)
    @test ExceptionUnwrapping.unwrap_exception(e) === e
    ExceptionUnwrapping._summarize_task_exceptions(devnull, e)
    @test ExceptionUnwrapping.result == [true]
    empty!(ExceptionUnwrapping.result)
end
ExceptionUnwrapping.unwrap_exception(e::ExceptionUnwrapping.X) = e.x
let e = ExceptionUnwrapping.X(nothing)
    @test !(ExceptionUnwrapping.unwrap_exception(e) === e)
    ExceptionUnwrapping._summarize_task_exceptions(devnull, e)
    @test ExceptionUnwrapping.result == [false, true]
    empty!(ExceptionUnwrapping.result)
end

fshadow() = 1
gshadow() = fshadow()
@test fshadow() === 1
@test gshadow() === 1
fshadow_m1 = which(fshadow, ())
fshadow() = 2
fshadow() = 3
@test fshadow() === 3
@test gshadow() === 3
fshadow_m3 = which(fshadow, ())
Base.delete_method(fshadow_m1)
@test fshadow() === 3
@test gshadow() === 3
Base.delete_method(fshadow_m3)
fshadow_m2 = which(fshadow, ())
@test fshadow() === 2
@test gshadow() === 2
Base.delete_method(fshadow_m2)
@test_throws MethodError(fshadow, (), Base.tls_world_age()) gshadow()
@test Base.morespecific(fshadow_m3, fshadow_m2)
@test Base.morespecific(fshadow_m2, fshadow_m1)
@test Base.morespecific(fshadow_m3, fshadow_m1)
@test !Base.morespecific(fshadow_m2, fshadow_m3)

# Generated functions without edges must have min_world = 1.
# N.B.: If changing this, move this test to precompile and make sure
# that the specialization survives revalidation.
function generated_no_edges_gen(world, args...)
    src = ccall(:jl_new_code_info_uninit, Ref{Core.CodeInfo}, ())
    src.code = Any[Core.ReturnNode(nothing)]
    src.slotnames = Symbol[:self]
    src.slotflags = UInt8[0x00]
    src.ssaflags = UInt32[0x00]
    src.ssavaluetypes = 1
    src.nargs = 1
    src.min_world = first(Base._methods(generated_no_edges, Tuple{}, -1, world)).method.primary_world

    return src
end

@eval function generated_no_edges()
    $(Expr(:meta, :generated, generated_no_edges_gen))
    $(Expr(:meta, :generated_only))
end

@test_throws ErrorException("Generated function result with `edges == nothing` and `max_world == typemax(UInt)` must have `min_world == 1`") generated_no_edges()

# Test that backdating of constants is working for structs
before_backdate_age = Base.tls_world_age()
struct FooBackdated
    x::Vector{FooBackdated}

    FooBackdated() = new(FooBackdated[])
end
@test Base.invoke_in_world(before_backdate_age, isdefined, @__MODULE__, :FooBackdated)

# Test that ambiguous binding intersect the using'd binding's world ranges
module AmbigWorldTest
    using Test
    module M1; export x; end
    module M2; export x; end
    using .M1, .M2
    Core.eval(M1, :(x=1))
    Core.eval(M2, :(x=2))
    @test_throws UndefVarError x
    @test convert(Core.Binding, GlobalRef(@__MODULE__, :x)).partitions.min_world == max(
        convert(Core.Binding, GlobalRef(M1, :x)).partitions.min_world,
        convert(Core.Binding, GlobalRef(M2, :x)).partitions.min_world
    )
end

module X57316; module Y57316; end; end
module A57316; using ..X57316.Y57316, .Y57316.Y57316; end
module B57316; import ..X57316.Y57316, .Y57316.Y57316; end
module C57316; import ..X57316.Y57316 as Z, .Z.Y57316 as W; end
@test X57316.Y57316 === A57316.Y57316 === B57316.Y57316 === C57316.Z === C57316.W
@test !isdefined(A57316, :X57316)
@test !isdefined(B57316, :X57316)
@test !isdefined(C57316, :X57316)
@test !isdefined(C57316, :Y57316)
