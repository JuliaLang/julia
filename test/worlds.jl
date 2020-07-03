# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for accurate updating of method tables

using Base: get_world_counter
tls_world_age() = ccall(:jl_get_tls_world_age, UInt, ())
@test typemax(UInt) > get_world_counter() == tls_world_age() > 0

# test simple method replacement
begin
    g265a() = f265a(0)
    f265a(x::Any) = 1
    @test g265a() == 1
    @test Base.return_types(g265a, ()) == Any[Int]
    @test Core.Compiler.return_type(g265a, ()) == Int

    f265a(x::Any) = 2.0
    @test g265a() == 2.0

    @test Base.return_types(g265a, ()) == Any[Float64]
    @test Core.Compiler.return_type(g265a, ()) == Float64
end

# test signature widening
begin
    f265b(x::Int) = 1
    let ty = Any[1, 2.0e0]
        global g265b(i::Int) = f265b(ty[i])
    end
    @test g265b(1) == 1
    @test Base.return_types(g265b, (Int,)) == Any[Int]
    @test Core.Compiler.return_type(g265b, (Int,)) == Int

    f265b(x::Any) = 2.0
    @test g265b(1) == 1
    @test g265b(2) == 2.0
    @test Base.return_types(g265b, (Int,)) == Any[Union{Int, Float64}]
    @test Core.Compiler.return_type(g265b, (Int,)) == Union{Int, Float64}
end

# test signature narrowing
begin
    g265c() = f265c(0)
    f265c(x::Any) = 1
    @test g265c() == 1
    @test Base.return_types(g265c, ()) == Any[Int]
    @test Core.Compiler.return_type(g265c, ()) == Int

    f265c(x::Int) = 2.0
    @test g265c() == 2.0

    @test Base.return_types(g265c, ()) == Any[Float64]
    @test Core.Compiler.return_type(g265c, ()) == Float64
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
@test Core.Compiler.return_type(B265_, (Int,)) == B265{Int}

  # add new constructors
B265(x::Float64, dummy::Nothing) = B265{Float64}(x, dummy)
B265(x::Any, dummy::Nothing) = B265{UInt8}(x, dummy)

  # make sure answers are updated
@test (B265_(1)::B265{Int}).field1 === 1
@test (B265_(2)::B265{Float64}).field1 === 2.0e0
@test (B265_(3)::B265{UInt8}).field1 === 0x03

@test Base.return_types(B265_, (Int,)) == Any[B265]
@test Core.Compiler.return_type(B265_, (Int,)) == B265


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
f265(::Any) = 1.0
@test wc265 + 1 == get_world_counter()
chnls, tasks = Base.channeled_tasks(2, wfunc)
t265 = tasks[1]

wc265 = get_world_counter()
@test put_n_take!(get_world_counter, ()) == wc265
@test put_n_take!(tls_world_age, ()) == wc265
f265(::Int) = 1
@test put_n_take!(get_world_counter, ()) == wc265 + 1 == get_world_counter() == tls_world_age()
@test put_n_take!(tls_world_age, ()) == wc265

@test g265() == Int[1, 1, 1]
@test Core.Compiler.return_type(f265, (Any,)) == Union{Float64, Int}
@test Core.Compiler.return_type(f265, (Int,)) == Int
@test Core.Compiler.return_type(f265, (Float64,)) == Float64

@test put_n_take!(g265, ()) == Float64[1.0, 1.0, 1.0]
@test put_n_take!(Core.Compiler.return_type, (f265, (Any,))) == Float64
@test put_n_take!(Core.Compiler.return_type, (f265, (Int,))) == Float64
@test put_n_take!(Core.Compiler.return_type, (f265, (Float64,))) == Float64
@test put_n_take!(Core.Compiler.return_type, (f265, (Float64,))) == Float64

# test that reflection ignores worlds
@test Base.return_types(f265, (Any,)) == Any[Int, Float64]
@test put_n_take!(Base.return_types, (f265, (Any,))) == Any[Int, Float64]

# test for method errors
h265() = true
loc_h265 = "$(@__FILE__):$(@__LINE__() - 1)"
@test h265()
@test_throws TaskFailedException(t265) put_n_take!(h265, ())
@test_throws TaskFailedException(t265) fetch(t265)
@test istaskdone(t265)
let ex = t265.exception
    @test ex isa MethodError
    @test ex.f == h265
    @test ex.args == ()
    @test ex.world == wc265
    str = sprint(showerror, ex)
    wc = get_world_counter()
    cmps = """
        MethodError: no method matching h265()
        The applicable method may be too new: running in world age $wc265, while current world is $wc."""
    @test startswith(str, cmps)
    cmps = "\n  h265() at $loc_h265 (method too new to be called from this world context.)"
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
g26506(x) = f26506(x[1])
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


## Invalidation tests

function instance(f, types)
    m = which(f, types)
    inst = nothing
    tt = Tuple{typeof(f), types...}
    specs = m.specializations
    if isa(specs, Nothing)
    elseif isa(specs, Core.SimpleVector)
        for i = 1:length(specs)
            if isassigned(specs, i)
                mi = specs[i]::Core.MethodInstance
                if mi.specTypes === tt
                    inst = mi
                    break
                end
            end
        end
    else
        Base.visit(specs) do mi
            if mi.specTypes === tt
                inst = mi
            end
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
           ci1.codelocs == ci2.codelocs &&
           ci1.ssavaluetypes == ci2.ssavaluetypes &&
           ci1.ssaflags == ci2.ssaflags &&
           ci1.method_for_inference_limit_heuristics == ci2.method_for_inference_limit_heuristics &&
           ci1.linetable == ci2.linetable &&
           ci1.slotnames == ci2.slotnames &&
           ci1.slotflags == ci2.slotflags &&
           ci1.slottypes == ci2.slottypes &&
           ci1.rettype == ci2.rettype
end
equal(p1::Pair, p2::Pair) = p1.second == p2.second && equal(p1.first, p2.first)

## Union-splitting based on state-of-the-world: check that each invalidation corresponds to new code
applyf35855(c) = f35855(c[1])
f35855(::Int) = 1
f35855(::Float64) = 2
applyf35855([1])
applyf35855([1.0])
applyf35855(Any[1])
wint   = worlds(instance(applyf35855, (Vector{Int},)))
wfloat = worlds(instance(applyf35855, (Vector{Float64},)))
wany2  = worlds(instance(applyf35855, (Vector{Any},)))
src2 = code_typed(applyf35855, (Vector{Any},))[1]
f35855(::String) = 3
applyf35855(Any[1])
@test worlds(instance(applyf35855, (Vector{Int},))) == wint
@test worlds(instance(applyf35855, (Vector{Float64},))) == wfloat
wany3 = worlds(instance(applyf35855, (Vector{Any},)))
src3 = code_typed(applyf35855, (Vector{Any},))[1]
@test (wany3 == wany2) == equal(src3, src2)   # don't invalidate unless you also change the code
f35855(::AbstractVector) = 4
applyf35855(Any[1])
wany4 = worlds(instance(applyf35855, (Vector{Any},)))
src4 = code_typed(applyf35855, (Vector{Any},))[1]
# this passes when max_methods == 3, fails when set to 4
@test (wany4 == wany3) == equal(src4, src3)
f35855(::Dict) = 5
applyf35855(Any[1])
wany5 = worlds(instance(applyf35855, (Vector{Any},)))
src5 = code_typed(applyf35855, (Vector{Any},))[1]
@test (wany5 == wany4) == equal(src5, src4)
f35855(::Set) = 6    # with current settings, this shouldn't invalidate
applyf35855(Any[1])
wany6 = worlds(instance(applyf35855, (Vector{Any},)))
src6 = code_typed(applyf35855, (Vector{Any},))[1]
@test (wany6 == wany5) == equal(src6, src5)

applyf35855_2(c) = f35855_2(c[1])
f35855_2(::Int) = 1
f35855_2(::Float64) = 2
applyf35855_2(Any[1])
wany3 = worlds(instance(applyf35855_2, (Vector{Any},)))
src3 = code_typed(applyf35855_2, (Vector{Any},))[1]
f35855_2(::AbstractVector) = 4         # next test would pass if this were ::Vector{Int}
applyf35855_2(Any[1])
wany4 = worlds(instance(applyf35855_2, (Vector{Any},)))
src4 = code_typed(applyf35855_2, (Vector{Any},))[1]
@test_broken (wany4 == wany3) == equal(src4, src3)

## ambiguities do not trigger invalidation
using Printf
Printf.gen("%f")
mi = instance(+, (AbstractChar, UInt8))
w = worlds(mi)

abstract type FixedPoint35855{T <: Integer} <: Real end
struct Normed35855 <: FixedPoint35855{UInt8}
    i::UInt8
    Normed35855(i::Integer, _) = new(i % UInt8)
end
(::Type{X})(x::Real) where X<:FixedPoint35855{T} where T = X(round(T, typemax(T)*x), 0)

@test_broken worlds(mi) == w

mi = instance(convert, (Type{Nothing}, String))
w = worlds(mi)
abstract type Colorant35855 end
Base.convert(::Type{C}, c) where C<:Colorant35855 = false
@test_broken worlds(mi) == w

# invoke_in_world
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

