# This file is a part of Julia. License is MIT: http://julialang.org/license

# tests for accurate updating of method tables

tls_world_age() = ccall(:jl_get_tls_world_age, UInt, ())
world_counter() = ccall(:jl_get_world_counter, UInt, ())
@test typemax(UInt) > world_counter() == tls_world_age() > 0

# test simple method replacement
begin
    g265a() = f265a(0)
    f265a(x::Any) = 1
    @test g265a() == 1
    @test Base.return_types(g265a, ()) == Any[Int]
    @test Core.Inference.return_type(g265a, ()) == Int

    f265a(x::Any) = 2.0
    @test g265a() == 2.0

    @test Base.return_types(g265a, ()) == Any[Float64]
    @test Core.Inference.return_type(g265a, ()) == Float64
end

# test signature widening
begin
    f265b(x::Int) = 1
    let ty = Any[1, 2.0e0]
        global g265b(i::Int) = f265b(ty[i])
    end
    @test g265b(1) == 1
    @test Base.return_types(g265b, (Int,)) == Any[Int]
    @test Core.Inference.return_type(g265b, (Int,)) == Int

    f265b(x::Any) = 2.0
    @test g265b(1) == 1
    @test g265b(2) == 2.0
    @test Base.return_types(g265b, (Int,)) == Any[Union{Int, Float64}]
    @test Core.Inference.return_type(g265b, (Int,)) == Union{Int, Float64}
end

# test signature narrowing
begin
    g265c() = f265c(0)
    f265c(x::Any) = 1
    @test g265c() == 1
    @test Base.return_types(g265c, ()) == Any[Int]
    @test Core.Inference.return_type(g265c, ()) == Int

    f265c(x::Int) = 2.0
    @test g265c() == 2.0

    @test Base.return_types(g265c, ()) == Any[Float64]
    @test Core.Inference.return_type(g265c, ()) == Float64
end

# test constructor narrowing
type A265{T}
    field1::T
end
A265_() = A265(1)
@test (A265_()::A265{Int}).field1 === 1
A265(fld::Int) = A265(Float64(fld))
@test (A265_()::A265{Float64}).field1 === 1.0e0

# test constructor widening
type B265{T}
    field1::T
    # dummy arg is present to prevent (::Type{T}){T}(arg) from matching the test calls
    B265(field1::Any, dummy::Void) = new(field1) # prevent generation of outer ctor
end
  # define some constructors
B265(x::Int, dummy::Void) = B265{Int}(x, dummy)
let ty = Any[1, 2.0e0, 3.0f0]
    global B265_(i::Int) = B265(ty[i], nothing)
end
  # test for correct answers
@test (B265_(1)::B265{Int}).field1 === 1
@test_throws MethodError B265_(2)
@test_throws MethodError B265_(3)
@test Base.return_types(B265_, (Int,)) == Any[B265{Int}]
@test Core.Inference.return_type(B265_, (Int,)) == B265{Int}

  # add new constructors
B265(x::Float64, dummy::Void) = B265{Float64}(x, dummy)
B265(x::Any, dummy::Void) = B265{UInt8}(x, dummy)

  # make sure answers are updated
@test (B265_(1)::B265{Int}).field1 === 1
@test (B265_(2)::B265{Float64}).field1 === 2.0e0
@test (B265_(3)::B265{UInt8}).field1 === 0x03

@test Base.return_types(B265_, (Int,)) == Any[Union{B265{Float64}, B265{Int}, B265{UInt8}}]
@test Core.Inference.return_type(B265_, (Int,)) == Union{B265{Float64}, B265{Int}, B265{UInt8}}


# test oldworld call / inference
g265() = [f265(x) for x in 1:3.]
wc265 = world_counter()
f265(::Any) = 1.0
@test wc265 + 1 == world_counter()
t265 = @async begin
    local ret = nothing
    while true
        (f, args) = produce(ret)
        ret = f(args...)
    end
end
@test consume(t265) === nothing
wc265 = world_counter()
@test consume(t265, world_counter, ()) == wc265
@test consume(t265, tls_world_age, ()) == wc265
f265(::Int) = 1
@test consume(t265, world_counter, ()) == wc265 + 1 == world_counter() == tls_world_age()
@test consume(t265, tls_world_age, ()) == wc265

@test g265() == Int[1, 1, 1]
@test Core.Inference.return_type(f265, (Any,)) == Union{Float64, Int}
@test Core.Inference.return_type(f265, (Int,)) == Int
@test Core.Inference.return_type(f265, (Float64,)) == Float64

@test consume(t265, g265, ()) == Float64[1.0, 1.0, 1.0]
@test consume(t265, Core.Inference.return_type, (f265, (Any,))) == Float64
@test consume(t265, Core.Inference.return_type, (f265, (Int,))) == Float64
@test consume(t265, Core.Inference.return_type, (f265, (Float64,))) == Float64
@test consume(t265, Core.Inference.return_type, (f265, (Float64,))) == Float64


# test that reflection ignores worlds
@test Base.return_types(f265, (Any,)) == Any[Int, Float64]
@test consume(t265, Base.return_types, (f265, (Any,))) == Any[Int, Float64]


# test for method errors
h265() = true
loc_h265 = "$(Base.source_path()):$(@__LINE__ - 1)"
@test h265()
@test_throws MethodError consume(t265, h265, ())
@test_throws MethodError wait(t265)
@test istaskdone(t265)
let ex = t265.exception
    @test ex.f == h265
    @test ex.args == ()
    @test ex.world == wc265
    str = sprint(showerror, ex)
    wc = world_counter()
    cmp = """
        MethodError: no method matching h265()
        The applicable method may be too new: running in world age $wc265, while current world is $wc."""
    @test startswith(str, cmp)
    cmp = "\n  h265() at $loc_h265 (method too new to be called from this world context.)"
    @test contains(str, cmp)
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
