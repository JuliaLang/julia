# This file is a part of Julia. License is MIT: https://julialang.org/license

# "is a null with type T", curried on 2nd argument
isnull_oftype(x::Nullable, T::Type) = eltype(x) == T && isnull(x)
isnull_oftype(T::Type) = x -> isnull_oftype(x, T)

# return true if nullables (or arrays of nullables) have the same type,
# nullity, and value (if they are non-null)
istypeequal(x::Nullable, y::Nullable) =
    typeof(x) == typeof(y) && isnull(filter(!, x .== y))
istypeequal(x::AbstractArray, y::AbstractArray) =
    length(x) == length(y) && all(xy -> istypeequal(xy...), zip(x, y))

types = [
    Bool,
    Float16,
    Float32,
    Float64,
    Int128,
    Int16,
    Int32,
    Int64,
    Int8,
    UInt16,
    UInt32,
    UInt64,
    UInt8,
]

# Nullable{T}() = new(true)
for T in types
    x = Nullable{T}()
    @test x.hasvalue === false
    @test isa(x.value, T)
    @test eltype(Nullable{T}) === T
    @test eltype(x) === T
end

# Nullable{T}(value::T) = new(false, value)
for T in types
    x = Nullable{T}(zero(T))
    @test x.hasvalue === true
    @test isa(x.value, T)
    @test x.value === zero(T)
    @test eltype(x) === T

    x = Nullable{T}(one(T))
    @test x.hasvalue === true
    @test isa(x.value, T)
    @test x.value === one(T)
    @test eltype(x) === T
end

# Nullable{T}(value::T, hasvalue::Bool) = new(hasvalue, value)
for T in types
    x = Nullable{T}(zero(T), true)
    @test x.hasvalue === true
    @test isa(x.value, T)
    @test x.value === zero(T)
    @test eltype(x) === T

    x = Nullable{T}(zero(T), false)
    @test x.hasvalue === false
    @test isa(x.value, T)
    @test eltype(Nullable{T}) === T
    @test eltype(x) === T
end


# struct NullException <: Exception
@test isa(NullException(), NullException)
@test_throws NullException throw(NullException())

# Nullable{T}(value::T) = Nullable{T}(value)
for T in types
    v = zero(T)
    x = Nullable(v)
    @test x.hasvalue === true
    @test isa(x.value, T)
    @test x.value === v

    v = one(T)
    x = Nullable(v)
    @test x.hasvalue === true
    @test isa(x.value, T)
    @test x.value === v
end

# show{T}(io::IO, x::Nullable{T})
io1 = IOBuffer()
io2 = IOBuffer()
for (i, T) in enumerate(types)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))
    show(io1, x1)
    @test String(take!(io1)) == @sprintf("Nullable{%s}()", T)
    show(io1, x2)
    showcompact(io2, get(x2))
    @test String(take!(io1)) == @sprintf("Nullable{%s}(%s)", T, String(take!(io2)))
    show(io1, x3)
    showcompact(io2, get(x3))
    @test String(take!(io1)) == @sprintf("Nullable{%s}(%s)", T, String(take!(io2)))

    a1 = [x2]
    show(IOContext(io1, :compact => false), a1)
    show(IOContext(io2, :compact => false), x2)
    @test String(take!(io1)) ==
        @sprintf("Nullable{%s}[%s]", string(T), String(take!(io2)))

    show(io1, a1)
    show(IOContext(io2, :compact => true), x2)
    @test String(take!(io1)) ==
        @sprintf("Nullable{%s}[%s]", string(T), String(take!(io2)))
end

module NullableTestEnum
import Test
# For curmod_*
include("testenv.jl")
io = IOBuffer()
@enum TestEnum a b
show(io, Nullable(a))
Test.@test String(take!(io)) == "Nullable{$(curmod_prefix)TestEnum}(a)"
end

# showcompact(io::IO, x::Nullable)
io1 = IOBuffer()
io2 = IOBuffer()
for (i, T) in enumerate(types)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))
    showcompact(io1, x1)
    @test String(take!(io1)) == "#NULL"
    showcompact(io1, x2)
    showcompact(io2, get(x2))
    @test String(take!(io1)) == String(take!(io2))
    showcompact(io1, x3)
    showcompact(io2, get(x3))
    @test String(take!(io1)) == String(take!(io2))

    a1 = [x2]
    showcompact(io1, a1)
    showcompact(io2, x2)
    @test String(take!(io1)) ==
        @sprintf("Nullable{%s}[%s]", string(T), String(take!(io2)))
end

# get(x::Nullable)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test_throws NullException get(x1)
    @test get(x2) === zero(T)
    @test get(x3) === one(T)
end

@test_throws NullException get(Nullable())

# get{S, T}(x::Nullable{S}, y::T)
for T in types
    x0 = Nullable()
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test get(x0, zero(T)) === zero(T)
    @test get(x0, one(T)) === one(T)
    @test get(x1, zero(T)) === zero(T)
    @test get(x1, one(T)) === one(T)
    @test get(x2, one(T)) === zero(T)
    @test get(x3, zero(T)) === one(T)
end

for T in types
    # unsafe_get(x::Nullable)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))
    a = rand(T)
    x4 = Nullable(a)

    @test isa(unsafe_get(x1), T)
    @test unsafe_get(x2) === zero(T)
    @test unsafe_get(x3) === one(T)
    @test unsafe_get(x4) === a

    # unsafe_get(x)
    x2 = zero(T)
    x3 = one(T)
    x4 = rand(T)

    @test unsafe_get(x2) === zero(T)
    @test unsafe_get(x3) === one(T)
    @test unsafe_get(x4) === x4
end

@test_throws UndefRefError unsafe_get(Nullable())
@test_throws UndefRefError unsafe_get(Nullable{String}())
@test_throws UndefRefError unsafe_get(Nullable{Array}())

for T in types
    # isnull(x::Nullable)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test isnull(x1) === true
    @test isnull(x2) === false
    @test isnull(x3) === false

    # isnull(x)
    x1 = zero(T)
    x2 = one(T)
    x3 = rand(T)

    @test isnull(x1) === false
    @test isnull(x2) === false
    @test isnull(x3) === false
end

@test isnull(Nullable())

# function =={S, T}(x::Nullable{S}, y::Nullable{T})
for T in types
    x0 = Nullable()
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test_throws NullException (x0 == x1)
    @test_throws NullException (x0 == x2)
    @test_throws NullException (x0 == x3)
    @test_throws NullException (x0 == x4)

    @test_throws NullException (x1 == x1)
    @test_throws NullException (x1 == x2)
    @test_throws NullException (x1 == x3)
    @test_throws NullException (x1 == x4)

    @test_throws NullException (x2 == x1)
    @test_throws NullException (x2 == x2)
    @test_throws NullException (x2 == x3)
    @test_throws NullException (x2 == x4)

    @test_throws NullException (x3 == x1)
    @test_throws NullException (x3 == x2)
    @test_throws NullException (x3 == x3)
    @test_throws NullException (x3 == x4)

    @test_throws NullException (x4 == x1)
    @test_throws NullException (x4 == x2)
    @test_throws NullException (x4 == x3)
    @test_throws NullException (x4 == x4)
end

# function hash(x::Nullable, h::UInt)
for T in types
    x0 = Nullable()
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test isa(hash(x0), UInt)
    @test isa(hash(x1), UInt)
    @test isa(hash(x2), UInt)
    @test isa(hash(x3), UInt)
    @test isa(hash(x4), UInt)

    @test hash(x0) == hash(x2)
    @test hash(x0) != hash(x3)
    @test hash(x0) != hash(x4)
    @test hash(x1) == hash(x2)
    @test hash(x1) != hash(x3)
    @test hash(x1) != hash(x4)
    @test hash(x2) != hash(x3)
    @test hash(x2) != hash(x4)
    @test hash(x3) != hash(x4)
end

mutable struct TestNType{T}
    v::Nullable{T}
end

for T in types
    x1 = TestNType{T}(Nullable{T}())
    @test isnull(x1.v)
    x1.v = one(T)
    @test !isnull(x1.v)
    @test get(x1.v, one(T)) === one(T)
end

# Operators
TestTypes = [[T.parameters[1] for T in Base.uniontypes(Base.NullSafeTypes)];
             [BigInt, BigFloat,
              Complex{Int}, Complex{Float64}, Complex{BigFloat},
              Rational{Int}, Rational{BigInt}]]
for S in TestTypes, T in TestTypes
    u0 = zero(S)
    u1 = one(S)
    if S <: AbstractFloat
        u2 = S(NaN)
    elseif S <: Complex && S.parameters[1] <: AbstractFloat
        u2 = S(NaN, NaN)
    else
        u2 = u1
    end

    v0 = zero(T)
    v1 = one(T)
    if T <: AbstractFloat
        v2 = T(NaN)
    elseif T <: Complex && T.parameters[1] <: AbstractFloat
        v2 = T(NaN, NaN)
    else
        v2 = v1
    end

    for u in (u0, u1, u2), v in (v0, v1, v2)
        # function isequal(x::Nullable, y::Nullable)
        @test isequal(Nullable(u), Nullable(v)) === isequal(u, v)
        @test isequal(Nullable(u), Nullable(u)) === true
        @test isequal(Nullable(v), Nullable(v)) === true

        @test isequal(Nullable(u), Nullable(v, false)) === false
        @test isequal(Nullable(u, false), Nullable(v)) === false
        @test isequal(Nullable(u, false), Nullable(v, false)) === true

        @test isequal(Nullable(u), Nullable{T}()) === false
        @test isequal(Nullable{S}(), Nullable(v)) === false
        @test isequal(Nullable{S}(), Nullable{T}()) === true

        @test isequal(Nullable(u), Nullable()) === false
        @test isequal(Nullable(), Nullable(v)) === false
        @test isequal(Nullable{S}(), Nullable()) === true
        @test isequal(Nullable(), Nullable{T}()) === true
        @test isequal(Nullable(), Nullable()) === true

        # function isless(x::Nullable, y::Nullable)
        if S <: Real && T <: Real
            @test isless(Nullable(u), Nullable(v)) === isless(u, v)
            @test isless(Nullable(u), Nullable(u)) === false
            @test isless(Nullable(v), Nullable(v)) === false

            @test isless(Nullable(u), Nullable(v, false)) === true
            @test isless(Nullable(u, false), Nullable(v)) === false
            @test isless(Nullable(u, false), Nullable(v, false)) === false

            @test isless(Nullable(u), Nullable{T}()) === true
            @test isless(Nullable{S}(), Nullable(v)) === false
            @test isless(Nullable{S}(), Nullable{T}()) === false

            @test isless(Nullable(u), Nullable()) === true
            @test isless(Nullable(), Nullable(v)) === false
            @test isless(Nullable{S}(), Nullable()) === false
            @test isless(Nullable(), Nullable{T}()) === false
            @test isless(Nullable(), Nullable()) === false
        end
    end
end

# issue #9462
for T in types
    @test isa(convert(Nullable{Number}, Nullable(one(T))), Nullable{Number})
    @test isa(convert(Nullable{Number}, one(T)), Nullable{Number})
    @test isa(convert(Nullable{T}, one(T)), Nullable{T})
    @test isa(convert(Nullable{Any}, Nullable(one(T))), Nullable{Any})
    @test isa(convert(Nullable{Any}, one(T)), Nullable{Any})

    # one(T) is convertible to every type in types
    # let's test that with Nullables
    for S in types
        @test isa(convert(Nullable{T}, one(S)), Nullable{T})
    end
end

@test isnull(convert(Nullable, nothing))
@test isnull(convert(Nullable{Int}, nothing))
@test isa(convert(Nullable{Int}, nothing), Nullable{Int})

@test convert(Nullable, 1) === Nullable(1)
@test convert(Nullable, Nullable(1)) === Nullable(1)
@test isequal(convert(Nullable, "a"), Nullable("a"))
@test isequal(convert(Nullable, Nullable("a")), Nullable("a"))

@test promote_type(Nullable{Int}, Int) === Nullable{Int}
@test promote_type(Nullable{Union{}}, Int) === Nullable{Int}
@test promote_type(Nullable{Float64}, Nullable{Int}) === Nullable{Float64}
@test promote_type(Nullable{Union{}}, Nullable{Int}) === Nullable{Int}
@test promote_type(Nullable{Date}, Nullable{DateTime}) === Nullable{DateTime}

@test Base.promote_op(+, Nullable{Int}, Nullable{Int}) == Nullable{Int}
@test Base.promote_op(-, Nullable{Int}, Nullable{Int}) == Nullable{Int}
@test Base.promote_op(+, Nullable{Float64}, Nullable{Int}) == Nullable{Float64}
@test Base.promote_op(-, Nullable{Float64}, Nullable{Int}) == Nullable{Float64}
@test Base.promote_op(-, Nullable{DateTime}, Nullable{DateTime}) == Nullable{Base.Dates.Millisecond}

# tests for istypeequal (which uses filter, broadcast)
@test istypeequal(Nullable(0), Nullable(0))
@test !istypeequal(Nullable(0), Nullable(0.0))
@test !istypeequal(Nullable(0), Nullable(1))
@test !istypeequal(Nullable(0), Nullable(1.0))
@test istypeequal([Nullable(0), Nullable(1)], [Nullable(0), Nullable(1)])
@test istypeequal([Nullable(0), Nullable(1)], Any[Nullable(0), Nullable(1)])
@test !istypeequal([Nullable(0), Nullable(1)], Any[Nullable(0.0), Nullable(1)])
@test !istypeequal([Nullable(0), Nullable(1)], [Nullable(0), Nullable(2)])
@test !istypeequal([Nullable(0), Nullable(1)],
                   [Nullable(0), Nullable(1), Nullable(2)])

# filter
for p in (_ -> true, _ -> false)
    @test @inferred(filter(p, Nullable()))      |> isnull_oftype(Union{})
    @test @inferred(filter(p, Nullable{Int}())) |> isnull_oftype(Int)
end
@test @inferred(filter(_ -> true, Nullable(85)))  === Nullable(85)
@test @inferred(filter(_ -> false, Nullable(85))) |> isnull_oftype(Int)
@test @inferred(filter(x -> x > 0, Nullable(85))) === Nullable(85)
@test @inferred(filter(x -> x < 0, Nullable(85))) |> isnull_oftype(Int)
@test get(@inferred(filter(x -> length(x) > 2, Nullable("test")))) == "test"
@test @inferred(filter(x -> length(x) > 5, Nullable("test"))) |>
    isnull_oftype(String)

# map
sqr(x) = x^2
@test @inferred(map(sqr, Nullable()))        |> isnull_oftype(Union{})
@test @inferred(map(sqr, Nullable{Int}()))   |> isnull_oftype(Int)
@test @inferred(map(sqr, Nullable(2)))       === Nullable(4)
@test @inferred(map(+, Nullable(0.0)))       === Nullable(0.0)
@test @inferred(map(+, Nullable(3.0, false)))=== Nullable(3.0, false)
@test @inferred(map(-, Nullable(1.0)))       === Nullable(-1.0)
@test @inferred(map(-, Nullable{Float64}())) |> isnull_oftype(Float64)
@test @inferred(map(sin, Nullable(1)))       === Nullable(sin(1))
@test @inferred(map(sin, Nullable{Int}()))   |> isnull_oftype(Float64)

# should not throw if function wouldn't be called
@test map(x -> x ? 0 : 0.0, Nullable())       |> isnull_oftype(Union{})
@test map(x -> x ? 0 : 0.0, Nullable(true))   === Nullable(0)
@test map(x -> x ? 0 : 0.0, Nullable(false))  === Nullable(0.0)
@test map(x -> x ? 0 : 0.0, Nullable{Bool}()) |> isnull_oftype(Union{})

# broadcast and elementwise
@test sin.(Nullable(0.0))            === Nullable(0.0)
@test sin.(Nullable{Float64}())      |> isnull_oftype(Float64)
@test @inferred(broadcast(sin, Nullable(0.0)))       === Nullable(0.0)
@test @inferred(broadcast(sin, Nullable{Float64}())) |> isnull_oftype(Float64)

@test Nullable(8) .+ Nullable(10)     === Nullable(18)
@test Nullable(8) .- Nullable(10)     === Nullable(-2)
@test Nullable(8) .+ Nullable{Int}()  |> isnull_oftype(Int)
@test Nullable{Int}() .- Nullable(10) |> isnull_oftype(Int)

@test @inferred(broadcast(log, 10, Nullable(1.0))) ===
      Nullable(0.0)
@test @inferred(broadcast(log, 10, Nullable{Float64}())) |>
      isnull_oftype(Float64)
@test @inferred(broadcast(log, Nullable(10), Nullable(1.0))) ===
      Nullable(0.0)
@test @inferred(broadcast(log, Nullable(10), Nullable{Float64}())) |>
      isnull_oftype(Float64)

@test Nullable(2) .^ Nullable(4)      === Nullable(16)
@test Nullable(2) .^ Nullable{Int}()  |> isnull_oftype(Int)

# multi-arg broadcast
@test (Nullable(1) .+ Nullable(1) .+ Nullable(1) .+ Nullable(1) .+ Nullable(1) .+
       Nullable(1) === Nullable(6))
@test (Nullable(1) .+ Nullable(1) .+ Nullable(1) .+ Nullable{Int}() .+
       Nullable(1) .+ Nullable(1) |> isnull_oftype(Int))

# these are not inferrable because there are too many arguments
us = map(Nullable, 1:20)
@test broadcast(max, us...) === Nullable(20)
@test isnull(broadcast(max, us..., Nullable{Int}()))

# test all elementwise operations
# note that elementwise operations are the same as broadcast
for op in (+, -, *, /, \, //, ==, <, !=, <=, ÷, %, <<, >>, ^)
    # op(1, 1) chosen because it works for all operations
    res = op(1, 1)
    @test @inferred(broadcast(op, Nullable(1), Nullable(1)))         ===
          Nullable(res)
    @test @inferred(broadcast(op, Nullable{Int}(), Nullable(1)))     |>
          isnull_oftype(typeof(res))
    @test @inferred(broadcast(op, Nullable(1), Nullable{Int}()))     |>
          isnull_oftype(typeof(res))
    @test @inferred(broadcast(op, Nullable{Int}(), Nullable{Int}())) |>
          isnull_oftype(typeof(res))
    @test @inferred(broadcast(op, Nullable(1), 1))                   ===
          Nullable(res)
    @test @inferred(broadcast(op, 1, Nullable(1)))                   ===
          Nullable(res)
end

# test reasonable results for Union{}
# the exact types of these is finnicky and depends on implementation details
# but is guaranteed to be at worst concrete and possibly Union{} on a good day
@test isnull(@inferred(Nullable() .+ Nullable()))
@test isnull(@inferred(Nullable() .+ 1))
@test isnull(@inferred(Nullable() .+ Nullable(1)))

# test that things don't pessimize because of non-homogenous types
@test Nullable(10.5) ===
    @inferred(broadcast(+, 1, 2, Nullable(3), Nullable(4.0), Nullable(1//2)))

# test fast path taken
for op in (+, *, -)
    for b1 in (false, true)
        for b2 in (false, true)
            @test Nullable{Int}(op(1, 2), b1 & b2) ===
                @inferred(broadcast(op, Nullable{Int}(1, b1),
                                        Nullable{Int}(2, b2)))
        end
    end
end

# issue #11675
@test repr(Nullable()) == "Nullable{Union{}}()"

# issue #19270
let f19270(x::S, y::T) where {S,T} = Base.promote_op(^, S, T)
    @test f19270(Nullable(0.0f0), Nullable(BigInt(0))) == Nullable{Float32}
end

# issue #21397
@test Nullable(Tuple) === Nullable{DataType}(Tuple)
