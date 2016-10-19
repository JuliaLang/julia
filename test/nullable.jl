# This file is a part of Julia. License is MIT: http://julialang.org/license

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


# immutable NullException <: Exception
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
    @test takebuf_string(io1) == @sprintf("Nullable{%s}()", T)
    show(io1, x2)
    showcompact(io2, get(x2))
    @test takebuf_string(io1) == @sprintf("Nullable{%s}(%s)", T, takebuf_string(io2))
    show(io1, x3)
    showcompact(io2, get(x3))
    @test takebuf_string(io1) == @sprintf("Nullable{%s}(%s)", T, takebuf_string(io2))

    a1 = [x2]
    show(IOContext(io1, compact=false), a1)
    show(IOContext(io2, compact=false), x2)
    @test takebuf_string(io1) ==
        @sprintf("Nullable{%s}[%s]", string(T), takebuf_string(io2))

    show(io1, a1)
    show(IOContext(io2, compact=true), x2)
    @test takebuf_string(io1) ==
        @sprintf("Nullable{%s}[%s]", string(T), takebuf_string(io2))
end

module NullableTestEnum
    io = IOBuffer()
    @enum TestEnum a b
    show(io, Nullable(a))
    Base.Test.@test takebuf_string(io) == "Nullable{NullableTestEnum.TestEnum}(a)"
end

# showcompact(io::IO, x::Nullable)
io1 = IOBuffer()
io2 = IOBuffer()
for (i, T) in enumerate(types)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))
    showcompact(io1, x1)
    @test takebuf_string(io1) == "#NULL"
    showcompact(io1, x2)
    showcompact(io2, get(x2))
    @test takebuf_string(io1) == takebuf_string(io2)
    showcompact(io1, x3)
    showcompact(io2, get(x3))
    @test takebuf_string(io1) == takebuf_string(io2)

    a1 = [x2]
    showcompact(io1, a1)
    showcompact(io2, x2)
    @test takebuf_string(io1) ==
        @sprintf("Nullable{%s}[%s]", string(T), takebuf_string(io2))
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

type TestNType{T}
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

SafeTestTypes = Union{Base.NullSafeTypes, BigInt, BigFloat,
                      Complex{Int}, Complex{Float64}, Rational{Int}}.types

# check for fast path (null-safe combinations of operators and types)
for S in Base.NullSafeTypes.types, T in Base.NullSafeTypes.types
    # mixing signed and unsigned types is unsafe (slow path tested below)
    if !((S <: Signed && T <: Signed) ||
         (S <: Unsigned && T <: Unsigned) ||
         (S <: AbstractFloat && T <: AbstractFloat) ||
         (S == T))
        continue
    end

    u0 = zero(S)
    u1 = one(S)
    u2 = rand(S)

    v0 = zero(T)
    v1 = one(T)
    v2 = rand(T)

    # safe unary operators
    for op in (+, -, ~, abs, abs2, cbrt)
        S <: AbstractFloat && op == (~) && continue
        !(T <: Real) && op == cbrt && continue

        @test op(Nullable(u0)) === Nullable(op(u0))
        @test op(Nullable(u1)) === Nullable(op(u1))
        @test op(Nullable(u2)) === Nullable(op(u2))
        @test op(Nullable(u0, false)) === Nullable(op(u0), false)
    end

    for u in (u0, u1, u2), v in (v0, v1, v2)
        # safe binary operators: === checks that the fast-path was taken (no branch)
        for op in (+, -, *, /, &, |, >>, <<, >>>,
                   Base.scalarmin, Base.scalarmax)
            (T <: AbstractFloat || S <: AbstractFloat) && op in (&, |, >>, <<, >>>) && continue

            @test op(Nullable(u), Nullable(v)) === Nullable(op(u, v))
            @test op(Nullable(u, false), Nullable(v, false)) === Nullable(op(u, v), false)
            @test op(Nullable(u), Nullable(v, false)) === Nullable(op(u, v), false)
            @test op(Nullable(u, false), Nullable(v)) === Nullable(op(u, v), false)
        end
    end
end

@test !Nullable(true) === Nullable(false)
@test !Nullable(false) === Nullable(true)
@test !(Nullable(true, false)) === Nullable(false, false)
@test !(Nullable(false, false)) === Nullable(true, false)

# test all types and operators (including null-unsafe ones)

ensure_neg(x::Unsigned) = -convert(Signed, x)
ensure_neg{T<:Complex}(x::T) = T(-abs(real(x)), -abs(imag(x)))
ensure_neg(x::Any) = -abs(x)

TestTypes = Union{Base.NullSafeTypes, BigInt, BigFloat,
                  Complex{Int}, Complex{Float64}, Complex{BigFloat},
                  Rational{Int}, Rational{BigInt}}.types
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

    # safe unary operators
    for op in (+, -, ~, abs, abs2, cbrt)
        !(T <: Integer) && op == (~) && continue
        !(T <: Real) && op == cbrt && continue

        R = Base.promote_op(op, T)
        x = op(Nullable(v0))
        @test isa(x, Nullable{R}) && isequal(x, Nullable(op(v0)))
        x = op(Nullable(v1))
        @test isa(x, Nullable{R}) && isequal(x, Nullable(op(v1)))
        x = op(Nullable(v2))
        @test isa(x, Nullable{R}) && isequal(x, Nullable(op(v2)))
        x = op(Nullable(v0, false))
        @test isa(x, Nullable{R}) && isnull(x)
        x = op(Nullable(v1, false))
        @test isa(x, Nullable{R}) && isnull(x)
        x = op(Nullable(v2, false))
        @test isa(x, Nullable{R}) && isnull(x)
        x = op(Nullable{R}())
        @test isa(x, Nullable{R}) && isnull(x)

        x = op(Nullable())
        @test isa(x, Nullable{Union{}}) && isnull(x)
    end

    # unsafe unary operators
    # sqrt
    T <: Real && @test_throws DomainError sqrt(Nullable(ensure_neg(v1)))
    R = Base.promote_op(sqrt, T)
    x = sqrt(Nullable(v0))
    @test isa(x, Nullable{R}) && isequal(x, Nullable(sqrt(v0)))
    x = sqrt(Nullable(v1))
    @test isa(x, Nullable{R}) && isequal(x, Nullable(sqrt(v1)))
    x = sqrt(Nullable(v0, false))
    @test isa(x, Nullable{R}) && isnull(x)
    x = sqrt(Nullable(ensure_neg(v1), false))
    @test isa(x, Nullable{R}) && isnull(x)
    x = sqrt(Nullable(ensure_neg(v2), false))
    @test isa(x, Nullable{R}) && isnull(x)
    x = sqrt(Nullable{R}())
    @test isa(x, Nullable{R}) && isnull(x)

    x = sqrt(Nullable())
    @test isa(x, Nullable{Union{}}) && isnull(x)

    for u in (u0, u1, u2), v in (v0, v1, v2)
        # safe binary operators
        for op in (+, -, *, /, &, |, >>, <<, >>>,
                   Base.scalarmin, Base.scalarmax)
            (T <: AbstractFloat || S <: AbstractFloat) && op in (&, |, >>, <<, >>>) && continue
            (T <: Bool || S <: Bool) && op in (>>, <<, >>>) && continue
            (T <: BigInt || S <: BigInt) && op in (&, |, >>, <<, >>>) && continue
            (T <: Complex || S <: Complex) && op in (&, |, >>, <<, >>>, Base.scalarmin, Base.scalarmax) && continue
            (T <: Rational || S <: Rational) && op in (-, /, &, |, >>, <<, >>>, Base.scalarmin, Base.scalarmax) && continue

            if S <: Unsigned || T <: Unsigned
                @test isequal(op(Nullable(abs(u)), Nullable(abs(v))), Nullable(op(abs(u), abs(v))))
            else
                @test isequal(op(Nullable(u), Nullable(v)), Nullable(op(u, v)))
            end
            R = Base.promote_op(op, S, T)
            x = op(Nullable(u, false), Nullable(v, false))
            @test isa(x, Nullable{R}) && isnull(x)
            x = op(Nullable(u), Nullable(v, false))
            @test isa(x, Nullable{R}) && isnull(x)
            x = op(Nullable(u, false), Nullable(v))
            @test isa(x, Nullable{R}) && isnull(x)

            x = op(Nullable(u, false), Nullable())
            @test isa(x, Nullable{S}) && isnull(x)
            x = op(Nullable(), Nullable(u, false))
            @test isa(x, Nullable{S}) && isnull(x)
            x = op(Nullable(), Nullable())
            @test isa(x, Nullable{Union{}}) && isnull(x)
        end

        # unsafe binary operators
        # ^
        if S <: Integer && T <: Integer && u != 0 && u != 1 && v != 0
            @test_throws DomainError Nullable(u)^Nullable(ensure_neg(v))
        end
        @test isequal(Nullable(u)^Nullable(one(T)+one(T)), Nullable(u^(one(T)+one(T))))
        R = Base.promote_op(^, S, T)
        if S <: Real && T <: Real
            x = Nullable(u, false)^Nullable(-abs(v), false)
            @test isnull(x) && eltype(x) === R
            x = Nullable(u, true)^Nullable(-abs(v), false)
            @test isnull(x) && eltype(x) === R
            x = Nullable(u, false)^Nullable(-abs(v), true)
            @test isnull(x) && eltype(x) === R
        else
            x = Nullable(u, false)^Nullable(v, false)
            @test isnull(x) && eltype(x) === R
            x = Nullable(u, true)^Nullable(v, false)
            @test isnull(x) && eltype(x) === R
            x = Nullable(u, false)^Nullable(v, true)
            @test isnull(x) && eltype(x) === R
        end

        x = Nullable(u, false)^Nullable()
        @test isa(x, Nullable{S}) && isnull(x)
        x = Nullable()^Nullable(u, false)
        @test isa(x, Nullable{S}) && isnull(x)
        x = Nullable()^Nullable()
        @test isa(x, Nullable{Union{}}) && isnull(x)

        if S <: Real && T <: Real
            # ÷ and %
            for op in (÷, %)
                if S <: Union{Integer, Rational} && T <: Union{Integer, Rational} && v == 0
                    @test_throws DivideError op(Nullable(u), Nullable(v))
                else
                    @test isequal(op(Nullable(u), Nullable(v)), Nullable(op(u, v)))
                end
                R = Base.promote_op(op, S, T)
                x = op(Nullable(u, false), Nullable(v, false))
                @test isnull(x) && eltype(x) === R
                x = op(Nullable(u, true), Nullable(v, false))
                @test isnull(x) && eltype(x) === R
                x = op(Nullable(u, false), Nullable(v, true))
                @test isnull(x) && eltype(x) === R

                x = op(Nullable(u, false), Nullable())
                @test isa(x, Nullable{S}) && isnull(x)
                x = op(Nullable(), Nullable(u, false))
                @test isa(x, Nullable{S}) && isnull(x)
                x = op(Nullable(), Nullable())
                @test isa(x, Nullable{Union{}}) && isnull(x)
            end
        end

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

# issue #11675
@test repr(Nullable()) == "Nullable{Union{}}()"
