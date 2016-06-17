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
    @test x.isnull === true
    @test isa(x.value, T)
    @test eltype(Nullable{T}) === T
    @test eltype(x) === T
end

# Nullable{T}(value::T) = new(false, value)
for T in types
    x = Nullable{T}(zero(T))
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === zero(T)
    @test eltype(x) === T

    x = Nullable{T}(one(T))
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === one(T)
    @test eltype(x) === T
end

# Nullable{T}(value::T, isnull::Bool) = new(isnull, value)
for T in types
    x = Nullable{T}(zero(T),false)
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === zero(T)
    @test eltype(x) === T

    x = Nullable{T}(zero(T),true)
    @test x.isnull === true
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
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === v

    v = one(T)
    x = Nullable(v)
    @test x.isnull === false
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

# get{S, T}(x::Nullable{S}, y::T)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test get(x1, zero(T)) === zero(T)
    @test get(x1, one(T)) === one(T)
    @test get(x2, one(T)) === zero(T)
    @test get(x3, zero(T)) === one(T)
end

# isnull(x::Nullable)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))

    @test isnull(x1) === true
    @test isnull(x2) === false
    @test isnull(x3) === false
end

# function isequal{S, T}(x::Nullable{S}, y::Nullable{T})
for T in types
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test isequal(x1, x1) === true
    @test isequal(x1, x2) === true
    @test isequal(x1, x3) === false
    @test isequal(x1, x4) === false

    @test isequal(x2, x1) === true
    @test isequal(x2, x2) === true
    @test isequal(x2, x3) === false
    @test isequal(x2, x4) === false

    @test isequal(x3, x1) === false
    @test isequal(x3, x2) === false
    @test isequal(x3, x3) === true
    @test isequal(x3, x4) === false

    @test isequal(x4, x1) === false
    @test isequal(x4, x2) === false
    @test isequal(x4, x3) === false
    @test isequal(x4, x4) === true
end

# function =={S, T}(x::Nullable{S}, y::Nullable{T})
for T in types
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

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
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test isa(hash(x1), UInt)
    @test isa(hash(x2), UInt)
    @test isa(hash(x3), UInt)
    @test isa(hash(x4), UInt)

    @test hash(x1) == hash(x2)
    @test hash(x1) != hash(x3)
    @test hash(x1) != hash(x4)
    @test hash(x2) != hash(x3)
    @test hash(x2) != hash(x4)
    @test hash(x3) != hash(x4)
end


## operators

srand(1)

# check for fast path (null-safe combinations of types)
for S in Union{Base.Checked.SignedInt, Base.Checked.UnsignedInt}.types,
    T in Union{Base.Checked.SignedInt, Base.Checked.UnsignedInt}.types
    # mixing signed and unsigned types is unsafe (slow path tested below)
    ((S <: Signed) $ (T <: Signed)) && continue

    u0 = zero(S)
    u1 = one(S)
    u2 = rand(S)

    v0 = zero(T)
    v1 = one(T)
    v2 = rand(T)
    # unary operators
    for op in (+, -, ~)
        @test op(Nullable(u0)) === Nullable(op(u0))
        @test op(Nullable(u1)) === Nullable(op(u1))
        @test op(Nullable(u2)) === Nullable(op(u2))
        @test op(Nullable(u0, true)) === Nullable(op(u0), true)
    end

    for u in (u0, u1, u2), v in (v0, v1, v2)
        # safe binary operators: === checks that the fast-path was taken (no branch)
        for op in (+, -, *, /, &, |, >>, <<, >>>,
                   <, >, <=, >=)
            @test op(Nullable(u), Nullable(v)) === Nullable(op(u, v))
            @test op(Nullable(u, true), Nullable(v, true)) === Nullable(op(u, v), true)
            @test op(Nullable(u), Nullable(v, true)) === Nullable(op(u, v), true)
            @test op(Nullable(u, true), Nullable(v)) === Nullable(op(u, v), true)
        end

        # unsafe binary operators: we cannot use === as the underlying value is undefined
        # ^
        if S <: Integer
            @test_throws DomainError Nullable(u)^Nullable(-signed(one(v)))
        end
        @test isequal(Nullable(u)^Nullable(2*one(T)), Nullable(u^(2*one(T))))
        R = Base.promote_op(^, S, T)
        x = Nullable(u, true)^Nullable(-abs(v), true)
        @test isnull(x) && eltype(x) === R
        x = Nullable(u, false)^Nullable(-abs(v), true)
        @test isnull(x) && eltype(x) === R
        x = Nullable(u, true)^Nullable(-abs(v), false)
        @test isnull(x) && eltype(x) === R

        # ÷ and %
        for op in (÷, %)
            if T <: Integer && v == 0
                @test_throws DivideError op(Nullable(u), Nullable(v))
            else
                @test isequal(op(Nullable(u), Nullable(v)), Nullable(op(u, v)))
            end
            R = Base.promote_op(op, S, T)
            x = op(Nullable(u, true), Nullable(v, true))
            @test isa(x, Nullable{R}) && isnull(x)
            x = op(Nullable(u, false), Nullable(v, true))
            @test isa(x, Nullable{R}) && isnull(x)
            x = op(Nullable(u, true), Nullable(v, false))
            @test isa(x, Nullable{R}) && isnull(x)
        end
    end
end

@test !Nullable(true) === Nullable(false)
@test !Nullable(false) === Nullable(true)
@test !(Nullable(true, true)) === Nullable(false, true)
@test !(Nullable(false, true)) === Nullable(true, true)

# test all types (including null-unsafe types and combinations of types)
for S in Union{Base.Checked.SignedInt, Base.Checked.UnsignedInt, BigInt, BigFloat}.types,
    T in Union{Base.Checked.SignedInt, Base.Checked.UnsignedInt, BigInt, BigFloat}.types
    u0 = zero(S)
    u1 = one(S)
    u2 = S <: Union{BigInt, BigFloat} ? S(rand(Int128)) : rand(S)

    v0 = zero(T)
    v1 = one(T)
    v2 = T <: Union{BigInt, BigFloat} ? T(rand(Int128)) : rand(T)

    v2 > 5 && (v2 = T(5)) # Work around issue #16989

    # unary operators
    for op in (+, -, ~) # !
        T <: BigFloat && op == (~) && continue
        R = Base.promote_op(op, T)
        x = op(Nullable(v0))
        @test isa(x, Nullable{R}) && isequal(x, Nullable(op(v0)))
        x = op(Nullable(v1))
        @test isa(x, Nullable{R}) && isequal(x, Nullable(op(v1)))
        x = op(Nullable(v2))
        @test isa(x, Nullable{R}) && isequal(x, Nullable(op(v2)))
        x = op(Nullable(v0, true))
        @test isa(x, Nullable{R}) && isnull(x)
        x = op(Nullable{R}())
        @test isa(x, Nullable{R}) && isnull(x)
    end

    for u in (u0, u1, u2), v in (v0, v1, v2)
        # safe binary operators
        for op in (+, -, *, /, &, |, >>, <<, >>>,
                   <, >, <=, >=)
            (T <: BigFloat || S <: BigFloat) && op in (&, |, >>, <<, >>>) && continue
            if S <: Unsigned || T <: Unsigned
                @test isequal(op(Nullable(abs(u)), Nullable(abs(v))), Nullable(op(abs(u), abs(v))))
            else
                @test isequal(op(Nullable(u), Nullable(v)), Nullable(op(u, v)))
            end
            R = Base.promote_op(op, S, T)
            x = op(Nullable(u, true), Nullable(v, true))
            @test isa(x, Nullable{R}) && isnull(x)
            x = op(Nullable(u), Nullable(v, true))
            @test isa(x, Nullable{R}) && isnull(x)
            x = op(Nullable(u, true), Nullable(v))
            @test isa(x, Nullable{R}) && isnull(x)
        end

        # unsafe binary operators
        # ^
        if S <: Integer && !(T <: BigFloat)
            @test_throws DomainError Nullable(u)^Nullable(-signed(one(v)))
        end
        @test isequal(Nullable(u)^Nullable(2*one(T)), Nullable(u^(2*one(T))))
        R = Base.promote_op(^, S, T)
        x = Nullable(u, true)^Nullable(-abs(v), true)
        @test isnull(x) && eltype(x) === R
        x = Nullable(u, false)^Nullable(-abs(v), true)
        @test isnull(x) && eltype(x) === R
        x = Nullable(u, true)^Nullable(-abs(v), false)
        @test isnull(x) && eltype(x) === R

        # ÷ and %
        for op in (÷, %)
            if S <: Integer && T <: Integer && v == 0
                @test_throws DivideError op(Nullable(u), Nullable(v))
            else
                @test isequal(op(Nullable(u), Nullable(v)), Nullable(op(u, v)))
            end
            R = Base.promote_op(op, S, T)
            x = op(Nullable(u, true), Nullable(v, true))
            @test isnull(x) && eltype(x) === R
            x = op(Nullable(u, false), Nullable(v, true))
            @test isnull(x) && eltype(x) === R
            x = op(Nullable(u, true), Nullable(v, false))
            @test isnull(x) && eltype(x) === R
        end
    end
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
