types = [
    Bool,
    Char,
    Float16,
    Float32,
    Float64,
    Int128,
    Int16,
    Int32,
    Int64,
    Int8,
    Uint16,
    Uint32,
    Uint64,
    Uint8,
]

# Nullable{T}() = new(true)
for T in types
    x = Nullable{T}()
    @test x.isnull === true
    @test isa(x.value, T)
end

# Nullable{T}(value::T) = new(false, value)
for T in types
    x = Nullable{T}(zero(T))
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === zero(T)

    x = Nullable{T}(one(T))
    @test x.isnull === false
    @test isa(x.value, T)
    @test x.value === one(T)
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

p1s = [
    "Nullable{Bool}()",
    "Nullable{Char}()",
    "Nullable{Float16}()",
    "Nullable{Float32}()",
    "Nullable{Float64}()",
    "Nullable{Int128}()",
    "Nullable{Int16}()",
    "Nullable{Int32}()",
    "Nullable{Int64}()",
    "Nullable{Int8}()",
    "Nullable{Uint16}()",
    "Nullable{Uint32}()",
    "Nullable{Uint64}()",
    "Nullable{Uint8}()",
]

p2s = [
    "Nullable(false)",
    "Nullable('\0')",
    "Nullable(float16(0.0))",
    "Nullable(0.0f0)",
    "Nullable(0.0)",
    "Nullable(0)",
    "Nullable(0)",
    "Nullable(0)",
    "Nullable(0)",
    "Nullable(0)",
    "Nullable(0x0000)",
    "Nullable(0x00000000)",
    "Nullable(0x0000000000000000)",
    "Nullable(0x00)",
]

p3s = [
    "Nullable(true)",
    "Nullable('\x01')",
    "Nullable(float16(1.0))",
    "Nullable(1.0f0)",
    "Nullable(1.0)",
    "Nullable(1)",
    "Nullable(1)",
    "Nullable(1)",
    "Nullable(1)",
    "Nullable(1)",
    "Nullable(0x0001)",
    "Nullable(0x00000001)",
    "Nullable(0x0000000000000001)",
    "Nullable(0x01)",
]

# show{T}(io::IO, x::Nullable{T})
io = IOBuffer()
for (i, T) in enumerate(types)
    x1 = Nullable{T}()
    x2 = Nullable(zero(T))
    x3 = Nullable(one(T))
    show(io, x1)
    takebuf_string(io) == p1s[i]
    show(io, x2)
    takebuf_string(io) == p2s[i]
    show(io, x3)
    takebuf_string(io) == p3s[i]
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

# function hash(x::Nullable, h::Uint)
for T in types
    x1 = Nullable{T}()
    x2 = Nullable{T}()
    x3 = Nullable(zero(T))
    x4 = Nullable(one(T))

    @test isa(hash(x1), Uint)
    @test isa(hash(x2), Uint)
    @test isa(hash(x3), Uint)
    @test isa(hash(x4), Uint)

    @test hash(x1) == hash(x2)
    @test hash(x1) != hash(x3)
    @test hash(x1) != hash(x4)
    @test hash(x2) != hash(x3)
    @test hash(x2) != hash(x4)
    @test hash(x3) != hash(x4)
end
