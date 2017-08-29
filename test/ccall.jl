# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base.copy, Base.==

const libccalltest = "libccalltest"

const verbose = false
ccall((:set_verbose, libccalltest), Void, (Int32,), verbose)


# Test for proper argument register truncation
ccall_test_func(x) = ccall((:testUcharX, libccalltest), Int32, (UInt8,), x % UInt8)
@test ccall_test_func(3) == 1
@test ccall_test_func(259) == 1


# Test for proper round-trip of Ref{T} type
function gen_ccall_echo(x, T, U, ret=nothing)
    # Construct a noninline function to do all the work, this is necessary
    # to make sure object x is still valid (rooted as argument)
    # when loading the pointer.
    # This works as long as we still keep the argument
    # rooted but might fail if we are smarter about eliminating dead root.

    # `eval` in global scope to make sure the function is not a closure
    func_ex = :(ccall((:test_echo_p, libccalltest), $T, ($U,), x))
    # It is not allowed to allocate after the ccall returns
    # and before calling `ret`.
    if ret !== nothing
        func_ex = :($ret($func_ex))
    end
    @gensym func_name
    @eval @noinline $func_name(x) = $func_ex
    :($func_name($(esc(x))))
end

macro ccall_echo_func(x, T, U)
    gen_ccall_echo(x, T, U)
end
macro ccall_echo_load(x, T, U)
    gen_ccall_echo(x, T, U, :unsafe_load)
end
macro ccall_echo_objref(x, T, U)
    gen_ccall_echo(x, :(Ptr{$T}), U, :unsafe_pointer_to_objref)
end

mutable struct IntLike
    x::Int
end
@test @ccall_echo_load(132, Ptr{Int}, Ref{Int}) === 132
@test @ccall_echo_load(Ref(921), Ptr{Int}, Ref{Int}) === 921
@test @ccall_echo_load(IntLike(993), Ptr{Int}, Ref{IntLike}) === 993
@test @ccall_echo_load(IntLike(881), Ptr{IntLike}, Ref{IntLike}).x === 881
@test @ccall_echo_func(532, Int, Int) === 532
if Sys.WORD_SIZE == 64
    # this test is valid only for x86_64 and win64
    @test @ccall_echo_func(164, IntLike, Int).x === 164
end
@test @ccall_echo_func(IntLike(828), Int, IntLike) === 828
@test @ccall_echo_func(913, Any, Any) === 913
@test @ccall_echo_objref(553, Ptr{Any}, Any) === 553
@test @ccall_echo_func(124, Ref{Int}, Any) === 124
@test @ccall_echo_load(422, Ptr{Any}, Ref{Any}) === 422
@test @ccall_echo_load([383], Ptr{Int}, Ref{Int}) === 383
@test @ccall_echo_load(Ref([144,172],2), Ptr{Int}, Ref{Int}) === 172
# @test @ccall_echo_load(Ref([8],1,1), Ptr{Int}, Ref{Int}) === 8


## Tests for passing and returning structs

let a, ci_ary, x
    a = 20 + 51im

    x = ccall((:ctest, libccalltest), Complex{Int}, (Complex{Int},), a)

    @test x == a + 1 - 2im

    ci_ary = [a] # Make sure the array is alive during unsafe_load
    x = unsafe_load(ccall((:cptest, libccalltest), Ptr{Complex{Int}},
                          (Ptr{Complex{Int}},), ci_ary))

    @test x == a + 1 - 2im
    @test a == 20 + 51im

    x = ccall((:cptest_static, libccalltest), Ptr{Complex{Int}}, (Ref{Complex{Int}},), a)
    @test unsafe_load(x) == a
    Libc.free(convert(Ptr{Void}, x))
end

let a, b, x
    a = 2.84 + 5.2im

    x = ccall((:cgtest, libccalltest), Complex128, (Complex128,), a)

    @test x == a + 1 - 2im

    b = [a] # Make sure the array is alive during unsafe_load
    x = unsafe_load(ccall((:cgptest, libccalltest), Ptr{Complex128}, (Ptr{Complex128},), b))

    @test x == a + 1 - 2im
    @test a == 2.84 + 5.2im
end

let a, b, x
    a = 3.34f0 + 53.2f0im

    x = ccall((:cftest, libccalltest), Complex64, (Complex64,), a)

    @test x == a + 1 - 2im

    b = [a] # Make sure the array is alive during unsafe_load
    x = unsafe_load(ccall((:cfptest, libccalltest), Ptr{Complex64}, (Ptr{Complex64},), b))

    @test x == a + 1 - 2im
    @test a == 3.34f0 + 53.2f0im
end


## Tests for native Julia data types

let a
    a = 2.84 + 5.2im

    @test_throws MethodError ccall((:cptest, libccalltest), Ptr{Complex{Int}}, (Ptr{Complex{Int}},), a)
    @test_throws MethodError ccall((:cptest, libccalltest), Ptr{Complex{Int}}, (Complex{Int},), &a)
end


## Tests for various sized data types (ByVal)

mutable struct Struct1
    x::Float32
    y::Float64
end
struct Struct1I
    x::Float32
    y::Float64
end
copy(a::Struct1) = Struct1(a.x, a.y)
copy(a::Struct1I) = a

function test_struct1(::Type{Struct}) where {Struct}
    a = Struct(352.39422f23, 19.287577)
    b = Float32(123.456)

    a2 = copy(a)
    if Struct === Struct1
        x = ccall((:test_1, libccalltest), Struct1, (Struct1, Float32), a2, b)
    else
        x = ccall((:test_1, libccalltest), Struct1I, (Struct1I, Float32), a2, b)
    end

    @test a2.x == a.x && a2.y == a.y
    @test !(a2 === x)

    @test x.x ≈ a.x + 1*b
    @test x.y ≈ a.y - 2*b
end
test_struct1(Struct1)
test_struct1(Struct1I)

let a, b, x
    a = Struct1(352.39422f23, 19.287577)
    b = Float32(123.456)
    a2 = copy(a)

    x = ccall((:test_1long_a, libccalltest), Struct1, (Int, Int, Int, Struct1, Float32), 2, 3, 4, a2, b)
    @test a2.x == a.x && a2.y == a.y
    @test !(a2 === x)
    @test x.x ≈ a.x + b + 9
    @test x.y ≈ a.y - 2*b

    x = ccall((:test_1long_b, libccalltest), Struct1, (Int, Float64, Int, Struct1, Float32), 2, 3, 4, a2, b)
    @test a2.x == a.x && a2.y == a.y
    @test !(a2 === x)
    @test x.x ≈ a.x + b + 9
    @test x.y ≈ a.y - 2*b

    x = ccall((:test_1long_c, libccalltest), Struct1, (Int, Float64, Int, Int, Struct1, Float32), 2, 3, 4, 5, a2, b)
    @test a2.x == a.x && a2.y == a.y
    @test !(a2 === x)
    @test x.x ≈ a.x + b + 14
    @test x.y ≈ a.y - 2*b
end

let a, b, x, y
    a = Complex{Int32}(Int32(10),Int32(31))
    b = Int32(42)

    x = ccall((:test_2a, libccalltest), Complex{Int32}, (Complex{Int32}, Int32), a, b)
    y = ccall((:test_2b, libccalltest), Complex{Int32}, (Complex{Int32},Int32), a, b)

    @test a == Complex{Int32}(Int32(10),Int32(31))

    @test x == y
    @test x == a + b*1 - b*2im
end

let a, b, x, y, z
    a = Complex{Int64}(Int64(20),Int64(51))
    b = Int64(42)

    x = ccall((:test_3a, libccalltest), Complex{Int64}, (Complex{Int64}, Int64), a, b)
    y = ccall((:test_3b, libccalltest), Complex{Int64}, (Complex{Int64}, Int64), a, b)
    z = ccall((:test_128, libccalltest), Complex{Int64}, (Complex{Int64}, Int64), a, b)

    @test a == Complex{Int64}(Int64(20),Int64(51))

    @test x == y
    @test x == a + b*1 - b*2im

    @test z == a + 1*b
end

mutable struct Struct4
    x::Int32
    y::Int32
    z::Int32
end
struct Struct4I
    x::Int32
    y::Int32
    z::Int32
end

function test_struct4(::Type{Struct}) where {Struct}
    a = Struct(-512275808,882558299,-2133022131)
    b = Int32(42)

    if Struct === Struct4
        x = ccall((:test_4, libccalltest), Struct4, (Struct4, Int32), a, b)
    else
        x = ccall((:test_4, libccalltest), Struct4I, (Struct4I, Int32), a, b)
    end

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
end
test_struct4(Struct4)
test_struct4(Struct4I)

mutable struct Struct5
    x::Int32
    y::Int32
    z::Int32
    a::Int32
end
struct Struct5I
    x::Int32
    y::Int32
    z::Int32
    a::Int32
end

function test_struct5(::Type{Struct}) where {Struct}
    a = Struct(1771319039, 406394736, -1269509787, -745020976)
    b = Int32(42)

    if Struct === Struct5
        x = ccall((:test_5, libccalltest), Struct5, (Struct5, Int32), a, b)
    else
        x = ccall((:test_5, libccalltest), Struct5I, (Struct5I, Int32), a, b)
    end

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
    @test x.a == a.a-b*4
end
test_struct5(Struct5)
test_struct5(Struct5I)

mutable struct Struct6
    x::Int64
    y::Int64
    z::Int64
end
struct Struct6I
    x::Int64
    y::Int64
    z::Int64
end

function test_struct6(::Type{Struct}) where {Struct}
    a = Struct(-654017936452753226, -5573248801240918230, -983717165097205098)
    b = Int64(42)

    if Struct === Struct6
        x = ccall((:test_6, libccalltest), Struct6, (Struct6, Int64), a, b)
    else
        x = ccall((:test_6, libccalltest), Struct6I, (Struct6I, Int64), a, b)
    end

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
end
test_struct6(Struct6)
test_struct6(Struct6I)

mutable struct Struct7
    x::Int64
    y::Cchar
end
struct Struct7I
    x::Int64
    y::Cchar
end

function test_struct7(::Type{Struct}) where {Struct}
    a = Struct(-384082741977533896, 'h')
    b = Int8(42)

    if Struct === Struct7
        x = ccall((:test_7, libccalltest), Struct7, (Struct7, Int8), a, b)
    else
        x = ccall((:test_7, libccalltest), Struct7I, (Struct7I, Int8), a, b)
    end

    @test x.x == a.x+Int(b)*1
    @test x.y == a.y-Int(b)*2
end
test_struct7(Struct7)
test_struct7(Struct7I)

mutable struct Struct8
    x::Int32
    y::Cchar
end
struct Struct8I
    x::Int32
    y::Cchar
end

function test_struct8(::Type{Struct}) where {Struct}
    a = Struct(-384082896, 'h')
    b = Int8(42)

    if Struct === Struct8
        r8 = ccall((:test_8, libccalltest), Struct8, (Struct8, Int8), a, b)
    else
        r8 = ccall((:test_8, libccalltest), Struct8I, (Struct8I, Int8), a, b)
    end

    @test r8.x == a.x+b*1
    @test r8.y == a.y-b*2
end
test_struct8(Struct8)
test_struct8(Struct8I)

mutable struct Struct9
    x::Int32
    y::Int16
end
struct Struct9I
    x::Int32
    y::Int16
end

function test_struct9(::Type{Struct}) where {Struct}
    a = Struct(-394092996, -3840)
    b = Int16(42)

    if Struct === Struct9
        x = ccall((:test_9, libccalltest), Struct9, (Struct9, Int16), a, b)
    else
        x = ccall((:test_9, libccalltest), Struct9I, (Struct9I, Int16), a, b)
    end

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
end
test_struct9(Struct9)
test_struct9(Struct9I)

mutable struct Struct10
    x::Cchar
    y::Cchar
    z::Cchar
    a::Cchar
end
struct Struct10I
    x::Cchar
    y::Cchar
    z::Cchar
    a::Cchar
end

function test_struct10(::Type{Struct}) where {Struct}
    a = Struct('0', '1', '2', '3')
    b = Int8(2)

    if Struct === Struct10
        x = ccall((:test_10, libccalltest), Struct10, (Struct10, Int8), a, b)
    else
        x = ccall((:test_10, libccalltest), Struct10I, (Struct10I, Int8), a, b)
    end

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
    @test x.a == a.a-b*4
end
test_struct10(Struct10)
test_struct10(Struct10I)

mutable struct Struct11
    x::Complex64
end
struct Struct11I
    x::Complex64
end

function test_struct11(::Type{Struct}) where {Struct}
    a = Struct(0.8877077f0 + 0.4591081f0im)
    b = Float32(42)

    if Struct === Struct11
        x = ccall((:test_11, libccalltest), Struct11, (Struct11, Float32), a, b)
    else
        x = ccall((:test_11, libccalltest), Struct11I, (Struct11I, Float32), a, b)
    end

    @test x.x ≈ a.x + b*1 - b*2im
end
test_struct11(Struct11)
test_struct11(Struct11I)

mutable struct Struct12
    x::Complex64
    y::Complex64
end
struct Struct12I
    x::Complex64
    y::Complex64
end

function test_struct12(::Type{Struct}) where {Struct}
    a = Struct(0.8877077f5 + 0.4591081f2im, 0.0004842868f0 - 6982.3265f3im)
    b = Float32(42)

    if Struct === Struct12
        x = ccall((:test_12, libccalltest), Struct12, (Struct12, Float32), a, b)
    else
        x = ccall((:test_12, libccalltest), Struct12I, (Struct12I, Float32), a, b)
    end

    @test x.x ≈ a.x + b*1 - b*2im
    @test x.y ≈ a.y + b*3 - b*4im
end
test_struct12(Struct12)
test_struct12(Struct12I)

mutable struct Struct13
    x::Complex128
end
struct Struct13I
    x::Complex128
end

function test_struct13(::Type{Struct}) where {Struct}
    a = Struct(42968.97560380495 - 803.0576845153616im)
    b = Float64(42)

    if Struct === Struct13
        x = ccall((:test_13, libccalltest), Struct13, (Struct13, Float64), a, b)
    else
        x = ccall((:test_13, libccalltest), Struct13I, (Struct13I, Float64), a, b)
    end

    @test x.x ≈ a.x + b*1 - b*2im
end
test_struct13(Struct13)
test_struct13(Struct13I)

mutable struct Struct14
    x::Float32
    y::Float32
end
struct Struct14I
    x::Float32
    y::Float32
end

function test_struct14(::Type{Struct}) where {Struct}
    a = Struct(0.024138331f0, 0.89759064f32)
    b = Float32(42)

    if Struct === Struct14
        x = ccall((:test_14, libccalltest), Struct14, (Struct14, Float32), a, b)
    else
        x = ccall((:test_14, libccalltest), Struct14I, (Struct14I, Float32), a, b)
    end

    @test x.x ≈ a.x + b*1
    @test x.y ≈ a.y - b*2
end
test_struct14(Struct14)
test_struct14(Struct14I)

mutable struct Struct15
    x::Float64
    y::Float64
end
struct Struct15I
    x::Float64
    y::Float64
end

function test_struct15(::Type{Struct}) where {Struct}
    a = Struct(4.180997967273657, -0.404218594294923)
    b = Float64(42)

    if Struct === Struct15
        x = ccall((:test_15, libccalltest), Struct15, (Struct15, Float64), a, b)
    else
        x = ccall((:test_15, libccalltest), Struct15I, (Struct15I, Float64), a, b)
    end

    @test x.x ≈ a.x + b*1
    @test x.y ≈ a.y - b*2
end
test_struct15(Struct15)
test_struct15(Struct15I)

mutable struct Struct16
    x::Float32
    y::Float32
    z::Float32
    a::Float64
    b::Float64
    c::Float64
end
struct Struct16I
    x::Float32
    y::Float32
    z::Float32
    a::Float64
    b::Float64
    c::Float64
end

function test_struct16(::Type{Struct}) where {Struct}
    a = Struct(0.1604656f0, 0.6297606f0, 0.83588994f0,
               0.6460273620993535, 0.9472692581106656, 0.47328535437352093)
    b = Float32(42)

    if Struct === Struct16
        x = ccall((:test_16, libccalltest), Struct16, (Struct16, Float32), a, b)
    else
        x = ccall((:test_16, libccalltest), Struct16I, (Struct16I, Float32), a, b)
    end

    @test x.x ≈ a.x + b*1
    @test x.y ≈ a.y - b*2
    @test x.z ≈ a.z + b*3
    @test x.a ≈ a.a - b*4
    @test x.b ≈ a.b + b*5
    @test x.c ≈ a.c - b*6
end
test_struct16(Struct16)
test_struct16(Struct16I)

mutable struct Struct17
    a::Int8
    b::Int16
end
struct Struct17I
    a::Int8
    b::Int16
end

function test_struct17(::Type{Struct}) where {Struct}
    a = Struct(2, 10)
    b = Int8(2)

    if Struct === Struct17
        x = ccall((:test_17, libccalltest), Struct17, (Struct17, Int8), a, b)
    else
        x = ccall((:test_17, libccalltest), Struct17I, (Struct17I, Int8), a, b)
    end

    @test x.a == a.a + b * 1
    @test x.b == a.b - b * 2
end
test_struct17(Struct17)
test_struct17(Struct17I)

mutable struct Struct18
    a::Int8
    b::Int8
    c::Int8
end
struct Struct18I
    a::Int8
    b::Int8
    c::Int8
end

function test_struct18(::Type{Struct}) where {Struct}
    a = Struct(2, 10, -3)
    b = Int8(2)

    if Struct === Struct18
        x = ccall((:test_18, libccalltest), Struct18, (Struct18, Int8), a, b)
    else
        x = ccall((:test_18, libccalltest), Struct18I, (Struct18I, Int8), a, b)
    end

    @test x.a == a.a + b * 1
    @test x.b == a.b - b * 2
    @test x.c == a.c + b * 3
end
test_struct18(Struct18)
test_struct18(Struct18I)

let a, b, x
    a = Int128(0x7f00123456789abc)<<64 + typemax(UInt64)
    b = Int64(1)

    x = ccall((:test_128, libccalltest), Int128, (Int128, Int64), a, b)

    @test x == a + b*1
    @test a == Int128(0x7f00123456789abc)<<64 + typemax(UInt64)
end

mutable struct Struct_Big
    x::Int
    y::Int
    z::Int8
end
struct Struct_BigI
    x::Int
    y::Int
    z::Int8
end
copy(a::Struct_Big) = Struct_Big(a.x, a.y, a.z)
copy(a::Struct_BigI) = a

function test_struct_big(::Type{Struct}) where {Struct}
    a = Struct(424,-5,Int8('Z'))
    a2 = copy(a)

    if Struct == Struct_Big
        x = ccall((:test_big, libccalltest), Struct_Big, (Struct_Big,), a2)
    else
        x = ccall((:test_big, libccalltest), Struct_BigI, (Struct_BigI,), a2)
    end

    @test a2.x == a.x && a2.y == a.y && a2.z == a.z
    @test x.x == a.x + 1
    @test x.y == a.y - 2
    @test x.z == a.z - Int('A')
end
test_struct_big(Struct_Big)
test_struct_big(Struct_BigI)

let a, a2, x
    a = Struct_Big(424,-5,Int8('Z'))
    a2 = copy(a)
    x = ccall((:test_big_long, libccalltest), Struct_Big, (Int, Int, Int, Struct_Big,), 2, 3, 4, a2)
    @test a2.x == a.x && a2.y == a.y && a2.z == a.z
    @test x.x == a.x + 10
    @test x.y == a.y - 2
    @test x.z == a.z - Int('A')
end

const Struct_huge1a = NTuple{8, Int64}
const Struct_huge1b = NTuple{9, Int64}
const Struct_huge2a = NTuple{8, Cdouble}
const Struct_huge2b = NTuple{9, Cdouble}
mutable struct Struct_huge3a
    cf::NTuple{3, Complex{Cfloat}}
    f7::Cfloat
    f8::Cfloat
end
mutable struct Struct_huge3b
    cf::NTuple{7, Complex{Cfloat}}
    r8a::Cfloat
    r8b::Cfloat
end
mutable struct Struct_huge3c
    cf::NTuple{7, Complex{Cfloat}}
    r8a::Cfloat
    r8b::Cfloat
    r9::Cfloat
end
mutable struct Struct_huge4a
    r12::Complex{Cdouble}
    r34::Complex{Cdouble}
    r5::Complex{Cfloat}
    r67::Complex{Cdouble}
    r8::Cdouble
end
mutable struct Struct_huge4b
    r12::Complex{Cdouble}
    r34::Complex{Cdouble}
    r5::Complex{Cfloat}
    r67::Complex{Cdouble}
    r89::Complex{Cdouble}
end
const Struct_huge5a = NTuple{8, Complex{Cint}}
const Struct_huge5b = NTuple{9, Complex{Cint}}

function verify_huge(init, a, b)
    @test typeof(init) === typeof(a) === typeof(b)
    verbose && @show (a, b)
    # make sure a was unmodified
    for i = 1:nfields(a)
        @test getfield(init, i) === getfield(a, i)
    end
    # make sure b was modifed as expected
    a1, b1 = getfield(a, 1), getfield(b, 1)
    while isa(a1, Tuple)
        @test a1[2:end] === b1[2:end]
        a1 = a1[1]
        b1 = b1[1]
    end
    if isa(a1, VecElement)
        a1 = a1.value
        b1 = b1.value
    end
    @test oftype(a1, a1 * 39) === b1
    for i = 2:nfields(a)
        @test getfield(a, i) === getfield(b, i)
    end
end
macro test_huge(i, b, init)
    f = QuoteNode(Symbol("test_huge", i, b))
    ty = Symbol("Struct_huge", i, b)
    return quote
        let a = $ty($(esc(init))...), f
            f(b) = ccall(($f, libccalltest), $ty, (Cchar, $ty, Cchar), '0' + $i, a, $b[1])
            #code_llvm(f, typeof((a,)))
            verify_huge($ty($(esc(init))...), a, f(a))
        end
    end
end
@test_huge 1 'a' ((1, 2, 3, 4, 5, 6, 7, 8),)
@test_huge 1 'b' ((1, 2, 3, 4, 5, 6, 7, 8, 9),)
@test_huge 2 'a' ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0),)
@test_huge 2 'b' ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0),)
@test_huge 3 'a' ((1.0 + 2.0im, 3.0 + 4.0im, 5.0 + 6.0im), 7.0, 8.0)
@test_huge 3 'b' ((1.0 + 2.0im, 3.0 + 4.0im, 5.0 + 6.0im, 7.0 + 8.0im, 9.0 + 10.0im, 11.0 + 12.0im, 13.0 + 14.0im), 7.0, 8.0)
@test_huge 3 'c' ((1.0 + 2.0im, 3.0 + 4.0im, 5.0 + 6.0im, 7.0 + 8.0im, 9.0 + 10.0im, 11.0 + 12.0im, 13.0 + 14.0im), 7.0, 8.0, 9.0)
@test_huge 4 'a' (1.0 + 2.0im, 3.0 + 4.0im, 5.0f0 + 6.0f0im, 7.0 + 8.0im, 9.0)
@test_huge 4 'b' (1.0 + 2.0im, 3.0 + 4.0im, 5.0f0 + 6.0f0im, 7.0 + 8.0im, 9.0 + 10.0im)
@test_huge 5 'a' ((1 + 2im, 3 + 4im, 5 + 6im, 7 + 8im, 9 + 10im, 11 + 12im, 13 + 14im, 15 + 16im),)
@test_huge 5 'b' ((1 + 2im, 3 + 4im, 5 + 6im, 7 + 8im, 9 + 10im, 11 + 12im, 13 + 14im, 15 + 16im, 17 + 17im),)

## cfunction roundtrip

verbose && Libc.flush_cstdio()
verbose && println("Testing cfunction roundtrip: ")

cf64 = 2.84+5.2im
cf32 = 3.34f0+53.2f0im
ci32 = Complex{Int32}(Int32(10),Int32(31))
ci64 = Complex{Int64}(Int64(20),Int64(51))
s1 = Struct1(352.39422f23, 19.287577)
==(a::Struct1,b::Struct1) = a.x == b.x && a.y == b.y

for (t,v) in ((Complex{Int32},:ci32),(Complex{Int64},:ci64),
              (Complex64,:cf32),(Complex128,:cf64),(Struct1,:s1))
    fname = Symbol("foo",v)
    fname1 = Symbol("foo1",v)
    @eval begin
        verbose && println($t)
        a = copy($v)
        verbose && println("A: ",a)
        function $fname1(s::$t)
            verbose && println("B: ",s)
            @test s == $v
            @test s === a
            global c = s
            s
        end
        function $fname1(s)
            @assert false
        end
        function $fname(s::$t)
            verbose && println("B: ",s)
            @test s == $v
            if($(t).mutable)
                @test !(s === a)
            end
            global c = s
            s
        end
        function $fname(s)
            @assert false
        end
        b = ccall(cfunction($fname1, Ref{$t}, Tuple{Ref{$t}}), Ref{$t}, (Ref{$t},), a)
        verbose && println("C: ",b)
        @test b == $v
        @test b === a
        @test b === c
        b = ccall(cfunction($fname, $t, Tuple{$t}), $t, ($t,), a)
        verbose && println("C: ",b)
        @test b == $v
        if ($(t).mutable)
            @test !(b === c)
            @test !(b === a)
        end
        b = ccall(cfunction($fname1, $t, Tuple{Ref{$t}}), $t, (Ref{$t},), a)
        verbose && println("C: ",b)
        @test b == $v
        if ($(t).mutable)
            @test !(b === c)
            @test !(b === a)
        end
        b = ccall(cfunction($fname, Ref{$t}, Tuple{$t}), Ref{$t}, ($t,), a)
        verbose && println("C: ",b)
        @test b == $v
        @test b === c
        if ($(t).mutable)
            @test !(b === a)
        end
        b = ccall(cfunction($fname, Any, Tuple{Ref{$t}}), Any, (Ref{$t},), $v)
        verbose && println("C: ",b)
        @test b == $v
        @test b === c
        if ($(t).mutable)
            @test !(b === a)
        end
        b = ccall(cfunction($fname, Any, Tuple{Ref{Any}}), Any, (Ref{Any},), $v)
        @test b == $v
        @test b === c
        if ($(t).mutable)
            @test !(b === a)
        end
        @test_throws TypeError ccall(cfunction($fname, Ref{AbstractString}, Tuple{Ref{Any}}), Any, (Ref{Any},), $v)
        @test_throws TypeError ccall(cfunction($fname, AbstractString, Tuple{Ref{Any}}), Any, (Ref{Any},), $v)
    end
end

# issue 13031
foo13031(x) = Cint(1)
foo13031p = cfunction(foo13031, Cint, Tuple{Ref{Tuple{}}})
ccall(foo13031p, Cint, (Ref{Tuple{}},), ())

foo13031(x,y,z) = z
foo13031p = cfunction(foo13031, Cint, Tuple{Ref{Tuple{}}, Ref{Tuple{}}, Cint})
ccall(foo13031p, Cint, (Ref{Tuple{}},Ref{Tuple{}},Cint), (), (), 8)

# issue 17219
function ccall_reassigned_ptr(ptr::Ptr{Void})
    ptr = Libdl.dlsym(Libdl.dlopen(libccalltest), "test_echo_p")
    ccall(ptr, Any, (Any,), "foo")
end
@test ccall_reassigned_ptr(C_NULL) == "foo"

# @threadcall functionality
threadcall_test_func(x) =
    @threadcall((:testUcharX, libccalltest), Int32, (UInt8,), x % UInt8)

@test threadcall_test_func(3) == 1
@test threadcall_test_func(259) == 1

# issue 17819
# NOTE: can't use cfunction or reuse ccalltest Struct methods, as those call into the runtime
@test @threadcall((:threadcall_args, libccalltest), Cint, (Cint, Cint), 1, 2) == 3

let n=3
    tids = Culong[]
    @sync for i in 1:10^n
        @async push!(tids, @threadcall(:uv_thread_self, Culong, ()))
    end

    # The work should not be done on the master thread
    t0 = ccall(:uv_thread_self, Culong, ())
    @test length(tids) == 10^n
    for t in tids
        @test t != t0
    end
end

@test ccall(:jl_getpagesize, Clong, ()) == @threadcall(:jl_getpagesize, Clong, ())

# Pointer finalizer (issue #15408)
let A = [1]
    ccall((:set_c_int, libccalltest), Void, (Cint,), 1)
    @test ccall((:get_c_int, libccalltest), Cint, ()) == 1
    finalizer(A, cglobal((:finalizer_cptr, libccalltest), Void))
    finalize(A)
    @test ccall((:get_c_int, libccalltest), Cint, ()) == -1
end

# Pointer finalizer at exit (PR #19911)
let result = read(`$(Base.julia_cmd()) --startup-file=no -e "A = Ref{Cint}(42); finalizer(A, cglobal((:c_exit_finalizer, \"$libccalltest\"), Void))"`, String)
    @test result == "c_exit_finalizer: 42, 0"
end

# SIMD Registers

const VecReg{N,T} = NTuple{N,VecElement{T}}
const V2xF32 = VecReg{2,Float32}
const V4xF32 = VecReg{4,Float32}
const V2xF64 = VecReg{2,Float64}
const V2xI32 = VecReg{2,Int32}
const V4xI32 = VecReg{4,Int32}

struct Struct_AA64_1
    v1::Int32
    v2::Int128
end
struct Struct_AA64_2
    v1::Float16
    v2::Float64
end

# This is a homogenious short vector aggregate
struct Struct_AA64_3
    v1::VecReg{8,Int8}
    v2::VecReg{2,Float32}
end
# This is NOT a homogenious short vector aggregate
struct Struct_AA64_4
    v2::VecReg{2,Float32}
    v1::VecReg{8,Int16}
end

mutable struct Struct_huge1_ppc64
    m::Int64
    v::V4xF32
end

mutable struct Struct_huge2_ppc64
    v1::V4xF32
    v2::V2xI32
end

mutable struct Struct_huge3_ppc64
    v1::V4xF32
    f::NTuple{4,Float32}
end

mutable struct Struct_huge4_ppc64
    v1::V2xF32
    v2::V2xF64
end

mutable struct Struct_huge5_ppc64
    v1::NTuple{9,V4xF32}
end

mutable struct Struct_huge6_ppc64
    v1::NTuple{8,V4xF32}
    v2::V4xF32
end

mutable struct Struct_huge7_ppc64
    v1::VecReg{3,Int32}
    v2::VecReg{3,Int32}
end

mutable struct Struct_huge1_ppc64_hva
    v1::NTuple{8,V4xF32}
end

mutable struct Struct_huge2_ppc64_hva
    v1::NTuple{2,NTuple{2,V4xF32}}
end

mutable struct Struct_huge3_ppc64_hva
    vf1::V4xF32
    vf2::Tuple{NTuple{2,V4xF32}}
end

mutable struct Struct_huge4_ppc64_hva
    v1::V4xI32
    v2::V4xF32
end

mutable struct Struct_huge5_ppc64_hva
    v1::V4xI32
    v2::V2xF64
end

if Sys.ARCH === :x86_64
    function test_sse(a1::V4xF32, a2::V4xF32, a3::V4xF32, a4::V4xF32)
        ccall((:test_m128, libccalltest), V4xF32, (V4xF32, V4xF32, V4xF32, V4xF32), a1, a2, a3, a4)
    end

    function test_sse(a1::V4xI32, a2::V4xI32, a3::V4xI32, a4::V4xI32)
        ccall((:test_m128i, libccalltest), V4xI32, (V4xI32, V4xI32, V4xI32, V4xI32), a1, a2, a3, a4)
    end

    foo_ams(a1, a2, a3, a4) = VecReg(ntuple(i -> VecElement(a1[i].value + a2[i].value * (a3[i].value - a4[i].value)), 4))

    for s in [Float32, Int32]
        T = NTuple{4, VecElement{s}}
        @eval function rt_sse(a1::$T, a2::$T, a3::$T, a4::$T)
            return ccall(
                cfunction(foo_ams, $T, Tuple{$T, $T, $T, $T}),
                $T,
                ($T, $T, $T, $T),
                a1,  a2,  a3, a4)
        end

        a1 = VecReg(ntuple(i -> VecElement(s(1i)), 4))
        a2 = VecReg(ntuple(i -> VecElement(s(2i)), 4))
        a3 = VecReg(ntuple(i -> VecElement(s(3i)), 4))
        a4 = VecReg(ntuple(i -> VecElement(s(4i)), 4))
        r = VecReg(ntuple(i -> VecElement(s(1i + 2i * (3i - 4i))), 4))
        @test test_sse(a1, a2, a3, a4) == r

        # cfunction round-trip
        @test rt_sse(a1, a2, a3, a4) == r
    end

elseif Sys.ARCH === :aarch64
    for v1 in 1:99:1000, v2 in -100:-1999:-20000
        @test ccall((:test_aa64_i128_1, libccalltest), Int128,
                    (Int64, Int128), v1, v2) == v1 * 2 - v2
    end
    for v1 in 1:4, v2 in -4:-1, v3_1 in 3:5, v3_2 in 7:9
        res = ccall((:test_aa64_i128_2, libccalltest), Struct_AA64_1,
                    (Int64, Int128, Struct_AA64_1),
                    v1, v2, Struct_AA64_1(v3_1, v3_2))
        expected = Struct_AA64_1(v1 ÷ 2 + 1 - v3_1, v2 * 2 - 1 - v3_2)
        @test res === expected
    end
    for v1 in 1:4, v2 in -4:-1, v3 in 3:5, v4 in -(1:3)
        res = ccall((:test_aa64_fp16_1, libccalltest), Float16,
                    (Cint, Float32, Float64, Float16),
                    v1, v2, v3, v4)
        expected = Float16(v1 + v2 * 2 + v3 * 3 + v4 * 4)
        @test res === expected

        res = ccall((:test_aa64_fp16_2, libccalltest), Struct_AA64_2,
                    (Cint, Float32, Float64, Float16),
                    v1, v2, v3, v4)
        expected = Struct_AA64_2(v4 / 2 + 1, v1 * 2 + v2 * 4 - v3)
        @test res === expected
    end
    for v1_1 in 1:4, v1_2 in -2:2, v2 in -4:-1, v3_1 in 3:5, v3_2 in 6:8
        res = ccall((:test_aa64_vec_1, libccalltest),
                    VecReg{2,Int64},
                    (VecReg{2,Int32}, Float32, VecReg{2,Int32}),
                    (VecElement(Int32(v1_1)), VecElement(Int32(v1_2))),
                    v2, (VecElement(Int32(v3_1)), VecElement(Int32(v3_2))))
        expected = (VecElement(v1_1 * v2 + v3_1), VecElement(v1_2 * v2 + v3_2))
        @test res === expected
    end
    for v1_11 in 1:4, v1_12 in -2:2, v1_21 in 1:4, v1_22 in -2:2,
        v2_11 in 1:4, v2_12 in -2:2, v2_21 in 1:4, v2_22 in -2:2
        v1 = Struct_AA64_3((VecElement(Int8(v1_11)), VecElement(Int8(v1_12)),
                            VecElement(Int8(0)), VecElement(Int8(0)),
                            VecElement(Int8(0)), VecElement(Int8(0)),
                            VecElement(Int8(0)), VecElement(Int8(0))),
                           (VecElement(Float32(v1_21)),
                            VecElement(Float32(v1_22))))
        v2 = Struct_AA64_4((VecElement(Float32(v2_21)),
                            VecElement(Float32(v2_22))),
                           (VecElement(Int16(v2_11)), VecElement(Int16(v2_12)),
                            VecElement(Int16(0)), VecElement(Int16(0)),
                            VecElement(Int16(0)), VecElement(Int16(0)),
                            VecElement(Int16(0)), VecElement(Int16(0))))
        res = ccall((:test_aa64_vec_2, libccalltest),
                    Struct_AA64_3, (Struct_AA64_3, Struct_AA64_4), v1, v2)
        expected = Struct_AA64_3((VecElement(Int8(v1_11 + v2_11)),
                                  VecElement(Int8(v1_12 + v2_12)),
                                  VecElement(Int8(0)), VecElement(Int8(0)),
                                  VecElement(Int8(0)), VecElement(Int8(0)),
                                  VecElement(Int8(0)), VecElement(Int8(0))),
                                 (VecElement(Float32(v1_21 - v2_21)),
                                  VecElement(Float32(v1_22 - v2_22))))
        @test res === expected
    end

elseif Sys.ARCH === :powerpc64le || Sys.ARCH === :ppc64le
@test_huge 1 "_ppc64" (1, (2.0, 3.0, 4.0, 5.0),)
@test_huge 2 "_ppc64" ((1.0, 2.0, 3.0, 4.0), (11, 12))
@test_huge 3 "_ppc64" ((1, 2, 3, 4), (11.0, 12.0, 13.0, 14.0))
@test_huge 4 "_ppc64" ((1, 2), (11.0, 12.0))
@test_huge 5 "_ppc64" ((((1.0, 2.0, 3.0, 4.0),
                         (5.0, 6.0, 7.0, 8.0),
                         (11.0, 12.0, 13.0, 14.0),
                         (15.0, 16.0, 17.0, 18.0),
                         (21.0, 22.0, 23.0, 24.0),
                         (25.0, 26.0, 27.0, 28.0),
                         (31.0, 32.0, 33.0, 34.0),
                         (35.0, 36.0, 37.0, 38.0),
                         (41.0, 42.0, 43.0, 44.0)),))
@test_huge 6 "_ppc64" ((((1.0, 2.0, 3.0, 4.0),
                         (5.0, 6.0, 7.0, 8.0),
                         (11.0, 12.0, 13.0, 14.0),
                         (15.0, 16.0, 17.0, 18.0),
                         (21.0, 22.0, 23.0, 24.0),
                         (25.0, 26.0, 27.0, 28.0),
                         (31.0, 32.0, 33.0, 34.0),
                         (35.0, 36.0, 37.0, 38.0)),
                        (41.0, 42.0, 43.0, 44.0)))
@test_huge 1 "_ppc64_hva" ((((1.0, 2.0, 3.0, 4.0),
                             (5.0, 6.0, 7.0, 8.0),
                             (11.0, 12.0, 13.0, 14.0),
                             (15.0, 16.0, 17.0, 18.0),
                             (21.0, 22.0, 23.0, 24.0),
                             (25.0, 26.0, 27.0, 28.0),
                             (31.0, 32.0, 33.0, 34.0),
                             (35.0, 36.0, 37.0, 38.0)),))
@test_huge 2 "_ppc64_hva" (((((1.0, 2.0, 3.0, 4.0),
                              (5.0, 6.0, 7.0, 8.0)),
                             ((11.0, 12.0, 13.0, 14.0),
                              (15.0, 16.0, 17.0, 18.0))),))
@test_huge 3 "_ppc64_hva" (((1.0, 2.0, 3.0, 4.0),
                            (((11.0, 12.0, 13.0, 14.0),
                              (15.0, 16.0, 17.0, 18.0)),)))
@test_huge 4 "_ppc64_hva" (((1, 2, 3, 4),
                            (11.0, 12.0, 13.0, 14.0)))
@test_huge 5 "_ppc64_hva" (((1, 2, 3, 4),
                            (11.0, 12.0)))

@test 18451 == ccall((:test_ppc64_vec1long, libccalltest), Int64,
    (Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Struct_huge1_ppc64),
    1, 2, 3, 4, 5, 6, 7, 8, 9, Struct_huge1_ppc64(18000, (100, 101, 102, 103)))

@test 941 == ccall((:test_ppc64_vec1long_vec, libccalltest), Int64,
    (Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, V4xF32),
    11, 12, 13, 14, 15, 16, 17, 18, 19, (200, 201, 202, 203))

@test V4xF32((614232, 614218, 614204, 614190)) ==
     ccall((:test_ppc64_vec2, libccalltest), V4xF32,
    (Int64, V4xF32, V4xF32, V4xF32, V4xF32,
     V4xF32, V4xF32, V4xF32, V4xF32, V4xF32,
     V4xF32, V4xF32, V4xF32, V4xF32, V4xF32),
    600000, (4, 3, 2, 1), (5, 4, 3, 2), (6, 5, 4, 3), (7, 6, 5, 4),
    (14, 13, 12, 11), (15, 14, 13, 12), (16, 15, 14, 13), (17, 16, 15, 14), (18, 17, 16, 15),
    (1024, 1023, 1022, 1021), (1025, 1024, 1023, 1022), (1026, 1025, 1024, 1023), (1027, 1026, 1025, 1024), (10028, 10027, 10026, 10025))

elseif Sys.ARCH !== :i686 && Sys.ARCH !== :arm # TODO
warn("ccall: no VecReg tests run for this platform")

end

# Special calling convention for `Array`
function f17204(a)
    b = similar(a)
    for i in eachindex(a)
        b[i] = a[i] + 10
    end
    return b
end
@test ccall(cfunction(f17204, Vector{Any}, Tuple{Vector{Any}}),
            Vector{Any}, (Vector{Any},), Any[1:10;]) == Any[11:20;]

# This used to trigger incorrect ccall callee inlining.
# Not sure if there's a more reliable way to test this.
# Do not put these in a function.
@noinline g17413() = rand()
@inline f17413() = (g17413(); g17413())
ccall((:test_echo_p, libccalltest), Ptr{Void}, (Any,), f17413())
for i in 1:3
    ccall((:test_echo_p, libccalltest), Ptr{Void}, (Any,), f17413())
end

struct SpillPint
    a::Ptr{Cint}
    b::Ptr{Cint}
end
Base.cconvert(::Type{SpillPint}, v::NTuple{2,Cint}) =
    Base.cconvert(Ref{NTuple{2,Cint}}, v)
function Base.unsafe_convert(::Type{SpillPint}, vr)
    ptr = Base.unsafe_convert(Ref{NTuple{2,Cint}}, vr)
    return SpillPint(ptr, ptr + 4)
end

macro test_spill_n(n::Int, intargs, floatargs)
    fname_int = Symbol(:test_spill_int, n)
    fname_float = Symbol(:test_spill_float, n)
    quote
        local ints = $(esc(intargs))
        local floats = $(esc(intargs))
        @test ccall(($(QuoteNode(fname_int)), libccalltest), Cint,
                    ($((:(Ref{Cint}) for j in 1:n)...), SpillPint),
                    $((:(ints[$j]) for j in 1:n)...),
                    (ints[$n + 1], ints[$n + 2])) == sum(ints[1:($n + 2)])
        @test ccall(($(QuoteNode(fname_float)), libccalltest), Float32,
                    ($((:Float32 for j in 1:n)...), NTuple{2,Float32}),
                    $((:(floats[$j]) for j in 1:n)...),
                    (floats[$n + 1], floats[$n + 2])) == sum(floats[1:($n + 2)])
    end
end

for i in 1:100
    local intargs = rand(1:10000, 14)
    local int32args = Int32.(intargs)
    local intsum = sum(intargs)
    local floatargs = rand(14)
    local float32args = Float32.(floatargs)
    local float32sum = sum(float32args)
    local float64sum = sum(floatargs)
    @test ccall((:test_long_args_intp, libccalltest), Cint,
                (Ref{Cint}, Ref{Cint}, Ref{Cint}, Ref{Cint},
                 Ref{Cint}, Ref{Cint}, Ref{Cint}, Ref{Cint},
                 Ref{Cint}, Ref{Cint}, Ref{Cint}, Ref{Cint},
                 Ref{Cint}, Ref{Cint}),
                intargs[1], intargs[2], intargs[3], intargs[4],
                intargs[5], intargs[6], intargs[7], intargs[8],
                intargs[9], intargs[10], intargs[11], intargs[12],
                intargs[13], intargs[14]) == intsum
    @test ccall((:test_long_args_int, libccalltest), Cint,
                (Cint, Cint, Cint, Cint, Cint, Cint, Cint, Cint,
                 Cint, Cint, Cint, Cint, Cint, Cint),
                intargs[1], intargs[2], intargs[3], intargs[4],
                intargs[5], intargs[6], intargs[7], intargs[8],
                intargs[9], intargs[10], intargs[11], intargs[12],
                intargs[13], intargs[14]) == intsum
    @test ccall((:test_long_args_float, libccalltest), Float32,
                (Float32, Float32, Float32, Float32, Float32, Float32,
                 Float32, Float32, Float32, Float32, Float32, Float32,
                 Float32, Float32),
                floatargs[1], floatargs[2], floatargs[3], floatargs[4],
                floatargs[5], floatargs[6], floatargs[7], floatargs[8],
                floatargs[9], floatargs[10], floatargs[11], floatargs[12],
                floatargs[13], floatargs[14]) ≈ float32sum
    @test ccall((:test_long_args_double, libccalltest), Float64,
                (Float64, Float64, Float64, Float64, Float64, Float64,
                 Float64, Float64, Float64, Float64, Float64, Float64,
                 Float64, Float64),
                floatargs[1], floatargs[2], floatargs[3], floatargs[4],
                floatargs[5], floatargs[6], floatargs[7], floatargs[8],
                floatargs[9], floatargs[10], floatargs[11], floatargs[12],
                floatargs[13], floatargs[14]) ≈ float64sum

    @test_spill_n 1 int32args float32args
    @test_spill_n 2 int32args float32args
    @test_spill_n 3 int32args float32args
    @test_spill_n 4 int32args float32args
    @test_spill_n 5 int32args float32args
    @test_spill_n 6 int32args float32args
    @test_spill_n 7 int32args float32args
    @test_spill_n 8 int32args float32args
    @test_spill_n 9 int32args float32args
    @test_spill_n 10 int32args float32args
end

# issue #20835
@test_throws(ErrorException("could not evaluate ccall argument type (it might depend on a local variable)"),
             eval(:(f20835(x) = ccall(:fn, Void, (Ptr{typeof(x)},), x))))
@test_throws(UndefVarError(:Something_not_defined_20835),
             eval(:(f20835(x) = ccall(:fn, Something_not_defined_20835, (Ptr{typeof(x)},), x))))

@noinline f21104at(::Type{T}) where {T} = ccall(:fn, Void, (Nullable{T},), 0)
@noinline f21104rt(::Type{T}) where {T} = ccall(:fn, Nullable{T}, ())
@test code_llvm(DevNull, f21104at, (Type{Float64},)) === nothing
@test code_llvm(DevNull, f21104rt, (Type{Float64},)) === nothing
@test_throws(ErrorException("ccall: the type of argument 1 doesn't correspond to a C type"),
             f21104at(Float64))
@test_throws(ErrorException("ccall: return type doesn't correspond to a C type"),
             f21104rt(Float64))

# test for malformed syntax errors
@test Expr(:error, "more arguments than types for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (), x)))
@test Expr(:error, "more arguments than types for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B,), x, y)))
@test Expr(:error, "more arguments than types for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B,), x, y, z)))
@test Expr(:error, "more arguments than types for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B,), x, y)))
@test Expr(:error, "more arguments than types for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B,), x, y, z)))
@test Expr(:error, "more arguments than types for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B, C), x, y, z)))
@test Expr(:error, "more types than arguments for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B,),)))
@test Expr(:error, "more types than arguments for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B, C), )))
@test Expr(:error, "more types than arguments for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B..., C...), )))
@test Expr(:error, "only the trailing ccall argument type should have '...'") == expand(@__MODULE__, :(ccall(:fn, A, (B..., C...), x)))
@test Expr(:error, "only the trailing ccall argument type should have '...'") == expand(@__MODULE__, :(ccall(:fn, A, (B..., C...), x, y, z)))
@test Expr(:error, "more types than arguments for ccall") == expand(@__MODULE__, :(ccall(:fn, A, (B, C...), )))

# cfunction on non-function singleton
struct CallableSingleton
end
(::CallableSingleton)(x, y) = x + y
@test ccall(cfunction(CallableSingleton(), Int, Tuple{Int,Int}),
            Int, (Int, Int), 1, 2) === 3

# 19805
mutable struct callinfos_19805{FUNC_FT<:Function}
    f :: FUNC_FT
end

evalf_callback_19805(ci::callinfos_19805{FUNC_FT}) where {FUNC_FT} = ci.f(0.5)::Float64

evalf_callback_c_19805(ci::callinfos_19805{FUNC_FT}) where {FUNC_FT} = cfunction(
    evalf_callback_19805, Float64, Tuple{callinfos_19805{FUNC_FT}})

@test_throws(ErrorException("ccall: the type of argument 1 doesn't correspond to a C type"),
             evalf_callback_c_19805( callinfos_19805(sin) ))

# test Ref{abstract_type} calling parameter passes a heap box
abstract type Abstract22734 end
struct Bits22734 <: Abstract22734
    x::Int
    y::Float64
end
function cb22734(ptr::Ptr{Void})
    gc()
    obj = unsafe_pointer_to_objref(ptr)::Bits22734
    obj.x + obj.y
end
ptr22734 = cfunction(cb22734, Float64, Tuple{Ptr{Void}})
function caller22734(ptr)
    obj = Bits22734(12, 20)
    ccall(ptr, Float64, (Ref{Abstract22734},), obj)
end
@test caller22734(ptr22734) === 32.0
