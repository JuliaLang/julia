# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.copy, Base.==

const libccalltest = "libccalltest"

const verbose = false
ccall((:set_verbose, libccalltest), Void, (Int32,), verbose)


# Test for proper argument register truncation
ccall_test_func(x) = ccall((:testUcharX, libccalltest), Int32, (UInt8,), x % UInt8)
@test ccall_test_func(3) == 1
@test ccall_test_func(259) == 1


# Test for proper round-trip of Ref{T} type
ccall_echo_func{T,U}(x, ::Type{T}, ::Type{U}) = ccall((:test_echo_p, libccalltest), T, (U,), x)
# Make sure object x is still valid (rooted as argument)
# when loading the pointer. This works as long as we still keep the argument
# rooted but might fail if we are smarter about eliminating dead root.
@noinline ccall_echo_load{T,U}(x, ::Type{T}, ::Type{U}) =
    unsafe_load(ccall_echo_func(x, T, U))
@noinline ccall_echo_objref{T,U}(x, ::Type{T}, ::Type{U}) =
    unsafe_pointer_to_objref(ccall_echo_func(x, Ptr{T}, U))
type IntLike
    x::Int
end
@test ccall_echo_load(132, Ptr{Int}, Ref{Int}) === 132
@test ccall_echo_load(Ref(921), Ptr{Int}, Ref{Int}) === 921
@test ccall_echo_load(IntLike(993), Ptr{Int}, Ref{IntLike}) === 993
@test ccall_echo_load(IntLike(881), Ptr{IntLike}, Ref{IntLike}).x === 881
@test ccall_echo_func(532, Int, Int) === 532
if Sys.WORD_SIZE == 64
    # this test is valid only for x86_64 and win64
    @test ccall_echo_func(164, IntLike, Int).x === 164
end
@test ccall_echo_func(IntLike(828), Int, IntLike) === 828
@test ccall_echo_func(913, Any, Any) === 913
@test ccall_echo_objref(553, Ptr{Any}, Any) === 553
@test ccall_echo_func(124, Ref{Int}, Any) === 124
@test ccall_echo_load(422, Ptr{Any}, Ref{Any}) === 422
@test ccall_echo_load([383], Ptr{Int}, Ref{Int}) === 383
@test ccall_echo_load(Ref([144,172],2), Ptr{Int}, Ref{Int}) === 172
# @test ccall_echo_load(Ref([8],1,1), Ptr{Int}, Ref{Int}) === 8


## Tests for passing and returning structs

let
    a = 20 + 51im

    x = ccall((:ctest, libccalltest), Complex{Int}, (Complex{Int},), a)

    @test x == a + 1 - 2im

    ci_ary = [a] # Make sure the array is alive during unsafe_load
    x = unsafe_load(ccall((:cptest, libccalltest), Ptr{Complex{Int}},
                          (Ptr{Complex{Int}},), ci_ary))

    @test x == a + 1 - 2im
    @test a == 20 + 51im

    x = ccall((:cptest_static, libccalltest), Ptr{Complex{Int}}, (Ptr{Complex{Int}},), &a)
    @test unsafe_load(x) == a
    Libc.free(convert(Ptr{Void},x))
end

let
    a = 2.84 + 5.2im

    x = ccall((:cgtest, libccalltest), Complex128, (Complex128,), a)

    @test x == a + 1 - 2im

    b = [a] # Make sure the array is alive during unsafe_load
    x = unsafe_load(ccall((:cgptest, libccalltest), Ptr{Complex128}, (Ptr{Complex128},), b))

    @test x == a + 1 - 2im
    @test a == 2.84 + 5.2im
end

let
    a = 3.34f0 + 53.2f0im

    x = ccall((:cftest, libccalltest), Complex64, (Complex64,), a)

    @test x == a + 1 - 2im

    b = [a] # Make sure the array is alive during unsafe_load
    x = unsafe_load(ccall((:cfptest, libccalltest), Ptr{Complex64}, (Ptr{Complex64},), b))

    @test x == a + 1 - 2im
    @test a == 3.34f0 + 53.2f0im
end


## Tests for native Julia data types

let
    a = 2.84 + 5.2im

    @test_throws MethodError ccall((:cptest, libccalltest), Ptr{Complex{Int}}, (Ptr{Complex{Int}},), a)
    @test_throws MethodError ccall((:cptest, libccalltest), Ptr{Complex{Int}}, (Complex{Int},), &a)
end


## Tests for various sized data types (ByVal)

type Struct1
    x::Float32
    y::Float64
end
copy(a::Struct1) = Struct1(a.x, a.y)

let
    a = Struct1(352.39422f23, 19.287577)
    b = Float32(123.456)

    a2 = copy(a)
    x = ccall((:test_1, libccalltest), Struct1, (Struct1, Float32), a2, b)

    @test a2.x == a.x && a2.y == a.y
    @test !(a2 === x)

    @test x.x ≈ a.x + 1*b
    @test x.y ≈ a.y - 2*b
end

let
    a = Complex{Int32}(Int32(10),Int32(31))
    b = Int32(42)

    x = ccall((:test_2a, libccalltest), Complex{Int32}, (Complex{Int32}, Int32), a, b)
    y = ccall((:test_2b, libccalltest), Complex{Int32}, (Complex{Int32},Int32), a, b)

    @test a == Complex{Int32}(Int32(10),Int32(31))

    @test x == y
    @test x == a + b*1 - b*2im
end

let
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

type Struct4
    x::Int32
    y::Int32
    z::Int32
end

let
    a = Struct4(-512275808,882558299,-2133022131)
    b = Int32(42)

    x = ccall((:test_4, libccalltest), Struct4, (Struct4,Int32), a, b)

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
end

type Struct5
    x::Int32
    y::Int32
    z::Int32
    a::Int32
end

let
    a = Struct5(1771319039, 406394736, -1269509787, -745020976)
    b = Int32(42)

    x = ccall((:test_5, libccalltest), Struct5, (Struct5,Int32), a, b)

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
    @test x.a == a.a-b*4
end

type Struct6
    x::Int64
    y::Int64
    z::Int64
end

let
    a = Struct6(-654017936452753226, -5573248801240918230, -983717165097205098)
    b = Int64(42)

    x = ccall((:test_6, libccalltest), Struct6, (Struct6, Int64), a, b)

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
end

type Struct7
    x::Int64
    y::Cchar
end

let
    a = Struct7(-384082741977533896, 'h')
    b = Int8(42)

    x = ccall((:test_7, libccalltest), Struct7, (Struct7,Int8), a, b)

    @test x.x == a.x+Int(b)*1
    @test x.y == a.y-Int(b)*2
end

type Struct8
    x::Int32
    y::Cchar
end

let
    a = Struct8(-384082896, 'h')
    b = Int8(42)

    r8 = ccall((:test_8, libccalltest), Struct8, (Struct8,Int8), a, b)

    @test r8.x == a.x+b*1
    @test r8.y == a.y-b*2
end

type Struct9
    x::Int32
    y::Int16
end

let
    a = Struct9(-394092996, -3840)
    b = Int16(42)

    x = ccall((:test_9, libccalltest), Struct9, (Struct9,Int16), a, b)

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
end

type Struct10
    x::Cchar
    y::Cchar
    z::Cchar
    a::Cchar
end

let
    a = Struct10('0', '1', '2', '3')
    b = Int8(2)

    x = ccall((:test_10, libccalltest), Struct10, (Struct10,Int8), a, b)

    @test x.x == a.x+b*1
    @test x.y == a.y-b*2
    @test x.z == a.z+b*3
    @test x.a == a.a-b*4
end

type Struct11
    x::Complex64
end

let
    a = Struct11(0.8877077f0 + 0.4591081f0im)
    b = Float32(42)

    x = ccall((:test_11, libccalltest), Struct11, (Struct11,Float32), a, b)

    @test x.x ≈ a.x + b*1 - b*2im
end

type Struct12
    x::Complex64
    y::Complex64
end

let
    a = Struct12(0.8877077f5 + 0.4591081f2im, 0.0004842868f0 - 6982.3265f3im)
    b = Float32(42)

    x = ccall((:test_12, libccalltest), Struct12, (Struct12,Float32), a, b)

    @test x.x ≈ a.x + b*1 - b*2im
    @test x.y ≈ a.y + b*3 - b*4im
end

type Struct13
    x::Complex128
end

let
    a = Struct13(42968.97560380495 - 803.0576845153616im)
    b = Float64(42)

    x = ccall((:test_13, libccalltest), Struct13, (Struct13,Float64), a, b)

    @test x.x ≈ a.x + b*1 - b*2im
end

type Struct14
    x::Float32
    y::Float32
end

let
    a = Struct14(0.024138331f0, 0.89759064f32)
    b = Float32(42)

    x = ccall((:test_14, libccalltest), Struct14, (Struct14,Float32), a, b)

    @test x.x ≈ a.x + b*1
    @test x.y ≈ a.y - b*2
end

type Struct15
    x::Float64
    y::Float64
end

let
    a = Struct15(4.180997967273657, -0.404218594294923)
    b = Float64(42)

    x = ccall((:test_15, libccalltest), Struct15, (Struct15,Float64), a, b)

    @test x.x ≈ a.x + b*1
    @test x.y ≈ a.y - b*2
end

type Struct16
    x::Float32
    y::Float32
    z::Float32
    a::Float64
    b::Float64
    c::Float64
end

let
    a = Struct16(0.1604656f0, 0.6297606f0, 0.83588994f0,
                 0.6460273620993535, 0.9472692581106656, 0.47328535437352093)
    b = Float32(42)

    x = ccall((:test_16, libccalltest), Struct16, (Struct16,Float32), a, b)

    @test x.x ≈ a.x + b*1
    @test x.y ≈ a.y - b*2
    @test x.z ≈ a.z + b*3
    @test x.a ≈ a.a - b*4
    @test x.b ≈ a.b + b*5
    @test x.c ≈ a.c - b*6
end

let
    a = Int128(0x7f00123456789abc)<<64 + typemax(UInt64)
    b = Int64(1)

    x = ccall((:test_128, libccalltest), Int128, (Int128, Int64), a, b)

    @test x == a + b*1
    @test a == Int128(0x7f00123456789abc)<<64 + typemax(UInt64)
end

type Struct_Big
    x::Int
    y::Int
    z::Int8
end
copy(a::Struct_Big) = Struct_Big(a.x, a.y, a.z)

let
    a = Struct_Big(424,-5,Int8('Z'))
    a2 = copy(a)

    x = ccall((:test_big, libccalltest), Struct_Big, (Struct_Big,), a2)

    @test a2.x == a.x && a2.y == a.y && a2.z == a.z
    @test x.x == a.x + 1
    @test x.y == a.y - 2
    @test x.z == a.z - Int('A')
end

const Struct_huge1a = NTuple{8, Int64}
const Struct_huge1b = NTuple{9, Int64}
const Struct_huge2a = NTuple{8, Cdouble}
const Struct_huge2b = NTuple{9, Cdouble}
type Struct_huge3a
    cf::NTuple{3, Complex{Cfloat}}
    f7::Cfloat
    f8::Cfloat
end
type Struct_huge3b
    cf::NTuple{7, Complex{Cfloat}}
    r8a::Cfloat
    r8b::Cfloat
end
type Struct_huge3c
    cf::NTuple{7, Complex{Cfloat}}
    r8a::Cfloat
    r8b::Cfloat
    r9::Cfloat
end
type Struct_huge4a
    r12::Complex{Cdouble}
    r34::Complex{Cdouble}
    r5::Complex{Cfloat}
    r67::Complex{Cdouble}
    r8::Cdouble
end
type Struct_huge4b
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
    if isa(a1, Tuple)
        @test oftype(a1[1], a1[1] * 39) === b1[1]
        @test a1[2:end] === b1[2:end]
    else
        @test oftype(a1, a1 * 39) === b1
    end
    for i = 2:nfields(a)
        @test getfield(a, i) === getfield(b, i)
    end
end
macro test_huge(i, b, init)
    f = QuoteNode(Symbol("test_huge", i, b))
    ty = Symbol("Struct_huge", i, b)
    return quote
        let a = $ty($(esc(init))...), f
            f(b) = ccall(($f, libccalltest), $ty, (Cchar, $ty, Cchar), '0' + $i, a, $b)
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
        b = ccall(cfunction($fname1,Ref{$t},(Ref{$t},)),Ref{$t},(Ref{$t},),a)
        verbose && println("C: ",b)
        @test b == $v
        @test b === a
        @test b === c
        b = ccall(cfunction($fname,$t,($t,)),$t,($t,),a)
        verbose && println("C: ",b)
        @test b == $v
        if ($(t).mutable)
            @test !(b === c)
            @test !(b === a)
        end
        b = ccall(cfunction($fname1,$t,(Ref{$t},)),$t,(Ref{$t},),a)
        verbose && println("C: ",b)
        @test b == $v
        if ($(t).mutable)
            @test !(b === c)
            @test !(b === a)
        end
        b = ccall(cfunction($fname,Ref{$t},($t,)),Ref{$t},($t,),a)
        verbose && println("C: ",b)
        @test b == $v
        @test b === c
        if ($(t).mutable)
            @test !(b === a)
        end
        b = ccall(cfunction($fname,Any,(Ref{$t},)),Any,(Ref{$t},),$v)
        verbose && println("C: ",b)
        @test b == $v
        @test b === c
        if ($(t).mutable)
            @test !(b === a)
        end
        b = ccall(cfunction($fname,Any,(Ref{Any},)),Any,(Ref{Any},),$v)
        @test b == $v
        @test b === c
        if ($(t).mutable)
            @test !(b === a)
        end
        @test_throws TypeError ccall(cfunction($fname,Ref{AbstractString},(Ref{Any},)),Any,(Ref{Any},),$v)
        @test_throws TypeError ccall(cfunction($fname,AbstractString,(Ref{Any},)),Any,(Ref{Any},),$v)
    end
end

# issue 13031
foo13031(x) = Cint(1)
foo13031p = cfunction(foo13031, Cint, (Ref{Tuple{}},))
ccall(foo13031p, Cint, (Ref{Tuple{}},), ())

foo13031(x,y,z) = z
foo13031p = cfunction(foo13031, Cint, (Ref{Tuple{}},Ref{Tuple{}},Cint))
ccall(foo13031p, Cint, (Ref{Tuple{}},Ref{Tuple{}},Cint), (), (), 8)

# @threadcall functionality
threadcall_test_func(x) =
    @threadcall((:testUcharX, libccalltest), Int32, (UInt8,), x % UInt8)

@test threadcall_test_func(3) == 1
@test threadcall_test_func(259) == 1

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

# SIMD Registers

typealias VecReg{N,T} NTuple{N,VecElement{T}}
typealias V4xF32 VecReg{4,Float32}
typealias V4xI32 VecReg{4,Int32}

immutable Struct_AA64_1
    v1::Int32
    v2::Int128
end
immutable Struct_AA64_2
    v1::Float16
    v2::Float64
end

# This is a homogenious short vector aggregate
immutable Struct_AA64_3
    v1::VecReg{8,Int8}
    v2::VecReg{2,Float32}
end
# This is NOT a homogenious short vector aggregate
immutable Struct_AA64_4
    v2::VecReg{2,Float32}
    v1::VecReg{8,Int16}
end

if Sys.ARCH === :x86_64

    function test_sse(a1::V4xF32,a2::V4xF32,a3::V4xF32,a4::V4xF32)
        ccall((:test_m128, libccalltest), V4xF32, (V4xF32,V4xF32,V4xF32,V4xF32), a1, a2, a3, a4)
    end

    function test_sse(a1::V4xI32,a2::V4xI32,a3::V4xI32,a4::V4xI32)
        ccall((:test_m128i, libccalltest), V4xI32, (V4xI32,V4xI32,V4xI32,V4xI32), a1, a2, a3, a4)
    end

    foo_ams(a1, a2, a3, a4) = VecReg(ntuple(i->VecElement(a1[i].value+a2[i].value*(a3[i].value-a4[i].value)),4))

    rt_sse{T}(a1::T,a2::T,a3::T,a4::T) = ccall(cfunction(foo_ams,T,(T,T,T,T)), T, (T,T,T,T), a1, a2, a3,a4)

    for s in [Float32,Int32]
        a1 = VecReg(ntuple(i->VecElement(s(1i)),4))
        a2 = VecReg(ntuple(i->VecElement(s(2i)),4))
        a3 = VecReg(ntuple(i->VecElement(s(3i)),4))
        a4 = VecReg(ntuple(i->VecElement(s(4i)),4))
        r = VecReg(ntuple(i->VecElement(s(1i+2i*(3i-4i))),4))
        @test test_sse(a1,a2,a3,a4) == r

        # cfunction round-trip
        @test rt_sse(a1,a2,a3,a4) == r
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
end
