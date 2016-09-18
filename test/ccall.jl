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
ccall_echo_load{T,U}(x, ::Type{T}, ::Type{U}) =
    unsafe_load(ccall_echo_func(x, T, U))
ccall_echo_objref{T,U}(x, ::Type{T}, ::Type{U}) =
    unsafe_pointer_to_objref(ccall_echo_func(x, Ptr{T}, U))
type IntLike
    x::Int
end
@test ccall_echo_load(132, Ptr{Int}, Ref{Int}) === 132
@test ccall_echo_load(Ref(921), Ptr{Int}, Ref{Int}) === 921
@test ccall_echo_load(IntLike(993), Ptr{Int}, Ref{IntLike}) === 993
@test ccall_echo_load(IntLike(881), Ptr{IntLike}, Ref{IntLike}).x === 881
@test ccall_echo_func(532, Int, Int) === 532
if WORD_SIZE == 64
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
    @test_throws ErrorException ccall((:cptest, libccalltest), Ptr{Complex{Int}}, (Complex{Int},), &a) #compile-time error
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

    @test_approx_eq x.x a.x + 1*b
    @test_approx_eq x.y a.y - 2*b
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

    @test_approx_eq x.x a.x + b*1 - b*2im
end

type Struct12
    x::Complex64
    y::Complex64
end

let
    a = Struct12(0.8877077f5 + 0.4591081f2im, 0.0004842868f0 - 6982.3265f3im)
    b = Float32(42)

    x = ccall((:test_12, libccalltest), Struct12, (Struct12,Float32), a, b)

    @test_approx_eq x.x a.x + b*1 - b*2im
    @test_approx_eq x.y a.y + b*3 - b*4im
end

type Struct13
    x::Complex128
end

let
    a = Struct13(42968.97560380495 - 803.0576845153616im)
    b = Float64(42)

    x = ccall((:test_13, libccalltest), Struct13, (Struct13,Float64), a, b)

    @test_approx_eq x.x a.x + b*1 - b*2im
end

type Struct14
    x::Float32
    y::Float32
end

let
    a = Struct14(0.024138331f0, 0.89759064f32)
    b = Float32(42)

    x = ccall((:test_14, libccalltest), Struct14, (Struct14,Float32), a, b)

    @test_approx_eq x.x a.x + b*1
    @test_approx_eq x.y a.y - b*2
end

type Struct15
    x::Float64
    y::Float64
end

let
    a = Struct15(4.180997967273657, -0.404218594294923)
    b = Float64(42)

    x = ccall((:test_15, libccalltest), Struct15, (Struct15,Float64), a, b)

    @test_approx_eq x.x a.x + b*1
    @test_approx_eq x.y a.y - b*2
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

    @test_approx_eq x.x a.x + b*1
    @test_approx_eq x.y a.y - b*2
    @test_approx_eq x.z a.z + b*3
    @test_approx_eq x.a a.a - b*4
    @test_approx_eq x.b a.b + b*5
    @test_approx_eq x.c a.c - b*6
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
    fname = symbol("foo"*string(v))
    fname1 = symbol("foo1"*string(v))
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
            verbose && println("B(Any): ",s)
            @test s == $v
            @test s === a
            global c = s
            s
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
            verbose && println("B(Any): ",s)
            @test s == $v
            if($(t).mutable)
                @test !(s === a)
            end
            global c = s
            s
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
        #b = ccall(cfunction($fname,Any,(Ref{Any},)),Any,(Ref{Any},),$v) # unimplemented
    end
end

# issue 13031
foo13031(x) = Cint(1)
foo13031p = cfunction(foo13031, Cint, (Ref{Tuple{}},))
ccall(foo13031p, Cint, (Ref{Tuple{}},), ())

foo13031(x,y,z) = z
foo13031p = cfunction(foo13031, Cint, (Ref{Tuple{}},Ref{Tuple{}},Cint))
ccall(foo13031p, Cint, (Ref{Tuple{}},Ref{Tuple{}},Cint), (), (), 8)

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
