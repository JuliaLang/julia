# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.copy, Base.==
const verbose = false
const libccalltest = joinpath(dirname(@__FILE__), "libccalltest")
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

# Tests for passing and returning structs
ci = 20+51im
b = ccall((:ctest, libccalltest), Complex{Int}, (Complex{Int},), ci)
@test b == ci + 1 - 2im
ci_ary = [ci] # Make sure the array is alive during unsafe_load
b = unsafe_load(ccall((:cptest, libccalltest), Ptr{Complex{Int}},
                      (Ptr{Complex{Int}},), ci_ary))
@test b == ci + 1 - 2im
@test ci == 20+51im

b = ccall((:cptest_static, libccalltest), Ptr{Complex{Int}}, (Ptr{Complex{Int}},), &ci)
@test unsafe_load(b) == ci
Libc.free(convert(Ptr{Void},b))

cf64 = 2.84+5.2im
b = ccall((:cgtest, libccalltest), Complex128, (Complex128,), cf64)
@test b == cf64 + 1 - 2im
cf64_ary = [cf64] # Make sure the array is alive during unsafe_load
b = unsafe_load(ccall((:cgptest, libccalltest), Ptr{Complex128}, (Ptr{Complex128},), cf64_ary))
@test b == cf64 + 1 - 2im
@test cf64 == 2.84+5.2im

cf32 = 3.34f0+53.2f0im
b = ccall((:cftest, libccalltest), Complex64, (Complex64,), cf32)
@test b == cf32 + 1 - 2im
cf32_ary = [cf32] # Make sure the array is alive during unsafe_load
b = unsafe_load(ccall((:cfptest, libccalltest), Ptr{Complex64}, (Ptr{Complex64},), cf32_ary))
@test b == cf32 + 1 - 2im
@test cf32 == 3.34f0+53.2f0im


# Tests for native Julia data types
@test_throws MethodError ccall((:cptest, libccalltest), Ptr{Complex{Int}}, (Ptr{Complex{Int}},), cf32)
@test_throws MethodError ccall((:cptest, libccalltest), Ptr{Complex{Int}}, (Complex{Int},), &cf32)

# Tests for various sized data types (ByVal)
type Struct1
    x::Float32
    y::Float64
end
==(a::Struct1,b::Struct1) = a.x == b.x && a.y == b.y
copy(a::Struct1) = Struct1(a.x, a.y)
s1 = Struct1(352.39422f23, 19.287577)
a = copy(s1)
b = ccall((:test_1, libccalltest), Struct1, (Struct1,), a)
@test a.x == s1.x && a.y == s1.y
@test !(a === b)
@test b.x == s1.x + 1 && b.y == s1.y - 2

function foos1(s::Struct1)
    @test !(s === a)
    @test s == a
    s
end

ci32 = Complex{Int32}(Int32(10),Int32(31))
ba = ccall((:test_2a, libccalltest), Complex{Int32}, (Complex{Int32},), ci32)
bb = ccall((:test_2b, libccalltest), Complex{Int32}, (Complex{Int32},), ci32)
@test ba == bb == ci32 + 1 - 2im
@test ci32 == Complex{Int32}(Int32(10),Int32(31))

ci64 = Complex{Int64}(Int64(20),Int64(51))
ba = ccall((:test_3a, libccalltest), Complex{Int64}, (Complex{Int64},), ci64)
bb = ccall((:test_3b, libccalltest), Complex{Int64}, (Complex{Int64},), ci64)
bc = ccall((:test_128, libccalltest), Complex{Int64}, (Complex{Int64},), ci64)
@test ba == bb == ci64 + 1 - 2im
@test bc == ci64 + 1
@test ci64 == Complex{Int64}(Int64(20),Int64(51))

i128 = Int128(0x7f00123456789abc)<<64 + typemax(UInt64)
b = ccall((:test_128, libccalltest), Int128, (Int128,), i128)
@test b == i128 + 1
@test i128 == Int128(0x7f00123456789abc)<<64 + typemax(UInt64)

type Struct_Big
    x::Int
    y::Int
    z::Int8
end
==(a::Struct_Big,b::Struct_Big) = a.x == b.x && a.y == b.y && a.z == b.z
copy(a::Struct_Big) = Struct_Big(a.x, a.y, a.z)
sbig = Struct_Big(424,-5,Int8('Z'))

a = copy(sbig)
b = ccall((:test_big, libccalltest), Struct_Big, (Struct_Big,), a)
@test a.x == sbig.x && a.y == sbig.y && a.z == sbig.z
@test b.x == sbig.x + 1
@test b.y == sbig.y - 2
@test b.z == sbig.z - Int('A')

verbose && Libc.flush_cstdio()
verbose && println("Testing cfunction roundtrip: ")
# cfunction roundtrip
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
