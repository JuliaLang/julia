import Base.copy, Base.==
const verbose  = false
ccall((:set_verbose, "./libccalltest"), Void, (Int32,), verbose)

# Test for proper argument register truncation
ccall_test_func(x) = ccall((:testUcharX, "./libccalltest"), Int32, (UInt8,), x % UInt8)
@test ccall_test_func(3) == 1
@test ccall_test_func(259) == 1

# Test for proper round-trip of Ref{T} type
ccall_echo_func{T,U}(x, ::Type{T}, ::Type{U}) = ccall((:test_echo_p, "./libccalltest"), T, (U,), x)
type IntLike
    x::Int
end
@test unsafe_load(ccall_echo_func(132, Ptr{Int}, Ref{Int})) === 132
@test unsafe_load(ccall_echo_func(Ref(921), Ptr{Int}, Ref{Int})) === 921
@test unsafe_load(ccall_echo_func(IntLike(993), Ptr{Int}, Ref{IntLike})) === 993
@test unsafe_load(ccall_echo_func(IntLike(881), Ptr{IntLike}, Ref{IntLike})).x === 881
@test ccall_echo_func(532, Int, Int) === 532
if WORD_SIZE == 64
    # this test is valid only for x86_64 and win64
    @test ccall_echo_func(164, IntLike, Int).x === 164
end
@test ccall_echo_func(IntLike(828), Int, IntLike) === 828
@test ccall_echo_func(913, Any, Any) === 913
@test unsafe_pointer_to_objref(ccall_echo_func(553, Ptr{Any}, Any)) === 553
@test ccall_echo_func(124, Ref{Int}, Any) === 124
@test unsafe_load(ccall_echo_func(422, Ptr{Any}, Ref{Any})) === 422
@test unsafe_load(ccall_echo_func([383], Ptr{Int}, Ref{Int})) === 383
@test unsafe_load(ccall_echo_func(Ref([144,172],2), Ptr{Int}, Ref{Int})) === 172
#@test unsafe_load(ccall_echo_func(Ref([8],1,1), Ptr{Int}, Ref{Int})) === 8

# Tests for passing and returning structs
ci = 20+51im
b = ccall((:ctest, "./libccalltest"), Complex{Int}, (Complex{Int},), ci)
@test b == ci + 1 - 2im
b = unsafe_load(ccall((:cptest, "./libccalltest"), Ptr{Complex{Int}}, (Ptr{Complex{Int}},), &ci))
@test b == ci + 1 - 2im
@test ci == 20+51im


cf64 = 2.84+5.2im
b = ccall((:cgtest, "./libccalltest"), Complex128, (Complex128,), cf64)
@test b == cf64 + 1 - 2im
b = unsafe_load(ccall((:cgptest, "./libccalltest"), Ptr{Complex128}, (Ptr{Complex128},), &cf64))
@test b == cf64 + 1 - 2im
@test cf64 == 2.84+5.2im

cf32 = 3.34f0+53.2f0im
b = ccall((:cftest, "./libccalltest"), Complex64, (Complex64,), cf32)
@test b == cf32 + 1 - 2im
b = unsafe_load(ccall((:cfptest, "./libccalltest"), Ptr{Complex64}, (Ptr{Complex64},), &cf32))
@test b == cf32 + 1 - 2im
@test cf32 == 3.34f0+53.2f0im


# Tests for native Julia data types
@test_throws MethodError ccall((:cptest, "./libccalltest"), Ptr{Complex{Int}}, (Ptr{Complex{Int}},), cf32)
@test_throws ErrorException ccall((:cptest, "./libccalltest"), Ptr{Complex{Int}}, (Complex{Int},), &cf32) #compile-time error

# Tests for various sized data types (ByVal)
type Struct1
    x::Float32
    y::Float64
end
==(a::Struct1,b::Struct1) = a.x == b.x && a.y == b.y
copy(a::Struct1) = Struct1(a.x, a.y)
s1 = Struct1(352.39422f23, 19.287577)
a = copy(s1)
b = ccall((:test_1, "./libccalltest"), Struct1, (Struct1,), a)
@test a.x == s1.x && a.y == s1.y
@test !(a === b)
@test b.x == s1.x + 1 && b.y == s1.y - 2

function foos1(s::Struct1)
    @test !(s === a)
    @test s == a
    s
end

ci32 = Complex{Int32}(int32(10),int32(31))
ba = ccall((:test_2a, "./libccalltest"), Complex{Int32}, (Complex{Int32},), ci32)
bb = ccall((:test_2b, "./libccalltest"), Complex{Int32}, (Complex{Int32},), ci32)
@test ba == bb == ci32 + 1 - 2im
@test ci32 == Complex{Int32}(int32(10),int32(31))

ci64 = Complex{Int64}(int64(20),int64(51))
ba = ccall((:test_3a, "./libccalltest"), Complex{Int64}, (Complex{Int64},), ci64)
bb = ccall((:test_3b, "./libccalltest"), Complex{Int64}, (Complex{Int64},), ci64)
bc = ccall((:test_128, "./libccalltest"), Complex{Int64}, (Complex{Int64},), ci64)
@test ba == bb == ci64 + 1 - 2im
@test bc == ci64 + 1
@test ci64 == Complex{Int64}(int64(20),int64(51))

i128 = int128(0x7f00123456789abc)<<64 + typemax(UInt64)
b = ccall((:test_128, "./libccalltest"), Int128, (Int128,), i128)
@test b == i128 + 1
@test i128 == int128(0x7f00123456789abc)<<64 + typemax(UInt64)

type Struct_Big
    x::Int
    y::Int
    z::Int8
end
==(a::Struct_Big,b::Struct_Big) = a.x == b.x && a.y == b.y && a.z == b.z
copy(a::Struct_Big) = Struct_Big(a.x, a.y, a.z)
sbig = Struct_Big(424,-5,int8('Z'))

a = copy(sbig)
b = ccall((:test_big, "./libccalltest"), Struct_Big, (Struct_Big,), a)
@test a.x == sbig.x && a.y == sbig.y && a.z == sbig.z
@test b.x == sbig.x + 1
@test b.y == sbig.y - 2
@test b.z == sbig.z - int('A')

verbose && flush_cstdio()
verbose && println("Testing cfunction roundtrip: ")
# cfunction roundtrip
for (t,v) in ((Complex{Int32},:ci32),(Complex{Int64},:ci64),
            (Complex64,:cf32),(Complex128,:cf64),(Struct1,:s1))
    @eval begin
        verbose && println($t)
        if($(t).mutable)
            a = copy($v)
        else
            a = $v
        end
        verbose && println("A: ",a)
        function $(symbol("foo"*string(v)))(s::$t)
            verbose && println("B: ",s)
            if($(t).mutable)
                @test !(s === a)
            end
            @test s == a
            s
        end
        b = ccall(cfunction($(symbol("foo"*string(v))),$t,($t,)),$t,($t,),$v)
        verbose && println("C: ",b)
        if ($(t).mutable)
            @test !(b === a)
        end
        @test a == b
    end
end
