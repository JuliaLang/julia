import Base.copy, Base.isequal
const verbose  = true
ccall((:set_verbose, "./libccalltest"), Void, (Int32,), verbose)

# Test for proper argument register truncation
ccall_test_func(x) = ccall((:testUcharX, "./libccalltest"), Int32, (Uint8,), x)
@test ccall_test_func(3) == 1
@test ccall_test_func(259) == 1

# Tests for passing and returning structs
ci = 20+51im
b = ccall((:ctest, "./libccalltest"), ComplexPair{Int}, (ComplexPair{Int},), ci)
@test b == ci + 1 - 2im
b = unsafe_load(ccall((:cptest, "./libccalltest"), Ptr{ComplexPair{Int}}, (Ptr{ComplexPair{Int}},), &ci))
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
@test_fails ccall((:cptest, "./libccalltest"), Ptr{ComplexPair{Int}}, (Ptr{ComplexPair{Int}},), cf32)
@test_fails ccall((:cptest, "./libccalltest"), Ptr{ComplexPair{Int}}, (ComplexPair{Int},), &cf32)

# Tests for various sized data types (ByVal)
type Struct1
    x::Float32
    y::Float64
end
isequal(a::Struct1,b::Struct1) = a.x == b.x && a.y == b.y
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

ci32 = ComplexPair{Int32}(int32(10),int32(31))
ba = ccall((:test_2a, "./libccalltest"), ComplexPair{Int32}, (ComplexPair{Int32},), ci32)
bb = ccall((:test_2b, "./libccalltest"), ComplexPair{Int32}, (ComplexPair{Int32},), ci32)
@test ba == bb == ci32 + 1 - 2im 
@test ci32 == ComplexPair{Int32}(int32(10),int32(31))

ci64 = ComplexPair{Int64}(int64(20),int64(51))
ba = ccall((:test_3a, "./libccalltest"), ComplexPair{Int64}, (ComplexPair{Int64},), ci64)
bb = ccall((:test_3b, "./libccalltest"), ComplexPair{Int64}, (ComplexPair{Int64},), ci64)
bc = ccall((:test_128, "./libccalltest"), ComplexPair{Int64}, (ComplexPair{Int64},), ci64)
@test ba == bb == ci64 + 1 - 2im
@test bc == ci64 + 1
@test ci64 == ComplexPair{Int64}(int64(20),int64(51))

i128 = int128(0x7f00123456789abc)<<64 + uint64(-1)
b = ccall((:test_128, "./libccalltest"), Int128, (Int128,), i128)
@test b == i128 + 1
@test i128 == int128(0x7f00123456789abc)<<64 + uint64(-1)

type Struct_Big
    x::Int
    y::Int
    z::Int8
end
isequal(a::Struct_Big,b::Struct_Big) = a.x == b.x && a.y == b.y && a.z == b.z
copy(a::Struct_Big) = Struct_Big(a.x, a.y, a.z)
sbig = Struct_Big(424,-5,int8('Z'))

a = copy(sbig)
b = ccall((:test_big, "./libccalltest"), Struct_Big, (Struct_Big,), a)
@test a.x == sbig.x && a.y == sbig.y && a.z == sbig.z
@test b.x == sbig.x + 1
@test b.y == sbig.y - 2
@test b.z == sbig.z - 'A'

verbose && println("Testing cfunction roundtrip: ")
# cfunction roundtrip
for (t,v) in ((ComplexPair{Int32},:ci32),(ComplexPair{Int64},:ci64),
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
        if($(t).mutable)
            @test !(b === a)
        end
        @test a == b
    end
end
