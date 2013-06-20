import Base.copy
ccall((:set_verbose, "./libccalltest"), Void, (Int32,), 0)

# Test for proper argument register truncation
ccall_test_func(x) = ccall((:testUcharX, "./libccalltest"), Int32, (Uint8,), x)
@test ccall_test_func(3) == 1
@test ccall_test_func(259) == 1

# Tests for passing and returning structs
copy{T}(x::ComplexPair{T}) = ComplexPair{T}(x.re,x.im)

ci = 20+51im
a = copy(ci)
b = ccall((:ctest, "./libccalltest"), ComplexPair{Int}, (ComplexPair{Int},), a)
@test a == ci
@test b == ci + 1 - 2im
a = copy(ci)
b = unsafe_load(ccall((:cptest, "./libccalltest"), Ptr{ComplexPair{Int}}, (Ptr{ComplexPair{Int}},), &a))
@test !(a === b)
@test b == ci + 1 - 2im

cf64 = 2.84+5.2im
a = copy(cf64)
b = ccall((:cgtest, "./libccalltest"), Complex128, (Complex128,), a)
@test a == cf64
@test b == cf64 + 1 - 2im
b = unsafe_load(ccall((:cgptest, "./libccalltest"), Ptr{Complex128}, (Ptr{Complex128},), &a))
@test !(a === b)
@test a == cf64
@test b == cf64 + 1 - 2im

cf32 = 3.34f0+53.2f0im
a = copy(cf32)
b = ccall((:cftest, "./libccalltest"), Complex64, (Complex64,), a)
@test a == cf32
@test b == cf32 + 1 - 2im
b = unsafe_load(ccall((:cfptest, "./libccalltest"), Ptr{Complex64}, (Ptr{Complex64},), &a))
@test !(a === b)
@test a == cf32
@test b == cf32 + 1 - 2im


# Tests for native Julia data types
@test_fails ccall((:cptest, "./libccalltest"), Ptr{ComplexPair{Int}}, (Ptr{ComplexPair{Int}},), a)
@test_fails ccall((:cptest, "./libccalltest"), Ptr{ComplexPair{Int}}, (ComplexPair{Int},), &a)

# Tests for various sized data types (ByVal)
type Struct1
    x::Float32
    y::Float64
end
copy(a::Struct1) = Struct1(a.x, a.y)
s1 = Struct1(352.39422f23, 19.287577)
a = copy(s1)
b = ccall((:test_1, "./libccalltest"), Struct1, (Struct1,), a)
@test a.x == s1.x && a.y == s1.y
@test !(a === b)
@test b.x == s1.x + 1 && b.y == s1.y - 2

ci32 = ComplexPair{Int32}(int32(10),int32(31))
a = copy(ci32)
ba = ccall((:test_2a, "./libccalltest"), ComplexPair{Int32}, (ComplexPair{Int32},), a)
bb = ccall((:test_2b, "./libccalltest"), ComplexPair{Int32}, (ComplexPair{Int32},), a)
@test a == ci32
@test ba == bb == ci32 + 1 - 2im 

ci64 = ComplexPair{Int64}(int64(20),int64(51))
a = copy(ci64)
ba = ccall((:test_3a, "./libccalltest"), ComplexPair{Int64}, (ComplexPair{Int64},), a)
bb = ccall((:test_3b, "./libccalltest"), ComplexPair{Int64}, (ComplexPair{Int64},), a)
bc = ccall((:test_4, "./libccalltest"), ComplexPair{Int64}, (ComplexPair{Int64},), a)
@test a == ci64
@test ba == bb == ci64 + 1 - 2im
@test bc == ci64 + 1

i128 = int128(0x7f00123456789abc)<<64 + uint64(-1)
a = copy(i128)
b = ccall((:test_4, "./libccalltest"), Int128, (Int128,), a)
@test a == i128
@test b == i128 + 1

type Struct_Big
    x::Int
    y::Int
    z::Int8
end
copy(a::Struct_Big) = Struct_Big(a.x, a.y, a.z)
sbig = Struct_Big(424,-5,int8('Z'))

a = copy(sbig)
b = ccall((:test_big, "./libccalltest"), Struct_Big, (Struct_Big,), a)
@test a.x == sbig.x && a.y == sbig.y && a.z == sbig.z
@test b.x == sbig.x + 1
@test b.y == sbig.y - 2
@test b.z == sbig.z - 'A'
