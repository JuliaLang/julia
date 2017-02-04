# This file is a part of Julia. License is MIT: http://julialang.org/license

# intrinsic functions
const curmod = current_module()
const curmod_name = fullname(curmod)
const curmod_prefix = "$(["$m." for m in curmod_name]...)"

# bits types
@test isa((() -> Core.Intrinsics.bitcast(Ptr{Int8}, 0))(), Ptr{Int8})
@test isa(convert(Char, 65), Char)

# runtime intrinsics
let f = Any[Core.Intrinsics.add_int, Core.Intrinsics.sub_int]
    @test f[1](1, 1) == 2
    @test f[2](1, 1) == 0
end

# issue #4581
bitstype 64 Date4581{T}
let
    x = Core.Intrinsics.bitcast(Date4581{Int}, Int64(1234))
    xs = Date4581[x]
    ys = copy(xs)
    @test ys !== xs
    @test ys == xs
end

# issue #6591
function f6591(d)
    Core.Intrinsics.bitcast(Int64, d)
    return (f -> f(d))(identity)
end
let d = Core.Intrinsics.bitcast(Date4581{Int}, Int64(1))
    @test isa(f6591(d), Date4581)
end

# test functionality of non-power-of-2 bitstype constants
bitstype 24 Int24
Int24(x::Int) = Core.Intrinsics.trunc_int(Int24, x)
Int(x::Int24) = Core.Intrinsics.zext_int(Int, x)
let x, y, f
    x = Int24(Int(0x12345678)) # create something (via truncation)
    @test Int(0x345678) === Int(x)
    f() = Int24(Int(0x02468ace))
    y = f() # invoke llvm constant folding
    @test Int(0x468ace) === Int(y)
    @test x !== y
    @test string(y) == "$(curmod_prefix)Int24(0x468ace)"
end

# test nonsensical valid conversions and errors

compiled_addi(x, y) = Core.Intrinsics.add_int(x, y)
@test compiled_addi(C_NULL, C_NULL) === C_NULL
@test_throws ErrorException compiled_addi(C_NULL, 1)
@test_throws ErrorException ((x)->compiled_addi(Float64(x), x))(1)
@test ((x)->compiled_addi(Float64(x), Float64(x)))(2) === -0.0
@test compiled_addi(0.5, 5.0e-323) === 0.5000000000000011
@test_throws ErrorException compiled_addi(Int8(1), UInt8(1))
@test compiled_addi(UInt8(1), UInt8(2)) === UInt8(3)
@test_throws ErrorException compiled_addi(UInt8(1), UInt16(2))
@test compiled_addi(Float32(.125), Float32(10)) === 2.1267648f38
@test compiled_addi(true, true) === false

compiled_addf(x, y) = Core.Intrinsics.add_float(x, y)
@test compiled_addf(C_NULL, C_NULL) === C_NULL
@test_throws ErrorException compiled_addf(C_NULL, 1)
@test compiled_addf(0.5, 5.0e-323) === 0.5
@test_throws ErrorException compiled_addf(im, im)
@test_throws ErrorException compiled_addf(true, true)

function compiled_conv{T}(::Type{T}, x)
    t = Core.Intrinsics.trunc_int(T, x)
    z = Core.Intrinsics.zext_int(typeof(x), t)
    s = Core.Intrinsics.sext_int(typeof(x), t)
    fpt = Core.Intrinsics.fptrunc(T, x)
    fpe = Core.Intrinsics.fpext(typeof(x), fpt)
    return (t, z, s, fpt, fpe)
end
@test compiled_conv(UInt32, Int64(0x8000_0000)) ==
    (0x80000000, Int64(0x80000000), -Int64(0x80000000), 0x00000000, 0)
@test compiled_conv(UInt32, UInt64(0xC000_BA98_8765_4321)) ==
    (0x87654321, 0x0000000087654321, 0xffffffff87654321, 0xc005d4c4, 0xc000ba9880000000)
@test_throws ErrorException compiled_conv(Bool, im)
