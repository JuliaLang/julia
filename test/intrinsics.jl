# This file is a part of Julia. License is MIT: https://julialang.org/license

# intrinsic functions

# For curmod_*
include("testenv.jl")

# bits types
@test isa((() -> Core.Intrinsics.bitcast(Ptr{Int8}, 0))(), Ptr{Int8})
@test isa(convert(Char, 65), Char)

# runtime intrinsics
@testset "runtime intrinsics" begin
    @test Core.Intrinsics.add_int(1, 1) == 2
    @test Core.Intrinsics.sub_int(1, 1) == 0
    @test_throws ErrorException("fpext: output bitsize must be >= input bitsize")    Core.Intrinsics.fpext(Int32, 0x0000_0000_0000_0000)
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Int32, 0x0000_0000)
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Int64, 0x0000_0000)
    @test_throws ErrorException("ZExt: output bitsize must be > input bitsize")     Core.Intrinsics.zext_int(Int8, 0x00)
    @test_throws ErrorException("SExt: output bitsize must be > input bitsize")     Core.Intrinsics.sext_int(Int8, 0x00)
    @test_throws ErrorException("ZExt: output bitsize must be > input bitsize")     Core.Intrinsics.zext_int(Int8, 0x0000)
    @test_throws ErrorException("SExt: output bitsize must be > input bitsize")     Core.Intrinsics.sext_int(Int8, 0x0000)
    @test_throws ErrorException("Trunc: output bitsize must be < input bitsize")    Core.Intrinsics.trunc_int(Int8, 0x00)
    @test_throws ErrorException("Trunc: output bitsize must be < input bitsize")    Core.Intrinsics.trunc_int(Int16, 0x00)
end

# issue #4581
primitive type Date4581{T} 64 end
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

# test functionality of non-power-of-2 primitive type constants
primitive type Int24 24 end
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

function compiled_conv(::Type{T}, x) where T
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

let f = Core.Intrinsics.ashr_int
    @test f(Int8(-17), 1) == -9
    @test f(Int32(-1), 33) == -1
    @test f(Int32(-1), -1) == -1
    @test f(Int32(-1), -10) == -1
    @test f(Int32(2), -1) == 0
end

# issue #29929
@test unsafe_store!(Ptr{Nothing}(C_NULL), nothing) === Ptr{Nothing}(0)
@test unsafe_load(Ptr{Nothing}(0)) === nothing
struct GhostStruct end
@test unsafe_load(Ptr{GhostStruct}(rand(Int))) === GhostStruct()

# macro to verify and compare the compiled output of an intrinsic with its runtime version
macro test_intrinsic(intr, args...)
    output = args[end]
    inputs = args[1:end-1]
    quote
        function f()
            $intr($(inputs...))
        end
        @test f() === Base.invokelatest($intr, $(inputs...))
        @test f() == $output
    end
end

@testset "Float16 intrinsics" begin
    # unary
    @test_intrinsic Core.Intrinsics.neg_float Float16(3.3) Float16(-3.3)
    @test_intrinsic Core.Intrinsics.fpext Float32 Float16(3.3) 3.3007812f0
    @test_intrinsic Core.Intrinsics.fpext Float64 Float16(3.3) 3.30078125
    @test_intrinsic Core.Intrinsics.fptrunc Float16 Float32(3.3) Float16(3.3)
    @test_intrinsic Core.Intrinsics.fptrunc Float16 Float64(3.3) Float16(3.3)

    # binary
    @test_intrinsic Core.Intrinsics.add_float Float16(3.3) Float16(2) Float16(5.3)
    @test_intrinsic Core.Intrinsics.sub_float Float16(3.3) Float16(2) Float16(1.301)
    @test_intrinsic Core.Intrinsics.mul_float Float16(3.3) Float16(2) Float16(6.6)
    @test_intrinsic Core.Intrinsics.div_float Float16(3.3) Float16(2) Float16(1.65)
    @test_intrinsic Core.Intrinsics.rem_float Float16(3.3) Float16(2) Float16(1.301)

    # ternary
    @test_intrinsic Core.Intrinsics.fma_float Float16(3.3) Float16(4.4) Float16(5.5) Float16(20.02)
    @test_intrinsic Core.Intrinsics.muladd_float Float16(3.3) Float16(4.4) Float16(5.5) Float16(20.02)

    # boolean
    @test_intrinsic Core.Intrinsics.eq_float Float16(3.3) Float16(3.3) true
    @test_intrinsic Core.Intrinsics.eq_float Float16(3.3) Float16(2) false
    @test_intrinsic Core.Intrinsics.ne_float Float16(3.3) Float16(3.3) false
    @test_intrinsic Core.Intrinsics.ne_float Float16(3.3) Float16(2) true
    @test_intrinsic Core.Intrinsics.le_float Float16(3.3) Float16(3.3) true
    @test_intrinsic Core.Intrinsics.le_float Float16(3.3) Float16(2) false

    # conversions
    @test_intrinsic Core.Intrinsics.sitofp Float16 3 Float16(3f0)
    @test_intrinsic Core.Intrinsics.uitofp Float16 UInt(3) Float16(3f0)
    @test_intrinsic Core.Intrinsics.fptosi Int Float16(3.3) 3
    @test_intrinsic Core.Intrinsics.fptoui UInt Float16(3.3) UInt(3)
end
