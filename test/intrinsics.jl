# This file is a part of Julia. License is MIT: https://julialang.org/license

# intrinsic functions

# For curmod_*
include("testenv.jl")

# bits types
@test isa((() -> Core.Intrinsics.bitcast(Ptr{Int8}, 0))(), Ptr{Int8})
@test isa(convert(Char, 65), Char)

truncbool(u) = reinterpret(UInt8, reinterpret(Bool, u))
@test truncbool(0x01) == 0x01
@test truncbool(0x02) == 0x00
@test truncbool(0x03) == 0x01

# runtime intrinsics
@testset "runtime intrinsics" begin
    @test Core.Intrinsics.add_int(1, 1) == 2
    @test Core.Intrinsics.sub_int(1, 1) == 0

    @test_throws ErrorException("fpext: output bitsize must be > input bitsize")    Core.Intrinsics.fpext(Float32, 1.0)
    @test_throws ErrorException("fpext: output bitsize must be > input bitsize")    Core.Intrinsics.fpext(Float32, 1.0)

    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Int32, 0x0000_0000)
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Int64, 0x0000_0000)
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Float16, Float16(1.0))
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Core.BFloat16, Float16(1.0))
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Float32, Float16(1.0))
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Float32, 1.0f0)
    @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Float64, 1.0)

    let bf16_1 = Core.Intrinsics.bitcast(Core.BFloat16, 0x3f80)
        @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Core.BFloat16, bf16_1)
        @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Float16, bf16_1)
        @test_throws ErrorException("fptrunc: output bitsize must be < input bitsize")  Core.Intrinsics.fptrunc(Float32, bf16_1)
    end

    @test_throws ErrorException("ZExt: output bitsize must be > input bitsize")     Core.Intrinsics.zext_int(Int8, 0x00)
    @test_throws ErrorException("SExt: output bitsize must be > input bitsize")     Core.Intrinsics.sext_int(Int8, 0x00)
    @test_throws ErrorException("ZExt: output bitsize must be > input bitsize")     Core.Intrinsics.zext_int(Int8, 0x0000)
    @test_throws ErrorException("SExt: output bitsize must be > input bitsize")     Core.Intrinsics.sext_int(Int8, 0x0000)
    @test_throws ErrorException("Trunc: output bitsize must be < input bitsize")    Core.Intrinsics.trunc_int(Int8, 0x00)
    @test_throws ErrorException("Trunc: output bitsize must be < input bitsize")    Core.Intrinsics.trunc_int(Int16, 0x00)

    @test_throws ErrorException("add_float: runtime floating point intrinsics require both arguments to be Float16, BFloat16, Float32, or Float64") Core.Intrinsics.add_float(1, 2)
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
Base.Int(x::Int24) = Core.Intrinsics.zext_int(Int, x)
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
@test_throws ErrorException compiled_addf(C_NULL, C_NULL)
@test_throws ErrorException compiled_addf(C_NULL, 1)
@test compiled_addf(0.5, 5.0e-323) === 0.5
@test_throws ErrorException compiled_addf(im, im)
@test_throws ErrorException compiled_addf(true, true)

function compiled_conv(::Type{T}, x) where T
    t = Core.Intrinsics.trunc_int(T, x)
    z = Core.Intrinsics.zext_int(typeof(x), t)
    s = Core.Intrinsics.sext_int(typeof(x), t)
    return (t, z, s)
end
@test compiled_conv(UInt32, Int64(0x8000_0000)) ==
    (0x80000000, Int64(0x80000000), -Int64(0x80000000))
@test compiled_conv(UInt32, UInt64(0xC000_BA98_8765_4321)) ==
    (0x87654321, 0x0000000087654321, 0xffffffff87654321)
@test_throws ErrorException compiled_conv(Bool, im)

function compiled_fptrunc(::Type{T}, x) where T
    return Core.Intrinsics.fptrunc(T, x)

end
#           1.234
#           0 01111111 00111011111001110110110
#   float32 0 01111111 00111011111001110110110
#   float16 0    01111 0011101111              (truncated/rtz)
#   float16 0    01111 0011110000              (round-to-nearest)
#  bfloat16 0 01111111 0011110                 (round-to-nearest)
@test compiled_fptrunc(Float16, 1.234) === reinterpret(Float16, 0b0_01111_0011110000)
# On arm64, LLVM gives an assertion failure when compiling this:
# LLVM ERROR: Cannot select: 0x106c8e570: bf16 = fp_round 0x106c8df50, TargetConstant:i64<0>, intrinsics.jl:114
#   0x106c8df50: f64,ch = CopyFromReg 0x104545960, Register:f64 %1
#     0x106c8dee0: f64 = Register %1
#   0x106c8e3b0: i64 = TargetConstant<0>
# In function: julia_compiled_fptrunc_3480
# @test compiled_fptrunc(Core.BFloat16, 1.234) === reinterpret(Core.BFloat16, 0b0_01111111_0011110)
@test compiled_fptrunc(Float32, 1.234) === 1.234f0
@test_throws ErrorException compiled_fptrunc(Float64, 1.234f0)
@test_throws ErrorException compiled_fptrunc(Int32, 1.234)
@test_throws ErrorException compiled_fptrunc(Float32, 1234)

function compiled_fpext(::Type{T}, x) where T
    return Core.Intrinsics.fpext(T, x)
end
#           1.234
#   float16 0    01111 0011110000
#           0 01111111 00111100000000000000000 = 1.234375

#           1.234
#   float32 0 01111111    00111011111001110110110
#   float64 0 01111111111 0011101111100111011011000000000000000000000000000000
#                         3be76c
@test compiled_fpext(Float32, reinterpret(Float16, 0b0_01111_0011110000)) === 1.234375f0
@test compiled_fpext(Float64, reinterpret(Float16, 0b0_01111_0011110000)) === 1.234375
@test compiled_fpext(Float64, 1.234f0) === 0x1.3be76cp0
@test_throws ErrorException compiled_fpext(Float16, Float16(1.0))
@test_throws ErrorException compiled_fpext(Float16, 1.0f0)
@test_throws ErrorException compiled_fpext(Float32, 1.0f0)
@test_throws ErrorException compiled_fpext(Float32, 1.0)
@test_throws ErrorException compiled_fpext(Float64, 1.0)

let f = Core.Intrinsics.ashr_int
    @test f(Int8(-17), 1) == -9
    @test f(Int32(-1), 33) == -1
    @test f(Int32(-1), -1) == -1
    @test f(Int32(-1), -10) == -1
    @test f(Int32(2), -1) == 0
end

const ReplaceType = ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T

@testset "elsize(::Type{<:Ptr})" begin
    @test Base.elsize(Ptr{Any}) == sizeof(Int)
    @test Base.elsize(Ptr{NTuple{3,Int8}}) == 3
    @test Base.elsize(Ptr{Cvoid}) == 0
    @test Base.elsize(Ptr{Base.RefValue{Any}}) == sizeof(Int)
    @test Base.elsize(Ptr{Int}) == sizeof(Int)
    @test_throws MethodError Base.elsize(Ptr)
    @test_throws ErrorException Base.elsize(Ptr{Ref{Int}})
    @test_throws ErrorException Base.elsize(Ptr{Ref})
    @test_throws ErrorException Base.elsize(Ptr{Complex})
end

# issue #29929
let p = Ptr{Nothing}(0)
    @test unsafe_store!(p, nothing) === C_NULL
    @test unsafe_load(p) === nothing
    @test unsafe_load(p, :sequentially_consistent) === nothing
    @test unsafe_store!(p, nothing, :sequentially_consistent) === p
    @test unsafe_swap!(p, nothing, :sequentially_consistent) === nothing
    @test unsafe_modify!(p, (i, j) -> j, nothing, :sequentially_consistent) === Pair(nothing, nothing)
    @test unsafe_replace!(p, nothing, nothing, :sequentially_consistent, :sequentially_consistent) === ReplaceType{Nothing}((nothing, true))
    @test unsafe_replace!(p, missing, nothing, :sequentially_consistent, :sequentially_consistent) === ReplaceType{Nothing}((nothing, false))
end

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
        @test f() === $output
    end
end

macro test_intrinsic_pred(intr, args...)
    p = args[end]
    inputs = args[1:end-1]
    quote
        function f()
            $intr($(inputs...))
        end
        @test $(p)(Base.invokelatest($intr, $(inputs...)))
        @test $(p)(f())
    end
end

@testset "Float64 intrinsics" begin
    # unary
    @test_intrinsic Core.Intrinsics.abs_float Float64(-3.3) Float64(3.3)
    @test_intrinsic Core.Intrinsics.neg_float Float64(3.3) Float64(-3.3)

    # binary
    @test_intrinsic Core.Intrinsics.add_float Float64(3.3) Float64(2) Float64(5.3)
    @test_intrinsic Core.Intrinsics.sub_float Float64(3.3) Float64(2) Float64(1.2999999999999998)
    @test_intrinsic Core.Intrinsics.mul_float Float64(3.3) Float64(2) Float64(6.6)
    @test_intrinsic Core.Intrinsics.div_float Float64(3.3) Float64(2) Float64(1.65)
    @test_intrinsic Core.Intrinsics.max_float Float64(1.0) Float64(2.0) Float64(2.0)
    @test_intrinsic Core.Intrinsics.min_float Float64(1.0) Float64(2.0) Float64(1.0)

    # ternary
    @test_intrinsic Core.Intrinsics.fma_float Float64(3.3) Float64(4.4) Float64(5.5) Float64(20.02)
    @test_intrinsic Core.Intrinsics.muladd_float Float64(3.3) Float64(4.4) Float64(5.5) Float64(20.02)
    @test_intrinsic Core.Intrinsics.fma_float 0x1.0000000000001p0 1.25 0x1p-54 0x1.4000000000002p0
    @test 0x1.0000000000001p0*1.25+0x1p-54 === 0x1.4000000000001p0 # for comparison

    # boolean
    @test_intrinsic Core.Intrinsics.eq_float Float64(3.3) Float64(3.3) true
    @test_intrinsic Core.Intrinsics.eq_float Float64(3.3) Float64(2) false
    @test_intrinsic Core.Intrinsics.ne_float Float64(3.3) Float64(3.3) false
    @test_intrinsic Core.Intrinsics.ne_float Float64(3.3) Float64(2) true
    @test_intrinsic Core.Intrinsics.le_float Float64(3.3) Float64(3.3) true
    @test_intrinsic Core.Intrinsics.le_float Float64(3.3) Float64(2) false

    # conversions
    @test_intrinsic Core.Intrinsics.sitofp Float64 3 Float64(3.0)
    @test_intrinsic Core.Intrinsics.uitofp Float64 UInt(3) Float64(3.0)
    @test_intrinsic Core.Intrinsics.fptosi Int Float64(3.3) 3
    @test_intrinsic Core.Intrinsics.fptoui UInt Float64(3.3) UInt(3)

    # #57384
    @test_intrinsic Core.Intrinsics.fptosi Int 1.5 1
    @test_intrinsic Core.Intrinsics.fptosi Int128 1.5 Int128(1)
end

@testset "Float32 intrinsics" begin
    # unary
    @test_intrinsic Core.Intrinsics.abs_float Float32(-3.3) Float32(3.3)
    @test_intrinsic Core.Intrinsics.neg_float Float32(3.3) Float32(-3.3)
    @test_intrinsic Core.Intrinsics.fpext Float64 Float32(3.3) 3.299999952316284
    @test_intrinsic Core.Intrinsics.fptrunc Float32 Float64(3.3) Float32(3.3)

    # binary
    @test_intrinsic Core.Intrinsics.add_float Float32(3.3) Float32(2) Float32(5.3)
    @test_intrinsic Core.Intrinsics.sub_float Float32(3.3) Float32(2) Float32(1.3)
    @test_intrinsic Core.Intrinsics.mul_float Float32(3.3) Float32(2) Float32(6.6)
    @test_intrinsic Core.Intrinsics.div_float Float32(3.3) Float32(2) Float32(1.65)
    @test_intrinsic Core.Intrinsics.max_float Float32(1.0) Float32(2.0) Float32(2.0)
    @test_intrinsic Core.Intrinsics.min_float Float32(1.0) Float32(2.0) Float32(1.0)

    # ternary
    @test_intrinsic Core.Intrinsics.fma_float Float32(3.3) Float32(4.4) Float32(5.5) Float32(20.02)
    @test_intrinsic Core.Intrinsics.muladd_float Float32(3.3) Float32(4.4) Float32(5.5) Float32(20.02)
    @test_intrinsic Core.Intrinsics.fma_float Float32(0x1.000002p0) 1.25f0 Float32(0x1p-25) Float32(0x1.400004p0)
    @test Float32(0x1.000002p0)*1.25f0+Float32(0x1p-25) === Float32(0x1.400002p0) # for comparison


    # boolean
    @test_intrinsic Core.Intrinsics.eq_float Float32(3.3) Float32(3.3) true
    @test_intrinsic Core.Intrinsics.eq_float Float32(3.3) Float32(2) false
    @test_intrinsic Core.Intrinsics.ne_float Float32(3.3) Float32(3.3) false
    @test_intrinsic Core.Intrinsics.ne_float Float32(3.3) Float32(2) true
    @test_intrinsic Core.Intrinsics.le_float Float32(3.3) Float32(3.3) true
    @test_intrinsic Core.Intrinsics.le_float Float32(3.3) Float32(2) false

    # conversions
    @test_intrinsic Core.Intrinsics.sitofp Float32 3 Float32(3.0)
    @test_intrinsic Core.Intrinsics.uitofp Float32 UInt(3) Float32(3.0)
    @test_intrinsic Core.Intrinsics.fptosi Int Float32(3.3) 3
    @test_intrinsic Core.Intrinsics.fptoui UInt Float32(3.3) UInt(3)
end

function f16(sign, exp, sig)
    x = (sign&1)<<15 | (exp&((1<<5)-1))<<10 | sig&((1<<10)-1)
    return reinterpret(Float16, UInt16(x))
end
function f32(sign, exp, sig)
    x = (sign&1)<<31 | (exp&((1<<8)-1))<<23 | sig&((1<<23)-1)
    return reinterpret(Float32, UInt32(x))
end
function f64(sign, exp, sig)
    x = Int64(sign&1)<<31 | Int64(exp&((1<<11)-1))<<52 | sig&((Int64(1)<<52)-1)
    return reinterpret(Float64, UInt64(x))
end

@testset "Float16 intrinsics" begin
    # unary
    @test_intrinsic Core.Intrinsics.abs_float Float16(-3.3) Float16(3.3)
    @test_intrinsic Core.Intrinsics.neg_float Float16(3.3) Float16(-3.3)
    # See <https://github.com/JuliaLang/julia/issues/57130>
    @test_intrinsic Core.Intrinsics.fpext Float32 Float16(3.3) 3.3007812f0
    @test_intrinsic Core.Intrinsics.fpext Float64 Float16(3.3) 3.30078125
    @test_intrinsic Core.Intrinsics.fptrunc Float16 Float32(3.3) Float16(3.3)
    @test_intrinsic Core.Intrinsics.fptrunc Float16 Float64(3.3) Float16(3.3)

    # #57805 - cases where rounding Float64 -> Float32 -> Float16 would fail
    #     2^-25 * 0b1.0000000000000000000000000000000000000001 binary
    #   0 01111100110 0000000000000000000000000000000000000001000000000000
    #     2^-25 * 0b1.0                                        binary
    #   0    01100110 00000000000000000000000
    #     2^-14 * 0b0.0000000001 (subnormal)
    #   0       00000 0000000001 (correct)
    #   0       00000 0000000000 (incorrect)
    @test_intrinsic Core.Intrinsics.fptrunc Float16 0x1.0000000001p-25 Float16(6.0e-8)
    @test_intrinsic Core.Intrinsics.fptrunc Float16 -0x1.0000000001p-25 Float16(-6.0e-8)

    # float_to_half/bfloat_to_float special cases
    @test_intrinsic Core.Intrinsics.fptrunc Float16 Inf32 Inf16
    @test_intrinsic Core.Intrinsics.fptrunc Float16 -Inf32 -Inf16
    @test_intrinsic Core.Intrinsics.fptrunc Float16 Inf64 Inf16
    @test_intrinsic Core.Intrinsics.fptrunc Float16 -Inf64 -Inf16

    # LLVM gives us three things that may happen to NaNs in an fptrunc on
    # "normal" platforms (x86, ARM):
    # - Return a canonical NaN (quiet, all-zero payload)
    # - Copy high bits of payload to output, and:
    #   - Set the quiet bit
    #   - Leave the quiet bit as-is.  This option isn't possible if doing so
    #     would result in an infinity (all-zero payload and quiet bit clear)
    #
    # We'll just test a NaN is returned at all.
    #
    # Refer to #49353 and https://llvm.org/docs/LangRef.html#floatnan

    # Canonical NaN
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 NaN32 isnan
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 NaN isnan
    # Quiet NaN
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 f32(0, 0xff, 1<<22 | 1<<13) isnan
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 f64(0, 0x7ff, Int64(1)<<51 | Int64(1)<<42) isnan
    # Signalling NaN that can be propagated to Float16
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 f32(0, 0xff, 1<<13) isnan
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 f64(0, 0x7ff, Int64(1)<<42) isnan
    # Signalling NaN that cannot be propagated to Float16
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 f32(0, 0xff, 1) isnan
    @test_intrinsic_pred Core.Intrinsics.fptrunc Float16 f64(0, 0x7ff, 1) isnan

    # binary
    @test_intrinsic Core.Intrinsics.add_float Float16(3.3) Float16(2) Float16(5.3)
    @test_intrinsic Core.Intrinsics.sub_float Float16(3.3) Float16(2) Float16(1.301)
    @test_intrinsic Core.Intrinsics.mul_float Float16(3.3) Float16(2) Float16(6.6)
    @test_intrinsic Core.Intrinsics.div_float Float16(3.3) Float16(2) Float16(1.65)
    @test_intrinsic Core.Intrinsics.max_float Float16(1.0) Float16(2.0) Float16(2.0)
    @test_intrinsic Core.Intrinsics.min_float Float16(1.0) Float16(2.0) Float16(1.0)

    # ternary
    @test_intrinsic Core.Intrinsics.fma_float Float16(3.3) Float16(4.4) Float16(5.5) Float16(20.02)
    @test_intrinsic Core.Intrinsics.muladd_float Float16(3.3) Float16(4.4) Float16(5.5) Float16(20.02)
    @test_intrinsic Core.Intrinsics.fma_float Float16(0x1.004p0) Float16(1.25) Float16(0x1p-12) Float16(0x1.408p0)
    @test Float16(0x1.004p0)*Float16(1.25)+Float16(0x1p-12) === Float16(0x1.404p0) # for comparison

    # boolean
    @test_intrinsic Core.Intrinsics.eq_float Float16(3.3) Float16(3.3) true
    @test_intrinsic Core.Intrinsics.eq_float Float16(3.3) Float16(2) false
    @test_intrinsic Core.Intrinsics.ne_float Float16(3.3) Float16(3.3) false
    @test_intrinsic Core.Intrinsics.ne_float Float16(3.3) Float16(2) true
    @test_intrinsic Core.Intrinsics.le_float Float16(3.3) Float16(3.3) true
    @test_intrinsic Core.Intrinsics.le_float Float16(3.3) Float16(2) false

    # conversions
    @test_intrinsic Core.Intrinsics.sitofp Float16 3 Float16(3.0)
    @test_intrinsic Core.Intrinsics.uitofp Float16 UInt(3) Float16(3.0)
    @test_intrinsic Core.Intrinsics.fptosi Int Float16(3.3) 3
    @test_intrinsic Core.Intrinsics.fptoui UInt Float16(3.3) UInt(3)
end

@testset "Float16 intrinsics (crt)" begin
    gnu_h2f_ieee(x::Float16) = ccall("julia__gnu_h2f_ieee", Float32, (Float16,), x)
    gnu_f2h_ieee(x::Float32) = ccall("julia__gnu_f2h_ieee", Float16, (Float32,), x)

    @test gnu_h2f_ieee(Float16(3.3)) == 3.3007812f0
    @test gnu_f2h_ieee(3.3f0) == Float16(3.3)
end

using Base.Experimental: @force_compile
@test_throws ConcurrencyViolationError("invalid atomic ordering") (@force_compile; Core.Intrinsics.atomic_fence(:u)) === nothing
@test_throws ConcurrencyViolationError("invalid atomic ordering") (@force_compile; Core.Intrinsics.atomic_fence(Symbol("u", "x"))) === nothing
@test_throws ConcurrencyViolationError("invalid atomic ordering") Core.Intrinsics.atomic_fence(Symbol("u", "x")) === nothing
for order in (:not_atomic, :monotonic, :acquire, :release, :acquire_release, :sequentially_consistent)
    @test Core.Intrinsics.atomic_fence(order) === nothing
    @test (order -> Core.Intrinsics.atomic_fence(order))(order) === nothing
    @test Base.invokelatest(@eval () -> Core.Intrinsics.atomic_fence($(QuoteNode(order)))) === nothing
end
@test Core.Intrinsics.atomic_pointerref(C_NULL, :sequentially_consistent) === nothing
@test (@force_compile; Core.Intrinsics.atomic_pointerref(C_NULL, :sequentially_consistent)) === nothing

primitive type Int256 <: Signed 256 end
Int256(i::Int) = Core.Intrinsics.sext_int(Int256, i)
primitive type Int512 <: Signed 512 end
Int512(i::Int) = Core.Intrinsics.sext_int(Int512, i)
function add(i::T, j)::T where {T}; return i + j; end
swap(i, j) = j

for TT in (Int8, Int16, Int32, Int64, Int128, Int256, Int512, Complex{Int32}, Complex{Int512}, Any)
    r = Ref{TT}(10)
    GC.@preserve r begin
        (@noinline function (::Type{TT}) where TT
            p = Base.unsafe_convert(Ptr{TT}, r)
            T(x) = convert(TT, x)
            S = UInt32
            if TT !== Any
                @test_throws TypeError Core.Intrinsics.atomic_pointerset(p, S(1), :sequentially_consistent)
                @test_throws TypeError Core.Intrinsics.atomic_pointerswap(p, S(2), :sequentially_consistent)
                @test_throws TypeError Core.Intrinsics.atomic_pointerreplace(p, T(10), S(3), :sequentially_consistent, :sequentially_consistent)
            end
            @test Core.Intrinsics.pointerref(p, 1, 1) === T(10) === r[]
            if sizeof(r) > 2*sizeof(Int)
                @test_throws ErrorException("atomic_pointerref: invalid pointer for atomic operation") unsafe_load(p, :sequentially_consistent)
                @test_throws ErrorException("atomic_pointerset: invalid pointer for atomic operation") unsafe_store!(p, T(1), :sequentially_consistent)
                @test_throws ErrorException("atomic_pointerswap: invalid pointer for atomic operation") unsafe_swap!(p, T(100), :sequentially_consistent)
                @test_throws ErrorException("atomic_pointermodify: invalid pointer for atomic operation") unsafe_modify!(p, add, T(1), :sequentially_consistent)
                @test_throws ErrorException("atomic_pointermodify: invalid pointer for atomic operation") unsafe_modify!(p, swap, S(1), :sequentially_consistent)
                @test_throws ErrorException("atomic_pointerreplace: invalid pointer for atomic operation") unsafe_replace!(p, T(100), T(2), :sequentially_consistent, :sequentially_consistent)
                @test_throws ErrorException("atomic_pointerreplace: invalid pointer for atomic operation") unsafe_replace!(p, S(100), T(2), :sequentially_consistent, :sequentially_consistent)
                @test Core.Intrinsics.pointerref(p, 1, 1) === T(10) === r[]
            else
                if TT !== Any
                    @test_throws TypeError Core.Intrinsics.atomic_pointermodify(p, swap, S(1), :sequentially_consistent)
                    @test_throws TypeError Core.Intrinsics.atomic_pointermodify(p, Returns(S(5)), T(10), :sequentially_consistent)
                end
                @test unsafe_load(p, :sequentially_consistent) === T(10)
                @test unsafe_store!(p, T(1), :sequentially_consistent) === p
                @test unsafe_load(p, :sequentially_consistent) === T(1)
                @test unsafe_replace!(p, T(1), T(100), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(1), true))
                @test unsafe_load(p, :sequentially_consistent) === T(100)
                @test unsafe_replace!(p, T(1), T(1), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(100), false))
                @test unsafe_load(p, :sequentially_consistent) === T(100)
                @test unsafe_modify!(p, add, T(1), :sequentially_consistent) === Pair{TT,TT}(T(100), T(101))
                @test unsafe_modify!(p, add, T(1), :sequentially_consistent) === Pair{TT,TT}(T(101), T(102))
                @test unsafe_load(p, :sequentially_consistent) === T(102)
                @test unsafe_swap!(p, T(103), :sequentially_consistent) === T(102)
                @test unsafe_replace!(p, S(100), T(2), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(103), false))
                @test unsafe_load(p, :sequentially_consistent) === T(103)
                @test unsafe_modify!(p, Returns(T(105)), nothing, :sequentially_consistent) === Pair{TT,TT}(T(103), T(105))
                @test unsafe_load(p, :sequentially_consistent) === T(105)
            end
            if TT === Any
                @test unsafe_modify!(p, swap, S(105), :sequentially_consistent) === Pair{TT,TT}(T(105), S(105))
                @test unsafe_load(p, :sequentially_consistent) === S(105)
                @test unsafe_store!(p, S(1), :sequentially_consistent) === p
                @test unsafe_swap!(p, S(100), :sequentially_consistent) === S(1)
                @test unsafe_replace!(p, T(100), S(2), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((S(100), false))
                @test unsafe_replace!(p, S(100), T(2), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((S(100), true))
                @test unsafe_load(p, :sequentially_consistent) === T(2)
            end
        end)(TT,)
    end
end

for TT in (Ptr{Nothing}, Ptr)
    r = Ref(nothing)
    GC.@preserve r begin
        p = Ref{TT}(Base.unsafe_convert(Ptr{Nothing}, r))
        (@noinline function (p::Ref)
            p = p[]
            S = UInt32
            @test_throws TypeError Core.Intrinsics.atomic_pointerset(p, S(1), :sequentially_consistent)
            @test_throws TypeError Core.Intrinsics.atomic_pointerswap(p, S(100), :sequentially_consistent)
            @test_throws TypeError Core.Intrinsics.atomic_pointerreplace(p, nothing, S(2), :sequentially_consistent, :sequentially_consistent)
            @test Core.Intrinsics.pointerref(p, 1, 1) === nothing === r[]
            @test_throws TypeError Core.Intrinsics.atomic_pointermodify(p, swap, S(1), :sequentially_consistent)
            @test_throws TypeError Core.Intrinsics.atomic_pointermodify(p, Returns(S(1)), nothing, :sequentially_consistent)
            @test Core.Intrinsics.atomic_pointerref(p, :sequentially_consistent) === nothing
            @test Core.Intrinsics.atomic_pointerset(p, nothing, :sequentially_consistent) === p
            @test Core.Intrinsics.atomic_pointerref(p, :sequentially_consistent) === nothing
            @test Core.Intrinsics.atomic_pointerreplace(p, nothing, nothing, :sequentially_consistent, :sequentially_consistent) === ReplaceType{Nothing}((nothing, true))
            @test Core.Intrinsics.atomic_pointerref(p, :sequentially_consistent) === nothing
            @test Core.Intrinsics.atomic_pointerreplace(p, S(1), nothing, :sequentially_consistent, :sequentially_consistent) === ReplaceType{Nothing}((nothing, false))
            @test Core.Intrinsics.atomic_pointerref(p, :sequentially_consistent) === nothing
            @test Core.Intrinsics.atomic_pointermodify(p, Returns(nothing), nothing, :sequentially_consistent) === Pair{Nothing,Nothing}(nothing, nothing)
            @test Core.Intrinsics.atomic_pointermodify(p, Returns(nothing), S(1), :sequentially_consistent) === Pair{Nothing,Nothing}(nothing, nothing)
            @test Core.Intrinsics.atomic_pointerref(p, :sequentially_consistent) === nothing
            @test Core.Intrinsics.atomic_pointerswap(p, nothing, :sequentially_consistent) === nothing
            @test Core.Intrinsics.atomic_pointerreplace(p, S(100), nothing, :sequentially_consistent, :sequentially_consistent) === ReplaceType{Nothing}((nothing, false))
            @test Core.Intrinsics.atomic_pointerref(p, :sequentially_consistent) === nothing
        end)(p,)
    end
end


mutable struct IntWrap <: Signed
    x::Int
end
Base.:(+)(a::IntWrap, b::Int) = IntWrap(a.x + b)
Base.:(+)(a::IntWrap, b::IntWrap) = IntWrap(a.x + b.x)
Base.show(io::IO, a::IntWrap) = print(io, "IntWrap(", a.x, ")")
(function()
    TT = IntWrap
    T(x) = convert(TT, x)
    r = Ref{TT}(10)
    p = Base.unsafe_convert(Ptr{TT}, r)
    GC.@preserve r begin
        S = UInt32
        @test_throws TypeError Core.Intrinsics.atomic_pointerset(p, S(1), :sequentially_consistent)
        @test_throws TypeError Core.Intrinsics.atomic_pointerswap(p, S(100), :sequentially_consistent)
        @test_throws TypeError Core.Intrinsics.atomic_pointerreplace(p, T(100), S(2), :sequentially_consistent, :sequentially_consistent)
        r2 = unsafe_load(p, 1)
        @test r2 isa IntWrap && r2.x === 10 === r[].x && r2 !== r[]
        @test_throws TypeError Core.Intrinsics.atomic_pointermodify(p, swap, S(1), :sequentially_consistent)
        r2 = unsafe_load(p, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 10 === r[].x && r2 !== r[]
        @test unsafe_store!(p, T(1), :sequentially_consistent) === p
        r2 = unsafe_load(p, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 1 === r[].x && r2 !== r[]
        r2, succ = unsafe_replace!(p, T(1), T(100), :sequentially_consistent, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 1 && r[].x === 100 && r2 !== r[]
        @test succ
        r2 = unsafe_load(p, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 100 === r[].x && r2 !== r[]
        r2, succ = unsafe_replace!(p, T(1), T(1), :sequentially_consistent, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 100 === r[].x && r2 !== r[]
        @test !succ
        r2 = unsafe_load(p, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 100 === r[].x && r2 !== r[]
        r2, r3 = unsafe_modify!(p, add, T(1), :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 100 !== r[].x && r2 !== r[]
        @test r3 isa IntWrap && r3.x === 101 === r[].x && r3 !== r[]
        r2, r3 = unsafe_modify!(p, add, T(1), :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 101 !== r[].x && r2 !== r[]
        @test r3 isa IntWrap && r3.x === 102 === r[].x && r3 !== r[]
        r2 = unsafe_load(p, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 102 === r[].x && r2 !== r[]
        r2 = unsafe_swap!(p, T(103), :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 102 !== r[].x && r[].x == 103 && r2 !== r[]
        r2, succ = unsafe_replace!(p, S(100), T(2), :sequentially_consistent, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 103 === r[].x && r2 !== r[]
        @test !succ
        r2 = unsafe_load(p, :sequentially_consistent)
        @test r2 isa IntWrap && r2.x === 103 === r[].x && r2 !== r[]
    end
end)()

@testset "issue #54548" begin
    @inline passthrough(ptr::Core.LLVMPtr{T,A}) where {T,A} = Base.llvmcall(("""
            define ptr addrspace(1) @entry(ptr addrspace(1) %0) #0 {
            entry:
                ret ptr addrspace(1) %0
            }

            attributes #0 = { alwaysinline }""", "entry"),
        Core.LLVMPtr{T,A}, Tuple{Core.LLVMPtr{T,A}}, ptr)
    f(gws) = passthrough(Core.bitcast(Core.LLVMPtr{UInt32,1}, gws))
    f(C_NULL)
end
