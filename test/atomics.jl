# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Base.Threads
using Core: ConcurrencyViolationError
import Base: copy

const ReplaceType = ccall(:jl_apply_cmpswap_type, Any, (Any,), T) where T

mutable struct ARefxy{T}
    @atomic x::T
    y::T
    ARefxy(x::T, y::T) where {T} = new{T}(x, y)
    ARefxy{T}(x, y) where {T} = new{T}(x, y)
    ARefxy{T}() where {T} = new{T}()
end

mutable struct Refxy{T}
    x::T
    y::T
    Refxy(x::T, y::T) where {T} = new{T}(x, y)
    Refxy{T}(x, y) where {T} = new{T}(x, y)
    Refxy{T}() where {T} = new() # unused, but sets ninitialized to 0
end

modname = String(nameof(@__MODULE__))
const orig_Refxy = Refxy
const orig_ARefxy = ARefxy
mutable struct ARefxy{T}
    @atomic x::T
    @atomic y::T
end
@test orig_ARefxy !== ARefxy
const ARefxy = orig_ARefxy
mutable struct ARefxy{T}
    x::T
    y::T
end
@test orig_ARefxy !== ARefxy
const ARefxy = orig_ARefxy
mutable struct ARefxy{T}
    x::T
    @atomic y::T
end
@test orig_ARefxy !== ARefxy
const ARefxy = orig_ARefxy
mutable struct Refxy{T}
    x::T
    @atomic y::T
end
@test orig_Refxy !== Refxy
const Refxy = orig_Refxy

copy(r::Union{Refxy,ARefxy}) = typeof(r)(r.x, r.y)
function add(x::T, y)::T where {T}; x + y; end
swap(x, y) = y

struct UndefComplex{T}
    re::T
    im::T
    UndefComplex{T}() where {T} = new{T}()
end
Base.convert(T::Type{<:UndefComplex}, S) = T()

let T1 = Refxy{NTuple{3,UInt8}},
    T2 = ARefxy{NTuple{3,UInt8}}
    @test sizeof(T1) == 6
    @test sizeof(T2) == 8
    @test fieldoffset(T1, 1) == 0
    @test fieldoffset(T2, 1) == 0
    @test fieldoffset(T1, 2) == 3
    @test fieldoffset(T2, 2) == 4
    @test !Base.datatype_haspadding(T1)
    @test Base.datatype_haspadding(T2)
    @test Base.datatype_alignment(T1) == 1
    @test Base.datatype_alignment(T2) == 4
end

# check that very large types are getting locks
let (x, y) = (Complex{Int128}(10, 30), Complex{Int128}(20, 40))
    r = Refxy(x, y)
    ar = ARefxy(x, y)
    mr = AtomicMemory{Pair{typeof(x),typeof(y)}}(undef, 20)
    @test 64 == sizeof(r) < sizeof(ar)
    @test sizeof(ar) == sizeof(r) + Int(fieldoffset(typeof(ar), 1))
    @test_broken Base.elsize(mr) == sizeof(ar)
    @test sizeof(mr) == length(mr) * (sizeof(r) + 16)
end

struct PadIntA <: Number # internal padding
    a::Int8
    b::Int16
    PadIntA(x) = new(82, x)
end
struct PadIntB <: Number # external padding
    a::UInt8
    b::UInt8
    c::UInt8
    PadIntB(x) = new(x & 0xff, (x >> 8) & 0xff, (x >> 16) & 0xff)
end
primitive type Int24 <: Signed 24 end # integral padding
Int24(x::Int) = Core.Intrinsics.trunc_int(Int24, x)
Base.Int(x::PadIntB) = x.a + (Int(x.b) << 8) + (Int(x.c) << 16)
Base.:(+)(x::PadIntA, b::Int) = PadIntA(x.b + b)
Base.:(==)(x::PadIntA, b::Int) = x == PadIntA(b)
Base.:(+)(x::PadIntB, b::Int) = PadIntB(Int(x) + b)
Base.:(+)(x::Int24, b::Int) = Core.Intrinsics.add_int(x, Int24(b))
Base.show(io::IO, x::PadIntA) = print(io, "PadIntA(", x.b, ")")
Base.show(io::IO, x::PadIntB) = print(io, "PadIntB(", Int(x), ")")
Base.show(io::IO, x::Int24) = print(io, "Int24(", Core.Intrinsics.zext_int(Int, x), ")")

## Fields

@noinline function _test_field_operators(r)
    r = r[]
    TT = fieldtype(typeof(r), :x)
    T = typeof(getfield(r, :x))
    @test getfield(r, :x, :sequentially_consistent) === T(123_10)
    @test setfield!(r, :x, T(123_1), :sequentially_consistent) === T(123_1)
    @test getfield(r, :x, :sequentially_consistent) === T(123_1)
    @test replacefield!(r, :x, 123_1 % UInt, T(123_30), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(123_1), false))
    @test replacefield!(r, :x, T(123_1), T(123_30), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(123_1), true))
    @test getfield(r, :x, :sequentially_consistent) === T(123_30)
    @test replacefield!(r, :x, T(123_1), T(123_1), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(123_30), false))
    @test getfield(r, :x, :sequentially_consistent) === T(123_30)
    @test modifyfield!(r, :x, add, 1, :sequentially_consistent) === Pair{TT,TT}(T(123_30), T(123_31))
    @test modifyfield!(r, :x, add, 1, :sequentially_consistent) === Pair{TT,TT}(T(123_31), T(123_32))
    @test getfield(r, :x, :sequentially_consistent) === T(123_32)
    @test swapfield!(r, :x, T(123_1), :sequentially_consistent) === T(123_32)
    @test getfield(r, :x, :sequentially_consistent) === T(123_1)
    nothing
end
@noinline function test_field_operators(r)
    _test_field_operators(Ref(copy(r)))
    _test_field_operators(Ref{Any}(copy(r)))
    nothing
end
test_field_operators(ARefxy{Int}(123_10, 123_20))
test_field_operators(ARefxy{Any}(123_10, 123_20))
test_field_operators(ARefxy{Union{Nothing,Int}}(123_10, nothing))
test_field_operators(ARefxy{Complex{Int32}}(123_10, 123_20))
test_field_operators(ARefxy{Complex{Int128}}(123_10, 123_20))
test_field_operators(ARefxy{Complex{Real}}(123_10, 123_20))
test_field_operators(ARefxy{PadIntA}(123_10, 123_20))
test_field_operators(ARefxy{PadIntB}(123_10, 123_20))
#FIXME: test_field_operators(ARefxy{Int24}(123_10, 123_20))
test_field_operators(ARefxy{Float64}(123_10, 123_20))

@noinline function _test_field_orderings(r, x, y)
    @nospecialize x y
    r = r[]
    TT = fieldtype(typeof(r), :x)

    @test getfield(r, :x) === x
    @test_throws ConcurrencyViolationError("invalid atomic ordering") getfield(r, :x, :u)
    @test_throws ConcurrencyViolationError("getfield: atomic field cannot be accessed non-atomically") getfield(r, :x, :not_atomic)
    @test getfield(r, :x, :unordered) === x
    @test getfield(r, :x, :monotonic) === x
    @test getfield(r, :x, :acquire) === x
    @test_throws ConcurrencyViolationError("invalid atomic ordering") getfield(r, :x, :release) === x
    @test_throws ConcurrencyViolationError("invalid atomic ordering") getfield(r, :x, :acquire_release) === x
    @test getfield(r, :x, :sequentially_consistent) === x
    @test isdefined(r, :x)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined(r, :x, :u)
    @test_throws ConcurrencyViolationError("isdefined: atomic field cannot be accessed non-atomically") isdefined(r, :x, :not_atomic)
    @test isdefined(r, :x, :unordered)
    @test isdefined(r, :x, :monotonic)
    @test isdefined(r, :x, :acquire)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined(r, :x, :release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined(r, :x, :acquire_release)
    @test isdefined(r, :x, :sequentially_consistent)

    @test getfield(r, :y) === y
    @test_throws ConcurrencyViolationError("invalid atomic ordering") getfield(r, :y, :u)
    @test getfield(r, :y, :not_atomic) === y
    @test_throws ConcurrencyViolationError("getfield: non-atomic field cannot be accessed atomically") getfield(r, :y, :unordered)
    @test_throws ConcurrencyViolationError("getfield: non-atomic field cannot be accessed atomically") getfield(r, :y, :monotonic)
    @test_throws ConcurrencyViolationError("getfield: non-atomic field cannot be accessed atomically") getfield(r, :y, :acquire)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") getfield(r, :y, :release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") getfield(r, :y, :acquire_release)
    @test_throws ConcurrencyViolationError("getfield: non-atomic field cannot be accessed atomically") getfield(r, :y, :sequentially_consistent)
    @test isdefined(r, :y)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined(r, :y, :u)
    @test isdefined(r, :y, :not_atomic)
    @test_throws ConcurrencyViolationError("isdefined: non-atomic field cannot be accessed atomically") isdefined(r, :y, :unordered)
    @test_throws ConcurrencyViolationError("isdefined: non-atomic field cannot be accessed atomically") isdefined(r, :y, :monotonic)
    @test_throws ConcurrencyViolationError("isdefined: non-atomic field cannot be accessed atomically") isdefined(r, :y, :acquire)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined(r, :y, :release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined(r, :y, :acquire_release)
    @test_throws ConcurrencyViolationError("isdefined: non-atomic field cannot be accessed atomically") isdefined(r, :y, :sequentially_consistent)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfield!(r, :x, y, :u)
    @test_throws ConcurrencyViolationError("setfield!: atomic field cannot be written non-atomically") setfield!(r, :x, y)
    @test_throws ConcurrencyViolationError("setfield!: atomic field cannot be written non-atomically") setfield!(r, :x, y, :not_atomic)
    @test getfield(r, :x) === x
    @test setfield!(r, :x, y, :unordered) === y
    @test setfield!(r, :x, y, :monotonic) === y
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfield!(r, :x, y, :acquire) === y
    @test setfield!(r, :x, y, :release) === y
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfield!(r, :x, y, :acquire_release) === y
    @test setfield!(r, :x, y, :sequentially_consistent) === y
    @test getfield(r, :x) === y

    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfield!(r, :y, x, :u)
    @test_throws ConcurrencyViolationError("setfield!: non-atomic field cannot be written atomically") setfield!(r, :y, x, :unordered)
    @test_throws ConcurrencyViolationError("setfield!: non-atomic field cannot be written atomically") setfield!(r, :y, x, :monotonic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfield!(r, :y, x, :acquire)
    @test_throws ConcurrencyViolationError("setfield!: non-atomic field cannot be written atomically") setfield!(r, :y, x, :release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfield!(r, :y, x, :acquire_release)
    @test_throws ConcurrencyViolationError("setfield!: non-atomic field cannot be written atomically") setfield!(r, :y, x, :sequentially_consistent)
    @test getfield(r, :y) === y
    @test setfield!(r, :y, x) === x
    @test setfield!(r, :y, x, :not_atomic) === x
    @test getfield(r, :y) === x

    @test_throws ConcurrencyViolationError("invalid atomic ordering") swapfield!(r, :y, y, :u)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") swapfield!(r, :y, y, :unordered)
    @test_throws ConcurrencyViolationError("swapfield!: non-atomic field cannot be written atomically") swapfield!(r, :y, y, :monotonic)
    @test_throws ConcurrencyViolationError("swapfield!: non-atomic field cannot be written atomically") swapfield!(r, :y, y, :acquire)
    @test_throws ConcurrencyViolationError("swapfield!: non-atomic field cannot be written atomically") swapfield!(r, :y, y, :release)
    @test_throws ConcurrencyViolationError("swapfield!: non-atomic field cannot be written atomically") swapfield!(r, :y, y, :acquire_release)
    @test_throws ConcurrencyViolationError("swapfield!: non-atomic field cannot be written atomically") swapfield!(r, :y, y, :sequentially_consistent)
    @test swapfield!(r, :y, y, :not_atomic) === x

    @test_throws ConcurrencyViolationError("invalid atomic ordering") modifyfield!(r, :y, swap, y, :u)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") modifyfield!(r, :y, swap, y, :unordered)
    @test_throws ConcurrencyViolationError("modifyfield!: non-atomic field cannot be written atomically") modifyfield!(r, :y, swap, y, :monotonic)
    @test_throws ConcurrencyViolationError("modifyfield!: non-atomic field cannot be written atomically") modifyfield!(r, :y, swap, y, :acquire)
    @test_throws ConcurrencyViolationError("modifyfield!: non-atomic field cannot be written atomically") modifyfield!(r, :y, swap, y, :release)
    @test_throws ConcurrencyViolationError("modifyfield!: non-atomic field cannot be written atomically") modifyfield!(r, :y, swap, y, :acquire_release)
    @test_throws ConcurrencyViolationError("modifyfield!: non-atomic field cannot be written atomically") modifyfield!(r, :y, swap, y, :sequentially_consistent)
    @test modifyfield!(r, :y, swap, x, :not_atomic) === Pair{TT,TT}(y, x)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :u, :not_atomic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :unordered, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: non-atomic field cannot be written atomically") replacefield!(r, :y, y, y, :monotonic, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: non-atomic field cannot be written atomically") replacefield!(r, :y, y, y, :acquire, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: non-atomic field cannot be written atomically") replacefield!(r, :y, y, y, :release, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: non-atomic field cannot be written atomically") replacefield!(r, :y, y, y, :acquire_release, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: non-atomic field cannot be written atomically") replacefield!(r, :y, y, y, :sequentially_consistent, :not_atomic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :not_atomic, :u)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :not_atomic, :unordered)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :not_atomic, :monotonic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :not_atomic, :acquire)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :not_atomic, :release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :not_atomic, :acquire_release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :y, y, y, :not_atomic, :sequentially_consistent)
    @test replacefield!(r, :y, x, y, :not_atomic, :not_atomic) === ReplaceType{TT}((x, true))
    @test replacefield!(r, :y, x, y, :not_atomic, :not_atomic) === ReplaceType{TT}((y, x === y))
    @test replacefield!(r, :y, y, y, :not_atomic) === ReplaceType{TT}((y, true))
    @test replacefield!(r, :y, y, y) === ReplaceType{TT}((y, true))

    @test_throws ConcurrencyViolationError("invalid atomic ordering") swapfield!(r, :x, x, :u)
    @test_throws ConcurrencyViolationError("swapfield!: atomic field cannot be written non-atomically") swapfield!(r, :x, x, :not_atomic)
    @test_throws ConcurrencyViolationError("swapfield!: atomic field cannot be written non-atomically") swapfield!(r, :x, x)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") swapfield!(r, :x, x, :unordered) === y
    @test swapfield!(r, :x, x, :monotonic) === y
    @test swapfield!(r, :x, x, :acquire) === x
    @test swapfield!(r, :x, x, :release) === x
    @test swapfield!(r, :x, x, :acquire_release) === x
    @test swapfield!(r, :x, x, :sequentially_consistent) === x

    @test_throws ConcurrencyViolationError("invalid atomic ordering") modifyfield!(r, :x, swap, x, :u)
    @test_throws ConcurrencyViolationError("modifyfield!: atomic field cannot be written non-atomically") modifyfield!(r, :x, swap, x, :not_atomic)
    @test_throws ConcurrencyViolationError("modifyfield!: atomic field cannot be written non-atomically") modifyfield!(r, :x, swap, x)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") modifyfield!(r, :x, swap, x, :unordered)
    @test modifyfield!(r, :x, swap, x, :monotonic) === Pair{TT,TT}(x, x)
    @test modifyfield!(r, :x, swap, x, :acquire) === Pair{TT,TT}(x, x)
    @test modifyfield!(r, :x, swap, x, :release) === Pair{TT,TT}(x, x)
    @test modifyfield!(r, :x, swap, x, :acquire_release) === Pair{TT,TT}(x, x)
    @test modifyfield!(r, :x, swap, x, :sequentially_consistent) === Pair{TT,TT}(x, x)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :u, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: atomic field cannot be written non-atomically") replacefield!(r, :x, x, x)
    @test_throws ConcurrencyViolationError("replacefield!: atomic field cannot be written non-atomically") replacefield!(r, :x, y, x, :not_atomic, :not_atomic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :unordered, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: atomic field cannot be accessed non-atomically") replacefield!(r, :x, x, x, :monotonic, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: atomic field cannot be accessed non-atomically") replacefield!(r, :x, x, x, :acquire, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: atomic field cannot be accessed non-atomically") replacefield!(r, :x, x, x, :release, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: atomic field cannot be accessed non-atomically") replacefield!(r, :x, x, x, :acquire_release, :not_atomic)
    @test_throws ConcurrencyViolationError("replacefield!: atomic field cannot be accessed non-atomically") replacefield!(r, :x, x, x, :sequentially_consistent, :not_atomic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :not_atomic, :u)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :not_atomic, :unordered)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :not_atomic, :monotonic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :not_atomic, :acquire)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :not_atomic, :release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :not_atomic, :acquire_release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") replacefield!(r, :x, x, x, :not_atomic, :sequentially_consistent)
    @test replacefield!(r, :x, x, y, :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((x, true))
    @test replacefield!(r, :x, x, y, :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((y, x === y))
    @test replacefield!(r, :x, y, x, :sequentially_consistent) === ReplaceType{TT}((y, true))

    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :u, :not_atomic)
    @test_throws ConcurrencyViolationError("setfieldonce!: atomic field cannot be written non-atomically") setfieldonce!(r, :x, x)
    @test_throws ConcurrencyViolationError("setfieldonce!: atomic field cannot be written non-atomically") setfieldonce!(r, :x, y, :not_atomic, :not_atomic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x,  x, :unordered, :not_atomic)
    @test_throws ConcurrencyViolationError("setfieldonce!: atomic field cannot be accessed non-atomically") setfieldonce!(r, :x, x, :monotonic, :not_atomic)
    @test_throws ConcurrencyViolationError("setfieldonce!: atomic field cannot be accessed non-atomically") setfieldonce!(r, :x, x, :acquire, :not_atomic)
    @test_throws ConcurrencyViolationError("setfieldonce!: atomic field cannot be accessed non-atomically") setfieldonce!(r, :x, x, :release, :not_atomic)
    @test_throws ConcurrencyViolationError("setfieldonce!: atomic field cannot be accessed non-atomically") setfieldonce!(r, :x, x, :acquire_release, :not_atomic)
    @test_throws ConcurrencyViolationError("setfieldonce!: atomic field cannot be accessed non-atomically") setfieldonce!(r, :x, x, :sequentially_consistent, :not_atomic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :not_atomic, :u)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :not_atomic, :unordered)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :not_atomic, :monotonic)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :not_atomic, :acquire)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :not_atomic, :release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :not_atomic, :acquire_release)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") setfieldonce!(r, :x, x, :not_atomic, :sequentially_consistent)
    @test setfieldonce!(r, :x, y, :sequentially_consistent, :sequentially_consistent) === false
    @test setfieldonce!(r, :x, y, :sequentially_consistent, :sequentially_consistent) === false
    @test setfieldonce!(r, :x, x, :sequentially_consistent) === false
    nothing
end
@noinline function test_field_orderings(r, x, y)
    @testset "$r" begin
        _test_field_orderings(Ref(copy(r)), x, y)
        _test_field_orderings(Ref{Any}(copy(r)), x, y)
    end
    nothing
end
@noinline test_field_orderings(x, y) = (@nospecialize; test_field_orderings(ARefxy(x, y), x, y))
test_field_orderings(10, 20)
test_field_orderings(true, false)
test_field_orderings("hi", "bye")
test_field_orderings(:hi, :bye)
test_field_orderings(nothing, nothing)
test_field_orderings(ARefxy{Any}(123_10, 123_20), 123_10, 123_20)
test_field_orderings(ARefxy{Any}(true, false), true, false)
test_field_orderings(ARefxy{Union{Nothing,Missing}}(nothing, missing), nothing, missing)
test_field_orderings(ARefxy{Union{Nothing,Int}}(nothing, 123_1), nothing, 123_1)
test_field_orderings(Complex{Int128}(10, 30), Complex{Int128}(20, 40))
test_field_orderings(Complex{Real}(10, 30), Complex{Real}(20, 40))
test_field_orderings(Complex{Rational{Integer}}(10, 30), Complex{Rational{Integer}}(20, 40))
test_field_orderings(10.0, 20.0)
test_field_orderings(NaN, Inf)

@noinline function _test_field_undef(r)
    r = r[]
    TT = fieldtype(typeof(r), :x)
    x = convert(TT, 12345_10)
    @test_throws UndefRefError getfield(r, :x)
    @test_throws UndefRefError getfield(r, :x, :sequentially_consistent)
    @test_throws UndefRefError modifyfield!(r, :x, add, 1, :sequentially_consistent)
    @test_throws (TT === Any ? UndefRefError : TypeError) replacefield!(r, :x, 1, 1.0, :sequentially_consistent)
    @test_throws UndefRefError replacefield!(r, :x, 1, x, :sequentially_consistent)
    @test_throws UndefRefError getfield(r, :x, :sequentially_consistent)
    @test_throws UndefRefError swapfield!(r, :x, x, :sequentially_consistent)
    @test getfield(r, :x, :sequentially_consistent) === x === getfield(r, :x)
    nothing
end
@noinline function test_field_undef(TT)
    _test_field_undef(Ref(TT()))
    _test_field_undef(Ref{Any}(TT()))
    nothing
end
test_field_undef(ARefxy{BigInt})
test_field_undef(ARefxy{Any})
test_field_undef(ARefxy{Union{Nothing,Integer}})
test_field_undef(ARefxy{UndefComplex{Any}})
test_field_undef(ARefxy{UndefComplex{UndefComplex{Any}}})

@noinline function _test_once_undef(r)
    r = r[]
    TT = fieldtype(typeof(r), :x)
    x = convert(TT, 12345_10)
    @test_throws UndefRefError getfield(r, :x)
    @test setfieldonce!(r, :x, x, :sequentially_consistent) === true
    @test getfield(r, :x, :sequentially_consistent) === x
    @test setfieldonce!(r, :x, convert(TT, 12345_20), :sequentially_consistent) === false
    nothing
end

@noinline function test_once_undef(TT)
    _test_once_undef(Ref(TT()))
    _test_once_undef(Ref{Any}(TT()))
    nothing
end

test_once_undef(ARefxy{BigInt})
test_once_undef(ARefxy{Any})
test_once_undef(ARefxy{Union{Nothing,Integer}})
test_once_undef(ARefxy{UndefComplex{Any}})
test_once_undef(ARefxy{UndefComplex{UndefComplex{Any}}})

@test_throws ErrorException @macroexpand @atomic foo()
@test_throws ErrorException @macroexpand @atomic foo += bar
@test_throws ErrorException @macroexpand @atomic foo += bar
@test_throws ErrorException @macroexpand @atomic foo = bar
@test_throws ErrorException @macroexpand @atomic foo()
@test_throws ErrorException @macroexpand @atomic foo(bar)
@test_throws ErrorException @macroexpand @atomic foo(bar, baz)
@test_throws ErrorException @macroexpand @atomic foo(bar, baz, bax)
@test_throws ErrorException @macroexpand @atomicreplace foo bar

# test macroexpansions
let a = ARefxy(1, -1)
    @test 1 === @atomic a.x
    @test 2 === @atomic :sequentially_consistent a.x = 2
    @test 3 === @atomic :monotonic a.x = 3
    local four = 4
    @test 4 === @atomic :monotonic a.x = four
    @test 3 === @atomic :monotonic a.x = four - 1
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x = 2
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x += 1

    @test 3 === @atomic :monotonic a.x
    @test 5 === @atomic a.x += 2
    @test 4 === @atomic :monotonic a.x -= 1
    @test 12 === @atomic :monotonic a.x *= 3

    @test 12 === @atomic a.x
    @test (12 => 13) === @atomic a.x + 1
    @test (13 => 15) === @atomic :monotonic a.x + 2
    @test (15 => 19) === @atomic a.x max 19
    @test (19 => 20) === @atomic :monotonic a.x max 20
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x + 1
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x max 30

    @test 20 === @atomic a.x
    @test 20 === @atomicswap a.x = 1
    @test 1 === @atomicswap :monotonic a.x = 2
    @test_throws ConcurrencyViolationError @atomicswap :not_atomic a.x = 1

    @test 2 === @atomic a.x
    @test ReplaceType{Int}((2, true)) === @atomicreplace a.x 2 => 1
    @test ReplaceType{Int}((1, false)) === @atomicreplace :monotonic a.x 2 => 1
    @test ReplaceType{Int}((1, false)) === @atomicreplace :monotonic :monotonic a.x 2 => 1
    @test_throws ConcurrencyViolationError @atomicreplace :not_atomic a.x 1 => 2
    @test_throws ConcurrencyViolationError @atomicreplace :monotonic :acquire a.x 1 => 2

    @test 1 === @atomic a.x
    xchg = 1 => 2
    @test ReplaceType{Int}((1, true)) === @atomicreplace a.x xchg
    @test ReplaceType{Int}((2, false)) === @atomicreplace :monotonic a.x xchg
    @test ReplaceType{Int}((2, false)) === @atomicreplace :acquire_release :monotonic a.x xchg
    @test_throws ConcurrencyViolationError @atomicreplace :not_atomic a.x xchg
    @test_throws ConcurrencyViolationError @atomicreplace :monotonic :acquire a.x xchg
end

function _test_atomic_get_set_swap_modify(T, x, y, z)
    @testset "atomic get,set,swap,modify" begin
        mem = AtomicMemory{T}(undef, 2)
        @test_throws CanonicalIndexError mem[1] = 3

        @test Base.setindex_atomic!(mem, Base.default_access_order(mem), x, 1) == x
        @test mem[1] == x
        @test Base.setindex_atomic!(mem, Base.default_access_order(mem), y, 2) == y
        @test mem[2] == y

        idx = UInt32(2)

        @test (@atomic mem[1]) == x
        @test (@atomic mem[idx]) == y

        (old, new) = (mem[idx], z)
        # old and new are intentionally of different types to test inner conversion
        @test (@atomic mem[idx] = new) == new
        @test mem[idx] == new
        @atomic mem[idx] = old

        @test (@atomicswap mem[idx] = new) == old
        @test mem[idx] == new
        @atomic mem[idx] = old

        try
            old + new
            @test (@atomic mem[idx] += new) == old + new
            @test mem[idx] == old + new
            @atomic mem[idx] = old
        catch err
            if !(err isa MethodError)
                rethrow(err)
            end
        end
    end
end

function _test_atomic_setonce_replace(T, initial, desired)
    @testset "atomic setonce,replace" begin
        mem = AtomicMemory{T}(undef, 2)
        if isassigned(mem, 2)
            @test (@atomiconce mem[2] = initial) == false
            @atomic mem[2] = initial
        else
            @test (@atomiconce mem[2] = initial) == true
            @test mem[2] == initial
            @test (@atomiconce mem[2] = desired) == false
            @test mem[2] == initial
            @test !isassigned(mem, 1)
        end

        idx = UInt(2)

        expected = @atomic mem[idx]
        @test (@atomicreplace mem[idx] expected => desired) == (old=expected, success=true)
        @test mem[idx] == desired

        @atomic mem[idx] = expected
        @test (@atomicreplace mem[idx] desired => desired) == (old=expected, success=false)
        @test mem[idx] == expected

        @atomic mem[idx] = expected
        @test (@atomicreplace mem[idx] Pair(expected, desired)) == (old=expected, success=true)
        @test mem[idx] == desired

        @atomic mem[idx] = expected
        @test (@atomicreplace mem[idx] Pair(desired, desired)) == (old=initial, success=false)
        @test mem[idx] == expected
    end
end
@testset "@atomic with AtomicMemory" begin

    _test_atomic_get_set_swap_modify(Float64, rand(), rand(), 10)
    _test_atomic_get_set_swap_modify(PadIntA, 123_1, 123_2, 10)
    _test_atomic_get_set_swap_modify(Union{Nothing,Int}, 123_1, nothing, 10)
    _test_atomic_get_set_swap_modify(Union{Nothing,Int}, 123_1, 234_5, 10)
    _test_atomic_get_set_swap_modify(Vector{BigInt}, BigInt[1, 2, 3], BigInt[1, 2], [2, 4])

    _test_atomic_setonce_replace(Float64, rand(), 42)
    _test_atomic_setonce_replace(PadIntA, 123_1, 123_2)
    _test_atomic_setonce_replace(Union{Nothing,Int}, 123_1, nothing)
    _test_atomic_setonce_replace(Vector{BigInt}, BigInt[1, 2], [3, 4])
    _test_atomic_setonce_replace(String, "abc", "cab")
end

let a = ARefxy{Union{Nothing,Integer}}()
    @test_throws ConcurrencyViolationError @atomiconce :not_atomic a.x = 2
    @test true === @atomiconce a.x = 1
    @test 1 === @atomic a.x
    @test false === @atomiconce a.x = 2
end

# atomic getfield with boundcheck
# via codegen
getx(a, boundcheck) = getfield(a, :x, :sequentially_consistent, boundcheck)
@test getx(ARefxy{Any}(42, 42), true) == 42
@test getx(ARefxy{Any}(42, 42), false) == 42
# via interpreter
ans = getfield(ARefxy{Any}(42, 42), :x, :sequentially_consistent, true)
@test ans == 42
ans = getfield(ARefxy{Any}(42, 42), :x, :sequentially_consistent, false)
@test ans == 42


## Globals

# the optimizer is terrible at handling PhiC nodes, so this must be a function
# generator with a custom inlining here of r, instead of being able to assume
# the inlining pass can inline a constant value correctly
function gen_test_global_operators(@nospecialize r)
    M = @__MODULE__
    return quote
        TT = Core.get_binding_type($M, $r)
        T = typeof(getglobal($M, $r))
        @test getglobal($M, $r, :sequentially_consistent) === T(123_10)
        @test setglobal!($M, $r, T(123_1), :sequentially_consistent) === T(123_1)
        @test getglobal($M, $r, :sequentially_consistent) === T(123_1)
        @test replaceglobal!($M, $r, 123_1 % UInt, T(123_30), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(123_1), false))
        @test replaceglobal!($M, $r, T(123_1), T(123_30), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(123_1), true))
        @test getglobal($M, $r, :sequentially_consistent) === T(123_30)
        @test replaceglobal!($M, $r, T(123_1), T(123_1), :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((T(123_30), false))
        @test getglobal($M, $r, :sequentially_consistent) === T(123_30)
        @test modifyglobal!($M, $r, add, 1, :sequentially_consistent) === Pair{TT,TT}(T(123_30), T(123_31))
        @test modifyglobal!($M, $r, add, 1, :sequentially_consistent) === Pair{TT,TT}(T(123_31), T(123_32))
        @test getglobal($M, $r, :sequentially_consistent) === T(123_32)
        @test swapglobal!($M, $r, T(123_1), :sequentially_consistent) === T(123_32)
        @test getglobal($M, $r, :sequentially_consistent) === T(123_1)
    end
end
@noinline function test_global_operators(T::Type)
    r = Symbol("g1_$T")
    @eval global $r::$T = 123_10
    invokelatest(@eval(() -> $(gen_test_global_operators(QuoteNode(r)))))
    r = Symbol("g2_$T")
    @eval global $r::$T = 123_10
    invokelatest(@eval(r -> $(gen_test_global_operators(:r))), r)
    nothing
end
test_global_operators(Int)
test_global_operators(Any)
test_global_operators(Union{Nothing,Int})
test_global_operators(Complex{Int32})
test_global_operators(Complex{Int128})
test_global_operators(Complex{Real})
test_global_operators(PadIntA)
test_global_operators(PadIntB)
#FIXME: test_global_operators(Int24)
test_global_operators(Float64)

function gen_test_global_orderings(@nospecialize r)
    M = @__MODULE__
    return quote
        @nospecialize x y
        TT = Core.get_binding_type($M, $r)

        @test getglobal($M, $r) === x
        @test_throws ConcurrencyViolationError("invalid atomic ordering") getglobal($M, $r, :u)
        @test_throws ConcurrencyViolationError("getglobal: module binding cannot be read non-atomically") getglobal($M, $r, :not_atomic)
        @test getglobal($M, $r, :unordered) === x
        @test getglobal($M, $r, :monotonic) === x
        @test getglobal($M, $r, :acquire) === x
        @test_throws ConcurrencyViolationError("invalid atomic ordering") getglobal($M, $r, :release) === x
        @test_throws ConcurrencyViolationError("invalid atomic ordering") getglobal($M, $r, :acquire_release) === x
        @test getglobal($M, $r, :sequentially_consistent) === x
        @test isdefined($M, $r)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined($M, $r, :u)
        @test_throws ConcurrencyViolationError("isdefined: module binding cannot be accessed non-atomically") isdefined($M, $r, :not_atomic)
        @test isdefined($M, $r, :unordered)
        @test isdefined($M, $r, :monotonic)
        @test isdefined($M, $r, :acquire)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined($M, $r, :release)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") isdefined($M, $r, :acquire_release)
        @test isdefined($M, $r, :sequentially_consistent)

        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobal!($M, $r, y, :u)
        @test_throws ConcurrencyViolationError("setglobal!: module binding cannot be written non-atomically") setglobal!($M, $r, y, :not_atomic)
        @test getglobal($M, $r) === x
        @test setglobal!($M, $r, y) === y
        @test setglobal!($M, $r, y, :unordered) === y
        @test setglobal!($M, $r, y, :monotonic) === y
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobal!($M, $r, y, :acquire) === y
        @test setglobal!($M, $r, y, :release) === y
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobal!($M, $r, y, :acquire_release) === y
        @test setglobal!($M, $r, y, :sequentially_consistent) === y
        @test getglobal($M, $r) === y

        @test_throws ConcurrencyViolationError("invalid atomic ordering") swapglobal!($M, $r, x, :u)
        @test_throws ConcurrencyViolationError("swapglobal!: module binding cannot be written non-atomically") swapglobal!($M, $r, x, :not_atomic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") swapglobal!($M, $r, x, :unordered) === y
        @test swapglobal!($M, $r, x, :monotonic) === y
        @test swapglobal!($M, $r, x, :acquire) === x
        @test swapglobal!($M, $r, x, :release) === x
        @test swapglobal!($M, $r, x, :acquire_release) === x
        @test swapglobal!($M, $r, x, :sequentially_consistent) === x
        @test swapglobal!($M, $r, x) === x

        @test_throws ConcurrencyViolationError("invalid atomic ordering") modifyglobal!($M, $r, swap, x, :u)
        @test_throws ConcurrencyViolationError("modifyglobal!: module binding cannot be written non-atomically") modifyglobal!($M, $r, swap, x, :not_atomic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") modifyglobal!($M, $r, swap, x, :unordered)
        @test modifyglobal!($M, $r, swap, x, :monotonic) === Pair{TT,TT}(x, x)
        @test modifyglobal!($M, $r, swap, x, :acquire) === Pair{TT,TT}(x, x)
        @test modifyglobal!($M, $r, swap, x, :release) === Pair{TT,TT}(x, x)
        @test modifyglobal!($M, $r, swap, x, :acquire_release) === Pair{TT,TT}(x, x)
        @test modifyglobal!($M, $r, swap, x, :sequentially_consistent) === Pair{TT,TT}(x, x)
        @test modifyglobal!($M, $r, swap, x) === Pair{TT,TT}(x, x)

        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :u, :not_atomic)
        @test_throws ConcurrencyViolationError("replaceglobal!: module binding cannot be written non-atomically") replaceglobal!($M, $r, y, x, :not_atomic, :not_atomic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :unordered, :not_atomic)
        @test_throws ConcurrencyViolationError("replaceglobal!: module binding cannot be accessed non-atomically") replaceglobal!($M, $r, x, x, :monotonic, :not_atomic)
        @test_throws ConcurrencyViolationError("replaceglobal!: module binding cannot be accessed non-atomically") replaceglobal!($M, $r, x, x, :acquire, :not_atomic)
        @test_throws ConcurrencyViolationError("replaceglobal!: module binding cannot be accessed non-atomically") replaceglobal!($M, $r, x, x, :release, :not_atomic)
        @test_throws ConcurrencyViolationError("replaceglobal!: module binding cannot be accessed non-atomically") replaceglobal!($M, $r, x, x, :acquire_release, :not_atomic)
        @test_throws ConcurrencyViolationError("replaceglobal!: module binding cannot be accessed non-atomically") replaceglobal!($M, $r, x, x, :sequentially_consistent, :not_atomic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :not_atomic, :u)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :not_atomic, :unordered)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :not_atomic, :monotonic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :not_atomic, :acquire)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :not_atomic, :release)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :not_atomic, :acquire_release)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") replaceglobal!($M, $r, x, x, :not_atomic, :sequentially_consistent)
        @test replaceglobal!($M, $r, x, y, :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((x, true))
        @test replaceglobal!($M, $r, x, y, :sequentially_consistent, :sequentially_consistent) === ReplaceType{TT}((y, x === y))
        @test replaceglobal!($M, $r, y, x, :sequentially_consistent) === ReplaceType{TT}((y, true))
        @test replaceglobal!($M, $r, x, x) === ReplaceType{TT}((x, true))

        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :u, :not_atomic)
        @test_throws ConcurrencyViolationError("setglobalonce!: module binding cannot be written non-atomically") setglobalonce!($M, $r, y, :not_atomic, :not_atomic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r,  x, :unordered, :not_atomic)
        @test_throws ConcurrencyViolationError("setglobalonce!: module binding cannot be accessed non-atomically") setglobalonce!($M, $r, x, :monotonic, :not_atomic)
        @test_throws ConcurrencyViolationError("setglobalonce!: module binding cannot be accessed non-atomically") setglobalonce!($M, $r, x, :acquire, :not_atomic)
        @test_throws ConcurrencyViolationError("setglobalonce!: module binding cannot be accessed non-atomically") setglobalonce!($M, $r, x, :release, :not_atomic)
        @test_throws ConcurrencyViolationError("setglobalonce!: module binding cannot be accessed non-atomically") setglobalonce!($M, $r, x, :acquire_release, :not_atomic)
        @test_throws ConcurrencyViolationError("setglobalonce!: module binding cannot be accessed non-atomically") setglobalonce!($M, $r, x, :sequentially_consistent, :not_atomic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :not_atomic, :u)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :not_atomic, :unordered)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :not_atomic, :monotonic)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :not_atomic, :acquire)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :not_atomic, :release)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :not_atomic, :acquire_release)
        @test_throws ConcurrencyViolationError("invalid atomic ordering") setglobalonce!($M, $r, x, :not_atomic, :sequentially_consistent)
        @test setglobalonce!($M, $r, x) === false
        @test setglobalonce!($M, $r, y, :sequentially_consistent, :sequentially_consistent) === false
        @test setglobalonce!($M, $r, y, :sequentially_consistent, :sequentially_consistent) === false
        @test setglobalonce!($M, $r, x, :sequentially_consistent) === false
    end
end
@noinline function test_global_orderings(T::Type, x, y)
    @nospecialize
    r = Symbol("h1_$T")
    @eval global $r::$T = $(QuoteNode(x))
    invokelatest(@eval((x, y) -> $(gen_test_global_orderings(QuoteNode(r)))), x, y)
    r = Symbol("h2_$T")
    @eval global $r::$T = $(QuoteNode(x))
    invokelatest(@eval((r, x, y) -> $(gen_test_global_orderings(:r))), r, x, y)
    nothing
end
test_global_orderings(Int, 10, 20)
test_global_orderings(Bool, true, false)
test_global_orderings(String, "hi", "bye")
test_global_orderings(Symbol, :hi, :bye)
test_global_orderings(Nothing, nothing, nothing)
test_global_orderings(Any, 123_10, 123_20)
test_global_orderings(Any, true, false)
test_global_orderings(Union{Nothing,Missing}, nothing, missing)
test_global_orderings(Union{Nothing,Int}, nothing, 123_1)
test_global_orderings(Complex{Int128}, Complex{Int128}(10, 30), Complex{Int128}(20, 40))
test_global_orderings(Complex{Real}, Complex{Real}(10, 30), Complex{Real}(20, 40))
test_global_orderings(Float64, 10.0, 20.0)
test_global_orderings(Float64, NaN, Inf)

function gen_test_global_undef(@nospecialize r)
    M = @__MODULE__
    return quote
        TT = Core.get_binding_type($M, $r)
        x = convert(TT, 12345_10)
        @test_throws UndefVarError getglobal($M, $r)
        @test_throws UndefVarError getglobal($M, $r, :sequentially_consistent)
        @test_throws UndefVarError modifyglobal!($M, $r, add, 1, :sequentially_consistent)
        @test_throws (TT === Any ? UndefVarError : Union{TypeError,ErrorException}) replaceglobal!($M, $r, 1, 1.0, :sequentially_consistent) # TODO: should this be TypeError or ErrorException
        @test_throws UndefVarError replaceglobal!($M, $r, 1, x, :sequentially_consistent)
        @test_throws UndefVarError getglobal($M, $r, :sequentially_consistent)
        @test_throws UndefVarError swapglobal!($M, $r, x, :sequentially_consistent)
        @test getglobal($M, $r, :sequentially_consistent) === x === getglobal($M, $r)
    end
end
@noinline function test_global_undef(T)
    r = Symbol("u1_$T")
    @eval global $r::$T
    invokelatest(@eval(() -> $(gen_test_global_undef(QuoteNode(r)))))
    r = Symbol("u2_$T")
    @eval global $r::$T
    invokelatest(@eval(r -> $(gen_test_global_undef(:r))), r)
    nothing
end
test_global_undef(BigInt)
test_global_undef(Any)
test_global_undef(Union{Nothing,Integer})
test_global_undef(UndefComplex{Any})
test_global_undef(UndefComplex{UndefComplex{Any}})
test_global_undef(Int)

function gen_test_globalonce(@nospecialize r)
    M = @__MODULE__
    return quote
        TT = Core.get_binding_type($M, $r)
        x = convert(TT, 12345_10)
        @test_throws UndefVarError getglobal($M, $r)
        @test setglobalonce!($M, $r, x, :sequentially_consistent) === true
        @test getglobal($M, $r, :sequentially_consistent) === x
        @test setglobalonce!($M, $r, convert(TT, 12345_20), :sequentially_consistent) === false
    end
end
@noinline function test_globalonce(T)
    r = Symbol("o1_$T")
    @eval global $r::$T
    invokelatest(@eval(() -> $(gen_test_globalonce(QuoteNode(r)))))
    r = Symbol("o2_$T")
    @eval global $r::$T
    invokelatest(@eval(r -> $(gen_test_globalonce(:r))), r)
    nothing
end
test_globalonce(BigInt)
test_globalonce(Any)
test_globalonce(Union{Nothing,Integer})
test_globalonce(UndefComplex{Any})
test_globalonce(UndefComplex{UndefComplex{Any}})
test_globalonce(Int)

# test macroexpansions
global x::Int
let a = @__MODULE__
    @test_throws ConcurrencyViolationError @atomiconce :not_atomic a.x = 2
    @test true === @atomiconce a.x = 1
    @test 1 === @atomic a.x
    @test false === @atomiconce a.x = 2
end
let a = @__MODULE__
    @test 1 === @atomic a.x
    @test 2 === @atomic :sequentially_consistent a.x = 2
    @test 3 === @atomic :monotonic a.x = 3
    local four = 4
    @test 4 === @atomic :monotonic a.x = four
    @test 3 === @atomic :monotonic a.x = four - 1
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x = 2
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x += 1

    @test 3 === @atomic :monotonic a.x
    @test 5 === @atomic a.x += 2
    @test 4 === @atomic :monotonic a.x -= 1
    @test 12 === @atomic :monotonic a.x *= 3

    @test 12 === @atomic a.x
    @test (12 => 13) === @atomic a.x + 1
    @test (13 => 15) === @atomic :monotonic a.x + 2
    @test (15 => 19) === @atomic a.x max 19
    @test (19 => 20) === @atomic :monotonic a.x max 20
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x + 1
    @test_throws ConcurrencyViolationError @atomic :not_atomic a.x max 30

    @test 20 === @atomic a.x
    @test 20 === @atomicswap a.x = 1
    @test 1 === @atomicswap :monotonic a.x = 2
    @test_throws ConcurrencyViolationError @atomicswap :not_atomic a.x = 1

    @test 2 === @atomic a.x
    @test ReplaceType{Int}((2, true)) === @atomicreplace a.x 2 => 1
    @test ReplaceType{Int}((1, false)) === @atomicreplace :monotonic a.x 2 => 1
    @test ReplaceType{Int}((1, false)) === @atomicreplace :monotonic :monotonic a.x 2 => 1
    @test_throws ConcurrencyViolationError @atomicreplace :not_atomic a.x 1 => 2
    @test_throws ConcurrencyViolationError @atomicreplace :monotonic :acquire a.x 1 => 2

    @test 1 === @atomic a.x
    xchg = 1 => 2
    @test ReplaceType{Int}((1, true)) === @atomicreplace a.x xchg
    @test ReplaceType{Int}((2, false)) === @atomicreplace :monotonic a.x xchg
    @test ReplaceType{Int}((2, false)) === @atomicreplace :acquire_release :monotonic a.x xchg
    @test_throws ConcurrencyViolationError @atomicreplace :not_atomic a.x xchg
    @test_throws ConcurrencyViolationError @atomicreplace :monotonic :acquire a.x xchg
end

## Memory

using InteractiveUtils
using Core: memoryrefget, memoryrefset!, memoryrefswap!, memoryrefreplace!, memoryrefmodify!, memoryrefsetonce!, memoryref_isassigned

@noinline function _test_memory_operators(r)
    r = r[]
    TT = eltype(r)
    T = typeof(r[])
    @test memoryrefget(r, :sequentially_consistent, true) === T(123_10)
    @test memoryrefset!(r, T(123_1), :sequentially_consistent, true) === T(123_1)
    @test memoryrefget(r, :sequentially_consistent, true) === T(123_1)
    @test memoryrefreplace!(r, 123_1 % UInt, T(123_30), :sequentially_consistent, :sequentially_consistent, true) === ReplaceType{TT}((T(123_1), false))
    @test memoryrefreplace!(r, T(123_1), T(123_30), :sequentially_consistent, :sequentially_consistent, true) === ReplaceType{TT}((T(123_1), true))
    @test memoryrefget(r, :sequentially_consistent, true) === T(123_30)
    @test memoryrefreplace!(r, T(123_1), T(123_1), :sequentially_consistent, :sequentially_consistent, true) === ReplaceType{TT}((T(123_30), false))
    @test memoryrefget(r, :sequentially_consistent, true) === T(123_30)
    @test memoryrefmodify!(r, add, 1, :sequentially_consistent, true) === Pair{TT,TT}(T(123_30), T(123_31))
    @test memoryrefmodify!(r, add, 1, :sequentially_consistent, true) === Pair{TT,TT}(T(123_31), T(123_32))
    @test memoryrefget(r, :sequentially_consistent, true) === T(123_32)
    @test memoryrefswap!(r, T(123_1), :sequentially_consistent, true) === T(123_32)
    @test memoryrefget(r, :sequentially_consistent, true) === T(123_1)
    nothing
end
@noinline function test_memory_operators(T::Type)
    x = convert(T, 123_10)
    r = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    memoryrefset!(r, x, :unordered, true) # @atomic r[] = x
    _test_memory_operators(Ref(r))
    r = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    memoryrefset!(r, x, :unordered, true) # @atomic r[] = x
    _test_memory_operators(Ref{Any}(r))
    nothing
end
test_memory_operators(Int)
test_memory_operators(Any)
test_memory_operators(Union{Nothing,Int})
test_memory_operators(Complex{Int32})
test_memory_operators(Complex{Int128})
test_memory_operators(Complex{Real})
test_memory_operators(PadIntA)
test_memory_operators(PadIntB)
#FIXME: test_memory_operators(Int24)
test_memory_operators(Float64)

@noinline function _test_memory_orderings(xr, yr, x, y)
    @nospecialize x y
    xr = xr[]
    yr = yr[]
    TT = eltype(yr)
    @test TT == eltype(xr)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefget(xr, :u, true)
    @test_throws ConcurrencyViolationError("memoryrefget: atomic memory cannot be accessed non-atomically") memoryrefget(xr, :not_atomic, true)
    @test memoryrefget(xr, :unordered, true) === x
    @test memoryrefget(xr, :monotonic, true) === x
    @test memoryrefget(xr, :acquire, true) === x
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefget(xr, :release, true) === x
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefget(xr, :acquire_release, true) === x
    @test memoryrefget(xr, :sequentially_consistent, true) === x
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryref_isassigned(xr, :u, true)
    @test_throws ConcurrencyViolationError("memoryref_isassigned: atomic memory cannot be accessed non-atomically") memoryref_isassigned(xr, :not_atomic, true)
    @test memoryref_isassigned(xr, :unordered, true)
    @test memoryref_isassigned(xr, :monotonic, true)
    @test memoryref_isassigned(xr, :acquire, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryref_isassigned(xr, :release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryref_isassigned(xr, :acquire_release, true)
    @test memoryref_isassigned(xr, :sequentially_consistent, true)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefget(yr, :u, true)
    @test memoryrefget(yr, :not_atomic, true) === y
    @test_throws ConcurrencyViolationError("memoryrefget: non-atomic memory cannot be accessed atomically") memoryrefget(yr, :unordered, true)
    @test_throws ConcurrencyViolationError("memoryrefget: non-atomic memory cannot be accessed atomically") memoryrefget(yr, :monotonic, true)
    @test_throws ConcurrencyViolationError("memoryrefget: non-atomic memory cannot be accessed atomically") memoryrefget(yr, :acquire, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefget(yr, :release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefget(yr, :acquire_release, true)
    @test_throws ConcurrencyViolationError("memoryrefget: non-atomic memory cannot be accessed atomically") memoryrefget(yr, :sequentially_consistent, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryref_isassigned(yr, :u, true)
    @test memoryref_isassigned(yr, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryref_isassigned: non-atomic memory cannot be accessed atomically") memoryref_isassigned(yr, :unordered, true)
    @test_throws ConcurrencyViolationError("memoryref_isassigned: non-atomic memory cannot be accessed atomically") memoryref_isassigned(yr, :monotonic, true)
    @test_throws ConcurrencyViolationError("memoryref_isassigned: non-atomic memory cannot be accessed atomically") memoryref_isassigned(yr, :acquire, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryref_isassigned(yr, :release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryref_isassigned(yr, :acquire_release, true)
    @test_throws ConcurrencyViolationError("memoryref_isassigned: non-atomic memory cannot be accessed atomically") memoryref_isassigned(yr, :sequentially_consistent, true)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefset!(xr, y, :u, true)
    @test_throws ConcurrencyViolationError("memoryrefset!: atomic memory cannot be written non-atomically") memoryrefset!(xr, y, :not_atomic, true)
    @test memoryrefget(xr, :unordered, true) === x
    @test memoryrefset!(xr, y, :unordered, true) === y
    @test memoryrefset!(xr, y, :monotonic, true) === y
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefset!(xr, y, :acquire, true) === y
    @test memoryrefset!(xr, y, :release, true) === y
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefset!(xr, y, :acquire_release, true) === y
    @test memoryrefset!(xr, y, :sequentially_consistent, true) === y
    @test memoryrefget(xr, :unordered, true) === y

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefset!(yr, x, :u, true)
    @test_throws ConcurrencyViolationError("memoryrefset!: non-atomic memory cannot be written atomically") memoryrefset!(yr, x, :unordered, true)
    @test_throws ConcurrencyViolationError("memoryrefset!: non-atomic memory cannot be written atomically") memoryrefset!(yr, x, :monotonic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefset!(yr, x, :acquire, true)
    @test_throws ConcurrencyViolationError("memoryrefset!: non-atomic memory cannot be written atomically") memoryrefset!(yr, x, :release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefset!(yr, x, :acquire_release, true)
    @test_throws ConcurrencyViolationError("memoryrefset!: non-atomic memory cannot be written atomically") memoryrefset!(yr, x, :sequentially_consistent, true)
    @test memoryrefget(yr, :not_atomic, true) === y
    @test memoryrefset!(yr, x, :not_atomic, true) === x
    @test memoryrefset!(yr, x, :not_atomic, true) === x
    @test memoryrefget(yr, :not_atomic, true) === x

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefswap!(yr, y, :u, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefswap!(yr, y, :unordered, true)
    @test_throws ConcurrencyViolationError("memoryrefswap!: non-atomic memory cannot be written atomically") memoryrefswap!(yr, y, :monotonic, true)
    @test_throws ConcurrencyViolationError("memoryrefswap!: non-atomic memory cannot be written atomically") memoryrefswap!(yr, y, :acquire, true)
    @test_throws ConcurrencyViolationError("memoryrefswap!: non-atomic memory cannot be written atomically") memoryrefswap!(yr, y, :release, true)
    @test_throws ConcurrencyViolationError("memoryrefswap!: non-atomic memory cannot be written atomically") memoryrefswap!(yr, y, :acquire_release, true)
    @test_throws ConcurrencyViolationError("memoryrefswap!: non-atomic memory cannot be written atomically") memoryrefswap!(yr, y, :sequentially_consistent, true)
    @test memoryrefswap!(yr, y, :not_atomic, true) === x

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefmodify!(yr, swap, y, :u, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefmodify!(yr, swap, y, :unordered, true)
    @test_throws ConcurrencyViolationError("memoryrefmodify!: non-atomic memory cannot be written atomically") memoryrefmodify!(yr, swap, y, :monotonic, true)
    @test_throws ConcurrencyViolationError("memoryrefmodify!: non-atomic memory cannot be written atomically") memoryrefmodify!(yr, swap, y, :acquire, true)
    @test_throws ConcurrencyViolationError("memoryrefmodify!: non-atomic memory cannot be written atomically") memoryrefmodify!(yr, swap, y, :release, true)
    @test_throws ConcurrencyViolationError("memoryrefmodify!: non-atomic memory cannot be written atomically") memoryrefmodify!(yr, swap, y, :acquire_release, true)
    @test_throws ConcurrencyViolationError("memoryrefmodify!: non-atomic memory cannot be written atomically") memoryrefmodify!(yr, swap, y, :sequentially_consistent, true)
    @test memoryrefmodify!(yr, swap, x, :not_atomic, true) === Pair{TT,TT}(y, x)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :u, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :unordered, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: non-atomic memory cannot be written atomically") memoryrefreplace!(yr, y, y, :monotonic, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: non-atomic memory cannot be written atomically") memoryrefreplace!(yr, y, y, :acquire, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: non-atomic memory cannot be written atomically") memoryrefreplace!(yr, y, y, :release, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: non-atomic memory cannot be written atomically") memoryrefreplace!(yr, y, y, :acquire_release, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: non-atomic memory cannot be written atomically") memoryrefreplace!(yr, y, y, :sequentially_consistent, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :not_atomic, :u, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :not_atomic, :unordered, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :not_atomic, :monotonic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :not_atomic, :acquire, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :not_atomic, :release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :not_atomic, :acquire_release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(yr, y, y, :not_atomic, :sequentially_consistent, true)
    @test memoryrefreplace!(yr, x, y, :not_atomic, :not_atomic, true) === ReplaceType{TT}((x, true))
    @test memoryrefreplace!(yr, x, y, :not_atomic, :not_atomic, true) === ReplaceType{TT}((y, x === y))
    @test memoryrefreplace!(yr, y, y, :not_atomic, :not_atomic, true) === ReplaceType{TT}((y, true))

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefswap!(xr, x, :u, true)
    @test_throws ConcurrencyViolationError("memoryrefswap!: atomic memory cannot be written non-atomically") memoryrefswap!(xr, x, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefswap!(xr, x, :unordered, true) === y
    @test memoryrefswap!(xr, x, :monotonic, true) === y
    @test memoryrefswap!(xr, x, :acquire, true) === x
    @test memoryrefswap!(xr, x, :release, true) === x
    @test memoryrefswap!(xr, x, :acquire_release, true) === x
    @test memoryrefswap!(xr, x, :sequentially_consistent, true) === x

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefmodify!(xr, swap, x, :u, true)
    @test_throws ConcurrencyViolationError("memoryrefmodify!: atomic memory cannot be written non-atomically") memoryrefmodify!(xr, swap, x, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefmodify!(xr, swap, x, :unordered, true)
    @test memoryrefmodify!(xr, swap, x, :monotonic, true) === Pair{TT,TT}(x, x)
    @test memoryrefmodify!(xr, swap, x, :acquire, true) === Pair{TT,TT}(x, x)
    @test memoryrefmodify!(xr, swap, x, :release, true) === Pair{TT,TT}(x, x)
    @test memoryrefmodify!(xr, swap, x, :acquire_release, true) === Pair{TT,TT}(x, x)
    @test memoryrefmodify!(xr, swap, x, :sequentially_consistent, true) === Pair{TT,TT}(x, x)

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :u, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: atomic memory cannot be written non-atomically") memoryrefreplace!(xr, y, x, :not_atomic, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :unordered, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: atomic memory cannot be accessed non-atomically") memoryrefreplace!(xr, x, x, :monotonic, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: atomic memory cannot be accessed non-atomically") memoryrefreplace!(xr, x, x, :acquire, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: atomic memory cannot be accessed non-atomically") memoryrefreplace!(xr, x, x, :release, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: atomic memory cannot be accessed non-atomically") memoryrefreplace!(xr, x, x, :acquire_release, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefreplace!: atomic memory cannot be accessed non-atomically") memoryrefreplace!(xr, x, x, :sequentially_consistent, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :not_atomic, :u, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :not_atomic, :unordered, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :not_atomic, :monotonic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :not_atomic, :acquire, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :not_atomic, :release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :not_atomic, :acquire_release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefreplace!(xr, x, x, :not_atomic, :sequentially_consistent, true)
    @test memoryrefreplace!(xr, x, y, :sequentially_consistent, :sequentially_consistent, true) === ReplaceType{TT}((x, true))
    @test memoryrefreplace!(xr, x, y, :sequentially_consistent, :sequentially_consistent, true) === ReplaceType{TT}((y, x === y))
    @test memoryrefreplace!(xr, y, x, :sequentially_consistent, :sequentially_consistent, true) === ReplaceType{TT}((y, true))

    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :u, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefsetonce!: atomic memory cannot be written non-atomically") memoryrefsetonce!(xr, y, :not_atomic, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr,  x, :unordered, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefsetonce!: atomic memory cannot be accessed non-atomically") memoryrefsetonce!(xr, x, :monotonic, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefsetonce!: atomic memory cannot be accessed non-atomically") memoryrefsetonce!(xr, x, :acquire, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefsetonce!: atomic memory cannot be accessed non-atomically") memoryrefsetonce!(xr, x, :release, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefsetonce!: atomic memory cannot be accessed non-atomically") memoryrefsetonce!(xr, x, :acquire_release, :not_atomic, true)
    @test_throws ConcurrencyViolationError("memoryrefsetonce!: atomic memory cannot be accessed non-atomically") memoryrefsetonce!(xr, x, :sequentially_consistent, :not_atomic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :not_atomic, :u, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :not_atomic, :unordered, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :not_atomic, :monotonic, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :not_atomic, :acquire, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :not_atomic, :release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :not_atomic, :acquire_release, true)
    @test_throws ConcurrencyViolationError("invalid atomic ordering") memoryrefsetonce!(xr, x, :not_atomic, :sequentially_consistent, true)
    @test memoryrefsetonce!(xr, y, :sequentially_consistent, :sequentially_consistent, true) === false
    @test memoryrefsetonce!(xr, y, :sequentially_consistent, :sequentially_consistent, true) === false
    @test memoryrefsetonce!(xr, x, :sequentially_consistent, :sequentially_consistent, true) === false
    nothing
end
@noinline function test_memory_orderings(T::Type, x, y)
    @nospecialize
    xr = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    memoryrefset!(xr, x, :unordered, true) # @atomic xr[] = x
    yr = GenericMemoryRef(Memory{T}(undef, 1))
    yr[] = y
    _test_memory_orderings(Ref(xr), Ref(yr), x, y)
    xr = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    memoryrefset!(xr, x, :unordered, true) # @atomic xr[] = x
    yr = GenericMemoryRef(Memory{T}(undef, 1))
    yr[] = y
    _test_memory_orderings(Ref{Any}(xr), Ref{Any}(yr), x, y)
    nothing
end
@noinline test_memory_orderings(x, y) = (@nospecialize; test_memory_orderings(typeof(x), x, y))
test_memory_orderings(10, 20)
test_memory_orderings(true, false)
test_memory_orderings("hi", "bye")
test_memory_orderings(:hi, :bye)
test_memory_orderings(nothing, nothing)
test_memory_orderings(Any, 123_10, 123_20)
test_memory_orderings(Any, true, false)
test_memory_orderings(Union{Nothing,Missing}, nothing, missing)
test_memory_orderings(Union{Nothing,Int}, nothing, 123_1)
test_memory_orderings(Complex{Int128}(10, 30), Complex{Int128}(20, 40))
test_memory_orderings(Complex{Real}(10, 30), Complex{Real}(20, 40))
test_memory_orderings(10.0, 20.0)
test_memory_orderings(NaN, Inf)

@noinline function _test_memory_undef(r)
    r = r[]
    TT = eltype(r)
    x = convert(TT, 12345_10)
    @test_throws UndefRefError memoryrefget(r, :sequentially_consistent, true)
    @test_throws UndefRefError memoryrefmodify!(r, add, 1, :sequentially_consistent, true)
    @test_throws (TT === Any ? UndefRefError : TypeError) memoryrefreplace!(r, 1, 1.0, :sequentially_consistent, :sequentially_consistent, true)
    @test_throws UndefRefError memoryrefreplace!(r, 1, x, :sequentially_consistent, :sequentially_consistent, true)
    @test_throws UndefRefError memoryrefget(r, :sequentially_consistent, true)
    @test_throws UndefRefError memoryrefswap!(r, x, :sequentially_consistent, true)
    @test memoryrefget(r, :sequentially_consistent, true) === x
    nothing
end
@noinline function test_memory_undef(T)
    r = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    _test_memory_undef(Ref(r))
    r = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    _test_memory_undef(Ref{Any}(r))
    nothing
end
test_memory_undef(BigInt)
test_memory_undef(Any)
test_memory_undef(Union{Nothing,Integer})
test_memory_undef(UndefComplex{Any})
test_memory_undef(UndefComplex{UndefComplex{Any}})

@noinline function _test_once_undef(r)
    r = r[]
    TT = eltype(r)
    x = convert(TT, 12345_10)
    @test_throws UndefRefError memoryrefget(r, :sequentially_consistent, true)
    @test memoryrefsetonce!(r, x, :sequentially_consistent, :sequentially_consistent, true) === true
    @test memoryrefget(r, :sequentially_consistent, true) === x
    @test memoryrefsetonce!(r, convert(TT, 12345_20), :sequentially_consistent, :sequentially_consistent, true) === false
    nothing
end
@noinline function test_once_undef(T)
    r = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    _test_once_undef(Ref(r))
    r = GenericMemoryRef(AtomicMemory{T}(undef, 1))
    _test_once_undef(Ref{Any}(r))
    nothing
end
test_once_undef(BigInt)
test_once_undef(Any)
test_once_undef(Union{Nothing,Integer})
test_once_undef(UndefComplex{Any})
test_once_undef(UndefComplex{UndefComplex{Any}})

mutable struct Atomic57190
    @atomic x::Int
end


function add_one57190!()
    @atomic (Atomic57190(0).x) += 1
end

@test add_one57190!() == 1
