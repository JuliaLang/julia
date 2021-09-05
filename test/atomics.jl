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

@test_throws ErrorException("invalid redefinition of constant ARefxy") @eval mutable struct ARefxy{T}
    @atomic x::T
    @atomic y::T
end
@test_throws ErrorException("invalid redefinition of constant ARefxy") @eval mutable struct ARefxy{T}
    x::T
    y::T
end
@test_throws ErrorException("invalid redefinition of constant ARefxy") @eval mutable struct ARefxy{T}
    x::T
    @atomic y::T
end
@test_throws ErrorException("invalid redefinition of constant Refxy") @eval mutable struct Refxy{T}
    x::T
    @atomic y::T
end

copy(r::Union{Refxy,ARefxy}) = typeof(r)(r.x, r.y)
function add(x::T, y)::T where {T}; x + y; end
swap(x, y) = y

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
    ar = ARefxy(x, y)
    r = Refxy(x, y)
    @test 64 == sizeof(r) < sizeof(ar)
    @test sizeof(r) == sizeof(ar) - Int(fieldoffset(typeof(ar), 1))
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
Base.:(+)(x::PadIntB, b::Int) = PadIntB(Int(x) + b)
Base.:(+)(x::Int24, b::Int) = Core.Intrinsics.add_int(x, Int24(b))
Base.show(io::IO, x::PadIntA) = print(io, "PadIntA(", x.b, ")")
Base.show(io::IO, x::PadIntB) = print(io, "PadIntB(", Int(x), ")")
Base.show(io::IO, x::Int24) = print(io, "Int24(", Core.Intrinsics.zext_int(Int, x), ")")

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
    nothing
end
@noinline function test_field_orderings(r, x, y)
    _test_field_orderings(Ref(copy(r)), x, y)
    _test_field_orderings(Ref{Any}(copy(r)), x, y)
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
test_field_orderings(10.0, 20.0)
test_field_orderings(NaN, Inf)

struct UndefComplex{T}
    re::T
    im::T
    UndefComplex{T}() where {T} = new{T}()
end
Base.convert(T::Type{<:UndefComplex}, S) = T()
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
