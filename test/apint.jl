module APIntTests

using Test, Random

const I = Core.Intrinsics

# ---------------------------------------------------------------------------
# Custom primitive types for non-standard widths
# ---------------------------------------------------------------------------

const STANDARD_WIDTHS = (8, 16, 32, 64, 128)

# Declare IntN/UIntN primitive types for non-standard widths
for w in (24, 40, 56, 72, 80, 88, 96, 120, 136, 176, 192, 200, 248, 256, 264, 320, 512)
    uname = Symbol("UIntN", w)
    sname = Symbol("IntN", w)
    @eval primitive type $uname <: Unsigned $w end
    @eval primitive type $sname <: Signed $w end
    # show: dump as hex bytes for debuggability
    @eval function Base.show(io::IO, x::$uname)
        r = Ref(x)
        GC.@preserve r begin
            p = Ptr{UInt8}(pointer_from_objref(r))
            print(io, $(string(uname)), "(0x")
            for i in $((w + 7) ÷ 8):-1:1
                print(io, string(unsafe_load(p, i); base=16, pad=2))
            end
            print(io, ")")
        end
    end
    @eval Base.show(io::IO, x::$sname) = (print(io, $(string(sname)), "("); show(io, reinterpret($uname, x)); print(io, ")"))
    nbytes = w ÷ 8
    # typemax(UIntN) = all 0xff bytes
    @eval Base.typemax(::Type{$uname}) = reinterpret($uname, $(ntuple(_ -> 0xff, nbytes)))
    # typemin(UIntN) = 0
    @eval Base.typemin(::Type{$uname}) = reinterpret($uname, $(ntuple(_ -> 0x00, nbytes)))
    # typemax(IntN) = 0111...1 (high bit clear)
    @eval Base.typemax(::Type{$sname}) = reinterpret($sname, I.xor_int(
        reinterpret($uname, $(ntuple(_ -> 0xff, nbytes))),
        reinterpret($uname, $(ntuple(i -> i == nbytes ? 0x80 : 0x00, nbytes)))))
    # typemin(IntN) = 1000...0 (high bit set)
    @eval Base.typemin(::Type{$sname}) = reinterpret($sname,
        reinterpret($uname, $(ntuple(i -> i == nbytes ? 0x80 : 0x00, nbytes))))
end

# Map bit-width to Julia type
function uint_type(n::Int)
    n ==   8 && return UInt8
    n ==  16 && return UInt16
    n ==  32 && return UInt32
    n ==  64 && return UInt64
    n == 128 && return UInt128
    return getglobal(@__MODULE__, Symbol("UIntN", n))
end
function int_type(n::Int)
    n ==   8 && return Int8
    n ==  16 && return Int16
    n ==  32 && return Int32
    n ==  64 && return Int64
    n == 128 && return Int128
    return getglobal(@__MODULE__, Symbol("IntN", n))
end

# ---------------------------------------------------------------------------
# Value construction helpers
# ---------------------------------------------------------------------------

# Construct an n-bit unsigned value from a byte tuple (little-endian)
function from_bytes(::Type{T}, bytes::NTuple{N,UInt8}) where {T,N}
    reinterpret(T, bytes)
end

# Construct the zero value for an unsigned type of width n
function make_zero(::Type{T}, n::Int) where T
    nbytes = n ÷ 8
    from_bytes(T, ntuple(_ -> 0x00, nbytes))
end

# Construct an unsigned value with a single byte set (0-indexed byte position)
function make_byte(::Type{T}, n::Int, byte_pos::Int, val::UInt8) where T
    nbytes = n ÷ 8
    from_bytes(T, ntuple(i -> i == byte_pos + 1 ? val : 0x00, nbytes))
end

# Construct value with bit `b` set (0-indexed)
function make_bit(::Type{T}, n::Int, b::Int) where T
    nbytes = n ÷ 8
    byte_idx = b ÷ 8     # 0-indexed
    bit_idx  = b % 8
    from_bytes(T, ntuple(i -> i == byte_idx + 1 ? UInt8(1) << bit_idx : 0x00, nbytes))
end

# Construct value with multiple bits set (0-indexed)
function make_bits(::Type{T}, n::Int, bits) where T
    nbytes = n ÷ 8
    bytes = zeros(UInt8, nbytes)
    for b in bits
        bytes[b ÷ 8 + 1] |= UInt8(1) << (b % 8)
    end
    from_bytes(T, NTuple{nbytes,UInt8}(bytes))
end

# ---------------------------------------------------------------------------
# Random value generation
# ---------------------------------------------------------------------------

# Valid range for fptosi/fptoui of n-bit integers (computed in Float64 to avoid overflow)
float_range(n::Int, signed::Bool) = signed ? (-ldexp(1.0, n - 1), ldexp(1.0, n - 1)) : (0.0, ldexp(1.0, n))

"""Generate interesting float values of type `F` for testing fp↔int conversions on `n`-bit integers."""
function interesting_floats(::Type{F}, n::Int; signed::Bool=false) where {F<:AbstractFloat}
    vals = F[]
    prec = F == Float32 ? 24 : 53   # significand bits (including implicit 1)
    lo, hi = float_range(n, signed)
    inrange(v) = lo <= Float64(v) < hi

    # Zeros and near-zero
    for v in F[0, -0.0, nextfloat(F(0)), prevfloat(F(0)),
               floatmin(F), prevfloat(floatmin(F))]
        inrange(v) && push!(vals, v)
    end
    # Small exact integers + fractional values (truncation tests)
    for v in F[0.5, -0.5, 0.9, -0.9, 1.1, -1.1, 1.5, -1.5, 1.9, -1.9,
               1, -1, 2, -2, 42, -42, 127, -128, 128, 255, 256, 1000, -1000]
        inrange(v) && push!(vals, v)
    end
    # Powers of 2 spanning the exponent range
    for e in 0:min(n - (signed ? 2 : 1), prec - 1)
        for v in [ldexp(F(1), e), -ldexp(F(1), e)]
            inrange(v) && push!(vals, v)
        end
    end
    # Precision boundary: 2^p-1, 2^p, 2^p+1 (where p = significand bits)
    for p in [prec - 1, prec]
        for v in [ldexp(F(1), p) - 1, ldexp(F(1), p), ldexp(F(1), p) + 1,
                  -(ldexp(F(1), p) - 1), -ldexp(F(1), p), -(ldexp(F(1), p) + 1)]
            inrange(v) && push!(vals, v)
        end
    end
    # Large representable values (1e15 for Float64, 1e7 for Float32)
    for v in (F == Float32 ? F[1e7, -1e7] : F[1e15, -1e15])
        inrange(v) && push!(vals, v)
    end
    # Largest/smallest floats still in range after truncation
    for v in F[prevfloat(F(hi)), prevfloat(F(lo) - 1)]
        inrange(v) && push!(vals, v)
    end

    return vals
end

"""Generate random float values of type `F` with exponents biased toward interesting regions."""
function random_floats(rng::AbstractRNG, ::Type{F}, n::Int, count::Int; signed::Bool=false) where {F<:AbstractFloat}
    vals = F[]
    prec = F == Float32 ? 24 : 53
    lo, hi = float_range(n, signed)
    inrange(v) = lo <= Float64(v) < hi

    fmax_exp = F == Float32 ? 127 : 1023
    max_exp = min(n - (signed ? 1 : 0), fmax_exp)
    nbits_mantissa = prec - 1

    # Pool of interesting exponents (with duplicates to bias selection)
    hot_exps = [0, 1, 2, 3, prec-2, prec-1, prec, prec+1,
                max_exp-2, max_exp-1, max_exp, 63, 64, 65, 127, 128, 129]
    filter!(e -> 0 <= e <= max_exp, hot_exps)

    for _ in 1:count
        # 50/50 split: biased toward interesting exponents vs. uniform
        e = rand(rng, Bool) ? rand(rng, hot_exps) : rand(rng, 0:max_exp)

        mant_frac = rand(rng, F) # uniform in [0, 1)
        v = ldexp(F(1) + mant_frac * (F(1) - ldexp(F(1), -nbits_mantissa)), e)

        if signed && rand(rng, Bool)
            v = -v
        end

        inrange(v) && push!(vals, v)
    end

    return vals
end

"""Generate a vector of random n-bit unsigned values as the appropriate type."""
function random_ints(rng::AbstractRNG, n::Int, count::Int)
    T = uint_type(n)
    nbytes = n ÷ 8
    [from_bytes(T, ntuple(_ -> rand(rng, UInt8), nbytes)) for _ in 1:count]
end

"""Generate interesting n-bit unsigned values as the appropriate type."""
function interesting_ints(n::Int)
    T = uint_type(n)
    ST = int_type(n)
    nbytes = n ÷ 8
    zero_v = make_zero(T, n)
    one_v  = make_byte(T, n, 0, 0x01)
    two_v  = make_byte(T, n, 0, 0x02)
    umax_v = typemax(T)
    smin_v = reinterpret(T, typemin(ST))
    smax_v = reinterpret(T, typemax(ST))
    # UMAX - 1: flip bit 0
    umax_m1 = I.xor_int(umax_v, one_v)
    # SMIN + 1
    smin_p1 = I.add_int(smin_v, one_v)

    vals = [zero_v, one_v, two_v, umax_v, umax_m1, smin_v, smax_v, smin_p1]

    # Alternating bit patterns: 0x55 and 0xAA bytes, masked to n bits
    alt55 = from_bytes(T, ntuple(_ -> 0x55, nbytes))
    altaa = from_bytes(T, ntuple(_ -> 0xaa, nbytes))
    push!(vals, alt55, altaa)

    # Word boundary values: bit 64, 128, ... set
    for w in 64:64:(n-1)
        bit_w = make_bit(T, n, w)
        push!(vals, bit_w)
        push!(vals, I.sub_int(bit_w, one_v))   # 2^w - 1
        push!(vals, I.add_int(bit_w, one_v))   # 2^w + 1
    end

    # Float32 double-rounding stress values: MSB at bit k, Float32 round bit
    # at k-24, sticky bit at k-54 (below the Float64 extraction window), so a
    # naive int→Float64→Float32 conversion loses the sticky and rounds wrong.
    if n > 56
        for k in [n - 1, 65, 64]
            k >= n && continue
            k < 25 && continue
            push!(vals, make_bits(T, n, [k, k - 24, max(k - 54, 0)]))
            push!(vals, make_bits(T, n, [k, k - 1, k - 24, max(k - 54, 0)]))
        end
    end

    return vals
end

# ---------------------------------------------------------------------------
# Intrinsic dispatch (runtime vs. codegen)
# ---------------------------------------------------------------------------

# This wrapper hits the "interpreted" (runtime_intrinsics.c) path
runtime_call(f, args...) = Base.invokelatest(f, args...)

# These wrappers hit the codegen / LLVM path.
@noinline _test_add(a, b)  = I.add_int(a, b)
@noinline _test_sub(a, b)  = I.sub_int(a, b)
@noinline _test_mul(a, b)  = I.mul_int(a, b)
@noinline _test_and(a, b)  = I.and_int(a, b)
@noinline _test_or(a, b)   = I.or_int(a, b)
@noinline _test_xor(a, b)  = I.xor_int(a, b)
@noinline _test_not(a)     = I.not_int(a)
@noinline _test_neg(a)     = I.neg_int(a)
@noinline _test_shl(a, b)  = I.shl_int(a, b)
@noinline _test_lshr(a, b) = I.lshr_int(a, b)
@noinline _test_ashr(a, b) = I.ashr_int(a, b)
@noinline _test_eq(a, b)   = I.eq_int(a, b)
@noinline _test_ne(a, b)   = I.ne_int(a, b)
@noinline _test_ult(a, b)  = I.ult_int(a, b)
@noinline _test_ule(a, b)  = I.ule_int(a, b)
@noinline _test_slt(a, b)  = I.slt_int(a, b)
@noinline _test_sle(a, b)  = I.sle_int(a, b)
@noinline _test_ctpop(a)   = I.ctpop_int(a)
@noinline _test_ctlz(a)    = I.ctlz_int(a)
@noinline _test_cttz(a)    = I.cttz_int(a)
@noinline _test_checked_sadd(a, b) = I.checked_sadd_int(a, b)
@noinline _test_checked_uadd(a, b) = I.checked_uadd_int(a, b)
@noinline _test_checked_ssub(a, b) = I.checked_ssub_int(a, b)
@noinline _test_checked_usub(a, b) = I.checked_usub_int(a, b)
@noinline _test_checked_smul(a, b) = I.checked_smul_int(a, b)
@noinline _test_checked_umul(a, b) = I.checked_umul_int(a, b)
@noinline _test_udiv(a, b) = I.udiv_int(a, b)
@noinline _test_urem(a, b) = I.urem_int(a, b)
@noinline _test_sdiv(a, b) = I.sdiv_int(a, b)
@noinline _test_srem(a, b) = I.srem_int(a, b)
@noinline _test_checked_sdiv(a, b) = I.checked_sdiv_int(a, b)
@noinline _test_checked_udiv(a, b) = I.checked_udiv_int(a, b)
@noinline _test_checked_srem(a, b) = I.checked_srem_int(a, b)
@noinline _test_checked_urem(a, b) = I.checked_urem_int(a, b)
@noinline _test_flipsign(a, b) = I.flipsign_int(a, b)
@noinline _test_bswap(a)   = I.bswap_int(a)
@noinline _test_sitofp(::Type{T}, a) where T = I.sitofp(T, a)
@noinline _test_uitofp(::Type{T}, a) where T = I.uitofp(T, a)
@noinline _test_fptosi(::Type{T}, x) where T = I.fptosi(T, x)
@noinline _test_fptoui(::Type{T}, x) where T = I.fptoui(T, x)
@noinline _test_sext(::Type{T}, a) where T   = I.sext_int(T, a)
@noinline _test_zext(::Type{T}, a) where T   = I.zext_int(T, a)
@noinline _test_trunc(::Type{T}, a) where T  = I.trunc_int(T, a)

"""
    @check_intrinsic

Test that compiled and interpreted paths agree.
Exceptions are disallowed unless `may_throw` is provided, and must be egal otherwise.

Examples:
    @check_intrinsic may_throw=ErrorException (compiled_expr, runtime_expr)
"""
macro check_intrinsic(args...)
    may_throw = nothing
    rest = args
    if length(args) >= 1 && Meta.isexpr(args[1], :(=)) && args[1].args[1] === :may_throw
        may_throw = args[1].args[2]
        rest = args[2:end]
    end
    if length(rest) == 1 && Meta.isexpr(rest[1], :tuple)
        compiled_expr = rest[1].args[1]
        interp_expr = rest[1].args[2]
    else
        error("@check_intrinsic expects (compiled_expr, runtime_expr)")
    end
    quote
        compiled_ok, compiled_val, compiled_err = try
            (true, $(esc(compiled_expr)), nothing)
        catch e
            (false, nothing, e)
        end
        interp_ok, interp_val, interp_err = try
            (true, $(esc(interp_expr)), nothing)
        catch e
            (false, nothing, e)
        end
        if compiled_ok && interp_ok
            @test compiled_val === interp_val
        elseif $(esc(may_throw)) !== nothing && !compiled_ok && !interp_ok
            @test compiled_err isa $(esc(may_throw))
            @test interp_err isa $(esc(may_throw))
            @test compiled_err === interp_err
        else
            @test false # one threw but the other didn't (or error not allowed)
        end
    end
end

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

const TEST_WIDTHS = [8, 16, 24, 32, 40, 56, 64, 72, 80, 88, 96, 120, 128, 136, 176, 192, 200, 248, 256, 264, 320, 512]
const NRANDOM = 50

@testset "APInt intrinsics" begin
    seed = 42
    rng = MersenneTwister(seed)

    for n in TEST_WIDTHS
        UT = uint_type(n)
        ST = int_type(n)

        iints = interesting_ints(n)
        rints = random_ints(rng, n, NRANDOM)
        allints = vcat(iints, rints)

        # Build test pairs: interesting×interesting[:8] + random pairs
        pairs = Tuple{Any,Any}[]
        short_iints = iints[1:min(8, length(iints))]
        for a in iints, b in short_iints
            push!(pairs, (a, b))
        end
        for i in 1:NRANDOM
            push!(pairs, (rints[min(i, end)], random_ints(rng, n, 1)[1]))
        end

        zero_u = typemin(UT)
        zero_s = reinterpret(ST, zero_u)
        neg1_s = reinterpret(ST, typemax(UT))
        smin_s = typemin(ST)

        @testset "$n-bit" begin
            # --- Binary arithmetic ---
            @testset "add" begin
                for (a, b) in pairs
                    @test _test_add(a, b) === runtime_call(I.add_int, a, b)
                end
            end
            @testset "sub" begin
                for (a, b) in pairs
                    @test _test_sub(a, b) === runtime_call(I.sub_int, a, b)
                end
            end
            @testset "mul" begin
                for (a, b) in pairs
                    @test _test_mul(a, b) === runtime_call(I.mul_int, a, b)
                end
            end
            @testset "neg" begin
                for a in allints
                    sa = reinterpret(ST, a)
                    @test _test_neg(sa) === runtime_call(I.neg_int, sa)
                end
            end

            # --- Bitwise ---
            @testset "and" begin
                for (a, b) in pairs
                    @test _test_and(a, b) === runtime_call(I.and_int, a, b)
                end
            end
            @testset "or" begin
                for (a, b) in pairs
                    @test _test_or(a, b) === runtime_call(I.or_int, a, b)
                end
            end
            @testset "xor" begin
                for (a, b) in pairs
                    @test _test_xor(a, b) === runtime_call(I.xor_int, a, b)
                end
            end
            @testset "not" begin
                for a in allints
                    @test _test_not(a) === runtime_call(I.not_int, a)
                end
            end

            # --- Shifts ---
            @testset "shl" begin
                for (a, b) in pairs
                    @test _test_shl(a, b) === runtime_call(I.shl_int, a, b)
                end
            end
            @testset "lshr" begin
                for (a, b) in pairs
                    @test _test_lshr(a, b) === runtime_call(I.lshr_int, a, b)
                end
            end
            @testset "ashr" begin
                for (a, b) in pairs
                    sa = reinterpret(ST, a)
                    @test _test_ashr(sa, b) === runtime_call(I.ashr_int, sa, b)
                end
            end

            # --- Comparisons ---
            @testset "eq"  begin
                for (a,b) in pairs
                    @test _test_eq(a,b)  === runtime_call(I.eq_int, a, b)
                end
            end
            @testset "ne"  begin
                for (a,b) in pairs
                    @test _test_ne(a,b)  === runtime_call(I.ne_int, a, b)
                end
            end
            @testset "ult" begin
                for (a,b) in pairs
                    @test _test_ult(a,b) === runtime_call(I.ult_int, a, b)
                end
            end
            @testset "ule" begin
                for (a,b) in pairs
                    @test _test_ule(a,b) === runtime_call(I.ule_int, a, b)
                end
            end
            @testset "slt" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @test _test_slt(sa, sb) === runtime_call(I.slt_int, sa, sb)
                end
            end
            @testset "sle" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @test _test_sle(sa, sb) === runtime_call(I.sle_int, sa, sb)
                end
            end

            # --- Bit counting ---
            @testset "ctpop" begin
                for a in allints
                    @test _test_ctpop(a) === runtime_call(I.ctpop_int, a)
                end
            end
            @testset "ctlz"  begin
                for a in allints
                    @test _test_ctlz(a) === runtime_call(I.ctlz_int, a)
                end
            end
            @testset "cttz"  begin
                for a in allints
                    @test _test_cttz(a) === runtime_call(I.cttz_int, a)
                end
            end

            # --- Checked arithmetic ---
            @testset "checked_sadd" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @test _test_checked_sadd(sa, sb) === runtime_call(I.checked_sadd_int, sa, sb)
                end
            end
            @testset "checked_uadd" begin
                for (a, b) in pairs
                    @test _test_checked_uadd(a, b) === runtime_call(I.checked_uadd_int, a, b)
                end
            end
            @testset "checked_ssub" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @test _test_checked_ssub(sa, sb) === runtime_call(I.checked_ssub_int, sa, sb)
                end
            end
            @testset "checked_usub" begin
                for (a, b) in pairs
                    @test _test_checked_usub(a, b) === runtime_call(I.checked_usub_int, a, b)
                end
            end
            @testset "checked_smul" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @test _test_checked_smul(sa, sb) === runtime_call(I.checked_smul_int, sa, sb)
                end
            end
            @testset "checked_umul" begin
                for (a, b) in pairs
                    @test _test_checked_umul(a, b) === runtime_call(I.checked_umul_int, a, b)
                end
            end

            # --- Division (skip zero divisors and SMIN/-1 UB) ---
            @testset "udiv" begin
                for (a, b) in pairs
                    b === zero_u && continue
                    @test _test_udiv(a, b) === runtime_call(I.udiv_int, a, b)
                end
            end
            @testset "urem" begin
                for (a, b) in pairs
                    b === zero_u && continue
                    @test _test_urem(a, b) === runtime_call(I.urem_int, a, b)
                end
            end
            @testset "sdiv" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    sb === zero_s && continue
                    sa === smin_s && sb === neg1_s && continue
                    @test _test_sdiv(sa, sb) === runtime_call(I.sdiv_int, sa, sb)
                end
            end
            @testset "srem" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    sb === zero_s && continue
                    sa === smin_s && sb === neg1_s && continue
                    @test _test_srem(sa, sb) === runtime_call(I.srem_int, sa, sb)
                end
            end

            # --- Checked division ---
            @testset "checked_sdiv" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @check_intrinsic may_throw=DivideError (
                        _test_checked_sdiv(sa, sb),
                        runtime_call(I.checked_sdiv_int, sa, sb),
                    )
                end
            end
            @testset "checked_udiv" begin
                for (a, b) in pairs
                    @check_intrinsic may_throw=DivideError (
                        _test_checked_udiv(a, b),
                        runtime_call(I.checked_udiv_int, a, b),
                    )
                end
            end
            @testset "checked_srem" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @check_intrinsic may_throw=DivideError (
                        _test_checked_srem(sa, sb),
                        runtime_call(I.checked_srem_int, sa, sb),
                    )
                end
            end
            @testset "checked_urem" begin
                for (a, b) in pairs
                    @check_intrinsic may_throw=DivideError (
                        _test_checked_urem(a, b),
                        runtime_call(I.checked_urem_int, a, b),
                    )
                end
            end

            # --- Flipsign ---
            @testset "flipsign" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    @test _test_flipsign(sa, sb) === runtime_call(I.flipsign_int, sa, sb)
                end
            end

            # --- Bswap (requires multiple of 16 bits) ---
            if n >= 16 && n % 16 == 0
                @testset "bswap" begin
                    for a in allints
                        @test _test_bswap(a) === runtime_call(I.bswap_int, a)
                    end
                end
            end

            # --- FP conversions ---
            @testset "sitofp Float64" begin
                for a in allints
                    sa = reinterpret(ST, a)
                    @test _test_sitofp(Float64, sa) === runtime_call(I.sitofp, Float64, sa)
                end
            end
            @testset "uitofp Float64" begin
                for a in allints
                    @test _test_uitofp(Float64, a) === runtime_call(I.uitofp, Float64, a)
                end
            end
            @testset "sitofp Float32" begin
                for a in allints
                    sa = reinterpret(ST, a)
                    interp = runtime_call(I.sitofp, Float32, sa)
                    if n > 128 && !isfinite(interp)
                        @test_broken false # https://github.com/llvm/llvm-project/issues/189054
                        break
                    else
                        @test _test_sitofp(Float32, sa) === interp
                    end
                end
            end
            @testset "uitofp Float32" begin
                for a in allints
                    interp = runtime_call(I.uitofp, Float32, a)
                    if n > 128 && !isfinite(interp)
                        @test_broken false # https://github.com/llvm/llvm-project/issues/189054
                        break
                    else
                        @test _test_uitofp(Float32, a) === interp
                    end
                end
            end
            # Out-of-range fptosi/fptoui (including NaN/Inf) is poison in
            # LLVM IR, so interesting_floats pre-filters to the valid range.
            for F in (Float64, Float32)
                @testset "fptosi $F" begin
                    for fv in vcat(interesting_floats(F, n; signed=true),
                                   random_floats(rng, F, n, NRANDOM; signed=true))
                        @test _test_fptosi(ST, fv) === runtime_call(I.fptosi, ST, fv)
                    end
                end
                @testset "fptoui $F" begin
                    for fv in vcat(interesting_floats(F, n; signed=false),
                                   random_floats(rng, F, n, NRANDOM; signed=false))
                        @test _test_fptoui(UT, fv) === runtime_call(I.fptoui, UT, fv)
                    end
                end
            end
        end
    end

    # --- sext / zext / trunc across all width pairs ---
    # Tests all three operations for every (from, to) pair.
    @testset "sext/zext/trunc" begin
        for from_n in TEST_WIDTHS, to_n in TEST_WIDTHS
            from_UT = uint_type(from_n)
            from_ST = int_type(from_n)
            to_UT = uint_type(to_n)
            to_ST = int_type(to_n)

            from_vals = vcat(interesting_ints(from_n), random_ints(rng, from_n, 10))

            # For valid conversions test all values; for invalid ones
            # the error depends only on types, so one value suffices.
            sext_vals = to_n > from_n ? from_vals : from_vals[1:1]
            zext_vals = to_n > from_n ? from_vals : from_vals[1:1]
            trunc_vals = to_n < from_n ? from_vals : from_vals[1:1]

            @testset "sext $from_n → $to_n" begin
                for a in sext_vals
                    sa = reinterpret(from_ST, a)
                    @check_intrinsic may_throw=ErrorException (
                        _test_sext(to_ST, sa),
                        runtime_call(I.sext_int, to_ST, sa),
                    )
                end
            end
            @testset "zext $from_n → $to_n" begin
                for a in zext_vals
                    @check_intrinsic may_throw=ErrorException (
                        _test_zext(to_UT, a),
                        runtime_call(I.zext_int, to_UT, a),
                    )
                end
            end
            @testset "trunc $from_n → $to_n" begin
                for a in trunc_vals
                    @check_intrinsic may_throw=ErrorException (
                        _test_trunc(to_UT, a),
                        runtime_call(I.trunc_int, to_UT, a),
                    )
                end
            end
        end
    end
end

end # module APIntTests
