# Tests for wide-integer (APInt) runtime intrinsics via Core.Intrinsics.*_int
# Compares compiled (codegen) vs interpreted (runtime) paths.

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
    # show: dump as hex limbs for debuggability
    nlimbs = (w + 63) ÷ 64
    @eval function Base.show(io::IO, x::$uname)
        r = Ref(x)
        GC.@preserve r begin
            p = Ptr{UInt64}(pointer_from_objref(r))
            print(io, $(string(uname)), "(")
            for i in $nlimbs:-1:1
                i < $nlimbs && print(io, "_")
                print(io, string(unsafe_load(p, i); base=16, pad=16))
            end
            print(io, ")")
        end
    end
    @eval Base.show(io::IO, x::$sname) = (print(io, $(string(sname)), "("); show(io, reinterpret($uname, x)); print(io, ")"))
end

# Map bit-width to Julia type
function uint_type(n::Int)
    n ==   8 && return UInt8
    n ==  16 && return UInt16
    n ==  32 && return UInt32
    n ==  64 && return UInt64
    n == 128 && return UInt128
    return @eval $(Symbol("UIntN", n))
end
function int_type(n::Int)
    n ==   8 && return Int8
    n ==  16 && return Int16
    n ==  32 && return Int32
    n ==  64 && return Int64
    n == 128 && return Int128
    return @eval $(Symbol("IntN", n))
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

# Construct UMAX (all 0xff)
function make_umax(::Type{T}, n::Int) where T
    nbytes = n ÷ 8
    from_bytes(T, ntuple(_ -> 0xff, nbytes))
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
    v = make_zero(T, n)
    for b in bits
        v = I.or_int(v, make_bit(T, n, b))
    end
    v
end

# Construct value from a UInt128 (for n <= 128 only, used in interesting_values)
function from_uint128(::Type{T}, val::UInt128, n::Int) where T
    nbytes = n ÷ 8
    from_bytes(T, ntuple(i -> i <= 16 ? ((val >> (8*(i-1))) % UInt8) : 0x00, nbytes))
end

# ---------------------------------------------------------------------------
# Random value generation
# ---------------------------------------------------------------------------

"""Generate a vector of random n-bit unsigned values as the appropriate type."""
function random_values(rng::AbstractRNG, n::Int, count::Int)
    T = uint_type(n)
    nbytes = n ÷ 8
    [from_bytes(T, ntuple(_ -> rand(rng, UInt8), nbytes)) for _ in 1:count]
end

"""Generate interesting n-bit unsigned values as the appropriate type."""
function interesting_values(n::Int)
    T = uint_type(n)
    nbytes = n ÷ 8
    zero_v = make_zero(T, n)
    one_v  = make_byte(T, n, 0, 0x01)
    two_v  = make_byte(T, n, 0, 0x02)
    umax_v = make_umax(T, n)
    smin_v = make_bit(T, n, n - 1)                                     # 1000...0
    # UMAX - 1: flip bit 0
    umax_m1 = I.xor_int(umax_v, one_v)
    # SMAX: UMAX with high bit cleared = xor(UMAX, SMIN)
    smax_v = I.xor_int(umax_v, smin_v)
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

    return vals
end

# Call intrinsic via runtime interpreter (not compiled)
runtime_call(f, args...) = Base.invokelatest(f, args...)

# Test that compiled and interpreted paths agree, even when either may throw.
function test_compiled_vs_interp(compiled_f, interp_f)
    compiled_ok, compiled_val, compiled_err = try
        (true, compiled_f(), nothing)
    catch e
        (false, nothing, e)
    end
    interp_ok, interp_val, interp_err = try
        (true, interp_f(), nothing)
    catch e
        (false, nothing, e)
    end
    if compiled_ok && interp_ok
        @test compiled_val === interp_val
    elseif !compiled_ok && !interp_ok
        @test typeof(compiled_err) === typeof(interp_err)
    else
        @test false # one threw but the other didn't
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

        ivals = interesting_values(n)
        rvals = random_values(rng, n, NRANDOM)
        allvals = vcat(ivals, rvals)

        # Values that stress Float32 rounding (int→float double-rounding).
        # Pattern: MSB at bit k, float round bit at k-24, sticky bit below
        # the double extraction window (bit k-54), so a naive int→double→float
        # conversion loses the sticky and rounds wrong.
        f32_rounding_vals = Any[]
        if n > 56
            for k in [n - 1, 65, 64]
                k >= n && continue
                k < 25 && continue
                # 2^k + 2^(k-24) + 2^(max(k-54, 0)): MSB + round + sticky
                v = make_bits(UT, n, [k, k - 24, max(k - 54, 0)])
                push!(f32_rounding_vals, v)
                # Also test with a few extra mantissa bits set
                push!(f32_rounding_vals, make_bits(UT, n, [k, k - 1, k - 24, max(k - 54, 0)]))
            end
        end

        # Build test pairs: interesting×interesting[:8] + random pairs
        pairs = Tuple{Any,Any}[]
        short_ivals = ivals[1:min(8, length(ivals))]
        for a in ivals, b in short_ivals
            push!(pairs, (a, b))
        end
        for i in 1:NRANDOM
            push!(pairs, (rvals[min(i, end)], random_values(rng, n, 1)[1]))
        end

        # Define @noinline wrappers for this width (forces codegen path)
        @eval begin
            @noinline _test_add(a::$UT, b::$UT) = I.add_int(a, b)
            @noinline _test_sub(a::$UT, b::$UT) = I.sub_int(a, b)
            @noinline _test_mul(a::$UT, b::$UT) = I.mul_int(a, b)
            @noinline _test_and(a::$UT, b::$UT) = I.and_int(a, b)
            @noinline _test_or(a::$UT, b::$UT)  = I.or_int(a, b)
            @noinline _test_xor(a::$UT, b::$UT) = I.xor_int(a, b)
            @noinline _test_not(a::$UT)          = I.not_int(a)
            @noinline _test_neg(a::$ST)          = I.neg_int(a)
            @noinline _test_shl(a::$UT, b::$UT)  = I.shl_int(a, b)
            @noinline _test_lshr(a::$UT, b::$UT) = I.lshr_int(a, b)
            @noinline _test_ashr(a::$ST, b::$UT) = I.ashr_int(a, b)
            @noinline _test_eq(a::$UT, b::$UT)  = I.eq_int(a, b)
            @noinline _test_ne(a::$UT, b::$UT)  = I.ne_int(a, b)
            @noinline _test_ult(a::$UT, b::$UT) = I.ult_int(a, b)
            @noinline _test_ule(a::$UT, b::$UT) = I.ule_int(a, b)
            @noinline _test_slt(a::$ST, b::$ST) = I.slt_int(a, b)
            @noinline _test_sle(a::$ST, b::$ST) = I.sle_int(a, b)
            @noinline _test_ctpop(a::$UT) = I.ctpop_int(a)
            @noinline _test_ctlz(a::$UT)  = I.ctlz_int(a)
            @noinline _test_cttz(a::$UT)  = I.cttz_int(a)
            @noinline _test_checked_sadd(a::$ST, b::$ST) = I.checked_sadd_int(a, b)
            @noinline _test_checked_uadd(a::$UT, b::$UT) = I.checked_uadd_int(a, b)
            @noinline _test_checked_ssub(a::$ST, b::$ST) = I.checked_ssub_int(a, b)
            @noinline _test_checked_usub(a::$UT, b::$UT) = I.checked_usub_int(a, b)
            @noinline _test_checked_smul(a::$ST, b::$ST) = I.checked_smul_int(a, b)
            @noinline _test_checked_umul(a::$UT, b::$UT) = I.checked_umul_int(a, b)
            @noinline _test_udiv(a::$UT, b::$UT) = I.udiv_int(a, b)
            @noinline _test_urem(a::$UT, b::$UT) = I.urem_int(a, b)
            @noinline _test_sdiv(a::$ST, b::$ST) = I.sdiv_int(a, b)
            @noinline _test_srem(a::$ST, b::$ST) = I.srem_int(a, b)
            @noinline _test_checked_sdiv(a::$ST, b::$ST) = I.checked_sdiv_int(a, b)
            @noinline _test_checked_udiv(a::$UT, b::$UT) = I.checked_udiv_int(a, b)
            @noinline _test_checked_srem(a::$ST, b::$ST) = I.checked_srem_int(a, b)
            @noinline _test_checked_urem(a::$UT, b::$UT) = I.checked_urem_int(a, b)
            @noinline _test_flipsign(a::$ST, b::$ST) = I.flipsign_int(a, b)
            @noinline _test_bswap(a::$UT) = I.bswap_int(a)
            @noinline _test_sitofp64(a::$ST)     = I.sitofp(Float64, a)
            @noinline _test_uitofp64(a::$UT)     = I.uitofp(Float64, a)
            @noinline _test_sitofp32(a::$ST)     = I.sitofp(Float32, a)
            @noinline _test_uitofp32(a::$UT)     = I.uitofp(Float32, a)
            @noinline _test_fptosi64(x::Float64) = I.fptosi($ST, x)
            @noinline _test_fptoui64(x::Float64) = I.fptoui($UT, x)
            @noinline _test_fptosi32(x::Float32) = I.fptosi($ST, x)
            @noinline _test_fptoui32(x::Float32) = I.fptoui($UT, x)
        end

        zero_u = make_zero(UT, n)
        zero_s = reinterpret(ST, zero_u)
        neg1_s = reinterpret(ST, make_umax(UT, n))
        smin_s = reinterpret(ST, make_bit(UT, n, n - 1))

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
                for a in allvals
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
                for a in allvals
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
            @testset "eq"  begin for (a,b) in pairs; @test _test_eq(a,b)  === runtime_call(I.eq_int, a, b); end end
            @testset "ne"  begin for (a,b) in pairs; @test _test_ne(a,b)  === runtime_call(I.ne_int, a, b); end end
            @testset "ult" begin for (a,b) in pairs; @test _test_ult(a,b) === runtime_call(I.ult_int, a, b); end end
            @testset "ule" begin for (a,b) in pairs; @test _test_ule(a,b) === runtime_call(I.ule_int, a, b); end end
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
            @testset "ctpop" begin for a in allvals; @test _test_ctpop(a) === runtime_call(I.ctpop_int, a); end end
            @testset "ctlz"  begin for a in allvals; @test _test_ctlz(a)  === runtime_call(I.ctlz_int, a); end end
            @testset "cttz"  begin for a in allvals; @test _test_cttz(a)  === runtime_call(I.cttz_int, a); end end

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
                    sb === zero_s && continue
                    test_compiled_vs_interp(
                        () -> _test_checked_sdiv(sa, sb),
                        () -> runtime_call(I.checked_sdiv_int, sa, sb))
                end
            end
            @testset "checked_udiv" begin
                for (a, b) in pairs
                    b === zero_u && continue
                    test_compiled_vs_interp(
                        () -> _test_checked_udiv(a, b),
                        () -> runtime_call(I.checked_udiv_int, a, b))
                end
            end
            @testset "checked_srem" begin
                for (a, b) in pairs
                    sa, sb = reinterpret(ST, a), reinterpret(ST, b)
                    sb === zero_s && continue
                    test_compiled_vs_interp(
                        () -> _test_checked_srem(sa, sb),
                        () -> runtime_call(I.checked_srem_int, sa, sb))
                end
            end
            @testset "checked_urem" begin
                for (a, b) in pairs
                    b === zero_u && continue
                    test_compiled_vs_interp(
                        () -> _test_checked_urem(a, b),
                        () -> runtime_call(I.checked_urem_int, a, b))
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
                    for a in allvals
                        @test _test_bswap(a) === runtime_call(I.bswap_int, a)
                    end
                end
            end

            # --- FP conversions ---
            @testset "sitofp Float64" begin
                for a in allvals
                    sa = reinterpret(ST, a)
                    @test _test_sitofp64(sa) === runtime_call(I.sitofp, Float64, sa)
                end
            end
            @testset "uitofp Float64" begin
                for a in allvals
                    @test _test_uitofp64(a) === runtime_call(I.uitofp, Float64, a)
                end
            end
            if n <= 128
                @testset "sitofp Float32" begin
                    for a in vcat(allvals, f32_rounding_vals)
                        sa = reinterpret(ST, a)
                        @test _test_sitofp32(sa) === runtime_call(I.sitofp, Float32, sa)
                    end
                end
                @testset "uitofp Float32" begin
                    for a in vcat(allvals, f32_rounding_vals)
                        @test _test_uitofp32(a) === runtime_call(I.uitofp, Float32, a)
                    end
                end
            else
                # FIXME: https://github.com/llvm/llvm-project/issues/189054
                @test_broken false # LLVM bug needs fixing
            end
            @testset "fptosi Float64" begin
                for fv in [0.0, 1.0, -1.0, 42.0, -42.0, 127.0, -128.0]
                    @test _test_fptosi64(fv) === runtime_call(I.fptosi, ST, fv)
                end
            end
            @testset "fptoui Float64" begin
                for fv in [0.0, 1.0, 42.0, 127.0, 255.0]
                    @test _test_fptoui64(fv) === runtime_call(I.fptoui, UT, fv)
                end
            end
            @testset "fptosi Float32" begin
                for fv in Float32[0.0, 1.0, -1.0, 42.0, -42.0, 127.0, -128.0]
                    @test _test_fptosi32(fv) === runtime_call(I.fptosi, ST, fv)
                end
            end
            @testset "fptoui Float32" begin
                for fv in Float32[0.0, 1.0, 42.0, 127.0, 255.0]
                    @test _test_fptoui32(fv) === runtime_call(I.fptoui, UT, fv)
                end
            end
        end
    end
end
