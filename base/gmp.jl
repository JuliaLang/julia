# This file is a part of Julia. License is MIT: https://julialang.org/license

module GMP

export BigInt

import .Base: *, *%, +, +%, -, -%, /, <, <<, >>, >>>, <=, ==, >, >=, ^, ~, &, |, xor,
             binomial, cmp, convert, div, divrem, factorial, cld, fld, gcd, gcdx, lcm, mod,
             ndigits, promote_rule, rem, show, isqrt, string, powermod, sum, prod,
             trailing_zeros, trailing_ones, count_ones, count_zeros, tryparse_internal,
             invmod, _prevpow2, _nextpow2, ndigits0zpb,
             widen, signed, unsafe_trunc, iszero, isone, big, flipsign, signbit,
             sign, isodd, iseven, digits!, hash, hash_integer, top_set_bit,
             ispositive, isnegative, clamp

import Core: Signed, Float16, Float32, Float64

if Clong == Int32
    const ClongMax = Union{Int8, Int16, Int32}
    const CulongMax = Union{UInt8, UInt16, UInt32}
else
    const ClongMax = Union{Int8, Int16, Int32, Int64}
    const CulongMax = Union{UInt8, UInt16, UInt32, UInt64}
end
const CdoubleMax = Union{Float16, Float32, Float64}

if Sys.iswindows()
    const libgmp = "libgmp-10.dll"
elseif Sys.isapple()
    const libgmp = "@rpath/libgmp.10.dylib"
else
    const libgmp = "libgmp.so.10"
end

_version() = unsafe_string(unsafe_load(cglobal((:__gmp_version, libgmp), Ptr{Cchar})))
version() = VersionNumber(_version())
major_version() = _version()[1]
bits_per_limb() = Int(unsafe_load(cglobal((:__gmp_bits_per_limb, libgmp), Cint)))

const VERSION = version()
const MAJOR_VERSION = major_version()
const BITS_PER_LIMB = bits_per_limb()

# GMP's mp_limb_t is by default a typedef of `unsigned long`, but can also be configured to be either
# `unsigned int` or `unsigned long long int`. The correct unsigned type is here named Limb, and must
# be used whenever mp_limb_t is in the signature of ccall'ed GMP functions.
if BITS_PER_LIMB == 32
    const Limb = UInt32
    const SLimbMax = Union{Int8, Int16, Int32}
    const ULimbMax = Union{UInt8, UInt16, UInt32}
elseif BITS_PER_LIMB == 64
    const Limb = UInt64
    const SLimbMax = Union{Int8, Int16, Int32, Int64}
    const ULimbMax = Union{UInt8, UInt16, UInt32, UInt64}
else
    error("GMP: cannot determine the type mp_limb_t (__gmp_bits_per_limb == $BITS_PER_LIMB)")
end

"""
    BigInt <: Signed

Arbitrary precision integer type.
"""
mutable struct BigInt <: Signed
    # sign(size) is the sign of the number, `abs(size)` is the number of significant limbs.
    # Invariants, matching GMP: `abs(size) <= length(d)`, `d[abs(size)] != 0` when `size != 0`.
    size::Int
    d::Memory{Limb}

    global _bigint(size::Int, d::Memory{Limb}) = new(size, d)
    BigInt(; nbits::Integer=0) = new(0, Memory{Limb}(undef, cld(Int(nbits), BITS_PER_LIMB)))
end

"""
    BigInt(x)

Create an arbitrary precision integer. `x` may be an `Int` (or anything that can be
converted to an `Int`). The usual mathematical operators are defined for this type, and
results are promoted to a [`BigInt`](@ref).

Instances can be constructed from strings via [`parse`](@ref), or using the `big`
string literal.

# Examples
```jldoctest
julia> parse(BigInt, "42")
42

julia> big"313"
313

julia> BigInt(10)^19
10000000000000000000
```
"""
BigInt(x)

"""
    ALLOC_OVERFLOW_FUNCTION

A reference that holds a boolean, if true, indicating julia is linked with a patched GMP that
does not abort on huge allocation and throws OutOfMemoryError instead.
"""
const ALLOC_OVERFLOW_FUNCTION = Ref(false)

function __init__()
    try
        if major_version() != MAJOR_VERSION || bits_per_limb() != BITS_PER_LIMB
            msg = """The dynamically loaded GMP library (v\"$(version())\" with __gmp_bits_per_limb == $(bits_per_limb()))
                     does not correspond to the compile time version (v\"$VERSION\" with __gmp_bits_per_limb == $BITS_PER_LIMB).
                     Please rebuild Julia."""
            bits_per_limb() != BITS_PER_LIMB ? @error(msg) : @warn(msg)
        end

        # GMP calls may run under a reset region and re-enter the runtime
        # through these allocation hooks. The hooks unpublish the region
        # around the allocator.
        ccall((:__gmp_set_memory_functions, libgmp), Cvoid,
              (Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid}),
              cglobal(:jl_gmp_counted_malloc),
              cglobal(:jl_gmp_counted_realloc_with_old_size),
              cglobal(:jl_gmp_counted_free_with_size))
    catch ex
        Base.showerror_nostdio(ex, "WARNING: Error during initialization of module GMP")
    end
    # This only works with a patched version of GMP, ignore otherwise
    try
        ccall((:__gmp_set_alloc_overflow_function, libgmp), Cvoid,
              (Ptr{Cvoid},),
              cglobal(:jl_throw_out_of_memory_error))
        ALLOC_OVERFLOW_FUNCTION[] = true
    catch ex
        # ErrorException("ccall: could not find function...")
        if typeof(ex) != ErrorException
            rethrow()
        end
    end
end


module MPZ
# We reimplement the mpz layer of gmp using either mpn or Julia implementations
# so that we can handle the memory management ourselves. For some complicated functions
# we wrap mpz using MPZView rather than BigInt.
using ..GMP: BigInt, Limb, BITS_PER_LIMB, libgmp

# `mpz_t` is `__mpz_struct*`, so `Ref{MPZView}` is ABI-identical.
# Valid only  for calls that never reallocate: those would hand a GC pointer to `free`.
struct MPZView
    alloc::Cint
    size::Cint
    d::Ptr{Limb}
end
const mpz_t = Ref{MPZView}
const bitcnt_t = Culong
const mp_size_t = Clong

@inline _view(a::BigInt) = MPZView(length(a.d) % Cint, a.size % Cint, pointer(a.d))

# `cconvert` hands back the limbs alongside the header, so `ccall` roots them
# for the duration of the call.
Base.cconvert(::Type{mpz_t}, a::BigInt) = (Ref(_view(a)), a.d)
Base.unsafe_convert(::Type{mpz_t}, t::Tuple{Base.RefValue{MPZView},Memory{Limb}}) =
    Base.unsafe_convert(mpz_t, t[1])

# Capacity for `n` limbs. Contents are not preserved and `x.d` may be replaced,
# so a caller that also reads `x`'s old limbs must bind them first.
@inline function _ensure!(x::BigInt, n::Int)
    d = x.d
    if length(d) < n
        d = Memory{Limb}(undef, n)
        x.d = d
    end
    return d
end

# An `n`-limb buffer for an output that must not overlap any of `avoid`:
# `x`'s own limbs when they are big enough and none of those, else a new one.
@inline function _dest(x::BigInt, n::Int, avoid::Vararg{Memory{Limb},N}) where {N}
    d = x.d
    return (length(d) >= n && !any(m -> m === d, avoid)) ? d : Memory{Limb}(undef, n)
end

# Publish `n` limbs of `d` as `x`'s value, with sign `neg`. Every write ends
# here, which is what restores `BigInt`'s invariants.
@inline function _finish!(x::BigInt, d::Memory{Limb}, n::Int, neg::Bool)
    x.d === d || (x.d = d)
    sz = something(findlast(!iszero, @view d[1:n]), 0)
    x.size = neg ? -sz : sz
    return x
end

@inline function _magsign(a)
    s = Int64(a)
    u = s % UInt64
    return (s < 0 ? -u : u, s < 0)
end

# Split `u` into limbs, writing them at `d[off+1:]`.
@inline function _store!(d::Memory{Limb}, off::Int, u::Union{UInt64,UInt128})
    @inbounds for i in 1:cld(8*sizeof(u), BITS_PER_LIMB)
        d[off+i] = (u >> ((i - 1) * BITS_PER_LIMB)) % Limb
    end
    return d
end

realloc2!(x::BigInt, a) = (_ensure!(x, cld(Int(a), BITS_PER_LIMB)); x)
realloc2(a) = realloc2!(BigInt(), a)

# A GMP-owned mpz, for the operations with no suitable `mpn` form.
mutable struct Scratch
    alloc::Cint
    size::Cint
    d::Ptr{Limb}
    Scratch() = new(0, 0, C_NULL)
end
const scratch_t = Ref{Scratch}

# Copy a GMP-owned mpz, given as its signed size and limb pointer, into `x`.
function _take!(x::BigInt, sz::Cint, p::Ptr{Limb})
    n = abs(Int(sz))
    d = _ensure!(x, n)
    n > 0 && GC.@preserve d unsafe_copyto!(pointer(d), p, n)
    x.size = Int(sz)
    return x
end

# Run `f` on a GMP-owned output mpz, move it into `x`, free it, return `f`'s value.
function with_output(f::F, x::BigInt) where {F}
    s = Scratch()
    ccall((:__gmpz_init, libgmp), Cvoid, (scratch_t,), s)
    try
        ret = f(s)
        _take!(x, s.size, s.d)
        return ret
    finally
        ccall((:__gmpz_clear, libgmp), Cvoid, (scratch_t,), s)
    end
end

# Limb-array kernels; callers must respect each one's overlap rules. `ccall`
# roots a `Memory`, so no caller needs `GC.@preserve`. `ro`/`uo` are the limb
# offsets of the shifts, the only kernels not applied at a buffer's start.
const MP = Memory{Limb}

_mpn_add(r::MP, u::MP, un, v::MP, vn) = ccall((:__gmpn_add, libgmp), Limb,
    (Ptr{Limb}, Ptr{Limb}, mp_size_t, Ptr{Limb}, mp_size_t), r, u, un, v, vn)
_mpn_add_1(r::MP, u::MP, un, v) = ccall((:__gmpn_add_1, libgmp), Limb,
    (Ptr{Limb}, Ptr{Limb}, mp_size_t, Limb), r, u, un, v)
_mpn_sub(r::MP, u::MP, un, v::MP, vn) = ccall((:__gmpn_sub, libgmp), Limb,
    (Ptr{Limb}, Ptr{Limb}, mp_size_t, Ptr{Limb}, mp_size_t), r, u, un, v, vn)
_mpn_sub_1(r::MP, u::MP, un, v) = ccall((:__gmpn_sub_1, libgmp), Limb,
    (Ptr{Limb}, Ptr{Limb}, mp_size_t, Limb), r, u, un, v)
_mpn_mul_1(r::MP, u::MP, un, v) = ccall((:__gmpn_mul_1, libgmp), Limb,
    (Ptr{Limb}, Ptr{Limb}, mp_size_t, Limb), r, u, un, v)
_mpn_cmp(u::MP, v::MP, n) = ccall((:__gmpn_cmp, libgmp), Cint,
    (Ptr{Limb}, Ptr{Limb}, mp_size_t), u, v, n)
# Single-limb division, which needs no buffer for the remainder: both return
# it. `mpn_divrem_1` allows its quotient to be the numerator buffer itself.
_mpn_divrem_1(q::MP, u::MP, un, v) = ccall((:__gmpn_divrem_1, libgmp), Limb,
    (Ptr{Limb}, mp_size_t, Ptr{Limb}, mp_size_t, Limb), q, 0, u, un, v)
_mpn_mod_1(u::MP, un, v) = ccall((:__gmpn_mod_1, libgmp), Limb,
    (Ptr{Limb}, mp_size_t, Limb), u, un, v)
_mpn_lshift(r::MP, ro::Int, u::MP, n, cnt) = GC.@preserve r ccall((:__gmpn_lshift, libgmp),
    Limb, (Ptr{Limb}, Ptr{Limb}, mp_size_t, Cuint), pointer(r, ro+1), u, n, cnt)
_mpn_rshift(r::MP, u::MP, uo::Int, n, cnt) = GC.@preserve u ccall((:__gmpn_rshift, libgmp),
    Limb, (Ptr{Limb}, Ptr{Limb}, mp_size_t, Cuint), r, pointer(u, uo+1), n, cnt)

# The superlinear kernels are the ones worth abandoning mid-flight, so they
# are `:reset_safe`. An unwind discards their output buffer, and the
# allocation hooks free any GMP temporary, so nothing that matters leaks.
_mpn_mul(r::MP, u::MP, un, v::MP, vn) = Base.@assume_effects :reset_safe @ccall libgmp.__gmpn_mul(
    r::Ptr{Limb}, u::Ptr{Limb}, un::mp_size_t, v::Ptr{Limb}, vn::mp_size_t)::Limb
_mpn_sqr(r::MP, u::MP, un) = Base.@assume_effects :reset_safe @ccall libgmp.__gmpn_sqr(
    r::Ptr{Limb}, u::Ptr{Limb}, un::mp_size_t)::Cvoid
_mpn_tdiv_qr(q::MP, r::MP, n::MP, nn, d::MP, dn) = Base.@assume_effects :reset_safe @ccall libgmp.__gmpn_tdiv_qr(
    q::Ptr{Limb}, r::Ptr{Limb}, 0::mp_size_t, n::Ptr{Limb}, nn::mp_size_t,
    d::Ptr{Limb}, dn::mp_size_t)::Cvoid
# the remainder is never wanted here, so `rp` is always NULL
_mpn_sqrt(s::MP, u::MP, n) = Base.@assume_effects :reset_safe @ccall libgmp.__gmpn_sqrtrem(
    s::Ptr{Limb}, C_NULL::Ptr{Limb}, u::Ptr{Limb}, n::mp_size_t)::mp_size_t

sizeinbase(a::BigInt, b) = Int(ccall((:__gmpz_sizeinbase, libgmp), Csize_t, (mpz_t, Cint), a, b))

function _ucmp(ad::Memory{Limb}, an::Int, bd::Memory{Limb}, bn::Int)
    an != bn && return an < bn ? -1 : 1
    an == 0 && return 0
    return Int(_mpn_cmp(ad, bd, an % mp_size_t))
end

# Move `y`'s limbs into `x`. `y` must be a temporary that no longer escapes.
@inline _move!(x::BigInt, y::BigInt) = (x.d = y.d; x.size = y.size; x)

function set!(x::BigInt, a::BigInt)
    x === a && return x
    n = abs(a.size)
    ad = a.d
    d = _ensure!(x, n)
    copyto!(d, 1, ad, 1, n)
    x.size = a.size
    return x
end
set(a::BigInt) = set!(BigInt(), a)

function _addsub!(x::BigInt, a::BigInt, b::BigInt, negb::Bool)
    as = a.size
    bs = negb ? -b.size : b.size
    # `mpn_add`/`mpn_sub` require a positive limb count, so zero stops here.
    bs == 0 && return set!(x, a)
    as == 0 && return negb ? neg!(x, b) : set!(x, b)
    an, bn = abs(as), abs(bs)
    # Bind the operands before `_ensure!` can replace `x.d` out from under them.
    ad, bd = a.d, b.d
    if (as > 0) == (bs > 0)
        an < bn && ((ad, an, bd, bn) = (bd, bn, ad, an)) # longer operand first
        d = _ensure!(x, an + 1)
        c = _mpn_add(d, ad, an % mp_size_t, bd, bn % mp_size_t)
        @inbounds d[an+1] = c
        return _finish!(x, d, an + 1, as < 0)
    end
    rel = _ucmp(ad, an, bd, bn)
    rel == 0 && (x.size = 0; return x)
    rel < 0 && ((ad, an, as, bd, bn) = (bd, bn, bs, ad, an))
    d = _ensure!(x, an)
    _mpn_sub(d, ad, an % mp_size_t, bd, bn % mp_size_t)
    return _finish!(x, d, an, as < 0)
end

add!(x::BigInt, a::BigInt, b::BigInt) = _addsub!(x, a, b, false)
sub!(x::BigInt, a::BigInt, b::BigInt) = _addsub!(x, a, b, true)

# x = a ± v. Here and below, `v` is an unsigned magnitude and `neg` its sign.
function _addsub_mag!(x::BigInt, a::BigInt, v::Limb, neg::Bool)
    v == 0 && return set!(x, a)
    as = a.size
    an = abs(as)
    ad = a.d
    if as != 0 && (as > 0) != neg      # magnitudes add
        d = _ensure!(x, an + 1)
        @inbounds d[an+1] = _mpn_add_1(d, ad, an % mp_size_t, v)
        return _finish!(x, d, an + 1, as < 0)
    elseif an > 1 || (an == 1 && (@inbounds ad[1]) >= v)   # |a| >= v
        d = _ensure!(x, an)
        _mpn_sub_1(d, ad, an % mp_size_t, v)
        return _finish!(x, d, an, as < 0)
    end
    # |a| < v, which `v` being one limb confines to `an <= 1`, so the result
    # is the single limb `v - |a|`, with `v`'s sign.
    d = _ensure!(x, 1)
    @inbounds d[1] = v - (an == 0 ? zero(Limb) : (@inbounds ad[1]))
    return _finish!(x, d, 1, neg)
end

add_ui!(x::BigInt, a::BigInt, b) = _addsub_mag!(x, a, Limb(b), false)
sub_ui!(x::BigInt, a::BigInt, b) = _addsub_mag!(x, a, Limb(b), true)
# x = a - b, with `a` an unsigned magnitude
ui_sub!(x::BigInt, a, b::BigInt) = (_addsub_mag!(x, b, Limb(a), true); x.size = -x.size; x)
ui_sub(a, b::BigInt) = ui_sub!(BigInt(), a, b)

_inc!(x::BigInt) = _addsub_mag!(x, x, one(Limb), false)
_dec!(x::BigInt) = _addsub_mag!(x, x, one(Limb), true)

function mul!(x::BigInt, a::BigInt, b::BigInt)
    Base.@cancel_check
    as, bs = a.size, b.size
    if as == 0 || bs == 0
        x.size = 0
        return x
    end
    an, bn = abs(as), abs(bs)
    ad, bd = a.d, b.d
    n = an + bn
    # mpn_mul forbids overlap, so unalias x if needed.
    d = _dest(x, n, ad, bd)
    if ad === bd && an == bn
        _mpn_sqr(d, ad, an % mp_size_t)
    elseif an >= bn
        _mpn_mul(d, ad, an % mp_size_t, bd, bn % mp_size_t)
    else
        _mpn_mul(d, bd, bn % mp_size_t, ad, an % mp_size_t)
    end
    return _finish!(x, d, n, (as < 0) != (bs < 0))
end

function _mul_mag!(x::BigInt, a::BigInt, v::Limb, neg::Bool)
    as = a.size
    if as == 0 || v == 0
        x.size = 0
        return x
    end
    an = abs(as)
    ad = a.d
    d = _ensure!(x, an + 1)
    @inbounds d[an+1] = _mpn_mul_1(d, ad, an % mp_size_t, v)
    return _finish!(x, d, an + 1, (as < 0) != neg)
end

mul_ui!(x::BigInt, a::BigInt, b) = _mul_mag!(x, a, Limb(b), false)
mul_si!(x::BigInt, a::BigInt, b) = ((v, neg) = _magsign(b); _mul_mag!(x, a, Limb(v), neg))

# Truncated division: q = trunc(a/b) and r = a - q*b, so `r` carries `a`'s
# sign. `mpn_tdiv_qr` writes both outputs and forbids overlap among them and
# its inputs, so each variant below gives the output it does not want scratch
# limbs, and only the wanted one a `_dest` buffer.
function tdiv_q!(x::BigInt, a::BigInt, b::BigInt)
    Base.@cancel_check
    as, bs = a.size, b.size
    bs == 0 && throw(DivideError())
    an, bn = abs(as), abs(bs)
    an < bn && (x.size = 0; return x)
    ad, bd = a.d, b.d
    neg = (as < 0) != (bs < 0)
    if bn == 1
        v = @inbounds bd[1]
        qd = _ensure!(x, an)
        _mpn_divrem_1(qd, ad, an % mp_size_t, v)
        return _finish!(x, qd, an, neg)
    end
    qn = an - bn + 1
    qd = _dest(x, qn, ad, bd)
    _mpn_tdiv_qr(qd, Memory{Limb}(undef, bn), ad, an % mp_size_t, bd, bn % mp_size_t)
    return _finish!(x, qd, qn, neg)
end

function tdiv_r!(x::BigInt, a::BigInt, b::BigInt)
    Base.@cancel_check
    as, bs = a.size, b.size
    bs == 0 && throw(DivideError())
    an, bn = abs(as), abs(bs)
    an < bn && return set!(x, a)
    ad, bd = a.d, b.d
    if bn == 1
        rl = _mpn_mod_1(ad, an % mp_size_t, @inbounds bd[1])
        d = _ensure!(x, 1)
        @inbounds d[1] = rl
        return _finish!(x, d, 1, as < 0)
    end
    rd = _dest(x, bn, ad, bd)
    _mpn_tdiv_qr(Memory{Limb}(undef, an - bn + 1), rd, ad, an % mp_size_t, bd, bn % mp_size_t)
    return _finish!(x, rd, bn, as < 0)
end

function tdiv_qr!(x::BigInt, y::BigInt, a::BigInt, b::BigInt)
    Base.@cancel_check
    as, bs = a.size, b.size
    bs == 0 && throw(DivideError())
    an, bn = abs(as), abs(bs)
    if an < bn
        x.size = 0
        set!(y, a)
        return x, y
    end
    ad, bd = a.d, b.d
    neg = (as < 0) != (bs < 0)
    if bn == 1
        v = @inbounds bd[1]
        qd = _dest(x, an, ad, bd)
        rl = _mpn_divrem_1(qd, ad, an % mp_size_t, v)
        rdy = _ensure!(y, 1)
        @inbounds rdy[1] = rl
        _finish!(x, qd, an, neg)
        _finish!(y, rdy, 1, as < 0)
        return x, y
    end
    qn = an - bn + 1
    qd = _dest(x, qn, ad, bd)
    rd = _dest(y, bn, ad, bd, qd)
    _mpn_tdiv_qr(qd, rd, ad, an % mp_size_t, bd, bn % mp_size_t)
    _finish!(x, qd, qn, neg)
    _finish!(y, rd, bn, as < 0)
    return x, y
end
tdiv_qr(a::BigInt, b::BigInt) = tdiv_qr!(BigInt(), BigInt(), a, b)

# Floor (`down`) and ceiling division. The truncated result is off by one
# exactly when the remainder is nonzero and carries the "wrong" sign, which is
# `a`'s, so `wrong` is known before the division runs.
function _rdiv_q!(x::BigInt, a::BigInt, b::BigInt, down::Bool)
    Base.@cancel_check
    as, bs = a.size, b.size
    bs == 0 && throw(DivideError())
    an, bn = abs(as), abs(bs)
    wrong = ((as < 0) != (bs < 0)) == down
    # |a| < |b|: the truncated quotient is 0 and the remainder is `a`.
    an < bn && return set_si!(x, (as != 0 && wrong) ? (down ? -1 : 1) : 0)
    ad, bd = a.d, b.d
    neg = (as < 0) != (bs < 0)
    # Only whether the discarded remainder is nonzero matters.
    if bn == 1
        v = @inbounds bd[1]
        qd = _ensure!(x, an)
        rnz = _mpn_divrem_1(qd, ad, an % mp_size_t, v) != 0
        _finish!(x, qd, an, neg)
    else
        qn = an - bn + 1
        qd = _dest(x, qn, ad, bd)
        rd = Memory{Limb}(undef, bn)
        _mpn_tdiv_qr(qd, rd, ad, an % mp_size_t, bd, bn % mp_size_t)
        _finish!(x, qd, qn, neg)
        rnz = any(!iszero, @view rd[1:bn])
    end
    wrong && rnz && (down ? _dec!(x) : _inc!(x))
    return x
end

function _rdiv_r!(x::BigInt, a::BigInt, b::BigInt, down::Bool)
    bneg = b.size < 0
    r = x === b ? BigInt() : x   # `b` is still needed after the division
    tdiv_r!(r, a, b)
    rs = r.size
    rs != 0 && ((rs < 0) != bneg) == down && _addsub!(r, r, b, !down)
    return r === x ? x : _move!(x, r)
end

fdiv_q!(x::BigInt, a::BigInt, b::BigInt) = _rdiv_q!(x, a, b, true)
cdiv_q!(x::BigInt, a::BigInt, b::BigInt) = _rdiv_q!(x, a, b, false)
fdiv_r!(x::BigInt, a::BigInt, b::BigInt) = _rdiv_r!(x, a, b, true)
cdiv_r!(x::BigInt, a::BigInt, b::BigInt) = _rdiv_r!(x, a, b, false)

# mpn_gcd destroys its operands and constrains their normalization and parity,
# so driving it from here would cost the same copying that mpz does anyway.
for op in (:gcd, :lcm)
    fname = Symbol(:__gmpz_, op)
    @eval function $(Symbol(op, :!))(x::BigInt, a::BigInt, b::BigInt)
        Base.@cancel_check
        with_output(x) do s
            Base.@assume_effects :reset_safe @ccall libgmp.$fname(s::scratch_t, a::mpz_t, b::mpz_t)::Cvoid
        end
        return x
    end
end

# GMP never exported mpn_and_n and friends, and mpz's semantics are those of
# an infinite two's-complement expansion, so these are done over limbs here.

# Limb `i` of the infinite two's-complement expansion of the value with
# magnitude `d[1:n]`. Negated, that is the magnitude complemented above its
# lowest nonzero limb `low`, where the +1 of the negation lands.
@inline function _tc(d::Memory{Limb}, n::Int, neg::Bool, low::Int, i::Int)
    v = i <= n ? (@inbounds d[i]) : zero(Limb)
    if neg
        v = i < low ? zero(Limb) : (i == low ? (-v) : ~v)
    end
    return v
end

function _bitop!(op::F, x::BigInt, a::BigInt, b::BigInt) where {F}
    as, bs = a.size, b.size
    an, bn = abs(as), abs(bs)
    aneg, bneg = as < 0, bs < 0
    # Bind before `_ensure!`; `d` may then be `ad` or `bd`, which the upward
    # scans tolerate.
    ad, bd = a.d, b.d
    if !aneg && !bneg
        # A non-negative magnitude is its own two's-complement expansion,
        # sign-extended with zeros, so `&` stops at the shorter operand while
        # `|`/`xor` pass the longer one's tail through. Keeps `bignum & small`
        # linear in the *small* operand.
        lo, hi = minmax(an, bn)
        hd = an >= bn ? ad : bd
        n = iszero(op(~zero(Limb), zero(Limb))) ? lo : hi
        d = _ensure!(x, n)
        @inbounds for i in 1:lo
            d[i] = op(ad[i], bd[i])
        end
        @inbounds for i in lo+1:n
            d[i] = op(hd[i], zero(Limb))
        end
        return _finish!(x, d, n, false)
    end
    alow = aneg ? findfirst(!iszero, @view ad[1:an])::Int : 0
    blow = bneg ? findfirst(!iszero, @view bd[1:bn])::Int : 0
    # One limb beyond both operands, holding each one's sign extension: 0 or
    # ~0, so it records the result's sign and absorbs the negation's carry.
    n = max(an, bn) + 1
    d = _ensure!(x, n)
    @inbounds for i in 1:n
        d[i] = op(_tc(ad, an, aneg, alow, i), _tc(bd, bn, bneg, blow, i))
    end
    neg = (@inbounds d[n]) != 0
    if neg
        # Back to sign-magnitude. The sign-extension limb is `~0`, so it
        # complements to 0 and always absorbs the carry.
        @inbounds for i in 1:n
            d[i] = ~d[i]
        end
        _mpn_add_1(d, d, n % mp_size_t, one(Limb))
    end
    return _finish!(x, d, n, neg)
end

and!(x::BigInt, a::BigInt, b::BigInt) = _bitop!(&, x, a, b)
ior!(x::BigInt, a::BigInt, b::BigInt) = _bitop!(|, x, a, b)
xor!(x::BigInt, a::BigInt, b::BigInt) = _bitop!(Base.xor, x, a, b)

function mul_2exp!(x::BigInt, a::BigInt, c)
    s = Int(c)
    as = a.size
    (as == 0 || s == 0) && return set!(x, a)
    an = abs(as)
    ad = a.d
    off, sh = divrem(s, BITS_PER_LIMB)
    n = an + off + (sh > 0)
    d = Memory{Limb}(undef, n)
    if sh == 0
        copyto!(d, off+1, ad, 1, an)
    else
        @inbounds d[an+off+1] = _mpn_lshift(d, off, ad, an % mp_size_t, sh % Cuint)
    end
    fill!(@view(d[1:off]), zero(Limb))
    return _finish!(x, d, n, as < 0)
end

# Arithmetic shift right, rounding towards -Inf as mpz_fdiv_q_2exp does.
function fdiv_q_2exp!(x::BigInt, a::BigInt, c)
    s = Int(c)
    as = a.size
    (as == 0 || s == 0) && return set!(x, a)
    an = abs(as)
    ad = a.d
    neg = as < 0
    off, sh = divrem(s, BITS_PER_LIMB)
    if off >= an
        # Everything is shifted out; flooring leaves -1 for a negative value.
        if neg
            d = _ensure!(x, 1)
            @inbounds d[1] = one(Limb)
            x.size = -1
        else
            x.size = 0
        end
        return x
    end
    # Flooring rounds away from zero when a negative value loses a set bit.
    lost = neg && (any(!iszero, @view ad[1:off]) ||
                   (sh > 0 && (@inbounds ad[off+1]) & ((one(Limb) << sh) - one(Limb)) != 0))
    n = an - off
    d = Memory{Limb}(undef, n)
    if sh == 0
        copyto!(d, 1, ad, off+1, n)
    else
        _mpn_rshift(d, ad, off, n % mp_size_t, sh % Cuint)
    end
    _finish!(x, d, n, neg)
    lost && _dec!(x)
    return x
end

neg!(x::BigInt, a::BigInt) = (set!(x, a); x.size = -x.size; x)
# ~a == -a - 1
com!(x::BigInt, a::BigInt) = (neg!(x, a); _dec!(x))

function sqrt!(x::BigInt, a::BigInt)
    Base.@cancel_check
    as = a.size
    as < 0 && throw(DomainError(a, "`x` must be non-negative"))
    if as == 0
        x.size = 0
        return x
    end
    an = as # `as > 0` here, so the size and the limb count agree
    ad = a.d
    sn = cld(an, 2)
    d = Memory{Limb}(undef, sn)
    _mpn_sqrt(d, ad, an % mp_size_t)
    return _finish!(x, d, sn, false)
end

# Operations with no suitable `mpn` form: computed into a GMP-owned mpz, with
# read-only inputs left as views. `z` marks a `BigInt` argument, `u` a `Culong`.
for (op, ret, kinds) in ((:invert, :Cint,  (:z, :z)),
                         (:powm,   :Cvoid, (:z, :z, :z)),
                         (:pow_ui, :Cvoid, (:z, :u)),
                         (:bin_ui, :Cvoid, (:z, :u)),
                         (:fac_ui, :Cvoid, (:u,)))
    fname = Symbol(:__gmpz_, op)
    ps = [Symbol(:a, i) for i in eachindex(kinds)]
    decls = [k === :z ? :($p::BigInt) : p for (p, k) in zip(ps, kinds)]
    cargs = [k === :z ? :($p::mpz_t) : :($p::Culong) for (p, k) in zip(ps, kinds)]
    out = ret === :Cvoid ? :x : :(Int(r))
    @eval function $(Symbol(op, :!))(x::BigInt, $(decls...))
        Base.@cancel_check
        r = with_output(x) do s
            Base.@assume_effects :reset_safe @ccall libgmp.$fname(s::scratch_t, $(cargs...))::$ret
        end
        return $out
    end
end

invert!(x::BigInt, b::BigInt) = invert!(x, x, b)
invert(a::BigInt, b::BigInt) = (ret = BigInt(); invert!(ret, a, b); ret)
powm(a::BigInt, b::BigInt, c::BigInt) = powm!(BigInt(), a, b, c)
powm!(x::BigInt, b::BigInt, c::BigInt) = powm!(x, x, b, c)
fac_ui(a) = fac_ui!(BigInt(), a)

function gcdext!(x::BigInt, y::BigInt, z::BigInt, a::BigInt, b::BigInt)
    Base.@cancel_check
    with_output(x) do sg
        with_output(y) do ss
            with_output(z) do st
                Base.@assume_effects :reset_safe @ccall libgmp.__gmpz_gcdext(
                    sg::scratch_t, ss::scratch_t, st::scratch_t, a::mpz_t, b::mpz_t)::Cvoid
            end
        end
    end
    return x, y, z
end
gcdext(a::BigInt, b::BigInt) = gcdext!(BigInt(), BigInt(), BigInt(), a, b)

set_str!(x::BigInt, a, b) = with_output(x) do s
    Base.@cancel_check
    Int(Base.@assume_effects :reset_safe @ccall libgmp.__gmpz_set_str(s::scratch_t, a::Ptr{UInt8}, b::Cint)::Cint)
end

function _set_mag!(x::BigInt, u::UInt64, neg::Bool)
    n = cld(64, BITS_PER_LIMB)
    d = _ensure!(x, n)
    return _finish!(x, _store!(d, 0, u), n, neg)
end

set_ui!(x::BigInt, a) = _set_mag!(x, UInt64(a), false)
set_si!(x::BigInt, a) = ((u, neg) = _magsign(a); _set_mag!(x, u, neg))

# Truncates towards zero, as mpz_set_d does.
function set_d!(x::BigInt, a)
    v = Float64(a)
    if -1.0 < v < 1.0
        x.size = 0
        return x
    end
    m, ex = frexp(abs(v))          # abs(v) == m * 2^ex, with 0.5 <= m < 1
    sig = unsafe_trunc(UInt64, ldexp(m, 53))  # exact: the 53-bit significand
    e = ex - 53                    # abs(v) truncated == sig * 2^e
    if e < 0
        sig >>= -e                 # truncate towards zero
        e = 0
    end
    off, sh = divrem(e, BITS_PER_LIMB)
    w = UInt128(sig) << sh         # at most 53 + 63 bits
    n = off + cld(128, BITS_PER_LIMB)
    d = _ensure!(x, n)
    fill!(@view(d[1:off]), zero(Limb))
    return _finish!(x, _store!(d, off, w), n, v < 0)
end

for op in (:set_ui, :set_si, :set_d)
    op! = Symbol(op, :!)
    @eval $op(a) = $op!(BigInt(), a)
end

for op in (:add, :sub, :mul, :fdiv_q, :tdiv_q, :cdiv_q, :fdiv_r, :tdiv_r, :cdiv_r,
           :gcd, :lcm, :and, :ior, :xor)
    op! = Symbol(op, :!)
    @eval begin
        $op(a::BigInt, b::BigInt) = $op!(BigInt(), a, b)
        $op!(x::BigInt, b::BigInt) = $op!(x, x, b)
    end
end

for op in (:add_ui, :sub_ui, :mul_ui, :mul_si, :mul_2exp, :fdiv_q_2exp, :pow_ui, :bin_ui)
    op! = Symbol(op, :!)
    @eval begin
        $op(a::BigInt, b) = $op!(BigInt(), a, b)
        $op!(x::BigInt, b) = $op!(x, x, b)
    end
end

for op in (:neg, :com, :sqrt)
    op! = Symbol(op, :!)
    @eval begin
        $op(a::BigInt) = $op!(BigInt(), a)
        $op!(x::BigInt) = $op!(x, x)
    end
end

# None of these writes its operand, so a borrowed view is sound throughout.

for op in (:scan1, :scan0)
    # when there is no meaningful answer, ccall returns typemax(Culong), where Culong can
    # be UInt32 (Windows) or UInt64; we return -1 in this case for all architectures
    fname = Symbol(:__gmpz_, op)
    @eval $op(a::BigInt, b) = Int(signed(ccall(($(QuoteNode(fname)), libgmp), Culong, (mpz_t, Culong), a, b)))
end

popcount(a::BigInt) = Int(signed(ccall((:__gmpz_popcount, libgmp), Culong, (mpz_t,), a)))

mpn_popcount(d::MP, s::Integer) = Int(ccall((:__gmpn_popcount, libgmp), Culong, (Ptr{Limb}, Csize_t), d, s))
mpn_popcount(a::BigInt) = mpn_popcount(a.d, abs(a.size))

cmp(a::BigInt, b::BigInt) = Int(ccall((:__gmpz_cmp, libgmp), Cint, (mpz_t, mpz_t), a, b))
cmp_si(a::BigInt, b) = Int(ccall((:__gmpz_cmp_si, libgmp), Cint, (mpz_t, Clong), a, b))
cmp_ui(a::BigInt, b) = Int(ccall((:__gmpz_cmp_ui, libgmp), Cint, (mpz_t, Culong), a, b))
cmp_d(a::BigInt, b) = Int(ccall((:__gmpz_cmp_d, libgmp), Cint, (mpz_t, Cdouble), a, b))

mpn_cmp(a::BigInt, b::BigInt, c) = _mpn_cmp(a.d, b.d, c)

get_str!(x, a, b::BigInt) = (Base.@cancel_check; Base.@assume_effects :reset_safe @ccall(libgmp.__gmpz_get_str(x::Ptr{Cchar}, a::Cint, b::mpz_t)::Ptr{Cchar}); x)
get_d(a::BigInt) = ccall((:__gmpz_get_d, libgmp), Cdouble, (mpz_t,), a)

tstbit(a::BigInt, b) = ccall((:__gmpz_tstbit, libgmp), Cint, (mpz_t, bitcnt_t), a, b) % Bool

function export!(a::AbstractVector{T}, n::BigInt; order::Integer=-1, nails::Integer=0, endian::Integer=0) where {T<:Base.BitInteger}
    stride(a, 1) == 1 || throw(ArgumentError("a must have stride 1"))
    ndigits = cld(sizeinbase(n, 2), 8*sizeof(T) - nails)
    length(a) < ndigits && resize!(a, ndigits)
    fill!(a, zero(T))
    count = Ref{Csize_t}()
    ccall((:__gmpz_export, libgmp), Ptr{T}, (Ptr{T}, Ref{Csize_t}, Cint, Csize_t, Cint, Csize_t, mpz_t),
        a, count, order, sizeof(T), endian, nails, n)
    @assert count[] ≤ length(a) "count[] > length(a)"
    return a, Int(count[])
end

# mpz_setbit's semantics are those of an infinite two's-complement expansion.
function setbit!(x::BigInt, a)
    b = Int(a)
    n = x.size
    if n >= 0
        # One limb's OR. Worth doing in place: the only caller sets bits in a
        # loop, and the scratch path below copies all of `x` twice per bit.
        i = (b ÷ BITS_PER_LIMB) + 1
        d = x.d
        if i > n
            if length(d) < i        # grow, preserving the limbs already set
                nd = Memory{Limb}(undef, i)
                copyto!(nd, 1, d, 1, n)
                x.d = d = nd
            end
            fill!(@view(d[n+1:i]), zero(Limb))
            x.size = i
        end
        @inbounds d[i] |= one(Limb) << (b % BITS_PER_LIMB)
        return x
    end
    # Stored sign-magnitude, so the bit to set is not a bit of any stored
    # limb; let mpz do it, on a scratch seeded from `x`.
    with_output(x) do s
        ccall((:__gmpz_set, libgmp), Cvoid, (scratch_t, mpz_t), s, x)
        ccall((:__gmpz_setbit, libgmp), Cvoid, (scratch_t, bitcnt_t), s, a)
    end
    return x
end

end # module MPZ

const ZERO = BigInt()
const ONE  = MPZ.set_ui(one(Limb))

widen(::Type{Int128})  = BigInt
widen(::Type{UInt128}) = BigInt
widen(::Type{BigInt})  = BigInt

signed(x::BigInt) = x

BigInt(x::BigInt) = x
Signed(x::BigInt) = x

function tryparse_internal(::Type{BigInt}, s::AbstractString, startpos::Int, endpos::Int, base_::Integer, raise::Bool)
    # don't make a copy in the common case where we are parsing a whole String
    bstr = startpos == firstindex(s) && endpos == lastindex(s) ? String(s) : String(SubString(s,startpos,endpos))

    sgn, base, i = Base.parseint_preamble(true,Int(base_),bstr,firstindex(bstr),lastindex(bstr))
    if !(2 <= base <= 62)
        raise && throw(ArgumentError("invalid base: base must be 2 ≤ base ≤ 62, got $base"))
        return nothing
    end
    if i == 0
        raise && throw(ArgumentError("premature end of integer: $(repr(bstr))"))
        return nothing
    end
    z = BigInt()
    if Base.containsnul(bstr)
        err = -1 # embedded NUL char (not handled correctly by GMP)
    else
        err = GC.@preserve bstr MPZ.set_str!(z, pointer(bstr)+(i-firstindex(bstr)), base)
    end
    if err != 0
        raise && throw(ArgumentError("invalid BigInt: $(repr(bstr))"))
        return nothing
    end
    flipsign!(z, sgn)
end

BigInt(x::Union{Clong,Int32}) = MPZ.set_si(x)
BigInt(x::Union{Culong,UInt32}) = MPZ.set_ui(x)
BigInt(x::Bool) = BigInt(UInt(x))

unsafe_trunc(::Type{BigInt}, x::Union{Float16,Float32,Float64}) = MPZ.set_d(x)

function BigInt(x::Float64)
    isinteger(x) || throw(InexactError(:BigInt, BigInt, x))
    unsafe_trunc(BigInt,x)
end

BigInt(x::Float16) = BigInt(Float64(x))
BigInt(x::Float32) = BigInt(Float64(x))

function BigInt(x::Integer)
    # On 64-bit Windows, `Clong` is `Int32`, not `Int64`, so construction of
    # `Int64` constants, e.g. `BigInt(3)`, uses this method.
    isbits(x) && typemin(Clong) <= x <= typemax(Clong) && return BigInt((x % Clong)::Clong)
    nd = ndigits(x, base=2)
    z = MPZ.realloc2(nd)
    d = z.d
    ux = unsigned(x < 0 ? -%(x) : x)
    size = 0
    limbnbits = sizeof(Limb) << 3
    while nd > 0
        size += 1
        @inbounds d[size] = ux % Limb
        ux >>= limbnbits
        nd -= limbnbits
    end
    z.size = x < 0 ? -size : size
    z
end


rem(x::BigInt, ::Type{Bool}) = iszero(x) ? false : ((@inbounds x.d[1]) % Bool)

rem(x::BigInt, ::Type{T}) where T<:Union{SLimbMax,ULimbMax} =
    iszero(x) ? zero(T) : flipsign((@inbounds x.d[1]) % T, x.size)

function rem(x::BigInt, ::Type{T}) where T<:Union{Base.BitUnsigned,Base.BitSigned}
    u = zero(T)
    d = x.d
    for l = 1:min(abs(x.size), cld(sizeof(T), sizeof(Limb)))
        u += ((@inbounds d[l]) % T) << ((sizeof(Limb)<<3)*(l-1))
    end
    flipsign(u, x.size)
end

rem(x::Integer, ::Type{BigInt}) = BigInt(x)

clamp(x, ::Type{BigInt}) = convert(BigInt, x)

isodd(x::BigInt) = MPZ.tstbit(x, 0)
iseven(x::BigInt) = !isodd(x)

function (::Type{T})(x::BigInt) where T<:Base.BitUnsigned
    if sizeof(T) < sizeof(Limb)
        convert(T, convert(Limb,x))
    else
        0 <= x.size <= cld(sizeof(T),sizeof(Limb)) || throw(InexactError(nameof(T), T, x))
        x % T
    end
end

function (::Type{T})(x::BigInt) where T<:Base.BitSigned
    n = abs(x.size)
    if sizeof(T) < sizeof(Limb)
        SLimb = typeof(Signed(one(Limb)))
        convert(T, convert(SLimb, x))
    else
        0 <= n <= cld(sizeof(T),sizeof(Limb)) || throw(InexactError(nameof(T), T, x))
        y = x % T
        ispositive(x) ⊻ (y > 0) && throw(InexactError(nameof(T), T, x)) # catch overflow
        y
    end
end


Float64(n::BigInt, ::RoundingMode{:ToZero}) = MPZ.get_d(n)

function (::Type{T})(n::BigInt, ::RoundingMode{:ToZero}) where T<:Union{Float16,Float32}
    T(Float64(n,RoundToZero),RoundToZero)
end

function (::Type{T})(n::BigInt, ::RoundingMode{:Down}) where T<:CdoubleMax
    x = T(n,RoundToZero)
    x > n ? prevfloat(x) : x
end
function (::Type{T})(n::BigInt, ::RoundingMode{:Up}) where T<:CdoubleMax
    x = T(n,RoundToZero)
    x < n ? nextfloat(x) : x
end

function Float64(x::BigInt, ::RoundingMode{:Nearest})
    x == 0 && return 0.0
    xsize = abs(x.size)
    if xsize*BITS_PER_LIMB > 1024
        z = Inf64
    elseif xsize == 1
        z = Float64(@inbounds x.d[1])
    elseif Limb == UInt32 && xsize == 2
        z = Float64(((@inbounds x.d[2]) % UInt64) << BITS_PER_LIMB + (@inbounds x.d[1]))
    else
        d = x.d
        y1 = (@inbounds d[xsize]) % UInt64
        n = top_set_bit(y1)
        # load first 54(1 + 52 bits of fraction + 1 for rounding)
        y = y1 >> (n - (precision(Float64)+1))
        if Limb == UInt64
            y += n > precision(Float64) ? 0 : ((@inbounds d[xsize-1]) >> (10+n))
        else
            y += ((@inbounds d[xsize-1]) % UInt64) >> (n-22)
            y += n > (precision(Float64) - 32) ? 0 : ((@inbounds d[xsize-2]) >> (10+n))
        end
        y = (y + 1) >> 1 # round, ties up
        y &= ~UInt64(trailing_zeros(x) == (n-54 + (xsize-1)*BITS_PER_LIMB)) # fix last bit to round to even
        d = ((n+1021) % UInt64) << 52
        z = reinterpret(Float64, d+y)
        z = ldexp(z, (xsize-1)*BITS_PER_LIMB)
    end
    return flipsign(z, x.size)
end

function Float32(x::BigInt, ::RoundingMode{:Nearest})
    x == 0 && return 0f0
    xsize = abs(x.size)
    if xsize*BITS_PER_LIMB > 128
        z = Inf32
    elseif xsize == 1
        z = Float32(@inbounds x.d[1])
    else
        d = x.d
        y1 = @inbounds d[xsize]
        n = BITS_PER_LIMB - leading_zeros(y1)
        # load first 25(1 + 23 bits of fraction + 1 for rounding)
        y = (y1 >> (n - (precision(Float32)+1))) % UInt32
        y += (n > precision(Float32) ? 0 : (@inbounds d[xsize-1]) >> (BITS_PER_LIMB - (25-n))) % UInt32
        y = (y + one(UInt32)) >> 1 # round, ties up
        y &= ~UInt32(trailing_zeros(x) == (n-25 + (xsize-1)*BITS_PER_LIMB)) # fix last bit to round to even
        d = ((n+125) % UInt32) << 23
        z = reinterpret(Float32, d+y)
        z = ldexp(z, (xsize-1)*BITS_PER_LIMB)
    end
    return flipsign(z, x.size)
end

function Float16(x::BigInt, ::RoundingMode{:Nearest})
    x == 0 && return Float16(0.0)
    y1 = @inbounds x.d[1]
    n = BITS_PER_LIMB - leading_zeros(y1)
    if n > 16 || abs(x.size) > 1
        z = Inf16
    else
        # load first 12(1 + 10 bits for fraction + 1 for rounding)
        y = (y1 >> (n - (precision(Float16)+1))) % UInt16
        y = (y + one(UInt16)) >> 1 # round, ties up
        y &= ~UInt16(trailing_zeros(x) == (n-12)) # fix last bit to round to even
        d = ((n+13) % UInt16) << 10
        z = reinterpret(Float16, d+y)
    end
    return flipsign(z, x.size)
end

Float64(n::BigInt) = Float64(n, RoundNearest)
Float32(n::BigInt) = Float32(n, RoundNearest)
Float16(n::BigInt) = Float16(n, RoundNearest)

promote_rule(::Type{BigInt}, ::Type{<:Integer}) = BigInt

"""
    big(x)

Convert a number to a maximum precision representation (typically [`BigInt`](@ref) or
`BigFloat`). See [`BigFloat`](@ref BigFloat(::Any, rounding::RoundingMode)) for
information about some pitfalls with floating-point numbers.

!!! note "big(x::BigFloat)"
    Unlike `BigFloat(x)`, `big(x)` is a no-op when `x` is already a `BigFloat`,
    ie. when doing `x = big(x)`, the precision of `x` remains unchanged even if the
    current `BigFloat` precision is different.
    ```
"""
function big end

big(::Type{<:Integer})  = BigInt
big(::Type{<:Rational}) = Rational{BigInt}

big(n::Integer) = convert(BigInt, n)

# Binary ops
for (fJ, fC) in ((:+, :add), (:-,:sub), (:*, :mul),
                 (:+%, :add), (:-%,:sub), (:*%, :mul),
                 (:mod, :fdiv_r), (:rem, :tdiv_r),
                 (:gcd, :gcd), (:lcm, :lcm),
                 (:&, :and), (:|, :ior), (:xor, :xor))
    @eval begin
        ($fJ)(x::BigInt, y::BigInt) = MPZ.$fC(x, y)
    end
end

for (r, f) in ((RoundToZero, :tdiv_q),
               (RoundDown, :fdiv_q),
               (RoundUp, :cdiv_q))
    @eval div(x::BigInt, y::BigInt, ::typeof($r)) = MPZ.$f(x, y)
end

# For compat only. Remove in 2.0.
div(x::BigInt, y::BigInt) = div(x, y, RoundToZero)
fld(x::BigInt, y::BigInt) = div(x, y, RoundDown)
cld(x::BigInt, y::BigInt) = div(x, y, RoundUp)

/(x::BigInt, y::BigInt) = float(x)/float(y)

function invmod(x::BigInt, y::BigInt)
    z = zero(BigInt)
    ya = abs(y)
    if ya == 1
        return z
    end
    if (y==0 || MPZ.invert!(z, x, ya) == 0)
        throw(DomainError(y))
    end
    # GMP always returns a positive inverse; we instead want to
    # normalize such that div(z, y) == 0, i.e. we want a negative z
    # when y is negative.
    if y < 0
        MPZ.add!(z, y)
    end
    # The postcondition is: mod(z * x, y) == mod(big(1), y) && div(z, y) == 0
    return z
end

# More efficient commutative operations
for (fJ, fC) in ((:+, :add), (:*, :mul), (:&, :and), (:|, :ior), (:xor, :xor))
    fC! = Symbol(fC, :!)
    @eval begin
        ($fJ)(a::BigInt, b::BigInt, c::BigInt) = MPZ.$fC!(MPZ.$fC(a, b), c)
        ($fJ)(a::BigInt, b::BigInt, c::BigInt, d::BigInt) = MPZ.$fC!(MPZ.$fC!(MPZ.$fC(a, b), c), d)
        ($fJ)(a::BigInt, b::BigInt, c::BigInt, d::BigInt, e::BigInt) =
            MPZ.$fC!(MPZ.$fC!(MPZ.$fC!(MPZ.$fC(a, b), c), d), e)
    end
end

# Basic arithmetic without promotion
+(x::BigInt, c::CulongMax) = MPZ.add_ui(x, c)
+(c::CulongMax, x::BigInt) = x + c

-(x::BigInt, c::CulongMax) = MPZ.sub_ui(x, c)
-(c::CulongMax, x::BigInt) = MPZ.ui_sub(c, x)

+(x::BigInt, c::ClongMax) = c < 0 ? -(x, -%(c % Culong)) : x + convert(Culong, c)
+(c::ClongMax, x::BigInt) = c < 0 ? -(x, -%(c % Culong)) : x + convert(Culong, c)
-(x::BigInt, c::ClongMax) = c < 0 ? +(x, -%(c % Culong)) : -(x, convert(Culong, c))
-(c::ClongMax, x::BigInt) = c < 0 ? -(x + -%(c % Culong)) : -(convert(Culong, c), x)

*(x::BigInt, c::CulongMax) = MPZ.mul_ui(x, c)
*(c::CulongMax, x::BigInt) = x * c
*(x::BigInt, c::ClongMax) = MPZ.mul_si(x, c)
*(c::ClongMax, x::BigInt) = x * c

/(x::BigInt, y::Union{ClongMax,CulongMax}) = float(x)/y
/(x::Union{ClongMax,CulongMax}, y::BigInt) = x/float(y)

# unary ops
(-)(x::BigInt) = MPZ.neg(x)
(~)(x::BigInt) = MPZ.com(x)

<<(x::BigInt, c::UInt) = c == 0 ? x : MPZ.mul_2exp(x, c)
>>(x::BigInt, c::UInt) = c == 0 ? x : MPZ.fdiv_q_2exp(x, c)
>>>(x::BigInt, c::UInt) = x >> c

function trailing_zeros(x::BigInt)
    c = MPZ.scan1(x, 0)
    c == -1 && throw(DomainError(x, "`x` must be non-zero"))
    c
end

function trailing_ones(x::BigInt)
    c = MPZ.scan0(x, 0)
    c == -1 && throw(DomainError(x, "`x` must not be equal to -1"))
    c
end

function count_ones(x::BigInt)
    c = MPZ.popcount(x)
    c == -1 && throw(DomainError(x, "`x` cannot be negative"))
    c
end

# generic definition is not used to provide a better error message
function count_zeros(x::BigInt)
    c = MPZ.popcount(~x)
    c == -1 && throw(DomainError(x, "`x` must be negative"))
    c
end

"""
    count_ones_abs(x::BigInt)

Number of ones in the binary representation of abs(x).
"""
count_ones_abs(x::BigInt) = iszero(x) ? 0 : MPZ.mpn_popcount(x)

# all uses of _bit_magnitude MUST ensure at callsite that `x` is strictly positive, otherwise it is UB
_bit_magnitude(x::BigInt) = x.size * sizeof(Limb) << 3 - leading_zeros(@inbounds x.d[x.size])

function exponent(x::BigInt)
    iszero(x) && throw(DomainError(x, "cannot be zero"))
    ux = abs(x)
    return _bit_magnitude(ux) - 1
end

function top_set_bit(x::BigInt)
    isnegative(x) && throw(DomainError(x, "top_set_bit only supports negative arguments when they have type BitSigned."))
    iszero(x) && return 0
    return _bit_magnitude(x)
end

divrem(x::BigInt, y::BigInt,  ::typeof(RoundToZero) = RoundToZero) = MPZ.tdiv_qr(x, y)
divrem(x::BigInt, y::Integer, ::typeof(RoundToZero) = RoundToZero) = MPZ.tdiv_qr(x, BigInt(y))

cmp(x::BigInt, y::BigInt) = sign(MPZ.cmp(x, y))
cmp(x::BigInt, y::ClongMax) = sign(MPZ.cmp_si(x, y))
cmp(x::BigInt, y::CulongMax) = sign(MPZ.cmp_ui(x, y))
cmp(x::BigInt, y::Integer) = cmp(x, big(y))
cmp(x::Integer, y::BigInt) = -cmp(y, x)

cmp(x::BigInt, y::CdoubleMax) = isnan(y) ? -1 : sign(MPZ.cmp_d(x, y))
cmp(x::CdoubleMax, y::BigInt) = -cmp(y, x)

isqrt(x::BigInt) = MPZ.sqrt(x)

^(x::BigInt, y::Culong) = MPZ.pow_ui(x, y)

function bigint_pow(x::BigInt, y::Integer)
    x == 1 && return x
    x == -1 && return isodd(y) ? x : -x
    if y<0; throw(DomainError(y, "`y` cannot be negative.")); end
    @noinline throw1(y) =
        throw(OverflowError("exponent $y is too large and computation will overflow"))
    if y>typemax(Culong)
       x==0 && return x

       #At this point, x is not 1, 0 or -1 and it is not possible to use
       #gmpz_pow_ui to compute the answer. Note that the magnitude of the
       #answer is:
       #- at least 2^(2^32-1) ≈ 10^(1.3e9) (if Culong === UInt32).
       #- at least 2^(2^64-1) ≈ 10^(5.5e18) (if Culong === UInt64).
       #
       #Assume that the answer will definitely overflow.

       throw1(y)
    end
    return x^convert(Culong, y)
end

^(x::BigInt , y::BigInt ) = bigint_pow(x, y)
^(x::BigInt , y::Bool   ) = y ? x : one(x)
^(x::BigInt , y::Integer) = bigint_pow(x, y)
^(x::Integer, y::BigInt ) = bigint_pow(BigInt(x), y)
^(x::Bool   , y::BigInt ) = Base.power_by_squaring(x, y)

function powermod(x::BigInt, p::BigInt, m::BigInt)
    r = MPZ.powm(x, p, m)
    return m < 0 && r > 0 ? MPZ.add!(r, m) : r # choose sign consistent with mod(x^p, m)
end

powermod(x::Integer, p::Integer, m::BigInt) = powermod(big(x), big(p), m)

function gcdx(a::BigInt, b::BigInt)
    g, s, t = MPZ.gcdext(a, b)
    if t == 0
        # work around a difference in some versions of GMP
        if a == b
            return g, t, s
        elseif abs(a)==abs(b)
            return g, t, -s
        end
    end
    g, s, t
end

+(x::BigInt, y::BigInt, rest::BigInt...) = sum(tuple(x, y, rest...))
sum(arr::Union{AbstractArray{BigInt}, Tuple{BigInt, Vararg{BigInt}}}) =
    foldl(MPZ.add!, arr; init=BigInt(0))

function prod(arr::AbstractArray{BigInt})
    any(iszero, arr) && return zero(BigInt)
    n = Int(length(arr))
    n == 0 && return one(BigInt)
    lo = Int(firstindex(arr))
    n == 1 && return MPZ.set(arr[lo])
    hi = lo + n - 1
    nlimbs = 0
    for i in lo:hi
        nlimbs += abs(arr[i].size)
    end
    if nlimbs <= 64 # with few total limbs linear prod is faster
        nbits = nlimbs*BITS_PER_LIMB
        acc = MPZ.set_si!(BigInt(; nbits=nbits), 1)
        tmp = BigInt(; nbits=nbits)
        for i in lo:hi
            MPZ.mul!(tmp, acc, arr[i])
            acc, tmp = tmp, acc
        end
        return acc
    end
    # Otherwise multiply pairwise, keeping both operands of comparable size 
    # DFS leaves 1 partial product live per level, so one scratch per level is enough
    # plus a spare to multiply into
    depth = top_set_bit(n)
    return _prod!([BigInt() for _ in 1:depth+2], arr, lo, hi, 1)
end

# The product of `arr[lo:hi]`: `scratch[d]` for a range of two or more, and
# for a single element that element, which the caller may then only read.
function _prod!(scratch::Vector{BigInt}, arr::AbstractArray{BigInt}, lo::Int, hi::Int, d::Int)
    lo == hi && return arr[lo]
    hi - lo == 1 && return MPZ.mul!(scratch[d], arr[lo], arr[hi])
    # Split so the left range holds two or more and so lands in `scratch[d+1]`;
    # only the right one can come back as an array element.
    mid = lo + ((hi - lo) >> 1)
    l = _prod!(scratch, arr, lo, mid, d + 1)
    # Park it: the right half reuses every slot below this level.
    scratch[d], scratch[d+1] = l, scratch[d]
    r = _prod!(scratch, arr, mid + 1, hi, d + 1)
    spare = scratch[end]   # neither operand, so mul! reuses its buffer
    MPZ.mul!(spare, scratch[d], r)
    scratch[d], scratch[end] = spare, scratch[d]
    return scratch[d]
end

factorial(n::BigInt) = !isnegative(n) ? MPZ.fac_ui(n) : throw(DomainError(n, "`n` must not be negative."))

function binomial(n::BigInt, k::Integer)
    k < 0 && return BigInt(0)
    k <= typemax(Culong) && return binomial(n, Culong(k))
    n < 0 && return isodd(k) ? -binomial(k - n - 1, k) : binomial(k - n - 1, k)
    κ = n - k
    κ < 0 && return BigInt(0)
    κ <= typemax(Culong) && return binomial(n, Culong(κ))
    throw(OverflowError("Computation would exceed memory"))
end
binomial(n::BigInt, k::Culong) = MPZ.bin_ui(n, k)

==(x::BigInt, y::BigInt) = cmp(x,y) == 0
==(x::BigInt, i::Integer) = cmp(x,i) == 0
==(i::Integer, x::BigInt) = cmp(x,i) == 0
==(x::BigInt, f::CdoubleMax) = isnan(f) ? false : cmp(x,f) == 0
==(f::CdoubleMax, x::BigInt) = isnan(f) ? false : cmp(x,f) == 0
iszero(x::BigInt) = x.size == 0
isone(x::BigInt) = x == Culong(1)

<=(x::BigInt, y::BigInt) = cmp(x,y) <= 0
<=(x::BigInt, i::Integer) = cmp(x,i) <= 0
<=(i::Integer, x::BigInt) = cmp(x,i) >= 0
<=(x::BigInt, f::CdoubleMax) = isnan(f) ? false : cmp(x,f) <= 0
<=(f::CdoubleMax, x::BigInt) = isnan(f) ? false : cmp(x,f) >= 0

<(x::BigInt, y::BigInt) = cmp(x,y) < 0
<(x::BigInt, i::Integer) = cmp(x,i) < 0
<(i::Integer, x::BigInt) = cmp(x,i) > 0
<(x::BigInt, f::CdoubleMax) = isnan(f) ? false : cmp(x,f) < 0
<(f::CdoubleMax, x::BigInt) = isnan(f) ? false : cmp(x,f) > 0
isnegative(x::BigInt) = x.size < 0
ispositive(x::BigInt) = x.size > 0

signbit(x::BigInt) = isnegative(x)
flipsign!(x::BigInt, y::Integer) = (signbit(y) && (x.size = -x.size); x)
flipsign( x::BigInt, y::Integer) = signbit(y) ? -x : x
flipsign( x::BigInt, y::BigInt)  = signbit(y) ? -x : x
# above method to resolving ambiguities with flipsign(::T, ::T) where T<:Signed
function sign(x::BigInt)
    isnegative(x) && return -one(x)
    ispositive(x) && return one(x)
    return x
end

show(io::IO, x::BigInt) = print(io, string(x))

function string(n::BigInt; base::Integer = 10, pad::Integer = 1)
    base < 0 && return Base._base(Int(base), n, pad, (base>0) & (n.size<0))
    2 <= base <= 62 || throw(ArgumentError("base must be 2 ≤ base ≤ 62, got $base"))
    iszero(n) && pad < 1 && return ""
    nd1 = ndigits(n, base=base)
    nd  = max(nd1, pad)
    str = Base._string_n(nd + isnegative(n))
    GC.@preserve str begin
        p = pointer(str)
        MPZ.get_str!(p + nd - nd1, base, n)
        pad_len = nd - nd1
        if pad_len > 0
            Base.memset(p + isnegative(n), UInt8('0'), pad_len)
        end
        isnegative(n) && unsafe_store!(p, UInt8('-'))
    end
    return str
end

function digits!(a::AbstractVector{T}, n::BigInt; base::Integer = 10) where {T<:Integer}
    if base ≥ 2
        if base ≤ 62
            # fast path using mpz_get_str via string(n; base)
            s = codeunits(string(n; base))
            i, j = firstindex(a)-1, length(s)+1
            lasti = min(lastindex(a), firstindex(a) + length(s)-1 - isnegative(n))
            while i < lasti
                # base ≤ 36: 0-9, plus a-z for 10-35
                # base > 36: 0-9, plus A-Z for 10-35 and a-z for 36..61
                x = s[j -= 1]
                a[i += 1] = base ≤ 36 ? (x>0x39 ? x-0x57 : x-0x30) : (x>0x39 ? (x>0x60 ? x-0x3d : x-0x37) : x-0x30)
            end
            lasti = lastindex(a)
            while i < lasti; a[i+=1] = zero(T); end
            return isnegative(n) ? map!(-,a,a) : a
        elseif a isa StridedVector{<:Base.BitInteger} && stride(a,1) == 1 && ispow2(base) && base-1 ≤ typemax(T)
            # fast path using mpz_export
            origlen = length(a)
            _, writelen = MPZ.export!(a, n; nails = 8sizeof(T) - trailing_zeros(base))
            length(a) != origlen && resize!(a, origlen) # truncate to least-significant digits
            a[begin+writelen:end] .= zero(T)
            return isnegative(n) ? map!(-,a,a) : a
        end
    end
    return invoke(digits!, Tuple{typeof(a), Integer}, a, n; base) # slow generic fallback
end

function ndigits0zpb(x::BigInt, b::Integer)
    b < 2 && throw(DomainError(b, "`b` cannot be less than 2."))
    x.size == 0 && return 0 # for consistency with other ndigits0z methods
    if ispow2(b) && 2 <= b <= 62 # GMP assumes b is in this range
        MPZ.sizeinbase(x, b)
    else
        # non-base 2 mpz_sizeinbase might return an answer 1 too big
        # use property that log(b, x) < ndigits(x, base=b) <= log(b, x) + 1
        n = MPZ.sizeinbase(x, 2)
        lb = log2(b) # assumed accurate to <1ulp (true for openlibm)
        q,r = divrem(n,lb)
        iq = Int(q)
        maxerr = q*eps(lb) # maximum error in remainder
        if r-1.0 < maxerr
            abs(x) >= big(b)^iq ? iq+1 : iq
        elseif lb-r < maxerr
            abs(x) >= big(b)^(iq+1) ? iq+2 : iq+1
        else
            iq+1
        end
    end
end

# Fast paths for nextpow(2, x::BigInt)
# below, ONE is always left-shifted by at least one digit, so a new BigInt is
# allocated, which can be safely mutated
_prevpow2(x::BigInt) = -2 <= x <= 2 ? x : flipsign!(ONE << (ndigits(x, base=2) - 1), x)
_nextpow2(x::BigInt) = count_ones_abs(x) <= 1 ? x : flipsign!(ONE << ndigits(x, base=2), x)

Base.checked_abs(x::BigInt) = abs(x)
Base.checked_neg(x::BigInt) = -x
Base.checked_add(a::BigInt, b::BigInt) = a + b
Base.checked_sub(a::BigInt, b::BigInt) = a - b
Base.checked_mul(a::BigInt, b::BigInt) = a * b
Base.checked_div(a::BigInt, b::BigInt) = div(a, b)
Base.checked_rem(a::BigInt, b::BigInt) = rem(a, b)
Base.checked_fld(a::BigInt, b::BigInt) = fld(a, b)
Base.checked_mod(a::BigInt, b::BigInt) = mod(a, b)
Base.checked_cld(a::BigInt, b::BigInt) = cld(a, b)
Base.add_with_overflow(a::BigInt, b::BigInt) = a + b, false
Base.sub_with_overflow(a::BigInt, b::BigInt) = a - b, false
Base.mul_with_overflow(a::BigInt, b::BigInt) = a * b, false

# checked_pow doesn't follow the same promotion rules as the others, above.
Base.checked_pow(x::BigInt, p::Integer) = x^p
Base.checked_pow(x::Integer, p::BigInt) = x^p
Base.checked_pow(x::BigInt, p::BigInt) = x^p

Base.deepcopy_impl(x::BigInt, stackdict::IdDict) = get!(() -> MPZ.set(x), stackdict, x)::BigInt

## streamlined hashing for BigInt, by avoiding allocation from shifts ##

Base._hash_shl!(x::BigInt, n) = MPZ.mul_2exp!(x, n)

if Limb === UInt64 === UInt
    # On 64 bit systems we can define
    # an optimized version for BigInt of hash_integer (used e.g. for Rational{BigInt}),
    # and of hash

    using .Base: HASH_SECRET, hash_bytes

    struct LimbView <: AbstractVector{UInt8}
        bigint::BigInt
        start_byte::Int
        num_bytes::Int
    end

    function Base.size(view::LimbView)
        return (view.num_bytes,)
    end

    function Base.getindex(view::LimbView, i::Int)
        @boundscheck checkbounds(view, i)
        limb_index = div(view.start_byte + i - 2, 8) + 1
        byte_in_limb = (view.start_byte + i - 2) % 8
        limb = @inbounds view.bigint.d[limb_index]
        return UInt8((limb >> (8 * byte_in_limb)) & 0xff)
    end

    function Base.iterate(view::LimbView, state::Int = 1)
        state > view.num_bytes && return nothing
        return @inbounds(view[state]), state + 1
    end

    function Base.length(view::LimbView)
        return view.num_bytes
    end

    function hash_integer(n::BigInt, h::UInt)
        iszero(n) && return hash_integer(0, h)
        s = n.size
        h ⊻= (s < 0)

        us = abs(s)
        leading_zero_bytes = div(leading_zeros(@inbounds n.d[us]), 8)
        num_bytes = 8 * us - leading_zero_bytes

        limb_view = LimbView(n, 1, num_bytes)
        return hash_bytes(limb_view, h, HASH_SECRET)
    end

    function hash(x::BigInt, h::UInt)
        sz = x.size
        sz == 0 && return hash(0, h)
        d = x.d
        if sz == 1
            return hash(@inbounds(d[1]), h)
        elseif sz == -1
            limb = @inbounds d[1]
            limb <= typemin(Int) % UInt && return hash(-%(limb % Int), h)
        end
        pow = trailing_zeros(x)
        nd = Base.ndigits0z(x, 2)
        idx = (pow >>> 6) + 1
        shift = (pow & 63) % UInt
        upshift = BITS_PER_LIMB - shift
        asz = abs(sz)
        if shift == 0
            limb = @inbounds d[idx]
        else
            limb1 = @inbounds d[idx]
            limb2 = idx < asz ? (@inbounds d[idx+1]) : UInt(0)
            limb = limb2 << upshift | limb1 >> shift
        end
        if nd <= 1024 && nd - pow <= 53
            return hash(ldexp(flipsign(Float64(limb), sz), pow), h)
        end
        h = hash_integer(pow, h)

        h ⊻= (sz < 0)
        leading_zero_bytes = div(leading_zeros(@inbounds d[asz]), 8)
        trailing_zero_bytes = div(pow, 8)
        num_bytes = 8 * asz - (leading_zero_bytes + trailing_zero_bytes)

        limb_view = LimbView(x, trailing_zero_bytes + 1, num_bytes)
        return hash_bytes(limb_view, h, HASH_SECRET)
    end
end

module MPQ

# Rational{BigInt}
import .Base: unsafe_rational, __throw_rational_argerror_zero
import ..GMP: BigInt, MPZ, Limb, libgmp

gmpq(op::Symbol) = Expr(:tuple, QuoteNode(Symbol(:__gmpq_, op)), GlobalRef(MPZ, :libgmp))

# As for `mpz` above: reads take `_MPQView`, a borrowed `__mpq_struct`, and
# writes run on a GMP-owned scratch `mpq`, released before the call returns.
struct _MPQView
    num_alloc::Cint
    num_size::Cint
    num_d::Ptr{Limb}
    den_alloc::Cint
    den_size::Cint
    den_d::Ptr{Limb}
end
const mpq_view_t = Ref{_MPQView}

@inline _view(x::Rational{BigInt}) = _MPQView(
    length(x.num.d) % Cint, x.num.size % Cint, pointer(x.num.d),
    length(x.den.d) % Cint, x.den.size % Cint, pointer(x.den.d))

Base.cconvert(::Type{mpq_view_t}, x::Rational{BigInt}) = (Ref(_view(x)), x.num.d, x.den.d)
Base.unsafe_convert(::Type{mpq_view_t}, t::Tuple{Base.RefValue{_MPQView},Memory{Limb},Memory{Limb}}) =
    Base.unsafe_convert(mpq_view_t, t[1])

mutable struct _MPQ
    num_alloc::Cint
    num_size::Cint
    num_d::Ptr{Limb}
    den_alloc::Cint
    den_size::Cint
    den_d::Ptr{Limb}
    _MPQ() = new(0, 0, C_NULL, 0, 0, C_NULL)
end
const mpq_t = Ref{_MPQ}

_init!(q::_MPQ) = (ccall((:__gmpq_init, libgmp), Cvoid, (mpq_t,), q); q)
_clear!(q::_MPQ) = ccall((:__gmpq_clear, libgmp), Cvoid, (mpq_t,), q)

function _take!(z::Rational{BigInt}, q::_MPQ)
    MPZ._take!(z.num, q.num_size, q.num_d)
    MPZ._take!(z.den, q.den_size, q.den_d)
    return z
end

# Only the output needs GMP-owned limbs; `mpq_add` and friends take const operands.
function _with_output(f::F, z::Rational{BigInt}) where {F}
    q = _init!(_MPQ())
    try
        f(q)
        return _take!(z, q)
    finally
        _clear!(q)
    end
end

function Rational{BigInt}(num::BigInt, den::BigInt)
    if iszero(den)
        iszero(num) && __throw_rational_argerror_zero(BigInt)
        return set_si(flipsign(1, num), 0)
    end
    return _with_output(unsafe_rational(BigInt(), BigInt())) do q
        ccall((:__gmpq_set_num, libgmp), Cvoid, (mpq_t, MPZ.mpz_t), q, num)
        ccall((:__gmpq_set_den, libgmp), Cvoid, (mpq_t, MPZ.mpz_t), q, den)
        ccall((:__gmpq_canonicalize, libgmp), Cvoid, (mpq_t,), q)
    end
end

# define set, set_ui, set_si, set_z, and their inplace versions
# `x` is already canonical, so its components are copied across directly.
set!(z::Rational{BigInt}, x::Rational{BigInt}) =
    (MPZ.set!(z.num, x.num); MPZ.set!(z.den, x.den); z)

set_z!(z::Rational{BigInt}, x::BigInt) =
    (MPZ.set!(z.num, x); MPZ.set_ui!(z.den, one(Limb)); z)

for (op, T) in ((:set, Rational{BigInt}), (:set_z, BigInt))
    op! = Symbol(op, :!)
    @eval $op(a::$T) = $op!(unsafe_rational(BigInt(), BigInt()), a)
end

# note that rationals returned from set_ui and set_si are not checked,
# set_ui(0, 0) will return 0//0 without errors, just like unsafe_rational
for (op, T1, T2) in ((:set_ui, Culong, Culong), (:set_si, Clong, Culong))
    op! = Symbol(op, :!)
    @eval begin
        $op!(z::Rational{BigInt}, a, b) = _with_output(z) do q
            ccall($(gmpq(op)), Cvoid, (mpq_t, $T1, $T2), q, a, b)
        end
        $op(a, b) = $op!(unsafe_rational(BigInt(), BigInt()), a, b)
    end
end

# define add, sub, mul, div, and their inplace versions
function add!(z::Rational{BigInt}, x::Rational{BigInt}, y::Rational{BigInt})
    if iszero(x.den) || iszero(y.den)
        if iszero(x.den) && iszero(y.den) && isnegative(x.num) != isnegative(y.num)
            throw(DivideError())
        end
        return set!(z, iszero(x.den) ? x : y)
    end
    return _with_output(z) do q
        ccall((:__gmpq_add, libgmp), Cvoid, (mpq_t, mpq_view_t, mpq_view_t), q, x, y)
    end
end

function sub!(z::Rational{BigInt}, x::Rational{BigInt}, y::Rational{BigInt})
    if iszero(x.den) || iszero(y.den)
        if iszero(x.den) && iszero(y.den) && isnegative(x.num) == isnegative(y.num)
            throw(DivideError())
        end
        iszero(x.den) && return set!(z, x)
        return set_si!(z, flipsign(-1, y.num), 0)
    end
    return _with_output(z) do q
        ccall((:__gmpq_sub, libgmp), Cvoid, (mpq_t, mpq_view_t, mpq_view_t), q, x, y)
    end
end

function mul!(z::Rational{BigInt}, x::Rational{BigInt}, y::Rational{BigInt})
    if iszero(x.den) || iszero(y.den)
        if iszero(x.num) || iszero(y.num)
            throw(DivideError())
        end
        return set_si!(z, ifelse(xor(isnegative(x.num), isnegative(y.num)), -1, 1), 0)
    end
    return _with_output(z) do q
        ccall((:__gmpq_mul, libgmp), Cvoid, (mpq_t, mpq_view_t, mpq_view_t), q, x, y)
    end
end

function div!(z::Rational{BigInt}, x::Rational{BigInt}, y::Rational{BigInt})
    if iszero(x.den)
        if iszero(y.den)
            throw(DivideError())
        end
        isnegative(y.num) || return set!(z, x)
        return set_si!(z, flipsign(-1, x.num), 0)
    elseif iszero(y.den)
        return set_si!(z, 0, 1)
    elseif iszero(y.num)
        if iszero(x.num)
            throw(DivideError())
        end
        return set_si!(z, flipsign(1, x.num), 0)
    end
    return _with_output(z) do q
        ccall((:__gmpq_div, libgmp), Cvoid, (mpq_t, mpq_view_t, mpq_view_t), q, x, y)
    end
end

for (fJ, fC) in ((:+, :add), (:-, :sub), (:*, :mul), (://, :div))
    fC! = Symbol(fC, :!)
    @eval begin
        ($fC!)(x::Rational{BigInt}, y::Rational{BigInt}) = $fC!(x, x, y)
        (Base.$fJ)(x::Rational{BigInt}, y::Rational{BigInt}) = $fC!(unsafe_rational(BigInt(), BigInt()), x, y)
    end
end

function Base.cmp(x::Rational{BigInt}, y::Rational{BigInt})
    Int(ccall((:__gmpq_cmp, libgmp), Cint, (mpq_view_t, mpq_view_t), x, y))
end

end # MPQ module

end # module
