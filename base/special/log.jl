# This file is a part of Julia. License is MIT: https://julialang.org/license

# The logarithms write x = 2^k z and use log(x) = k*log(2) + log(c) + log1p(z/c - 1), where c is
# taken from the table `t_log_table_compact` below, whose reciprocals of c are short enough that
# r = z/c - 1 is computed exactly with one fma, so no division is needed.

# Does not currently handle floating point flags (inexact, div-by-zero, etc).

logb(::Type{Float32},::Val{2})  = 1.4426950408889634
logb(::Type{Float32},::Val{:ℯ}) = 1.0
logb(::Type{Float32},::Val{10}) = 0.4342944819032518
logbU(::Type{Float64},::Val{2})  = 1.4426950408889634
logbL(::Type{Float64},::Val{2})  = 2.0355273740931033e-17
logbU(::Type{Float64},::Val{:ℯ}) = 1.0
logbL(::Type{Float64},::Val{:ℯ}) = 0.0
logbU(::Type{Float64},::Val{10}) = 0.4342944819032518
logbL(::Type{Float64},::Val{10}) = 1.098319650216765e-17

@noinline log2(x::Float16)  = _log(x, Val(2),  :log2)
@noinline log(x::Float16)   = _log(x, Val(:ℯ), :log)
@noinline log10(x::Float16) = _log(x, Val(10), :log10)
@noinline log2(x::Float32)  = _log(x, Val(2),  :log2)
@noinline log(x::Float32)   = _log(x, Val(:ℯ), :log)
@noinline log10(x::Float32) = _log(x, Val(10), :log10)
@noinline log2(x::Float64)  = _log(x, Val(2),  :log2)
@noinline log(x::Float64)   = _log(x, Val(:ℯ), :log)
@noinline log10(x::Float64) = _log(x, Val(10), :log10)

@inline function _log(x::Float64, base, func::Symbol)
    xu = reinterpret(UInt64, x)
    # x <= 0 (including -0.0), subnormal, +Inf, or NaN
    if xu - 0x0010_0000_0000_0000 >= 0x7fe0_0000_0000_0000
        return _log_special(x, base, func)
    end
    hi, lo = _log_kernel(xu)
    return _log_scale(hi, lo, base)
end
@noinline function _log_special(x::Float64, base, func::Symbol)
    x == 0.0 && return -Inf
    isnan(x) && return NaN
    x == Inf && return x
    x < 0.0 && throw_complex_domainerror(func, x)
    # x is subnormal: normalize it; the kernel's signed shift handles the exponent field going below zero
    hi, lo = _log_kernel(reinterpret(UInt64, x * 0x1p54) -% (UInt64(54) << 52))
    return _log_scale(hi, lo, base)
end
@inline function _log_scale(hi::Float64, lo::Float64, base)
    base === Val(:ℯ) && return hi + lo
    m_hi = logbU(Float64, base)
    m_lo = logbL(Float64, base)
    return fma(m_hi, hi, fma(m_hi, lo, m_lo*hi))
end

@inline function _log(x::Float32, base, func::Symbol)
    # x <= 0 (including -0.0), +Inf, or NaN; positive subnormals are handled by the kernel
    if reinterpret(UInt32, x) - 0x0000_0001 >= 0x7f7f_ffff
        return _log_special(x, func)
    end
    return Float32(logb(Float32, base) * _log_kernel_f32(Float64(x)))
end
@noinline function _log_special(x::T, func::Symbol) where {T<:Union{Float16,Float32}}
    x == 0 && return T(-Inf)
    isnan(x) && return T(NaN)
    x == Inf && return x
    throw_complex_domainerror(func, x)
end

# Float16 needs no table: in Float32, x = 2^k m with m in [sqrt(2)/2, sqrt(2)) and f = m - 1 exact,
# then log1p(f) from one polynomial.
@inline function _log_f16_reduce(x::Float32)
    xu = reinterpret(UInt32, x)
    tmp = reinterpret(Int32, xu - 0x3f3504f3) # sqrt(2)/2
    m = reinterpret(Float32, xu - (reinterpret(UInt32, tmp) & 0xff80_0000))
    return tmp >> 23, m - 1f0
end
# log1p(f) for f in [sqrt(2)/2 - 1, sqrt(2) - 1], relative error 2^-22.4
@inline function _log1p_f16_poly(f::Float32)
    f2 = f*f
    f4 = f2*f2
    q = muladd(f2, muladd(f, 0.19906883f0, -0.24971229f0), muladd(f, 0.33334714f0, -0.5000032f0)) +
        f4 * muladd(f2, -0.10082381f0, muladd(f, 0.16180502f0, -0.17243224f0))
    return muladd(f2, q, f)
end
@inline function _log(x::Float16, base, func::Symbol)
    # x <= 0 (including -0.0), +Inf, or NaN; positive subnormals are normal in Float32
    if reinterpret(UInt16, x) - 0x0001 >= 0x7bff
        return _log_special(x, func)
    end
    k, f = _log_f16_reduce(Float32(x))
    p = _log1p_f16_poly(f)
    kf = Float32(k)
    base === Val(2) && return Float16(muladd(p, 1.442695f0, kf))
    base === Val(10) && return Float16(muladd(kf, 0.30103f0, p * 0.4342945f0))
    return Float16(muladd(kf, 0.6931472f0, p))
end


function log1p(x::Float64)
    # log1p(x) == x - x^2/2 to within 2^-80 relative; this also keeps the sign of zero. Below, the
    # correction d would be too large relative to the result for its approximation.
    abs(x) < 0x1p-40 && return fma(x, -0.5 * x, x)
    s = 1.0 + x
    # x <= -1, NaN, or +Inf (s is +Inf only for x == Inf)
    if reinterpret(UInt64, s) - 0x0000_0000_0000_0001 >= 0x7fef_ffff_ffff_ffff
        x == -1.0 && return -Inf
        isnan(x) && return NaN
        x == Inf && return x
        throw_complex_domainerror_neg1(:log1p, x)
    end
    c = ifelse(x > 1.0, 1.0 - (s - x), x - (s - 1.0)) # 1 + x == s + c exactly
    tmp, z, k, invc, logc, logctail = _log_tab_reduce(reinterpret(UInt64, s))
    # 1 + x == 2^k (z + c*2^-k), so its reduced argument is r + d with d = c*2^-k*invc. 2^-k is formed
    # as 2^(1-k)/2, since 2^(1-k) is normal for every k here except k == 1024, where it becomes 0 and
    # c no longer matters.
    scale2 = reinterpret(Float64, 0x4000000000000000 -% (tmp & 0xfff0000000000000))
    r = fma(z, invc, -1.0)
    d = (c * scale2) * (0.5 * invc)
    hi, lo = _log_tab_finish(k, logc, logctail, r)
    # log1p(r + d) ≈ log1p(r) + d*(1 - r), keeping d off the polynomial's critical path
    return hi + (lo + fma(-d, r, d))
end

function log1p(x::Float32)
    if !(x > -1f0)
        x == -1f0 && return -Inf32
        isnan(x) && return x
        throw_complex_domainerror_neg1(:log1p, x)
    end
    x == Inf32 && return x
    d = Float64(x)
    s = 1.0 + d # exact unless |x| < 2^-29 or x > 2^29
    # 1 + x == s + c, so log1p(x) ≈ log(s) + c/s. The correction only matters when s ≈ 1.
    c = ifelse(s < 2.0, d - (s - 1.0), 0.0)
    # copysign keeps log1p(-0.0) == -0.0
    return copysign(Float32(_log_kernel_f32(s) + c), x)
end

function log1p(x::Float16)
    xf = Float32(x)
    s = 1f0 + xf # exact unless |x| < 2^-13
    # x <= -1, NaN, or +Inf (s is +Inf only for x == Inf)
    if reinterpret(UInt32, s) - 0x0000_0001 >= 0x7f7f_ffff
        x == -1 && return -Inf16
        isnan(x) && return x
        x == Inf16 && return x
        throw_complex_domainerror_neg1(:log1p, x)
    end
    k, f = _log_f16_reduce(s)
    # with k == 0, x itself is the reduced argument, which is exact even where s is not
    f = ifelse(k == 0, xf, f)
    # copysign keeps log1p(-0.0) == -0.0
    return Float16(copysign(muladd(Float32(k), 0.6931472f0, _log1p_f16_poly(f)), xf))
end

#function make_compact_table(N)
#    table = Tuple{UInt64,Float64}[]
#    lo, hi = 0x1.69555p-1, 0x1.69555p0
#    for i in 0:N-1
#        # I am not fully sure why this is the right formula to use, but it apparently is
#        center = i/(2*N) + lo < 1 ? (i+.5)/(2*N) + lo : (i+.5)/N + hi -1
#        invc = Float64(center < 1 ? round(N/center)/N : round(2*N/center)/(N*2))
#        c = inv(big(invc))
#        logc = Float64(round(0x1p43*log(c))/0x1p43)
#        logctail = reinterpret(Float64, Float64(log(c) - logc))
#        p1 = (reinterpret(UInt64,invc) >> 45) % UInt8
#        push!(table, (p1|reinterpret(UInt64,logc),logctail))
#    end
#    return Tuple(table)
#end
#const t_log_table_compact = make_compact_table(128)
const t_log_table_compact = (
    (0xbfd62c82f2b9c8b5, 5.929407345889625e-15),
    (0xbfd5d1bdbf5808b4, -2.544157440035963e-14),
    (0xbfd57677174558b3, -3.443525940775045e-14),
    (0xbfd51aad872df8b2, -2.500123826022799e-15),
    (0xbfd4be5f957778b1, -8.929337133850617e-15),
    (0xbfd4618bc21c60b0, 1.7625431312172662e-14),
    (0xbfd404308686a8af, 1.5688303180062087e-15),
    (0xbfd3a64c556948ae, 2.9655274673691784e-14),
    (0xbfd347dd9a9880ad, 3.7923164802093147e-14),
    (0xbfd2e8e2bae120ac, 3.993416384387844e-14),
    (0xbfd2895a13de88ab, 1.9352855826489123e-14),
    (0xbfd2895a13de88ab, 1.9352855826489123e-14),
    (0xbfd22941fbcf78aa, -1.9852665484979036e-14),
    (0xbfd1c898c16998a9, -2.814323765595281e-14),
    (0xbfd1675cababa8a8, 2.7643769993528702e-14),
    (0xbfd1058bf9ae48a7, -4.025092402293806e-14),
    (0xbfd0a324e27390a6, -1.2621729398885316e-14),
    (0xbfd0402594b4d0a5, -3.600176732637335e-15),
    (0xbfd0402594b4d0a5, -3.600176732637335e-15),
    (0xbfcfb9186d5e40a4, 1.3029797173308663e-14),
    (0xbfcef0adcbdc60a3, 4.8230289429940886e-14),
    (0xbfce27076e2af0a2, -2.0592242769647135e-14),
    (0xbfcd5c216b4fc0a1, 3.149265065191484e-14),
    (0xbfcc8ff7c79aa0a0, 4.169796584527195e-14),
    (0xbfcc8ff7c79aa0a0, 4.169796584527195e-14),
    (0xbfcbc286742d909f, 2.2477465222466186e-14),
    (0xbfcaf3c94e80c09e, 3.6507188831790577e-16),
    (0xbfca23bc1fe2b09d, -3.827767260205414e-14),
    (0xbfca23bc1fe2b09d, -3.827767260205414e-14),
    (0xbfc9525a9cf4509c, -4.7641388950792196e-14),
    (0xbfc87fa06520d09b, 4.9278276214647115e-14),
    (0xbfc7ab890210e09a, 4.9485167661250996e-14),
    (0xbfc7ab890210e09a, 4.9485167661250996e-14),
    (0xbfc6d60fe719d099, -1.5003333854266542e-14),
    (0xbfc5ff3070a79098, -2.7194441649495324e-14),
    (0xbfc5ff3070a79098, -2.7194441649495324e-14),
    (0xbfc526e5e3a1b097, -2.99659267292569e-14),
    (0xbfc44d2b6ccb8096, 2.0472357800461955e-14),
    (0xbfc44d2b6ccb8096, 2.0472357800461955e-14),
    (0xbfc371fc201e9095, 3.879296723063646e-15),
    (0xbfc29552f81ff094, -3.6506824353335045e-14),
    (0xbfc1b72ad52f6093, -5.4183331379008994e-14),
    (0xbfc1b72ad52f6093, -5.4183331379008994e-14),
    (0xbfc0d77e7cd09092, 1.1729485484531301e-14),
    (0xbfc0d77e7cd09092, 1.1729485484531301e-14),
    (0xbfbfec9131dbe091, -3.811763084710266e-14),
    (0xbfbe27076e2b0090, 4.654729747598445e-14),
    (0xbfbe27076e2b0090, 4.654729747598445e-14),
    (0xbfbc5e548f5bc08f, -2.5799991283069902e-14),
    (0xbfba926d3a4ae08e, 3.7700471749674615e-14),
    (0xbfba926d3a4ae08e, 3.7700471749674615e-14),
    (0xbfb8c345d631a08d, 1.7306161136093256e-14),
    (0xbfb8c345d631a08d, 1.7306161136093256e-14),
    (0xbfb6f0d28ae5608c, -4.012913552726574e-14),
    (0xbfb51b073f06208b, 2.7541708360737882e-14),
    (0xbfb51b073f06208b, 2.7541708360737882e-14),
    (0xbfb341d7961be08a, 5.0396178134370583e-14),
    (0xbfb341d7961be08a, 5.0396178134370583e-14),
    (0xbfb16536eea38089, 1.8195060030168815e-14),
    (0xbfaf0a30c0118088, 5.213620639136504e-14),
    (0xbfaf0a30c0118088, 5.213620639136504e-14),
    (0xbfab42dd71198087, 2.532168943117445e-14),
    (0xbfab42dd71198087, 2.532168943117445e-14),
    (0xbfa77458f632c086, -5.148849572685811e-14),
    (0xbfa77458f632c086, -5.148849572685811e-14),
    (0xbfa39e87b9fec085, 4.6652946995830086e-15),
    (0xbfa39e87b9fec085, 4.6652946995830086e-15),
    (0xbf9f829b0e780084, -4.529814257790929e-14),
    (0xbf9f829b0e780084, -4.529814257790929e-14),
    (0xbf97b91b07d58083, -4.361324067851568e-14),
    (0xbf8fc0a8b0fc0082, -1.7274567499706107e-15),
    (0xbf8fc0a8b0fc0082, -1.7274567499706107e-15),
    (0xbf7fe02a6b100081, -2.298941004620351e-14),
    (0xbf7fe02a6b100081, -2.298941004620351e-14),
    (0x0000000000000080, 0.0),
    (0x0000000000000080, 0.0),
    (0x3f8010157589007e, -1.4902732911301337e-14),
    (0x3f9020565893807c, -3.527980389655325e-14),
    (0x3f98492528c9007a, -4.730054772033249e-14),
    (0x3fa0415d89e74078, 7.580310369375161e-15),
    (0x3fa466aed42e0076, -4.9893776716773285e-14),
    (0x3fa894aa149fc074, -2.262629393030674e-14),
    (0x3faccb73cdddc072, -2.345674491018699e-14),
    (0x3faeea31c006c071, -1.3352588834854848e-14),
    (0x3fb1973bd146606f, -3.765296820388875e-14),
    (0x3fb3bdf5a7d1e06d, 5.1128335719851986e-14),
    (0x3fb5e95a4d97a06b, -5.046674438470119e-14),
    (0x3fb700d30aeac06a, 3.1218748807418837e-15),
    (0x3fb9335e5d594068, 3.3871241029241416e-14),
    (0x3fbb6ac88dad6066, -1.7376727386423858e-14),
    (0x3fbc885801bc4065, 3.957125899799804e-14),
    (0x3fbec739830a2063, -5.2849453521890294e-14),
    (0x3fbfe89139dbe062, -3.767012502308738e-14),
    (0x3fc1178e8227e060, 3.1859736349078334e-14),
    (0x3fc1aa2b7e23f05f, 5.0900642926060466e-14),
    (0x3fc2d1610c86805d, 8.710783796122478e-15),
    (0x3fc365fcb015905c, 6.157896229122976e-16),
    (0x3fc4913d8333b05a, 3.821577743916796e-14),
    (0x3fc527e5e4a1b059, 3.9440046718453496e-14),
    (0x3fc6574ebe8c1057, 2.2924522154618074e-14),
    (0x3fc6f0128b757056, -3.742530094732263e-14),
    (0x3fc7898d85445055, -2.5223102140407338e-14),
    (0x3fc8beafeb390053, -1.0320443688698849e-14),
    (0x3fc95a5adcf70052, 1.0634128304268335e-14),
    (0x3fca93ed3c8ae050, -4.3425422595242564e-14),
    (0x3fcb31d8575bd04f, -1.2527395755711364e-14),
    (0x3fcbd087383be04e, -5.204008743405884e-14),
    (0x3fcc6ffbc6f0104d, -3.979844515951702e-15),
    (0x3fcdb13db0d4904b, -4.7955860343296286e-14),
    (0x3fce530effe7104a, 5.015686013791602e-16),
    (0x3fcef5ade4dd0049, -7.252318953240293e-16),
    (0x3fcf991c6cb3b048, 2.4688324156011588e-14),
    (0x3fd07138604d5846, 5.465121253624792e-15),
    (0x3fd0c42d67616045, 4.102651071698446e-14),
    (0x3fd1178e8227e844, -4.996736502345936e-14),
    (0x3fd16b5ccbacf843, 4.903580708156347e-14),
    (0x3fd1bf99635a6842, 5.089628039500759e-14),
    (0x3fd214456d0eb841, 1.1782016386565151e-14),
    (0x3fd2bef07cdc903f, 4.727452940514406e-14),
    (0x3fd314f1e1d3603e, -4.4204083338755686e-14),
    (0x3fd36b6776be103d, 1.548345993498083e-14),
    (0x3fd3c2527733303c, 2.1522127491642888e-14),
    (0x3fd419b423d5e83b, 1.1054030169005386e-14),
    (0x3fd4718dc271c83a, -5.534326352070679e-14),
    (0x3fd4c9e09e173039, -5.351646604259541e-14),
    (0x3fd522ae0738a038, 5.4612144489920215e-14),
    (0x3fd57bf753c8d037, 2.8136969901227338e-14),
    (0x3fd5d5bddf596036, -1.156568624616423e-14))

 @inline function log_tab_unpack(t::UInt64)
    invc = UInt64(t&UInt64(0xff)|0x1ff00)<<45
    logc = t&(~UInt64(0xff))
    return (reinterpret(Float64, invc), reinterpret(Float64, logc))
end

# Reduction shared by the logarithms: x = 2^k z with z in [0x1.69555p-1, 0x1.69555p0), and the
# table entry (1/c, log(c) as a sum of two terms) for the subinterval containing z.
@inline function _log_tab_reduce(xu::UInt64)
    tmp = reinterpret(Int64, xu -% 0x3fe6955500000000) # 0x1.69555p-1
    z = reinterpret(Float64, xu -% (tmp & 0xfff0000000000000))
    k = Float64(tmp >> 52)
    # N.B. :nothrow and :noub since `idx` is known to be `1 ≤ idx ≤ length(t_log_table_compact)`
    idx = (tmp >> 45) & (length(t_log_table_compact)-1) + 1
    t, logctail = Base.@assume_effects :nothrow :noub @inbounds t_log_table_compact[idx]
    invc, logc = log_tab_unpack(t)
    return tmp, z, k, invc, logc, logctail
end

# k*log(2) + log(c) + log1p(r) as an unevaluated sum hi + lo, accurate to about 2^-60 relative.
@inline function _log_tab_finish(k::Float64, logc::Float64, logctail::Float64, r::Float64)
    t1 = muladd(k, 0.6931471805598903, logc) # ln(2) hi part
    tl = muladd(k, 5.497923018708371e-14, logctail) # ln(2) lo part
    r2 = r*r
    r4 = r2*r2
    # log1p(r) - r, with the coefficients of `_log_ext`. Next to 1 the table has c == 1, so there
    # |r| < 0.0053 while the result is about r, which needs all of degree 8.
    p = r2 * (muladd(r, 0x1.555555555556p-2, -0.5) +
              r2 * muladd(r, 0x1.999999959554ep-3, -0x1.0000000000006p-2) +
              r4 * muladd(r2, -0x1.0002b8b263fc3p-3, muladd(r, 0x1.2495b9b4845e9p-3, -0x1.555555529a47ap-3)))
    hi = t1 + r
    return hi, ((t1 - hi) + r) + (tl + p)
end

# log(x) for positive normal Float64 x, as hi + lo
@inline function _log_kernel(xu::UInt64)
    _, z, k, invc, logc, logctail = _log_tab_reduce(xu)
    r = fma(z, invc, -1.0) # exact, see `_log_ext`
    return _log_tab_finish(k, logc, logctail, r)
end

# log(x) for positive normal Float64 x, accurate to about 2^-32 (enough for Float32 results).
@inline function _log_kernel_f32(x::Float64)
    _, z, k, invc, logc, _ = _log_tab_reduce(reinterpret(UInt64, x))
    r = fma(z, invc, -1.0) # |r| < 0.0056
    hi = muladd(k, 0.6931471805599453, logc)
    r2 = r*r
    return (hi + r) + muladd(r2, muladd(r, 0.3333333333333333, -0.5), (r2*r2)*-0.25) # log1p(r) - r
end

# Log implementation that returns 2 numbers which sum to give true value with about 68 bits of precision
# Since `log` only makes sense for positive exponents, we speed up the implementation by stealing the sign bit
# of the input for an extra bit of the exponent which is used to normalize subnormal inputs.
# Does not normalize results.
# Adapted and modified from https://github.com/ARM-software/optimized-routines/blob/master/math/pow.c
# Copyright (c) 2018-2020, Arm Limited. (which is also MIT licensed)
# note that this isn't an exact translation as this version compacts the table to reduce cache pressure.
function _log_ext(xu::UInt64)
    # x = 2^k z; where z is in range [0x1.69555p-1,0x1.69555p-0) and exact.
    # The range is split into N subintervals.
    # The ith subinterval contains z and c is near the center of the interval.
    tmp = reinterpret(Int64, xu -% 0x3fe6955500000000) #0x1.69555p-1
    z = reinterpret(Float64, xu -% (tmp & 0xfff0000000000000))
    k = Float64(tmp >> 52)
    # log(x) = k*Ln2 + log(c) + log1p(z/c-1).
    # N.B. :nothrow and :noub since `idx` is known to be `1 ≤ idx ≤ length(t_log_table_compact)`
    idx = (tmp >> 45) & (length(t_log_table_compact)-1) + 1
    t, logctail = Base.@assume_effects :nothrow :noub @inbounds t_log_table_compact[idx]
    invc, logc = log_tab_unpack(t)
    # Note: invc is j/N or j/N/2 where j is an integer in [N,2N) and
    # |z/c - 1| < 1/N, so r = z/c - 1 is exactly representable.
    r = fma(z, invc, -1.0)
    # k*Ln2 + log(c) + r.
    t1 = muladd(k, 0.6931471805598903, logc) #ln(2) hi part
    t2 = t1 + r
    lo1 = muladd(k, 5.497923018708371e-14, logctail) #ln(2) lo part
    lo2 = t1 - t2 + r
    ar = -0.5 * r
    ar2, lo3 = two_mul(r, ar)
    # k*Ln2 + log(c) + r + .5*r*r.
    hi = t2 + ar2
    lo4 = t2 - hi + ar2
    p = evalpoly(r, (-0x1.555555555556p-1, 0x1.0000000000006p-1, -0x1.999999959554ep-2, 0x1.555555529a47ap-2, -0x1.2495b9b4845e9p-2, 0x1.0002b8b263fc3p-2))
    lo = lo1 + lo2 + lo3 + muladd(r*ar2, p, lo4)
    return hi, lo
end
