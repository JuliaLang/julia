# This file is a part of Julia. License is MIT: https://julialang.org/license

# Implementation of
#  "Table-driven Implementation of the Logarithm Function in IEEE Floating-point Arithmetic"
#  Tang, Ping-Tak Peter
#  ACM Trans. Math. Softw. (1990), 16(4):378--400
#  https://doi.org/10.1145/98267.98294

# Does not currently handle floating point flags (inexact, div-by-zero, etc).

import .Base.unsafe_trunc
import .Base.Math.@horner
import .Base.TwicePrecision
import .Base.Math.highword


# Float64 lookup table.
# to generate values:
  # N=39 # (can be up to N=42).
  # sN = 2.0^N
  # isN = 1.0/sN
  # s7 = 2.0^7
  # is7 = 1.0/s7
  # for j=0:128
  #   l_big = Base.log(big(1.0+j*is7))
  #   l_hi = isN*Float64(round(sN*l_big))
  #   l_lo = Float64(l_big-l_hi)
  #   j % 2 == 0 && print("\n    ")
  #   print("(",l_hi,",",l_lo,"),")
  # end

const t_log_Float64 = ((0.0,0.0),(0.007782140442941454,-8.865052917267247e-13),
    (0.015504186536418274,-4.530198941364935e-13),(0.0231670592820592,-5.248209479295644e-13),
    (0.03077165866670839,4.529814257790929e-14),(0.0383188643027097,-5.730994833076631e-13),
    (0.04580953603181115,-5.16945692881222e-13),(0.053244514518155484,6.567993368985218e-13),
    (0.06062462181580486,6.299848199383311e-13),(0.06795066190898069,-4.729424109166329e-13),
    (0.07522342123775161,-1.6408301585598662e-13),(0.08244366921098845,8.614512936087814e-14),
    (0.08961215869021544,-5.283050530808144e-13),(0.09672962645890948,-3.5836667430094137e-13),
    (0.10379679368088546,7.581073923016376e-13),(0.11081436634049169,-2.0157368416016215e-13),
    (0.11778303565552051,8.629474042969438e-13),(0.1247034785010328,-7.556920687451337e-14),
    (0.1315763577895268,-8.075373495358435e-13),(0.13840232285838283,7.363043577087051e-13),
    (0.14518200984457508,-7.718001336828099e-14),(0.15191604202664166,-7.996871607743758e-13),
    (0.15860503017574956,8.890223439724663e-13),(0.16524957289584563,-5.384682618788232e-13),
    (0.17185025692742784,-7.686134224018169e-13),(0.17840765747314435,-3.2605717931058157e-13),
    (0.18492233849428885,-2.7685884310448306e-13),(0.1913948530000198,-3.903387893794952e-13),
    (0.1978257433293038,6.160755775588723e-13),(0.20421554142922105,-5.30156516006026e-13),
    (0.21056476910780475,-4.55112422774782e-13),(0.21687393830143264,-8.182853292737783e-13),
    (0.22314355131493357,-7.238189921749681e-13),(0.22937410106533207,-4.86240001538379e-13),
    (0.23556607131286,-9.30945949519689e-14),(0.24171993688651128,6.338907368997553e-13),
    (0.24783616390413954,4.4171755371315547e-13),(0.25391520998164196,-6.785208495970588e-13),
    (0.25995752443668607,2.3999540484211735e-13),(0.2659635484978935,-7.555569400283742e-13),
    (0.27193371548310097,5.407904186145515e-13),(0.2778684510030871,3.692037508208009e-13),
    (0.28376817313073843,-9.3834172236637e-14),(0.28963329258294834,9.43339818951269e-14),
    (0.29546421289342106,4.148131870425857e-13),(0.3012613305781997,-3.7923164802093147e-14),
    (0.3070250352957373,-8.25463138725004e-13),(0.31275571000333,5.668653582900739e-13),
    (0.318453731119007,-4.723727821986367e-13),(0.32411946865431673,-1.0475750058776541e-13),
    (0.32975328637257917,-1.1118671389559323e-13),(0.33535554192167183,-5.339989292003297e-13),
    (0.3409265869704541,1.3912841212197566e-13),(0.3464667673470103,-8.017372713972018e-13),
    (0.35197642315688427,2.9391859187648e-13),(0.3574558889213222,4.815896111723205e-13),
    (0.3629054936900502,-6.817539406325327e-13),(0.36832556115950865,-8.009990055432491e-13),
    (0.3737164097929053,6.787566823158706e-13),(0.37907835293481185,1.5761203773969435e-13),
    (0.3844116989112081,-8.760375990774874e-13),(0.38971675114044046,-4.152515806343612e-13),
    (0.3949938082405424,3.2655698896907146e-13),(0.40024316412745975,-4.4704265010452445e-13),
    (0.4054651081078191,3.452764795203977e-13),(0.4106599249844294,8.390050778518307e-13),
    (0.4158278951435932,1.1776978751369214e-13),(0.4209692946442374,-1.0774341461609579e-13),
    (0.42608439531068143,2.186334329321591e-13),(0.43117346481813,2.413263949133313e-13),
    (0.4362367667745275,3.90574622098307e-13),(0.44127456080423144,6.437879097373207e-13),
    (0.44628710262804816,3.713514191959202e-13),(0.45127464413963025,-1.7166921336082432e-13),
    (0.4562374334818742,-2.8658285157914353e-13),(0.4611757151214988,6.713692791384601e-13),
    (0.46608972992544295,-8.437281040871276e-13),(0.4709797152190731,-2.821014384618127e-13),
    (0.4758459048698569,1.0701931762114255e-13),(0.4806885293455707,1.8119346366441111e-13),
    (0.4855078157816024,9.840465278232627e-14),(0.49030398804461583,5.780031989454028e-13),
    (0.49507726679803454,-1.8302857356041668e-13),(0.4998278695566114,-1.620740015674495e-13),
    (0.5045560107519123,4.83033149495532e-13),(0.5092619017905236,-7.156055317238212e-13),
    (0.5139457511013461,8.882123951857185e-13),(0.5186077642083546,-3.0900580513238243e-13),
    (0.5232481437651586,-6.10765519728515e-13),(0.5278670896204858,3.565996966334783e-13),
    (0.532464798869114,3.5782396591276384e-13),(0.5370414658973459,-4.622608700154458e-13),
    (0.5415972824321216,6.227976291722515e-13),(0.5461324375974073,7.283894727206574e-13),
    (0.5506471179523942,2.680964661521167e-13),(0.5551415075406112,-1.0960825046059278e-13),
    (0.5596157879353996,2.3119493838005378e-14),(0.5640701382853877,-5.846905800529924e-13),
    (0.5685047353526897,-2.1037482511444942e-14),(0.5729197535620187,-2.332318294558741e-13),
    (0.5773153650352469,-4.2333694288141915e-13),(0.5816917396350618,-4.3933937969737843e-13),
    (0.5860490450031648,4.1341647073835564e-13),(0.590387446602108,6.841763641591467e-14),
    (0.5947071077462169,4.758553400443064e-13),(0.5990081896452466,8.367967867475769e-13),
    (0.6032908514389419,-8.576373464665864e-13),(0.6075552502243227,2.1913281229340092e-13),
    (0.6118015411066153,-6.224284253643115e-13),(0.6160298772156239,-1.098359432543843e-13),
    (0.6202404097512044,6.531043137763365e-13),(0.6244332880123693,-4.758019902171077e-13),
    (0.6286086594227527,-3.785425126545704e-13),(0.6327666695706284,4.0939233218678666e-13),
    (0.636907462236195,8.742438391485829e-13),(0.6410311794206791,2.521818845684288e-13),
    (0.6451379613736208,-3.6081313604225574e-14),(0.649227946625615,-5.05185559242809e-13),
    (0.6533012720119586,7.869940332335532e-13),(0.6573580727090302,-6.702087696194906e-13),
    (0.6613984822452039,1.6108575753932459e-13),(0.6654226325445052,5.852718843625151e-13),
    (0.6694306539429817,-3.5246757297904794e-13),(0.6734226752123504,-1.8372084495629058e-13),
    (0.6773988235909201,8.860668981349492e-13),(0.6813592248072382,6.64862680714687e-13),
    (0.6853040030982811,6.383161517064652e-13),(0.6892332812385575,2.5144230728376075e-13),
    (0.6931471805601177,-1.7239444525614835e-13))


# Float32 lookup table
# to generate values:
  # N=16
  # sN = 2f0^N
  # isN = 1f0/sN
  # s7 = 2.0^7
  # is7 = 1.0/s7
  # for j=0:128
  #   j % 4 == 0 && print("\n    ")
  #   print(float64(Base.log(big(1.0+j*is7))),",")
  # end

const t_log_Float32 = (0.0,0.007782140442054949,0.015504186535965254,0.02316705928153438,
    0.030771658666753687,0.0383188643021366,0.0458095360312942,0.053244514518812285,
    0.06062462181643484,0.06795066190850775,0.07522342123758753,0.08244366921107459,
    0.08961215868968714,0.09672962645855111,0.10379679368164356,0.11081436634029011,
    0.11778303565638346,0.12470347850095724,0.13157635778871926,0.13840232285911913,
    0.1451820098444979,0.15191604202584197,0.15860503017663857,0.16524957289530717,
    0.17185025692665923,0.1784076574728183,0.184922338494012,0.19139485299962947,
    0.19782574332991987,0.2042155414286909,0.21056476910734964,0.21687393830061436,
    0.22314355131420976,0.22937410106484582,0.2355660713127669,0.24171993688714516,
    0.24783616390458127,0.25391520998096345,0.25995752443692605,0.26596354849713794,
    0.27193371548364176,0.2778684510034563,0.2837681731306446,0.28963329258304266,
    0.2954642128938359,0.3012613305781618,0.3070250352949119,0.3127557100038969,
    0.3184537311185346,0.324119468654212,0.329753286372468,0.3353555419211378,
    0.3409265869705932,0.34646676734620857,0.3519764231571782,0.3574558889218038,
    0.3629054936893685,0.3683255611587076,0.37371640979358406,0.37907835293496944,
    0.38441169891033206,0.3897167511400252,0.394993808240869,0.4002431641270127,
    0.4054651081081644,0.4106599249852684,0.415827895143711,0.42096929464412963,
    0.4260843953109001,0.4311734648183713,0.43623676677491807,0.4412745608048752,
    0.44628710262841953,0.45127464413945856,0.4562374334815876,0.46117571512217015,
    0.46608972992459924,0.470979715218791,0.4758459048699639,0.4806885293457519,
    0.4855078157817008,0.4903039880451938,0.4950772667978515,0.4998278695564493,
    0.5045560107523953,0.5092619017898079,0.5139457511022343,0.5186077642080457,
    0.5232481437645479,0.5278670896208424,0.5324647988694718,0.5370414658968836,
    0.5415972824327444,0.5461324375981357,0.5506471179526623,0.5551415075405016,
    0.5596157879354227,0.564070138284803,0.5685047353526688,0.5729197535617855,
    0.5773153650348236,0.5816917396346225,0.5860490450035782,0.5903874466021763,
    0.5947071077466928,0.5990081896460834,0.6032908514380843,0.6075552502245418,
    0.6118015411059929,0.616029877215514,0.6202404097518576,0.6244332880118935,
    0.6286086594223741,0.6327666695710378,0.6369074622370692,0.6410311794209312,
    0.6451379613735847,0.6492279466251099,0.6533012720127457,0.65735807270836,
    0.661398482245365,0.6654226325450905,0.6694306539426292,0.6734226752121667,
    0.6773988235918061,0.6813592248079031,0.6853040030989194,0.689233281238809,
    0.6931471805599453)

# determine if hardware FMA is available
# should probably check with LLVM, see #9855.
const FMA_NATIVE = muladd(nextfloat(1.0),nextfloat(1.0),-nextfloat(1.0,2)) != 0

# truncate lower order bits (up to 26)
# ideally, this should be able to use ANDPD instructions, see #9868.
@inline function truncbits(x::Float64)
    reinterpret(Float64, reinterpret(UInt64,x) & 0xffff_ffff_f800_0000)
end


# Procedure 1
@inline function log_proc1(y::Float64,mf::Float64,F::Float64,f::Float64,jp::Int)
    ## Steps 1 and 2
    @inbounds hi,lo = t_log_Float64[jp]
    l_hi = mf* 0.6931471805601177 + hi
    l_lo = mf*-1.7239444525614835e-13 + lo

    ## Step 3
    # @inbounds u = f*c_invF[jp]
    # u = f/F
    # q = u*u*@horner(u,
    #                 -0x1.0_0000_0000_0001p-1,
    #                 +0x1.5_5555_5550_9ba5p-2,
    #                 -0x1.f_ffff_ffeb_6526p-3,
    #                 +0x1.9_99b4_dfed_6fe4p-3,
    #                 -0x1.5_5576_6647_2e04p-3)

    ## Step 3' (alternative)
    u = (2.0f)/(y+F)
    v = u*u
    q = u*v*@horner(v,
                    0.08333333333303913,
                    0.012500053168098584)

    ## Step 4
    l_hi + (u + (q + l_lo))
end

# Procedure 2
@inline function log_proc2(f::Float64)
    ## Step 1
    g = 1.0/(2.0+f)
    u = 2.0*f*g
    v = u*u

    ## Step 2
    q = u*v*@horner(v,
                    0.08333333333333179,
                    0.012500000003771751,
                    0.0022321399879194482,
                    0.0004348877777076146)

    ## Step 3
    # based on:
    #   2(f-u) = 2(f(2+f)-2f)/(2+f) = 2f^2/(2+f) = fu
    #   2(f-u1-u2) - f*(u1+u2) = 0
    #   2(f-u1) - f*u1 = (2+f)u2
    #   u2 = (2(f-u1) - f*u1)/(2+f)
    if FMA_NATIVE
        return u + fma(fma(-u,f,2(f-u)), g, q)
    else
        u1 = truncbits(u) # round to 24 bits
        f1 = truncbits(f)
        f2 = f-f1
        u2 = ((2.0*(f-u1)-u1*f1)-u1*f2)*g
        ## Step 4
        return u1 + (u2 + q)
    end
end


@inline function log_proc1(y::Float32,mf::Float32,F::Float32,f::Float32,jp::Int)
    ## Steps 1 and 2
    @inbounds hi = t_log_Float32[jp]
    l = mf*0.6931471805599453 + hi

    ## Step 3
    # @inbounds u = f*c_invF[jp]
    # q = u*u*@horner(u,
    #                 Float32(-0x1.00006p-1),
    #                 Float32(0x1.55546cp-2))

    ## Step 3' (alternative)
    u = (2f0f)/(y+F)
    v = u*u
    q = u*v*0.08333351f0

    ## Step 4
    Float32(l + (u + q))
end

@inline function log_proc2(f::Float32)
    ## Step 1
    # compute in higher precision
    u64 = Float64(2f0*f)/(2.0+f)
    u = Float32(u64)
    v = u*u

    ## Step 2
    q = u*v*@horner(v,
                    0.08333332f0,
                    0.012512346f0)

    ## Step 3: not required

    ## Step 4
    Float32(u64 + q)
end


function log(x::Float64)
    if x > 0.0
        x == Inf && return x

        # Step 2
        if 0.9394130628134757 < x < 1.0644944589178595
            f = x-1.0
            return log_proc2(f)
        end

        # Step 3
        xu = reinterpret(UInt64,x)
        m = Int(xu >> 52) & 0x07ff
        if m == 0 # x is subnormal
            x *= 1.8014398509481984e16 # 0x1p54, normalise significand
            xu = reinterpret(UInt64,x)
            m = Int(xu >> 52) & 0x07ff - 54
        end
        m -= 1023
        y = reinterpret(Float64,(xu & 0x000f_ffff_ffff_ffff) | 0x3ff0_0000_0000_0000)

        mf = Float64(m)
        F = (y + 3.5184372088832e13) - 3.5184372088832e13 # 0x1p-7*round(0x1p7*y)
        f = y-F
        jp = unsafe_trunc(Int,128.0*F)-127

        return log_proc1(y,mf,F,f,jp)
    elseif x == 0.0
        -Inf
    elseif isnan(x)
        NaN
    else
        throw_complex_domainerror(:log, x)
    end
end

function log(x::Float32)
    if x > 0f0
        x == Inf32 && return x

        # Step 2
        if 0.939413f0 < x < 1.0644945f0
            f = x-1f0
            return log_proc2(f)
        end

        # Step 3
        xu = reinterpret(UInt32,x)
        m = Int(xu >> 23) & 0x00ff
        if m == 0 # x is subnormal
            x *= 3.3554432f7 # 0x1p25, normalise significand
            xu = reinterpret(UInt32,x)
            m = Int(xu >> 23) & 0x00ff - 25
        end
        m -= 127
        y = reinterpret(Float32,(xu & 0x007f_ffff) | 0x3f80_0000)

        mf = Float32(m)
        F = (y + 65536.0f0) - 65536.0f0 # 0x1p-7*round(0x1p7*y)
        f = y-F
        jp = unsafe_trunc(Int,128.0f0*F)-127

        log_proc1(y,mf,F,f,jp)
    elseif x == 0f0
        -Inf32
    elseif isnan(x)
        NaN32
    else
        throw_complex_domainerror(:log, x)
    end
end


function log1p(x::Float64)
    if x > -1.0
        x == Inf && return x
        if -1.1102230246251565e-16 < x < 1.1102230246251565e-16
            return x # Inexact

        # Step 2
        elseif -0.06058693718652422 < x < 0.06449445891785943
            return log_proc2(x)
        end

        # Step 3
        z = 1.0 + x
        zu = reinterpret(UInt64,z)
        s = reinterpret(Float64,0x7fe0_0000_0000_0000 - (zu & 0xfff0_0000_0000_0000)) # 2^-m
        m = Int(zu >> 52) & 0x07ff - 1023 # z cannot be subnormal
        c = m > 0 ? 1.0-(z-x) : x-(z-1.0) # 1+x = z+c exactly
        y = reinterpret(Float64,(zu & 0x000f_ffff_ffff_ffff) | 0x3ff0_0000_0000_0000)

        mf = Float64(m)
        F = (y + 3.5184372088832e13) - 3.5184372088832e13 # 0x1p-7*round(0x1p7*y)
        f = (y - F) + c*s #2^m(F+f) = 1+x = z+c
        jp = unsafe_trunc(Int,128.0*F)-127

        log_proc1(y,mf,F,f,jp)
    elseif x == -1.0
        -Inf
    elseif isnan(x)
        NaN
    else
        throw_complex_domainerror(:log1p, x)
    end
end

function log1p(x::Float32)
    if x > -1f0
        x == Inf32 && return x
        if -5.9604645f-8 < x < 5.9604645f-8
            return x # Inexact
        # Step 2
        elseif -0.06058694f0 < x < 0.06449446f0
            return log_proc2(x)
        end

        # Step 3
        z = 1f0 + x
        zu = reinterpret(UInt32,z)
        s = reinterpret(Float32,0x7f000000 - (zu & 0xff80_0000)) # 2^-m
        m = Int(zu >> 23) & 0x00ff - 127 # z cannot be subnormal
        c = m > 0 ? 1f0-(z-x) : x-(z-1f0) # 1+x = z+c
        y = reinterpret(Float32,(zu & 0x007f_ffff) | 0x3f80_0000)

        mf = Float32(m)
        F = (y + 65536.0f0) - 65536.0f0 # 0x1p-7*round(0x1p7*y)
        f = (y - F) + s*c #2^m(F+f) = 1+x = z+c
        jp = unsafe_trunc(Int,128.0*F)-127

        log_proc1(y,mf,F,f,jp)
    elseif x == -1f0
        -Inf32
    elseif isnan(x)
        NaN32
    else
        throw_complex_domainerror(:log1p, x)
    end
end

for f in (:log,:log1p)
    @eval begin
        ($f)(x::Real) = ($f)(float(x))
    end
end

# Below is line are log10 and log2 ports from Openlibm. The following copyright
# notice is taken from there, but the original source is FDLIBM (see LICENSE.md).
# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunSoft, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================

# Constants and utilities
# Argument reduction
"""
    k, f = _log_range_reduction(x)

Given `realmin(x) < x < Inf`, compute

    x == 2^k * (1+f)

where `k` is an integer and `√2/2 ~< 1+f ~< √2`.
"""
@inline function _log_range_reduction(x::T) where T <: Union{Float32, Float64}
    u = reinterpret(Unsigned, x)
    k = (u >> significand_bits(T))%Int - exponent_bias(T) # exponent(T)

    h = highword(u)  # assume that x > 0
    if h & highword(significand_mask(T)) >= highword(sqrt(T(2))) & highword(significand_mask(T))
        k += 1
        w = exponent_half(T)
    else
        w = exponent_one(T)
    end
    x = reinterpret(T, (u & significand_mask(T)) | w)
    f = x-1
    return f, k
end


_trunc_lo(x::Float64) = reinterpret(Float64, reinterpret(UInt64, x) & 0xffff_ffff_0000_0000)
_trunc_lo(x::Float32) = reinterpret(Float32, reinterpret(UInt32, x) & 0xffff_f000)


"""
    _log_kernel(f, y, invlnb, invl2b)

The kernel function of the logarithm.
 - `f, y` are the output of `_log_range_reduction`
 - `invlnb` is `1/log(base)` (computed using extended precision)
 - `invlog2b` is `1/log2(base)` (computed using extended precision, or 1)
"""
@inline function _log_kernel(f::T, k, invlnb, invlog2b) where {T<:Union{Float32,Float64}}
    s  = f/(2+f)
    s² = s * s
    s⁴ = s² * s²
    if T == Float32
        t = s² * @horner(s⁴, 0.6666666f0, 0.28498787f0) +
            s⁴ * @horner(s⁴, 0.40000972f0, 0.24279079f0)
    elseif T == Float64
        t = s² * @horner(s⁴, 6.666666666666735130e-01,
                         2.857142874366239149e-01,
                         1.818357216161805012e-01,
                         1.479819860511658591e-01) +
            s⁴ * @horner(s⁴, 3.999999999940941908e-01,
                         2.222219843214978396e-01,
                         1.531383769920937332e-01)
    end
    hf² = f*f/2
    r = s*(hf²+t) # log(1+f) - f + f^2/2

    A_hi = _trunc_lo(f - hf²)
    A_lo = (f - A_hi) - hf² + r

    if invlog2b isa TwicePrecision
        B_hi = k*invlog2b.hi
        B_lo = k*invlog2b.lo
    else
        B_hi = T(k*invlog2b)
        B_lo = -zero(T) # exploit the fact that -0.0 + x is a no-op
    end

    V_hi = A_hi*invlnb.hi
    V_lo = B_lo + (A_lo+A_hi)*invlnb.lo + A_lo*invlnb.hi
    if T == Float32
        return V_lo + V_hi + B_hi
    elseif T == Float64
        # Extra precision in for adding y*log10_2hi is not strictly needed
        # since there is no very large cancellation near x = sqrt(2) or
        # x = 1/sqrt(2), but we do it anyway since it costs little on CPUs
        # with some parallelism and it reduces the error for many args.

        W_hi = B_hi + V_hi
        W_lo = V_lo + ((B_hi - W_hi) + V_hi)
        return W_hi + W_lo
    end
end

@inline function _log_base(x::T, invlnb, invlog2b) where {T}
    j = 0
    if x <= realmin(T)
        if x <= 0
            if x == 0
                return -T(Inf) # log(+-0)
            else
                throw(DomainError(x, "log(b,x) is only defined for non-negative x."))
            end
        end
        x *= maxintfloat(T)/2
        j -= significand_bits(T)
    elseif !isfinite(x)
        return x # +Inf/NaN
    end
    # x == 1       && return T(0) # logk(1) = +0 for any k

    f, k = _log_range_reduction(x)
    _log_kernel(f, k+j, invlnb, invlog2b)
end

# Wrapper function for logs
invln2x(::Type{Float32}) = TwicePrecision(1.4428710938f+00, -1.7605285393f-04)
invln2x(::Type{Float64}) = TwicePrecision(1.44269504072144627571e+00, 1.67517131648865118353e-10)

log2(x::Real) = log2(float(x))
log2(x::T) where T<:Union{Float32, Float64} =
    _log_base(x, invln2x(T), 1)

invln10x(::Type{Float32}) = TwicePrecision(4.3432617188f-01, -3.1689971365f-05)
invln10x(::Type{Float64}) = TwicePrecision(4.34294481878168880939e-01, 2.50829467116452752298e-11)

invlb10x(::Type{Float32}) = TwicePrecision(3.0102920532f-01, 7.9034151668f-07)
invlb10x(::Type{Float64}) = TwicePrecision(3.01029995663611771306e-01, 3.69423907715893078616e-13)

log10(x::Real) = log10(float(x))
log10(x::T) where T<:Union{Float32, Float64} =
    _log_base(x, invln10x(T), invlb10x(T))
