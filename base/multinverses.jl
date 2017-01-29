# This file is a part of Julia. License is MIT: http://julialang.org/license

module MultiplicativeInverses

import Base: div, divrem, rem, unsigned
using  Base: LinearFast, LinearSlow, tail
export multiplicativeinverse

unsigned(::Type{Int8}) = UInt8
unsigned(::Type{Int16}) = UInt16
unsigned(::Type{Int32}) = UInt32
unsigned(::Type{Int64}) = UInt64
unsigned(::Type{Int128}) = UInt128
unsigned{T<:Unsigned}(::Type{T}) = T

abstract MultiplicativeInverse{T}

# Computes integer division by a constant using multiply, add, and bitshift.

# The idea here is to compute floor(n/d) as floor(m*n/2^p) and then
# implement division by 2^p as a right bitshift.  The trick is finding
# m (the "magic number") and p. Roughly speaking, one can think of this as
#        floor(n/d) = floor((n/2^p) * (2^p/d))
# so that m is effectively 2^p/d.
#
# A few examples are illustrative:
# Division of Int32 by 3:
#   floor((2^32+2)/3 * n/2^32) = floor(n/3 + 2n/(3*2^32))
# The correction term, 2n/(3*2^32), is strictly less than 1/3 for any
# nonnegative n::Int32, so this divides any nonnegative Int32 by 3.
# (When n < 0, we add 1, and one can show that this computes
# ceil(n/d) = -floor(abs(n)/d).)
#
# Division of Int32 by 5 uses a magic number (2^33+3)/5 and then
# right-shifts by 33 rather than 32. Consequently, the size of the
# shift depends on the specific denominator.
#
# Division of Int32 by 7 would be problematic, because a viable magic
# number of (2^34+5)/7 is too big to represent as an Int32 (the
# unsigned representation needs 32 bits). We can exploit wrap-around
# and use (2^34+5)/7 - 2^32 (an Int32 < 0), and then correct the
# 64-bit product with an add (the `addmul` field below).
#
# Further details can be found in Hacker's Delight, Chapter 10.

immutable SignedMultiplicativeInverse{T<:Signed} <: MultiplicativeInverse{T}
    divisor::T
    multiplier::T
    addmul::Int8
    shift::UInt8

    function SignedMultiplicativeInverse{T}(d::T) where T<:Signed
        d == 0 && throw(ArgumentError("cannot compute magic for d == $d"))
        signedmin = unsigned(typemin(T))
        UT = unsigned(T)

        # Algorithm from Hacker's Delight, section 10-4
        ad = unsigned(abs(d))
        t = signedmin + signbit(d)
        anc = t - one(UT) - rem(t, ad)   # absolute value of nc
        p = sizeof(d)*8 - 1
        q1, r1 = divrem(signedmin, anc)
        q2, r2 = divrem(signedmin, ad)
        while true
            p += 1                       # loop until we find a satisfactory p
            # update q1, r1 = divrem(2^p, abs(nc))
            q1 = q1<<1
            r1 = r1<<1
            if r1 >= anc                 # must be unsigned comparison
                q1 += one(UT)
                r1 -= anc
            end
            # update q2, r2 = divrem(2^p, abs(d))
            q2 = q2<<1
            r2 = r2<<1
            if r2 >= ad
                q2 += one(UT)
                r2 -= ad
            end
            delta = ad - r2
            (q1 < delta || (q1 == delta && r1 == 0)) || break
        end

        m = flipsign((q2 + one(UT)) % T, d)  # resulting magic number
        s = p - sizeof(d)*8                  # resulting shift
        new(d, m, d > 0 && m < 0 ? Int8(1) : d < 0 && m > 0 ? Int8(-1) : Int8(0), UInt8(s))
    end
end
SignedMultiplicativeInverse(x::Signed) = SignedMultiplicativeInverse{typeof(x)}(x)

immutable UnsignedMultiplicativeInverse{T<:Unsigned} <: MultiplicativeInverse{T}
    divisor::T
    multiplier::T
    add::Bool
    shift::UInt8

    function UnsignedMultiplicativeInverse{T}(d::T) where T<:Unsigned
        d == 0 && throw(ArgumentError("cannot compute magic for d == $d"))
        u2 = convert(T, 2)
        add = false
        signedmin = one(d) << (sizeof(d)*8-1)
        signedmax = signedmin - one(T)
        allones = (zero(d) - 1) % T

        nc = allones - rem(convert(T, allones - d), d)
        p = 8*sizeof(d) - 1
        q1, r1 = divrem(signedmin, nc)
        q2, r2 = divrem(signedmax, d)
        while true
            p += 1
            if r1 >= convert(T, nc - r1)
                q1 = q1 + q1 + one(T)
                r1 = r1 + r1 - nc
            else
                q1 = q1 + q1
                r1 = r1 + r1
            end
            if convert(T, r2 + one(T)) >= convert(T, d - r2)
                add |= q2 >= signedmax
                q2 = q2 + q2 + one(T)
                r2 = r2 + r2 + one(T) - d
            else
                add |= q2 >= signedmin
                q2 = q2 + q2
                r2 = r2 + r2 + one(T)
            end
            delta = d - one(T) - r2
            (p < sizeof(d)*16 && (q1 < delta || (q1 == delta && r1 == 0))) || break
        end
        m = q2 + one(T)              # resulting magic number
        s = p - sizeof(d)*8 - add    # resulting shift
        new(d, m, add, s % UInt8)
    end
end
UnsignedMultiplicativeInverse(x::Unsigned) = UnsignedMultiplicativeInverse{typeof(x)}(x)

function div{T}(a::T, b::SignedMultiplicativeInverse{T})
    x = ((widen(a)*b.multiplier) >>> sizeof(a)*8) % T
    x += (a*b.addmul) % T
    ifelse(abs(b.divisor) == 1, a*b.divisor, (signbit(x) + (x >> b.shift)) % T)
end
function div{T}(a::T, b::UnsignedMultiplicativeInverse{T})
    x = ((widen(a)*b.multiplier) >>> sizeof(a)*8) % T
    x = ifelse(b.add, convert(T, convert(T, (convert(T, a - x) >>> 1)) + x), x)
    ifelse(b.divisor == 1, a, x >>> b.shift)
end

rem{T}(a::T, b::MultiplicativeInverse{T}) =
    a - div(a, b)*b.divisor

function divrem{T}(a::T, b::MultiplicativeInverse{T})
    d = div(a, b)
    (d, a - d*b.divisor)
end

multiplicativeinverse(x::Signed) = SignedMultiplicativeInverse(x)
multiplicativeinverse(x::Unsigned) = UnsignedMultiplicativeInverse(x)

end
