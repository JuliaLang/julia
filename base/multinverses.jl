module MultiplicativeInverses

import Base: div, divrem, rem
using  Base: LinearFast, LinearSlow, tail
export multiplicativeinverse

unsigned_type(::Int8) = UInt8
unsigned_type(::Int16) = UInt16
unsigned_type(::Int32) = UInt32
unsigned_type(::Int64) = UInt64
unsigned_type(::Int128) = UInt128

abstract MultiplicativeInverse{T}

immutable SignedMultiplicativeInverse{T<:Signed} <: MultiplicativeInverse{T}
    divisor::T
    multiplier::T
    addmul::Int8
    shift::UInt8

    function SignedMultiplicativeInverse(d::T)
        d == 0 && error("cannot compute magic for d == $d")
        ut = unsigned_type(d)
        signedmin = reinterpret(ut, typemin(d))

        ad::ut = abs(d)
        t::ut = signedmin + signbit(d)
        anc::ut = t - 1 - rem(t, ad)   # absolute value of nc
        p = sizeof(d)*8 - 1            # initialize p
        q1::ut, r1::ut = divrem(signedmin, anc)
        q2::ut, r2::ut = divrem(signedmin, ad)
        while true
            p += 1
            q1 *= 2                    # update q1 = 2p/abs(nc)
            r1 *= 2                    # update r1 = rem(2p/abs(nc))
            if r1 >= anc               # must be unsigned comparison
                q1 += 1
                r1 -= anc
            end
            q2 *= 2                    # update q2 = 2p/abs(d)
            r2 *= 2                    # update r2 = rem(2p/abs(d))
            if r2 >= ad                # must be unsigned comparison
                q2 += 1
                r2 -= ad
            end
            delta::ut = ad - r2
            (q1 < delta || (q1 == delta && r1 == 0)) || break
        end

        m = flipsign((q2 + 1) % T, d)    # resulting magic number
        s = p - sizeof(d)*8                   # resulting shift
        new(d, m, d > 0 && m < 0 ? Int8(1) : d < 0 && m > 0 ? Int8(-1) : Int8(0), UInt8(s))
    end
end
SignedMultiplicativeInverse(x::Signed) = SignedMultiplicativeInverse{typeof(x)}(x)

immutable UnsignedMultiplicativeInverse{T<:Unsigned} <: MultiplicativeInverse{T}
    divisor::T
    multiplier::T
    add::Bool
    shift::UInt8

    function UnsignedMultiplicativeInverse(d::T)
        d == 0 && error("cannot compute magic for d == $d")
        u2 = convert(T, 2)
        add = false
        signedmin::typeof(d) = one(d) << (sizeof(d)*8-1)
        signedmax::typeof(d) = signedmin - 1
        allones = (zero(d) - 1) % T

        nc::typeof(d) = allones - rem(convert(T, allones - d), d)
        p = 8*sizeof(d) - 1                    # initialize p
        q1::typeof(d), r1::typeof(d) = divrem(signedmin, nc)
        q2::typeof(d), r2::typeof(d) = divrem(signedmax, d)
        while true
            p += 1
            if r1 >= convert(T, nc - r1)
                q1 = q1 + q1 + T(1)     # update q1
                r1 = r1 + r1 - nc       # update r1
            else
                q1 = q1 + q1            # update q1
                r1 = r1 + r1            # update r1
            end
            if convert(T, r2 + T(1)) >= convert(T, d - r2)
                add |= q2 >= signedmax
                q2 = q2 + q2 + 1        # update q2
                r2 = r2 + r2 + T(1) - d # update r2
            else
                add |= q2 >= signedmin
                q2 = q2 + q2            # update q2
                r2 = r2 + r2 + T(1)     # update r2
            end
            delta::typeof(d) = d - 1 - r2
            (p < sizeof(d)*16 && (q1 < delta || (q1 == delta && r1 == 0))) || break
        end
        m = q2 + 1                   # resulting magic number
        s = p - sizeof(d)*8 - add    # resulting shift
        new(d, m % T, add, s % UInt8)
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
