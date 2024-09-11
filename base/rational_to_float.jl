# This file is a part of Julia. License is MIT: https://julialang.org/license

module RationalToFloat

# Performance optimization. Unlike raw `<<` or `>>>`, this is supposed
# to compile to a single instruction, because the semantics correspond
# to what hardware usually provides.
function machine_shift(shift::S, a::T, b) where {S,T<:Base.BitInteger}
    @inline begin
        mask = 8*sizeof(T) - 1
        c = b & mask
        shift(a, c)
    end
end

machine_shift(::S, a::Bool, ::Any) where {S} = error("unsupported")

# Fallback for `BigInt` etc.
machine_shift(shift::S, a, b) where {S} = shift(a, b)

# Arguments are positive integers.
function div_significand_with_remainder(num, den, minimum_significand_size)
    clamped = x -> max(zero(x), x)::typeof(x)
    bw = Base.top_set_bit  # bit width
    shift = clamped(minimum_significand_size + bw(den) - bw(num) + 0x2)
    t = machine_shift(<<, num, shift)
    (divrem(t, den, RoundToZero)..., shift)
end

# `divrem(n, 1<<k, RoundToZero)`
function divrem_2(n, k)
    quo = machine_shift(>>>, n,   k)
    tmp = machine_shift(<<,  quo, k)
    rem = n - tmp
    (quo, rem)
end

function to_float_components_impl(num, den, precision, max_subnormal_exp)
    # `+1` because we need an extra, "round", bit for some rounding modes.
    #
    # TODO: as a performance optimization, only do this when required
    #       by the rounding mode
    prec_p_1 = precision + true

    (quo0, rem0, shift) = div_significand_with_remainder(num, den, prec_p_1)
    width = Base.top_set_bit(quo0)
    excess_width = width - prec_p_1
    exp = width - shift - true

    exp_underflow = if isnothing(max_subnormal_exp)
        zero(exp)
    else
        let d = max_subnormal_exp - exp, T = typeof(d), z = zero(d)::T
            (signbit(d) ? z : d + true)::T
        end
    end

    (quo1, rem1) = divrem_2(quo0, exp_underflow + excess_width)
    integral_significand = quo1 >>> true
    round_bit = quo1 % Bool
    sticky_bit = !iszero(rem1) | !iszero(rem0)

    (; integral_significand, exponent = exp, round_bit, sticky_bit)
end

struct RoundingIncrementHelper
    final_bit::Bool
    round_bit::Bool
    sticky_bit::Bool
end

(h::RoundingIncrementHelper)(::Base.Rounding.FinalBit) = h.final_bit
(h::RoundingIncrementHelper)(::Base.Rounding.RoundBit) = h.round_bit
(h::RoundingIncrementHelper)(::Base.Rounding.StickyBit) = h.sticky_bit

function to_float_components_rounded(num, den, precision, max_subnormal_exp, romo, sign_bit)
    overflows = (x, p) -> x == machine_shift(<<, one(x), p)
    t = to_float_components_impl(num, den, precision, max_subnormal_exp)
    raw_significand = t.integral_significand
    rh = RoundingIncrementHelper(raw_significand % Bool, t.round_bit, t.sticky_bit)
    incr = Base.Rounding.correct_rounding_requires_increment(rh, romo, sign_bit)
    rounded = raw_significand + incr
    (integral_significand, exponent) = let exp = t.exponent
        if overflows(rounded, precision)
            (rounded >>> true, exp + true)
        else
            (rounded, exp)
        end
    end
    (; integral_significand, exponent)
end

function to_float_components(::Type{T}, num, den, precision, max_subnormal_exp, romo, sb) where {T}
    to_float_components_rounded(abs(T(num)), den, precision, max_subnormal_exp, romo, sb)
end

function to_floating_point_fallback(::Type{T}, ::Type{S}, num, den, rm, prec) where {T,S}
    num_is_zero = iszero(num)
    den_is_zero = iszero(den)
    sb = signbit(num)
    is_zero = num_is_zero & !den_is_zero
    is_inf = !num_is_zero & den_is_zero
    is_regular = !num_is_zero & !den_is_zero
    if is_regular
        let
            c = to_float_components(S, num, den, prec, nothing, rm, sb)
            signif = T(c.integral_significand)::T
            x = ldexp(signif, c.exponent - prec + true)::T
            sb ? -x : x
        end::T
    else
        if is_zero
            zero(T)::T
        elseif is_inf
            T(Inf)::T
        else
            T(NaN)::T
        end
    end::T
end

function to_floating_point_impl(::Type{T}, ::Type{S}, num, den, rm, prec) where {T,S}
    to_floating_point_fallback(T, S, num, den, rm, prec)
end

function to_floating_point_impl(::Type{T}, ::Type{S}, num, den, rm, prec) where {T<:Base.IEEEFloat,S}
    num_is_zero = iszero(num)
    den_is_zero = iszero(den)
    sb = signbit(num)
    is_zero = num_is_zero & !den_is_zero
    is_inf = !num_is_zero & den_is_zero
    is_regular = !num_is_zero & !den_is_zero
    Rnd = Base.Rounding
    (rm_is_to_zero, rm_is_from_zero) = if Rnd.rounds_to_nearest(rm)
        (false, false)
    else
        let from = Rnd.rounds_away_from_zero(rm, sb)
            (!from, from)
        end
    end::NTuple{2,Bool}
    exp_max = Base.exponent_max(T)
    exp_min = Base.exponent_min(T)
    ieee_repr = Base.ieee754_representation
    repr_zero = ieee_repr(T, sb, Val(:zero))
    repr_inf  = ieee_repr(T, sb, Val(:inf))
    repr_nan  = ieee_repr(T, sb, Val(:nan))
    U = typeof(repr_zero)
    repr_zero::U
    repr_inf::U
    repr_nan::U

    ret_u = if is_regular
        let
            c = to_float_components(S, num, den, prec, exp_min - 1, rm, sb)
            exp = c.exponent
            exp_diff = exp - exp_min
            is_normal = 0 ≤ exp_diff
            exp_is_huge_p = exp_max < exp
            exp_is_huge_n = signbit(exp_diff + prec)
            rounds_to_inf  = exp_is_huge_p & !rm_is_to_zero
            rounds_to_zero = exp_is_huge_n & !rm_is_from_zero

            if !rounds_to_zero & !exp_is_huge_p
                let signif = (c.integral_significand % U) & Base.significand_mask(T)
                    exp_field = (max(exp_diff, zero(exp_diff)) + is_normal) % U
                    ieee_repr(T, sb, exp_field, signif)::U
                end
            elseif rounds_to_zero
                repr_zero
            elseif rounds_to_inf
                repr_inf
            else
                ieee_repr(T, sb, Val(:omega))
            end
        end
    else
        if is_zero
            repr_zero
        elseif is_inf
            repr_inf
        else
            repr_nan
        end
    end::U

    reinterpret(T, ret_u)::T
end

# `BigInt` is a safe default.
to_float_promote_type(::Type{F}, ::Type{S}) where {F,S} = BigInt

const BitIntegerOrBool  = Union{Bool,Base.BitInteger}

# As an optimization, use an integer type narrower than `BigInt` when possible.
function to_float_promote_type(::Type{F}, ::Type{S}) where {F<:Base.IEEEFloat,S<:BitIntegerOrBool}
    Max = if sizeof(F) ≤ sizeof(S)
        S
    else
        (S <: Signed) ? Base.inttype(F) : Base.uinttype(F)
    end
    widen(Max)
end

function to_floating_point(::Type{F}, num::T, den::T, rm, prec) where {F,T}
    S = to_float_promote_type(F, T)
    to_floating_point_impl(F, S, num, den, rm, prec)
end

end
