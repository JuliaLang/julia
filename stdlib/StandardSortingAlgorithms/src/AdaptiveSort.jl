# This file is a part of Julia. License is MIT: https://julialang.org/license

# For AbstractVector{Bool}, counting sort is always best.
# This is an implementation of counting sort specialized for Bools.
function sort!(v::AbstractVector{B}, lo::Integer, hi::Integer, a::AdaptiveSort, o::Ordering,
        t::Union{AbstractVector{B}, Nothing}=nothing) where {B <: Bool}
    first = lt(o, false, true) ? false : lt(o, true, false) ? true : return v
    count = 0
    @inbounds for i in lo:hi
        if v[i] == first
            count += 1
        end
    end
    @inbounds v[lo:lo+count-1] .= first
    @inbounds v[lo+count:hi] .= !first
    v
end
function sort!(v::AbstractVector{T}, lo::Integer, hi::Integer, a::AdaptiveSort, o::Ordering,
            t::Union{AbstractVector{T}, Nothing}=nothing) where T
    # if the sorting task is not UIntMappable, then we can't radix sort or sort_int_range!
    # so we skip straight to the fallback algorithm which is comparison based.
    U = UIntMappable(T, o)
    U === nothing && return sort!(v, lo, hi, a.fallback, o)

    # to avoid introducing excessive detection costs for the trivial sorting problem
    # and to avoid overflow, we check for small inputs before any other runtime checks
    hi <= lo && return v
    lenm1 = maybe_unsigned(hi-lo) # adding 1 would risk overflow
    # only count sort on a short range can compete with insertion sort when lenm1 < 40
    # and the optimization is not worth the detection cost, so we use insertion sort.
    lenm1 < 40 && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

    # For most arrays, a presorted check is cheap (overhead < 5%) and for most large
    # arrays it is essentially free (<1%). Insertion sort runs in a fast O(n) on presorted
    # input and this guarantees presorted input will always be efficiently handled
    issorted(view(v, lo:hi), o) && return v

    # For large arrays, a reverse-sorted check is essentially free (overhead < 1%)
    if lenm1 >= 500 && issorted(view(v, lo:hi), ReverseOrdering(o))
        reverse!(view(v, lo:hi))
        return v
    end

    # UInt128 does not support fast bit shifting so we never
    # dispatch to radix sort but we may still perform count sort
    if sizeof(U) > 8
        if T <: Integer && o isa DirectOrdering
            v_min, v_max = _extrema(v, lo, hi, Forward)
            v_range = maybe_unsigned(v_max-v_min)
            v_range == 0 && return v # all same

            # we know lenm1 ≥ 40, so this will never underflow.
            # if lenm1 > 3.7e18 (59 exabytes), then this may incorrectly dispatch to fallback
            if v_range < 5lenm1-100 # count sort will outperform comparison sort if v's range is small
                return sort_int_range!(v, Int(v_range+1), v_min, o === Forward ? identity : reverse, lo, hi)
            end
        end
        return sort!(v, lo, hi, a.fallback, o)
    end

    v_min, v_max = _extrema(v, lo, hi, o)
    lt(o, v_min, v_max) || return v # all same
    if T <: Integer && o isa DirectOrdering
        R = o === Reverse
        v_range = maybe_unsigned(R ? v_min-v_max : v_max-v_min)
        if v_range < div(lenm1, 2) # count sort will be superior if v's range is very small
            return sort_int_range!(v, Int(v_range+1), R ? v_max : v_min, R ? reverse : identity, lo, hi)
        end
    end

    u_min, u_max = uint_map(v_min, o), uint_map(v_max, o)
    u_range = maybe_unsigned(u_max-u_min)
    if u_range < div(lenm1, 2) # count sort will be superior if u's range is very small
        u = uint_map!(v, lo, hi, o)
        sort_int_range!(u, Int(u_range+1), u_min, identity, lo, hi)
        return uint_unmap!(v, u, lo, hi, o)
    end

    # if u's range is small, then once we subtract out v_min, we'll get a vector like
    # UInt16[0x001a, 0x0015, 0x0006, 0x001b, 0x0008, 0x000c, 0x0001, 0x000e, 0x001c, 0x0009]
    # where we only need to radix over the last few bits (5, in the example).
    bits = unsigned(8sizeof(u_range) - leading_zeros(u_range))

    # radix sort runs in O(bits * lenm1), insertion sort runs in O(lenm1^2). Radix sort
    # has a constant factor that is three times higher, so radix runtime is 3bits * lenm1
    # and insertion runtime is lenm1^2. Empirically, insertion is faster than radix iff
    # lenm1 < 3bits.
    # Insertion < Radix
    #   lenm1^2 < 3 * bits * lenm1
    #     lenm1 < 3bits
    if lenm1 < 3bits
        # at lenm1 = 64*3-1, QuickSort is about 20% faster than InsertionSort.
        alg = a.fallback === QuickSort && lenm1 > 120 ? QuickSort : SMALL_ALGORITHM
        return sort!(v, lo, hi, alg, o)
    end

    # At this point, we are committed to radix sort.
    u = uint_map!(v, lo, hi, o)

    # we subtract u_min to avoid radixing over unnecessary bits. For example,
    # Int32[3, -1, 2] uint_maps to UInt32[0x80000003, 0x7fffffff, 0x80000002]
    # which uses all 32 bits, but once we subtract u_min = 0x7fffffff, we are left with
    # UInt32[0x00000004, 0x00000000, 0x00000003] which uses only 3 bits, and
    # Float32[2.012, 400.0, 12.345] uint_maps to UInt32[0x3fff3b63, 0x3c37ffff, 0x414570a4]
    # which is reduced to UInt32[0x03c73b64, 0x00000000, 0x050d70a5] using only 26 bits.
    # the overhead for this subtraction is small enough that it is worthwhile in many cases.

    # this is faster than u[lo:hi] .-= u_min as of v1.9.0-DEV.100
    @inbounds for i in lo:hi
        u[i] -= u_min
    end

    u2 = radix_sort!(u, lo, hi, bits, reinterpret(U, workspace(v, t, hi)))
    uint_unmap!(v, u2, lo, hi, o, u_min)
end

maybe_unsigned(x::Integer) = x # this is necessary to avoid calling unsigned on BigInt
maybe_unsigned(x::BitSigned) = unsigned(x)
function _extrema(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    mn = mx = v[lo]
    @inbounds for i in (lo+1):hi
        vi = v[i]
        lt(o, vi, mn) && (mn = vi)
        lt(o, mx, vi) && (mx = vi)
    end
    mn, mx
end
