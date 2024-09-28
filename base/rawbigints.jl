# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Segment of raw words of bits interpreted as a big integer. Less
significant words come first. Each word is in machine-native bit-order.
"""
struct RawBigInt{T<:Unsigned}
    d::String
    word_count::Int

    function RawBigInt{T}(d::String, word_count::Int) where {T<:Unsigned}
        new{T}(d, word_count)
    end
end

elem_count(x::RawBigInt, ::Val{:words}) = x.word_count
elem_count(x::Unsigned, ::Val{:bits}) = sizeof(x) * 8
word_length(::RawBigInt{T}) where {T} = elem_count(zero(T), Val(:bits))
elem_count(x::RawBigInt{T}, ::Val{:bits}) where {T} = word_length(x) * elem_count(x, Val(:words))
reversed_index(n::Int, i::Int) = n - i - 1
reversed_index(x, i::Int, v::Val) = reversed_index(elem_count(x, v), i)::Int
split_bit_index(x::RawBigInt, i::Int) = divrem(i, word_length(x), RoundToZero)

function get_elem_words_raw(x::RawBigInt{T}, i::Int) where {T}
    @boundscheck if (i < 0) || (elem_count(x, Val(:words)) ≤ i)
        throw(BoundsError(x, i))
    end
    d = x.d
    j = i + 1
    (GC.@preserve d unsafe_load(Ptr{T}(pointer(d)), j))::T
end

"""
`i` is the zero-based index of the wanted word in `x`, starting from
the less significant words.
"""
function get_elem(x::RawBigInt, i::Int, ::Val{:words}, ::Val{:ascending})
    @inbounds @inline get_elem_words_raw(x, i)
end

function get_elem(x, i::Int, v::Val, ::Val{:descending})
    j = reversed_index(x, i, v)
    get_elem(x, j, v, Val(:ascending))
end

word_is_nonzero(x::RawBigInt, i::Int, v::Val) = !iszero(get_elem(x, i, Val(:words), v))

word_is_nonzero(x::RawBigInt, v::Val) = let x = x
    i -> word_is_nonzero(x, i, v)
end

"""
Returns a `Bool` indicating whether the `len` least significant words
of `x` are nonzero.
"""
function tail_is_nonzero(x::RawBigInt, len::Int, ::Val{:words})
    any(word_is_nonzero(x, Val(:ascending)), 0:(len - 1))
end

"""
Returns a `Bool` indicating whether the `len` least significant bits of
the `i`-th (zero-based index) word of `x` are nonzero.
"""
function tail_is_nonzero(x::RawBigInt, len::Int, i::Int, ::Val{:word})
    !iszero(len) &&
    !iszero(get_elem(x, i, Val(:words), Val(:ascending)) << (word_length(x) - len))
end

"""
Returns a `Bool` indicating whether the `len` least significant bits of
`x` are nonzero.
"""
function tail_is_nonzero(x::RawBigInt, len::Int, ::Val{:bits})
    if 0 < len
        word_count, bit_count_in_word = split_bit_index(x, len)
        tail_is_nonzero(x, bit_count_in_word, word_count, Val(:word)) ||
        tail_is_nonzero(x, word_count, Val(:words))
    else
        false
    end::Bool
end

"""
Returns a `Bool` that is the `i`-th (zero-based index) bit of `x`.
"""
function get_elem(x::Unsigned, i::Int, ::Val{:bits}, ::Val{:ascending})
    (x >>> i) % Bool
end

"""
Returns a `Bool` that is the `i`-th (zero-based index) bit of `x`.
"""
function get_elem(x::RawBigInt, i::Int, ::Val{:bits}, v::Val{:ascending})
    vb = Val(:bits)
    if 0 ≤ i < elem_count(x, vb)
        word_index, bit_index_in_word = split_bit_index(x, i)
        word = get_elem(x, word_index, Val(:words), v)
        get_elem(word, bit_index_in_word, vb, v)
    else
        false
    end::Bool
end

"""
Returns an integer of type `R`, consisting of the `len` most
significant bits of `x`. If there are less than `len` bits in `x`,
the least significant bits are zeroed.
"""
function truncated(::Type{R}, x::RawBigInt, len::Int) where {R<:Integer}
    ret = zero(R)
    if 0 < len
        word_count, bit_count_in_word = split_bit_index(x, len)
        k = word_length(x)
        vals = (Val(:words), Val(:descending))
        lenx = elem_count(x, first(vals))

        for w ∈ 0:(word_count - 1)
            ret <<= k
            if w < lenx
                word = get_elem(x, w, vals...)
                ret |= R(word)
            end
        end

        if !iszero(bit_count_in_word)
            ret <<= bit_count_in_word
            if word_count < lenx
                wrd = get_elem(x, word_count, vals...)
                ret |= R(wrd >>> (k - bit_count_in_word))
            end
        end
    end
    ret::R
end

struct RawBigIntRoundingIncrementHelper{T<:Unsigned}
    n::RawBigInt{T}
    trunc_len::Int

    final_bit::Bool
    round_bit::Bool

    function RawBigIntRoundingIncrementHelper{T}(n::RawBigInt{T}, len::Int) where {T<:Unsigned}
        vals = (Val(:bits), Val(:descending))
        f = get_elem(n, len - 1, vals...)
        r = get_elem(n, len    , vals...)
        new{T}(n, len, f, r)
    end
end

function RawBigIntRoundingIncrementHelper(n::RawBigInt{T}, len::Int) where {T<:Unsigned}
    RawBigIntRoundingIncrementHelper{T}(n, len)
end

(h::RawBigIntRoundingIncrementHelper)(::Rounding.FinalBit) = h.final_bit

(h::RawBigIntRoundingIncrementHelper)(::Rounding.RoundBit) = h.round_bit

function (h::RawBigIntRoundingIncrementHelper)(::Rounding.StickyBit)
    v = Val(:bits)
    n = h.n
    tail_is_nonzero(n, elem_count(n, v) - h.trunc_len - 1, v)
end
