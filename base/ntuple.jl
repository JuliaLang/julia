# This file is a part of Julia. License is MIT: https://julialang.org/license

# `ntuple`, for constructing tuples of a given length

"""
    ntuple(f, n::Integer)

Create a tuple of length `n`, computing each element as `f(i)`,
where `i` is the index of the element.

# Examples
```jldoctest
julia> ntuple(i -> 2*i, 4)
(2, 4, 6, 8)
```
"""
@inline function ntuple(f::F, n::Int) where F
    # marked inline since this benefits from constant propagation of `n`
    t = n == 0  ? () :
        n == 1  ? (f(1),) :
        n == 2  ? (f(1), f(2)) :
        n == 3  ? (f(1), f(2), f(3)) :
        n == 4  ? (f(1), f(2), f(3), f(4)) :
        n == 5  ? (f(1), f(2), f(3), f(4), f(5)) :
        n == 6  ? (f(1), f(2), f(3), f(4), f(5), f(6)) :
        n == 7  ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7)) :
        n == 8  ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8)) :
        n == 9  ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9)) :
        n == 10 ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10)) :
        _ntuple(f, n)
    return t
end
ntuple(f::F, n::Integer) where F = ntuple(f, convert(Int, n)::Int)

# `n` should always be an Int (#55790)
function _ntuple(f::F, n::Int) where F
    @noinline
    (n >= 0) || throw(ArgumentError(LazyString("tuple length should be ≥ 0, got ", n)))
    ([f(i) for i = 1:n]...,)
end

function ntupleany(f, n)
    @noinline
    (n >= 0) || throw(ArgumentError(LazyString("tuple length should be ≥ 0, got ", n)))
    (Any[f(i) for i = 1:n]...,)
end

# inferable ntuple (enough for bootstrapping)
ntuple(f, ::Val{0}) = ()
ntuple(f, ::Val{1}) = (@inline; (f(1),))
ntuple(f, ::Val{2}) = (@inline; (f(1), f(2)))
ntuple(f, ::Val{3}) = (@inline; (f(1), f(2), f(3)))

"""
    ntuple(f, ::Val{N})

Create a tuple of length `N`, computing each element as `f(i)`,
where `i` is the index of the element. By taking a `Val(N)`
argument, it is possible that this version of ntuple may
generate more efficient code than the version taking the
length as an integer. But `ntuple(f, N)` is preferable to
`ntuple(f, Val(N))` in cases where `N` cannot be determined
at compile time.

# Examples
```jldoctest
julia> ntuple(i -> 2*i, Val(4))
(2, 4, 6, 8)
```
"""
@inline function ntuple(f::F, ::Val{N}) where {F,N}
    N::Int
    (N >= 0) || throw(ArgumentError(LazyString("tuple length should be ≥ 0, got ", N)))
    if @generated
        :(@ntuple $N i -> f(i))
    else
        Tuple(f(i) for i = 1:(N::Int))
    end
end
typeof(function ntuple end).name.max_methods = UInt8(5)

@inline function fill_to_length(t::Tuple, val, ::Val{_N}) where {_N}
    M = length(t)
    N = _N::Int
    M > N && throw(ArgumentError(LazyString("input tuple of length ", M, ", requested ", N)))
    if @generated
        quote
            (t..., $(fill(:val, (_N::Int) - length(t.parameters))...))
        end
    else
        (t..., fill(val, N-M)...)
    end
end


# Specialized extensions for NTuple
function reverse(t::NTuple{N}) where N
    ntuple(Val{N}()) do i
        t[end+1-i]
    end
end
