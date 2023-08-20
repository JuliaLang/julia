# This file is a part of Julia. License is MIT: https://julialang.org/license
#
# Parallel Partitioned Shuffle
#
using Base.Threads
export ppshuffle!, ppshuffle, pprandperm!, pprandperm

## ppshuffle! & ppshuffle

"""
    _ppshuffle!(rng::TaskLocalRNG, B::AbstractArray{T}, A::AbstractArray{T}, mask<:Union{UInt8, UInt16})

Parallel Partitioned Shuffle
1. partition input randomly
2. shuffle partitions concurrently

Arg `mask` determines number of partitions (mask + 1) to be used.
"""
function _ppshuffle!(r::TaskLocalRNG, B::AbstractArray{T}, A::AbstractArray{T}, mask::Tu) where {T, Tu<:Union{UInt8, UInt16}}
    # determine number of partitions
    nparts = mask + 1
    @assert ispow2(nparts) "invalid mask $(mask)"
    @assert length(A) == length(B)

    n = length(A)
    s = Random.SamplerType{Tu}()

    # an array to count partition hits by threads
    # partitions map to rows (pid)
    # threads map to cols (tid)
    # we add an extra cache line for each thread
    nrows = nparts < 8 ? 8 : nparts + 8
    hits = zeros(Int, nrows, nthreads())

    # save initial random state
    r0 = copy(r)
    # 1st pass
    # assign input to partitions uniformly at random
    # count cells hit by each thread in every partition
    @threads :static for i in 1:n
        local tid, pid = threadid(), rand(r, s) & mask + 1
        @inbounds hits[pid, tid] += 1
    end

    # cumsum partition hits
    # to mark boundaries of space reserved by each thread in every partition
    # note that the 1st column will contain boundaries of entire partitions
    prev = 0
    for pid = 1:nparts, tid = 1:nthreads()
        @inbounds prev = hits[pid, tid] += prev
    end
    # mark the end of the last partition
    hits[nparts + 1, 1] = n

    # recover random state
    copy!(r, r0)
    # 2nd pass
    # scatter input accross partitions uniformly at random
    # note that input distribution is identical as in the 1st pass
    # since we recovered the initial random state
    @threads :static for i in 1:n
        local tid, pid = threadid(), rand(r, s) & mask + 1
        @inbounds B[hits[pid, tid]] = A[i]
        @inbounds hits[pid, tid] -= 1
    end

    # input is partitioned
    # shuffle partitions in parallel
    @threads :static for pid in 1:nparts
        @inbounds local chunk = view(B, hits[pid, 1] + 1:hits[pid + 1, 1])
        shuffle!(r, chunk)
    end
    B
end

"""
    ppshuffle!([rng::TaskLocalRNG=default_rng(),] B::AbstractArray{T}, A::AbstractArray{T}, mask<:Union{UInt8, UInt16})

A multi-threaded implementation of [`shuffle!`](@ref).
Construct in `B` a permuted copy of `A`.
Optional arg `rng` specifies a random number generator (see [`TaskLocalRNG`](@ref)).

# Examples
```jldoctest
julia> b = Vector{Int}(undef, 16);

julia> ppshuffle!(b, 1:16);

julia> isperm(b)
true
```
"""
function ppshuffle!(r::TaskLocalRNG, B::AbstractArray{T}, A::AbstractArray{T}) where {T<:Integer}
    nparts = max(2, (length(A) * sizeof(T)) >> 21)
    nparts = nextpow(2, nparts)
    mask = nparts <= typemax(UInt8) + 1 ? UInt8(nparts - 1) : UInt16(nparts - 1)
    _ppshuffle!(r, B, A, mask)
end
ppshuffle!(B::AbstractArray{T}, A::AbstractArray{T}) where {T<:Integer} = ppshuffle!(default_rng(), B, A)


"""
    ppshuffle([rng=default_rng(),] A::AbstractArray)

A multi-threaded implementation of [`shuffle`](@ref).
Expected to run noticeably faster for `A` large.

Return a randomly permuted copy of `A`. The optional `rng` argument specifies a random
number generator (see [`TaskLocalRNG`](@ref)).
To permute `A` in-place, see [`ppshuffle!`](@ref). To obtain randomly permuted
indices, see [`pprandperm`](@ref).

# Examples
```jldoctest
julia> isperm(ppshuffle(Vector(1:16)))
true
```
"""
ppshuffle(r::TaskLocalRNG, A::AbstractArray{T}) where {T<:Integer} = ppshuffle!(r, similar(A), A)
ppshuffle(A::AbstractArray{T}) where {T<:Integer} = ppshuffle(default_rng(), A)


## pprandperm! & pprandperm

"""
    pprandperm([rng::TaskLocalRNG=default_rng(),] n::{T<:Integer})

A multi-threaded implementation of [`randperm`](@ref).
Expected to run noticeably faster for `n` large.

Construct a random permutation of length `n`. The optional `rng`
argument specifies a random number generator (see [`TaskLocalRNG`](@ref)).
The element type of the result is the same as the type of `n`.

# Examples
```jldoctest
julia> isperm(pprandperm(1024))
true
```
"""
pprandperm(r::TaskLocalRNG, n::T) where {T<:Integer} = ppshuffle(r, Base.OneTo(n))
pprandperm(n::T) where {T<:Integer} = ppshuffle(Base.OneTo(n))

"""
    pprandperm!([rng=default_rng(),] A::Array{<:Integer})

A multi-threaded implementation of [`randperm!`](@ref).
Expected to run noticeably faster for `A` large.

Construct in `A` a random permutation of length `length(A)`.
The optional `rng` argument specifies a random
number generator (see [`TaskLocalRNG`](@ref)).
To randomly permute an arbitrary vector, see
[`ppshuffle`](@ref) or [`ppshuffle!`](@ref).

# Examples
```jldoctest
julia> A = Vector{Int}(undef, 1024);

julia> pprandperm!(A);

julia> isperm(A)
true
```
"""
pprandperm!(r::TaskLocalRNG, A::AbstractArray{T}) where {T<:Integer} = ppshuffle!(r, A, Base.OneTo(length(A)%eltype(A)))
pprandperm!(A::AbstractArray{T}) where {T<:Integer} = pprandperm!(default_rng(), A)
