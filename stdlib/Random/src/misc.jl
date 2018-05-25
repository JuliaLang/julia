# This file is a part of Julia. License is MIT: https://julialang.org/license

## rand!(::BitArray) && bitrand

function rand!(rng::AbstractRNG, B::BitArray, ::SamplerType{Bool})
    isempty(B) && return B
    Bc = B.chunks
    rand!(rng, Bc)
    Bc[end] &= Base._msk_end(B)
    return B
end

"""
    bitrand([rng=GLOBAL_RNG], [dims...])

Generate a `BitArray` of random boolean values.

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> bitrand(rng, 10)
10-element BitArray{1}:
 false
  true
  true
  true
  true
 false
  true
 false
 false
  true
```
"""
bitrand(r::AbstractRNG, dims::Dims)   = rand!(r, BitArray(undef, dims))
bitrand(r::AbstractRNG, dims::Integer...) = rand!(r, BitArray(undef, convert(Dims, dims)))

bitrand(dims::Dims)   = rand!(BitArray(undef, dims))
bitrand(dims::Integer...) = rand!(BitArray(undef, convert(Dims, dims)))


## randstring (often useful for temporary filenames/dirnames)

"""
    randstring([rng=GLOBAL_RNG], [chars], [len=8])

Create a random string of length `len`, consisting of characters from
`chars`, which defaults to the set of upper- and lower-case letters
and the digits 0-9. The optional `rng` argument specifies a random
number generator, see [Random Numbers](@ref).

# Examples
```jldoctest
julia> srand(0); randstring()
"Qgt7sUOP"

julia> randstring(MersenneTwister(0), 'a':'z', 6)
"oevnou"

julia> randstring("ACGT")
"TATCGGTC"
```

!!! note
    `chars` can be any collection of characters, of type `Char` or
    `UInt8` (more efficient), provided [`rand`](@ref) can randomly
    pick characters from it.
"""
function randstring end

let b = UInt8['0':'9';'A':'Z';'a':'z']
    global randstring
    randstring(r::AbstractRNG, chars=b, n::Integer=8) = String(rand(r, chars, n))
    randstring(r::AbstractRNG, n::Integer) = randstring(r, b, n)
    randstring(chars=b, n::Integer=8) = randstring(GLOBAL_RNG, chars, n)
    randstring(n::Integer) = randstring(GLOBAL_RNG, b, n)
end


## randsubseq & randsubseq!

# Fill S (resized as needed) with a random subsequence of A, where
# each element of A is included in S with independent probability p.
# (Note that this is different from the problem of finding a random
#  size-m subset of A where m is fixed!)
function randsubseq!(r::AbstractRNG, S::AbstractArray, A::AbstractArray, p::Real)
    0 <= p <= 1 || throw(ArgumentError("probability $p not in [0,1]"))
    n = length(A)
    p == 1 && return copyto!(resize!(S, n), A)
    empty!(S)
    p == 0 && return S
    nexpected = p * length(A)
    sizehint!(S, round(Int,nexpected + 5*sqrt(nexpected)))
    if p > 0.15 # empirical threshold for trivial O(n) algorithm to be better
        for i = 1:n
            rand(r) <= p && push!(S, A[i])
        end
    else
        # Skip through A, in order, from each element i to the next element i+s
        # included in S. The probability that the next included element is
        # s==k (k > 0) is (1-p)^(k-1) * p, and hence the probability (CDF) that
        # s is in {1,...,k} is 1-(1-p)^k = F(k).   Thus, we can draw the skip s
        # from this probability distribution via the discrete inverse-transform
        # method: s = ceil(F^{-1}(u)) where u = rand(), which is simply
        # s = ceil(log(rand()) / log1p(-p)).
        # -log(rand()) is an exponential variate, so can use randexp().
        L = -1 / log1p(-p) # L > 0
        i = 0
        while true
            s = randexp(r) * L
            s >= n - i && return S # compare before ceil to avoid overflow
            push!(S, A[i += ceil(Int,s)])
        end
        # [This algorithm is similar in spirit to, but much simpler than,
        #  the one by Vitter for a related problem in "Faster methods for
        #  random sampling," Comm. ACM Magazine 7, 703-718 (1984).]
    end
    return S
end

"""
    randsubseq!(S, A, p)

Like [`randsubseq`](@ref), but the results are stored in `S`
(which is resized as needed).
"""
randsubseq!(S::AbstractArray, A::AbstractArray, p::Real) = randsubseq!(GLOBAL_RNG, S, A, p)

randsubseq(r::AbstractRNG, A::AbstractArray{T}, p::Real) where {T} =
    randsubseq!(r, T[], A, p)

"""
    randsubseq(A, p) -> Vector

Return a vector consisting of a random subsequence of the given array `A`, where each
element of `A` is included (in order) with independent probability `p`. (Complexity is
linear in `p*length(A)`, so this function is efficient even if `p` is small and `A` is
large.) Technically, this process is known as "Bernoulli sampling" of `A`.
"""
randsubseq(A::AbstractArray, p::Real) = randsubseq(GLOBAL_RNG, A, p)


## rand Less Than Masked 52 bits (helper function)

"Return a sampler generating a random `Int` (masked with `mask`) in ``[0, n)``, when `n <= 2^52`."
ltm52(n::Int, mask::Int=nextpow2(n)-1) = LessThan(n-1, Masked(mask, UInt52Raw(Int)))

## shuffle & shuffle!

"""
    shuffle!([rng=GLOBAL_RNG,] v::AbstractArray)

In-place version of [`shuffle`](@ref): randomly permute `v` in-place,
optionally supplying the random-number generator `rng`.

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> shuffle!(rng, Vector(1:16))
16-element Array{Int64,1}:
  2
 15
  5
 14
  1
  9
 10
  6
 11
  3
 16
  7
  4
 12
  8
 13
```
"""
function shuffle!(r::AbstractRNG, a::AbstractArray)
    n = length(a)
    @assert n <= Int64(2)^52
    mask = nextpow2(n) - 1
    for i = n:-1:2
        (mask >> 1) == i && (mask >>= 1)
        j = 1 + rand(r, ltm52(i, mask))
        a[i], a[j] = a[j], a[i]
    end
    return a
end

shuffle!(a::AbstractArray) = shuffle!(GLOBAL_RNG, a)

"""
    shuffle([rng=GLOBAL_RNG,] v::AbstractArray)

Return a randomly permuted copy of `v`. The optional `rng` argument specifies a random
number generator (see [Random Numbers](@ref)).
To permute `v` in-place, see [`shuffle!`](@ref). To obtain randomly permuted
indices, see [`randperm`](@ref).

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> shuffle(rng, Vector(1:10))
10-element Array{Int64,1}:
  6
  1
 10
  2
  3
  9
  5
  7
  4
  8
```
"""
shuffle(r::AbstractRNG, a::AbstractArray) = shuffle!(r, copymutable(a))
shuffle(a::AbstractArray) = shuffle(GLOBAL_RNG, a)


## randperm & randperm!

"""
    randperm([rng=GLOBAL_RNG,] n::Integer)

Construct a random permutation of length `n`. The optional `rng`
argument specifies a random number generator (see [Random Numbers](@ref)).
To randomly permute an arbitrary vector, see [`shuffle`](@ref)
or [`shuffle!`](@ref).

# Examples
```jldoctest
julia> randperm(MersenneTwister(1234), 4)
4-element Array{Int64,1}:
 2
 1
 4
 3
```
"""
randperm(r::AbstractRNG, n::Integer) = randperm!(r, Vector{Int}(undef, n))
randperm(n::Integer) = randperm(GLOBAL_RNG, n)

"""
    randperm!([rng=GLOBAL_RNG,] A::Array{<:Integer})

Construct in `A` a random permutation of length `length(A)`. The
optional `rng` argument specifies a random number generator (see
[Random Numbers](@ref)). To randomly permute an arbitrary vector, see
[`shuffle`](@ref) or [`shuffle!`](@ref).

# Examples
```jldoctest
julia> randperm!(MersenneTwister(1234), Vector{Int}(undef, 4))
4-element Array{Int64,1}:
 2
 1
 4
 3
```
"""
function randperm!(r::AbstractRNG, a::Array{<:Integer})
    n = length(a)
    @assert n <= Int64(2)^52
    n == 0 && return a
    a[1] = 1
    mask = 3
    @inbounds for i = 2:n
        j = 1 + rand(r, ltm52(i, mask))
        if i != j # a[i] is undef (and could be #undef)
            a[i] = a[j]
        end
        a[j] = i
        i == 1+mask && (mask = 2mask + 1)
    end
    return a
end

randperm!(a::Array{<:Integer}) = randperm!(GLOBAL_RNG, a)


## randcycle & randcycle!

"""
    randcycle([rng=GLOBAL_RNG,] n::Integer)

Construct a random cyclic permutation of length `n`. The optional `rng`
argument specifies a random number generator, see [Random Numbers](@ref).

# Examples
```jldoctest
julia> randcycle(MersenneTwister(1234), 6)
6-element Array{Int64,1}:
 3
 5
 4
 6
 1
 2
```
"""
randcycle(r::AbstractRNG, n::Integer) = randcycle!(r, Vector{Int}(undef, n))
randcycle(n::Integer) = randcycle(GLOBAL_RNG, n)

"""
    randcycle!([rng=GLOBAL_RNG,] A::Array{<:Integer})

Construct in `A` a random cyclic permutation of length `length(A)`.
The optional `rng` argument specifies a random number generator, see
[Random Numbers](@ref).

# Examples
```jldoctest
julia> randcycle!(MersenneTwister(1234), Vector{Int}(undef, 6))
6-element Array{Int64,1}:
 3
 5
 4
 6
 1
 2
```
"""
function randcycle!(r::AbstractRNG, a::Array{<:Integer})
    n = length(a)
    n == 0 && return a
    @assert n <= Int64(2)^52
    a[1] = 1
    mask = 3
    @inbounds for i = 2:n
        j = 1 + rand(r, ltm52(i-1, mask))
        a[i] = a[j]
        a[j] = i
        i == 1+mask && (mask = 2mask + 1)
    end
    return a
end

randcycle!(a::Array{<:Integer}) = randcycle!(GLOBAL_RNG, a)
