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
    bitrand([rng=default_rng()], [dims...])

Generate a `BitArray` of random boolean values.

# Examples
```jldoctest
julia> bitrand(Xoshiro(123), 10)
10-element BitVector:
 0
 1
 0
 1
 0
 1
 0
 0
 1
 1
```
"""
bitrand(r::AbstractRNG, dims::Dims)   = rand!(r, BitArray(undef, dims))
bitrand(r::AbstractRNG, dims::Integer...) = rand!(r, BitArray(undef, convert(Dims, dims)))

bitrand(dims::Dims)   = rand!(BitArray(undef, dims))
bitrand(dims::Integer...) = rand!(BitArray(undef, convert(Dims, dims)))


## randstring (often useful for temporary filenames/dirnames)

"""
    randstring([rng=default_rng()], [chars], [len=8])

Create a random string of length `len`, consisting of characters from
`chars`, which defaults to the set of upper- and lower-case letters
and the digits 0-9. The optional `rng` argument specifies a random
number generator, see [Random Numbers](@ref).

# Examples
```jldoctest
julia> Random.seed!(3); randstring()
"Lxz5hUwn"

julia> randstring(Xoshiro(3), 'a':'z', 6)
"iyzcsm"

julia> randstring("ACGT")
"TGCTCCTC"
```

!!! note
    `chars` can be any collection of characters, of type `Char` or
    `UInt8` (more efficient), provided [`rand`](@ref) can randomly
    pick characters from it.
"""
function randstring end

let b = UInt8['0':'9';'A':'Z';'a':'z']
    global randstring

    function randstring(r::AbstractRNG, chars=b, n::Integer=8)
        T = eltype(chars)
        if T === UInt8
            str = Base._string_n(n)
            GC.@preserve str rand!(r, UnsafeView(pointer(str), n), chars)
            return str
        else
            v = Vector{T}(undef, n)
            rand!(r, v, chars)
            return String(v)
        end
    end

    randstring(r::AbstractRNG, n::Integer) = randstring(r, b, n)
    randstring(chars=b, n::Integer=8) = randstring(default_rng(), chars, n)
    randstring(n::Integer) = randstring(default_rng(), b, n)
end


## randsubseq & randsubseq!

# Fill S (resized as needed) with a random subsequence of A, where
# each element of A is included in S with independent probability p.
# (Note that this is different from the problem of finding a random
#  size-m subset of A where m is fixed!)
function randsubseq!(r::AbstractRNG, S::AbstractArray, A::AbstractArray, p::Real)
    require_one_based_indexing(S, A)
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
    randsubseq!([rng=default_rng(),] S, A, p)

Like [`randsubseq`](@ref), but the results are stored in `S`
(which is resized as needed).

# Examples
```jldoctest
julia> S = Int64[];

julia> randsubseq!(Xoshiro(123), S, 1:8, 0.3)
2-element Vector{Int64}:
 4
 7

julia> S
2-element Vector{Int64}:
 4
 7
```
"""
randsubseq!(S::AbstractArray, A::AbstractArray, p::Real) = randsubseq!(default_rng(), S, A, p)

randsubseq(r::AbstractRNG, A::AbstractArray{T}, p::Real) where {T} =
    randsubseq!(r, T[], A, p)

"""
    randsubseq([rng=default_rng(),] A, p) -> Vector

Return a vector consisting of a random subsequence of the given array `A`, where each
element of `A` is included (in order) with independent probability `p`. (Complexity is
linear in `p*length(A)`, so this function is efficient even if `p` is small and `A` is
large.) Technically, this process is known as "Bernoulli sampling" of `A`.

# Examples
```jldoctest
julia> randsubseq(Xoshiro(123), 1:8, 0.3)
2-element Vector{Int64}:
 4
 7
```
"""
randsubseq(A::AbstractArray, p::Real) = randsubseq(default_rng(), A, p)


## rand Less Than Masked 52 bits (helper function)

"Return a sampler generating a random `Int` (masked with `mask`) in ``[0, n)``, when `n <= 2^52`."
ltm52(n::Int, mask::Int=nextpow(2, n)-1) = LessThan(n-1, Masked(mask, UInt52Raw(Int)))

## shuffle & shuffle!

"""
    shuffle!([rng=default_rng(),] v::AbstractArray)

In-place version of [`shuffle`](@ref): randomly permute `v` in-place,
optionally supplying the random-number generator `rng`.

# Examples
```jldoctest
julia> shuffle!(Xoshiro(123), Vector(1:10))
10-element Vector{Int64}:
  5
  4
  2
  3
  6
 10
  8
  1
  9
  7
```
"""
function shuffle!(r::AbstractRNG, a::AbstractArray)
    # keep it consistent with `randperm!` and `randcycle!` if possible
    require_one_based_indexing(a)
    n = length(a)
    @assert n <= Int64(2)^52
    n == 0 && return a
    mask = 3
    @inbounds for i = 2:n
        j = 1 + rand(r, ltm52(i, mask))
        a[i], a[j] = a[j], a[i]
        i == 1 + mask && (mask = 2 * mask + 1)
    end
    return a
end

function shuffle!(r::AbstractRNG, a::AbstractArray{Bool})
    old_count = count(a)
    len = length(a)
    uncommon_value = 2old_count <= len
    fuel = uncommon_value ? old_count : len - old_count
    fuel == 0 && return a
    a .= !uncommon_value
    while fuel > 0
        k = rand(r, eachindex(a))
        fuel -= a[k] != uncommon_value
        a[k] = uncommon_value
    end
    a
end

shuffle!(a::AbstractArray) = shuffle!(default_rng(), a)

"""
    shuffle([rng=default_rng(),] v::AbstractArray)

Return a randomly permuted copy of `v`. The optional `rng` argument specifies a random
number generator (see [Random Numbers](@ref)).
To permute `v` in-place, see [`shuffle!`](@ref). To obtain randomly permuted
indices, see [`randperm`](@ref).

# Examples
```jldoctest
julia> shuffle(Xoshiro(123), Vector(1:10))
10-element Vector{Int64}:
  5
  4
  2
  3
  6
 10
  8
  1
  9
  7
```
"""
shuffle(r::AbstractRNG, a::AbstractArray) = shuffle!(r, copymutable(a))
shuffle(a::AbstractArray) = shuffle(default_rng(), a)

shuffle(r::AbstractRNG, a::Base.OneTo) = randperm(r, last(a))

## randperm & randperm!

"""
    randperm([rng=default_rng(),] n::Integer)

Construct a random permutation of length `n`. The optional `rng`
argument specifies a random number generator (see [Random
Numbers](@ref)). The element type of the result is the same as the type
of `n`.

To randomly permute an arbitrary vector, see [`shuffle`](@ref) or
[`shuffle!`](@ref).

!!! compat "Julia 1.1"
    In Julia 1.1 `randperm` returns a vector `v` with `eltype(v) == typeof(n)`
    while in Julia 1.0 `eltype(v) == Int`.

# Examples
```jldoctest
julia> randperm(Xoshiro(123), 4)
4-element Vector{Int64}:
 1
 4
 2
 3
```
"""
randperm(r::AbstractRNG, n::T) where {T <: Integer} = randperm!(r, Vector{T}(undef, n))
randperm(n::Integer) = randperm(default_rng(), n)

"""
    randperm!([rng=default_rng(),] A::Array{<:Integer})

Construct in `A` a random permutation of length `length(A)`. The
optional `rng` argument specifies a random number generator (see
[Random Numbers](@ref)). To randomly permute an arbitrary vector, see
[`shuffle`](@ref) or [`shuffle!`](@ref).

# Examples
```jldoctest
julia> randperm!(Xoshiro(123), Vector{Int}(undef, 4))
4-element Vector{Int64}:
 1
 4
 2
 3
```
"""
function randperm!(r::AbstractRNG, a::Array{<:Integer})
    # keep it consistent with `shuffle!` and `randcycle!` if possible
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
        i == 1 + mask && (mask = 2 * mask + 1)
    end
    return a
end

randperm!(a::Array{<:Integer}) = randperm!(default_rng(), a)


## randcycle & randcycle!

"""
    randcycle([rng=default_rng(),] n::Integer)

Construct a random cyclic permutation of length `n`. The optional `rng`
argument specifies a random number generator, see [Random Numbers](@ref).
The element type of the result is the same as the type of `n`.

Here, a "cyclic permutation" means that all of the elements lie within
a single cycle.  If `n > 0`, there are ``(n-1)!`` possible cyclic permutations,
which are sampled uniformly.  If `n == 0`, `randcycle` returns an empty vector.

[`randcycle!`](@ref) is an in-place variant of this function.

!!! compat "Julia 1.1"
    In Julia 1.1 and above, `randcycle` returns a vector `v` with
    `eltype(v) == typeof(n)` while in Julia 1.0 `eltype(v) == Int`.

# Examples
```jldoctest
julia> randcycle(Xoshiro(123), 6)
6-element Vector{Int64}:
 5
 4
 2
 6
 3
 1
```
"""
randcycle(r::AbstractRNG, n::T) where {T <: Integer} = randcycle!(r, Vector{T}(undef, n))
randcycle(n::Integer) = randcycle(default_rng(), n)

"""
    randcycle!([rng=default_rng(),] A::Array{<:Integer})

Construct in `A` a random cyclic permutation of length `n = length(A)`.
The optional `rng` argument specifies a random number generator, see
[Random Numbers](@ref).

Here, a "cyclic permutation" means that all of the elements lie within a single cycle.
If `A` is nonempty (`n > 0`), there are ``(n-1)!`` possible cyclic permutations,
which are sampled uniformly.  If `A` is empty, `randcycle!` leaves it unchanged.

[`randcycle`](@ref) is a variant of this function that allocates a new vector.

# Examples
```jldoctest
julia> randcycle!(Xoshiro(123), Vector{Int}(undef, 6))
6-element Vector{Int64}:
 5
 4
 2
 6
 3
 1
```
"""
function randcycle!(r::AbstractRNG, a::Array{<:Integer})
    # keep it consistent with `shuffle!` and `randperm!` if possible
    n = length(a)
    @assert n <= Int64(2)^52
    n == 0 && return a
    a[1] = 1
    mask = 3
    # Sattolo's algorithm:
    @inbounds for i = 2:n
        j = 1 + rand(r, ltm52(i-1, mask))
        a[i] = a[j]
        a[j] = i
        i == 1 + mask && (mask = 2 * mask + 1)
    end
    return a
end

randcycle!(a::Array{<:Integer}) = randcycle!(default_rng(), a)
