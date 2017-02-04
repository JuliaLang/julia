# This file is a part of Julia. License is MIT: http://julialang.org/license

# Factorials

const _fact_table64 =
    Int64[1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,6227020800,
          87178291200,1307674368000,20922789888000,355687428096000,6402373705728000,
          121645100408832000,2432902008176640000]

const _fact_table128 =
    UInt128[0x00000000000000000000000000000001, 0x00000000000000000000000000000002,
            0x00000000000000000000000000000006, 0x00000000000000000000000000000018,
            0x00000000000000000000000000000078, 0x000000000000000000000000000002d0,
            0x000000000000000000000000000013b0, 0x00000000000000000000000000009d80,
            0x00000000000000000000000000058980, 0x00000000000000000000000000375f00,
            0x00000000000000000000000002611500, 0x0000000000000000000000001c8cfc00,
            0x0000000000000000000000017328cc00, 0x0000000000000000000000144c3b2800,
            0x00000000000000000000013077775800, 0x00000000000000000000130777758000,
            0x00000000000000000001437eeecd8000, 0x00000000000000000016beecca730000,
            0x000000000000000001b02b9306890000, 0x000000000000000021c3677c82b40000,
            0x0000000000000002c5077d36b8c40000, 0x000000000000003ceea4c2b3e0d80000,
            0x000000000000057970cd7e2933680000, 0x00000000000083629343d3dcd1c00000,
            0x00000000000cd4a0619fb0907bc00000, 0x00000000014d9849ea37eeac91800000,
            0x00000000232f0fcbb3e62c3358800000, 0x00000003d925ba47ad2cd59dae000000,
            0x0000006f99461a1e9e1432dcb6000000, 0x00000d13f6370f96865df5dd54000000,
            0x0001956ad0aae33a4560c5cd2c000000, 0x0032ad5a155c6748ac18b9a580000000,
            0x0688589cc0e9505e2f2fee5580000000, 0xde1bc4d19efcac82445da75b00000000]

function factorial_lookup(n::Integer, table, lim)
    n < 0 && throw(DomainError())
    n > lim && throw(OverflowError())
    n == 0 && return one(n)
    @inbounds f = table[n]
    return oftype(n, f)
end

factorial(n::Int128) = factorial_lookup(n, _fact_table128, 33)
factorial(n::UInt128) = factorial_lookup(n, _fact_table128, 34)
factorial(n::Union{Int64,UInt64}) = factorial_lookup(n, _fact_table64, 20)

if Int === Int32
    factorial(n::Union{Int8,UInt8,Int16,UInt16}) = factorial(Int32(n))
    factorial(n::Union{Int32,UInt32}) = factorial_lookup(n, _fact_table64, 12)
else
    factorial(n::Union{Int8,UInt8,Int16,UInt16,Int32,UInt32}) = factorial(Int64(n))
end

function gamma(n::Union{Int8,UInt8,Int16,UInt16,Int32,UInt32,Int64,UInt64})
    n < 0 && throw(DomainError())
    n == 0 && return Inf
    n <= 2 && return 1.0
    n > 20 && return gamma(Float64(n))
    @inbounds return Float64(_fact_table64[n-1])
end


# Basic functions for working with permutations

"""
    isperm(v) -> Bool

Returns `true` if `v` is a valid permutation.

```jldoctest
julia> isperm([1; 2])
true

julia> isperm([1; 3])
false
```
"""
function isperm(A)
    n = length(A)
    used = falses(n)
    for a in A
        (0 < a <= n) && (used[a] ⊻= true) || return false
    end
    true
end

isperm(p::Tuple{}) = true
isperm(p::Tuple{Int}) = p[1] == 1
isperm(p::Tuple{Int,Int}) = ((p[1] == 1) & (p[2] == 2)) | ((p[1] == 2) & (p[2] == 1))

function permute!!{T<:Integer}(a, p::AbstractVector{T})
    count = 0
    start = 0
    while count < length(a)
        ptr = start = findnext(p, start+1)
        temp = a[start]
        next = p[start]
        count += 1
        while next != start
            a[ptr] = a[next]
            p[ptr] = 0
            ptr = next
            next = p[next]
            count += 1
        end
        a[ptr] = temp
        p[ptr] = 0
    end
    a
end

"""
    permute!(v, p)

Permute vector `v` in-place, according to permutation `p`. No checking is done
to verify that `p` is a permutation.

To return a new permutation, use `v[p]`. Note that this is generally faster than
`permute!(v,p)` for large vectors.

See also [`ipermute!`](@ref)

```jldoctest
julia> A = [1, 1, 3, 4];

julia> perm = [2, 4, 3, 1];

julia> permute!(A, perm);

julia> A
4-element Array{Int64,1}:
 1
 4
 3
 1
```
"""
permute!(a, p::AbstractVector) = permute!!(a, copymutable(p))

function ipermute!!{T<:Integer}(a, p::AbstractVector{T})
    count = 0
    start = 0
    while count < length(a)
        start = findnext(p, start+1)
        temp = a[start]
        next = p[start]
        count += 1
        while next != start
            temp_next = a[next]
            a[next] = temp
            temp = temp_next
            ptr = p[next]
            p[next] = 0
            next = ptr
            count += 1
        end
        a[next] = temp
        p[next] = 0
    end
    a
end

"""
    ipermute!(v, p)

Like `permute!`, but the inverse of the given permutation is applied.

```jldoctest
julia> A = [1, 1, 3, 4];

julia> perm = [2, 4, 3, 1];

julia> ipermute!(A, perm);

julia> A
4-element Array{Int64,1}:
 4
 1
 3
 1
```
"""
ipermute!(a, p::AbstractVector) = ipermute!!(a, copymutable(p))

"""
    invperm(v)

Return the inverse permutation of `v`.
If `B = A[v]`, then `A == B[invperm(v)]`.

```jldoctest
julia> v = [2; 4; 3; 1];

julia> invperm(v)
4-element Array{Int64,1}:
 4
 1
 3
 2

julia> A = ['a','b','c','d'];

julia> B = A[v]
4-element Array{Char,1}:
 'b'
 'd'
 'c'
 'a'

julia> B[invperm(v)]
4-element Array{Char,1}:
 'a'
 'b'
 'c'
 'd'
```
"""
function invperm(a::AbstractVector)
    b = zero(a) # similar vector of zeros
    n = length(a)
    @inbounds for (i, j) in enumerate(a)
        ((1 <= j <= n) && b[j] == 0) ||
            throw(ArgumentError("argument is not a permutation"))
        b[j] = i
    end
    b
end

function invperm(p::Union{Tuple{},Tuple{Int},Tuple{Int,Int}})
    isperm(p) || throw(ArgumentError("argument is not a permutation"))
    p  # in dimensions 0-2, every permutation is its own inverse
end
invperm(a::Tuple) = (invperm([a...])...,)

#XXX This function should be moved to Combinatorics.jl but is currently used by Base.DSP.
"""
    nextprod([k_1, k_2,...], n)

Next integer greater than or equal to `n` that can be written as ``\\prod k_i^{p_i}`` for integers
``p_1``, ``p_2``, etc.

```jldoctest
julia> nextprod([2, 3], 105)
108

julia> 2^2 * 3^3
108
```
"""
function nextprod(a::Vector{Int}, x)
    if x > typemax(Int)
        throw(ArgumentError("unsafe for x > typemax(Int), got $x"))
    end
    k = length(a)
    v = ones(Int, k)                  # current value of each counter
    mx = [nextpow(ai,x) for ai in a]  # maximum value of each counter
    v[1] = mx[1]                      # start at first case that is >= x
    p::widen(Int) = mx[1]             # initial value of product in this case
    best = p
    icarry = 1

    while v[end] < mx[end]
        if p >= x
            best = p < best ? p : best  # keep the best found yet
            carrytest = true
            while carrytest
                p = div(p, v[icarry])
                v[icarry] = 1
                icarry += 1
                p *= a[icarry]
                v[icarry] *= a[icarry]
                carrytest = v[icarry] > mx[icarry] && icarry < k
            end
            if p < x
                icarry = 1
            end
        else
            while p < x
                p *= a[1]
                v[1] *= a[1]
            end
        end
    end
    # might overflow, but want predictable return type
    return mx[end] < best ? Int(mx[end]) : Int(best)
end
