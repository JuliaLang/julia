# This file is a part of Julia. License is MIT: https://julialang.org/license

module Random

using Base.dSFMT
using Base.GMP: Limb, MPZ
using Base: BitInteger, BitInteger_types, BitUnsigned, @gc_preserve

import Base: copymutable, copy, copy!, ==, hash

export srand,
       rand, rand!,
       randn, randn!,
       randexp, randexp!,
       bitrand,
       randstring,
       randsubseq, randsubseq!,
       shuffle, shuffle!,
       randperm, randperm!,
       randcycle, randcycle!,
       AbstractRNG, MersenneTwister, RandomDevice,
       GLOBAL_RNG, randjump,
       Distribution, Combine, Uniform, Sampler


## __init__ & include

function __init__()
    try
        srand()
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module Random")
    end
end

include("definitions.jl")
include("containers.jl")
include("RNGs.jl")
include("generation.jl")
include("normal.jl")
include("misc.jl")


## rand & rand! & srand docstrings

"""
    rand([rng=GLOBAL_RNG], [S], [C...])

Pick a random element or collection of random elements from the set of values specified by `S`;
`S` can be

* an indexable collection (for example `1:n` or `['x','y','z']`),
* an `AbstractDict` or `AbstractSet` object,
* a string (considered as a collection of characters), or
* a type: the set of values to pick from is then equivalent to `typemin(S):typemax(S)` for
  integers (this is not applicable to [`BigInt`](@ref)), and to ``[0, 1)`` for floating
  point numbers;
* a `Distribution` object, e.g. `Normal()` for a normal distribution (like `randn()`),
  or `CloseOpen(10.0, 20.0)` for uniform `Float64` numbers in the range ``[10.0, 20.0)``;
* a `Combine` object, which can be either `Combine(Pair, S1, S2)` or `Combine(Complex, S1, S2)`,
  where `S1` and `S2` are one of the specifications above; `Pair` or `Complex` can optionally be
  given as concrete types, e.g. `Combine(ComplexF64, 1:3, Int)` to generate `ComplexF64` instead
  of `Complex{Int}`.

`S` usually defaults to [`Float64`](@ref).

If `C...` is not specified, `rand` produces a scalar. Otherwise, `C...` can be:

* a set of integers, or a tuple of `Int`, which specify the dimensions of an `Array` to generate;
* `(p::AbstractFloat, m::Integer, [n::Integer])`, which produces a sparse array of dimensions `(m, n)`,
  in which the probability of any element being nonzero is independently given by `p`
* `(String, [n=8])`, which produces a random `String` of length `n`; the generated string consists of `Char`
  taken from a predefined set like `randstring`, and can be specified with the `S` parameter.
* `(Dict, n)`, which produces a `Dict` of length `n`; `S` must then specify the type of its elements,
  e.g. `Combine(Pair, Int, 2:3)`;
* `(Set, n)`, which produces a `Set` of length `n`;
* `(BitArray, dims...)`, which produces a `BitArray` with the specified dimensions.

# Examples
```julia-repl
julia> rand(Int, 2)
2-element Array{Int64,1}:
 1339893410598768192
 1575814717733606317

julia> rand(MersenneTwister(0), Dict(1=>2, 3=>4))
1=>2

julia> rand("abc", String, 12)
"bccaacaabaac"

julia> rand(1:10, Set, 3)
Set([3, 8, 6])
```

!!! note
    The complexity of `rand(rng, s::Union{AbstractDict,AbstractSet})`
    is linear in the length of `s`, unless an optimized method with
    constant complexity is available, which is the case for `Dict`,
    `Set` and `BitSet`. For more than a few calls, use `rand(rng,
    collect(s))` instead, or either `rand(rng, Dict(s))` or `rand(rng,
    Set(s))` as appropriate.
"""
rand

"""
    rand!([rng=GLOBAL_RNG], A, [S=eltype(A)])

Populate the array `A` with random values. If `S` is specified
(`S` can be a type or a collection, cf. [`rand`](@ref) for details),
the values are picked randomly from `S`.
This is equivalent to `copyto!(A, rand(rng, S, size(A)))`
but without allocating a new array.

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> rand!(rng, zeros(5))
5-element Array{Float64,1}:
 0.5908446386657102
 0.7667970365022592
 0.5662374165061859
 0.4600853424625171
 0.7940257103317943
```
"""
rand!

"""
    srand([rng=GLOBAL_RNG], seed) -> rng
    srand([rng=GLOBAL_RNG]) -> rng

Reseed the random number generator: `rng` will give a reproducible
sequence of numbers if and only if a `seed` is provided. Some RNGs
don't accept a seed, like `RandomDevice`.
After the call to `srand`, `rng` is equivalent to a newly created
object initialized with the same seed.

# Examples
```julia-repl
julia> srand(1234);

julia> x1 = rand(2)
2-element Array{Float64,1}:
 0.590845
 0.766797

julia> srand(1234);

julia> x2 = rand(2)
2-element Array{Float64,1}:
 0.590845
 0.766797

julia> x1 == x2
true

julia> rng = MersenneTwister(1234); rand(rng, 2) == x1
true

julia> MersenneTwister(1) == srand(rng, 1)
true

julia> rand(srand(rng), Bool) # not reproducible
true

julia> rand(srand(rng), Bool)
false

julia> rand(MersenneTwister(), Bool) # not reproducible either
true
```
"""
srand(rng::AbstractRNG, ::Nothing) = srand(rng)

end # module
