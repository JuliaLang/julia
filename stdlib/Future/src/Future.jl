# This file is a part of Julia. License is MIT: https://julialang.org/license

"The `Future` module implements future behavior of already existing functions,
which will replace the current version in a future release of Julia."
module Future

using Random

## copy!

# This has now been moved to Base (#29178), and should be deprecated in the
# next "deprecation phase".

"""
    Future.copy!(dst, src) -> dst

Copy `src` into `dst`.

!!! compat "Julia 1.1"
    This function has moved to `Base` with Julia 1.1, consider using `copy!(dst, src)` instead.
    `Future.copy!` will be deprecated in the future.
"""
copy!(dst::AbstractSet, src::AbstractSet) = Base.copy!(dst, src)
copy!(dst::AbstractDict, src::AbstractDict) = Base.copy!(dst, src)
copy!(dst::AbstractArray, src::AbstractArray) = Base.copy!(dst, src)


## randjump

"""
    randjump(r::MersenneTwister, steps::Integer) -> MersenneTwister

Create an initialized `MersenneTwister` object, whose state is moved forward
(without generating numbers) from `r` by `steps` steps.
One such step corresponds to the generation of two `Float64` numbers.
For each different value of `steps`, a large polynomial has to be generated internally.
One is already pre-computed for `steps=big(10)^20`.
"""
randjump(r::MersenneTwister, steps::Integer) =
    Random._randjump(r, Random.DSFMT.calc_jump(steps))

end # module Future
