# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"The `Future` module implements future behavior of already existing functions,
which will replace the current version in a future release of Julia."
module Future

using Random

## copy!

"""
    Future.copy!(dst, src) -> dst

Copy `src` into `dst`.
For collections of the same type, copy the elements of `src` into `dst`,
discarding any pre-existing elements in `dst`.
Usually, `dst == src` holds after the call.
"""
copy!(dst::AbstractSet, src::AbstractSet) = union!(empty!(dst), src)
copy!(dst::AbstractDict, src::AbstractDict) = merge!(empty!(dst), src)
copy!(dst::AbstractVector, src::AbstractVector) = append!(empty!(dst), src)

function copy!(dst::AbstractArray, src::AbstractArray)
    size(dst) == size(src) || throw(ArgumentError(
        "arrays must have the same size for copy! (consider using `copyto!`)"))
    copyto!(dst, src)
end


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
