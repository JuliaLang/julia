# This file is a part of Julia. License is MIT: https://julialang.org/license

"The `Future` module implements future behavior of already existing functions,
which will replace the current version in a future release of Julia."
module Future

using Random

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
