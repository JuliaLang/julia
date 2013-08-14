
f = float16(2.)
g = float16(1.)

@test -f === float16(-2.)

@test f+g === 3f0
@test f-g === 1f0
@test f*g === 2f0
@test f/g === 2f0

@test f + 2 === 4f0
@test f - 2 === 0f0
@test f*2 === 4f0
@test f/2 === 1f0
@test f + 2. === 4.
@test f - 2. === 0.
@test f*2. === 4.
@test f/2. === 1.

@test_approx_eq sin(f) sin(2f0)

@test isnan(NaN16)
@test NaN16 != NaN16
@test isequal(NaN16, NaN16)
@test sprint(showcompact, NaN16) == "NaN16"
@test sprint(showcompact, Inf16) == "Inf16"
