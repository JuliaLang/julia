
f = float16(2.)
g = float16(1.)

@test -f === float16(-2.)

@test f+g === float16(3f0)
@test f-g === float16(1f0)
@test f*g === float16(2f0)
@test f/g === float16(2f0)

@test f + 2 === float32(4f0)
@test f - 2 === float32(0f0)
@test f*2 === float32(4f0)
@test f/2 === float32(1f0)
@test f + 2. === 4.
@test f - 2. === 0.
@test f*2. === 4.
@test f/2. === 1.

@test_approx_eq sin(f) sin(2f0)

@test isnan(NaN16)
@test isnan(-NaN16)
@test !isnan(Inf16)
@test !isnan(-Inf16)
@test !isnan(float16(2.6))
@test NaN16 != NaN16
@test isequal(NaN16, NaN16)
@test repr(NaN16) == "NaN16"
@test sprint(showcompact, NaN16) == "NaN"

@test isinf(Inf16)
@test isinf(-Inf16)
@test !isinf(NaN16)
@test !isinf(-NaN16)
@test !isinf(float16(2.6))
@test Inf16 == Inf16
@test Inf16 != -Inf16
@test -Inf16 < Inf16
@test isequal(Inf16, Inf16)
@test repr(Inf16) == "Inf16"
@test sprint(showcompact, Inf16) == "Inf"

@test float16(0.0) == float16(0.0)
@test float16(-0.0) == float16(0.0)
@test float16(0.0) == float16(-0.0)
@test float16(-0.0) == float16(-0.0)
@test float16(2.5) == float16(2.5)
@test float16(2.5) != float16(2.6)
@test isequal(float16(0.0), float16(0.0))
@test !isequal(float16(-0.0), float16(0.0))
@test !isequal(float16(0.0), float16(-0.0))
