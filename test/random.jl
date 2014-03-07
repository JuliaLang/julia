@test rand() != rand()
@test 0.0 <= rand() < 1.0
@test rand(Uint32) >= 0
@test -10 <= rand(-10:-5) <= -5
@test -10 <= rand(-10:5) <= 5
@test minimum([rand(int32(1):int32(7^7)) for i = 1:100000]) > 0
@test(typeof(rand(false:true)) == Bool)

# Test rand(::AbstractRNG, ::Type{...})
@test rand(MersenneTwister(), Uint32) == 0x02631e40
@test rand(MersenneTwister(42), Float16) == float16(0.5332)
@test rand(MersenneTwister(42), Float32) == 0.53318304f0
@test rand(MersenneTwister(42), Float64) == 0.5331830160438613
@test rand(MersenneTwister(42), Uint8) == 0x73
@test rand(MersenneTwister(42), Uint16) == 0x0e73
@test rand(MersenneTwister(42), Uint32) == 0xea0b0e73
@test rand(MersenneTwister(42), Uint64) == 0x0e0c7263ea0b0e73
@test rand(MersenneTwister(42), Uint128) == 0x0e0c7263ea0b0e736c7aeb39fb64f900
@test rand(MersenneTwister(42), Int8) == 115
@test rand(MersenneTwister(42), Int16) == 3699
@test rand(MersenneTwister(42), Int32) == -368374157
@test rand(MersenneTwister(42), Int64) == 1012309789705440883
@test rand(MersenneTwister(42), Int128) == 18673819614007004079327070939043395840
@test rand(MersenneTwister(42), Complex{Int8}) == 115 + 99im
@test rand(MersenneTwister(42), Complex{Int64}) == 1012309789705440883 + 7816818737518278912im

@test length(randn(4, 5)) == 20
@test length(randbool(4, 5)) == 20

@test rand(MersenneTwister()) == 0.8236475079774124
@test rand(MersenneTwister(0)) == 0.8236475079774124
@test rand(MersenneTwister(42)) == 0.5331830160438613
# Try a seed larger than 2^32
@test rand(MersenneTwister(5294967296)) == 0.3498809918210497

for T in (Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Int128, Uint128, Char, BigInt,
	Float16, Float32, Float64, Rational{Int})
    r = rand(convert(T, 97):convert(T, 122))
    @test typeof(r) == T
    @test 97 <= r <= 122
    r = rand(convert(T, 97):convert(T,2):convert(T, 122),2)[1]
    @test typeof(r) == T
    @test 97 <= r <= 122
    @test mod(r,2)==1
end

if sizeof(Int32) < sizeof(Int)
    r = rand(int32(-1):typemax(Int32))
    @test typeof(r) == Int32
    @test -1 <= r <= typemax(Int32)
end
