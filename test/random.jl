@test rand() != rand()
@test 0.0 <= rand() < 1.0
@test rand(Uint32) >= 0
@test -10 <= rand(-10:-5) <= -5
@test -10 <= rand(-10:5) <= 5
@test minimum([rand(int32(1):int32(7^7)) for i = 1:100000]) > 0
@test(typeof(rand(false:true)) == Bool)

@test length(randn(4, 5)) == 20
@test length(randbool(4, 5)) == 20

@test rand(MersenneTwister()) == 0.8236475079774124
@test rand(MersenneTwister(0)) == 0.8236475079774124
@test rand(MersenneTwister(42)) == 0.5331830160438613
# Try a seed larger than 2^32
@test rand(MersenneTwister(5294967296)) == 0.3498809918210497

# randn
@test randn(MersenneTwister(42)) == -0.5560268761438381
A = zeros(2, 2)
randn!(MersenneTwister(42), A)
@test A == [-0.5560268761438381  0.02715533800914659;
            -0.4443833571072952 -0.29948409035852047]

for T in (Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Int128, Uint128,
          Char, Float16, Float32, Float64, Rational{Int})
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
