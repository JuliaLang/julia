@test rand() != rand()
@test 0.0 <= rand() < 1.0
@test rand(Uint32) >= 0
@test -10 <= rand(-10:-5) <= -5
@test -10 <= rand(-10:5) <= 5
@test minimum([rand(int32(1):int32(7^7)) for i = 1:100000]) > 0
@test(typeof(rand(false:true)) == Bool)

for T in (Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Int128, Uint128, Char, BigInt)
    r = rand(convert(T, 97):convert(T, 122))
    @test typeof(r) == T
    @test 97 <= r <= 122
end

if sizeof(Int32) < sizeof(Int)
    r = rand(int32(-1):typemax(Int32))
    @test typeof(r) == Int32
    @test -1 <= r <= typemax(Int32)
end

range = big(typemax(Int128)):(big(typemax(Int128)) + 10000)
@test rand(range) != rand(range)
@test big(typemax(Int128)) <= rand(range) <= big(typemax(Int128)) + 10000
r = rand(range, 1, 2)
@test size(r) == (1, 2)
@test typeof(r) == Matrix{BigInt}

srand(0)
r = rand(range)
f = rand(BigFloat)
srand(0)
@test rand(range) == r
@test rand(BigFloat) == f

@test rand(BigFloat) != rand(BigFloat)
@test 0.0 <= rand(BigFloat) < 1.0
@test typeof(rand(BigFloat)) == BigFloat

r = rand(BigFloat, 1, 3)
@test size(r) == (1, 3)
@test typeof(r) == Matrix{BigFloat}
