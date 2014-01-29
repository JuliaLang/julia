@test rand() != rand()
@test 0.0 <= rand() < 1.0
@test rand(Uint32) >= 0
@test -10 <= rand(-10:-5) <= -5
@test -10 <= rand(-10:5) <= 5
@test minimum([rand(int32(1):int32(7^7)) for i = 1:100000]) > 0
@test(typeof(rand(false:true)) == Bool)

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

#same random numbers on for small ranges on all systems

seed = rand(Uint) #leave state nondeterministic as above
srand(seed)
r = int64(rand(int32(97:122)))
srand(seed)
@test r == rand(int64(97:122))

srand(seed)
r = uint64(rand(uint32(97:122)))
srand(seed)
@test r == rand(uint64(97:122))

@test all([div(typemax(Uint32),k)*k == Base.Random.RandIntGen(uint64(1:k)).u for k in 13 .+ int64(2).^(1:31)])
@test all([div(typemax(Uint64),k)*k == Base.Random.RandIntGen(uint64(1:k)).u for k in 13 .+ int64(2).^(32:62)])
@test all([div(typemax(Uint32),k)*k == Base.Random.RandIntGen(int64(1:k)).u for k in 13 .+ int64(2).^(1:31)])
@test all([div(typemax(Uint64),k)*k == Base.Random.RandIntGen(int64(1:k)).u for k in 13 .+ int64(2).^(32:61)])
