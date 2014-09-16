# Issue #6573
srand(0); rand(); x = rand(384);
@test find(x .== rand()) == []

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

# Test array filling, Issue #7643
@test rand(MersenneTwister(0), 1) == [0.8236475079774124]
A = zeros(2, 2)
rand!(MersenneTwister(0), A)
@test A == [0.8236475079774124  0.16456579813368521;
            0.9103565379264364  0.17732884646626457]

# randn
@test randn(MersenneTwister(42)) == -0.5560268761463861
A = zeros(2, 2)
randn!(MersenneTwister(42), A)
@test A == [-0.5560268761463861  0.027155338009193845;
            -0.444383357109696  -0.29948409035891055]

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
    @test all([div(0x00010000000000000000,k)*k - 1 == Base.Random.RandIntGen(uint64(1:k)).u for k in 13 .+ int64(2).^(32:62)])
    @test all([div(0x00010000000000000000,k)*k - 1 == Base.Random.RandIntGen(int64(1:k)).u for k in 13 .+ int64(2).^(32:61)])

end

randn(100000)
randn!(Array(Float64, 100000))
randn(MersenneTwister(10), 100000)
randn!(MersenneTwister(10), Array(Float64, 100000))

# Test ziggurat tables
ziggurat_table_size = 256
nmantissa           = int64(2)^51 # one bit for the sign
ziggurat_nor_r      = BigFloat("3.65415288536100879635194725185604664812733315920964488827246397029393565706474")
nor_section_area    = ziggurat_nor_r*exp(-ziggurat_nor_r^2/2) + erfc(ziggurat_nor_r/sqrt(BigFloat(2)))*sqrt(big(π)/2)
emantissa           = int64(2)^52
ziggurat_exp_r      = BigFloat("7.69711747013104971404462804811408952334296818528283253278834867283241051210533")
exp_section_area    = (ziggurat_exp_r + 1)*exp(-ziggurat_exp_r)

const ki = Array(Uint64, ziggurat_table_size)
const wi = Array(Float64, ziggurat_table_size)
const fi = Array(Float64, ziggurat_table_size)
# Tables for exponential variates
const ke = Array(Uint64, ziggurat_table_size)
const we = Array(Float64, ziggurat_table_size)
const fe = Array(Float64, ziggurat_table_size)
function randmtzig_fill_ziggurat_tables() # Operates on the global arrays
    wib = big(wi)
    fib = big(fi)
    web = big(we)
    feb = big(fe)
    # Ziggurat tables for the normal distribution
    x1 = ziggurat_nor_r
    wib[256] = x1/nmantissa
    fib[256] = exp(-0.5*x1*x1)
    # Index zero is special for tail strip, where Marsaglia and Tsang
    # defines this as
    # k_0 = 2^31 * r * f(r) / v, w_0 = 0.5^31 * v / f(r), f_0 = 1,
    # where v is the area of each strip of the ziggurat.
    ki[1] = uint64(itrunc(x1*fib[256]/nor_section_area*nmantissa))
    wib[1] = nor_section_area/fib[256]/nmantissa
    fib[1] = one(BigFloat)

    for i = 255:-1:2
        # New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
        # need inverse operator of y = exp(-0.5*x*x) -> x = sqrt(-2*ln(y))
        x = sqrt(-2.0*log(nor_section_area/x1 + fib[i+1]))
        ki[i+1] = uint64(itrunc(x/x1*nmantissa))
        wib[i] = x/nmantissa
        fib[i] = exp(-0.5*x*x)
        x1 = x
    end

    ki[2] = uint64(0)

    # Zigurrat tables for the exponential distribution
    x1 = ziggurat_exp_r
    web[256] = x1/emantissa
    feb[256] = exp(-x1)

    # Index zero is special for tail strip, where Marsaglia and Tsang
    # defines this as
    # k_0 = 2^32 * r * f(r) / v, w_0 = 0.5^32 * v / f(r), f_0 = 1,
    # where v is the area of each strip of the ziggurat.
    ke[1] = uint64(itrunc(x1*feb[256]/exp_section_area*emantissa))
    web[1] = exp_section_area/feb[256]/emantissa
    feb[1] = one(BigFloat)

    for i = 255:-1:2
        # New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
        # need inverse operator of y = exp(-x) -> x = -ln(y)
        x = -log(exp_section_area/x1 + feb[i+1])
        ke[i+1] = uint64(itrunc(x/x1*emantissa))
        web[i] = x/emantissa
        feb[i] = exp(-x)
        x1 = x
    end
    ke[2] = zero(Uint64)

    wi[:] = float64(wib)
    fi[:] = float64(fib)
    we[:] = float64(web)
    fe[:] = float64(feb)
    return nothing
end
randmtzig_fill_ziggurat_tables()
@test all(ki == Base.Random.ki)
@test all(wi == Base.Random.wi)
@test all(fi == Base.Random.fi)
@test all(ke == Base.Random.ke)
@test all(we == Base.Random.we)
@test all(fe == Base.Random.fe)

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

@test all([div(0x000100000000,k)*k - 1 == Base.Random.RandIntGen(uint64(1:k)).u for k in 13 .+ int64(2).^(1:30)])
@test all([div(0x000100000000,k)*k - 1 == Base.Random.RandIntGen(int64(1:k)).u for k in 13 .+ int64(2).^(1:30)])

import Base.Random: uuid4, UUID

# UUID
a = uuid4()
@test a == UUID(string(a)) == UUID(utf16(string(a))) == UUID(utf32(string(a)))
@test_throws ArgumentError UUID("550e8400e29b-41d4-a716-446655440000")
@test_throws ArgumentError UUID("550e8400e29b-41d4-a716-44665544000098")
@test_throws ArgumentError UUID("z50e8400-e29b-41d4-a716-446655440000")

#issue 8257
i8257 = 1:1/3:100
for i = 1:100
    @test rand(i8257) in i8257
end
