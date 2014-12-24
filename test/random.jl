# Issue #6573
srand(0); rand(); x = rand(384);
@test find(x .== rand()) == []

@test rand() != rand()
@test 0.0 <= rand() < 1.0
@test rand(UInt32) >= 0
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

# Test array filling, Issues #7643, #8360
@test rand(MersenneTwister(0), 1) == [0.8236475079774124]
A = zeros(2, 2)
rand!(MersenneTwister(0), A)
@test A == [0.8236475079774124  0.16456579813368521;
            0.9103565379264364  0.17732884646626457]
@test rand(MersenneTwister(0), Int64, 1) == [4439861565447045202]
A = zeros(Int64, 2, 2)
rand!(MersenneTwister(0), A)
@test A == [858542123778948672  5715075217119798169;
            8690327730555225005 8435109092665372532]

# rand from AbstractArray
let mt = MersenneTwister()
    srand(mt)
    @test rand(mt, 0:3:1000) in 0:3:1000
    @test issubset(rand!(mt, Array(Int, 100), 0:3:1000), 0:3:1000)
    coll = Any[2, UInt128(128), big(619), "string", 'c']
    @test rand(mt, coll) in coll
    @test issubset(rand(mt, coll, 2, 3), coll)

    # check API with default RNG:
    rand(0:3:1000)
    rand!(Array(Int, 100), 0:3:1000)
    rand(coll)
    rand(coll, 2, 3)
end

# randn
@test randn(MersenneTwister(42)) == -0.5560268761463861
A = zeros(2, 2)
randn!(MersenneTwister(42), A)
@test A == [-0.5560268761463861  0.027155338009193845;
            -0.444383357109696  -0.29948409035891055]

for T in (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt,
          Float16, Float32, Float64, Rational{Int})
    r = rand(convert(T, 97):convert(T, 122))
    @test typeof(r) == T
    @test 97 <= r <= 122
    r = rand(convert(T, 97):convert(T,2):convert(T, 122),2)[1]
    @test typeof(r) == T
    @test 97 <= r <= 122
    @test mod(r,2)==1

    if T<:Integer && !(T===BigInt)
        x = rand(typemin(T):typemax(T))
        @test isa(x,T)
        @test typemin(T) <= x <= typemax(T)
    end
end

if sizeof(Int32) < sizeof(Int)
    r = rand(int32(-1):typemax(Int32))
    @test typeof(r) == Int32
    @test -1 <= r <= typemax(Int32)
    @test all([div(0x00010000000000000000,k)*k - 1 == Base.Random.RangeGenerator(uint64(1:k)).u for k in 13 .+ int64(2).^(32:62)])
    @test all([div(0x00010000000000000000,k)*k - 1 == Base.Random.RangeGenerator(int64(1:k)).u for k in 13 .+ int64(2).^(32:61)])

end

# BigInt specific
for T in [UInt32, UInt64, UInt128, Int128]
    s = big(typemax(T)-1000) : big(typemax(T)) + 10000
    @test rand(s) != rand(s)
    @test big(typemax(T)-1000) <= rand(s) <= big(typemax(T)) + 10000
    r = rand(s, 1, 2)
    @test size(r) == (1, 2)
    @test typeof(r) == Matrix{BigInt}

    srand(0)
    r = rand(s)
    srand(0)
    @test rand(s) == r
end

# Test ziggurat tables
ziggurat_table_size = 256
nmantissa           = int64(2)^51 # one bit for the sign
ziggurat_nor_r      = BigFloat("3.65415288536100879635194725185604664812733315920964488827246397029393565706474")
nor_section_area    = ziggurat_nor_r*exp(-ziggurat_nor_r^2/2) + erfc(ziggurat_nor_r/sqrt(BigFloat(2)))*sqrt(big(π)/2)
emantissa           = int64(2)^52
ziggurat_exp_r      = BigFloat("7.69711747013104971404462804811408952334296818528283253278834867283241051210533")
exp_section_area    = (ziggurat_exp_r + 1)*exp(-ziggurat_exp_r)

const ki = Array(UInt64, ziggurat_table_size)
const wi = Array(Float64, ziggurat_table_size)
const fi = Array(Float64, ziggurat_table_size)
# Tables for exponential variates
const ke = Array(UInt64, ziggurat_table_size)
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
    ki[1] = trunc(UInt64,x1*fib[256]/nor_section_area*nmantissa)
    wib[1] = nor_section_area/fib[256]/nmantissa
    fib[1] = one(BigFloat)

    for i = 255:-1:2
        # New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
        # need inverse operator of y = exp(-0.5*x*x) -> x = sqrt(-2*ln(y))
        x = sqrt(-2.0*log(nor_section_area/x1 + fib[i+1]))
        ki[i+1] = trunc(UInt64,x/x1*nmantissa)
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
    ke[1] = trunc(UInt64,x1*feb[256]/exp_section_area*emantissa)
    web[1] = exp_section_area/feb[256]/emantissa
    feb[1] = one(BigFloat)

    for i = 255:-1:2
        # New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
        # need inverse operator of y = exp(-x) -> x = -ln(y)
        x = -log(exp_section_area/x1 + feb[i+1])
        ke[i+1] = trunc(UInt64,x/x1*emantissa)
        web[i] = x/emantissa
        feb[i] = exp(-x)
        x1 = x
    end
    ke[2] = zero(UInt64)

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

seed = rand(UInt) #leave state nondeterministic as above
srand(seed)
r = int64(rand(int32(97:122)))
srand(seed)
@test r == rand(int64(97:122))

srand(seed)
r = uint64(rand(uint32(97:122)))
srand(seed)
@test r == rand(uint64(97:122))

@test all([div(0x000100000000,k)*k - 1 == Base.Random.RangeGenerator(uint64(1:k)).u for k in 13 .+ int64(2).^(1:30)])
@test all([div(0x000100000000,k)*k - 1 == Base.Random.RangeGenerator(int64(1:k)).u for k in 13 .+ int64(2).^(1:30)])

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

# test code paths of rand!

let mt = MersenneTwister(0)
    A128 = Array(UInt128, 0)
    @test length(rand!(mt, A128)) == 0
    for (i,n) in enumerate([1, 3, 5, 6, 10, 11, 30])
        resize!(A128, n)
        rand!(mt, A128)
        @test length(A128) == n
        @test A128[end] == UInt128[0x15de6b23025813ad129841f537a04e40,
                                   0xcfa4db38a2c65bc4f18c07dc91125edf,
                                   0x33bec08136f19b54290982449b3900d5,
                                   0xde41af3463e74cb830dad4add353ca20,
                                   0x066d8695ebf85f833427c93416193e1f,
                                   0x48fab49cc9fcee1c920d6dae629af446,
                                   0x4b54632b4619f4eca22675166784d229][i]
    end

    srand(mt,0)
    for (i,T) in enumerate([Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, Float16, Float32])
        A = Array(T, 16)
        B = Array(T, 31)
        rand!(mt, A)
        rand!(mt, B)
        @test A[end] == Any[21,0x7b,17385,0x3086,-1574090021,0xadcb4460,6797283068698303107,0x4e91c9c4d4f5f759,
                            -3482609696641744459568613291754091152,float16(0.03125),0.68733835f0][i]

        @test B[end] == Any[49,0x65,-3725,0x719d,814246081,0xdf61843a,-3010919637398300844,0x61b367cf8810985d,
                            -33032345278809823492812856023466859769,float16(0.95),0.51829386f0][i]
    end

    srand(mt,0)
    AF64 = Array(Float64, Base.Random.dsfmt_get_min_array_size()-1)
    @test rand!(mt, AF64)[end] == 0.957735065345398
    @test rand!(mt, AF64)[end] == 0.6492481059865669
    resize!(AF64, 2*length(mt.vals))
    @test Base.Random.rand_AbstractArray_Float64!(mt, AF64)[end]  == 0.432757268470779
end

# Issue #9037
let mt = MersenneTwister()
    a = Array(Float64, 0)
    resize!(a, 1000) # could be 8-byte aligned
    b = Array(Float64, 1000) # should be 16-byte aligned
    c8 = Array(UInt8, 8001)
    c = pointer_to_array(Ptr{Float64}(pointer(c8, 2)), 1000) # Int(pointer(c)) % 16 == 1

    for A in (a, b, c)
        srand(mt, 0)
        rand(mt) # this is to fill mt.vals, cf. #9040
        rand!(mt, A) # must not segfault even if Int(pointer(A)) % 16 != 0
        @test A[end-4:end] == [0.49508297796349776,0.3408340446375888,0.3211229457075784,0.9103565379264364,0.16456579813368521]
    end
end

# test all rand APIs
for rng in ([], [MersenneTwister()], [RandomDevice()])
    for f in [rand, randn, randexp]
        f(rng...)        ::Float64
        f(rng..., 5)     ::Vector{Float64}
        f(rng..., 2, 3)  ::Array{Float64, 2}
    end
    for f! in [randn!, randexp!]
        f!(rng..., Array(Float64, 5))    ::Vector{Float64}
        f!(rng..., Array(Float64, 2, 3)) ::Array{Float64, 2}
    end

    randbool(rng...)               ::Bool
    randbool(rng..., 5)            ::BitArray{1}
    randbool(rng..., 2, 3)         ::BitArray{2}
    rand!(rng..., BitArray(5))     ::BitArray{1}
    rand!(rng..., BitArray(2, 3))  ::BitArray{2}

    for T in [Base.IntTypes..., Bool, Float16, Float32, Float64]
        a0 = rand(rng..., T)       ::T
        a1 = rand(rng..., T, 5)    ::Vector{T}
        a2 = rand(rng..., T, 2, 3) ::Array{T, 2}
        if T <: FloatingPoint
            for a in [a0, a1..., a2...]
                @test 0.0 <= a < 1.0
            end
        end
        for A in (Array(T, 5), Array(T, 2, 3))
            rand!(rng..., A)            ::typeof(A)
            rand!(rng..., A, T[0,1,2])  ::typeof(A)
            rand!(rng..., sparse(A))            ::typeof(sparse(A))
            rand!(rng..., sparse(A), T[0,1,2])  ::typeof(sparse(A))
        end
    end
end

# test uniform distribution of floats
let bins = [prevfloat(0.0):0.25:1.0]
    for rng in [srand(MersenneTwister()), RandomDevice()]
        for T in [Float16,Float32,Float64]
            # array version
            _, counts = hist(rand(rng, T, 2000), bins)
            @test minimum(counts) > 300 # should fail with proba < 1e-26
            # scalar version
            _, counts = hist([rand(rng, T) for i in 1:2000], bins)
            @test minimum(counts) > 300
        end
    end
end
