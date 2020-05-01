# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SparseArrays
using Test: guardseed

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays

using Random
using Random.DSFMT

using Random: Sampler, SamplerRangeFast, SamplerRangeInt, SamplerRangeNDL, MT_CACHE_F, MT_CACHE_I

import Future # randjump

@testset "Issue #6573" begin
    Random.seed!(0)
    rand()
    x = rand(384)
    @test findall(x .== rand()) == []
end

@test rand() != rand()
@test 0.0 <= rand() < 1.0
@test rand(UInt32) >= 0
@test -10 <= rand(-10:-5) <= -5
@test -10 <= rand(-10:5) <= 5
@test minimum([rand(Int32(1):Int32(7^7)) for i = 1:100000]) > 0
@test typeof(rand(false:true)) === Bool
@test typeof(rand(Char)) === Char
@test length(randn(4, 5)) == 20
@test length(randn(ComplexF64, 4, 5)) == 20
@test length(bitrand(4, 5)) == 20

@test rand(MersenneTwister(0)) == 0.8236475079774124
@test rand(MersenneTwister(42)) == 0.5331830160438613
# Try a seed larger than 2^32
@test rand(MersenneTwister(5294967296)) == 0.3498809918210497

# Test array filling, Issues #7643, #8360
@test rand(MersenneTwister(0), 1) == [0.8236475079774124]
let A = zeros(2, 2)
    rand!(MersenneTwister(0), A)
    @test A == [0.8236475079774124  0.16456579813368521;
                0.9103565379264364  0.17732884646626457]
end
let A = zeros(2, 2)
    @test_throws ArgumentError rand!(MersenneTwister(0), A, 5)
    @test rand(MersenneTwister(0), Int64, 1) == [2118291759721269919]
end
let A = zeros(Int64, 2, 2)
    rand!(MersenneTwister(0), A)
    @test A == [858542123778948672  5715075217119798169;
                8690327730555225005 8435109092665372532]
end

# rand from AbstractArray
let mt = MersenneTwister()
    @test rand(mt, 0:3:1000) in 0:3:1000
    @test issubset(rand!(mt, Vector{Int}(undef, 100), 0:3:1000), 0:3:1000)
    coll = Any[2, UInt128(128), big(619), "string"]
    @test rand(mt, coll) in coll
    @test issubset(rand(mt, coll, 2, 3), coll)

    # check API with default RNG:
    rand(0:3:1000)
    rand!(Vector{Int}(undef, 100), 0:3:1000)
    rand(coll)
    rand(coll, 2, 3)
end

# randn
@test randn(MersenneTwister(42)) == -0.5560268761463861
let A = zeros(2, 2)
    randn!(MersenneTwister(42), A)
    @test A == [-0.5560268761463861  0.027155338009193845;
                -0.444383357109696  -0.29948409035891055]
end

let B = zeros(ComplexF64, 2)
    randn!(MersenneTwister(42), B)
    @test B == [ComplexF64(-0.5560268761463861,-0.444383357109696),
                ComplexF64(0.027155338009193845,-0.29948409035891055)] * 0.7071067811865475244008
end

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

@test !any([(Random.maxmultiple(i)+i) > 0xFF for i in 0x00:0xFF])
@test all([(Random.maxmultiple(i)+1) % i for i in 0x01:0xFF] .== 0)
@test all([(Random.maxmultiple(i)+1+i) > 0xFF for i in 0x00:0xFF])
@test length(0x00:0xFF)== Random.maxmultiple(0x0)+1


if sizeof(Int32) < sizeof(Int)
    local r = rand(Int32(-1):typemax(Int32))
    @test typeof(r) == Int32
    @test -1 <= r <= typemax(Int32)
    for U = (Int64, UInt64)
        @test all(div(one(UInt128) << 52, k)*k - 1 == SamplerRangeInt(map(U, 1:k)).u
                  for k in 13 .+ Int64(2).^(32:51))
        @test all(div(one(UInt128) << 64, k)*k - 1 == SamplerRangeInt(map(U, 1:k)).u
                  for k in 13 .+ Int64(2).^(52:62))
    end
end

# BigInt specific
for T in [UInt32, UInt64, UInt128, Int128]
    local r, s
    s = big(typemax(T)-1000) : big(typemax(T)) + 10000
    # s is a 11001-length array
    @test rand(s) isa BigInt
    @test sum(rand(s, 1000) .== rand(s, 1000)) <= 20
    @test big(typemax(T)-1000) <= rand(s) <= big(typemax(T)) + 10000
    r = rand(s, 1, 2)
    @test size(r) == (1, 2)
    @test typeof(r) == Matrix{BigInt}
    guardseed() do
        Random.seed!(0)
        r = rand(s)
        Random.seed!(0)
        @test rand(s) == r
    end
end

# Test ziggurat tables
ziggurat_table_size = 256
nmantissa           = Int64(2)^51 # one bit for the sign
ziggurat_nor_r      = parse(BigFloat,"3.65415288536100879635194725185604664812733315920964488827246397029393565706474")
erfc_zigg_root2     = parse(BigFloat,"2.580324876539008898343885504487203185398584536409033046076029509351995983934371e-04")
nor_section_area    = ziggurat_nor_r*exp(-ziggurat_nor_r^2/2) + erfc_zigg_root2*sqrt(big(π)/2)
emantissa           = Int64(2)^52
ziggurat_exp_r      = parse(BigFloat,"7.69711747013104971404462804811408952334296818528283253278834867283241051210533")
exp_section_area    = (ziggurat_exp_r + 1)*exp(-ziggurat_exp_r)

ki = Vector{UInt64}(undef, ziggurat_table_size)
wi = Vector{Float64}(undef, ziggurat_table_size)
fi = Vector{Float64}(undef, ziggurat_table_size)
# Tables for exponential variates
ke = Vector{UInt64}(undef, ziggurat_table_size)
we = Vector{Float64}(undef, ziggurat_table_size)
fe = Vector{Float64}(undef, ziggurat_table_size)
function randmtzig_fill_ziggurat_tables() # Operates on the global arrays
    wib = big.(wi)
    fib = big.(fi)
    web = big.(we)
    feb = big.(fe)
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

    ki[2] = UInt64(0)

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

    wi[:] = wib
    fi[:] = fib
    we[:] = web
    fe[:] = feb
    return nothing
end
randmtzig_fill_ziggurat_tables()
@test all(ki == Random.ki)
@test all(wi == Random.wi)
@test all(fi == Random.fi)
@test all(ke == Random.ke)
@test all(we == Random.we)
@test all(fe == Random.fe)

for U in (Int64, UInt64)
    @test all(div(one(UInt64) << 52, k)*k - 1 == SamplerRangeInt(map(U, 1:k)).u
              for k in 13 .+ Int64(2).^(1:30))
end

#issue 8257
let i8257 = 1:1/3:100
    for i = 1:100
        @test rand(i8257) in i8257
    end
end

# test code paths of rand!

let mt = MersenneTwister(0)
    A128 = Vector{UInt128}()
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

    Random.seed!(mt, 0)
    for (i,T) in enumerate([Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, Float16, Float32])
        A = Vector{T}(undef, 16)
        B = Vector{T}(undef, 31)
        rand!(mt, A)
        rand!(mt, B)
        @test A[end] == Any[21, 0x7b, 17385, 0x3086, -1574090021, 0xadcb4460, 6797283068698303107, 0xc8e6453e139271f3,
                            69855512850528774484795047199183096941, Float16(0.16895), 0.21086597f0][i]
        @test B[end] == Any[49, 0x65, -3725, 0x719d, 814246081, 0xdf61843a, 2120308604158549401, 0xcb28c236e9c0f608,
                            61881313582466480231846019869039259750, Float16(0.38672), 0.20027375f0][i]
    end

    Random.seed!(mt, 0)
    AF64 = Vector{Float64}(undef, Random.dsfmt_get_min_array_size()-1)
    @test rand!(mt, AF64)[end] == 0.957735065345398
    @test rand!(mt, AF64)[end] == 0.6492481059865669
    resize!(AF64, 2*length(mt.vals))
    @test invoke(rand!, Tuple{MersenneTwister,AbstractArray{Float64},Random.SamplerTrivial{Random.CloseOpen01_64}},
                 mt, AF64, Random.SamplerTrivial(Random.CloseOpen01()))[end]  == 0.1142787906708973
end

# Issue #9037
let mt = MersenneTwister(0)
    a = Vector{Float64}()
    resize!(a, 1000) # could be 8-byte aligned
    b = Vector{Float64}(undef, 1000) # should be 16-byte aligned
    c8 = Vector{UInt64}(undef, 1001)
    pc8 = pointer(c8)
    if Int(pc8) % 16 == 0
        # Make sure pc8 is not 16-byte aligned since that's what we want to test.
        # It still has to be 8-byte aligned since it is otherwise invalid on
        # certain architectures (e.g. ARM)
        pc8 += 8
    end
    c = unsafe_wrap(Array, Ptr{Float64}(pc8), 1000) # Int(pointer(c)) % 16 == 8

    for A in (a, b, c)
        local A
        Random.seed!(mt, 0)
        rand(mt) # this is to fill mt.vals, cf. #9040
        rand!(mt, A) # must not segfault even if Int(pointer(A)) % 16 != 0
        @test A[end-4:end] == [0.3371041633752143, 0.41147647589610803, 0.6063082992397912, 0.9103565379264364, 0.16456579813368521]
    end
end

# make sure reading 128-bit ints from RandomDevice works
let a = [rand(RandomDevice(), UInt128) for i=1:10]
    @test reduce(|, a)>>>64 != 0
end

# test all rand APIs
for rng in ([], [MersenneTwister(0)], [RandomDevice()])
    ftypes = [Float16, Float32, Float64]
    cftypes = [ComplexF16, ComplexF32, ComplexF64, ftypes...]
    types = [Bool, Char, BigFloat, Base.BitInteger_types..., ftypes...]
    randset = Set(rand(Int, 20))
    randdict = Dict(zip(rand(Int,10), rand(Int, 10)))
    collections = [BitSet(rand(1:100, 20))          => Int,
                   randset                          => Int,
                   GenericSet(randset)              => Int,
                   randdict                         => Pair{Int,Int},
                   GenericDict(randdict)            => Pair{Int,Int},
                   1:100                            => Int,
                   rand(Int, 100)                   => Int,
                   Int                              => Int,
                   Float64                          => Float64,
                   "qwèrtï"                         => Char,
                   GenericString("qwèrtï")          => Char,
                   OffsetArray(rand(2, 3), (4, -5)) => Float64]
    functypes = Dict(rand  => types, randn  => cftypes, randexp  => ftypes,
                     rand! => types, randn! => cftypes, randexp! => ftypes)

    b2 = big(2)
    u3 = UInt(3)
    for f in [rand, randn, randexp]
        f(rng...)                     ::Float64
        f(rng..., 5)                  ::Vector{Float64}
        f(rng..., 2, 3)               ::Array{Float64, 2}
        f(rng..., b2, u3)             ::Array{Float64, 2}
        for T in functypes[f]
            a0 = f(rng..., T)         ::T
            a1 = f(rng..., T, 5)      ::Vector{T}
            a2 = f(rng..., T, 2, 3)   ::Array{T, 2}
            a3 = f(rng..., T, b2, u3) ::Array{T, 2}
            a4 = f(rng..., T, (2, 3)) ::Array{T, 2}
            if T <: AbstractFloat && f === rand
                for a in [a0, a1..., a2..., a3..., a4...]
                    @test 0.0 <= a < 1.0
                end
            end
        end
    end
    for (C, T) in collections
        a0  = rand(rng..., C)                                                       ::T
        a1  = rand(rng..., C, 5)                                                    ::Vector{T}
        a2  = rand(rng..., C, 2, 3)                                                 ::Array{T, 2}
        a3  = rand(rng..., C, (2, 3))                                               ::Array{T, 2}
        a4  = rand(rng..., C, b2, u3)                                               ::Array{T, 2}
        a5  = rand!(rng..., Array{T}(undef, 5), C)                          ::Vector{T}
        a6  = rand!(rng..., Array{T}(undef, 2, 3), C)                       ::Array{T, 2}
        a7  = rand!(rng..., GenericArray{T}(undef, 5), C)                   ::GenericArray{T, 1}
        a8  = rand!(rng..., GenericArray{T}(undef, 2, 3), C)                ::GenericArray{T, 2}
        a9  = rand!(rng..., OffsetArray(Array{T}(undef, 5), 9), C)          ::OffsetArray{T, 1}
        a10 = rand!(rng..., OffsetArray(Array{T}(undef, 2, 3), (-2, 4)), C) ::OffsetArray{T, 2}
        @test size(a1) == (5,)
        @test size(a2) == size(a3) == (2, 3)
        for a in [a0, a1..., a2..., a3..., a4..., a5..., a6..., a7..., a8..., a9..., a10...]
            if C isa Type
                @test a isa C
            else
                @test a in C
            end
        end
    end
    for C in [1:0, Dict(), Set(), BitSet(), Int[],
              GenericDict(Dict()), GenericSet(Set()),
              "", Test.GenericString("")]
        @test_throws ArgumentError rand(rng..., C)
        @test_throws ArgumentError rand(rng..., C, 5)
    end
    for f! in [rand!, randn!, randexp!]
        for T in functypes[f!]
            X = T == Bool ? T[0,1] : T[0,1,2]
            for A in (Vector{T}(undef, 5),
                      Matrix{T}(undef, 2, 3),
                      GenericArray{T}(undef, 5),
                      GenericArray{T}(undef, 2, 3),
                      OffsetArray(Array{T}(undef, 5), -3),
                      OffsetArray(Array{T}(undef, 2, 3), (4, 5)))
                local A
                f!(rng..., A)                    ::typeof(A)
                if f! === rand!
                    f!(rng..., A, X)             ::typeof(A)
                    if A isa Array && T !== Char # Char/Integer comparison
                        f!(rng..., sparse(A))    ::typeof(sparse(A))
                        f!(rng..., sparse(A), X) ::typeof(sparse(A))
                    end
                end
            end
        end
    end

    bitrand(rng..., 5)             ::BitArray{1}
    bitrand(rng..., 2, 3)          ::BitArray{2}
    bitrand(rng..., b2, u3)        ::BitArray{2}
    rand!(rng..., BitVector(undef, 5))     ::BitArray{1}
    rand!(rng..., BitMatrix(undef, 2, 3))  ::BitArray{2}

    # Test that you cannot call randn or randexp with non-Float types.
    for r in [randn, randexp, randn!, randexp!]
        local r
        @test_throws MethodError r(Int)
        @test_throws MethodError r(Int32)
        @test_throws MethodError r(Bool)
        @test_throws MethodError r(String)
        @test_throws MethodError r(AbstractFloat)
        # TODO(#17627): Consider adding support for randn(BigFloat) and removing this test.
        @test_throws MethodError r(BigFloat)

        @test_throws MethodError r(Int64, (2,3))
        @test_throws MethodError r(String, 1)

        @test_throws MethodError r(rng..., Number, (2,3))
        @test_throws MethodError r(rng..., Any, 1)
    end
end

function hist(X, n)
    v = zeros(Int, n)
    for x in X
        v[floor(Int, x*n) + 1] += 1
    end
    v
end

# test uniform distribution of floats
for rng in [MersenneTwister(), RandomDevice()],
    T in [Float16, Float32, Float64, BigFloat],
        prec in (T == BigFloat ? [3, 53, 64, 100, 256, 1000] : [256])
    setprecision(BigFloat, prec) do
        # array version
        counts = hist(rand(rng, T, 2000), 4)
        @test minimum(counts) > 300 # should fail with proba < 1e-26
        # scalar version
        counts = hist([rand(rng, T) for i in 1:2000], 4)
        @test minimum(counts) > 300
    end
end

@testset "rand(Bool) uniform distribution" begin
    for n in [rand(1:8), rand(9:16), rand(17:64)]
        a = zeros(Bool, n)
        as = zeros(Int, n)
        # we will test statistical properties for each position of a,
        # but also for 3 linear combinations of positions (for the array version)
        lcs = unique!.([rand(1:n, 2), rand(1:n, 3), rand(1:n, 5)])
        aslcs = zeros(Int, 3)
        for rng = (MersenneTwister(), RandomDevice())
            for scalar = [false, true]
                fill!(a, 0)
                fill!(as, 0)
                fill!(aslcs, 0)
                for _ = 1:49
                    if scalar
                        for i in eachindex(as)
                            as[i] += rand(rng, Bool)
                        end
                    else
                        as .+= rand!(rng, a)
                        aslcs .+= [xor(getindex.(Ref(a), lcs[i])...) for i in 1:3]
                    end
                end
                @test all(x -> 7 <= x <= 42, as) # for each x, fails with proba ≈ 2/35_000_000
                if !scalar
                    @test all(x -> 7 <= x <= 42, aslcs)
                end
            end
        end
    end
end

# test reproducility of methods
let mta = MersenneTwister(42), mtb = MersenneTwister(42)

    @test rand(mta) == rand(mtb)
    @test rand(mta,10) == rand(mtb,10)
    @test randn(mta) == randn(mtb)
    @test randn(mta,10) == randn(mtb,10)
    @test randexp(mta) == randexp(mtb)
    @test randexp(mta,10) == randexp(mtb,10)
    @test rand(mta,1:100) == rand(mtb,1:100)
    @test rand(mta,1:10,10) == rand(mtb,1:10,10)
    @test rand(mta,Bool) == rand(mtb,Bool)
    @test bitrand(mta,10) == bitrand(mtb,10)

    @test randstring(mta) == randstring(mtb)
    @test randstring(mta,10) == randstring(mtb,10)

    @test randsubseq(mta,1:10,0.4) == randsubseq(mtb,1:10,0.4)
    @test randsubseq!(mta,Int[],1:10,0.4) == randsubseq!(mtb,Int[],1:10,0.4)

    @test shuffle(mta,Vector(1:10)) == shuffle(mtb,Vector(1:10))
    @test shuffle!(mta,Vector(1:10)) == shuffle!(mtb,Vector(1:10))
    @test shuffle(mta,Vector(2:11)) == shuffle(mtb,2:11)
    @test shuffle!(mta, rand(mta, 2, 3)) == shuffle!(mtb, rand(mtb, 2, 3))
    @test shuffle(mta, rand(mta, 2, 3)) == shuffle(mtb, rand(mtb, 2, 3))

    @test randperm(mta,10) == randperm(mtb,10)
    @test sort!(randperm(10)) == sort!(shuffle(1:10)) == 1:10
    @test randperm(mta,big(10)) == randperm(mtb,big(10)) # cf. #16376
    @test randperm(0) == []
    @test_throws ArgumentError randperm(-1)

    let p = randperm(UInt16(12))
        @test typeof(p) ≡ Vector{UInt16}
        @test sort!(p) == 1:12
    end

    A, B = Vector{Int}(undef, 10), Vector{Int}(undef, 10)
    @test randperm!(mta, A) == randperm!(mtb, B)
    @test randperm!(A) === A

    @test randcycle(mta,10) == randcycle(mtb,10)
    @test randcycle!(mta, A) == randcycle!(mtb, B)
    @test randcycle!(A) === A

    let p = randcycle(UInt16(10))
        @test typeof(p) ≡ Vector{UInt16}
        @test sort!(p) == 1:10
    end

    @test sprand(mta,1,1,0.9) == sprand(mtb,1,1,0.9)
    @test sprand(mta,10,10,0.3) == sprand(mtb,10,10,0.3)
end

@testset "MersenneTwister polynomial generation and jump" begin
    seed = rand(UInt)
    mta = MersenneTwister(seed)
    mtb = MersenneTwister(seed)
    step = 25000*2
    size = 4
    jump25000 = "35931a4947eeab70a9abbfaca4a79cfcf2610a35a586c6f4e4bdfa826d538cbfb0432c37321fcec4f6c98de3df06685087032988b0ad9a2144562aa82e06f2f6f256b5b412c524e35383a894da7b04e142c4156290585186d8fc06d3141a778220c2519a851b5a9e5947a3f745b71804631988825e21dba40392ff4c036b30d2d013b45e2be94b5e130a9c6424d2e82f48c855c81bd10757fdb5a91e23e9e312e430514ea31631d8897b4cf26eb39b37be0c92706e5637d4b34c1e4046b741e455df195cb512e8e0f8d578175a3da5e00d7ce247d9b92042b1b515d01f7f89fe661ebccb06dfb77bc0fbb99806921b472ccce58f2166ac058d9cf427ad7d74986e60a56d2fee0a8b680e466a8ea4e508a76c058b6f97b99c9aa5b10297b1a1bd6a8e80f3a79e008fa55a4a8915fbdec78b6b117ad67e195311fe79fc084c33f6db546f5b7602d010fa8b830e3f1b00cef00ee16840178fc7e9aa5f1cee625d43de8488bf6c8bd379ea6f97c55c7a9ee091477a23533d5e52e194bd9d4e17b02a64a2736feb3779fabd5777e448ffee0f2d4b38a8e7441822b882fc6df0bde8541e85c0c78a05936cff0c88a50980b7a84971fba3650991fe2cba425ac4b4289e7b06ce2cfabfcc8a553201e8c74b45e4ae74b6d054e37af95e6fd55e029b7c526b85ecfb3be8db670218ee3dda7b2a54ab1ed26eefe4cd1d2a9c589a6e94d0aa3ebe29e40e616aa0b731061c3d6e247ec610024a1a97b7adb7919308b0fb5dd5d51a58aa2f55d77b88037de7c1a74823c96cb09d22dd7f90dba14eefdcffaab34d323c829f24742f6f6b32b0724a26ae4a81130a8a275d30c21e6245fa27cf26d606a49bccba2980697c32d9efe583c4ee2140569025c4f044d744bc40cec1660d9e4d2de3a4de83bae4f0a9fdb34ef4509b2b4e6c37967a485a52d69d1573bb826bc64c966de9c792b7c2f07b645c56a29381911a98928e48516f246a55bcaa78f3c7d1c30127df5f06ba0a2d6a5e54605a20e60fab30c01a9472cb610ca0ef2418a985af00c7e47539111bf539dd554297d0374a7ff627d879600595b442c8dcffcffa3bbb07e5c7882ff0858142be4deac448698f0917fe2b7a9b686a9df1fa929f06a51aff992a6ee0b0605f8b34b87600cfa0af2475333b78625ce1520c793dc5080218247b4e41bbd7d9dab163470fe17a3d2622cdce979cc5565b0bc04eabaf656f21fa072a18ab33c656b665248ef20321407fef263b1c67316f2c6f236951990099e42d4614d8e08b27aa89d9f4548fa321d4b381d2da04fd7f17d6b9a68adfd0e4427196d25dcad869f8a155c6242f7d072baa5e7405ceb65dfaa3eb864bfe679a17df34273fde5037befe9ed5391b932cee271f59128c61ab3f0fc3f7cf8ff051fbda8382c64579efddd494c79850c56bda73bcd39c20c2820d191995b3335253c3b0ac8f5e5373f40c228886e6c526c2c249a5304578ba2a80f591c34ca1eaa84d6cc9399cf3f1207e61c4acada647e4e87ad5fba84aeeff6b6881d35bda77c74384fc5e279a0f495d509bc882c2b8bc790651a6d7a4ecba23a3f05111e1d8be37c03439fbd484668ceab69a52b7d519b169cbbcf634ee5e3bf78a5f8771f95fea03f2cb889e116a9f5de3abeacb8e42475fb5d022484b02d11f1e406332e0a773098fc4f0baa57cda2863c554f291d4eb74e63e4b3d44b0ed156bff1820003d407a3aaa9e6dfaa226ba7ef2fd0eff90a5482926f47f24f67019edccb6fd329eef30b5fb2125276aa1fe75a702b32c907ab133c72a74e77e0a5eb48fc5176b9d65b75b0038e1a9ed74ec2a3dcd2348fa54256f082abc01a301bacef7380f20ee0411c08a35dafdaae9f9fc123448da28626ffcc654e9d522bc8b8776b13a3310f7eeb4d27290ef4cbc7492fbcb5409d455748a8a1f087430cf5e6f453e2caa0c5343fcf4374cc38bead49941d8ab59b4d5181716c238aa88dbf1c4a2da3a9a9b9435d5ee1d51d27b0655a4308c1252aaf633cd8f44a351ffc8cec65de0b7e4e2556100e2ae9bc511044351109a6254b2d387b1a72c768f43fa7be6b93806e323b55c3e7925ed627dc708fde0954b299b1ca33bb7fbe33e0f9e4ce5b4f26efaf8e5b9507ada4f8658998eb7167afbd4482ee47cc60f4039d6a77c1fb126033bfc2e7c6162ff7561f22e263325c53a014a4ac9390fe6fab5d433c1e9896fe561f22fc8290f3f4560b676f3dfbfd1fe605343a0685349241b83a28d61cc0292d1f638a36d3d87bfa9f72f9df1cfe90692dfda5bd5e698362f5316984cbe73a132a801acbca76b5f5c23d98a217f2159b6cbbcdf8f52d23ea24c9471a31562a651b20e05cd0300ee500a450cfdaa4d2d83f7e7e27f8b7c793cf80f8067dadef77e49a64c373b97bac4dd472e5145072c73d0fdd68d9646c8a8ed9aec6c40bc915ae44ae27391ca0f1a4d2cb1c3d097be614e6eba807f4549d769a5872f268ccf770f2682d844490348f0b6a0d2b51aadbb5523cf708b66f9928eed12b35a39cf42d283b29f5283e1c8ba1f73457af17b14cdfdf9a85b0589acf1f9504e46b0bab8be848dac5673587035b99f56c41d3195bbba1616b149a22193cfb760d6bf2d84861653cd21be9a2d33187cb25d47fbecdd4626d1d97202f460a39b7128cadb77ddf682feca61fb6de0290df598a565a6361a91d76c0c685046489ed4cb1dcc4f1cea849c12dc4a3d38d3010567f387590532b78927e92f0b718c84e882b3df071a78a011d0fd56d4101dcb009914a16a781b240a6fb2440c72b0ffb365be9d3459d114e665a0d35d7b8bd280101d85d1211d939ba0b15ab528c4f9dd2b001172561d211671b96873010ae3c2b8317f773d735698914228764b831423ae19dd4bbb008b9f1bd1e3ebdd626e629a46a9dd70bdd4bb30e2279e83c12bbbead6479b5f9980b1a9c785395520703a9367d931b45c1566c9d314b1745cafc6d0667cc9bc94d0c53a960c829eb09b768ab6bb2133e4fea1d939f3d3f8e1237210cf3577c830f0493073dc1d189abf27402b8b31b7c172c43dbf331a0828adfe737380e763d0ab0bfaaf94ec04830f94380a83718f340c4eeb20d7eb22b94613be84a9ed332ab364efff6cb37eec35d186185cca725e7a748f6bdb427604fb1628d49a7424a5a62a2e930fe142b035503af332fe748d5e63591b9ac54071ca843d5e474a48837de8b80387f3269ab50d2fd99c08c971e015d13fa02c7c315922ce58bdacbf8ee48827851a61fca59882d7eadcce3166dfe012aa9ec849e698e776a4d384f4755b506a222636942a81bbbffa1ff47e4d81fe68120aebcfd1a7e0000fd0cffdc44e1f0cd69ea2b4936564c78af51fed1cc8e34f0b46d6330b4b50ddee09335b7b0be0bc9f7f8e48415e15d08f811653d21bc6dd152742b086caadcc6dff5e27b40da42c2f1ebf3dd2bd51c418718e499859239317fcab10892eadf1c0ebf7a4246bce4cce3617193032f3e41b977dc8650298ac39631c527460364effea0f0bfd043df72ead0406aba1bcd636d65d7b89979eb8e1";
    jump1e20  = "e172e20c5d2de26b567c0cace9e7c6cc4407bd5ffcd22ca59d37b73d54fdbd937cd3abc6f502e8c186dbd4f1a06b9e2b894f31be77424f94dddfd5a45888a84ca66eeeb242eefe6764ed859dafccae7a6a635b3a63fe9dfbbd5f2d3f2610d39388f53060e84edae75be4f4f2272c0f1f26d1231836ad040ab091550f8a3a5423fb3ab83e068fe2684057f15691c4dc757a3aee4bca8595bf1ad03500d9620a5dbe3b2d64380694895d2f379ca928238293ea267ce14236d5be816a61f018fe4f6bc3c9865f5d4d4186e320ab653d1f3c035ae83e2ad725648a67d3480331e763a1dcdfb5711b56796170b124f5febd723a664a2deefbfa9999d922a108b0e683582ae8d3baacb5bb56683405ea9e6e0d71ddb24b2229c72bb9d07061f2d1fa097ade823b607a2029d6e121ae09d93de01a154199e8e6a6e77c970bda72ba8079b2b3a15dd494a3188b1d94a25ae108a8a5bd0b050e6ce64a365a21420e07fdeebecae02eb68a4304b59283055d22c27d680ea35952834d828c9b9b9dd1a886b4f7fe82fe8f2a962e1e5390e563dc281c799aee2a441b7a813facb6ff5e94c059710dcfe7e6b1635e21ae0dc878dd5f7cc0e1101a74452495a67d23a2672c939f32c81d4a2611073990e92a084cc3a62fd42ee566f29d963a9cc5100ccd0a200f49ce0a74fa891efa1b974d342b7fedf9269e40d9b34e3c59c3d37201aecd5a04f4ae3d0c9a68c7ab78c662390e4cf36cb63ea3539c442efd0bf4aace4b8c8bde93c3d84b4d6290adfae1c5e3fcd457b6f3159e501f17b72ff6bc13d6bf61fbdafabefd16ac1dae0bca667e4e16a2b800732f1d0a9274c8a4c6cccd2db62fc275dc308c31c11cd6fda78de2f81f0e542b76b42b2cc09ed8f965d94c714c9918064f53af5379cfbbc31edf9cbce694f63a75f122048de6e57b094908f749661456813a908027f5d8397ab7962bf75ac779a3e1b7ae3fbc93397a67b486bb849befff1de6162ef2819715a88f41881e366ace692a900796a2806393898dd1750ac2b4ca3d34ca48942322fb6375f0c9a00c9701048ee8d7d7a17e11739177a7ad5027556e85835daf8594d84a97fe6621c0fce1495ae6ab8676cdc992d247acf5a4e5ec8c4755fde28117228d2c3ecf89edb91e93d949e2174924572265e36d176d082ed1be884e51d885ba3cda175c51edcee5042eaf519d292aa05aa4185b03858d710a9d0880b3d4e5111f858a52fe352cbe0a24f06a3d977ae2eb85e2a03a68131d0ab91dac4941067cf90ecd0fce156bcd40b8968cd4aa11e0b4353b14508d79d13ac00af4a4d452496b7f2393699889aa1e508427dbf0be3db91d955feb51e559af57640c6b3f9d5f95609852c28f9462a9869dd93acbdb1aafb2381ebb886a0b3fcec278f8bb0f62c23e157e49b89245b0881268ce594acbddd3605b9eaa77c9ff513e0dbad514914136d96fe2843fe2b4e886a0b718a9b8d1132132110618d0d3595da284cd2a4c9d09386199e4f4d7723983d3a374b51cf20dac5cabb4ff7e7197c2ebd9318463409baa583d6a6115c1b768282ff37b0fe152c97671e400d5ccba7d6875df0bf95c5d91257fedb124de393f31908d0e36251326aa29dd5be86291c80b4bf78f419ec151eeaeff643a58b48ab35ad2cd2c0b77b1965966ef3db6b6373cb2c4b590cef2f16f4d6f62f13a6cbf1a481565b5935edd4e76f7b6a8fd0d74bc336b40a803aec38125c006c877dfdcdb9ba2b7aecab5cafe6076e024c73e3567adf97f607a71d180402c22a20a8388f517484cc4198f97c2fe4f3407e0dc577e61f0f71354aa601cf4e3e42e1edd8722d50f5af3441f68caa568cc1c3a19956c1233f265bb47236afab24ee42b27b0042b90693d77c1923147360ae6503f6ba6abbc9dd52a7b4c36a3b6b55f6a80cfa7f101dd9f1bfc7d7eaf09a5d636b510228f245bfb37b4625025d2c911435cdf6f878113753e0804ab8ecab870ad733b9728d7636b17578b41239393e7de47cbce871137d2b61729dda67b2b84cd3363aad64c5dd5bd172f1f091305b1ff78982abe7dab1588036d097cf497e300e6c78a926048febd1b9462c07f5868928357b74297c87f503056b89f786d22a538b6702e290bca04639a0f1d0939b67f409e5e58e472a6a07fa543e2531c2567ec73c41f6769b6ba94c5aa0a030d006f5b6b1c5fb218b86a8f63a48bc867466f20f699859e87956f48a182d26ed451861dd21201ecc7239037ada67319bdf0849c387c73a110af798b4c5f9018bc97993e060ea2a2937fa2eb095d65ec07009fc407a350f1d6fb3c98a0a5f204be985b0cb6962f0eb7844a179c4598a92ea32d2d706c800034d2e960ded5b476d77073316b933fb3e6ba2f4f24a3b73a1e4d8ed1491d757ecf56fd72465dac0000736744d28d29073091587c8bccad302f7054e8a32bb8724974d9f3e449fc70b2a41f0008b548f717ac0a2c3a6580bfb50774933a578ad6acdcb89940bb406ea540893f097d8a88d1609ed605f25499de939083a0c8a7c6db462df5dfa06c298dd233e249433a54267d5cdc22e5524705b7d6b16b96bb2cb83e00cef62e21d91528a74cf95bfd1d391590c93f4058e9bb02656fd087a5b63d738d1c3b5cf533fd59c81cf9136bfcd3e955c19daf9906ef175791fde6a1d98155d7881e241c3522551cf9fcae42e1e46929ea39fd00943446823f9755085ccc8456a3090b73a3031a201d9c704a4ad4868dd9b6d06205560013973f60d637de2f18354bf4523d9d81dc2a7e78cd42c586364bbe0ed86fde0f081f801c1a4abb830839b7796d9a01f141bec8bd93144104c6dc59170162c0a5a639eb63a0a164970de50eb2e04f027394b26ed48d341f7851994df79d7cd663672a556f25e5e16a3adbe1003d631de938fabfed234df12b5ff3027f4a2da823834cb098e0f977a4eb9614579d5c7a1d400a1a933a657aef8ea1a66743d73b0cf37a7d64e9a63e4c7b09945f0db750b311b39783fb5ea216616751967d480a630d3da7c89d1c7beae20369137e96734a4cfedca56a7887f076fe4fe97534ad3e4f74d1a81750581a5ea214b440c7f30331ab86c257534c71175d1e731303a48b01c589fda4fb0d4368b4dd63d91204cb6fc389b2202aa94391907bfb72902a4031f5589ed5f391c2ce92aa998c200ba3c77d8bd747b9d0a29fa85cda3949a6d2bd0c3402e68f98fd451aa27b6c2dfd170e004577cbdb25e3a1b9852e9f66a370789c47bfce722446dade1b32ceae71ee0e1d96edf7ed08a93e3690056f46c3d8e63f88e53673ee71d72cfedbeba493ee91333120e09e9ce9f9c9a7a400f814ea618b1de48f9805e092f4e20f301fbb65caa83735a2a5c89befe4bce4116dca3688e1e14c6f09a945671dedbb5c0ba526842b6cae31d8b5ff978bae928a17a75c134630dd9de988f6ad3d89a071b33775a9660a40b48ec61ad3f93ac81cb1c65d8b0bab5c214786abd13cc10a8ea2e2a370e86e2fa1a372d83c9697b5e37b281e51507685f714fdaebe49ffc93a5582e1936eaee8e4140a4b72"
    @test DSFMT.GF2X(jump25000) == DSFMT.calc_jump(25000)
    @test DSFMT.GF2X(jump1e20)  == DSFMT.calc_jump(big(10)^20)

    # check validity of the implementation of copy(::GF2X)
    let z = big(1); @assert z !== z+0 end

    # test PRNG jump

    function randjumpvec(m, steps, len) # old version of randjump
        mts = accumulate(Future.randjump, fill(steps, len-1); init=m)
        pushfirst!(mts, m)
        mts
    end

    mts = randjumpvec(mta, 25000, size)
    @test length(mts) == 4

    for x in (rand(mts[k], Float64) for j=1:step, k=1:size)
        @test rand(mtb, Float64) == x
    end

    @testset "generated RNGs are in a deterministic state (relatively to ==)" begin
        m = MersenneTwister()
        @test Future.randjump(m, 25000) == Future.randjump(m, 25000)
    end
end

# test that the following is not an error (#16925)
guardseed() do
    Random.seed!(typemax(UInt))
    Random.seed!(typemax(UInt128))
end

# copy, == and hash
let seed = rand(UInt32, 10)
    r = MersenneTwister(seed)
    @test r == MersenneTwister(seed) # r.vals should be all zeros
    @test hash(r) == hash(MersenneTwister(seed))
    s = copy(r)
    @test s == r && s !== r
    @test hash(s) == hash(r)
    skip, len = rand(0:2000, 2)
    for j=1:skip
        rand(r)
        rand(s)
    end
    @test rand(r, len) == rand(s, len)
    @test s == r
    @test hash(s) == hash(r)
    h = rand(UInt)
    @test hash(s, h) == hash(r, h)
end

# MersenneTwister initialization with invalid values
@test_throws DomainError DSFMT.DSFMT_state(zeros(Int32, rand(0:DSFMT.JN32-1)))

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, 10), zeros(UInt128, MT_CACHE_I>>4), 0, 0)

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, MT_CACHE_F), zeros(UInt128, MT_CACHE_I>>4), -1, 0)

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, MT_CACHE_F), zeros(UInt128, MT_CACHE_I>>3), 0, 0)

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, MT_CACHE_F), zeros(UInt128, MT_CACHE_I>>4), 0, -1)

# seed is private to MersenneTwister
let seed = rand(UInt32, 10)
    r = MersenneTwister(seed)
    @test r.seed == seed && r.seed !== seed
    # RNGs do not share their seed in randjump
    let r2 = Future.randjump(r, big(10)^20)
        @test  r.seed !== r2.seed
        Random.seed!(r2)
        @test seed == r.seed != r2.seed
    end
    resize!(seed, 4)
    @test r.seed != seed
end

# Random.seed!(rng, ...) returns rng (#21248)
guardseed() do
    g = Random.default_rng()
    m = MersenneTwister(0)
    @test Random.seed!() === g
    @test Random.seed!(rand(UInt)) === g
    @test Random.seed!(rand(UInt32, rand(1:10))) === g
    @test Random.seed!(m) === m
    @test Random.seed!(m, rand(UInt)) === m
    @test Random.seed!(m, rand(UInt32, rand(1:10))) === m
    @test Random.seed!(m, rand(1:10)) === m
end

# Issue 20062 - ensure internal functions reserve_1, reserve are type-stable
let r = MersenneTwister(0)
    @inferred Random.reserve_1(r)
    @inferred Random.reserve(r, 1)
end

# test randstring API
let b = ['0':'9';'A':'Z';'a':'z']
    for rng = [[], [MersenneTwister(0)]]
        @test length(randstring(rng...)) == 8
        @test length(randstring(rng..., 20)) == 20
        @test issubset(randstring(rng...), b)
        for c = ['a':'z', "qwèrtï", Set(codeunits("gcat"))],
                len = [8, 20]
            s = len == 8 ? randstring(rng..., c) : randstring(rng..., c, len)
            @test length(s) == len
            if eltype(c) == Char
                @test issubset(s, c)
            else # UInt8
                @test issubset(s, Set(Char(v) for v in c))
            end
        end
    end
    @test randstring(MersenneTwister(0)) == randstring(MersenneTwister(0), b)
end

# this shouldn't crash (#22403)
@test_throws ArgumentError rand!(Union{UInt,Int}[1, 2, 3])

@testset "$RNG() & Random.seed!(rng::$RNG) initializes randomly" for RNG in (MersenneTwister, RandomDevice)
    m = RNG()
    a = rand(m, Int)
    m = RNG()
    @test rand(m, Int) != a
    # passing `nothing` is equivalent to passing nothing
    m = RNG(nothing)
    b = rand(m, Int)
    @test b != a
    Random.seed!(m)
    c = rand(m, Int)
    @test c ∉ (a, b)
    Random.seed!(m)
    @test rand(m, Int) ∉ (a, b, c)
    Random.seed!(m, nothing)
    d = rand(m, Int)
    @test d ∉ (a, b, c)
    Random.seed!(m, nothing)
    @test rand(m, Int) ∉ (a, b, c, d)
end

@testset "MersenneTwister($seed) & Random.seed!(m::MersenneTwister, $seed) produce the same stream" for seed in [0:5; 10000:10005]
    m = MersenneTwister(seed)
    a = [rand(m) for _=1:100]
    Random.seed!(m, seed)
    @test a == [rand(m) for _=1:100]
end

struct RandomStruct23964 end
@testset "error message when rand not defined for a type" begin
    @test_throws ArgumentError rand(nothing)
    @test_throws ArgumentError rand(RandomStruct23964())
end

@testset "rand(::$(typeof(RNG)), ::UnitRange{$T}" for RNG ∈ (MersenneTwister(rand(UInt128)), RandomDevice()),
                                                        T ∈ (Int8, Int16, Int32, UInt32, Int64, Int128, UInt128)
    for S in (SamplerRangeInt, SamplerRangeFast, SamplerRangeNDL)
        S == SamplerRangeNDL && sizeof(T) > 8 && continue
        r = T(1):T(108)
        @test rand(RNG, S(r)) ∈ r
        @test rand(RNG, S(typemin(T):typemax(T))) isa T
        a, b = sort!(rand(-1000:1000, 2) .% T)
        @test rand(RNG, S(a:b)) ∈ a:b
    end
end

@testset "rand! is allocation-free" begin
    for A in (Array{Int}(undef, 20), Array{Float64}(undef, 5, 4), BitArray(undef, 20), BitArray(undef, 50, 40))
        rand!(A)
        @test @allocated(rand!(A)) == 0
    end
end

@testset "gentype for UniformBits" begin
    @test Random.gentype(Random.UInt52()) == UInt64
    @test Random.gentype(Random.UInt52(UInt128)) == UInt128
    @test Random.gentype(Random.UInt104()) == UInt128
end

@testset "shuffle[!]" begin
    a = []
    @test shuffle(a) == a # issue #28727
    @test shuffle!(a) === a
    a = rand(Int, 1)
    @test shuffle(a) == a
end

@testset "rand(::Tuple)" begin
    for x in (0x1, 1)
        @test rand((x,)) == 0x1
        @test rand((x, 2)) ∈ 1:2
        @test rand((x, 2, 3)) ∈ 1:3
        @test rand((x, 2, 3, 4)) ∈ 1:4
        @test rand((x, 2, 3, 4, 5)) ∈ 1:5
        @test rand((x, 2, 3, 4, 6)) ∈ 1:6
    end
end

@testset "GLOBAL_RNG" begin
    local GLOBAL_RNG = Random.GLOBAL_RNG
    local LOCAL_RNG = Random.default_rng()
    @test VERSION < v"2" # deprecate this in v2

    @test Random.seed!(GLOBAL_RNG, nothing) === LOCAL_RNG
    @test Random.seed!(GLOBAL_RNG, UInt32[0]) === LOCAL_RNG
    @test Random.seed!(GLOBAL_RNG, 0) === LOCAL_RNG
    @test Random.seed!(GLOBAL_RNG) === LOCAL_RNG

    mt = MersenneTwister(1)
    @test copy!(mt, GLOBAL_RNG) === mt
    @test mt == LOCAL_RNG
    Random.seed!(mt, 2)
    @test mt != LOCAL_RNG
    @test copy!(GLOBAL_RNG, mt) === LOCAL_RNG
    @test mt == LOCAL_RNG
    mt2 = copy(GLOBAL_RNG)
    @test mt2 isa typeof(LOCAL_RNG)
    @test mt2 !== LOCAL_RNG
    @test mt2 == LOCAL_RNG

    for T in (Random.UInt52Raw{UInt64},
              Random.UInt2x52Raw{UInt128},
              Random.UInt104Raw{UInt128},
              Random.CloseOpen12_64)
        x = Random.SamplerTrivial(T())
        @test rand(GLOBAL_RNG, x) === rand(mt, x)
    end
    for T in (Int64, UInt64, Int128, UInt128, Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32)
        x = Random.SamplerType{T}()
        @test rand(GLOBAL_RNG, x) === rand(mt, x)
    end

    A = fill(0.0, 100, 100)
    B = fill(1.0, 100, 100)
    vA = view(A, :, :)
    vB = view(B, :, :)
    I1 = Random.SamplerTrivial(Random.CloseOpen01{Float64}())
    I2 = Random.SamplerTrivial(Random.CloseOpen12{Float64}())
    @test rand!(GLOBAL_RNG, A, I1) === A == rand!(mt, B, I1) === B
    B = fill!(B, 1.0)
    @test rand!(GLOBAL_RNG, vA, I1) === vA
    rand!(mt, vB, I1)
    @test A == B
    for T in (Float16, Float32)
        B = fill!(B, 1.0)
        @test rand!(GLOBAL_RNG, A, I2) === A == rand!(mt, B, I2) === B
        B = fill!(B, 1.0)
        @test rand!(GLOBAL_RNG, A, I1) === A == rand!(mt, B, I1) === B
    end
    for T in Base.BitInteger_types
        x = Random.SamplerType{T}()
        B = fill!(B, 1.0)
        @test rand!(GLOBAL_RNG, A, x) === A == rand!(mt, B, x) === B
    end
    # issue #33170
    @test Sampler(GLOBAL_RNG, 2:4, Val(1)) isa SamplerRangeNDL
    @test Sampler(GLOBAL_RNG, 2:4, Val(Inf)) isa SamplerRangeNDL
end

@testset "RNGs broadcast as scalars: T" for T in (MersenneTwister, RandomDevice)
    @test length.(rand.(T(), 1:3)) == 1:3
end
