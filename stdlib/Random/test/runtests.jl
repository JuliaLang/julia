# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SparseArrays
using Test: guardseed

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays

using Random
using Random.DSFMT

using Random: default_rng, Sampler, SamplerRangeFast, SamplerRangeInt, SamplerRangeNDL,
    SeedHasher, MT_CACHE_F, MT_CACHE_I

import SHA

include("jump.jl")

function test_uniform(xs::AbstractArray{T}) where {T<:AbstractFloat}
    # TODO: refine
    prec = isempty(xs) ? precision(T) : precision(first(xs))
    proba_nocollision = prod((1.0 - i/2.0^prec for i=1:length(xs)-1), init=1.0) # rough estimate
    xsu = Set(xs)
    if (1.0 - proba_nocollision) < 2.0^-64
        @test length(xsu) == length(xs)
    elseif prec > 52 && length(xs) < 3000
        # if proba of collisions is high enough, allow at most one collision;
        # with the constraints on precision and length, more than one collision would happen
        # with proba less than 2.0^-62
        @test length(xsu) >= length(xs)-1
    end
    @test all(x -> zero(x) <= x < one(x), xs)
end

function test_uniform(xs::AbstractArray{T}) where {T<:Base.BitInteger}
    # TODO: refine
    prec = 8*sizeof(T)
    proba_nocollision = prod((1.0 - i/2.0^prec for i=1:length(xs)-1), init=1.0)
    xsu = Set(xs)
    if (1.0 - proba_nocollision) < 2.0^-64
        @test length(xsu) == length(xs)
    elseif prec > 52 && length(xs) < 3000
        @test length(xsu) >= length(xs)-1
    end
end


@testset "MersenneTwister: do not do update the same global state in incompatible ways" begin
    # Issue #6573
    mm = MersenneTwister(rand(UInt128))
    rand(mm)
    xs = rand(mm, 384)
    @test rand(mm) ∉ xs
    test_uniform(xs)
end

@testset "rand from AbstractArray" begin
    seed = rand(UInt128)
    for rng ∈ ([MersenneTwister(seed)], [Xoshiro(seed)], [])
        # issue 8257
        i8257 = 1:1/3:100
        for _ = 1:100
            @test rand(rng... ,i8257) in i8257
        end

        @test rand(rng..., 0:3:1000) in 0:3:1000
        @test issubset(rand!(rng..., Vector{Int}(undef, 100), 0:3:1000), 0:3:1000)
        coll = Any[2, UInt128(128), big(619), "string"]
        @test rand(rng..., coll) in coll
        @test issubset(rand(rng..., coll, 2, 3), coll)

        for T in (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt,
                  Float16, Float32, Float64, Rational{Int})
            r = rand(rng..., convert(T, 97):convert(T, 122))
            @test typeof(r) == T
            @test 97 <= r <= 122
            r = rand(rng..., convert(T, 97):convert(T,2):convert(T, 122),2)[1]
            @test typeof(r) == T
            @test 97 <= r <= 122
            @test mod(r,2)==1

            if T<:Integer && !(T===BigInt)
                x = rand(rng..., typemin(T):typemax(T))
                @test isa(x,T)
                @test typemin(T) <= x <= typemax(T)
            end
        end
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

@testset "test code paths of rand!(::MersenneTwister)" begin
    mt = MersenneTwister(rand(UInt128))
    A128 = UInt128[]
    @test length(rand!(mt, A128)) == 0
    for (i, n) in enumerate([1, 3, 5, 6, 10, 11, 30])
        resize!(A128, n)
        rand!(mt, A128)
        @test length(A128) == n
        test_uniform(A128)
    end

    for (i, T) in enumerate([Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, Float16, Float32])
        A = Vector{T}(undef, 16)
        B = Vector{T}(undef, 31)
        rand!(mt, A)
        rand!(mt, B)
        @test length(A) == 16
        @test length(B) == 31
        test_uniform(A)
        test_uniform(B)
    end

    AF64 = Vector{Float64}(undef, Random.dsfmt_get_min_array_size()-1)
    rand!(mt, AF64)
    test_uniform(AF64)
    resize!(AF64, 2*length(mt.vals))
    invoke(rand!, Tuple{MersenneTwister,AbstractArray{Float64},Random.SamplerTrivial{Random.CloseOpen01_64}},
           mt, AF64, Random.SamplerTrivial(Random.CloseOpen01()))
    test_uniform(AF64)
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
        test_uniform(A)
    end
end

# make sure reading 128-bit ints from RandomDevice works
let a = [rand(RandomDevice(), UInt128) for i=1:10]
    @test reduce(|, a)>>>64 != 0
end

# wrapper around Float64 to check fallback random generators
struct FakeFloat64 <: AbstractFloat
    x::Float64
end
Base.rand(rng::AbstractRNG, ::Random.SamplerTrivial{Random.CloseOpen01{FakeFloat64}}) = FakeFloat64(rand(rng))
for f in (:sqrt, :log, :log1p, :one, :zero, :abs, :+, :-)
    @eval Base.$f(x::FakeFloat64) = FakeFloat64($f(x.x))
end
for f in (:+, :-, :*, :/)
    @eval begin
        Base.$f(x::FakeFloat64, y::FakeFloat64) = FakeFloat64($f(x.x,y.x))
        Base.$f(x::FakeFloat64, y::Real) = FakeFloat64($f(x.x,y))
        Base.$f(x::Real, y::FakeFloat64) = FakeFloat64($f(x,y.x))
    end
end
for f in (:<, :<=, :>, :>=, :(==), :(!=))
    @eval begin
        Base.$f(x::FakeFloat64, y::FakeFloat64) = $f(x.x,y.x)
        Base.$f(x::FakeFloat64, y::Real) = $f(x.x,y)
        Base.$f(x::Real, y::FakeFloat64) = $f(x,y.x)
    end
end

# test all rand APIs
for rng in ([], [MersenneTwister(0)], [RandomDevice()], [Xoshiro(0)], [SeedHasher(0)])
    realrng = rng == [] ? default_rng() : only(rng)
    ftypes = [Float16, Float32, Float64, FakeFloat64, BigFloat]
    cftypes = [ComplexF16, ComplexF32, ComplexF64, ftypes...]
    types = [Bool, Char, BigFloat, Tuple{Bool, Tuple{Int, Char}}, Pair{Int8, UInt32},
             Base.BitInteger_types..., cftypes...]
    randset = Set(rand(Int, 20))
    randdict = Dict(zip(rand(Int,10), rand(Int, 10)))

    randwidetup = Tuple{Bool, Char, Vararg{Tuple{Int, Float64}, 14}}
    @inferred rand(rng..., randwidetup)

    collections = [BitSet(rand(1:100, 20))          => Int,
                   randset                          => Int,
                   GenericSet(randset)              => Int,
                   randdict                         => Pair{Int,Int},
                   keys(randdict)                   => Int,
                   values(randdict)                 => Int,
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
        f1 = f(rng...)                     ::Float64
        f2 = f(rng..., 5)                  ::Vector{Float64}
        f3 = f(rng..., 2, 3)               ::Array{Float64, 2}
        f4 = f(rng..., b2, u3)             ::Array{Float64, 2}
        @test size(f1) == ()
        @test size(f2) == (5,)
        @test size(f3) == size(f4) == (2, 3)
        for T in functypes[f]
            tts = f == rand ? (T, Sampler(realrng, T, Val(1)), Sampler(realrng, T, Val(Inf))) : (T,)
            for tt in tts
                a0 = f(rng..., tt)         ::T
                a1 = f(rng..., tt, 5)      ::Vector{T}
                a2 = f(rng..., tt, 2, 3)   ::Array{T, 2}
                a3 = f(rng..., tt, b2, u3) ::Array{T, 2}
                a4 = f(rng..., tt, (2, 3)) ::Array{T, 2}
                if T <: Number
                    @test size(a0) == ()
                end
                @test size(a1) == (5,)
                @test size(a2) == size(a3) == size(a4) == (2, 3)
                if T <: AbstractFloat && f === rand
                    for a in T[a0, a1..., a2..., a3..., a4...]
                        @test 0.0 <= a < 1.0
                    end
                end
            end
        end
    end
    for (C, T) in collections
        for cc = (C, Sampler(realrng, C, Val(1)), Sampler(realrng, C, Val(Inf)))
            a0  = rand(rng..., cc)                                               ::T
            a1  = rand(rng..., cc, 5)                                            ::Vector{T}
            a2  = rand(rng..., cc, 2, 3)                                         ::Array{T, 2}
            a3  = rand(rng..., cc, (2, 3))                                       ::Array{T, 2}
            a4  = rand(rng..., cc, b2, u3)                                       ::Array{T, 2}
            a5  = rand!(rng..., Array{T}(undef, 5), cc)                          ::Vector{T}
            a6  = rand!(rng..., Array{T}(undef, 2, 3), cc)                       ::Array{T, 2}
            a7  = rand!(rng..., GenericArray{T}(undef, 5), cc)                   ::GenericArray{T, 1}
            a8  = rand!(rng..., GenericArray{T}(undef, 2, 3), cc)                ::GenericArray{T, 2}
            a9  = rand!(rng..., OffsetArray(Array{T}(undef, 5), 9), cc)          ::OffsetArray{T, 1}
            a10 = rand!(rng..., OffsetArray(Array{T}(undef, 2, 3), (-2, 4)), cc) ::OffsetArray{T, 2}
            a11 = rand!(rng..., Memory{T}(undef, 5), cc)                         ::Memory{T}
            @test size(a1) == (5,)
            @test size(a2) == size(a3) == (2, 3)
            for a in [a0, a1..., a2..., a3..., a4..., a5..., a6..., a7..., a8..., a9..., a10..., a11...]
                if C isa Type
                    @test a isa C
                else
                    @test a in C
                end
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
            (T <: Tuple || T <: Pair) && continue
            X = T == Bool ? T[0,1] : T[0,1,2]
            for A in (Vector{T}(undef, 5),
                      Memory{T}(undef, 5),
                      Matrix{T}(undef, 2, 3),
                      GenericArray{T}(undef, 5),
                      GenericArray{T}(undef, 2, 3),
                      OffsetArray(Array{T}(undef, 5), -3),
                      OffsetArray(Array{T}(undef, 2, 3), (4, 5)))
                local A
                A2 = f!(rng..., A)               ::typeof(A)
                @test A2 === A
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

    z1 = bitrand(rng..., 5)             ::BitArray{1}
    @test size(z1) == (5,)
    z2 = bitrand(rng..., 2, 3)          ::BitArray{2}
    @test size(z2) == (2, 3)
    z3 = bitrand(rng..., b2, u3)        ::BitArray{2}
    @test size(z3) == (b2, u3)
    z4 = rand!(rng..., BitVector(undef, 5))     ::BitArray{1}
    @test size(z4) == (5,)
    z5 = rand!(rng..., BitMatrix(undef, 2, 3))  ::BitArray{2}
    @test size(z5) == (2, 3)

    # Test that you cannot call randn or randexp with non-Float types.
    for r in [randn, randexp]
        @test_throws MethodError r(Int)
        @test_throws MethodError r(Int32)
        @test_throws MethodError r(Bool)
        @test_throws MethodError r(String)
        @test_throws MethodError r(AbstractFloat)

        @test_throws MethodError r(Int64, (2,3))
        @test_throws MethodError r(String, 1)

        @test_throws MethodError r(rng..., Number, (2,3))
        @test_throws MethodError r(rng..., Any, 1)
    end

    # Test that you cannot call rand with a tuple type of unknown size or with isbits parameters
    @test_throws ArgumentError rand(rng..., Tuple{Vararg{Int}})
    @test_throws TypeError rand(rng..., Tuple{1:2})
end

function hist(X, n)
    v = zeros(Int, n)
    for x in X
        v[floor(Int, x*n) + 1] += 1
    end
    v
end

@testset "uniform distribution of floats" begin
    seed = rand(UInt128)
    for rng in [MersenneTwister(seed), RandomDevice(), Xoshiro(seed), SeedHasher(seed)],
        T in [Float16, Float32, Float64, BigFloat],
        prec in (T == BigFloat ? [3, 53, 64, 100, 256, 1000] : [256])

        setprecision(BigFloat, prec) do
            if precision(T) >= precision(Float32)
                @test rand(rng, T) != rand(rng, T)
            end
            # array version
            counts = hist(rand(rng, T, 2000), 4)
            @test minimum(counts) > 300 # should fail with proba < 1e-26
            # scalar version
            counts = hist([rand(rng, T) for i in 1:2000], 4)
            @test minimum(counts) > 300
        end
    end
end

@testset "rand(Bool) uniform distribution" begin
    for n in [rand(1:8), rand(9:16), rand(17:64)]
        a = zeros(Bool, n)
        a8 = unsafe_wrap(Array, Ptr{UInt8}(pointer(a)), length(a); own=false) # unsafely observe the actual bit patterns in `a`
        as = zeros(Int, n)
        # we will test statistical properties for each position of a,
        # but also for 3 linear combinations of positions (for the array version)
        lcs = unique!.([rand(1:n, 2), rand(1:n, 3), rand(1:n, 5)])
        aslcs = zeros(Int, 3)
        seed = rand(UInt128)
        for rng = (MersenneTwister(seed), RandomDevice(), Xoshiro(seed), SeedHasher(seed))
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
                        @test all(x -> x === 0x00 || x === 0x01, a8)
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

@testset "reproducility of methods for $RNG" for RNG=(MersenneTwister,Xoshiro)
    mta, mtb = RNG(42), RNG(42)

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
    @test shuffle!(mta, rand(mta, Bool, 2, 3)) == shuffle!(mtb, rand(mtb, Bool, 2, 3))
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

@testset "copy, == and hash" begin
    for RNG = (MersenneTwister, Xoshiro)
        seed = rand(UInt32, 10)
        r = RNG(seed)
        t = RNG(seed)
        @test r == t
        @test hash(r) == hash(t)
        s = copy(r)
        @test s == r == t && s !== r
        @test hash(s) == hash(r)
        skip, len = rand(0:2000, 2)
        for j=1:skip
            rand(r)
            @test r != s
            @test hash(r) != hash(s)
            rand(s)
        end
        @test rand(r, len) == rand(s, len)
        @test s == r
        @test hash(s) == hash(r)
        h = rand(UInt)
        @test hash(s, h) == hash(r, h)
        if RNG == Xoshiro
            t = copy(TaskLocalRNG())
            @test hash(t) == hash(TaskLocalRNG())
            @test hash(t, h) == hash(TaskLocalRNG(), h)
            x = rand()
            @test hash(t) != hash(TaskLocalRNG())
            @test rand(t) == x
            @test hash(t) == hash(TaskLocalRNG())
            copy!(TaskLocalRNG(), r)
            @test hash(TaskLocalRNG()) == hash(r)
            @test TaskLocalRNG() == r
        end
    end
end

# MersenneTwister initialization with invalid values
@test_throws DomainError DSFMT.DSFMT_state(zeros(Int32, rand(0:DSFMT.JN32-1)))

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, 10), zeros(UInt128, MT_CACHE_I>>4), 0, 0, 0, 0, -1, -1)

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, MT_CACHE_F), zeros(UInt128, MT_CACHE_I>>4), -1, 0, 0, 0, -1, -1)

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, MT_CACHE_F), zeros(UInt128, MT_CACHE_I>>3), 0, 0, 0, 0, -1, -1)

@test_throws DomainError MersenneTwister(zeros(UInt32, 1), DSFMT.DSFMT_state(),
                                         zeros(Float64, MT_CACHE_F), zeros(UInt128, MT_CACHE_I>>4), 0, -1, 0, 0, -1, -1)

# seed is private to MersenneTwister
let seed = rand(UInt32, 10)
    r = MersenneTwister(seed)
    @test r.seed == seed && r.seed !== seed
    let r2 = Random.jump(r)
        Random.seed!(r2)
        @test seed == r.seed != r2.seed
    end
    resize!(seed, 4)
    @test r.seed != seed
end

@testset "Random.seed!(rng, ...) returns rng" begin
    # issue #21248
    seed = rand(UInt)
    for m = ([MersenneTwister(seed)], [Xoshiro(seed)], [SeedHasher(seed)], [])
        m2 = m == [] ? default_rng() : m[1]
        @test Random.seed!(m...) === m2
        @test Random.seed!(m..., rand(UInt)) === m2
        @test Random.seed!(m..., rand(UInt32, rand(1:10))) === m2
        @test Random.seed!(m..., rand(1:10)) === m2
        # Try a seed larger than 2^32
        @test Random.seed!(m..., 5294967296) === m2

        # test that the following is not an error (#16925)
        @test Random.seed!(m..., typemax(UInt)) === m2
        @test Random.seed!(m..., typemax(UInt128)) === m2
        @test Random.seed!(m..., "a random seed") === m2
        @test Random.seed!(m..., Random.default_rng()) === m2
    end
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
@test_throws MethodError rand!(Union{UInt,Int}[1, 2, 3])

@testset "$RNG() & Random.seed!(rng::$RNG) initializes randomly" for RNG in (MersenneTwister, RandomDevice, Xoshiro, SeedHasher)
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

@testset "$RNG(seed) & Random.seed!(m::$RNG, seed) produce the same stream" for RNG=(MersenneTwister, Xoshiro, SeedHasher)
    seeds = Any[0, 1, 2, 10000, 10001, rand(UInt32, 8), randstring(), randstring(), rand(UInt128, 3)...]
    if RNG == Xoshiro
        push!(seeds, rand(UInt64, rand(1:4)))
    end
    for seed=seeds
        m = RNG(seed)
        a = [rand(m) for _=1:100]
        Random.seed!(m, seed)
        @test a == [rand(m) for _=1:100]
    end
    # rng as a seed
    m = RNG(Xoshiro(0))
    a = [rand(m) for _=1:100]
    Random.seed!(m, Xoshiro(0))
    @test a == [rand(m) for _=1:100]
end

@testset "Random.seed!(seed) sets Random.GLOBAL_SEED" begin
    seeds = Any[0, rand(UInt128), rand(UInt64, 4), randstring(20)]

    for seed=seeds
        Random.seed!(seed)
        @test Random.get_tls_seed() == default_rng()
    end

    for ii = 1:8
        iseven(ii) ? Random.seed!(nothing) : Random.seed!()
        push!(seeds, copy(Random.get_tls_seed()))
        @test Random.get_tls_seed() isa Xoshiro # could change, but must not be nothing
    end
    @test allunique(seeds)
end

struct RandomStruct23964 end
@testset "error message when rand not defined for a type" begin
    @test_throws MethodError rand(nothing)
    @test_throws MethodError rand(RandomStruct23964())
end

@testset "rand(::$(typeof(RNG)), ::UnitRange{$T}" for RNG ∈ (MersenneTwister(rand(UInt128)),
                                                             RandomDevice(),
                                                             Xoshiro(rand(UInt128)),
                                                             SeedHasher(rand(UInt128))),
                                                        T ∈ (Bool, Int8, Int16, Int32, UInt32, Int64, Int128, UInt128)
    if T === Bool
        @test rand(RNG, false:true) ∈ (false, true)
        @test rand(RNG, false:false) === false
        @test rand(RNG, true:true) === true
        @test_throws ArgumentError rand(RNG, true:false)
        continue
    end
    for S in (identity, SamplerRangeInt, SamplerRangeFast, SamplerRangeNDL)
        if T === Int32 && RNG isa MersenneTwister
            @test minimum([rand(RNG, T(1):T(7^7)) for i = 1:100000]) > 0
        end

        (S == SamplerRangeNDL || S == identity) && sizeof(T) > 8 && continue
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

@testset "rand(::Type{<:Tuple})" begin
    @test_throws ArgumentError rand(Tuple)
    @test rand(Tuple{}) == ()
    @inferred rand(Tuple{Int32,Int64,Float64})
    @inferred rand(NTuple{20,Int})
    @test_throws TypeError rand(Tuple{1:2,3:4})

    @testset "rand(::RandomDevice, ::Type{NTuple{N, Int}})" begin
        # RandomDevice has a specialization for homogeneous tuple types of builtin integers
        rd = RandomDevice()
        @test () == rand(rd, Tuple{})
        xs = rand(rd, Tuple{Int, Int})
        @test xs isa Tuple{Int, Int} && xs[1] != xs[2]
        xs = rand(rd, NTuple{2, Int})
        @test xs isa Tuple{Int, Int} && xs[1] != xs[2]
        xs = rand(rd, Tuple{Int, UInt}) # not NTuple
        @test xs isa Tuple{Int, UInt} && xs[1] != xs[2]
        xs = rand(rd, Tuple{Bool}) # not included in the specialization
        @test xs isa Tuple{Bool}
    end
end

@testset "GLOBAL_RNG" begin
    @test VERSION < v"2" # deprecate this in v2 (GLOBAL_RNG must go)
    local GLOBAL_RNG = Random.GLOBAL_RNG
    local LOCAL_RNG = Random.default_rng()

    @test Random.seed!(GLOBAL_RNG, nothing) === LOCAL_RNG
    @test Random.seed!(GLOBAL_RNG, UInt32[0]) === LOCAL_RNG
    @test Random.seed!(GLOBAL_RNG, 0) === LOCAL_RNG
    @test Random.seed!(GLOBAL_RNG) === LOCAL_RNG

    xo = Xoshiro()
    @test copy!(xo, GLOBAL_RNG) === xo
    @test xo == LOCAL_RNG
    Random.seed!(xo, 2)
    @test xo != LOCAL_RNG
    @test copy!(GLOBAL_RNG, xo) === LOCAL_RNG
    @test xo == LOCAL_RNG
    xo2 = copy(GLOBAL_RNG)
    @test xo2 !== LOCAL_RNG
    @test xo2 == LOCAL_RNG

    for T in (Random.UInt52Raw{UInt64},
              Random.UInt104Raw{UInt128},
              Random.CloseOpen12_64)
        x = Random.SamplerTrivial(T())
        @test rand(GLOBAL_RNG, x) === rand(xo, x)
    end
    for T in (Int64, UInt64, Int128, UInt128, Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32)
        x = Random.SamplerType{T}()
        @test rand(GLOBAL_RNG, x) === rand(xo, x)
    end

    A = fill(0.0, 100, 100)
    B = fill(1.0, 100, 100)
    vA = view(A, :, :)
    vB = view(B, :, :)
    I1 = Random.SamplerTrivial(Random.CloseOpen01{Float64}())
    I2 = Random.SamplerTrivial(Random.CloseOpen12{Float64}())
    @test rand!(GLOBAL_RNG, A, I1) === A == rand!(xo, B, I1) === B
    B = fill!(B, 1.0)
    @test rand!(GLOBAL_RNG, vA, I1) === vA
    rand!(xo, vB, I1)
    @test A == B
    for T in (Float16, Float32)
        B = fill!(B, 1.0)
        @test rand!(GLOBAL_RNG, A, I2) === A == rand!(xo, B, I2) === B
        B = fill!(B, 1.0)
        @test rand!(GLOBAL_RNG, A, I1) === A == rand!(xo, B, I1) === B
    end
    for T in Base.BitInteger_types
        x = Random.SamplerType{T}()
        B = fill!(B, 1.0)
        @test rand!(GLOBAL_RNG, A, x) === A == rand!(xo, B, x) === B
    end
    # issue #33170
    @test Sampler(GLOBAL_RNG, 2:4, Val(1)) isa SamplerRangeNDL
    @test Sampler(GLOBAL_RNG, 2:4, Val(Inf)) isa SamplerRangeNDL

    rng = copy(GLOBAL_RNG)
    # make sure _GLOBAL_RNG and the underlying implementation use the same code path
    @test rand(rng) == rand(GLOBAL_RNG)
    @test rand(rng) == rand(GLOBAL_RNG)
    @test rand(rng) == rand(GLOBAL_RNG)
    @test rand(rng) == rand(GLOBAL_RNG)
end

@testset "RNGs broadcast as scalars: $(typeof(RNG))" for RNG in (MersenneTwister(0),
                                                                 RandomDevice(),
                                                                 Xoshiro(0),
                                                                 SeedHasher(0))
    @test length.(rand.(RNG, 1:3)) == 1:3
end

@testset "generated scalar integers do not overlap" begin
    m = MersenneTwister()
    xs = reinterpret(UInt64, m.ints)
    x = rand(m, UInt128)  # m.idxI % 16 == 0
    @test x % UInt64 == xs[end-1]
    x = rand(m, UInt64)
    @test x == xs[end-2]
    x = rand(m, UInt64)
    @test x == xs[end-3]
    x = rand(m, UInt64)
    @test x == xs[end-4]
    x = rand(m, UInt128) # m.idxI % 16 == 8
    @test (x >> 64) % UInt64 == xs[end-6]
    @test x % UInt64 == xs[end-7]
    x = rand(m, UInt64)
    @test x == xs[end-8]
    @test x != xs[end-7]

    s = Set{UInt64}()
    n = 0
    for _=1:2000
        x = rand(m, rand((UInt64, UInt128, Int64, Int128)))
        if sizeof(x) == 8
            push!(s, x % UInt64)
            n += 1
        else
            push!(s, x % UInt64, (x >> 64) % UInt64)
            n += 2
        end
    end
    @test length(s) == n
end

@testset "show" begin
    @testset "MersenneTwister" begin
        m = MersenneTwister(123)
        @test string(m) == "MersenneTwister(123)"
        Random.jump!(m, by=2*big(10)^20)
        @test string(m) == "MersenneTwister(123, (200000000000000000000, 0))"
        @test m == MersenneTwister(123, (200000000000000000000, 0))
        rand(m)
        @test string(m) == "MersenneTwister(123, (200000000000000000000, 1002, 0, 1))"

        @test m == MersenneTwister(123, (200000000000000000000, 1002, 0, 1))
        rand(m, Int64)
        @test string(m) == "MersenneTwister(123, (200000000000000000000, 2256, 0, 1, 1002, 1))"
        @test m == MersenneTwister(123, (200000000000000000000, 2256, 0, 1, 1002, 1))

        m = MersenneTwister(0x0ecfd77f89dcd508caa37a17ebb7556b)
        @test string(m) == "MersenneTwister(0x0ecfd77f89dcd508caa37a17ebb7556b)"
        rand(m, Int64)
        @test string(m) == "MersenneTwister(0x0ecfd77f89dcd508caa37a17ebb7556b, (0, 1254, 0, 0, 0, 1))"
        @test m == MersenneTwister(0xecfd77f89dcd508caa37a17ebb7556b, (0, 1254, 0, 0, 0, 1))

        m = MersenneTwister(0); rand(m, Int64); rand(m)
        @test string(m) == "MersenneTwister(0, (0, 2256, 1254, 1, 0, 1))"
        @test m == MersenneTwister(0, (0, 2256, 1254, 1, 0, 1))

        # negative seeds
        Random.seed!(m, -3)
        @test string(m) == "MersenneTwister(-3)"
        Random.seed!(m, typemin(Int8))
        @test string(m) == "MersenneTwister(-128)"

        # string seeds
        Random.seed!(m, "seed 1")
        @test string(m) == "MersenneTwister(\"seed 1\")"
        x = rand(m)
        @test x == rand(MersenneTwister("seed 1"))
        @test string(m) == """MersenneTwister("seed 1", (0, 1002, 0, 1))"""
        # test that MersenneTwister's fancy constructors accept string seeds
        @test MersenneTwister("seed 1", (0, 1002, 0, 1)) == m
    end

    @testset "RandomDevice" begin
        @test string(RandomDevice()) == "$RandomDevice()"
    end
end

@testset "rand[!] for BigInt/BigFloat" begin
    rng = MersenneTwister()
    s = Random.SamplerBigInt(MersenneTwister, 1:big(9))
    x = rand(s)
    @test x isa BigInt
    y = rand!(rng, x, s)
    @test y === x
    @test x in 1:9

    for t = BigInt[0, 10, big(2)^100]
        s = Random.Sampler(rng, t:t) # s.nlimbs == 0
        @test rand(rng, s) == t
        @test x === rand!(rng, x, s) == t

        s = Random.Sampler(rng, big(-1):t) # s.nlimbs != 0
        @test rand(rng, s) ∈ -1:t
        @test x === rand!(rng, x, s) ∈ -1:t

    end

    s = Random.Sampler(MersenneTwister, Random.CloseOpen01(BigFloat))
    x = rand(s)
    @test x isa BigFloat
    y = rand!(rng, x, s)
    @test y === x
    @test 0 <= x < 1
    s = Random.Sampler(MersenneTwister, Random.CloseOpen12(BigFloat))
    y = rand!(rng, x, s)
    @test y === x
    @test 1 <= x < 2

    old_prec = precision(BigFloat)
    setprecision(100) do
        x = rand(s) # should use precision of s
        @test precision(x) == old_prec
        x = BigFloat()
        @test_throws ArgumentError rand!(rng, x, s) # incompatible precision
    end
    s = setprecision(100) do
        Random.Sampler(MersenneTwister, Random.CloseOpen01(BigFloat))
    end
    x = rand(s) # should use precision of s
    @test precision(x) == 100
    x = BigFloat()
    @test_throws ArgumentError rand!(rng, x, s) # incompatible precision
end

@testset "shuffle! for BitArray" begin
    # Test that shuffle! is uniformly random on BitArrays
    rng = MersenneTwister(123)
    a = (reshape(1:(4*5), 4, 5) .<= 2) # 4x5 BitMatrix whose first two elements are true, rest are false
    m = sum(1:50_000) do _
        shuffle!(rng, a)
    end/50_000 # mean result of shuffle!-ing a 50_000 times. If the shuffle! is uniform, then each index has a
    # 10% chance of having a true in it, so each value should converge to 0.1.
    @test minimum(m) >= 0.094
    @test maximum(m) <= 0.106
end

# issue #42752
# test that running finalizers that launch tasks doesn't change RNG stream
function f42752(do_gc::Bool, cell = (()->Any[[]])())
    a = rand()
    if do_gc
        finalizer(cell[1]) do _
            @async nothing
        end
        cell[1] = nothing
        GC.gc()
    end
    b = rand()
    (a, b)
end
guardseed() do
    for _ in 1:4
        Random.seed!(1)
        val = f42752(false)
        Random.seed!(1)
        @test f42752(true) === val
    end
end

@testset "TaskLocalRNG: stream collision smoke test" begin
    # spawn a trinary tree of tasks:
    # - spawn three recursive child tasks in each
    # - generate a random UInt64 in each before, after and between
    # - collect and count all the generated random values
    # these should all be distinct across all tasks
    function gen(d)
        r = rand(UInt64)
        vals = [r]
        if d ≥ 0
            append!(vals, gent(d - 1))
            isodd(r) && append!(vals, gent(d - 1))
            push!(vals, rand(UInt64))
            iseven(r) && append!(vals, gent(d - 1))
        end
        push!(vals, rand(UInt64))
    end
    gent(d) = fetch(@async gen(d))
    seeds = rand(RandomDevice(), UInt64, 5)
    for seed in seeds
        Random.seed!(seed)
        vals = gen(6)
        @test allunique(vals)
    end
end

@testset "TaskLocalRNG: child doesn't affect parent" begin
    seeds = rand(RandomDevice(), UInt64, 5)
    for seed in seeds
        Random.seed!(seed)
        x = rand(UInt64)
        y = rand(UInt64)
        n = 3
        for i = 1:n
            Random.seed!(seed)
            @sync for j = 0:i
                @async rand(UInt64)
            end
            @test x == rand(UInt64)
            @sync for j = 0:(n-i)
                @async rand(UInt64)
            end
            @test y == rand(UInt64)
        end
    end
end

@testset "TaskLocalRNG: copy and copy! handle the splitmix state" begin
    seeds = rand(RandomDevice(), UInt64, 5)
    for seed in seeds
        Random.seed!(seed)
        rng1 = copy(TaskLocalRNG())
        x = fetch(@async rand(UInt64))
        rng2 = copy(TaskLocalRNG())
        y = fetch(@async rand(UInt64))
        rng3 = copy(TaskLocalRNG())
        @test x != y
        @test rng1 != rng2
        Random.seed!(seed)
        @test TaskLocalRNG() == rng1
        @test x == fetch(@async rand(UInt64))
        @test TaskLocalRNG() == rng2
        # this should be a no-op:
        copy!(TaskLocalRNG(), copy(TaskLocalRNG()))
        @test TaskLocalRNG() == rng2
        @test y == fetch(@async rand(UInt64))
        @test TaskLocalRNG() == rng3
    end
end

@testset "seed! and hash_seed" begin
    function hash_seed(seed)
        ctx = SHA.SHA2_256_CTX()
        Random.hash_seed(seed, ctx)
        bytes2hex(SHA.digest!(ctx))
    end

    # Test that:
    # 1) if n == m, then hash_seed(n) == hash_seed(m)
    # 2) if n != m, then hash_seed(n) != hash_seed(m)
    rngs = (Xoshiro(0), TaskLocalRNG(), MersenneTwister(0))
    seeds = Any[]
    for T = Base.BitInteger_types
        append!(seeds, rand(T, 8))
        push!(seeds, typemin(T), typemin(T) + T(1), typemin(T) + T(2),
              typemax(T), typemax(T) - T(1), typemax(T) - T(2))
        T <: Signed && push!(seeds, T(0), T(1), T(2), T(-1), T(-2))
    end

    vseeds = Dict{String, BigInt}()
    for seed = seeds
        bigseed = big(seed)
        vseed = hash_seed(bigseed)
        # test property 1) above
        @test hash_seed(seed) == vseed
        # test property 2) above
        @test bigseed == get!(vseeds, vseed, bigseed)
        # test that the property 1) is actually inherited by `seed!`
        for rng = rngs
            rng2 = copy(Random.seed!(rng, seed))
            Random.seed!(rng, bigseed)
            @test rng == rng2
        end
    end

    seed32 = rand(UInt32, rand(1:9))
    hash32 = hash_seed(seed32)
    @test hash_seed(map(UInt64, seed32)) == hash32
    @test hash32 ∉ keys(vseeds)

    seed_str = randstring()
    seed_gstr = GenericString(seed_str)
    @test hash_seed(seed_str) == hash_seed(seed_gstr)
    string_seeds = Set{String}()
    for ch = 'A':'z'
        vseed = hash_seed(string(ch))
        @test vseed ∉ keys(vseeds)
        @test vseed ∉ string_seeds
        push!(string_seeds, vseed)
    end
end

@testset "rand(::Type{<:Pair})" begin
    @test rand(Pair{Int, Int}) isa Pair{Int, Int}
    @test rand(Pair{Int, Float64}) isa Pair{Int, Float64}
    @test rand(Pair{Int, Float64}, 3) isa Array{Pair{Int, Float64}}

    # test that making an array out of a sampler works
    # (i.e. that gentype(sp) is correct)
    sp = Random.Sampler(AbstractRNG, Pair{Bool, Char})
    xs = rand(sp, 3)
    @test xs isa Vector{Pair{Bool, Char}}
    @test length(xs) == 3
end

@testset "Float32 RNG typo" begin
    for T in (Float16, Float32, Float64)
        # Make sure generated numbers are sufficiently diverse
        # for both SIMD and non-SIMD RNG code paths for all types.
        @test length(unique!(rand(T, 7))) > 3
        @test length(unique!(rand(T, 14))) > 10
        @test length(unique!(rand(T, 34))) > 20
    end
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(Random))
end
