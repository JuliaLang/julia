# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, SparseArrays
using Test: guardseed

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays

using Random
using Random.DSFMT

using Random: default_rng, Sampler, SamplerRangeFast, SamplerRangeInt, SamplerRangeNDL, MT_CACHE_F, MT_CACHE_I
using Random: jump_128, jump_192, jump_128!, jump_192!

import Future # randjump

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
for rng in ([], [MersenneTwister(0)], [RandomDevice()], [Xoshiro()])
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
    for rng in [MersenneTwister(), RandomDevice(), Xoshiro()],
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
        for rng = (MersenneTwister(), RandomDevice(), Xoshiro())
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
    let r2 = Future.randjump(r, big(10)^20)
        Random.seed!(r2)
        @test seed == r.seed != r2.seed
    end
    resize!(seed, 4)
    @test r.seed != seed
end

@testset "Random.seed!(rng, ...) returns rng" begin
    # issue #21248
    seed = rand(UInt)
    for m = ([MersenneTwister(seed)], [Xoshiro(seed)], [])
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

@testset "$RNG() & Random.seed!(rng::$RNG) initializes randomly" for RNG in (MersenneTwister, RandomDevice, Xoshiro)
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

@testset "$RNG(seed) & Random.seed!(m::$RNG, seed) produce the same stream" for RNG=(MersenneTwister,Xoshiro)
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

@testset "rand(::$(typeof(RNG)), ::UnitRange{$T}" for RNG ∈ (MersenneTwister(rand(UInt128)), RandomDevice(), Xoshiro()),
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

@testset "RNGs broadcast as scalars: T" for T in (MersenneTwister, RandomDevice)
    @test length.(rand.(T(), 1:3)) == 1:3
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
        Random.jump!(m, 2*big(10)^20)
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

# Xoshiro jumps
@testset "Xoshiro jump, basic" begin
    x1 = Xoshiro(1)
    x2 = Xoshiro(1)

    @test x1 === jump_128!(jump_128!(x1))
    @test x2 === jump_128!(x2, 2)
    @test x1 == x2

    xo1 = Xoshiro(0xfff0241072ddab67, 0xc53bc12f4c3f0b4e, 0x56d451780b2dd4ba, 0x50a4aa153d208dd8)
    @test rand(jump_128(xo1), UInt64) == 0x87c158da8c35824d
    @test rand(jump_192(xo1), UInt64) == 0xcaecd5afdd0847d5

    @test rand(jump_128(xo1, 98765), UInt64) == 0xcbec1d5053142608
    @test rand(jump_192(xo1, 98765), UInt64) == 0x3b97a94c44d66216

    # Throws where appropriate
    @test_throws DomainError jump_128(Xoshiro(1), -1)
    @test_throws DomainError jump_128!(Xoshiro(1), -1)
    @test_throws DomainError jump_192(Xoshiro(1), -1)
    @test_throws DomainError jump_192!(Xoshiro(1), -1)

    # clean copy when non-mut and no state advance
    x = Xoshiro(1)
    @test jump_128(x, 0) == x
    @test jump_128(x, 0) !== x
    @test jump_192(x, 0) == x
    @test jump_192(x, 0) !== x

    y = Xoshiro(1)
    @test jump_128!(x, 0) == y
    @test jump_192!(x, 0) == y
end

@testset "Xoshiro jump_128, various seeds" begin
    for seed in (0, 1, 0xa0a3f09d0cecd878, 0x7ff8)
        x = Xoshiro(seed)
        @test jump_128(jump_128(jump_128(x))) == jump_128(x, 3)
        x1 = Xoshiro(seed)
        @test jump_128!(jump_128!(jump_128!(x1))) == jump_128(x, 3)
        jump_128!(x1, 997)
        x2 = jump_128!(Xoshiro(seed), 1000)
        for T ∈ (Float64, UInt64, Int, Char, Bool)
            @test rand(x1, T, 5) == rand(x2, T, 5)
            @test rand(jump_128!(x1), T, 5) == rand(jump_128!(x2), T, 5)
        end
    end
end

@testset "Xoshiro jump_192, various seeds" begin
    for seed in (0, 1, 0xa0a3f09d0cecd878, 0x7ff8)
        x = Xoshiro(seed)
        @test jump_192(jump_192(jump_192(x))) == jump_192(x, 3)
        x1 = Xoshiro(seed)
        @test jump_192!(jump_192!(jump_192!(x1))) == jump_192(x, 3)
        jump_192!(x1, 997)
        x2 = jump_192!(Xoshiro(seed), 1000)
        for T ∈ (Float64, UInt64, Int, Char, Bool)
            @test rand(x1, T, 5) == rand(x2, T, 5)
            @test rand(jump_192!(x1), T, 5) == rand(jump_192!(x2), T, 5)
        end
    end
end

@testset "seed! and hash_seed" begin
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

    vseeds = Dict{Vector{UInt8}, BigInt}()
    for seed = seeds
        bigseed = big(seed)
        vseed = Random.hash_seed(bigseed)
        # test property 1) above
        @test Random.hash_seed(seed) == vseed
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
    hash32 = Random.hash_seed(seed32)
    @test Random.hash_seed(map(UInt64, seed32)) == hash32
    @test hash32 ∉ keys(vseeds)

    seed_str = randstring()
    seed_gstr = GenericString(seed_str)
    @test Random.hash_seed(seed_str) == Random.hash_seed(seed_gstr)
    string_seeds = Set{Vector{UInt8}}()
    for ch = 'A':'z'
        vseed = Random.hash_seed(string(ch))
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
