# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "mean" begin
    @test mean((1,2,3)) === 2.
    @test mean([0]) === 0.
    @test mean([1.]) === 1.
    @test mean([1.,3]) == 2.
    @test mean([1,2,3]) == 2.
    @test mean([0 1 2; 4 5 6], dims=1) == [2.  3.  4.]
    @test mean([1 2 3; 4 5 6], dims=1) == [2.5 3.5 4.5]
    @test mean(-, [1 2 3 ; 4 5 6], dims=1) == [-2.5 -3.5 -4.5]
    @test mean(-, [1 2 3 ; 4 5 6], dims=2) == transpose([-2.0 -5.0])
    @test mean(-, [1 2 3 ; 4 5 6], dims=(1, 2)) == -3.5 .* ones(1, 1)
    @test mean(-, [1 2 3 ; 4 5 6], dims=(1, 1)) == [-2.5 -3.5 -4.5]
    @test mean(-, [1 2 3 ; 4 5 6], dims=()) == Float64[-1 -2 -3 ; -4 -5 -6]
    @test mean(i->i+1, 0:2) === 2.
    @test mean(isodd, [3]) === 1.
    @test mean(x->3x, (1,1)) === 3.

    # mean of iterables:
    n = 10; a = randn(n); b = randn(n)
    @test mean(Tuple(a)) ≈ mean(a)
    @test mean(Tuple(a + b*im)) ≈ mean(a + b*im)
    @test mean(cos, Tuple(a)) ≈ mean(cos, a)
    @test mean(x->x/2, a + b*im) ≈ mean(a + b*im) / 2.
    @test ismissing(mean(Tuple((1, 2, missing, 4, 5))))

    @test isnan(mean([NaN]))
    @test isnan(mean([0.0,NaN]))
    @test isnan(mean([NaN,0.0]))

    @test isnan(mean([0.,Inf,-Inf]))
    @test isnan(mean([1.,-1.,Inf,-Inf]))
    @test isnan(mean([-Inf,Inf]))
    @test isequal(mean([NaN 0.0; 1.2 4.5], dims=2), reshape([NaN; 2.85], 2, 1))

    @test ismissing(mean([1, missing]))
    @test ismissing(mean([NaN, missing]))
    @test ismissing(mean([missing, NaN]))
    @test isequal(mean([missing 1.0; 2.0 3.0], dims=1), [missing 2.0])
    @test mean(skipmissing([1, missing, 2])) === 1.5
    @test isequal(mean(Complex{Float64}[]), NaN+NaN*im)
    @test mean(Complex{Float64}[]) isa Complex{Float64}
    @test isequal(mean(skipmissing(Complex{Float64}[])), NaN+NaN*im)
    @test mean(skipmissing(Complex{Float64}[])) isa Complex{Float64}
    @test isequal(mean(abs, Complex{Float64}[]), NaN)
    @test mean(abs, Complex{Float64}[]) isa Float64
    @test isequal(mean(abs, skipmissing(Complex{Float64}[])), NaN)
    @test mean(abs, skipmissing(Complex{Float64}[])) isa Float64
    @test isequal(mean(Int[]), NaN)
    @test mean(Int[]) isa Float64
    @test isequal(mean(skipmissing(Int[])), NaN)
    @test mean(skipmissing(Int[])) isa Float64
    @test_throws MethodError mean([])
    @test_throws MethodError mean(skipmissing([]))
    @test_throws ArgumentError mean((1 for i in 2:1))
    if VERSION >= v"1.6.0-DEV.83"
        @test_throws ArgumentError mean(())
        @test_throws ArgumentError mean(Union{}[])
    end

    # Check that small types are accumulated using wider type
    for T in (Int8, UInt8)
        x = [typemax(T) typemax(T)]
        g = (v for v in x)
        @test mean(x) == mean(g) == typemax(T)
        @test mean(identity, x) == mean(identity, g) == typemax(T)
        @test mean(x, dims=2) == [typemax(T)]'
    end
    # Check that mean avoids integer overflow (#22)
    let x = fill(typemax(Int), 10), a = tuple(x...)
        @test (mean(x) == mean(x, dims=1)[] == mean(float, x)
               == mean(a) == mean(v for v in x)  == mean(v for v in a)
               ≈ float(typemax(Int)))
    end
    let x = rand(10000)  # mean should use sum's accurate pairwise algorithm
        @test mean(x) == sum(x) / length(x)
    end
    @test mean(Number[1, 1.5, 2+3im]) === 1.5+1im # mixed-type array
    @test mean(v for v in Number[1, 1.5, 2+3im]) === 1.5+1im
    @test isnan(@inferred mean(Int[]))
    @test isnan(@inferred mean(Float32[]))
    @test isnan(@inferred mean(Float64[]))
    @test isnan(@inferred mean(Iterators.filter(x -> true, Int[])))
    @test isnan(@inferred mean(Iterators.filter(x -> true, Float32[])))
    @test isnan(@inferred mean(Iterators.filter(x -> true, Float64[])))
end

@testset "mean for ranges" begin
    for n = 2:5
        @test mean(2:n) == mean([2:n;])
        @test mean(2:0.1:n) ≈ mean([2:0.1:n;])
    end
    @test mean(2:1) === NaN
    @test mean(big(2):1) isa BigFloat
end

@testset "var & std" begin
    # edge case: empty vector
    # iterable; this has to throw for type stability
    @test_throws MethodError var(())
    @test_throws MethodError var((); corrected=false)
    @test_throws MethodError var((); mean=2)
    @test_throws MethodError var((); mean=2, corrected=false)
    # reduction
    @test isnan(var(Int[]))
    @test isnan(var(Int[]; corrected=false))
    @test isnan(var(Int[]; mean=2))
    @test isnan(var(Int[]; mean=2, corrected=false))
    # reduction across dimensions
    @test isequal(var(Int[], dims=1), [NaN])
    @test isequal(var(Int[], dims=1; corrected=false), [NaN])
    @test isequal(var(Int[], dims=1; mean=[2]), [NaN])
    @test isequal(var(Int[], dims=1; mean=[2], corrected=false), [NaN])

    # edge case: one-element vector
    # iterable
    @test isnan(@inferred(var((1,))))
    @test var((1,); corrected=false) === 0.0
    @test var((1,); mean=2) === Inf
    @test var((1,); mean=2, corrected=false) === 1.0
    # reduction
    @test isnan(@inferred(var([1])))
    @test var([1]; corrected=false) === 0.0
    @test var([1]; mean=2) === Inf
    @test var([1]; mean=2, corrected=false) === 1.0
    # reduction across dimensions
    @test isequal(@inferred(var([1], dims=1)), [NaN])
    @test var([1], dims=1; corrected=false) ≈ [0.0]
    @test var([1], dims=1; mean=[2]) ≈ [Inf]
    @test var([1], dims=1; mean=[2], corrected=false) ≈ [1.0]

    @test var(1:8) == 6.
    @test var(1:8, mean=1) == var(Vector(1:8), mean=1)
    @test isnan(var(1:1, mean=1))
    @test isnan(var(1:1))
    @test isnan(var(1:-1))

    @test @inferred(var(1.0:8.0)) == 6.
    @test var(1.0:8.0, mean=1.0) == var(Vector(1.0:8.0), mean=1)
    @test isnan(var(1.0:1.0, mean=1.0))
    @test isnan(var(1.0:1.0))
    @test isnan(var(1.0:-1.0))

    @test @inferred(var(1.0f0:8.0f0)) === 6.f0
    @test var(1.0f0:8.0f0, mean=1.0f0) == var(Vector(1.0f0:8.0f0), mean=1)
    @test isnan(var(1.0f0:1.0f0, mean=1.0f0))
    @test isnan(var(1.0f0:1.0f0))
    @test isnan(var(1.0f0:-1.0f0))

    @test var([1,2,3], mean=2) ≈ 1.
    @test var([1,2,3]) ≈ 1.
    @test var([1,2,3]; corrected=false) ≈ 2.0/3
    @test var([1,2,3]; mean=0) ≈ 7.
    @test var([1,2,3]; mean=0, corrected=false) ≈ 14.0/3

    @test var((1,2,3), mean=2) ≈ 1.
    @test var((1,2,3)) ≈ 1.
    @test var((1,2,3); corrected=false) ≈ 2.0/3
    @test var((1,2,3); mean=0) ≈ 7.
    @test var((1,2,3); mean=0, corrected=false) ≈ 14.0/3
    @test_throws ArgumentError var((1,2,3); mean=())

    @test var([1 2 3 4 5; 6 7 8 9 10], dims=2) ≈ [2.5 2.5]'
    @test var([1 2 3 4 5; 6 7 8 9 10], dims=2; corrected=false) ≈ [2.0 2.0]'

    @test var(collect(1:99), dims=1) ≈ [825]
    @test var(Matrix(transpose(collect(1:99))), dims=2) ≈ [825]

    @test std([1,2,3], mean=2) ≈ 1.
    @test std([1,2,3]) ≈ 1.
    @test std([1,2,3]; corrected=false) ≈ sqrt(2.0/3)
    @test std([1,2,3]; mean=0) ≈ sqrt(7.0)
    @test std([1,2,3]; mean=0, corrected=false) ≈ sqrt(14.0/3)

    @test std([1.0,2,3], mean=2) ≈ 1.
    @test std([1.0,2,3]) ≈ 1.
    @test std([1.0,2,3]; corrected=false) ≈ sqrt(2.0/3)
    @test std([1.0,2,3]; mean=0) ≈ sqrt(7.0)
    @test std([1.0,2,3]; mean=0, corrected=false) ≈ sqrt(14.0/3)

    @test std([1.0,2,3], mean=[0]; dims=1, corrected=false)[] ≈ sqrt(14.0/3)
    @test std([1.0,2,3]; dims=1)[] ≈ 1.
    @test std([1.0,2,3]; dims=1, corrected=false)[] ≈ sqrt(2.0/3)
    @test std([1.0,2,3]; dims=1, mean=[0])[] ≈ sqrt(7.0)
    @test std([1.0,2,3]; dims=1, mean=[0], corrected=false)[] ≈ sqrt(14.0/3)

    @test std((1,2,3), mean=2) ≈ 1.
    @test std((1,2,3)) ≈ 1.
    @test std((1,2,3); corrected=false) ≈ sqrt(2.0/3)
    @test std((1,2,3); mean=0) ≈ sqrt(7.0)
    @test std((1,2,3); mean=0, corrected=false) ≈ sqrt(14.0/3)

    @test std([1 2 3 4 5; 6 7 8 9 10], mean=[3.0,8.0], dims=2) ≈ sqrt.([2.5 2.5]')
    @test std([1 2 3 4 5; 6 7 8 9 10], mean=[3.0,8.0], dims=2; corrected=false) ≈ sqrt.([2.0 2.0]')
    @test std([1 2 3 4 5; 6 7 8 9 10], dims=2) ≈ sqrt.([2.5 2.5]')
    @test std([1 2 3 4 5; 6 7 8 9 10], dims=2; corrected=false) ≈ sqrt.([2.0 2.0]')

    let A = ComplexF64[exp(i*im) for i in 1:10^4]
        @test var(A, mean=0.) ≈ sum(map(abs2, A)) / (length(A) - 1)
        @test var(A, mean=mean(A)) ≈ var(A)
    end

    @test var([1//1, 2//1]) isa Rational{Int}
    @test var([1//1, 2//1], dims=1) isa Vector{Rational{Int}}

    @test std([1//1, 2//1]) isa Float64
    @test std([1//1, 2//1], dims=1) isa Vector{Float64}

    @testset "var: empty cases" begin
        A = Matrix{Int}(undef, 0,1)
        @test var(A) === NaN

        @test isequal(var(A, dims=1), fill(NaN, 1, 1))
        @test isequal(var(A, dims=2), fill(NaN, 0, 1))
        @test isequal(var(A, dims=(1, 2)), fill(NaN, 1, 1))
        @test isequal(var(A, dims=3), fill(NaN, 0, 1))
    end

    # issue #6672
    @test std(AbstractFloat[1,2,3], dims=1) == [1.0]

    for f in (var, std)
        @test ismissing(f([1, missing]))
        @test ismissing(f([NaN, missing]))
        @test ismissing(f([missing, NaN]))
        @test isequal(f([missing 1.0; 2.0 3.0], dims=1), [missing f([1.0, 3.0])])
        @test f(skipmissing([1, missing, 2])) === f([1, 2])

        @test ismissing(f([1, missing], mean=0))
        @test ismissing(f([1, 2], mean=missing))
        @test ismissing(f([1, NaN], mean=missing))
        @test ismissing(f([NaN, missing], mean=0))
        @test ismissing(f([missing, NaN], mean=0))
        @test ismissing(f([NaN, missing], mean=missing))
        @test ismissing(f([missing, NaN], mean=missing))
        @test f(skipmissing([1, missing, 2]), mean=0) === f([1, 2], mean=0)
    end

    @test isequal(var(Complex{Float64}[]), NaN)
    @test var(Complex{Float64}[]) isa Float64
    @test isequal(var(skipmissing(Complex{Float64}[])), NaN)
    @test var(skipmissing(Complex{Float64}[])) isa Float64
    @test_throws MethodError var([])
    @test_throws MethodError var(skipmissing([]))
    @test_throws MethodError var((1 for i in 2:1))
    @test isequal(var(Int[]), NaN)
    @test var(Int[]) isa Float64
    @test isequal(var(skipmissing(Int[])), NaN)
    @test var(skipmissing(Int[])) isa Float64

    # over dimensions with provided means
    x = [1 2 3; 4 5 6]
    @test var(x, dims=1, mean=mean(x, dims=1)) == var(x, dims=1)
    @test var(x, dims=1, mean=reshape(mean(x, dims=1), 1, :, 1)) == var(x, dims=1)
    @test var(x, dims=2, mean=mean(x, dims=2)) == var(x, dims=2)
    @test var(x, dims=2, mean=reshape(mean(x, dims=2), :)) == var(x, dims=2)
    @test var(x, dims=2, mean=reshape(mean(x, dims=2), :, 1, 1)) == var(x, dims=2)
    @test_throws DimensionMismatch var(x, dims=1, mean=ones(size(x, 1)))
    @test_throws DimensionMismatch var(x, dims=1, mean=ones(size(x, 1), 1))
    @test_throws DimensionMismatch var(x, dims=2, mean=ones(1, size(x, 2)))
    @test_throws DimensionMismatch var(x, dims=1, mean=ones(1, 1, size(x, 2)))
    @test_throws DimensionMismatch var(x, dims=2, mean=ones(1, size(x, 2), 1))
    @test_throws DimensionMismatch var(x, dims=2, mean=ones(size(x, 1), 1, 5))
    @test_throws DimensionMismatch var(x, dims=1, mean=ones(1, size(x, 2), 5))
end
