@testset "skip" begin
    x = skip(missing, [1, 2, missing, 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = skip(missing, [1  2; missing 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = collect(skip(missing, [missing]))
    @test eltype(x) === Union{}
    @test isempty(collect(x))
    @test collect(x) isa Vector{Union{}}

    x = collect(skip(missing, Union{Int, Missing}[]))
    @test eltype(x) === Int
    @test isempty(collect(x))
    @test collect(x) isa Vector{Int}

    x = skip(missing, [missing, missing, 1, 2, missing, 4, missing, missing])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = skip(missing, v for v in [missing, 1, missing, 2, 4])
    @test eltype(x) === Any
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    @testset "indexing" begin
        x = skip(missing, [1, missing, 2, missing, missing])
        @test collect(eachindex(x)) == collect(keys(x)) == [1, 3]
        @test x[1] === 1
        @test x[3] === 2
        @test_throws MissingException x[2]
        @test_throws BoundsError x[6]
        @test findfirst(==(2), x) == 3
        @test findall(==(2), x) == [3]
        @test argmin(x) == 1
        @test findmin(x) == (1, 1)
        @test argmax(x) == 3
        @test findmax(x) == (2, 3)

        x = skip(missing, [missing 2; 1 missing])
        @test collect(eachindex(x)) == [2, 3]
        @test collect(keys(x)) == [CartesianIndex(2, 1), CartesianIndex(1, 2)]
        @test x[2] === x[2, 1] === 1
        @test x[3] === x[1, 2] === 2
        @test_throws MissingException x[1]
        @test_throws MissingException x[1, 1]
        @test_throws BoundsError x[5]
        @test_throws BoundsError x[3, 1]
        @test findfirst(==(2), x) == CartesianIndex(1, 2)
        @test findall(==(2), x) == [CartesianIndex(1, 2)]
        @test argmin(x) == CartesianIndex(2, 1)
        @test findmin(x) == (1, CartesianIndex(2, 1))
        @test argmax(x) == CartesianIndex(1, 2)
        @test findmax(x) == (2, CartesianIndex(1, 2))

        for x in (skip(missing, []), skip(missing, [missing, missing]))
            @test isempty(collect(eachindex(x)))
            @test isempty(collect(keys(x)))
            @test_throws BoundsError x[3]
            @test_throws BoundsError x[3, 1]
            @test findfirst(==(2), x) === nothing
            @test isempty(findall(==(2), x))
            @test_throws ArgumentError argmin(x)
            @test_throws ArgumentError findmin(x)
            @test_throws ArgumentError argmax(x)
            @test_throws ArgumentError findmax(x)
        end
    end

    @testset "mapreduce" begin
        # Vary size to test splitting blocks with several configurations of missing values
        for T in (Int, Float64),
            A in (rand(T, 10), rand(T, 1000), rand(T, 10000))
            if T === Int
                @test sum(A) === sum(skip(missing, A)) ===
                    reduce(+, skip(missing, A)) === mapreduce(identity, +, skip(missing, A))
            else
                @test sum(A) ≈ sum(skip(missing, A)) ===
                    reduce(+, skip(missing, A)) === mapreduce(identity, +, skip(missing, A))
            end
            @test mapreduce(cos, *, A) ≈ mapreduce(cos, *, skip(missing, A))

            B = Vector{Union{T,Missing}}(A)
            replace!(x -> rand(Bool) ? x : missing, B)
            if T === Int
                @test sum(collect(skip(missing, B))) === sum(skip(missing, B)) ===
                    reduce(+, skip(missing, B)) === mapreduce(identity, +, skip(missing, B))
            else
                @test sum(collect(skip(missing, B))) ≈ sum(skip(missing, B)) ===
                    reduce(+, skip(missing, B)) === mapreduce(identity, +, skip(missing, B))
            end
            @test mapreduce(cos, *, collect(skip(missing, A))) ≈ mapreduce(cos, *, skip(missing, A))

            # Test block full of missing values
            B[1:length(B)÷2] .= missing
            if T === Int
                @test sum(collect(skip(missing, B))) == sum(skip(missing, B)) ==
                    reduce(+, skip(missing, B)) == mapreduce(identity, +, skip(missing, B))
            else
                @test sum(collect(skip(missing, B))) ≈ sum(skip(missing, B)) ==
                    reduce(+, skip(missing, B)) == mapreduce(identity, +, skip(missing, B))
            end

            @test mapreduce(cos, *, collect(skip(missing, A))) ≈ mapreduce(cos, *, skip(missing, A))
        end

        # Patterns that exercize code paths for inputs with 1 or 2 non-missing values
        @test sum(skip(missing, [1, missing, missing, missing])) === 1
        @test sum(skip(missing, [missing, missing, missing, 1])) === 1
        @test sum(skip(missing, [1, missing, missing, missing, 2])) === 3
        @test sum(skip(missing, [missing, missing, missing, 1, 2])) === 3

        for n in 0:3
            itr = skip(missing, Vector{Union{Int,Missing}}(fill(missing, n)))
            @test sum(itr) == reduce(+, itr) == mapreduce(identity, +, itr) === 0
            @test_throws ArgumentError reduce(x -> x/2, itr)
            @test_throws ArgumentError mapreduce(x -> x/2, +, itr)
        end
    end

    @testset "filter" begin
        allmiss = Vector{Union{Int,Missing}}(missing, 10)
        @test isempty(filter(isodd, skip(missing, allmiss))::Vector{Int})
        twod1 = [1.0f0 missing; 3.0f0 missing]
        @test filter(x->x > 0, skip(missing, twod1))::Vector{Float32} == [1, 3]
        twod2 = [1.0f0 2.0f0; 3.0f0 4.0f0]
        @test filter(x->x > 0, skip(missing, twod2)) == reshape(twod2, (4,))
    end
end
