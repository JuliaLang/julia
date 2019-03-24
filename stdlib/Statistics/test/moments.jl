using Statistics
using Test

@testset "Moments" begin
weight_funcs = (weights, aweights, fweights, pweights)

##### weighted var & std

x = [0.57, 0.10, 0.91, 0.72, 0.46, 0.0]
w = [3.84, 2.70, 8.29, 8.91, 9.71, 0.0]

@testset "Uncorrected with $f" for f in weight_funcs
    wv = f(w)
    m = mean(x, weights=wv)

    # expected uncorrected output
    expected_var = sum(abs2.(x .- m) .* wv) / sum(wv)
    expected_std = sqrt.(expected_var)

    @testset "Variance" begin
        @test var(x, weights=wv, corrected=false)           ≈ expected_var
        @test var(x, weights=wv, mean=m, corrected=false)   ≈ expected_var
    end

    @testset "Standard Deviation" begin
        @test std(x, weights=wv, corrected=false)           ≈ expected_std
        @test std(x, weights=wv, mean=m, corrected=false)   ≈ expected_std
    end
end

# expected corrected output for (weights, aweights, fweights, pweights)
expected_var = [NaN, 0.0694434191182236, 0.05466601256158146, 0.06628969012045285]
expected_std = sqrt.(expected_var)

@testset "Corrected with $(weight_funcs[i])" for i in eachindex(weight_funcs)
    wv = weight_funcs[i](w)
    m = mean(x, weights=wv)

    @testset "Variance" begin
        if isa(wv, Weights)
            @test_throws ArgumentError var(x, weights=wv, corrected=true)
        else
            @test var(x, weights=wv, corrected=true)           ≈ expected_var[i]
            @test var(x, weights=wv, mean=m, corrected=true)   ≈ expected_var[i]
        end
    end

    @testset "Standard Deviation" begin
        if isa(wv, Weights)
            @test_throws ArgumentError std(x, weights=wv, corrected=true)
        else
            @test std(x, weights=wv, corrected=true)           ≈ expected_std[i]
            @test std(x, weights=wv, mean=m, corrected=true)   ≈ expected_std[i]
        end
    end
end

x = rand(5, 6)
w1 = rand(5)
w2 = rand(6)

@testset "Uncorrected with $f" for f in weight_funcs
    wv1 = f(w1)
    wv2 = f(w2)
    m1 = mean(x, weights=wv1, dims=1)
    m2 = mean(x, weights=wv2, dims=2)

    expected_var1 = sum(abs2.(x .- m1) .* w1, dims = 1) ./ sum(wv1)
    expected_var2 = sum(abs2.(x .- m2) .* w2', dims = 2) ./ sum(wv2)
    expected_std1 = sqrt.(expected_var1)
    expected_std2 = sqrt.(expected_var2)

    @testset "Variance" begin
        @test var(x, weights=wv1, dims=1, corrected=false) ≈ expected_var1
        @test var(x, weights=wv2, dims=2, corrected=false) ≈ expected_var2
        @test var(x, weights=wv1, dims=1, mean=m1, corrected=false) ≈ expected_var1
        @test var(x, weights=wv2, dims=2, mean=m2, corrected=false) ≈ expected_var2
    end

    @testset "Standard Deviation" begin
        @test std(x, weights=wv1, dims=1, corrected=false)          ≈ expected_std1
        @test std(x, weights=wv2, dims=2, corrected=false)          ≈ expected_std2
        @test std(x, weights=wv1, dims=1, mean=m1, corrected=false) ≈ expected_std1
        @test std(x, weights=wv2, dims=2, mean=m2, corrected=false) ≈ expected_std2
    end
end

@testset "Corrected with $f" for f in weight_funcs
    wv1 = f(w1)
    wv2 = f(w2)
    m1 = mean(x, weights=wv1, dims=1)
    m2 = mean(x, weights=wv2, dims=2)

    if !isa(wv1, Weights)
        expected_var1 = sum(abs2.(x .- m1) .* w1, dims = 1) .* Statistics.varcorrection(wv1, true)
        expected_var2 = sum(abs2.(x .- m2) .* w2', dims = 2) .* Statistics.varcorrection(wv2, true)
        expected_std1 = sqrt.(expected_var1)
        expected_std2 = sqrt.(expected_var2)
    end

    @testset "Variance" begin
        if isa(wv1, Weights)
            @test_throws ArgumentError var(x, weights=wv1, dims=1, corrected=true)
        else
            @test var(x, weights=wv1, dims=1, corrected=true) ≈ expected_var1
            @test var(x, weights=wv2, dims=2, corrected=true) ≈ expected_var2
            @test var(x, weights=wv1, dims=1, mean=m1, corrected=true) ≈ expected_var1
            @test var(x, weights=wv2, dims=2, mean=m2, corrected=true) ≈ expected_var2
        end
    end

    @testset "Standard Deviation" begin
        if isa(wv1, Weights)
            @test_throws ArgumentError std(x, weights=wv1, dims=1, corrected=true)
        else
            @test std(x, weights=wv1, dims=1, corrected=true)          ≈ expected_std1
            @test std(x, weights=wv2, dims=2, corrected=true)          ≈ expected_std2
            @test std(x, weights=wv1, dims=1, mean=m1, corrected=true) ≈ expected_std1
            @test std(x, weights=wv2, dims=2, mean=m2, corrected=true) ≈ expected_std2
        end
    end
end

@testset "Skewness and Kurtosis with $f" for f in weight_funcs
    wv = f(ones(5) * 2.0)

    @test skewness(1:5)             ≈  0.0
    @test skewness([1, 2, 3, 4, 5]) ≈  0.0
    @test skewness([1, 2, 2, 2, 5]) ≈  1.1731251294063556
    @test skewness([1, 4, 4, 4, 5]) ≈ -1.1731251294063556

    @test skewness([1, 2, 2, 2, 5], weights=wv) ≈ 1.1731251294063556

    @test kurtosis(1:5)             ≈ -1.3
    @test kurtosis([1, 2, 3, 4, 5]) ≈ -1.3
    @test kurtosis([1, 2, 3, 3, 2]) ≈ -1.1530612244897953

    @test kurtosis([1, 2, 3, 4, 5], weights=wv) ≈ -1.3
end

end
