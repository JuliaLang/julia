# This file is a part of Julia. License is MIT: https://julialang.org/license

module SparseMatrixTests

using Test
using SparseArrays
using Random

@testset "other constructors" begin

    @testset "sprand & sprandn" begin
        let xr = sprand(30, 30, 0.9)
            @test isa(xr, SparseMatrixCSC{Float64,Int})
            @test size(xr) == (30, 30)
            @test all(nonzeros(xr) .>= 0.0)
        end

        let xr = sprand(Float32, 30, 30, 0.9)
            @test isa(xr, SparseMatrixCSC{Float32,Int})
            @test size(xr) == (30, 30)
            @test all(nonzeros(xr) .>= 0.0)
        end

        let xr = sprandn(30, 30, 0.9)
            @test isa(xr, SparseMatrixCSC{Float64,Int})
            @test size(xr) == (30, 30)
            if !isempty(nonzeros(xr))
                @test any(nonzeros(xr) .> 0.0) && any(nonzeros(xr) .< 0.0)
            end
        end

        let xr = sprandn(Float32, 30, 30, 0.9)
            @test isa(xr, SparseMatrixCSC{Float32,Int})
            @test size(xr) == (30, 30)
            if !isempty(nonzeros(xr))
                @test any(nonzeros(xr) .> 0.0) && any(nonzeros(xr) .< 0.0)
            end
        end

        let xr = sprandn(Complex{Float64}, 30, 30, 0.9)
            @test isa(xr, SparseMatrixCSC{Complex{Float64},Int})
            @test size(xr) == (30, 30)
            if !isempty(nonzeros(xr))
                @test any(real.(nonzeros(xr)) .> 0.0) && any(real.(nonzeros(xr)) .< 0.0) && any(imag.(nonzeros(xr)) .> 0.0) && any(imag.(nonzeros(xr)) .< 0.0)
            end
        end

        let xr = sprand(Bool, 30, 30, 0.9)
            @test isa(xr, SparseMatrixCSC{Bool,Int})
            @test size(xr) == (30, 30)
            @test all(nonzeros(xr))
        end

        let r1 = MersenneTwister(0), r2 = MersenneTwister(0)
            @test sprand(r1, 10, 10, .9) == sprand(r2, 10, 10, .9)
            @test sprandn(r1, 10, 10, .9) == sprandn(r2, 10, 10, .9)
            @test sprand(r1, Bool, 10, 10, .9) == sprand(r2,  Bool, 10, 10, .9)
            @test sprandn(r1, Float32, 10, 10, .9) == sprand(r2,  Float32, 10, 10, .9)
            @test sprandn(r1, Complex{Float64}, 10, 10, .9) == sprand(r2,  Complex{Float64}, 10, 10, .9)
        end
    end

end

end # module
