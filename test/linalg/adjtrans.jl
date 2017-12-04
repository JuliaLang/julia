# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.LinAlg: Adjoint, Transpose

@testset "Adjoint and Transpose inner constructor basics" begin
    intvec, intmat = [1, 2], [1 2; 3 4]
    # Adjoint/Transpose eltype must match the type of the Adjoint/Transpose of the input eltype
    @test_throws ErrorException Adjoint{Float64,Vector{Int}}(intvec)
    @test_throws ErrorException Adjoint{Float64,Matrix{Int}}(intmat)
    @test_throws ErrorException Transpose{Float64,Vector{Int}}(intvec)
    @test_throws ErrorException Transpose{Float64,Matrix{Int}}(intmat)
    # Adjoint/Transpose wrapped array type must match the input array type
    @test_throws MethodError Adjoint{Int,Vector{Float64}}(intvec)
    @test_throws MethodError Adjoint{Int,Matrix{Float64}}(intmat)
    @test_throws MethodError Transpose{Int,Vector{Float64}}(intvec)
    @test_throws MethodError Transpose{Int,Matrix{Float64}}(intmat)
    # Adjoint/Transpose inner constructor basic functionality, concrete scalar eltype
    @test (Adjoint{Int,Vector{Int}}(intvec)::Adjoint{Int,Vector{Int}}).parent === intvec
    @test (Adjoint{Int,Matrix{Int}}(intmat)::Adjoint{Int,Matrix{Int}}).parent === intmat
    @test (Transpose{Int,Vector{Int}}(intvec)::Transpose{Int,Vector{Int}}).parent === intvec
    @test (Transpose{Int,Matrix{Int}}(intmat)::Transpose{Int,Matrix{Int}}).parent === intmat
    # Adjoint/Transpose inner constructor basic functionality, abstract scalar eltype
    anyvec, anymat = Any[1, 2], Any[1 2; 3 4]
    @test (Adjoint{Any,Vector{Any}}(anyvec)::Adjoint{Any,Vector{Any}}).parent === anyvec
    @test (Adjoint{Any,Matrix{Any}}(anymat)::Adjoint{Any,Matrix{Any}}).parent === anymat
    @test (Transpose{Any,Vector{Any}}(anyvec)::Transpose{Any,Vector{Any}}).parent === anyvec
    @test (Transpose{Any,Matrix{Any}}(anymat)::Transpose{Any,Matrix{Any}}).parent === anymat
    # Adjoint/Transpose inner constructor basic functionality, concrete array eltype
    intvecvec = [[1, 2], [3, 4]]
    intmatmat = [[[1 2]] [[3 4]] [[5 6]]; [[7 8]] [[9 10]] [[11 12]]]
    @test (X = Adjoint{Adjoint{Int,Vector{Int}},Vector{Vector{Int}}}(intvecvec);
            isa(X, Adjoint{Adjoint{Int,Vector{Int}},Vector{Vector{Int}}}) && X.parent === intvecvec)
    @test (X = Adjoint{Adjoint{Int,Matrix{Int}},Matrix{Matrix{Int}}}(intmatmat);
            isa(X, Adjoint{Adjoint{Int,Matrix{Int}},Matrix{Matrix{Int}}}) && X.parent === intmatmat)
    @test (X = Transpose{Transpose{Int,Vector{Int}},Vector{Vector{Int}}}(intvecvec);
            isa(X, Transpose{Transpose{Int,Vector{Int}},Vector{Vector{Int}}}) && X.parent === intvecvec)
    @test (X = Transpose{Transpose{Int,Matrix{Int}},Matrix{Matrix{Int}}}(intmatmat);
            isa(X, Transpose{Transpose{Int,Matrix{Int}},Matrix{Matrix{Int}}}) && X.parent === intmatmat)
end

@testset "Adjoint and Transpose outer constructor basics" begin
    intvec, intmat = [1, 2], [1 2; 3 4]
    # the wrapped array's eltype strictly determines the Adjoint/Transpose eltype
    # so Adjoint{T}/Transpose{T} constructors are somewhat unnecessary and error-prone
    # so ascertain that such calls throw whether or not T and the input eltype are compatible
    @test_throws MethodError Adjoint{Int}(intvec)
    @test_throws MethodError Adjoint{Int}(intmat)
    @test_throws MethodError Adjoint{Float64}(intvec)
    @test_throws MethodError Adjoint{Float64}(intmat)
    @test_throws MethodError Transpose{Int}(intvec)
    @test_throws MethodError Transpose{Int}(intmat)
    @test_throws MethodError Transpose{Float64}(intvec)
    @test_throws MethodError Transpose{Float64}(intmat)
    # Adjoint/Transpose outer constructor basic functionality, concrete scalar eltype
    @test (Adjoint(intvec)::Adjoint{Int,Vector{Int}}).parent === intvec
    @test (Adjoint(intmat)::Adjoint{Int,Matrix{Int}}).parent === intmat
    @test (Transpose(intvec)::Transpose{Int,Vector{Int}}).parent === intvec
    @test (Transpose(intmat)::Transpose{Int,Matrix{Int}}).parent === intmat
    # the tests for the inner constructors exercise abstract scalar and concrete array eltype, forgoing here
end

@testset "Adjoint and Transpose of Numbers" begin
    @test Adjoint(1) == 1
    @test Adjoint(1.0) == 1.0
    @test Adjoint(1im) == -1im
    @test Adjoint(1.0im) == -1.0im
    @test Transpose(1) == 1
    @test Transpose(1.0) == 1.0
    @test Transpose(1im) == 1im
    @test Transpose(1.0im) == 1.0im
end

@testset "Adjoint and Transpose unwrapping" begin
    intvec, intmat = [1, 2], [1 2; 3 4]
    @test Adjoint(Adjoint(intvec)) === intvec
    @test Adjoint(Adjoint(intmat)) === intmat
    @test Transpose(Transpose(intvec)) === intvec
    @test Transpose(Transpose(intmat)) === intmat
end

@testset "Adjoint and Transpose basic AbstractArray functionality" begin
    # vectors and matrices with real scalar eltype, and their adjoints/transposes
    intvec, intmat = [1, 2], [1 2 3; 4 5 6]
    tintvec, tintmat = [1 2], [1 4; 2 5; 3 6]
    @testset "length methods" begin
        @test length(Adjoint(intvec)) == length(intvec)
        @test length(Adjoint(intmat)) == length(intmat)
        @test length(Transpose(intvec)) == length(intvec)
        @test length(Transpose(intmat)) == length(intmat)
    end
    @testset "size methods" begin
        @test size(Adjoint(intvec)) == (1, length(intvec))
        @test size(Adjoint(intmat)) == reverse(size(intmat))
        @test size(Transpose(intvec)) == (1, length(intvec))
        @test size(Transpose(intmat)) == reverse(size(intmat))
    end
    @testset "indices methods" begin
        @test indices(Adjoint(intvec)) == (Base.OneTo(1), Base.OneTo(length(intvec)))
        @test indices(Adjoint(intmat)) == reverse(indices(intmat))
        @test indices(Transpose(intvec)) == (Base.OneTo(1), Base.OneTo(length(intvec)))
        @test indices(Transpose(intmat)) == reverse(indices(intmat))
    end
    @testset "IndexStyle methods" begin
        @test IndexStyle(Adjoint(intvec)) == IndexLinear()
        @test IndexStyle(Adjoint(intmat)) == IndexCartesian()
        @test IndexStyle(Transpose(intvec)) == IndexLinear()
        @test IndexStyle(Transpose(intmat)) == IndexCartesian()
    end
    # vectors and matrices with complex scalar eltype, and their adjoints/transposes
    complexintvec, complexintmat = [1im, 2im], [1im 2im 3im; 4im 5im 6im]
    tcomplexintvec, tcomplexintmat = [1im 2im], [1im 4im; 2im 5im; 3im 6im]
    acomplexintvec, acomplexintmat = conj.(tcomplexintvec), conj.(tcomplexintmat)
    # vectors and matrices with real-vector and real-matrix eltype, and their adjoints/transposes
    intvecvec = [[1, 2], [3, 4]]
    tintvecvec = [[[1 2]] [[3 4]]]
    intmatmat = [[[1 2]] [[3  4]] [[ 5  6]];
                 [[7 8]] [[9 10]] [[11 12]]]
    tintmatmat = [[hcat([1, 2])] [hcat([7, 8])];
                  [hcat([3, 4])] [hcat([9, 10])];
                  [hcat([5, 6])] [hcat([11, 12])]]
    # vectors and matrices with complex-vector and complex-matrix eltype, and their adjoints/transposes
    complexintvecvec, complexintmatmat = im .* (intvecvec, intmatmat)
    tcomplexintvecvec, tcomplexintmatmat = im .* (tintvecvec, tintmatmat)
    acomplexintvecvec, acomplexintmatmat = conj.(tcomplexintvecvec), conj.(tcomplexintmatmat)
    @testset "getindex methods, elementary" begin
        # implicitly test elementary definitions, for arrays with concrete real scalar eltype
        @test Adjoint(intvec) == tintvec
        @test Adjoint(intmat) == tintmat
        @test Transpose(intvec) == tintvec
        @test Transpose(intmat) == tintmat
        # implicitly test elementary definitions, for arrays with concrete complex scalar eltype
        @test Adjoint(complexintvec) == acomplexintvec
        @test Adjoint(complexintmat) == acomplexintmat
        @test Transpose(complexintvec) == tcomplexintvec
        @test Transpose(complexintmat) == tcomplexintmat
        # implicitly test elementary definitions, for arrays with concrete real-array eltype
        @test Adjoint(intvecvec) == tintvecvec
        @test Adjoint(intmatmat) == tintmatmat
        @test Transpose(intvecvec) == tintvecvec
        @test Transpose(intmatmat) == tintmatmat
        # implicitly test elementary definitions, for arrays with concrete complex-array type
        @test Adjoint(complexintvecvec) == acomplexintvecvec
        @test Adjoint(complexintmatmat) == acomplexintmatmat
        @test Transpose(complexintvecvec) == tcomplexintvecvec
        @test Transpose(complexintmatmat) == tcomplexintmatmat
    end
    @testset "getindex(::AdjOrTransVec, ::Colon, ::AbstractArray{Int}) methods that preserve wrapper type" begin
        # for arrays with concrete scalar eltype
        @test Adjoint(intvec)[:, [1, 2]] == Adjoint(intvec)
        @test Transpose(intvec)[:, [1, 2]] == Transpose(intvec)
        @test Adjoint(complexintvec)[:, [1, 2]] == Adjoint(complexintvec)
        @test Transpose(complexintvec)[:, [1, 2]] == Transpose(complexintvec)
        # for arrays with concrete array eltype
        @test Adjoint(intvecvec)[:, [1, 2]] == Adjoint(intvecvec)
        @test Transpose(intvecvec)[:, [1, 2]] == Transpose(intvecvec)
        @test Adjoint(complexintvecvec)[:, [1, 2]] == Adjoint(complexintvecvec)
        @test Transpose(complexintvecvec)[:, [1, 2]] == Transpose(complexintvecvec)
    end
    @testset "getindex(::AdjOrTransVec, ::Colon, ::Colon) methods that preserve wrapper type" begin
        # for arrays with concrete scalar eltype
        @test Adjoint(intvec)[:, :] == Adjoint(intvec)
        @test Transpose(intvec)[:, :] == Transpose(intvec)
        @test Adjoint(complexintvec)[:, :] == Adjoint(complexintvec)
        @test Transpose(complexintvec)[:, :] == Transpose(complexintvec)
        # for arrays with concrete array elype
        @test Adjoint(intvecvec)[:, :] == Adjoint(intvecvec)
        @test Transpose(intvecvec)[:, :] == Transpose(intvecvec)
        @test Adjoint(complexintvecvec)[:, :] == Adjoint(complexintvecvec)
        @test Transpose(complexintvecvec)[:, :] == Transpose(complexintvecvec)
    end
    @testset "getindex(::AdjOrTransVec, ::Colon, ::Int) should preserve wrapper type on result entries" begin
        # for arrays with concrete scalar eltype
        @test Adjoint(intvec)[:, 2] == intvec[2:2]
        @test Transpose(intvec)[:, 2] == intvec[2:2]
        @test Adjoint(complexintvec)[:, 2] == conj.(complexintvec[2:2])
        @test Transpose(complexintvec)[:, 2] == complexintvec[2:2]
        # for arrays with concrete array eltype
        @test Adjoint(intvecvec)[:, 2] == Adjoint.(intvecvec[2:2])
        @test Transpose(intvecvec)[:, 2] == Transpose.(intvecvec[2:2])
        @test Adjoint(complexintvecvec)[:, 2] == Adjoint.(complexintvecvec[2:2])
        @test Transpose(complexintvecvec)[:, 2] == Transpose.(complexintvecvec[2:2])
    end
    @testset "setindex! methods" begin
        # for vectors with real scalar eltype
        @test (wv = Adjoint(copy(intvec));
                wv === setindex!(wv, 3, 2) &&
                 wv == setindex!(copy(tintvec), 3, 1, 2)    )
        @test (wv = Transpose(copy(intvec));
                wv === setindex!(wv, 4, 2) &&
                 wv == setindex!(copy(tintvec), 4, 1, 2)    )
        # for matrices with real scalar eltype
        @test (wA = Adjoint(copy(intmat));
                wA === setindex!(wA, 7, 3, 1) &&
                 wA == setindex!(copy(tintmat), 7, 3, 1)    )
        @test (wA = Transpose(copy(intmat));
                wA === setindex!(wA, 7, 3, 1) &&
                 wA == setindex!(copy(tintmat), 7, 3, 1)    )
        # for vectors with complex scalar eltype
        @test (wz = Adjoint(copy(complexintvec));
                wz === setindex!(wz, 3im, 2) &&
                 wz == setindex!(copy(acomplexintvec), 3im, 1, 2)   )
        @test (wz = Transpose(copy(complexintvec));
                wz === setindex!(wz, 4im, 2) &&
                 wz == setindex!(copy(tcomplexintvec), 4im, 1, 2)   )
        # for  matrices with complex scalar eltype
        @test (wZ = Adjoint(copy(complexintmat));
                wZ === setindex!(wZ, 7im, 3, 1) &&
                 wZ == setindex!(copy(acomplexintmat), 7im, 3, 1)   )
        @test (wZ = Transpose(copy(complexintmat));
                wZ === setindex!(wZ, 7im, 3, 1) &&
                 wZ == setindex!(copy(tcomplexintmat), 7im, 3, 1)   )
        # for vectors with concrete real-vector eltype
        @test (wv = Adjoint(copy(intvecvec));
                wv === setindex!(wv, Adjoint([5, 6]), 2) &&
                 wv == setindex!(copy(tintvecvec), [5 6], 2))
        @test (wv = Transpose(copy(intvecvec));
                wv === setindex!(wv, Transpose([5, 6]), 2) &&
                 wv == setindex!(copy(tintvecvec), [5 6], 2))
        # for matrices with concrete real-matrix eltype
        @test (wA = Adjoint(copy(intmatmat));
                wA === setindex!(wA, Adjoint([13 14]), 3, 1) &&
                 wA == setindex!(copy(tintmatmat), hcat([13, 14]), 3, 1))
        @test (wA = Transpose(copy(intmatmat));
                wA === setindex!(wA, Transpose([13 14]), 3, 1) &&
                 wA == setindex!(copy(tintmatmat), hcat([13, 14]), 3, 1))
        # for vectors with concrete complex-vector eltype
        @test (wz = Adjoint(copy(complexintvecvec));
                wz === setindex!(wz, Adjoint([5im, 6im]), 2) &&
                 wz == setindex!(copy(acomplexintvecvec), [-5im -6im], 2))
        @test (wz = Transpose(copy(complexintvecvec));
                wz === setindex!(wz, Transpose([5im, 6im]), 2) &&
                 wz == setindex!(copy(tcomplexintvecvec), [5im 6im], 2))
        # for matrices with concrete complex-matrix eltype
        @test (wZ = Adjoint(copy(complexintmatmat));
                wZ === setindex!(wZ, Adjoint([13im 14im]), 3, 1) &&
                 wZ == setindex!(copy(acomplexintmatmat), hcat([-13im, -14im]), 3, 1))
        @test (wZ = Transpose(copy(complexintmatmat));
                wZ === setindex!(wZ, Transpose([13im 14im]), 3, 1) &&
                 wZ == setindex!(copy(tcomplexintmatmat), hcat([13im, 14im]), 3, 1))
    end
end

@testset "Adjoint and Transpose convert methods that convert underlying storage" begin
    intvec, intmat = [1, 2], [1 2 3; 4 5 6]
    @test convert(Adjoint{Float64,Vector{Float64}}, Adjoint(intvec))::Adjoint{Float64,Vector{Float64}} == Adjoint(intvec)
    @test convert(Adjoint{Float64,Matrix{Float64}}, Adjoint(intmat))::Adjoint{Float64,Matrix{Float64}} == Adjoint(intmat)
    @test convert(Transpose{Float64,Vector{Float64}}, Transpose(intvec))::Transpose{Float64,Vector{Float64}} == Transpose(intvec)
    @test convert(Transpose{Float64,Matrix{Float64}}, Transpose(intmat))::Transpose{Float64,Matrix{Float64}} == Transpose(intmat)
end

@testset "Adjoint and Transpose similar methods" begin
    intvec, intmat = [1, 2], [1 2 3; 4 5 6]
    # similar with no additional specifications, vector (rewrapping) semantics
    @test size(similar(Adjoint(intvec))::Adjoint{Int,Vector{Int}}) == size(Adjoint(intvec))
    @test size(similar(Transpose(intvec))::Transpose{Int,Vector{Int}}) == size(Transpose(intvec))
    # similar with no additional specifications, matrix (no-rewrapping) semantics
    @test size(similar(Adjoint(intmat))::Matrix{Int}) == size(Adjoint(intmat))
    @test size(similar(Transpose(intmat))::Matrix{Int}) == size(Transpose(intmat))
    # similar with element type specification, vector (rewrapping) semantics
    @test size(similar(Adjoint(intvec), Float64)::Adjoint{Float64,Vector{Float64}}) == size(Adjoint(intvec))
    @test size(similar(Transpose(intvec), Float64)::Transpose{Float64,Vector{Float64}}) == size(Transpose(intvec))
    # similar with element type specification, matrix (no-rewrapping) semantics
    @test size(similar(Adjoint(intmat), Float64)::Matrix{Float64}) == size(Adjoint(intmat))
    @test size(similar(Transpose(intmat), Float64)::Matrix{Float64}) == size(Transpose(intmat))
    # similar with element type and arbitrary dims specifications
    shape = (2, 2, 2)
    @test size(similar(Adjoint(intvec), Float64, shape)::Array{Float64,3}) == shape
    @test size(similar(Adjoint(intmat), Float64, shape)::Array{Float64,3}) == shape
    @test size(similar(Transpose(intvec), Float64, shape)::Array{Float64,3}) == shape
    @test size(similar(Transpose(intmat), Float64, shape)::Array{Float64,3}) == shape
end

@testset "Adjoint and Transpose parent methods" begin
    intvec, intmat = [1, 2], [1 2 3; 4 5 6]
    @test parent(Adjoint(intvec)) === intvec
    @test parent(Adjoint(intmat)) === intmat
    @test parent(Transpose(intvec)) === intvec
    @test parent(Transpose(intmat)) === intmat
end

@testset "Adjoint and Transpose vector vec methods" begin
    intvec = [1, 2]
    @test vec(Adjoint(intvec)) === intvec
    @test vec(Transpose(intvec)) === intvec
end
