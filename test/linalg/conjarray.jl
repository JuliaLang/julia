# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "Core" begin
    m = [1+im 2; 2 4-im]
    cm = ConjMatrix(m)
    @test cm[1,1] == 1-im
    @test cm[1] == 1-im
    @test trace(cm*m) == 27
    @test adjoint(cm) == m

    cm[:,2] = [3; 3-im] #setindex! with a vector
    @test conj(cm) == [1+im 3; 2 3+im]

    v = [[1+im], [1-im]]
    cv = ConjVector(v)
    @test cv[1] == [1-im]
end

@testset "RowVector conjugates" begin
    # these definitions are what adjoint(...) and transpose(...) meant
    # meant prior to the Adjoint/Transpose transition, and the tests
    # below are re-expressed in them to shield them against changes
    # to adjoint(...), transpose(...), .', ', and A[ct]_(mul|ldiv|rdiv)_B[ct]
    using Base.LinAlg: _conj, ConjRowVector
    rvadjoint(v::AbstractVector) = RowVector(_conj(v))
    rvtranspose(v::AbstractVector) = RowVector(v)
    rvadjoint(v::RowVector) = conj(v.vec)
    rvadjoint(v::RowVector{<:Real}) = v.vec
    rvtranspose(v::RowVector) = v.vec
    rvtranspose(v::ConjRowVector) = copy(v.vec)

    v = [1+im, 1-im]
    rv = rvadjoint(v)
    @test (parent(rv) isa ConjArray)
    @test rvadjoint(rv) === v

    # Currently, view behavior defaults to only RowVectors.
    @test isa(rvtranspose(rvadjoint(v)), Vector)
    @test isa(rvadjoint(rvtranspose(v)), Vector)
end
