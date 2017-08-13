# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "ConjArray core" begin
    m = [1+im 2; 2 4-im]
    cm = ConjMatrix(m)
    @test cm[1,1] == 1-im
    @test cm[1] == 1-im
    @test trace(cm*m) == 27
    @test cm' == m

    cm[:,2] = [3; 3-im] #setindex! with a vector
    @test conj(cm) == [1+im 3; 2 3+im]

    v = [[1+im], [1-im]]
    cv = ConjVector(v)
    @test cv[1] == [1-im]
end

@testset "RowVector conjugation" begin
    v = [1+im, 1-im]
    rv = conj(v.')
    @test (parent(rv) isa ConjArray)
    @test rv' === v
end
