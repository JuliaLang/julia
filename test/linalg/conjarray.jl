# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "Core" begin
    m = [1+im 2; 2 4-im]
    cm = ConjArray(m)
    @test cm[1,1] == 1-im
    @test trace(cm*m) == 27

    v = [[1+im], [1-im]]
    cv = ConjArray(v)
    @test cv[1] == [1-im]
end

@testset "RowVector conjugates" begin
    v = [1+im, 1-im]
    rv = v'
    @test (parent(rv) isa ConjArray)
    @test rv' === v

    # Currently, view behavior defaults to only RowVectors.
    @test isa((v').', Vector)
    @test isa((v.')', Vector)
end
