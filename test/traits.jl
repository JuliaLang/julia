@testset "EachIndexSupport" begin
    @test Base.EachIndexSupport(1) == Base.HasEachIndex()
    @test Base.EachIndexSupport([1]) == Base.HasEachIndex()
    @test Base.EachIndexSupport((1,2,3)) == Base.HasEachIndex()
    @test Base.EachIndexSupport((a=1,b=2)) == Base.HasEachIndex()
    @test Base.EachIndexSupport(`echo`) == Base.HasEachIndex()
    @test Base.EachIndexSupport(stdout) == Base.HasEachIndex()
    @test Base.EachIndexSupport(Core.svec(1,2,3)) == Base.HasEachIndex()
    @test Base.EachIndexSupport(pairs([1,2,3])) == Base.HasEachIndex()
    @test Base.EachIndexSupport(Dict(1=>2)) == Base.HasEachIndex()
    @test Base.EachIndexSupport(skipmissing([1,2,3])) == Base.HasEachIndex()
    @test Base.EachIndexSupport(skipmissing(:x)) == Base.NoEachIndex()
    @test Base.EachIndexSupport(Ref(1)) == Base.NoEachIndex()
    @test Base.EachIndexSupport(:x) == Base.NoEachIndex()
end

@testset "eachindextype" begin
    @test Base.eachindextype(1) == Base.OneTo{Int}
    @test Base.eachindextype([1,2,3]) == Base.OneTo{Int}
    @test Base.eachindextype(Dict(1=>2)) == Base.KeySet{Int, Dict{Int,Int}}
    @test Base.eachindextype(:x) === missing
    @test Base.eachindextype(Ref(1)) === missing
end

@testset "AdjacentIndexSupport" begin
    @test Base.AdjacentIndexSupport([1,2,3]) == Base.HasAdjacentIndex()
    @test Base.AdjacentIndexSupport((1,2,3)) == Base.HasAdjacentIndex()
    @test Base.AdjacentIndexSupport((a=1,b=2)) == Base.HasAdjacentIndex()
    @test Base.AdjacentIndexSupport("abc") == Base.HasAdjacentIndex()
    @test Base.AdjacentIndexSupport(1) == Base.NoAdjacentIndex()
    @test Base.AdjacentIndexSupport(:x) == Base.NoAdjacentIndex()
    @test Base.AdjacentIndexSupport(Ref(1)) == Base.NoAdjacentIndex()
end
