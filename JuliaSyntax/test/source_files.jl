@testset "SourceFile lines and column indexing" begin
    @test source_location(SourceFile("a"), 1) == (1,1)
    @test source_location(SourceFile("a"), 2) == (1,2)

    @test source_location(SourceFile("a\n"), 2) == (1,2)
    @test source_location(SourceFile("a\n"), 3) == (1,3)

    @test source_location(SourceFile("a\nb\n"), 2) == (1,2)
    @test source_location(SourceFile("a\nb\n"), 3) == (2,1)
    @test source_location(SourceFile("a\nb\n"), 4) == (2,2)
    @test source_location(SourceFile("a\nb\n"), 5) == (2,3)
end
