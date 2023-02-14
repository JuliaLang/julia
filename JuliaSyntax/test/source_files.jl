@testset "SourceFile lines and column indexing" begin
    @test source_location(SourceFile("a"), 1) == (1,1)
    @test source_location(SourceFile("a"), 2) == (1,2)

    @test source_location(SourceFile("a\n"), 2) == (1,2)
    @test source_location(SourceFile("a\n"), 3) == (1,3)

    @test source_location(SourceFile("a\nb\n"), 2) == (1,2)
    @test source_location(SourceFile("a\nb\n"), 3) == (2,1)
    @test source_location(SourceFile("a\nb\n"), 4) == (2,2)
    @test source_location(SourceFile("a\nb\n"), 5) == (2,3)

    @test source_location(SourceFile("a"; first_line=7), 1) == (7,1)
    @test source_location(SourceFile("a"; first_line=7), 2) == (7,2)

    @test source_location(SourceFile("a\n"; first_line=7), 2) == (7,2)
    @test source_location(SourceFile("a\n"; first_line=7), 3) == (7,3)

    @test source_location(SourceFile("a\nb\n"; first_line=7), 2) == (7,2)
    @test source_location(SourceFile("a\nb\n"; first_line=7), 3) == (8,1)
    @test source_location(SourceFile("a\nb\n"; first_line=7), 4) == (8,2)
    @test source_location(SourceFile("a\nb\n"; first_line=7), 5) == (8,3)

    mktemp() do path, io
        write(io, "a\n")
        @test source_location(SourceFile(; filename=path), 1) == (1,1)
        @test source_location(SourceFile(; filename=path, first_line=7), 1) == (7,1)
    end
end
