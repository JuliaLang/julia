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

@testset "SourceFile position indexing" begin
    @test SourceFile("a\nb\n")[1:2] == "a\n"
    @test SourceFile("a\nb\n")[3:end] == "b\n"
    if Base.VERSION >= v"1.4"
        # Protect the `[begin` from being viewed by the parser on older Julia versions
        @test eval(Meta.parse("""SourceFile("a\nb\n")[begin:end]""")) == "a\nb\n"
    end

    # unicode
    @test SourceFile("αβ")[1:2] == "α"
    @test SourceFile("αβ")[3] == 'β'
end

@testset "SourceFile printing and text extraction" begin
    srcf = SourceFile("module Foo\nend")
    @test sprint(show, MIME("text/plain"), srcf) == """
    ## SourceFile ##
    module Foo
    end"""
    @test sourcetext(srcf) == "module Foo\nend"
end
