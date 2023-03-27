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


@testset "highlight()" begin
    src = JuliaSyntax.SourceFile("""
        abcd
        αβγδ
        +-*/""")

    # Empty ranges
    @test sprint(highlight, src, 1:0) == "abcd\n└\nαβγδ\n+-*/"
    @test sprint(highlight, src, 2:1) == "abcd\n#└\nαβγδ\n+-*/"
    @test sprint(highlight, src, 3:2) == "abcd\n# └\nαβγδ\n+-*/"
    @test sprint(highlight, src, 4:3) == "abcd\n#  └\nαβγδ\n+-*/"
    @test sprint(highlight, src, 5:4) == "abcd\n#   └\nαβγδ\n+-*/"
    @test sprint(highlight, src, 6:5) == "abcd\nαβγδ\n└\n+-*/"
    @test sprint(highlight, src, 19:18) == "abcd\nαβγδ\n+-*/\n#   └"
    @test sprint(io->highlight(io, src, 1:0, context_lines_after=0, note="hi")) ==
        "abcd\n└ ── hi"

    # Single line ranges
    @test sprint(highlight, src, 1:4) == "abcd\n└──┘\nαβγδ\n+-*/"
    @test sprint(highlight, src, 2:4) == "abcd\n#└─┘\nαβγδ\n+-*/"
    @test sprint(highlight, src, 3:4) == "abcd\n# └┘\nαβγδ\n+-*/"
    @test sprint(highlight, src, 4:4) == "abcd\n#  ╙\nαβγδ\n+-*/"
    @test sprint(highlight, src, 5:5) == "abcd\n#   └\nαβγδ\n+-*/"

    # multi-byte chars
    @test sprint(highlight, src, 8:13) == """
        abcd
        αβγδ
        #└─┘
        +-*/"""

    # Multi-line ranges
    @test sprint(highlight, src, 1:7) == """
        ┌───
        abcd
        αβγδ
        ┘
        +-*/"""
    @test sprint(highlight, src, 2:7) == """
        #┌──
        abcd
        αβγδ
        ┘
        +-*/"""
    @test sprint(highlight, src, 2:9) == """
        #┌──
        abcd
        αβγδ
        #┘
        +-*/"""
    @test sprint(highlight, src, 4:9) == """
        #  ┌
        abcd
        αβγδ
        #┘
        +-*/"""
    @test sprint(highlight, src, 5:9) == """
        #   ┌
        abcd
        αβγδ
        #┘
        +-*/"""
    @test sprint(highlight, src, 1:18) == """
        ┌───
        abcd
        αβγδ
        +-*/
        #──┘"""

    # context lines
    @test sprint(io->highlight(io, src, 8:13;
                               context_lines_before=0,
                               context_lines_after=0)) == """
        αβγδ
        #└─┘"""
    @test sprint(io->highlight(io, src, 8:13; context_lines_after=0)) == """
        abcd
        αβγδ
        #└─┘"""
    @test sprint(io->highlight(io, src, 8:13; context_lines_before=0)) == """
        αβγδ
        #└─┘
        +-*/"""
    @test sprint(io->highlight(io, src, 1:18; context_lines_inner=0)) == """
        ┌───
        abcd
        ⋮
        +-*/
        #──┘"""

    # annotations
    @test sprint(io->highlight(io, src, 8:13; note="hello")) == """
        abcd
        αβγδ
        #└─┘ ── hello
        +-*/"""
    @test sprint(io->highlight(io, src, 1:13; note="hello")) == """
        ┌───
        abcd
        αβγδ
        #──┘ ── hello
        +-*/"""
    @test sprint(io->highlight(io, src, 8:13;
                               note=(io,indent,w)->print(io, "\n$indent$('!'^w) hello"))) == """
        abcd
        αβγδ
        #└─┘
        #!!! hello
        +-*/"""

    # colored output
    @test sprint(io->highlight(io, src, 8:13; context_lines_after=0, note="hello", notecolor=:light_red),
                 context=:color=>true) ==
        "abcd\nα\e[48;2;120;70;70mβγδ\e[0;0m\n\e[90m#└─┘ ── \e[0;0m\e[91mhello\e[0;0m"
    @test sprint(io->highlight(io, src, 1:13; context_lines_after=0, note="hello", notecolor=(255,0,0)),
                 context=:color=>true) ==
        "\e[90m┌───\e[0;0m\n\e[48;2;120;70;70mabcd\e[0;0m\n\e[48;2;120;70;70mαβγδ\e[0;0m\n\e[90m#──┘ ── \e[0;0m\e[38;2;255;0;0mhello\e[0;0m"
    @test sprint(io->highlight(io, src, 1:18, context_lines_inner=0),
                 context=:color=>true) ==
        "\e[90m┌───\e[0;0m\n\e[48;2;120;70;70mabcd\e[0;0m\n\e[48;2;120;70;70m\e[0;0m⋮\n\e[48;2;120;70;70m+-*/\e[0;0m\n\e[90m#──┘\e[0;0m"
end
