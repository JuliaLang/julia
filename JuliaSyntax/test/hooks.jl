@testset "Hooks for Core integration" begin
    @testset "parsing empty strings" begin
        @test JuliaSyntax.core_parser_hook("", "somefile", 0, :statement) == Core.svec(nothing, 0)
        @test JuliaSyntax.core_parser_hook("", "somefile", 0, :statement) == Core.svec(nothing, 0)

        @test JuliaSyntax.core_parser_hook("  ", "somefile", 2, :statement) == Core.svec(nothing,2)
        @test JuliaSyntax.core_parser_hook(" #==# ", "somefile", 6, :statement) == Core.svec(nothing,6)
    end

    @testset "filename is used" begin
        ex = JuliaSyntax.core_parser_hook("@a", "somefile", 0, :statement)[1]
        @test Meta.isexpr(ex, :macrocall)
        @test ex.args[2] == LineNumberNode(1, "somefile")
    end

    @testset "enable_in_core!" begin
        JuliaSyntax.enable_in_core!()

        @test Meta.parse("x + 1") == :(x + 1)
        @test Meta.parse("x + 1", 1) == (:(x + 1), 6)

        # Test that parsing statements incrementally works and stops after
        # whitespace / comment trivia
        @test Meta.parse("x + 1\n(y)\n", 1) == (:(x + 1), 7)
        @test Meta.parse("x + 1\n(y)\n", 7) == (:y, 11)
        @test Meta.parse(" x#==#", 1) == (:x, 7)
        @test Meta.parse(" #==# ", 1) == (nothing, 7)

        # Check that Meta.parse throws the JuliaSyntax.ParseError rather than
        # Meta.ParseError when Core integration is enabled.
        @test_throws JuliaSyntax.ParseError Meta.parse("[x")

        JuliaSyntax.enable_in_core!(false)
    end
end
