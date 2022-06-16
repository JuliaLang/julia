@testset "Hooks for Core integration" begin
    JuliaSyntax.enable_in_core!()

    @test Meta.parse("x + 1") == :(x + 1)
    @test Meta.parse("x + 1", 1) == (:(x + 1), 6)

    # Test that parsing statements incrementally works
    @test Meta.parse("x + 1\n(y)", 1) == (:(x + 1), 6)
    @test Meta.parse("x + 1\n(y)", 6) == (:y, 10)

    # Check that Meta.parse throws the JuliaSyntax.ParseError rather than
    # Meta.ParseError when Core integration is enabled.
    @test_throws JuliaSyntax.ParseError Meta.parse("[x")

    JuliaSyntax.enable_in_core!(false)
end
