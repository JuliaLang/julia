@testset "SyntaxNode" begin
    # Child access
    t = parse(SyntaxNode, "a*b + c")

    @test sourcetext(child(t, 1))    == "a*b"
    @test sourcetext(child(t, 1, 1)) == "a"
    @test sourcetext(child(t, 1, 2)) == "*"
    @test sourcetext(child(t, 1, 3)) == "b"
    @test sourcetext(child(t, 2))    == "+"
    @test sourcetext(child(t, 3))    == "c"

    # Child indexing
    @test t[1]    === child(t, 1)
    @test t[1, 1] === child(t, 1, 1)
    @test t[end]  === child(t, 3)
    # Unfortunately, can't make t[1, end] work
    # as `lastindex(t, 2)` isn't well defined
end
