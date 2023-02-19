@testset "SyntaxNode" begin
    # Child access
    tt = "a*b + c"
    t = parse(SyntaxNode, tt)

    @test sourcetext(child(t, 1))    == "a*b"
    @test sourcetext(child(t, 1, 1)) == "a"
    @test sourcetext(child(t, 1, 2)) == "*"
    @test sourcetext(child(t, 1, 3)) == "b"
    @test sourcetext(child(t, 2))    == "+"
    @test sourcetext(child(t, 3))    == "c"

    @test JuliaSyntax.first_byte(child(t, 2)) == findfirst(==('+'), tt)

    # Child indexing
    @test t[1]    === child(t, 1)
    @test t[1, 1] === child(t, 1, 1)
    @test t[end]  === child(t, 3)
    # Unfortunately, can't make t[1, end] work
    # as `lastindex(t, 2)` isn't well defined

    @test sprint(show, t) == "(call-i (call-i a * b) + c)"
    str = sprint(show, MIME("text/plain"), t)
    # These tests are deliberately quite relaxed to avoid being too specific about display style
    @test occursin("line:col", str)
    @test occursin("call-i", str)
    @test sprint(JuliaSyntax.highlight, tt, t, 1, 3) == "a*\e[48;2;40;40;70mb\e[0;0m + c"
    @test sprint(JuliaSyntax.highlight, tt, t.raw, 5) == "a*b + \e[48;2;40;40;70mc\e[0;0m"

    node = parse(SyntaxNode, "f()")
    push!(node, parse(SyntaxNode, "x"))
    @test length(children(node)) == 2
    node[2] = parse(SyntaxNode, "y")
    @test sourcetext(child(node, 2)) == "y"
end
