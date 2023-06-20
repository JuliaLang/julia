@testset "SyntaxNode" begin
    # Child access
    tt = "a*b + c"
    t = parsestmt(SyntaxNode, tt)

    @test sourcetext(child(t, 1))    == "a*b"
    @test sourcetext(child(t, 1, 1)) == "a"
    @test sourcetext(child(t, 1, 2)) == "*"
    @test sourcetext(child(t, 1, 3)) == "b"
    @test sourcetext(child(t, 2))    == "+"
    @test sourcetext(child(t, 3))    == "c"

    @test JuliaSyntax.first_byte(child(t, 2)) == findfirst(==('+'), tt)
    @test JuliaSyntax.source_line(child(t, 3)) == 1
    @test source_location(child(t, 3)) == (1, 7)

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
    @test sprint(highlight, child(t, 1, 3)) == "a*b + c\n# ╙"
    @test sprint(highlight, t.source, t.raw, 1, 3) == "a*b + c\n# ╙"

    # Pass-through field access
    node = child(t, 1, 1)
    @test node.val === :a
    # The specific error text has evolved over Julia versions. Check that it involves `SyntaxData` and immutability
    e = try node.val = :q catch e e end
    @test occursin("immutable", e.msg) && occursin("SyntaxData", e.msg)

    # copy
    t = parsestmt(SyntaxNode, "a*b + c")
    ct = copy(t)
    ct.data = nothing
    @test ct.data === nothing && t.data !== nothing
    @test child(ct, 1).parent === ct
    @test child(ct, 1) !== child(t, 1)

    node = parsestmt(SyntaxNode, "f()")
    push!(node, parsestmt(SyntaxNode, "x"))
    @test length(children(node)) == 2
    node[2] = parsestmt(SyntaxNode, "y")
    @test sourcetext(child(node, 2)) == "y"

    # SyntaxNode with offsets
    t,_ = parsestmt(SyntaxNode, "begin a end\nbegin b end", 13)
    @test t.position == 13
    @test child(t,1).position == 19
    @test child(t,1).val == :b
end

@testset "SyntaxNode pretty printing" begin
    t = parsestmt(SyntaxNode, "f(a*b,\n  c)", filename="foo.jl")
    @test sprint(show, MIME("text/plain"), t) == """
    line:col│ tree                                   │ file_name
       1:1  │[call]                                  │foo.jl
       1:1  │  f
       1:3  │  [call-i]
       1:3  │    a
       1:4  │    *
       1:5  │    b
       2:3  │  c
    """
    @test sprint(io->show(io, MIME("text/plain"), t, show_byte_offsets=true)) == """
    line:col│ byte_range  │ tree                                   │ file_name
       1:1  │     1:11    │[call]                                  │foo.jl
       1:1  │     1:1     │  f
       1:3  │     3:5     │  [call-i]
       1:3  │     3:3     │    a
       1:4  │     4:4     │    *
       1:5  │     5:5     │    b
       2:3  │    10:10    │  c
    """

    t,_ = parsestmt(SyntaxNode, "begin a end\nbegin b end", 13)
    @test sprint(show, MIME("text/plain"), t) == """
    line:col│ tree                                   │ file_name
       1:1  │[block]
       1:7  │  b
    """
end
