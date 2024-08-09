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
    @test sprint(io->show(io, MIME("text/x.sexpression"), t, show_kind=true)) ==
        "(call-i (call-i a::Identifier *::* b::Identifier) +::+ c::Identifier)"

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

    # Unicode character ranges
    src = "ab + αβ"
    t = parsestmt(SyntaxNode, src)
    @test char_range(t[1]) == 1:2
    @test char_range(t[2]) == 4:4
    @test char_range(t[3]) == 6:8
    # conversely, β takes two bytes so char_range(t[3]) != byte_range(t[3])
    @test byte_range(t[3]) == 6:9
end

@testset "SyntaxNode pretty printing" begin
    t = parsestmt(SyntaxNode, "f(a*b,\n  c)", filename="foo.jl")
    @test sprint(show, MIME("text/plain"), t) == """
    SyntaxNode:
    [call]
      f                                      :: Identifier
      [call-i]
        a                                    :: Identifier
        *                                    :: *
        b                                    :: Identifier
      c                                      :: Identifier
    """

    @test sprint(io->show(io, MIME("text/plain"), t, show_location=true)) == """
    SyntaxNode:
    line:col│ byte_range  │ tree
     -file- │ "foo.jl"
       1:1  │     1:11    │[call]
       1:1  │     1:1     │  f                                      :: Identifier
       1:3  │     3:5     │  [call-i]
       1:3  │     3:3     │    a                                    :: Identifier
       1:4  │     4:4     │    *                                    :: *
       1:5  │     5:5     │    b                                    :: Identifier
       2:3  │    10:10    │  c                                      :: Identifier
    """

    @test sprint(io->show(io, MIME("text/plain"), t, show_kind=false)) == """
    SyntaxNode:
    [call]
      f
      [call-i]
        a
        *
        b
      c
    """

    t,_ = parsestmt(SyntaxNode, "begin a end\nbegin b end", 13, first_line=100)
    @test sprint(io->show(io, MIME("text/plain"), t, show_location=true)) == """
    SyntaxNode:
    line:col│ byte_range  │ tree
     100:1  │    13:23    │[block]
     100:7  │    19:19    │  b                                      :: Identifier
    """
end
