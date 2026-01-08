@testset "SyntaxNode" begin
    # Child access
    tt = "a*b + c"
    t = parsestmt(SyntaxNode, tt)

    @test sourcetext(t[1])    == "a*b"
    @test sourcetext(t[1][1]) == "a"
    @test sourcetext(t[1][2]) == "*"
    @test sourcetext(t[1][3]) == "b"
    @test sourcetext(t[2])    == "+"
    @test sourcetext(t[3])    == "c"

    @test JuliaSyntax.first_byte(t[2]) == findfirst(==('+'), tt)
    @test JuliaSyntax.source_line(t[3]) == 1
    @test source_location(t[3]) == (1, 7)

    # Child indexing
    @test t[end] === t[3]
    @test sourcetext.(t[2:3]) == ["+", "c"]
    @test sourcetext.(t[2:end]) == ["+", "c"]
    @test firstindex(t) == 1
    @test lastindex(t) == 3
    @test !is_leaf(t)
    @test is_leaf(t[3])

    @test sprint(show, t) == "(call-i (call-i a * b) + c)"
    @test sprint(io->show(io, MIME("text/x.sexpression"), t, show_kind=true)) ==
        "(call-i (call-i a::Identifier *::Identifier b::Identifier) +::Identifier c::Identifier)"

    @test sprint(highlight, t[1][3]) == "a*b + c\n# ╙"

    # Pass-through field access
    node = t[1][1]
    @test node.val === :a
    # The specific error text has evolved over Julia versions. Check that it involves `SyntaxData` and immutability
    e = try node.val = :q catch e e end
    @test occursin("immutable", e.msg) && occursin("SyntaxData", e.msg)

    # Newline-terminated source
    t = parsestmt(SyntaxNode, "a*b + c\n")
    @test sprint(highlight, t[1][3]) == "a*b + c\n# ╙"

    # copy
    t = parsestmt(SyntaxNode, "a*b + c")
    ct = copy(t)
    ct.data = nothing
    @test ct.data === nothing && t.data !== nothing
    @test ct[1].parent === ct
    @test ct[1] !== t[1]

    node = parsestmt(SyntaxNode, "f()")
    push!(node, parsestmt(SyntaxNode, "x"))
    @test length(children(node)) == 2
    node[2] = parsestmt(SyntaxNode, "y")
    @test sourcetext(node[2]) == "y"

    # SyntaxNode with offsets
    t,_ = parsestmt(SyntaxNode, "begin a end\nbegin b end", 13)
    @test first(byte_range(t)) == 13
    @test first(byte_range(t[1])) == 19
    @test t[1].val == :b

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
        *                                    :: Identifier
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
       1:4  │     4:4     │    *                                    :: Identifier
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
