# Prototype ParseStream interface
#
# Here we test the ParseStream interface, by taking input code and checking
# that the correct sequence of emit() and bump() produces a valid parse tree.

using .JuliaSyntax: ParseStream,
    peek_token,
    bump, bump_trivia, bump_invisible,
    emit, emit_diagnostic, TRIVIA_FLAG, INFIX_FLAG,
    ParseStreamPosition, first_child_position, last_child_position,
    parsestmt

# Here we manually issue parse events in the order the Julia parser would issue
# them
@testset "ParseStream" begin
    code = """
    for i = 1:10
        xx[i] + 2
        # hi
        yy
    end
    """
    st = ParseStream(code)

    p1 = JuliaSyntax.position(st)
        @test JuliaSyntax.peek(st) == K"for"
        bump(st, TRIVIA_FLAG)
        p2 = JuliaSyntax.position(st)
            @test JuliaSyntax.peek(st) == K"Identifier"    # 'i'
            bump(st)
            @test JuliaSyntax.peek(st) == K"="
            bump(st, TRIVIA_FLAG)
            p3 = JuliaSyntax.position(st)
                @test JuliaSyntax.peek(st) == K"Integer"   # 1
                bump(st)
                @test JuliaSyntax.peek(st) == K":"
                bump(st) # :
                @test JuliaSyntax.peek(st) == K"Integer"   # 10
                bump(st) # 10
            emit(st, p3, K"call", INFIX_FLAG)
        emit(st, p2, K"=")
        @test JuliaSyntax.peek(st) == K"NewlineWs"
        bump(st, TRIVIA_FLAG)
        p4 = JuliaSyntax.position(st)
            p5 = JuliaSyntax.position(st) # [call]
                p6 = JuliaSyntax.position(st) # [ref]
                    @test JuliaSyntax.peek(st) == K"Identifier" # 'xx'
                    bump(st)
                    @test JuliaSyntax.peek(st) == K"["
                    bump(st, TRIVIA_FLAG)
                    @test JuliaSyntax.peek(st) == K"Identifier" # 'i'
                    bump(st)
                    @test JuliaSyntax.peek(st) == K"]"
                    bump(st, TRIVIA_FLAG)
                emit(st, p6, K"ref")
                @test JuliaSyntax.peek(st) == K"+"
                bump(st)
                @test JuliaSyntax.peek(st) == K"Integer"        # 2
                bump(st)
            emit(st, p5, K"call", INFIX_FLAG)
            @test JuliaSyntax.peek(st) == K"NewlineWs"
            bump(st, TRIVIA_FLAG)
            @test JuliaSyntax.peek(st) == K"NewlineWs"
            bump(st, TRIVIA_FLAG)
            @test JuliaSyntax.peek(st) == K"Identifier" # 'yy'
            bump(st)
        emit(st, p4, K"block")
        @test JuliaSyntax.peek(st) == K"NewlineWs"
        bump(st, TRIVIA_FLAG)
        bump(st, TRIVIA_FLAG) # end
    emit(st, p1, K"for")
    @test JuliaSyntax.peek(st) == K"NewlineWs"
    bump(st, TRIVIA_FLAG)
    emit(st, p1, K"toplevel")
end

@testset "ParseStream constructors" begin
    @testset "Byte buffer inputs" begin
        # Vector{UInt8}
        let
            st = ParseStream(Vector{UInt8}("x+y"))
            bump(st)
            @test build_tree(Expr, st) == :x
            @test JuliaSyntax.last_byte(st) == 1
        end
        let
            st = ParseStream(Vector{UInt8}("x+y"), 3)
            bump(st)
            @test build_tree(Expr, st) == :y
            @test JuliaSyntax.last_byte(st) == 3
        end
        # Ptr{UInt8}, len
        code = "x+y"
        GC.@preserve code begin
            let
                st = ParseStream(pointer(code), 3)
                bump(st)
                @test build_tree(Expr, st) == :x
                @test JuliaSyntax.last_byte(st) == 1
            end
        end
    end
end

@testset "ParseStream tree traversal" begin
    # NB: ParseStreamPosition.node_index includes an initial sentinel token so
    # indices here are one more than "might be expected". Additionally, note that
    # the byte index points to the first byte after the token.
    st = parse_sexpr("((a b) c)")
    child1_pos = first_child_position(st, JuliaSyntax.position(st))
    @test child1_pos == ParseStreamPosition(7, 8)
    @test first_child_position(st, child1_pos) == ParseStreamPosition(4, 4)
    @test last_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(9, 10)
    @test last_child_position(st, child1_pos) == ParseStreamPosition(6, 6)

    st = parse_sexpr("( (a b) c)")
    child1_pos = first_child_position(st, JuliaSyntax.position(st))
    @test child1_pos == ParseStreamPosition(8, 9)
    @test first_child_position(st, child1_pos) == ParseStreamPosition(5, 5)
    @test last_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(10, 11)
    @test last_child_position(st, child1_pos) == ParseStreamPosition(7, 7)

    st = parse_sexpr("(a (b c))")
    @test first_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(3, 3)
    child2_pos = last_child_position(st, JuliaSyntax.position(st))
    @test child2_pos == ParseStreamPosition(9, 10)
    @test first_child_position(st, child2_pos) == ParseStreamPosition(6, 6)
    @test last_child_position(st, child2_pos) == ParseStreamPosition(8, 8)

    st = parse_sexpr("( a (b c))")
    @test first_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(4, 4)
    child2_pos = last_child_position(st, JuliaSyntax.position(st))
    @test child2_pos == ParseStreamPosition(10, 11)
    @test first_child_position(st, child2_pos) == ParseStreamPosition(7, 7)
    @test last_child_position(st, child2_pos) == ParseStreamPosition(9, 9)

    st = parse_sexpr("a (b c)")
    @test first_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(5, 5)
    @test last_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(7, 7)

    st = parse_sexpr("(a) (b c)")
    @test first_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(7, 8)
    @test last_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(9, 10)

    st = parse_sexpr("(() ())")
    @test first_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(4, 5)
    @test last_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(7, 9)

    # No non-trivia children or tokens
    st = parse_sexpr("()")
    @test first_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(0, 0)
    @test last_child_position(st, JuliaSyntax.position(st)) == ParseStreamPosition(0, 0)

    # `empty!` keeps or restores the sentinel node
    for clear_output in (false, true)
        clear_output && empty!(st.output)
        empty!(st)
        @test length(st.output) == 1 && kind(st.output[1]) == K"TOMBSTONE"
        @test st.next_byte == 1
    end
end

@testset "SubString{GenericString} (issue #505)" begin
    x = Test.GenericString("1 2")
    @test x == "1 2"
    y = split(x)[1]
    @test y == "1"
    @test y isa SubString{GenericString}
    @test ParseStream(y) isa ParseStream
    @test parsestmt(Expr, y) == parsestmt(Expr, "1")
end

@testset "peek_behind_pos with negative byte index" begin
    # Test that peek_behind_pos doesn't cause InexactError when byte_idx goes negative
    # This can happen when parsing certain incomplete keywords like "do"
    # where trivia skipping walks back past the beginning of the stream
    @test_throws JuliaSyntax.ParseError parseall(GreenNode, "do")
    @test_throws JuliaSyntax.ParseError parseall(GreenNode, "do ")
    @test_throws JuliaSyntax.ParseError parseall(GreenNode, " do")
    @test_throws JuliaSyntax.ParseError parseall(GreenNode, "do\n")
end

@testset "ParseStream printing and tree cursors" begin
    st = ParseStream("a + b")
    parse!(st; rule=:statement)
    @test sprint(show, MIME("text/plain"), st) == "ParseStream at position 6\n"
    @test repr(st.output[2]) == "Identifier (1 bytes, orig_kind=Identifier)"
    @test repr(st.output[end]) == "call (5 bytes, 5 children)"
    tok = JuliaSyntax.SyntaxToken(SyntaxHead(K"Identifier", EMPTY_FLAGS), K"Identifier", false, 3)
    @test repr(tok) == "Identifier      |3"

    gc = JuliaSyntax.GreenTreeCursor(st)
    @test repr(gc) == "call @7"
    @test repr.(collect(Iterators.reverse(gc))) ==
        ["Identifier @6", "Whitespace @5", "Identifier @4", "Whitespace @3", "Identifier @2"]
    rc = RedTreeCursor(st)
    @test startswith(repr(rc), "call @7 [")
    @test JuliaSyntax.treesize(rc) == 5
    @test [byte_range(c) for c in collect(Iterators.reverse(rc))] == [5:5, 4:4, 3:3, 2:2, 1:1]

    # A partial parse with several top level nodes is wrapped
    st = ParseStream("a b")
    parse!(st; rule=:atom)
    JuliaSyntax.bump_trivia(st)
    parse!(st; rule=:atom)
    t = build_tree(GreenNode, st)
    @test kind(t) == K"wrapper"
    @test kind.(children(t)) == [K"Identifier", K"Whitespace", K"Identifier"]
end
