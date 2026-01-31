# Prototype ParseStream interface
#
# Here we test the ParseStream interface, by taking input code and checking
# that the correct sequence of emit() and bump() produces a valid parse tree.

using .JuliaSyntax: ParseStream,
    peek, peek_token,
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

    p1 = position(st)
        @test peek(st) == K"for"
        bump(st, TRIVIA_FLAG)
        p2 = position(st)
            @test peek(st) == K"Identifier"    # 'i'
            bump(st)
            @test peek(st) == K"="
            bump(st, TRIVIA_FLAG)
            p3 = position(st)
                @test peek(st) == K"Integer"   # 1
                bump(st)
                @test peek(st) == K":"
                bump(st) # :
                @test peek(st) == K"Integer"   # 10
                bump(st) # 10
            emit(st, p3, K"call", INFIX_FLAG)
        emit(st, p2, K"=")
        @test peek(st) == K"NewlineWs"
        bump(st, TRIVIA_FLAG)
        p4 = position(st)
            p5 = position(st) # [call]
                p6 = position(st) # [ref]
                    @test peek(st) == K"Identifier" # 'xx'
                    bump(st)
                    @test peek(st) == K"["
                    bump(st, TRIVIA_FLAG)
                    @test peek(st) == K"Identifier" # 'i'
                    bump(st)
                    @test peek(st) == K"]"
                    bump(st, TRIVIA_FLAG)
                emit(st, p6, K"ref")
                @test peek(st) == K"+"
                bump(st)
                @test peek(st) == K"Integer"        # 2
                bump(st)
            emit(st, p5, K"call", INFIX_FLAG)
            @test peek(st) == K"NewlineWs"
            bump(st, TRIVIA_FLAG)
            @test peek(st) == K"NewlineWs"
            bump(st, TRIVIA_FLAG)
            @test peek(st) == K"Identifier" # 'yy'
            bump(st)
        emit(st, p4, K"block")
        @test peek(st) == K"NewlineWs"
        bump(st, TRIVIA_FLAG)
        bump(st, TRIVIA_FLAG) # end
    emit(st, p1, K"for")
    @test peek(st) == K"NewlineWs"
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
    child1_pos = first_child_position(st, position(st))
    @test child1_pos == ParseStreamPosition(7, 8)
    @test first_child_position(st, child1_pos) == ParseStreamPosition(4, 4)
    @test last_child_position(st, position(st)) == ParseStreamPosition(9, 10)
    @test last_child_position(st, child1_pos) == ParseStreamPosition(6, 6)

    st = parse_sexpr("( (a b) c)")
    child1_pos = first_child_position(st, position(st))
    @test child1_pos == ParseStreamPosition(8, 9)
    @test first_child_position(st, child1_pos) == ParseStreamPosition(5, 5)
    @test last_child_position(st, position(st)) == ParseStreamPosition(10, 11)
    @test last_child_position(st, child1_pos) == ParseStreamPosition(7, 7)

    st = parse_sexpr("(a (b c))")
    @test first_child_position(st, position(st)) == ParseStreamPosition(3, 3)
    child2_pos = last_child_position(st, position(st))
    @test child2_pos == ParseStreamPosition(9, 10)
    @test first_child_position(st, child2_pos) == ParseStreamPosition(6, 6)
    @test last_child_position(st, child2_pos) == ParseStreamPosition(8, 8)

    st = parse_sexpr("( a (b c))")
    @test first_child_position(st, position(st)) == ParseStreamPosition(4, 4)
    child2_pos = last_child_position(st, position(st))
    @test child2_pos == ParseStreamPosition(10, 11)
    @test first_child_position(st, child2_pos) == ParseStreamPosition(7, 7)
    @test last_child_position(st, child2_pos) == ParseStreamPosition(9, 9)

    st = parse_sexpr("a (b c)")
    @test first_child_position(st, position(st)) == ParseStreamPosition(5, 5)
    @test last_child_position(st, position(st)) == ParseStreamPosition(7, 7)

    st = parse_sexpr("(a) (b c)")
    @test first_child_position(st, position(st)) == ParseStreamPosition(7, 8)
    @test last_child_position(st, position(st)) == ParseStreamPosition(9, 10)

    st = parse_sexpr("(() ())")
    @test first_child_position(st, position(st)) == ParseStreamPosition(4, 5)
    @test last_child_position(st, position(st)) == ParseStreamPosition(7, 9)
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
