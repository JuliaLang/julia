# Prototype ParseStream interface
#
# Here we test the ParseStream interface, by taking input code and checking
# that the correct sequence of emit() and bump() produces a valid parse tree.

using JuliaSyntax: ParseStream,
    peek, peek_token,
    bump, bump_trivia, bump_invisible,
    emit, emit_diagnostic, TRIVIA_FLAG, INFIX_FLAG

code = """
for i = 1:10
    xx[i] + 2
    # hi
    yy
end
"""

st = ParseStream(code)

# Here we manually issue parse events in the order the Julia parser would issue
# them
@testset "ParseStream" begin
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

    @test build_tree(GreenNode, st) isa JuliaSyntax.GreenNode
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
