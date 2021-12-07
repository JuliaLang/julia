# Prototype ParseStream interface
#
# Here we test the ParseStream interface, by taking input code and checking
# that the correct sequence of emit() and bump() produces a valid parse tree.

code = """
for i = 1:10
    xx[i] + 2
    # hi
    yy
end
"""

st = ParseStream(code)

# Here we manually issue parse events in the order a Julia parser would issue
# them (if such a parser existed... which it doesn't yet!)
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
            @test peek(st) == K"Identifier" # 'yy'
            bump(st)
        emit(st, p4, K"block")
        bump(st, TRIVIA_FLAG) # end
    emit(st, p1, K"for")
    bump(st, TRIVIA_FLAG) # \n
    emit(st, p1, K"toplevel")
end

t = JuliaSyntax.to_raw_tree(st)

# ## Input code
println("-----------------------")
print(code)
println()

# ## Output tree
show(stdout, MIME"text/plain"(), t, code, show_trivia=true)
