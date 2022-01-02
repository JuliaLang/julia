using Test, Printf

import JuliaSyntax.Tokenize

# Takes 10s to run
# include("lex_yourself.jl")
@testset "lexer" begin
include("lexer.jl")
end
