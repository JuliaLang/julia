using Base.Test

import Tokenize

include("lex_yourself.jl")
@testset "lexer" begin
include("lexer.jl")
end
