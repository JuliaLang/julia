 __precompile__()

module Tokenize

include("token.jl")
include("lexer.jl")

import .Lexers: tokenize
import .Tokens: untokenize

export tokenize

# include("precompile.jl")
# _precompile_()

end # module
