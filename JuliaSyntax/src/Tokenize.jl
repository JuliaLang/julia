 __precompile__()

module Tokenize

include("token.jl")
include("lexer.jl")

import .Lexers: tokenize
import .Tokens: untokenize

export tokenize

if VERSION > v"0.5-"
    include("precompile.jl")
    _precompile_()
end

end # module
