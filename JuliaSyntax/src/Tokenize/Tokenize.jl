module Tokenize

if isdefined(Base, :Experimental) && isdefined(Base.Experimental, Symbol("@optlevel"))
    @eval Base.Experimental.@optlevel 1
end

include("token.jl")
include("lexer.jl")

import .Lexers: tokenize
import .Tokens: untokenize

export tokenize, untokenize, Tokens

end # module
