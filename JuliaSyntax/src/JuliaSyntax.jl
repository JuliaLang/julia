module JuliaSyntax

import Tokenize
using Tokenize.Tokens: RawToken
const TzTokens = Tokenize.Tokens

include("utils.jl")

include("source_files.jl")

include("token_kinds.jl")
include("lexer.jl")

include("syntax_tree.jl")
include("parser.jl")

end
