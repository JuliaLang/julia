module JuliaSyntax

# Use a git subtree for a modified version of Tokenize.jl, as we need several
# significant changes
include("../Tokenize/src/Tokenize.jl")
using .Tokenize.Tokens: RawToken
const TzTokens = Tokenize.Tokens

include("utils.jl")

include("source_files.jl")

include("green_tree.jl")

include("tokens.jl")

include("syntax_tree.jl")
include("parse_stream.jl")

include("parser.jl")

include("hooks.jl")

end
