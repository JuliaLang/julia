module JuliaSyntax

using Mmap

# Internal utilities which aren't related to JuliaSyntax per se.
include("utils.jl")

# Lexing
#
# We're using a git subtree for a modified version of Tokenize.jl, as we need
# several significant changes.
# TODO: Perhaps integrate these back into Tokenize? Or maybe JuliaSyntax would
# be a sensible home for the Tokenize lexer in the future?
include("../Tokenize/src/Tokenize.jl")
using .Tokenize.Tokens: Token
const TzTokens = Tokenize.Tokens
include("tokens.jl")

# Source and diagnostics
include("source_files.jl")
include("diagnostics.jl")

# Parsing
include("parse_stream.jl")
include("parser.jl")
include("parser_api.jl")
include("value_parsing.jl")

# Tree data structures
include("green_tree.jl")
include("syntax_tree.jl")
include("expr.jl")

# Hooks to integrate the parser with Base
include("hooks.jl")

end
