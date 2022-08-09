module JuliaSyntax

using Mmap

# Helper utilities
include("utils.jl")

include("kinds.jl")

# Lexing uses a significantly modified version of Tokenize.jl
include("Tokenize/Tokenize.jl")
using .Tokenize: Token

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
