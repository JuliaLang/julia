module JuliaSyntax

# Conservative list of exports - only export the most common/useful things
# here.

# Parsing. See also
#   parse!(), ParseStream
export parsestmt, parseall, parseatom
# Tokenization
export tokenize, Token, untokenize
# Source file handling. See also
#   highlight() sourcetext() source_line() source_location()
export SourceFile
# Expression heads/kinds. See also
#   flags() and related predicates.
export @K_str, kind, head
# Syntax tree types. See also
#   GreenNode
export SyntaxNode

# Helper utilities
include("utils.jl")

include("kinds.jl")

# Lexing uses a significantly modified version of Tokenize.jl
include("tokenize.jl")

# Source and diagnostics
include("source_files.jl")
include("diagnostics.jl")

# Parsing
include("parse_stream.jl")
include("parser.jl")
include("parser_api.jl")
include("literal_parsing.jl")

# Tree data structures
include("green_tree.jl")
include("syntax_tree.jl")
include("expr.jl")

# Hooks to integrate the parser with Base
include("hooks.jl")
include("precompile.jl")

end
