module JuliaSyntax

macro _public(syms)
    if VERSION >= v"1.11"
        names = syms isa Symbol ? [syms] : syms.args
        esc(Expr(:public, names...))
    else
        nothing
    end
end

# Public API, in the order of docs/src/api.md

# Parsing.
export parsestmt,
    parseall,
    parseatom

@_public parse!,
    ParseStream,
    build_tree

# Tokenization
export tokenize,
    Token,
    untokenize

# Source file handling
@_public sourcefile,
    byte_range,
    char_range,
    first_byte,
    last_byte,
    filename,
    source_line,
    source_location,
    sourcetext,
    highlight

export SourceFile
@_public source_line_range

# Expression predicates, kinds and flags
export @K_str, kind
@_public Kind

@_public flags,
    SyntaxHead,
    head,
    is_trivia,
    is_prefix_call,
    is_infix_op_call,
    is_prefix_op_call,
    is_postfix_op_call,
    is_dotted,
    is_suffixed,
    is_decorated,
    numeric_flags,
    has_flags,
    TRIPLE_STRING_FLAG,
    RAW_STRING_FLAG,
    PARENS_FLAG,
    COLON_QUOTE,
    TOPLEVEL_SEMICOLONS_FLAG,
    MUTABLE_FLAG,
    BARE_MODULE_FLAG,
    SHORT_FORM_FUNCTION_FLAG

# Syntax trees
@_public is_leaf,
    numchildren,
    children

export SyntaxNode

@_public GreenNode,
    span

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
