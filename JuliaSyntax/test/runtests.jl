using JuliaSyntax
using Test

using Base.Meta: @dump

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    raw_flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

using JuliaSyntax: Kind, @K_str, isliteral, iskeyword, isoperator
using JuliaSyntax: highlight
using JuliaSyntax: ParseStream,
    peek, peek_token,
    bump, bump_trivia, bump_invisible,
    emit, emit_diagnostic
using JuliaSyntax: ParseState

# Shortcuts for defining raw syntax nodes

# Trivia nodes
T(k, s) = GreenNode(SyntaxHead(k, raw_flags(trivia=true)), s, )
# Non-trivia nodes
N(k, s) = GreenNode(SyntaxHead(k, raw_flags()), s)
N(k, args::GreenNode...) = GreenNode(SyntaxHead(k, raw_flags()), args...)
# Non-trivia, infix form
NI(k, args::GreenNode...) = GreenNode(SyntaxHead(k, raw_flags(infix=true)), args...)

include("parse_stream.jl")
include("parser.jl")

# Prototypes
#include("syntax_trees.jl")
#include("syntax_interpolation.jl")
#include("simple_parser.jl")
