using JuliaSyntax
using Test

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    raw_flags, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

using JuliaSyntax: Kind, @K_str, isliteral, iskeyword, isoperator
using JuliaSyntax: highlight
using JuliaSyntax: ParseStream, bump, peek, emit
using JuliaSyntax: ParseState

# Shortcuts for defining raw syntax nodes

# Trivia nodes
T(k, s) = GreenNode(SyntaxHead(k, raw_flags(trivia=true)), s, )
# Non-trivia nodes
N(k, s) = GreenNode(SyntaxHead(k, raw_flags()), s)
N(k, args::GreenNode...) = GreenNode(SyntaxHead(k, raw_flags()), args...)
# Non-trivia, infix form
NI(k, args::GreenNode...) = GreenNode(SyntaxHead(k, raw_flags(infix=true)), args...)


include("syntax_trees.jl")
include("syntax_interpolation.jl")
include("parse_stream.jl")
include("simple_parser.jl")
include("parser.jl")
