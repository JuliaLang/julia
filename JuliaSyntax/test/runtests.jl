using JuliaSyntax
using Test

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

using JuliaSyntax.Tokenize
using JuliaSyntax.Tokenize.Lexers

@testset "Tokenize" begin
    include("lexer.jl")
end

include("test_utils.jl")
include("parse_stream.jl")
include("parser.jl")
include("parser_api.jl")
include("expr.jl")
@testset "Parsing values from strings" begin
    include("value_parsing.jl")
end
include("hooks.jl")
include("parse_packages.jl")

# Prototypes
#include("syntax_interpolation.jl")
#include("simple_parser.jl")
