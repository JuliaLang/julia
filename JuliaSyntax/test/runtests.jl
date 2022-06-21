using JuliaSyntax
using Test

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

module TokenizeTests
    using Test
    @testset "Tokenize" begin
        include("../Tokenize/test/runtests.jl")
    end
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
