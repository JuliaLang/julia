using JuliaSyntax
using Test

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

@testset "Tokenize" begin
    include("tokenize.jl")
end

include("test_utils.jl")
include("parse_stream.jl")
include("parser.jl")
include("diagnostics.jl")
include("parser_api.jl")
include("expr.jl")
@testset "Parsing literals from strings" begin
    include("value_parsing.jl")
end
include("hooks.jl")
include("parse_packages.jl")
include("source_files.jl")

