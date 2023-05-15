using JuliaSyntax
using Test

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

include("test_utils.jl")
include("test_utils_tests.jl")
include("fuzz_test.jl")

include("utils.jl")

@testset "Tokenize" begin
    include("tokenize.jl")
end

include("parse_stream.jl")
include("parser.jl")
include("green_node.jl")
include("syntax_tree.jl")
include("diagnostics.jl")
include("parser_api.jl")
include("expr.jl")
@testset "Parsing literals from strings" begin
    include("literal_parsing.jl")
end
include("source_files.jl")

if VERSION >= v"1.6"
    # Tests restricted to 1.6+ due to
    # * Core._parse hook doesn't exist on v1.5 and lower
    # * Reference parser bugs which would need workarounds for package parse comparisons
    include("hooks.jl")
    include("parse_packages.jl")
end

