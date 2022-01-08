using JuliaSyntax
using Test

using Base.Meta: @dump

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

using JuliaSyntax: Kind, @K_str, is_literal, is_keyword, is_operator
using JuliaSyntax: highlight
using JuliaSyntax: ParseStream,
    peek, peek_token,
    bump, bump_trivia, bump_invisible,
    emit, emit_diagnostic
using JuliaSyntax: ParseState

function test_parse_file(root_path, path)
    fullpath = joinpath(root_path, path)
    if endswith(path, ".jl") && isfile(fullpath)
        @testset "Parse $path" begin
            code = read(fullpath, String)
            @test JuliaSyntax.remove_linenums!(JuliaSyntax.parse_all(Expr, code)) == 
                  JuliaSyntax.remove_linenums!(JuliaSyntax.flisp_parse_all(code))
        end
    end
end

# Shortcuts for defining raw syntax nodes

# Trivia nodes
T(k, s) = GreenNode(SyntaxHead(k, flags(trivia=true)), s, )
# Non-trivia nodes
N(k, s) = GreenNode(SyntaxHead(k, flags()), s)
N(k, args::GreenNode...) = GreenNode(SyntaxHead(k, flags()), args...)
# Non-trivia, infix form
NI(k, args::GreenNode...) = GreenNode(SyntaxHead(k, flags(infix=true)), args...)

module TokenizeTests
    include("../Tokenize/test/runtests.jl")
end

include("parse_stream.jl")
include("parser.jl")
include("value_parsing.jl")
include("self_parse.jl")

# Prototypes
#include("syntax_trees.jl")
#include("syntax_interpolation.jl")
#include("simple_parser.jl")
