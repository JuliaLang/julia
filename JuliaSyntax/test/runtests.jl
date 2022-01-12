using JuliaSyntax
using Test

using Base.Meta: @dump

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead, parse_all

using JuliaSyntax: Kind, @K_str, is_literal, is_keyword, is_operator
using JuliaSyntax: highlight
using JuliaSyntax: ParseStream,
    peek, peek_token,
    bump, bump_trivia, bump_invisible,
    emit, emit_diagnostic
using JuliaSyntax: ParseState

function parsers_agree_on_file(path)
    code = read(path, String)
    JuliaSyntax.remove_linenums!(JuliaSyntax.parse_all(Expr, code)) == 
    JuliaSyntax.remove_linenums!(JuliaSyntax.flisp_parse_all(code))
end

function find_source_in_path(basedir)
    src_list = String[]
    for (root, dirs, files) in walkdir(basedir)
        append!(src_list, (joinpath(root, f) for f in files if endswith(f, ".jl")))
    end
    src_list
end

function test_parse_all_in_path(basedir)
    for f in find_source_in_path(basedir)
        @testset "Parse $(relpath(f, basedir))" begin
            @test parsers_agree_on_file(f)
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
    using Test
    @testset "Tokenize" begin
        include("../Tokenize/test/runtests.jl")
    end
end

include("parse_stream.jl")
include("parser.jl")

@testset "Parsing values from strings" begin
    include("value_parsing.jl")
end

include("parse_packages.jl")

# Prototypes
#include("syntax_trees.jl")
#include("syntax_interpolation.jl")
#include("simple_parser.jl")
