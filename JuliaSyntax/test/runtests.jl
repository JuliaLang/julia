using JuliaSyntax
using Test

using JuliaSyntax: SourceFile

using JuliaSyntax: GreenNode, SyntaxNode,
    flags, EMPTY_FLAGS, TRIVIA_FLAG, INFIX_FLAG,
    children, child, setchild!, SyntaxHead

include("test_utils.jl")
# Tests for the test_utils go here to allow the utils to be included on their
# own without invoking the tests.
@testset "Test tools" begin
    @test exprs_roughly_equal(Expr(:global, :x, :y),
                              Expr(:global, Expr(:tuple, :x, :y)))
    @test exprs_roughly_equal(Expr(:local, :x, :y),
                              Expr(:local, Expr(:tuple, :x, :y)))
    @test exprs_roughly_equal(1.5,
                              Expr(:call, :*, 1.5, :f))
    @test exprs_roughly_equal(1.5,
                              Expr(:call, :*, 1.5, :f0))
    @test exprs_roughly_equal(Expr(:do, Expr(:macrocall, Symbol("@f"), LineNumberNode(1), Expr(:kw, :a, 1)),
                                   Expr(:->, Expr(:tuple), Expr(:block, LineNumberNode(1)))),
                              Expr(:do, Expr(:macrocall, Symbol("@f"), LineNumberNode(1), Expr(:(=), :a, 1)),
                                   Expr(:->, Expr(:tuple), Expr(:block, LineNumberNode(1)))))
end

@testset "Tokenize" begin
    include("tokenize.jl")
end

include("parse_stream.jl")
include("parser.jl")
include("diagnostics.jl")
include("parser_api.jl")
include("expr.jl")
@testset "Parsing literals from strings" begin
    include("value_parsing.jl")
end
include("source_files.jl")

if VERSION >= v"1.6"
    # Tests restricted to 1.6+ due to
    # * Core._parse hook doesn't exist on v1.5 and lower
    # * Reference parser bugs which would need workarounds for package parse comparisons
    include("hooks.jl")
    include("parse_packages.jl")
end

