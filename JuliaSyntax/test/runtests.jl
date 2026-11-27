if !(@isdefined JuliaSyntax)
    using JuliaSyntax
end

using Test

# JuliaSyntax is not only tested on master
if isdefined(Test, :detect_closure_boxes)
    @test isempty(Test.detect_closure_boxes(JuliaSyntax))
end

include("test_utils.jl")
include("test_utils_tests.jl")
include("fuzz_test.jl")

include("utils.jl")
include("kinds.jl")

@testset "Tokenize" begin
    include("tokenize.jl")
end

include("parse_stream.jl")
include("parser.jl")
include("green_node.jl")
include("syntax_node.jl")
if isdefined(Base, :Syntax)
    include("syntax.jl")
end
include("diagnostics.jl")
include("parser_api.jl")
include("expr.jl")
@testset "Parsing literals from strings" begin
    include("literal_parsing.jl")
end
include("source_files.jl")

include("hooks.jl")
# Comparison with the reference flisp parser, in julia versions which have it
if JuliaSyntax._has_flisp
    include("parse_packages.jl")
end

include("serialization.jl")

@static if isdefined(Base, :Syntax)
    @testset "Basic inference tests" begin
        @test Base.infer_return_type(JuliaSyntax.sourcetext, (JuliaSyntax.Syntax,)) <: AbstractString
        @test Base.infer_return_type(JuliaSyntax.byte_range, (JuliaSyntax.Syntax,)) == UnitRange{Int}
        @test Base.infer_return_type(JuliaSyntax.hasproperty, (JuliaSyntax.Syntax,Symbol)) == Bool
    end
end
