# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import REPL, REPL.REPLCompletions
import Markdown

function get_help_io(input)
    buf = IOBuffer()
    eval(REPL.helpmode(buf, input))
    String(take!(buf))
end
get_help_standard(input) = string(eval(REPL.helpmode(IOBuffer(), input)))

@testset "symbol completion" begin
    @test startswith(get_help_io("α"), "\"α\" can be typed by \\alpha<tab>\n")
    @test startswith(get_help_io("🐨"), "\"🐨\" can be typed by \\:koala:<tab>\n")
    @test startswith(get_help_io("ᵞ₁₂₃¹²³α"), "\"ᵞ₁₂₃¹²³α\" can be typed by \\^gamma<tab>\\_123<tab>\\^123<tab>\\alpha<tab>\n")

    # Check that all symbols with several completions have a canonical mapping (#39148)
    symbols = values(REPLCompletions.latex_symbols)
    duplicates = [v for v in unique(symbols) if count(==(v), symbols) > 1]
    @test all(duplicates .∈ Ref(keys(REPLCompletions.symbols_latex_canonical)))
end

@testset "quoting in doc search" begin
    str = get_help_io("mutable s")
    @test occursin("'mutable struct'", str)
    @test occursin("Couldn't find 'mutable s'", str)
end

@testset "Non-Markdown" begin
    # https://github.com/JuliaLang/julia/issues/37765
    @test isa(REPL.insert_hlines(IOBuffer(), Markdown.Text("foo")), Markdown.Text)
    # https://github.com/JuliaLang/julia/issues/37757
    @test REPL.insert_hlines(IOBuffer(), nothing) === nothing
end

@testset "Check @var_str also completes to var\"\" in REPL.doc_completions()" begin
    checks = ["var", "raw", "r"]
    symbols = "@" .* checks .* "_str"
    results = checks .* "\"\""
    for (i,r) in zip(symbols,results)
        @test r ∈ REPL.doc_completions(i)
    end
end
@testset "fuzzy score" begin
    # https://github.com/JunoLab/FuzzyCompletions.jl/issues/7
    # shouldn't throw when there is a space in a middle of query
    @test (REPL.matchinds("a ", "a file.txt"); true)
    @test isapprox(REPL.fuzzyscore("abcdef", ""), 0.0; atol=0.001)
    @test 0.8 < REPL.fuzzyscore(
    "supercalifragilisticexpialidocious",
    "bupercalifragilisticexpialidocious"
    ) < 1.0

    # Unicode
    @test 1.0 > REPL.fuzzyscore("αkδψm", "αkδm") > 0.0
    @test 1.0 > REPL.fuzzyscore("αkδψm", "α") > 0.0
end

@testset "Unicode doc lookup (#41589)" begin
    @test REPL.lookup_doc(:(÷=)) isa Markdown.MD
end

@testset "#44009" begin
    R = Complex{<:Integer}
    b = REPL.Binding(@__MODULE__, :R)
    @test REPL.summarize(b, Tuple{}) isa Markdown.MD
end

@testset "Struct field help (#51178)" begin
    struct StructWithNoFields end
    struct StructWithOneField
        field1
    end
    struct StructWithTwoFields
        field1
        field2
    end
    struct StructWithThreeFields
        field1
        field2
        field3
    end

    @test endswith(get_help_standard("StructWithNoFields.not_a_field"), "StructWithNoFields` has no fields.\n")
    @test endswith(get_help_standard("StructWithOneField.not_a_field"), "StructWithOneField` has field `field1`.\n")
    @test endswith(get_help_standard("StructWithTwoFields.not_a_field"), "StructWithTwoFields` has fields `field1`, and `field2`.\n")
    @test endswith(get_help_standard("StructWithThreeFields.not_a_field"), "StructWithThreeFields` has fields `field1`, `field2`, and `field3`.\n")
end
