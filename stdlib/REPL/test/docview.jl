# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import REPL, REPL.REPLCompletions
import Markdown

function get_help(input)
    buf = IOBuffer()
    Core.eval(Main, REPL.helpmode(buf, input))
    String(take!(buf))
end

@testset "symbol completion" begin
    @test startswith(get_help("α"), "\"α\" can be typed by \\alpha<tab>\n")
    @test startswith(get_help("🐨"), "\"🐨\" can be typed by \\:koala:<tab>\n")
    @test startswith(get_help("ᵞ₁₂₃¹²³α"), "\"ᵞ₁₂₃¹²³α\" can be typed by \\^gamma<tab>\\_123<tab>\\^123<tab>\\alpha<tab>\n")

    # Check that all symbols with several completions have a canonical mapping (#39148)
    symbols = values(REPLCompletions.latex_symbols)
    duplicates = [v for v in unique(symbols) if count(==(v), symbols) > 1]
    @test all(duplicates .∈ Ref(keys(REPLCompletions.symbols_latex_canonical)))
end

@testset "quoting in doc search" begin
    str = get_help("mutable s")
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

@testset "Struct field help (#?????)" begin
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

    @test get_help("StructWithNoFields.not_a_field")  == "`StructWithNoFields` has no fields."
    @test get_help("StructWithOneField.not_a_field")  == "`StructWithOneField` has field `field1`."
    @test get_help("StructWithTwoFields.not_a_field")  == "`StructWithTwoField` has fields `field1`, and `field2`."
    @test get_help("StructWithThreeFields.not_a_field")  == "`StructWithThreeField` has fields `field1`, `field2`, and `field3`."
end
