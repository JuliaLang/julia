# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import REPL, REPL.REPLCompletions
import Markdown

function get_help_io(input, mod=Main)
    buf = IOBuffer()
    eval(REPL.helpmode(buf, input, mod))
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

@testset "non-loaded packages in doc search" begin
    temp_package = mktempdir()
    write(joinpath(temp_package, "Project.toml"),
        """
        name = "FooPackage"
        uuid = "2e6e0b2d-0e7f-4b7f-9f3b-6f3f3f3f3f3f"
        """)
    mkpath(joinpath(temp_package, "src"))
    write(joinpath(temp_package, "src", "FooPackage.jl"),
        """
        module FooPackage
        end
        """)
    push!(LOAD_PATH, temp_package)
    str = get_help_io("FooPackage")
    @test occursin("Couldn't find FooPackage, but a loadable package with that name exists.", str)
    @test pop!(LOAD_PATH) == temp_package
end

@testset "Check @var_str also completes to var\"\" in REPL.doc_completions()" begin
    checks = ["var", "raw", "r"]
    symbols = "@" .* checks .* "_str"
    results = checks .* "\"\""
    for (i,r) in zip(symbols,results)
        @test r ∈ string.(REPL.doc_completions(i))
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

    exact_match_export = REPL.fuzzyscore("thing", REPL.AccessibleBinding(:thing))
    exact_match_public = REPL.fuzzyscore("thing", REPL.AccessibleBinding("A", "thing"))
    inexact_match_export = REPL.fuzzyscore("thing", REPL.AccessibleBinding(:thang))
    inexact_match_public = REPL.fuzzyscore("thing", REPL.AccessibleBinding("A", "thang"))
    @test exact_match_export > exact_match_public > inexact_match_export > inexact_match_public
    @test exact_match_export ≈ 1.0
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

    # Shouldn't error if the struct doesn't have any field documentations at all.
    @test endswith(get_help_standard("Int.not_a_field"), "`$Int` has no fields.\n")
end

@testset "Parametric struct field help (#59524)" begin
    "NonParametricStruct docstring"
    struct NonParametricStruct
        "field_x docstring"
        field_x::Float64
    end

    "ParametricStruct docstring"
    struct ParametricStruct{T<:Real}
        "field_y docstring"
        field_y::T
    end

    @test occursin("field_x docstring", get_help_standard("NonParametricStruct.field_x"))
    @test occursin("field_y docstring", get_help_standard("ParametricStruct.field_y"))
    @test endswith(get_help_standard("ParametricStruct.not_a_field"), "ParametricStruct` has field `field_y`.\n")
end

module InternalWarningsTests

    module A
        public B, B3
        module B
            public e
            c = 4
            "d is 5"
            d = 5
            "e is 6"
            e = 6
        end

        module B2
            module C
                public e
                d = 1
                "e is 2"
                e = 2
            end
        end

        module B3 end
    end

    using Test, REPL
    @testset "internal warnings" begin
        header = "!!! warning\n    The following bindings may be internal; they may change or be removed in future versions:\n\n"
        prefix(warnings) = header * join("      * `$(@__MODULE__).$w`\n" for w in warnings) * "\n\n"
        docstring(input) = string(eval(REPL.helpmode(IOBuffer(), input, @__MODULE__)))

        @test docstring("A") == "No docstring or readme file found for internal module `$(@__MODULE__).A`.\n\n# Public names\n\n`B`, `B3`\n"
        @test docstring("A.B") == "No docstring or readme file found for public module `$(@__MODULE__).A.B`.\n\n# Public names\n\n`e`\n"
        @test startswith(docstring("A.B.c"), prefix(["A.B.c"]))
        @test startswith(docstring("A.B.d"), prefix(["A.B.d"]))
        @test docstring("A.B.e") == "e is 6\n"
        @test startswith(docstring("A.B2"), prefix(["A.B2"]))
        @test startswith(docstring("A.B2.C"), prefix(["A.B2", "A.B2.C"]))
        @test startswith(docstring("A.B2.C.d"), prefix(["A.B2", "A.B2.C", "A.B2.C.d"]))
        @test startswith(docstring("A.B2.C.e"), prefix(["A.B2", "A.B2.C"]))
        @test docstring("A.B3") == "No docstring or readme file found for public module `$(@__MODULE__).A.B3`.\n\nModule does not have any public names.\n"
    end
end

# Issue #51344, don't print "internal binding" warning for non-existent bindings.
@test string(eval(REPL.helpmode("Base.no_such_symbol"))) == "No documentation found.\n\nBinding `Base.no_such_symbol` does not exist.\n"

module TestSuggestPublic
    export dingo
    public dango
    dingo(x) = x + 1
    dango(x) = x = 2
end
using .TestSuggestPublic
helplines(s) = map(strip, split(get_help_io(s, @__MODULE__), '\n'; keepempty=false))
@testset "search lists public names" begin
    lines = helplines("dango")
    # Ensure that public names that exactly match the search query are listed first
    # even if they aren't exported, as long as no exact exported/local match exists
    @test startswith(lines[1], "search: TestSuggestPublic.dango dingo")
    @test lines[2] == "Couldn't find dango"  # 🙈🍡
    @test startswith(lines[3], "Perhaps you meant TestSuggestPublic.dango, dingo")
end
dango() = "🍡"
@testset "search prioritizes exported names" begin
    # Prioritize exported/local names if they exactly match
    lines = helplines("dango")
    @test startswith(lines[1], "search: dango TestSuggestPublic.dango dingo")
end
