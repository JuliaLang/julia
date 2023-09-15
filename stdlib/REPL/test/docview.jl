# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import REPL, REPL.REPLCompletions
import Markdown

@testset "symbol completion" begin
    @test startswith(let buf = IOBuffer()
            Core.eval(Main, REPL.helpmode(buf, "α"))
            String(take!(buf))
        end, "\"α\" can be typed by \\alpha<tab>\n")

    @test startswith(let buf = IOBuffer()
            Core.eval(Main, REPL.helpmode(buf, "🐨"))
            String(take!(buf))
        end, "\"🐨\" can be typed by \\:koala:<tab>\n")

    @test startswith(let buf = IOBuffer()
            Core.eval(Main, REPL.helpmode(buf, "ᵞ₁₂₃¹²³α"))
            String(take!(buf))
        end, "\"ᵞ₁₂₃¹²³α\" can be typed by \\^gamma<tab>\\_123<tab>\\^123<tab>\\alpha<tab>\n")

    # Check that all symbols with several completions have a canonical mapping (#39148)
    symbols = values(REPLCompletions.latex_symbols)
    duplicates = [v for v in unique(symbols) if count(==(v), symbols) > 1]
    @test all(duplicates .∈ Ref(keys(REPLCompletions.symbols_latex_canonical)))
end

@testset "quoting in doc search" begin
    str = let buf = IOBuffer()
        Core.eval(Main, REPL.helpmode(buf, "mutable s"))
        String(take!(buf))
    end
    @test occursin("'mutable struct'", str)
    @test occursin("Couldn't find 'mutable s'", str)
end

@testset "Non-Markdown" begin
    # https://github.com/JuliaLang/julia/issues/37765
    @test isa(REPL.insert_hlines(Markdown.Text("foo")), Markdown.Text)
    # https://github.com/JuliaLang/julia/issues/37757
    @test REPL.insert_hlines(nothing) === nothing
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
        docstring(input) = string(eval(REPL.helpmode(input, @__MODULE__)))

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
