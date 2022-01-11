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
    @test isa(REPL.insert_hlines(IOBuffer(), Markdown.Text("foo")), Markdown.Text)
    # https://github.com/JuliaLang/julia/issues/37757
    @test REPL.insert_hlines(IOBuffer(), nothing) === nothing
end

@testset "fuzzy score" begin
    # https://github.com/JunoLab/FuzzyCompletions.jl/issues/7
    # shouldn't throw when there is a space in a middle of query
    @test (REPL.matchinds("a ", "a file.txt"); true)
end

@testset "Unicode doc lookup (#41589)" begin
    @test REPL.lookup_doc(:(÷=)) isa Markdown.MD
end
