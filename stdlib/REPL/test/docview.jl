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
end

@testset "Unicode doc lookup (#41589)" begin
    @test REPL.lookup_doc(:(÷=)) isa Markdown.MD
end

@testset "#44009" begin
    R = Complex{<:Integer}
    b = REPL.Binding(@__MODULE__, :R)
    @test REPL.summarize(b, Tuple{}) isa Markdown.MD
end

@testset "replace_dotaccess_with_f" begin
    function func50105 end
    @test REPL.replace_dotaccess_with_f(func50105, :(print.abc)) == :($func50105(print, :abc))
    @test REPL.replace_dotaccess_with_f(func50105, :(print.())) == :(print.())
    @test REPL.replace_dotaccess_with_f(func50105, :(print.().abc)) == :($func50105(print.(), :abc))
    @test REPL.replace_dotaccess_with_f(func50105, :(println.([1,2], [3,4]))) == :(println.([1,2], [3,4]))
end

@testset "hook_into_nonpublic_getproperty" begin
    module Mod50105
        expr = :(Mod50105.A.B.x.field)
        module A
            module B
                struct T
                    field::Int
                end
                x = T(4)
            end
            public B
        end
        log = []
        logged_expr = REPL.hook_into_nonpublic_getproperty(expr) do m, s
            push!(log, (m, s))
        end
        using Test
        @test eval(logged_expr) == 4
        @test log == [(Mod50105, :A), (Mod50105.A.B, :x)]
    end
end

@testset "moveme" begin
    module A
    end
end
