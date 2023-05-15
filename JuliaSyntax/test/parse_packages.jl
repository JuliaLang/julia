# Full-scale parsing tests of JuliaSyntax itself, Julia Base, etc.

juliasyntax_dir = joinpath(@__DIR__, "..")
@testset "Parse JuliaSyntax" begin
    test_parse_all_in_path(joinpath(juliasyntax_dir, "src"))
end
@testset "Parse JuliaSyntax tests" begin
    test_parse_all_in_path(joinpath(juliasyntax_dir, "test"))
end

base_path = let
    p = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "base") 
    if !isdir(p)
        # For julia 1.9 images.
        p = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "src", "base") 
        if !isdir(p)
            error("source for Julia base not found")
        end
    end
    p
end
@testset "Parse Base at $base_path" begin
    test_parse_all_in_path(base_path)
end

base_tests_path = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test")
@testset "Parse Base tests at $base_tests_path" begin
    test_parse_all_in_path(base_tests_path) do f
        # In julia-1.6, test/copy.jl had spurious syntax which became the
        # multidimensional array syntax in 1.7.
        if endswith(f, "copy.jl") && v"1.6" <= VERSION < v"1.7"
            return nothing
        end

        # syntax.jl has some intentially weird syntax which we parse
        # differently than the flisp parser, and some cases which we've
        # decided are syntax errors.
        if endswith(f, "syntax.jl")
            return nothing
        end

        if endswith(f, "core.jl")
            # Loose comparison due to `for f() = 1:3` syntax
            return exprs_roughly_equal
        end

        return exprs_equal_no_linenum
    end
end

@testset "Parse Julia stdlib at $(Sys.STDLIB)" begin
    for stdlib in readdir(Sys.STDLIB)
        fulldir = joinpath(Sys.STDLIB, stdlib)
        if isdir(fulldir)
            @testset "Parse $stdlib" begin
                test_parse_all_in_path(joinpath(Sys.STDLIB, fulldir))
            end
        end
    end
end
