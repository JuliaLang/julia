# Full-scale parsing tests of JuliaSyntax itself, Julia Base, etc.

juliasyntax_dir = joinpath(@__DIR__, "..")
@testset "Parse JuliaSyntax" begin
    test_parse_all_in_path(joinpath(juliasyntax_dir, "src"))
end
@testset "Parse JuliaSyntax tests" begin
    test_parse_all_in_path(joinpath(juliasyntax_dir, "test"))
end

base_path = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "base")
isdir(base_path) || error("source for Julia base not found")
@testset "Parse Base at $base_path" begin
    test_parse_all_in_path(base_path) do f
        if endswith(f, "gmp.jl")
            # Loose comparison due to `f(::g(w) = z) = a` syntax
            return exprs_roughly_equal
        end
        return exprs_equal_no_linenum
    end
end

base_tests_path = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test")
@testset "Parse Base tests at $base_tests_path" begin
    test_parse_all_in_path(base_tests_path) do f
        # syntax.jl has some intentionally weird syntax which we parse
        # differently than the flisp parser, and some cases which we've
        # decided are syntax errors.
        if endswith(f, "syntax.jl")
            return nothing
        end

        if endswith(f, "core.jl")
            # The test
            # @test Union{Tuple{T}, Tuple{T,Int}} where {T} === widen_diagonal(Union{Tuple{T}, Tuple{T,Int}} where {T})
            # depends on a JuliaSyntax bugfix and parses differently (wrong) using
            # flisp. This was added in julia#52228 and backported in julia#52045
            return nothing
        end

        # subtype.jl also depends on the where precedence JuliaSyntax bugfix as of julia#53034
        if endswith(f, "subtype.jl") && v"1.11.0-DEV.1382" <= VERSION
            return nothing
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
