# Full-scale parsing tests of JuliaSyntax itself, Julia Base, etc.

@testset "Parse JuliaSyntax" begin
    pkgdir = joinpath(@__DIR__, "..")
    test_parse_all_in_path(joinpath(pkgdir, "src"))
    test_parse_all_in_path(joinpath(pkgdir, "test"))
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

if haskey(ENV, "PARSE_BASE_TEST")
# TODO: Turn on by default

base_tests_path = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test")
@testset "Parse Base tests at $base_tests_path" begin
    test_parse_all_in_path(base_tests_path)
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
