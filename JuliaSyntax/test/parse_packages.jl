# Full-scale parsing tests of JuliaSyntax itself, Julia Base, etc.

@testset "Parse JuliaSyntax" begin
    pkgdir = joinpath(@__DIR__, "..")
    test_parse_all_in_path(joinpath(pkgdir, "src"))
    test_parse_all_in_path(joinpath(pkgdir, "test"))
end

base_path = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "base") 
@testset "Parse Base at $base_path" begin
    test_parse_all_in_path(base_path)
end

#=
base_tests_path = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test")
@testset "Parse Base tests at $base_tests_path" begin
    test_parse_all_in_path(base_tests_path)
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
=#
