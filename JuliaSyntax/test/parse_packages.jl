# Full-scale parsing tests of JuliaSyntax itself, Julia Base, etc.

@testset "Parse JuliaSyntax" begin
    pkgdir = joinpath(@__DIR__, "..")
    parse_all_in_path(joinpath(pkgdir, "src"))
    parse_all_in_path(joinpath(pkgdir, "test"))
end

@testset "Parse Base" begin
    parse_all_in_path(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "base"))
end

#=
@testset "Parse Base tests" begin
    parse_all_in_path(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test"))
end

@testset "Parse Julia stdlib" begin
    parse_all_in_path(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "stdlib"))
end
=#
