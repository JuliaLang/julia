@testset "JuliaSyntax self-parsing" begin
    pkgdir = joinpath(@__DIR__, "..")
    for f in readdir(joinpath(pkgdir, "src"))
        test_parse_file(pkgdir, joinpath("src",f))
    end

    for f in readdir(joinpath(pkgdir, "test"))
        test_parse_file(pkgdir, joinpath("test",f))
    end
end

