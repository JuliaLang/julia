function test_parse_file(root_path, path)
    fullpath = joinpath(root_path, path)
    if endswith(path, ".jl") && isfile(fullpath)
        @testset "Parse $path" begin
            code = read(fullpath, String)
            @test JuliaSyntax.remove_linenums!(JuliaSyntax.parse_all(Expr, code)) == 
                  JuliaSyntax.remove_linenums!(JuliaSyntax.flisp_parse_all(code))
        end
    end
end

@testset "JuliaSyntax self-parsing" begin
    pkgdir = joinpath(@__DIR__, "..")
    for f in readdir(joinpath(pkgdir, "src"))
        test_parse_file(pkgdir, joinpath("src",f))
    end

    for f in readdir(joinpath(pkgdir, "test"))
        test_parse_file(pkgdir, joinpath("test",f))
    end
end

#=
@testset "JuliaSyntax Base parsing" begin
    basedir = "/home/chris/dev/julia/base"
    for f in readdir(joinpath(basedir))
        test_parse_file(basedir, f)
    end
end
=#
