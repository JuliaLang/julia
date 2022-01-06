
@testset "JuliaSyntax self-parsing" begin
    srcdir = joinpath(@__DIR__, "..", "src")
    @testset "Parse $(joinpath("src",f))" for f in readdir(srcdir)
        code = read(joinpath(srcdir, f), String)
        @test JuliaSyntax.remove_linenums!(JuliaSyntax.parse_all(Expr, code)) == 
              JuliaSyntax.remove_linenums!(JuliaSyntax.flisp_parse_all(code))
    end
end

@testset "JuliaSyntax self-parsing tests" begin
    testdir = @__DIR__
    @testset "Parse $(joinpath("test",f))" for f in readdir(testdir)
        code = read(joinpath(testdir, f), String)
        @test JuliaSyntax.remove_linenums!(JuliaSyntax.parse_all(Expr, code)) == 
              JuliaSyntax.remove_linenums!(JuliaSyntax.flisp_parse_all(code))
    end
end
