
@testset "JuliaSyntax self-parsing" begin
    srcdir = joinpath(@__DIR__, "..", "src")
    @testset "Parse $(joinpath("src",f))" for f in readdir(srcdir)
        code = read(joinpath(srcdir, f), String)
        @test JuliaSyntax.remove_linenums!(JuliaSyntax.parse_all(Expr, code)) == 
              JuliaSyntax.remove_linenums!(JuliaSyntax.flisp_parse_all(code))
    end
end
