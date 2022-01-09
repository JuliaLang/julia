
@testset "JuliaSyntax Base parsing" begin
    basedir = "/home/chris/dev/julia/base"
    for f in readdir(joinpath(basedir))
        test_parse_file(basedir, f)
    end
end
