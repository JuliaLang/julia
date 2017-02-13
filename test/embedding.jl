# test that the embedding example runs without error
let
    embedding_bin = joinpath(JULIA_HOME,"..","libexec","embedding")
    lines = readlines(pipeline(`$(embedding_bin)`,
                               stderr=DevNull))
    @test length(lines) == 5
    @test parse(Float64, lines[1]) ≈ sqrt(2)
end
