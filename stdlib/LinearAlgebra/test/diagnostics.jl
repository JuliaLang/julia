import LinearAlgebra

@testset "__diagnostics__()" begin
    # check that `LinearAlgebra.__diagnostics__(io)` doesn't error, produces some output
    buf = PipeBuffer()
    LinearAlgebra.__diagnostics__(buf)
    output = read(buf, String)
    @test !isempty(strip(output))

    LinearAlgebra.__diagnostics__(stdout) # TODO: delete this line before merging the PR
end
