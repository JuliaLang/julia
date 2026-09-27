@testset "IR tests" begin
    testdir = @__DIR__
    for filename in readdir(testdir)
        if endswith(filename, "_ir.jl")
            @testset "$filename" begin
                test_ir_cases(joinpath(testdir, filename))
            end
        end
    end
end
