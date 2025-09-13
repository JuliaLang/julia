# This file is a part of Julia. License is MIT: https://julialang.org/license

# Only run when testing Base compiler
if Base.identify_package("Compiler") === nothing
    mktempdir() do dir
        withenv("JULIA_DEPOT_PATH" => dir * (Sys.iswindows() ? ";" : ":"), "JULIA_LOAD_PATH" => nothing) do
            cd(joinpath(@__DIR__, "CompilerLoadingTest")) do
                @test success(pipeline(`$(Base.julia_cmd()[1]) --startup-file=no --project=. compiler_loading_test.jl`; stdout, stderr))
            end
        end
    end
end
