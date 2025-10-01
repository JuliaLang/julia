# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test", "tempdepot.jl"))
mkdepottempdir() do dir
    withenv("JULIA_DEPOT_PATH" => dir * (Sys.iswindows() ? ";" : ":"), "JULIA_LOAD_PATH" => nothing) do
        cd(joinpath(@__DIR__, "CompilerLoadingTest")) do
            @test success(pipeline(`$(Base.julia_cmd()[1]) --startup-file=no --project=. compiler_loading_test.jl`; stdout, stderr))
        end
    end
end
