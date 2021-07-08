using Test

const repository_root = dirname(dirname(@__DIR__))

for filename in ARGS
    path = joinpath(repository_root, filename)
    @info "Starting $(filename)"
    try
        @testset "$(filename)" begin
            include(path)
        end
    catch ex
        @error "" exception=(ex, catch_backtrace())
    end
    @info "Finished $(filename)"
end
