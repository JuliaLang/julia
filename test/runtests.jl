using Pkg3
using Base.Test
using Pkg3.Types

function temp_pkg_dir(fn::Function)
    local depot
    local project_path
    try
        depot = joinpath(tempdir(), randstring())
        empty!(Pkg3.DEPOTS)
        push!(Pkg3.DEPOTS, depot)
        project_path = joinpath(tempdir(), randstring())

        withenv("JULIA_ENV" => project_path) do
            fn()
        end
    finally
        rm(depot, recursive=true, force=true)
        rm(project_path, recursive=true, force=true)
    end
end

temp_pkg_dir() do
    Pkg3.API.add("Example")
    @eval import Example
    Pkg3.API.up()
    Pkg3.API.rm("Example")

    nonexisting_pkg = randstring(14)
    @test_throws CommandError Pkg3.API.add(nonexisting_pkg)
    @test_throws CommandError Pkg3.API.up(nonexisting_pkg)
    @test_warn "not in project" Pkg3.API.rm(nonexisting_pkg)
end

