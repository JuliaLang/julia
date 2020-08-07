function temp_pkg_dir(fn::Function; rm=true)
    old_load_path = copy(LOAD_PATH)
    old_depot_path = copy(DEPOT_PATH)
    old_home_project = Base.HOME_PROJECT[]
    old_active_project = Base.ACTIVE_PROJECT[]
    try
        empty!(LOAD_PATH)
        empty!(DEPOT_PATH)
        Base.HOME_PROJECT[] = nothing
        Base.ACTIVE_PROJECT[] = nothing
        withenv("JULIA_PROJECT" => nothing,
                "JULIA_LOAD_PATH" => nothing,
                "JULIA_PKG_DEVDIR" => nothing) do
            env_dir = mktempdir()
            depot_dir = mktempdir()
            try
                push!(LOAD_PATH, "@", "@v#.#", "@stdlib")
                push!(DEPOT_PATH, depot_dir)
                Pkg.develop(path=dirname(@__DIR__))
                fn(env_dir)
            finally
                try
                    rm && Base.rm(env_dir; force=true, recursive=true)
                    rm && Base.rm(depot_dir; force=true, recursive=true)
                catch err
                    # Avoid raising an exception here as it will mask the original exception
                    println(Base.stderr, "Exception in finally: $(sprint(showerror, err))")
                end
            end
        end
    finally
        empty!(LOAD_PATH)
        empty!(DEPOT_PATH)
        append!(LOAD_PATH, old_load_path)
        append!(DEPOT_PATH, old_depot_path)
        Base.HOME_PROJECT[] = old_home_project
        Base.ACTIVE_PROJECT[] = old_active_project
    end
end

function install_test_ScratchUsage(project_path::String, version::VersionNumber)
    # Clear out any previously-installed ScratchUsage versions
    rm(joinpath(project_path, "ScratchUsage"); force=true, recursive=true)

    # Copy ScratchUsage into our temporary project path
    mkpath(project_path)
    cp(joinpath(@__DIR__, "ScratchUsage"), joinpath(project_path, "ScratchUsage"))

    # Overwrite the version with the given version (So that we can test our version-specific
    # code within `ScratchUsage`)
    fpath = joinpath(project_path, "ScratchUsage", "Project.toml")
    write(fpath, replace(read(fpath, String), "99.99.99" => string(version)))

    # dev() that path, to add it to the environment, then test it!
    @show Base.active_project()
    Pkg.develop(path=joinpath(project_path, "ScratchUsage"))
    Pkg.test("ScratchUsage")
end