function temp_pkg_dir(fn::Function)
    local env_dir
    local old_load_path
    local old_depot_path
    try
        old_load_path = copy(LOAD_PATH)
        old_depot_path = copy(DEPOT_PATH)
        empty!(LOAD_PATH)
        empty!(DEPOT_PATH)
        mktempdir() do env_dir
            mktempdir() do depot_dir
                pushfirst!(LOAD_PATH, env_dir)
                pushfirst!(DEPOT_PATH, depot_dir)
                # Add the standard library paths back
                vers = "v$(VERSION.major).$(VERSION.minor)"
                push!(LOAD_PATH, abspath(Sys.BINDIR, "..", "local", "share", "julia", "site", vers))
                push!(LOAD_PATH, abspath(Sys.BINDIR, "..", "share", "julia", "site", vers))
                fn(env_dir)
            end
        end
    finally
        empty!(LOAD_PATH)
        empty!(DEPOT_PATH)
        append!(LOAD_PATH, old_load_path)
        append!(DEPOT_PATH, old_depot_path)
    end
end

function cd_tempdir(f)
    mktempdir() do tmp
        cd(tmp) do
            f(tmp)
        end
    end
end

isinstalled(pkg) = Base.locate_package(Base.PkgId(pkg.uuid, pkg.name)) !== nothing