# This file is a part of Julia. License is MIT: https://julialang.org/license

## Pkg stuff needed before Pkg has loaded

const Pkg_pkgid = Base.PkgId(Base.UUID("44cfe95a-1eb2-52ea-b672-e2afdf69b78f"), "Pkg")
load_pkg() = Base.require_stdlib(Pkg_pkgid, "REPLExt", REPL)

## Below here copied/tweaked from Pkg Types.jl so that the dummy Pkg prompt
# can populate the env correctly before Pkg loads

function safe_realpath(path)
    isempty(path) && return path
    if ispath(path)
        try
            return realpath(path)
        catch
            return path
        end
    end
    a, b = splitdir(path)
    return joinpath(safe_realpath(a), b)
end

function find_project_file(env::Union{Nothing,String}=nothing)
    project_file = nothing
    if env isa Nothing
        project_file = Base.active_project()
        project_file === nothing && return nothing # in the Pkg version these are pkgerrors
    elseif startswith(env, '@')
        project_file = Base.load_path_expand(env)
        project_file === nothing && return nothing
    elseif env isa String
        if isdir(env)
            isempty(readdir(env)) || return nothing
            project_file = joinpath(env, Base.project_names[end])
        else
            project_file = endswith(env, ".toml") ? abspath(env) :
                abspath(env, Base.project_names[end])
        end
    end
    @assert project_file isa String &&
        (isfile(project_file) || !ispath(project_file) ||
         isdir(project_file) && isempty(readdir(project_file)))
    return safe_realpath(project_file)
end

function find_root_base_project(start_project::String)
    project_file = start_project
    while true
        base_project_file = Base.base_project(project_file)
        base_project_file === nothing && return project_file
        project_file = base_project_file
    end
end

function relative_project_path(project_file::String, path::String)
    # compute path relative the project
    # realpath needed to expand symlinks before taking the relative path
    return relpath(safe_realpath(abspath(path)), safe_realpath(dirname(project_file)))
end

function projname(project_file::String)
    if isfile(project_file)
        name = try
            # The `nothing` here means that this TOML parser does not return proper Dates.jl
            # objects - but that's OK since we're just checking the name here.
            p = Base.TOML.Parser{nothing}()
            Base.TOML.reinit!(p, read(project_file, String); filepath=project_file)
            proj = Base.TOML.parse(p)
            get(proj, "name", nothing)
        catch
            nothing
        end
    else
        name = nothing
    end
    if name === nothing
        name = basename(dirname(project_file))
    end
    for depot in Base.DEPOT_PATH
        envdir = joinpath(depot, "environments")
        if startswith(safe_realpath(project_file), safe_realpath(envdir))
            return "@" * name
        end
    end
    return name
end

prev_project_file = nothing
prev_project_timestamp = nothing
prev_prefix = ""

function Pkg_promptf()
    global prev_project_timestamp, prev_prefix, prev_project_file
    project_file = find_project_file()
    prefix = ""
    if project_file !== nothing
        if prev_project_file == project_file && prev_project_timestamp == mtime(project_file)
            prefix = prev_prefix
        else
            project_name = projname(project_file)
            if project_name !== nothing
                root = find_root_base_project(project_file)
                rootname = projname(root)
                if root !== project_file
                    path_prefix = "/" * dirname(relative_project_path(root, project_file))
                else
                    path_prefix = ""
                end
                if textwidth(rootname) > 30
                    rootname = first(rootname, 27) * "..."
                end
                prefix = "($(rootname)$(path_prefix)) "
                prev_prefix = prefix
                prev_project_timestamp = mtime(project_file)
                prev_project_file = project_file
            end
        end
    end
    # Note no handling of Pkg.offline, as the Pkg version does here
    return "$(prefix)$(PKG_PROMPT)"
end
