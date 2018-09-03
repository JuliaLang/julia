# This file is a part of Julia. License is MIT: https://julialang.org/license

## initdefs.jl - initialization and runtime management definitions

"""
    PROGRAM_FILE

A string containing the script name passed to Julia from the command line. Note that the
script name remains unchanged from within included files. Alternatively see
[`@__FILE__`](@ref).
"""
global PROGRAM_FILE = ""

"""
    ARGS

An array of the command line arguments passed to Julia, as strings.
"""
const ARGS = String[]

"""
    exit(code=0)

Stop the program with an exit code. The default exit code is zero, indicating that the
program completed successfully. In an interactive session, `exit()` can be called with
the keyboard shortcut `^D`.
"""
exit(n) = ccall(:jl_exit, Cvoid, (Int32,), n)
exit() = exit(0)

const roottask = current_task()

is_interactive = false

"""
    isinteractive() -> Bool

Determine whether Julia is running an interactive session.
"""
isinteractive() = (is_interactive::Bool)

## package depots (registries, packages, environments) ##

const DEPOT_PATH = String[]

function init_depot_path()
    if haskey(ENV, "JULIA_DEPOT_PATH")
        depots = split(ENV["JULIA_DEPOT_PATH"], Sys.iswindows() ? ';' : ':')
        append!(empty!(DEPOT_PATH), map(expanduser, depots))
    else
        push!(empty!(DEPOT_PATH), joinpath(homedir(), ".julia"))
        push!(DEPOT_PATH, abspath(Sys.BINDIR, "..", "local", "share", "julia"))
        push!(DEPOT_PATH, abspath(Sys.BINDIR, "..", "share", "julia"))
    end
end

## LOAD_PATH, HOME_PROJECT & ACTIVE_PROJECT ##

# JULIA_LOAD_PATH: split on `:` (or `;` on Windows)
# first empty entry is replaced with DEFAULT_LOAD_PATH, the rest are skipped
# entries starting with `@` are named environments:
#  - the first three `#`s in a named environment are replaced with version numbers
#  - `@stdlib` is a special name for the standard library and expands to its path

# if you want a current env setup, use direnv and
# have your .envrc do something like this:
#
#   export JULIA_LOAD_PATH="$(pwd):$JULIA_LOAD_PATH"
#
# this will inherit an existing JULIA_LOAD_PATH value or if there is none, leave
# a trailing empty entry in JULIA_LOAD_PATH which will be replaced with defaults.

const DEFAULT_LOAD_PATH = ["@", "@v#.#", "@stdlib"]

"""
    LOAD_PATH

An array of paths for `using` and `import` statements to consdier as project
environments or package directories when loading code. See Code Loading.
"""
const LOAD_PATH = copy(DEFAULT_LOAD_PATH)
const HOME_PROJECT = Ref{Union{String,Nothing}}(nothing)
const ACTIVE_PROJECT = Ref{Union{String,Nothing}}(nothing)

function current_project(dir::AbstractString)
    # look for project file in current dir and parents
    home = homedir()
    while true
        for proj in project_names
            file = joinpath(dir, proj)
            isfile_casesensitive(file) && return file
        end
        # bail at home directory or top of git repo
        (dir == home || ispath(joinpath(dir, ".git"))) && break
        old, dir = dir, dirname(dir)
        dir == old && break
    end
end

function current_project()
    dir = try pwd()
    catch err
        err isa IOError || rethrow(err)
        return nothing
    end
    return current_project(dir)
end

function parse_load_path(str::String)
    envs = String[]
    isempty(str) && return envs
    for env in split(str, Sys.iswindows() ? ';' : ':')
        if isempty(env)
            for env′ in DEFAULT_LOAD_PATH
                env′ in envs || push!(envs, env′)
            end
        else
            if env == "@."
                env = current_project()
                env === nothing && continue
            end
            env in envs || push!(envs, env)
        end
    end
    return envs
end

function init_load_path()
    if Base.creating_sysimg
        paths = ["@stdlib"]
    elseif haskey(ENV, "JULIA_LOAD_PATH")
        paths = parse_load_path(ENV["JULIA_LOAD_PATH"])
    else
        paths = filter!(env -> env !== nothing,
            [env == "@." ? current_project() : env for env in DEFAULT_LOAD_PATH])
    end
    project = (JLOptions().project != C_NULL ?
        unsafe_string(Base.JLOptions().project) :
        get(ENV, "JULIA_PROJECT", nothing))
    HOME_PROJECT[] =
        project == nothing ? nothing :
        project == "" ? nothing :
        project == "@." ? current_project() : abspath(project)
    append!(empty!(LOAD_PATH), paths)
end

## load path expansion: turn LOAD_PATH entries into concrete paths ##

function load_path_expand(env::AbstractString)::Union{String, Nothing}
    # named environment?
    if startswith(env, '@')
        # `@` in JULIA_LOAD_PATH is expanded early (at startup time)
        # if you put a `@` in LOAD_PATH manually, it's expanded late
        env == "@" && return active_project(false)
        env == "@." && return current_project()
        env == "@stdlib" && return Sys.STDLIB
        env = replace(env, '#' => VERSION.major, count=1)
        env = replace(env, '#' => VERSION.minor, count=1)
        env = replace(env, '#' => VERSION.patch, count=1)
        name = env[2:end]
        # look for named env in each depot
        for depot in DEPOT_PATH
            path = joinpath(depot, "environments", name)
            isdir(path) || continue
            for proj in project_names
                file = abspath(path, proj)
                isfile_casesensitive(file) && return file
            end
            return path
        end
        isempty(DEPOT_PATH) && return nothing
        return abspath(DEPOT_PATH[1], "environments", name, project_names[end])
    end
    # otherwise, it's a path
    path = abspath(env)
    if isdir(path)
        # directory with a project file?
        for proj in project_names
            file = joinpath(path, proj)
            isfile_casesensitive(file) && return file
        end
    end
    # package dir or path to project file
    return path
end
load_path_expand(::Nothing) = nothing

function active_project(search_load_path::Bool=true)
    for project in (ACTIVE_PROJECT[], HOME_PROJECT[])
        project == "@" && continue
        project = load_path_expand(project)
        project === nothing && continue
        if !isfile_casesensitive(project) && basename(project) ∉ project_names
            project = abspath(project, "Project.toml")
        end
        return project
    end
    search_load_path || return
    for project in LOAD_PATH
        project == "@" && continue
        project = load_path_expand(project)
        project === nothing && continue
        isfile_casesensitive(project) && return project
        ispath(project) && continue
        basename(project) in project_names && return project
    end
end

function load_path()
    paths = String[]
    for env in LOAD_PATH
        path = load_path_expand(env)
        path !== nothing && path ∉ paths && push!(paths, path)
    end
    return paths
end

## atexit: register exit hooks ##

const atexit_hooks = []

"""
    atexit(f)

Register a zero-argument function `f()` to be called at process exit. `atexit()` hooks are
called in last in first out (LIFO) order and run before object finalizers.
"""
atexit(f::Function) = (pushfirst!(atexit_hooks, f); nothing)

function _atexit()
    for f in atexit_hooks
        try
            f()
        catch err
            show(stderr, err)
            println(stderr)
        end
    end
end
