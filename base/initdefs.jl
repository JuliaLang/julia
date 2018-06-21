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

## LOAD_PATH ##

# split on `:` (or `;` on Windows)
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

const DEFAULT_LOAD_PATH = ["@@", "@v#.#", "@stdlib"]

"""
    LOAD_PATH

An array of paths for `using` and `import` statements to consdier as project
environments or package directories when loading code. See Code Loading.
"""
const LOAD_PATH = copy(DEFAULT_LOAD_PATH)

function current_env(dir::AbstractString)
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

function current_env()
    dir = try pwd()
    catch err
        err isa UVError || rethrow(err)
        return nothing
    end
    return current_env(dir)
end

function parse_load_path(str::String)
    envs = String[]
    isempty(str) && return envs
    first_empty = true
    for env in split(str, Sys.iswindows() ? ';' : ':')
        if isempty(env)
            first_empty && append!(envs, DEFAULT_LOAD_PATH)
            first_empty = false
        elseif env == "@" # use "@@" to do delayed expansion
            dir = current_env()
            dir !== nothing && push!(envs, dir)
        else
            push!(envs, env)
        end
    end
    return envs
end

function init_load_path()
    if Base.creating_sysimg
        load_path = ["@stdlib"]
    elseif haskey(ENV, "JULIA_LOAD_PATH")
        load_path = parse_load_path(ENV["JULIA_LOAD_PATH"])
    else
        load_path = filter!(env -> env !== nothing,
            [env == "@" ? current_env() : env for env in DEFAULT_LOAD_PATH])
    end
    append!(empty!(LOAD_PATH), load_path)
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
