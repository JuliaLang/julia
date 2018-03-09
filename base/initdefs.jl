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

function init_depot_path(BINDIR = Sys.BINDIR)
    if haskey(ENV, "JULIA_DEPOT_PATH")
        depots = split(ENV["JULIA_DEPOT_PATH"], Sys.iswindows() ? ';' : ':')
        append!(empty!(DEPOT_PATH), map(expanduser, depots))
    else
        push!(DEPOT_PATH, joinpath(homedir(), ".julia"))
    end
end

## load-path types ##

abstract type AbstractEnv end

struct CurrentEnv <: AbstractEnv
    create::Bool
    CurrentEnv(; create::Bool=false) = new(create)
end

struct NamedEnv <: AbstractEnv
    name::String
    create::Bool
    NamedEnv(name::String; create::Bool=false) = new(name, create)
end

function show(io::IO, env::CurrentEnv)
    print(io, CurrentEnv, "(")
    env.create && print(io, "create=true")
    print(io, ")")
end

function show(io::IO, env::NamedEnv)
    print(io, NamedEnv, "(", repr(env.name))
    env.create && print(io, ", create=true")
    print(io, ")")
end

function parse_env(env::Union{String,SubString{String}})
    isempty(env) && return Any[]
    env == "@" && return CurrentEnv()
    env == "@!" && return CurrentEnv(create=true)
    if env[1] == '@'
        create = env[2] == '!'
        name = env[2+create:end]
        name = replace(name, '#' => VERSION.major, count=1)
        name = replace(name, '#' => VERSION.minor, count=1)
        name = replace(name, '#' => VERSION.patch, count=1)
        return NamedEnv(name, create=create)
    end
    return env # literal path
end

"""
    LOAD_PATH

An array of paths as strings or custom loader objects for the `require`
function and `using` and `import` statements to consider when loading
code.
"""
const LOAD_PATH = Any[]

function parse_load_path(str::String)
    envs = Any[split(str, Sys.iswindows() ? ';' : ':');]
    for (i, env) in enumerate(envs)
        if '|' in env
            envs[i] = Any[parse_env(e) for e in split(env, '|')]
        else
            envs[i] = parse_env(env)
        end
    end
    return envs
end

function init_load_path(BINDIR = Sys.BINDIR)
    if !Base.creating_sysimg
        load_path = get(ENV, "JULIA_LOAD_PATH", "@|@v#.#.#|@v#.#|@v#|@default|@!v#.#")
        append!(empty!(LOAD_PATH), parse_load_path(load_path))
    end
    vers = "v$(VERSION.major).$(VERSION.minor)"
    push!(LOAD_PATH, abspath(BINDIR, "..", "local", "share", "julia", "site", vers))
    push!(LOAD_PATH, abspath(BINDIR, "..", "share", "julia", "site", vers))
end

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
