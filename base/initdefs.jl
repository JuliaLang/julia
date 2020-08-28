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

"""
    DEPOT_PATH

A stack of "depot" locations where the package manager, as well as Julia's code
loading mechanisms, look for package registries, installed packages, named
environments, repo clones, cached compiled package images, and configuration
files. By default it includes:

1. `~/.julia` where `~` is the user home as appropriate on the system;
2. an architecture-specific shared system directory, e.g. `/usr/local/share/julia`;
3. an architecture-independent shared system directory, e.g. `/usr/share/julia`.

So `DEPOT_PATH` might be:
```julia
[joinpath(homedir(), ".julia"), "/usr/local/share/julia", "/usr/share/julia"]
```
The first entry is the "user depot" and should be writable by and owned by the
current user. The user depot is where: registries are cloned, new package versions
are installed, named environments are created and updated, package repos are cloned,
newly compiled package image files are saved, log files are written, development
packages are checked out by default, and global configuration data is saved. Later
entries in the depot path are treated as read-only and are appropriate for
registries, packages, etc. installed and managed by system administrators.

`DEPOT_PATH` is populated based on the [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH)
environment variable if set.

See also:
[`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH), and
[Code Loading](@ref Code-Loading).
"""
const DEPOT_PATH = String[]

function append_default_depot_path!(DEPOT_PATH)
    path = joinpath(homedir(), ".julia")
    path in DEPOT_PATH || push!(DEPOT_PATH, path)
    path = abspath(Sys.BINDIR, "..", "local", "share", "julia")
    path in DEPOT_PATH || push!(DEPOT_PATH, path)
    path = abspath(Sys.BINDIR, "..", "share", "julia")
    path in DEPOT_PATH || push!(DEPOT_PATH, path)
end

function init_depot_path()
    empty!(DEPOT_PATH)
    if haskey(ENV, "JULIA_DEPOT_PATH")
        str = ENV["JULIA_DEPOT_PATH"]
        isempty(str) && return
        for path in split(str, Sys.iswindows() ? ';' : ':')
            if isempty(path)
                append_default_depot_path!(DEPOT_PATH)
            else
                path = expanduser(path)
                path in DEPOT_PATH || push!(DEPOT_PATH, path)
            end
        end
    else
        append_default_depot_path!(DEPOT_PATH)
    end
end

## LOAD_PATH & ACTIVE_PROJECT ##

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

An array of paths for `using` and `import` statements to consider as project
environments or package directories when loading code. It is populated based on
the [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH) environment variable if set;
otherwise it defaults to `["@", "@v#.#", "@stdlib"]`. Entries starting with `@`
have special meanings:

- `@` refers to the "current active environment", the initial value of which is
  initially determined by the [`JULIA_PROJECT`](@ref JULIA_PROJECT) environment
  variable or the `--project` command-line option.

- `@stdlib` expands to the absolute path of the current Julia installation's
  standard library directory.

- `@name` refers to a named environment, which are stored in depots (see
  [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH)) under the `environments`
  subdirectory. The user's named environments are stored in
  `~/.julia/environments` so `@name` would refer to the environment in
  `~/.julia/environments/name` if it exists and contains a `Project.toml` file.
  If `name` contains `#` characters, then they are replaced with the major, minor
  and patch components of the Julia version number. For example, if you are
  running Julia 1.2 then `@v#.#` expands to `@v1.2` and will look for an
  environment by that name, typically at `~/.julia/environments/v1.2`.

The fully expanded value of `LOAD_PATH` that is searched for projects and packages
can be seen by calling the `Base.load_path()` function.

See also:
[`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH),
[`JULIA_PROJECT`](@ref JULIA_PROJECT),
[`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH), and
[Code Loading](@ref Code-Loading).
"""
const LOAD_PATH = copy(DEFAULT_LOAD_PATH)
# HOME_PROJECT is no longer used, here just to avoid breaking things
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
        # bail at home directory
        dir == home && break
        old, dir = dir, dirname(dir)
        dir == old && break
    end
end

function current_project()
    dir = try pwd()
    catch err
        err isa IOError || rethrow()
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
            env = expanduser(env)
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
            String[env == "@." ? current_project() : env for env in DEFAULT_LOAD_PATH])
    end
    append!(empty!(LOAD_PATH), paths)
end

function init_active_project()
    project = (JLOptions().project != C_NULL ?
        unsafe_string(Base.JLOptions().project) :
        get(ENV, "JULIA_PROJECT", nothing))
    ACTIVE_PROJECT[] =
        project === nothing ? nothing :
        project == "" ? nothing :
        project == "@." ? current_project() : abspath(expanduser(project))
end

## load path expansion: turn LOAD_PATH entries into concrete paths ##

function load_path_expand(env::AbstractString)::Union{String, Nothing}
    # named environment?
    if startswith(env, '@')
        # `@` in JULIA_LOAD_PATH is expanded early (at startup time)
        # if you put a `@` in LOAD_PATH manually, it's expanded late
        env == "@" && return active_project(false)
        env == "@." && return current_project()
        env == "@stdlib" && return Sys.STDLIB::String
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
    for project in (ACTIVE_PROJECT[],)
        project == "@" && continue
        project = load_path_expand(project)
        project === nothing && continue
        # while this seems well-inferred, nevertheless without the type annotation below
        # there are backedges here from abspath(::AbstractString, ::String)
        project = project::String
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

const atexit_hooks = Callable[
    () -> Filesystem.temp_cleanup_purge(force=true)
]

"""
    atexit(f)

Register a zero-argument function `f()` to be called at process exit. `atexit()` hooks are
called in last in first out (LIFO) order and run before object finalizers.

Exit hooks are allowed to call `exit(n)`, in which case Julia will exit with
exit code `n` (instead of the original exit code). If more than one exit hook
calls `exit(n)`, then Julia will exit with the exit code corresponding to the
last called exit hook that calls `exit(n)`. (Because exit hooks are called in
LIFO order, "last called" is equivalent to "first registered".)
"""
atexit(f::Function) = (pushfirst!(atexit_hooks, f); nothing)

function _atexit()
    while !isempty(atexit_hooks)
        f = popfirst!(atexit_hooks)
        try
            f()
        catch ex
            showerror(stderr, ex)
            Base.show_backtrace(stderr, catch_backtrace())
            println(stderr)
        end
    end
end

## hook for disabling threaded libraries ##

library_threading_enabled = true
const disable_library_threading_hooks = []

function at_disable_library_threading(f)
    push!(disable_library_threading_hooks, f)
    if !library_threading_enabled
        disable_library_threading()
    end
    return
end

function disable_library_threading()
    global library_threading_enabled = false
    while !isempty(disable_library_threading_hooks)
        f = pop!(disable_library_threading_hooks)
        try
            f()
        catch err
            @warn("a hook from a library to disable threading failed:",
                  exception = (err, catch_backtrace()))
        end
    end
    return
end
