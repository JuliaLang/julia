using Base: PkgId, UUID, SHA1, parsed_toml, project_file_name_uuid, project_names,
            project_file_manifest_path, get_deps, preferences_names, isaccessibledir, isfile_casesensitive
using UUIDs

#########################
# Implicit environments #
#########################

struct ImplicitEnvPkg
    path::String # The entry point of the package relative to the implicit environment path
    uuid::Union{Nothing, UUID}
    deps::Union{Nothing, Vector{PkgId}} # Eventual deps of project file of package (in case it has a Project.toml file)
end

# An implicit environment (or package directory) is a folder in the LOAD_PATH without a project file.
# A package X exists in a package directory if the directory contains one of the following "entry point" files:
mutable struct ImplicitEnv
    path::String # mutable so that we can replace the cached STDLIB implicit env with the runtime path
    const pkgs::Dict{String, ImplicitEnvPkg}
end

function _ImplicitEnv(envpath::String)
    envpath = abspath(envpath)
    pkgs = Dict{String, ImplicitEnvPkg}()
    for path in readdir(envpath; join=true)
        dir, ext = splitext(path)
        name = basename(dir)
        pkg = nothing
        # Package defined by a single file X.jl:
        if ext == ".jl" && isfile_casesensitive(path)
            pkg = ImplicitEnvPkg(path, nothing, nothing)
        # Package defined by a folder X/src/X.jl or X.jl/src/X.jl:
        elseif isaccessibledir(path)
            entry_point = joinpath(path, "src", name * ".jl")
            if isfile_casesensitive(entry_point)
                # Does the package have a project file?
                project_file = nothing
                for proj in project_names
                    maybe_project_file = joinpath(dir, proj)
                    isfile_casesensitive(maybe_project_file) && (project_file = maybe_project_file)
                end
                # It did have a project file:
                if project_file !== nothing
                    project_d = parsed_toml(project_file)
                    uuid = project_file_name_uuid(envpath, "").uuid
                    deps = PkgId[]
                    # Get the explicit deps, these are the only deps that can be loaded inside the package:
                    for (name, uuid) in get(Dict{String, Any}, project_d, "deps")
                        uuid = uuid::String
                        push!(deps, PkgId(UUID(uuid), name))
                    end
                    pkg = ImplicitEnvPkg(relpath(entry_point, envpath), uuid, deps)
                # No project file: no uuid and no explicit deps:
                else
                    pkg = ImplicitEnvPkg(relpath(entry_point, envpath), nothing, nothing)
                end
            end
        end
        if pkg !== nothing
            pkgs[name] = pkg
        end
        # TODO: It is possible to both have e.g. a `X.jl` file and a `X/src/X.jl` package which is a name collison.
        # warn about that?
    end
    return ImplicitEnv(envpath, pkgs)
end

# The stdlib environment is an implicit environemnt.
# It is also constant so we might as well just cache it.
const STDLIB_ENVIRONMENT = _ImplicitEnv(Sys.STDLIB)
init_stdlib_path_env() = STDLIB_ENVIRONMENT.path = Sys.STDLIB

function ImplicitEnv(envpath::String)
    if envpath == Sys.STDLIB
        return STDLIB_ENVIRONMENT
    else
        return _ImplicitEnv(envpath)
    end
end

function _identify_package(env::ImplicitEnv, name::String)::Union{Nothing, PkgId}
    pkg = get(env.pkgs, name, nothing)
    pkg === nothing && return nothing
    return PkgId(pkg.uuid, name)
end

function _locate_package(env::ImplicitEnv, pkg::PkgId)::Union{Nothing, String}
    pkg′ = get(env.pkgs, pkg.name, nothing)
    pkg′ === nothing && return nothing
    # We also need to check that an eventual uuid in `pkg` matches the one in the env:
    return pkg′.uuid !== pkg.uuid ? nothing : joinpath(env.path, pkg′.path)
end


#########################
# Explicit environments #
#########################

# An explicit environment is a folder with a `Project.toml` file and (most often)
# a `Manifest.toml` file. The `Project.toml` file describes what can be loaded at
# top-level and the Manifest.toml describes what packages can be loaded in other packages
# as well as how the path is looked up for a package
struct ExplicitEnv
    path::String
    project_deps::Vector{UUID} # [deps] in Project.toml
    project_weakdeps::Vector{UUID} # [weakdeps] in Project.toml
    project_extras::Vector{UUID} # [extras] in Project.toml
    project_extensions::Dict{String, Vector{UUID}} # [exts] in Project.toml
    deps::Dict{UUID, Vector{UUID}} # all dependencies in Manifest.toml
    weakdeps::Dict{UUID, Vector{UUID}} # all weak dependencies in Manifest.toml
    extensions::Dict{UUID, Dict{String, Vector{UUID}}}
    # Lookup name for a UUID
    names::Dict{UUID, String}
    lookup_strategy::Dict{UUID, Union{
                                      SHA1,     # `git-tree-sha1` entry
                                      String,   # `path` entry
                                      Nothing,  # stdlib (no `path` nor `git-tree-sha1`)
                                      Missing}} # not present in the manifest
    #prefs::Union{Nothing, Dict{String, Any}}
    #local_prefs::Union{Nothing, Dict{String, Any}}
end

#=

[[deps.PGFPlotsX]]
deps = ["ArgCheck", "Dates", "DefaultApplication", "DocStringExtensions", "MacroTools", "OrderedCollections", "Parameters", "Requires", "Tables"]
path = "../../../../../../../Users/kristoffercarlsson/JuliaPkgs/PGFPlotsX.jl"
uuid = "8314cec4-20b6-5062-9cdb-752b83310925"
version = "1.6.1"

    [deps.PGFPlotsX.extensions]
    ColorsExt = "Colors"
    ContourExt = "Contour"
    MeasurementsExt = "Measurements"
    StatsBaseExt = "StatsBase"

    [deps.PGFPlotsX.weakdeps]
    Colors = "5ae59095-9a9b-59fe-a467-6f913c188581"
    Contour = "d38c429a-6771-53c6-b99e-75d170b6e991"
    Measurements = "eff96d63-e80a-5855-80a2-b1b0885c5ab7"
    StatsBase = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
=#


function ExplicitEnv(envpath::String)
    envpath = abspath(envpath)
    project_d = parsed_toml(envpath)

    # TODO: Perhaps verify that two packages with the same UUID do not have different names?
    names = Dict{UUID, String}()
    project_uuid_to_name = Dict{String, UUID}()

    project_deps = UUID[]
    project_weakdeps = UUID[]
    project_extras = UUID[]

    # Collect all direct dependencies of the project
    for key in ["deps", "weakdeps", "extras"]
        for (name, _uuid) in get(Dict{String, Any}, project_d, key)::Dict{String, Any}
            v = key == "deps" ? project_deps :
                key == "weakdeps" ? project_weakdeps :
                key == "extras" ? project_extras :
                error()
            uuid = UUID(_uuid)
            push!(v, uuid)
            names[UUID(uuid)] = name
            project_uuid_to_name[name] = UUID(uuid)
        end
    end

    project_extensions = Dict{String, Vector{UUID}}()
    # Collect all extensions of the project
    for (name, triggers::Union{String, Vector{String}}) in get(Dict{String, Any}, project_d, "extensions")::Dict{String, Any}
        if triggers isa String
            triggers = [triggers]
        end
        uuids = UUID[]
        for trigger in triggers
            uuid = get(project_uuid_to_name, trigger, nothing)
            if uuid === nothing
                error("Trigger $trigger for extension $name not found in project")
            end
            push!(uuids, uuid)
        end
        project_extensions[name] = uuids
    end

    # This project might be a package, in that case, that is also a "dependency"
    # of the project.
    proj_name = get(project_d, "name", nothing)::Union{String, Nothing}
    _proj_uuid = get(project_d, "uuid", nothing)::Union{String, Nothing}
    proj_uuid = _proj_uuid === nothing ? nothing : UUID(_proj_uuid)
    if proj_name !== nothing
        # TODO: Error on missing uuid?
        push!(project_deps, UUID(proj_uuid))
        names[UUID(proj_uuid)] = proj_name
    end

    manifest = project_file_manifest_path(envpath)
    manifest_d = manifest === nothing ? Dict{String, Any}() : parsed_toml(manifest)

    # Dependencies in a manifest can either be stored compressed (when name is unique among all packages)
    # in which case it is a `Vector{String}` or expanded where it is a `name => uuid` mapping.
    deps = Dict{UUID, Union{Vector{String}, Vector{UUID}}}()
    weakdeps = Dict{UUID, Union{Vector{String}, Vector{UUID}}}()
    extensions = Dict{UUID, Dict{String, Vector{String}}}()
    name_to_uuid = Dict{String, UUID}()
    lookup_strategy = Dict{UUID, Union{SHA1, String, Nothing, Missing}}()

    sizehint!(deps, length(manifest_d))
    sizehint!(weakdeps, length(manifest_d))
    sizehint!(extensions, length(manifest_d))
    sizehint!(name_to_uuid, length(manifest_d))
    sizehint!(lookup_strategy, length(manifest_d))

    for (name, pkg_infos) in get_deps(manifest_d)
        pkg_infos = pkg_infos::Vector{Any}
        for pkg_info in pkg_infos
            m_uuid = UUID(pkg_info["uuid"]::String)

            # If we have multiple packages with the same name we will overwrite things here
            # but that is fine since we will only use the information in here for packages
            # with unique names
            names[m_uuid] = name
            name_to_uuid[name] = m_uuid

            for key in ["deps", "weakdeps"]
                deps_pkg = get(Vector{String}, pkg_info, key)::Union{Vector{String}, Dict{String, Any}}
                d = key == "deps" ? deps :
                    key == "weakdeps" ? weakdeps :
                    error()

                # Compressed format with unique names:
                if deps_pkg isa Vector{String}
                    d[m_uuid] = deps_pkg
                # Expanded format:
                else
                    uuids = UUID[]
                    for (name_dep, _dep_uuid::String) in deps_pkg
                        dep_uuid = UUID(_dep_uuid)
                        push!(uuids, dep_uuid)
                        names[dep_uuid] = name_dep
                    end
                    d[m_uuid] = uuids
                end
            end

            # Extensions
            deps_pkg = get(Dict{String, Any}, pkg_info, "extensions")::Dict{String, Any}
            for (ext, triggers) in deps_pkg
                triggers = triggers::Union{String, Vector{String}}
                if triggers isa String
                    triggers = [triggers]
                end
                deps_pkg[ext] = triggers
            end
            extensions[m_uuid] = deps_pkg

            # Determine strategy to find package
            lookup_strat = begin
                if (path = get(pkg_info, "path", nothing)::Union{String, Nothing}) !== nothing
                    path
                elseif (git_tree_sha_str = get(pkg_info, "git-tree-sha1", nothing)::Union{String, Nothing}) !== nothing
                    SHA1(git_tree_sha_str)
                else
                    nothing
                end
            end
            lookup_strategy[m_uuid] = lookup_strat
        end
    end

    # No matter if the deps were stored compressed or not in the manifest,
    # we internally store them expanded
    deps_expanded = Dict{UUID, Vector{UUID}}()
    weakdeps_expanded = Dict{UUID, Vector{UUID}}()
    extensions_expanded = Dict{UUID, Dict{String, Vector{UUID}}}()
    sizehint!(deps_expanded, length(deps))
    sizehint!(weakdeps_expanded, length(deps))
    sizehint!(extensions_expanded, length(deps))

    if proj_name !== nothing
        deps_expanded[proj_uuid] = project_deps
        path = get(project_d, "path", nothing)
        entry_point = path !== nothing ? path : dirname(envpath)
        lookup_strategy[proj_uuid] = entry_point
    end

    for key in ["deps", "weakdeps"]
        d = key == "deps" ? deps :
            key == "weakdeps" ? weakdeps :
            error()
        d_expanded = key == "deps" ? deps_expanded :
                     key == "weakdeps" ? weakdeps_expanded :
                     error()
        for (pkg, deps) in d
            # dependencies was already expanded so use it directly:
            if deps isa Vector{UUID}
                d_expanded[pkg] = deps
                for dep in deps
                    name_to_uuid[names[dep]] = dep
                end
            # find the (unique) UUID associated with the name
            else
                deps_pkg = UUID[]
                sizehint!(deps_pkg, length(deps))
                for dep in deps
                    push!(deps_pkg, name_to_uuid[dep])
                end
                d_expanded[pkg] = deps_pkg
            end
        end
    end

    for (pkg, exts) in extensions
        exts_expanded = Dict{String, Vector{UUID}}()
        for (ext, triggers) in exts
            triggers_expanded = UUID[]
            sizehint!(triggers_expanded, length(triggers))
            for trigger in triggers
                push!(triggers_expanded, name_to_uuid[trigger])
            end
            exts_expanded[ext] = triggers_expanded
        end
        extensions_expanded[pkg] = exts_expanded
    end

    # Everything that does not yet have a lookup_strategy is missing from the manifest
    for uuid in project_deps
        get!(lookup_strategy, uuid, missing)
    end

    #=
    # Preferences:
    prefs = get(project_d, "preferences", nothing)

    # `(Julia)LocalPreferences.toml`
    project_dir = dirname(envpath)
    local_prefs = nothing
    for name in preferences_names
        toml_path = joinpath(project_dir, name)
        if isfile(toml_path)
            local_prefs = parsed_toml(toml_path)
            break
        end
    end
    =#

    return ExplicitEnv(envpath, project_deps, project_weakdeps, project_extras,
                       project_extensions, deps_expanded, weakdeps_expanded, extensions_expanded,
                       names, lookup_strategy, #=prefs, local_prefs=#)
end

# Marker to return when we should have been able to load a package but failed.
# At that point, we should not keep searching for the package in other environments
const STOP = :stop

function _identify_package(env::ExplicitEnv, where::PkgId, name::String)::Union{Nothing, PkgId, Symbol}
    where.name == name && return where # Loading X inside X
    where.uuid === nothing && return identify_package(env, name)
    where_pkg = get(env.deps, where.uuid, nothing)
    where_pkg === nothing && return nothing # `where` is not in current env
    uuid = get(where_pkg, name, nothing)
    uuid === nothing && return STOP # we found `where` but not allowed to load `name` in it.
    return PkgId(uuid, name)
end

function _identify_package(env::ExplicitEnv, name::String)::Union{Nothing, PkgId}
    uuid = get(env.project_deps, name, nothing)
    uuid === nothing && return nothing
    return PkgId(uuid, name)
end

const STDLIBS = Set(readdir(Sys.STDLIB::String))

function _locate_package(env::ExplicitEnv, pkg::PkgId)::Union{Nothing, String, Symbol}
    pkg.uuid === nothing && return nothing
    haskey(env.lookup_strategy, pkg.uuid) || return nothing

    lookup_strategy = env.lookup_strategy[pkg.uuid]

    # Not found in manifest:
    if lookup_strategy isa Missing
        return STOP
    # Stdlib:
    elseif lookup_strategy isa Nothing
        # @assert pkg.name in STDLIBS
        # Check that UUID is matching?
        stdlib_path = joinpath(Sys.STDLIB::String, pkg.name, "src", pkg.name * ".jl")
        return isfile_casesensitive(stdlib_path) ? stdlib_path : nothing
    # Path:
    elseif lookup_strategy isa String
        # `path` in a manifest are defined relative the project path
        path = normpath(dirname(env.path), lookup_strategy)
        if isaccessibledir(path)
            path = joinpath(path, "src", pkg.name * ".jl")
            return isfile_casesensitive(path) ? path : STOP
        elseif isfile(path)
            return path
        else
            return STOP
        end
    # Versioned
    elseif lookup_strategy isa SHA1
        hash = lookup_strategy
        # Try find it in a depot
        for slug in (version_slug(pkg.uuid, hash), version_slug(pkg.uuid, hash, 4))
            for depot in DEPOT_PATH
                path = joinpath(depot, "packages", pkg.name, slug, "src", pkg.name * ".jl")
                isfile(path) && return path
            end
        end
        return STOP
    else
        error("unhandled lookup strategy")
    end
end


####################
# EnvironmentStack #
####################

# An environment stack is the stack of environments formed via load_path() (the expanded LOAD_PATH)
struct EnvironmentStack
    load_path::Vector{String}
    envs::Vector{Union{ImplicitEnv, ExplicitEnv}}
end


# Caching

const CACHED_ENV_STACK = Ref{Union{EnvironmentStack, Nothing}}(nothing)

function EnvironmentStack(environments = nothing)
    if CACHED_ENV_STACK[] !== nothing
        return CACHED_ENV_STACK[]
    end
    # Avoid looking up `load_path` until it is really needed.
    if environments === nothing
        environments = load_path()
    end
    envs = Union{ImplicitEnv, ExplicitEnv}[]
    for env in environments
        if isfile(env)
            push!(envs, ExplicitEnv(env))
        elseif isaccessibledir(env)
            push!(envs, ImplicitEnv(env))
        end
    end
    return EnvironmentStack(environments, envs)
end

function identify_package(envstack::EnvironmentStack, where::PkgId, name::String)::Union{Nothing, PkgId, Symbol}
    where.name == name && return where # X loaded in X
    where.uuid === nothing && return identify_package(envstack, name)
    for env in envstack.envs
        if env isa ExplicitEnv
            pkg = _identify_package(env, where, name)
            if pkg === STOP
                return nothing
            elseif pkg isa PkgId
                return pkg
            end
            # keep looking
        else
            where_pkg = get(env.pkgs, where.name, nothing)
            # Found a package with the correct name in the implicit environment:
            if where_pkg !== nothing
                # But wrong uuid, keep looking:
                where_pkg.uuid == where.uuid || continue
                for env in envstack.envs
                    maybe_pkg = _identify_package(env, name)
                    if maybe_pkg !== nothing
                        if where_pkg.deps === nothing || maybe_pkg in where_pkg.deps
                            return maybe_pkg
                        end
                    end
                end
                # Could not find a valid package with the correct name that could be
                # loaded in `where`.
                return nothing
            end
        end
    end
    return nothing
end

function identify_package(envstack::EnvironmentStack, name::String)::Union{Nothing, PkgId}
    for env in envstack.envs
        pkg = _identify_package(env, name)
        pkg !== nothing && return pkg
    end
    return nothing
end

function locate_package(envstack::EnvironmentStack, pkg::PkgId)::Union{Nothing, String}
    for env in envstack.envs
        path = _locate_package(env, pkg)
        path === STOP && return nothing
        path !== nothing && return path
    end
    return nothing
end

"""
    Base.identify_package(name::String)::Union{PkgId, Nothing}
    Base.identify_package(where::Union{Module,PkgId}, name::String)::Union{PkgId, Nothing}

Identify the package by its name from the current environment stack, returning
its `PkgId`, or `nothing` if it cannot be found.

If only the `name` argument is provided, it searches each environment in the
stack and its named direct dependencies.

There `where` argument provides the context from where to search for the
package: in this case it first checks if the name matches the context itself,
otherwise it searches all recursive dependencies (from the resolved manifest of
each environment) until it locates the context `where`, and from there
identifies the dependency with with the corresponding name.

```julia-repl
julia> Base.identify_package("Pkg") # Pkg is a dependency of the default environment
Pkg [44cfe95a-1eb2-52ea-b672-e2afdf69b78f]

julia> using LinearAlgebra

julia> Base.identify_package(LinearAlgebra, "Pkg") # Pkg is not a dependency of LinearAlgebra
```
"""
identify_package(where::PkgId, name::String) = identify_package(EnvironmentStack(), where, name)
identify_package(name::String) = identify_package(EnvironmentStack(), name)

identify_package(where::Module, name::String) = identify_package(PkgId(where), name)


"""
    Base.locate_package(pkg::PkgId)::Union{String, Nothing}

The path to the entry-point file for the package corresponding to the identifier
`pkg`, or `nothing` if not found. See also [`identify_package`](@ref).

```julia-repl
julia> pkg = Base.identify_package("Pkg")
Pkg [44cfe95a-1eb2-52ea-b672-e2afdf69b78f]

julia> Base.locate_package(pkg)
"/path/to/julia/stdlib/v$(VERSION.major).$(VERSION.minor)/Pkg/src/Pkg.jl"
```
"""
locate_package(pkg::PkgId) = locate_package(EnvironmentStack(), pkg)

# Used by Pkg but not used in loading itself
function find_package(envstack::EnvironmentStack, arg)
    pkg = identify_package(envstack, arg)
    pkg === nothing && return nothing
    return locate_package(envstack, pkg)
end
find_package(arg) = find_package(EnvironmentStack(), arg)
