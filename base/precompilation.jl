module Precompilation

using Base: PkgId, UUID, SHA1, parsed_toml, project_file_name_uuid, project_names,
            project_file_manifest_path, get_deps, preferences_names, isaccessibledir, isfile_casesensitive,
            base_project, isdefined

# This is currently only used for pkgprecompile but the plan is to use this in code loading in the future
# see the `kc/codeloading2.0` branch
struct ExplicitEnv
    path::String
    project_deps::Dict{String, UUID} # [deps] in Project.toml
    project_weakdeps::Dict{String, UUID} # [weakdeps] in Project.toml
    project_extras::Dict{String, UUID} # [extras] in Project.toml
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

function ExplicitEnv(envpath::String=Base.active_project())
    if !isfile(envpath)
        error("expected a project file at $(repr(envpath))")
    end
    envpath = abspath(envpath)
    project_d = parsed_toml(envpath)

    # TODO: Perhaps verify that two packages with the same UUID do not have different names?
    names = Dict{UUID, String}()
    project_uuid_to_name = Dict{String, UUID}()

    project_deps = Dict{String, UUID}()
    project_weakdeps = Dict{String, UUID}()
    project_extras = Dict{String, UUID}()

    # Collect all direct dependencies of the project
    for key in ["deps", "weakdeps", "extras"]
        for (name, _uuid) in get(Dict{String, Any}, project_d, key)::Dict{String, Any}
            v = key == "deps" ? project_deps :
                key == "weakdeps" ? project_weakdeps :
                key == "extras" ? project_extras :
                error()
            uuid = UUID(_uuid::String)
            v[name] = uuid
            names[UUID(uuid)] = name
            project_uuid_to_name[name] = UUID(uuid)
        end
    end

    # A package in both deps and weakdeps is in fact only a weakdep
    for (name, _) in project_weakdeps
        delete!(project_deps, name)
    end

    # This project might be a package, in that case, that is also a "dependency"
    # of the project.
    proj_name = get(project_d, "name", nothing)::Union{String, Nothing}
    _proj_uuid = get(project_d, "uuid", nothing)::Union{String, Nothing}
    proj_uuid = _proj_uuid === nothing ? nothing : UUID(_proj_uuid)

    project_is_package = proj_name !== nothing && proj_uuid !== nothing
    if project_is_package
        # TODO: Error on missing uuid?
        project_deps[proj_name] = UUID(proj_uuid)
        names[UUID(proj_uuid)] = proj_name
    end

    project_extensions = Dict{String, Vector{UUID}}()
    # Collect all extensions of the project
    for (name, triggers) in get(Dict{String, Any}, project_d, "extensions")::Dict{String, Any}
        if triggers isa String
            triggers = [triggers]
        else
            triggers = triggers::Vector{String}
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
        for pkg_info in pkg_infos::Vector{Any}
            pkg_info = pkg_info::Dict{String, Any}
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
                    for (name_dep, _dep_uuid) in deps_pkg
                        dep_uuid = UUID(_dep_uuid::String)
                        push!(uuids, dep_uuid)
                        names[dep_uuid] = name_dep
                    end
                    d[m_uuid] = uuids
                end
            end

            # Extensions
            deps_pkg = get(Dict{String, Any}, pkg_info, "extensions")::Dict{String, Any}
            deps_pkg_concrete = Dict{String, Vector{String}}()
            for (ext, triggers) in deps_pkg
                if triggers isa String
                    triggers = [triggers]
                else
                    triggers = triggers::Vector{String}
                end
                deps_pkg_concrete[ext] = triggers
            end
            extensions[m_uuid] = deps_pkg_concrete

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

    if proj_name !== nothing && proj_uuid !== nothing
        deps_expanded[proj_uuid] = filter!(!=(proj_uuid), collect(values(project_deps)))
        extensions_expanded[proj_uuid] = project_extensions
        path = get(project_d, "path", nothing)::Union{String, Nothing}
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
    for (_, uuid) in project_deps
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

## PROGRESS BAR

# using Printf
Base.@kwdef mutable struct MiniProgressBar
    max::Int = 1.0
    header::String = ""
    color::Symbol = :nothing
    width::Int = 40
    current::Int = 0.0
    prev::Int = 0.0
    has_shown::Bool = false
    time_shown::Float64 = 0.0
    percentage::Bool = true
    always_reprint::Bool = false
    indent::Int = 4
end

const PROGRESS_BAR_TIME_GRANULARITY = Ref(1 / 30.0) # 30 fps
const PROGRESS_BAR_PERCENTAGE_GRANULARITY = Ref(0.1)

function start_progress(io::IO, _::MiniProgressBar)
    ansi_disablecursor = "\e[?25l"
    print(io, ansi_disablecursor)
end

function show_progress(io::IO, p::MiniProgressBar; termwidth=nothing, carriagereturn=true)
    if p.max == 0
        perc = 0.0
        prev_perc = 0.0
    else
        perc = p.current / p.max * 100
        prev_perc = p.prev / p.max * 100
    end
    # Bail early if we are not updating the progress bar,
    # Saves printing to the terminal
    if !p.always_reprint && p.has_shown && !((perc - prev_perc) > PROGRESS_BAR_PERCENTAGE_GRANULARITY[])
        return
    end
    t = time()
    if !p.always_reprint && p.has_shown && (t - p.time_shown) < PROGRESS_BAR_TIME_GRANULARITY[]
        return
    end
    p.time_shown = t
    p.prev = p.current
    p.has_shown = true

    progress_text = if false # p.percentage
        # @sprintf "%2.1f %%" perc
    else
        string(p.current, "/",  p.max)
    end
    termwidth = @something termwidth displaysize(io)[2]
    max_progress_width = max(0, min(termwidth - textwidth(p.header) - textwidth(progress_text) - 10 , p.width))
    n_filled = floor(Int, max_progress_width * perc / 100)
    partial_filled = (max_progress_width * perc / 100) - n_filled
    n_left = max_progress_width - n_filled
    headers = split(p.header, ' ')
    to_print = sprint(; context=io) do io
        print(io, " "^p.indent)
        printstyled(io, headers[1], " "; color=:green, bold=true)
        printstyled(io, join(headers[2:end], ' '))
        print(io, " ")
        printstyled(io, "━"^n_filled; color=p.color)
        if n_left > 0
            if partial_filled > 0.5
                printstyled(io, "╸"; color=p.color) # More filled, use ╸
            else
                printstyled(io, "╺"; color=:light_black) # Less filled, use ╺
            end
            printstyled(io, "━"^(n_left-1); color=:light_black)
        end
        printstyled(io, " "; color=:light_black)
        print(io, progress_text)
        carriagereturn && print(io, "\r")
    end
    # Print everything in one call
    print(io, to_print)
end

function end_progress(io, p::MiniProgressBar)
    ansi_enablecursor = "\e[?25h"
    ansi_clearline = "\e[2K"
    print(io, ansi_enablecursor * ansi_clearline)
end

function print_progress_bottom(io::IO)
    ansi_clearline = "\e[2K"
    ansi_movecol1 = "\e[1G"
    ansi_moveup(n::Int) = string("\e[", n, "A")
    print(io, "\e[S" * ansi_moveup(1) * ansi_clearline * ansi_movecol1)
end


############
struct PkgPrecompileError <: Exception
    msg::String
end
Base.showerror(io::IO, err::PkgPrecompileError) = print(io, err.msg)
Base.showerror(io::IO, err::PkgPrecompileError, bt; kw...) = Base.showerror(io, err) # hide stacktrace

# This needs a show method to make `julia> err` show nicely
Base.show(io::IO, err::PkgPrecompileError) = print(io, "PkgPrecompileError: ", err.msg)

import Base: StaleCacheKey

can_fancyprint(io::IO) = io isa Base.TTY && (get(ENV, "CI", nothing) != "true")

function printpkgstyle(io, header, msg; color=:green)
    printstyled(io, header; color, bold=true)
    println(io, " ", msg)
end

const Config = Pair{Cmd, Base.CacheFlags}
const PkgConfig = Tuple{PkgId,Config}

# name or parent → ext
function full_name(ext_to_parent::Dict{PkgId, PkgId}, pkg::PkgId)
    if haskey(ext_to_parent, pkg)
        return string(ext_to_parent[pkg].name, " → ", pkg.name)
    else
        return pkg.name
    end
end

function excluded_circular_deps_explanation(io::IOContext{IO}, ext_to_parent::Dict{PkgId, PkgId}, circular_deps, cycles)
    outer_deps = copy(circular_deps)
    cycles_names = ""
    for cycle in cycles
        filter!(!in(cycle), outer_deps)
        cycle_str = ""
        for (i, pkg) in enumerate(cycle)
            j = max(0, i - 1)
            if length(cycle) == 1
                line = " ─ "
            elseif i == 1
                line = " ┌ "
            elseif i < length(cycle)
                line = " │ " * " " ^j
            else
                line = " └" * "─" ^j * " "
            end
            hascolor = get(io, :color, false)::Bool
            line = _color_string(line, :light_black, hascolor) * full_name(ext_to_parent, pkg) * "\n"
            cycle_str *= line
        end
        cycles_names *= cycle_str
    end
    plural1 = length(cycles) > 1 ? "these cycles" : "this cycle"
    plural2 = length(cycles) > 1 ? "cycles" : "cycle"
    msg = """Circular dependency detected.
    Precompilation will be skipped for dependencies in $plural1:
    $cycles_names"""
    if !isempty(outer_deps)
        msg *= "Precompilation will also be skipped for the following, which depend on the above $plural2:\n"
        msg *= join(("  " * full_name(ext_to_parent, pkg) for pkg in outer_deps), "\n")
    end
    return msg
end

function precompilepkgs(pkgs::Vector{String}=String[];
                        internal_call::Bool=false,
                        strict::Bool = false,
                        warn_loaded::Bool = true,
                        timing::Bool = false,
                        _from_loading::Bool=false,
                        configs::Union{Config,Vector{Config}}=(``=>Base.CacheFlags()),
                        io::IO=stderr,
                        # asking for timing disables fancy mode, as timing is shown in non-fancy mode
                        fancyprint::Bool = can_fancyprint(io) && !timing,
                        manifest::Bool=false,
                        ignore_loaded::Bool=true)
    # monomorphize this to avoid latency problems
    _precompilepkgs(pkgs, internal_call, strict, warn_loaded, timing, _from_loading,
                   configs isa Vector{Config} ? configs : [configs],
                   IOContext{IO}(io), fancyprint, manifest, ignore_loaded)
end

function _precompilepkgs(pkgs::Vector{String},
                         internal_call::Bool,
                         strict::Bool,
                         warn_loaded::Bool,
                         timing::Bool,
                         _from_loading::Bool,
                         configs::Vector{Config},
                         io::IOContext{IO},
                         fancyprint::Bool,
                         manifest::Bool,
                         ignore_loaded::Bool)
    requested_pkgs = copy(pkgs) # for understanding user intent

    time_start = time_ns()

    env = ExplicitEnv()

    # Windows sometimes hits a ReadOnlyMemoryError, so we halve the default number of tasks. Issue #2323
    # TODO: Investigate why this happens in windows and restore the full task limit
    default_num_tasks = Sys.iswindows() ? div(Sys.CPU_THREADS::Int, 2) + 1 : Sys.CPU_THREADS::Int + 1
    default_num_tasks = min(default_num_tasks, 16) # limit for better stability on shared resource systems

    num_tasks = parse(Int, get(ENV, "JULIA_NUM_PRECOMPILE_TASKS", string(default_num_tasks)))
    parallel_limiter = Base.Semaphore(num_tasks)

    if _from_loading && !Sys.isinteractive() && Base.get_bool_env("JULIA_TESTS", false)
        # suppress passive loading printing in julia test suite. `JULIA_TESTS` is set in Base.runtests
        io = IOContext{IO}(devnull)
    end

    nconfigs = length(configs)
    hascolor = get(io, :color, false)::Bool
    color_string(cstr::String, col::Union{Int64, Symbol}) = _color_string(cstr, col, hascolor)

    stale_cache = Dict{StaleCacheKey, Bool}()
    cachepath_cache = Dict{PkgId, Vector{String}}()

    # a map from packages/extensions to their direct deps
    direct_deps = Dict{Base.PkgId, Vector{Base.PkgId}}()
    # a map from parent → extension, including all extensions that are loadable
    # in the current environment (i.e. their triggers are present)
    parent_to_exts = Dict{Base.PkgId, Vector{Base.PkgId}}()
    # inverse map of `parent_to_ext` above (ext → parent)
    ext_to_parent = Dict{Base.PkgId, Base.PkgId}()

    function describe_pkg(pkg::PkgId, is_project_dep::Bool, flags::Cmd, cacheflags::Base.CacheFlags)
        name = full_name(ext_to_parent, pkg)
        name = is_project_dep ? name : color_string(name, :light_black)
        if nconfigs > 1 && !isempty(flags)
            config_str = join(flags, " ")
            name *= color_string(" `$config_str`", :light_black)
        end
        if nconfigs > 1
            config_str = join(Base.translate_cache_flags(cacheflags, Base.DefaultCacheFlags), " ")
            name *= color_string(" $config_str", :light_black)
        end
        return name
    end

    triggers = Dict{Base.PkgId,Vector{Base.PkgId}}()
    for (dep, deps) in env.deps
        pkg = Base.PkgId(dep, env.names[dep])
        Base.in_sysimage(pkg) && continue
        deps = [Base.PkgId(x, env.names[x]) for x in deps]
        direct_deps[pkg] = filter!(!Base.in_sysimage, deps)
        for (ext_name, trigger_uuids) in env.extensions[dep]
            ext_uuid = Base.uuid5(pkg.uuid, ext_name)
            ext = Base.PkgId(ext_uuid, ext_name)
            triggers[ext] = Base.PkgId[pkg] # depends on parent package
            all_triggers_available = true
            for trigger_uuid in trigger_uuids
                trigger_name = env.names[trigger_uuid]
                if trigger_uuid in keys(env.deps)
                    push!(triggers[ext], Base.PkgId(trigger_uuid, trigger_name))
                else
                    all_triggers_available = false
                    break
                end
            end
            all_triggers_available || continue
            ext_to_parent[ext] = pkg
            direct_deps[ext] = filter(!Base.in_sysimage, triggers[ext])

            if !haskey(parent_to_exts, pkg)
                parent_to_exts[pkg] = Base.PkgId[ext]
            else
                push!(parent_to_exts[pkg], ext)
            end
        end
    end

    project_deps = [
        Base.PkgId(uuid, name)
        for (name, uuid) in env.project_deps if !Base.in_sysimage(Base.PkgId(uuid, name))
    ]

    # consider exts of project deps to be project deps so that errors are reported
    append!(project_deps, keys(filter(d->last(d).name in keys(env.project_deps), ext_to_parent)))

    @debug "precompile: deps collected"

    # An extension effectively depends on another extension if it has a strict superset of its triggers
    for ext_a in keys(ext_to_parent)
        for ext_b in keys(ext_to_parent)
            if triggers[ext_a] ⊋ triggers[ext_b]
                push!(direct_deps[ext_a], ext_b)
            end
        end
    end

    # A package depends on an extension if it (indirectly) depends on all extension triggers
    function expand_indirect_dependencies(direct_deps)
        function visit!(visited, node, all_deps)
            if node in visited
                return
            end
            push!(visited, node)
            for dep in get(Set{Base.PkgId}, direct_deps, node)
                if !(dep in all_deps)
                    push!(all_deps, dep)
                    visit!(visited, dep, all_deps)
                end
            end
        end

        indirect_deps = Dict{Base.PkgId, Set{Base.PkgId}}()
        for package in keys(direct_deps)
            # Initialize a set to keep track of all dependencies for 'package'
            all_deps = Set{Base.PkgId}()
            visited = Set{Base.PkgId}()
            visit!(visited, package, all_deps)
            # Update direct_deps with the complete set of dependencies for 'package'
            indirect_deps[package] = all_deps
        end
        return indirect_deps
    end

    # this loop must be run after the full direct_deps map has been populated
    indirect_deps = expand_indirect_dependencies(direct_deps)
    for ext in keys(ext_to_parent)
        ext_loadable_in_pkg = Dict{Base.PkgId,Bool}()
        for pkg in keys(direct_deps)
            is_trigger = in(pkg, direct_deps[ext])
            is_extension = in(pkg, keys(ext_to_parent))
            has_triggers = issubset(direct_deps[ext], indirect_deps[pkg])
            ext_loadable_in_pkg[pkg] = !is_extension && has_triggers && !is_trigger
        end
        for (pkg, ext_loadable) in ext_loadable_in_pkg
            if ext_loadable && !any((dep)->ext_loadable_in_pkg[dep], direct_deps[pkg])
                # add an edge if the extension is loadable by pkg, and was not loadable in any
                # of the pkg's dependencies
                push!(direct_deps[pkg], ext)
            end
        end
    end
    @debug "precompile: extensions collected"

    # return early if no deps
    if isempty(direct_deps)
        if isempty(pkgs)
            return
        elseif _from_loading
            # if called from loading precompilation it may be a package from another environment stack so
            # don't error and allow serial precompilation to try
            # TODO: actually handle packages from other envs in the stack
            return
        else
            error("No direct dependencies outside of the sysimage found matching $(pkgs)")
        end
    end

    # initialize signalling
    started = Dict{PkgConfig,Bool}()
    was_processed = Dict{PkgConfig,Base.Event}()
    was_recompiled = Dict{PkgConfig,Bool}()
    for config in configs
        for pkgid in keys(direct_deps)
            pkg_config = (pkgid, config)
            started[pkg_config] = false
            was_processed[pkg_config] = Base.Event()
            was_recompiled[pkg_config] = false
        end
    end
    @debug "precompile: signalling initialized"

    # find and guard against circular deps
    cycles = Vector{Base.PkgId}[]
    # For every scanned package, true if pkg found to be in a cycle
    # or depends on packages in a cycle and false otherwise.
    could_be_cycle = Dict{Base.PkgId, Bool}()
    # temporary stack for the SCC-like algorithm below
    stack = Base.PkgId[]
    function scan_pkg!(pkg, dmap)
        if haskey(could_be_cycle, pkg)
            return could_be_cycle[pkg]
        else
            return scan_deps!(pkg, dmap)
        end
    end
    function scan_deps!(pkg, dmap)
        push!(stack, pkg)
        cycle = nothing
        for dep in dmap[pkg]
            if dep in stack
                # Created fresh cycle
                cycle′ = stack[findlast(==(dep), stack):end]
                if cycle === nothing || length(cycle′) < length(cycle)
                    cycle = cycle′ # try to report smallest cycle possible
                end
            elseif scan_pkg!(dep, dmap)
                # Reaches an existing cycle
                could_be_cycle[pkg] = true
                pop!(stack)
                return true
            end
        end
        pop!(stack)
        if cycle !== nothing
            push!(cycles, cycle)
            could_be_cycle[pkg] = true
            return true
        end
        could_be_cycle[pkg] = false
        return false
    end
    # set of packages that depend on a cycle (either because they are
    # a part of a cycle themselves or because they transitively depend
    # on a package in some cycle)
    circular_deps = Base.PkgId[]
    for pkg in keys(direct_deps)
        @assert isempty(stack)
        if scan_pkg!(pkg, direct_deps)
            push!(circular_deps, pkg)
            for pkg_config in keys(was_processed)
                # notify all to allow skipping
                pkg_config[1] == pkg && notify(was_processed[pkg_config])
            end
        end
    end
    if !isempty(circular_deps)
        @warn excluded_circular_deps_explanation(io, ext_to_parent, circular_deps, cycles)
    end
    @debug "precompile: circular dep check done"

    if !manifest
        if isempty(pkgs)
            pkgs = [pkg.name for pkg in project_deps]
        end
        # restrict to dependencies of given packages
        function collect_all_deps(direct_deps, dep, alldeps=Set{Base.PkgId}())
            for _dep in direct_deps[dep]
                if !(_dep in alldeps)
                    push!(alldeps, _dep)
                    collect_all_deps(direct_deps, _dep, alldeps)
                end
            end
            return alldeps
        end
        keep = Set{Base.PkgId}()
        for dep in direct_deps
            dep_pkgid = first(dep)
            if dep_pkgid.name in pkgs
                push!(keep, dep_pkgid)
                collect_all_deps(direct_deps, dep_pkgid, keep)
            end
        end
        for ext in keys(ext_to_parent)
            if issubset(collect_all_deps(direct_deps, ext), keep) # if all extension deps are kept
                push!(keep, ext)
            end
        end
        filter!(d->in(first(d), keep), direct_deps)
        if isempty(direct_deps)
            if _from_loading
                # if called from loading precompilation it may be a package from another environment stack so
                # don't error and allow serial precompilation to try
                # TODO: actually handle packages from other envs in the stack
                return
            else
                return
            end
        end
    end

    target = nothing
    if nconfigs == 1
        if !isempty(only(configs)[1])
            target = "for configuration $(join(only(configs)[1], " "))"
        end
    else
        target = "for $nconfigs compilation configurations..."
    end
    @debug "precompile: packages filtered"

    pkg_queue = PkgConfig[]
    failed_deps = Dict{PkgConfig, String}()
    precomperr_deps = PkgConfig[] # packages that may succeed after a restart (i.e. loaded packages with no cache file)

    print_lock = io.io isa Base.LibuvStream ? io.io.lock::ReentrantLock : ReentrantLock()
    first_started = Base.Event()
    printloop_should_exit::Bool = !fancyprint # exit print loop immediately if not fancy printing
    interrupted_or_done = Base.Event()

    ansi_moveup(n::Int) = string("\e[", n, "A")
    ansi_movecol1 = "\e[1G"
    ansi_cleartoend = "\e[0J"
    ansi_cleartoendofline = "\e[0K"
    ansi_enablecursor = "\e[?25h"
    ansi_disablecursor = "\e[?25l"
    n_done::Int = 0
    n_already_precomp::Int = 0
    n_loaded::Int = 0
    interrupted = false

    function handle_interrupt(err, in_printloop = false)
        notify(interrupted_or_done)
        in_printloop || wait(t_print) # wait to let the print loop cease first
        if err isa InterruptException
            lock(print_lock) do
                println(io, " Interrupted: Exiting precompilation...", ansi_cleartoendofline)
            end
            interrupted = true
            return true
        else
            return false
        end
    end
    std_outputs = Dict{PkgConfig,IOBuffer}()
    taskwaiting = Set{PkgConfig}()
    pkgspidlocked = Dict{PkgConfig,String}()
    pkg_liveprinted = nothing

    function monitor_std(pkg_config, pipe; single_requested_pkg=false)
        pkg, config = pkg_config
        try
            liveprinting = false
            while !eof(pipe)
                str = readline(pipe, keep=true)
                if single_requested_pkg && (liveprinting || !isempty(str))
                    lock(print_lock) do
                        if !liveprinting
                            printpkgstyle(io, :Info, "Given $(pkg.name) was explicitly requested, output will be shown live $ansi_cleartoendofline",
                                color = Base.info_color())
                            liveprinting = true
                            pkg_liveprinted = pkg
                        end
                        print(io, ansi_cleartoendofline, str)
                    end
                end
                write(get!(IOBuffer, std_outputs, pkg_config), str)
                if !in(pkg_config, taskwaiting) && occursin("waiting for IO to finish", str)
                    !fancyprint && lock(print_lock) do
                        println(io, pkg.name, color_string(" Waiting for background task / IO / timer.", Base.warn_color()))
                    end
                    push!(taskwaiting, pkg_config)
                end
                if !fancyprint && in(pkg_config, taskwaiting)
                    lock(print_lock) do
                        print(io, str)
                    end
                end
            end
        catch err
            err isa InterruptException || rethrow()
        end
    end

    ## fancy print loop
    t_print = @async begin
        try
            wait(first_started)
            (isempty(pkg_queue) || interrupted_or_done.set) && return
            lock(print_lock) do
                if target !== nothing
                    printpkgstyle(io, :Precompiling, target)
                end
                if fancyprint
                    print(io, ansi_disablecursor)
                end
            end
            t = Timer(0; interval=1/10)
            anim_chars = ["◐","◓","◑","◒"]
            i = 1
            last_length = 0
            bar = MiniProgressBar(; indent=0, header = "Precompiling packages ", color = :green, percentage=false, always_reprint=true)
            n_total = length(direct_deps) * length(configs)
            bar.max = n_total - n_already_precomp
            final_loop = false
            n_print_rows = 0
            while !printloop_should_exit
                lock(print_lock) do
                    term_size = displaysize(io)
                    num_deps_show = max(term_size[1] - 3, 2) # show at least 2 deps
                    pkg_queue_show = if !interrupted_or_done.set && length(pkg_queue) > num_deps_show
                        last(pkg_queue, num_deps_show)
                    else
                        pkg_queue
                    end
                    str_ = sprint() do iostr
                        if i > 1
                            print(iostr, ansi_cleartoend)
                        end
                        bar.current = n_done - n_already_precomp
                        bar.max = n_total - n_already_precomp
                        # when sizing to the terminal width subtract a little to give some tolerance to resizing the
                        # window between print cycles
                        termwidth = displaysize(io)[2] - 4
                        if !final_loop
                            str = sprint(io -> show_progress(io, bar; termwidth, carriagereturn=false); context=io)
                            print(iostr, Base._truncate_at_width_or_chars(true, str, termwidth), "\n")
                        end
                        for pkg_config in pkg_queue_show
                            dep, config = pkg_config
                            loaded = warn_loaded && haskey(Base.loaded_modules, dep)
                            flags, cacheflags = config
                            name = describe_pkg(dep, dep in project_deps, flags, cacheflags)
                            line = if pkg_config in precomperr_deps
                                string(color_string("  ? ", Base.warn_color()), name)
                            elseif haskey(failed_deps, pkg_config)
                                string(color_string("  ✗ ", Base.error_color()), name)
                            elseif was_recompiled[pkg_config]
                                !loaded && interrupted_or_done.set && continue
                                loaded || @async begin # keep successful deps visible for short period
                                    sleep(1);
                                    filter!(!isequal(pkg_config), pkg_queue)
                                end
                                string(color_string("  ✓ ", loaded ? Base.warn_color() : :green), name)
                            elseif started[pkg_config]
                                # Offset each spinner animation using the first character in the package name as the seed.
                                # If not offset, on larger terminal fonts it looks odd that they all sync-up
                                anim_char = anim_chars[(i + Int(dep.name[1])) % length(anim_chars) + 1]
                                anim_char_colored = dep in project_deps ? anim_char : color_string(anim_char, :light_black)
                                waiting = if haskey(pkgspidlocked, pkg_config)
                                    who_has_lock = pkgspidlocked[pkg_config]
                                    color_string(" Being precompiled by $(who_has_lock)", Base.info_color())
                                elseif pkg_config in taskwaiting
                                    color_string(" Waiting for background task / IO / timer. Interrupt to inspect", Base.warn_color())
                                else
                                    ""
                                end
                                string("  ", anim_char_colored, " ", name, waiting)
                            else
                                string("    ", name)
                            end
                            println(iostr, Base._truncate_at_width_or_chars(true, line, termwidth))
                        end
                    end
                    last_length = length(pkg_queue_show)
                    n_print_rows = count("\n", str_)
                    print(io, str_)
                    printloop_should_exit = interrupted_or_done.set && final_loop
                    final_loop = interrupted_or_done.set # ensures one more loop to tidy last task after finish
                    i += 1
                    printloop_should_exit || print(io, ansi_moveup(n_print_rows), ansi_movecol1)
                end
                wait(t)
            end
        catch err
            handle_interrupt(err, true) || rethrow()
        finally
            fancyprint && print(io, ansi_enablecursor)
        end
    end
    tasks = Task[]
    if !_from_loading
        Base.LOADING_CACHE[] = Base.LoadingCache()
    end
    @debug "precompile: starting precompilation loop" direct_deps project_deps
    ## precompilation loop

    for (pkg, deps) in direct_deps
        cachepaths = get!(() -> Base.find_all_in_cache_path(pkg), cachepath_cache, pkg)
        sourcepath = Base.locate_package(pkg)
        single_requested_pkg = length(requested_pkgs) == 1 && only(requested_pkgs) == pkg.name
        for config in configs
            pkg_config = (pkg, config)
            if sourcepath === nothing
                failed_deps[pkg_config] = "Error: Missing source file for $(pkg)"
                notify(was_processed[pkg_config])
                continue
            end
            # Heuristic for when precompilation is disabled
            if occursin(r"\b__precompile__\(\s*false\s*\)", read(sourcepath, String))
                notify(was_processed[pkg_config])
                continue
            end
            flags, cacheflags = config
            task = @async begin
                try
                    loaded = haskey(Base.loaded_modules, pkg)
                    for dep in deps # wait for deps to finish
                        wait(was_processed[(dep,config)])
                    end
                    circular = pkg in circular_deps
                    is_stale = !Base.isprecompiled(pkg; ignore_loaded, stale_cache, cachepath_cache, cachepaths, sourcepath, flags=cacheflags)
                    if !circular && is_stale
                        Base.acquire(parallel_limiter)
                        is_project_dep = pkg in project_deps

                        # std monitoring
                        std_pipe = Base.link_pipe!(Pipe(); reader_supports_async=true, writer_supports_async=true)
                        t_monitor = @async monitor_std(pkg_config, std_pipe; single_requested_pkg)

                        name = describe_pkg(pkg, is_project_dep, flags, cacheflags)
                        lock(print_lock) do
                            if !fancyprint && isempty(pkg_queue)
                                printpkgstyle(io, :Precompiling, something(target, "packages..."))
                            end
                        end
                        push!(pkg_queue, pkg_config)
                        started[pkg_config] = true
                        fancyprint && notify(first_started)
                        if interrupted_or_done.set
                            notify(was_processed[pkg_config])
                            Base.release(parallel_limiter)
                            return
                        end
                        try
                            # allows processes to wait if another process is precompiling a given package to
                            # a functionally identical package cache (except for preferences, which may differ)
                            t = @elapsed ret = precompile_pkgs_maybe_cachefile_lock(io, print_lock, fancyprint, pkg_config, pkgspidlocked, hascolor, parallel_limiter, ignore_loaded) do
                                Base.with_logger(Base.NullLogger()) do
                                    # whether to respect already loaded dependency versions
                                    keep_loaded_modules = !ignore_loaded
                                    # for extensions, any extension in our direct dependencies is one we have a right to load
                                    # for packages, we may load any extension (all possible triggers are accounted for above)
                                    loadable_exts = haskey(ext_to_parent, pkg) ? filter((dep)->haskey(ext_to_parent, dep), direct_deps[pkg]) : nothing
                                    Base.compilecache(pkg, sourcepath, std_pipe, std_pipe, keep_loaded_modules;
                                                      flags, cacheflags, loadable_exts)
                                end
                            end
                            if ret isa Base.PrecompilableError
                                push!(precomperr_deps, pkg_config)
                                !fancyprint && lock(print_lock) do
                                    println(io, _timing_string(t), color_string("  ? ", Base.warn_color()), name)
                                end
                            else
                                !fancyprint && lock(print_lock) do
                                    println(io, _timing_string(t), color_string("  ✓ ", loaded ? Base.warn_color() : :green), name)
                                end
                                was_recompiled[pkg_config] = true
                            end
                            loaded && (n_loaded += 1)
                        catch err
                            # @show err
                            close(std_pipe.in) # close pipe to end the std output monitor
                            wait(t_monitor)
                            if err isa ErrorException || (err isa ArgumentError && startswith(err.msg, "Invalid header in cache file"))
                                errmsg = String(take!(get(IOBuffer, std_outputs, pkg_config)))
                                delete!(std_outputs, pkg_config) # so it's not shown as warnings, given error report
                                failed_deps[pkg_config] = (strict || is_project_dep) ? string(sprint(showerror, err), "\n", strip(errmsg)) : ""
                                !fancyprint && lock(print_lock) do
                                    println(io, " "^9, color_string("  ✗ ", Base.error_color()), name)
                                end
                            else
                                rethrow()
                            end
                        finally
                            isopen(std_pipe.in) && close(std_pipe.in) # close pipe to end the std output monitor
                            wait(t_monitor)
                            Base.release(parallel_limiter)
                        end
                    else
                        is_stale || (n_already_precomp += 1)
                    end
                    n_done += 1
                    notify(was_processed[pkg_config])
                catch err_outer
                    # For debugging:
                    # println("Task failed $err_outer")
                    # Base.display_error(ErrorException(""), Base.catch_backtrace())# logging doesn't show here
                    handle_interrupt(err_outer) || rethrow()
                    notify(was_processed[pkg_config])
                finally
                    filter!(!istaskdone, tasks)
                    length(tasks) == 1 && notify(interrupted_or_done)
                end
            end
            Base.errormonitor(task) # interrupts are handled separately so ok to watch for other errors like this
            push!(tasks, task)
        end
    end
    isempty(tasks) && notify(interrupted_or_done)
    try
        wait(interrupted_or_done)
    catch err
        handle_interrupt(err) || rethrow()
    finally
        Base.LOADING_CACHE[] = nothing
    end
    notify(first_started) # in cases of no-op or !fancyprint
    fancyprint && wait(t_print)
    quick_exit = !all(istaskdone, tasks) || interrupted # if some not finished internal error is likely
    seconds_elapsed = round(Int, (time_ns() - time_start) / 1e9)
    ndeps = count(values(was_recompiled))
    if ndeps > 0 || !isempty(failed_deps) || (quick_exit && !isempty(std_outputs))
        str = sprint(context=io) do iostr
            if !quick_exit
                if fancyprint # replace the progress bar
                    what = isempty(requested_pkgs) ? "packages finished." : "$(join(requested_pkgs, ", ", " and ")) finished."
                    printpkgstyle(iostr, :Precompiling, what)
                end
                plural = length(configs) > 1 ? "dependency configurations" : ndeps == 1 ? "dependency" : "dependencies"
                print(iostr, "  $(ndeps) $(plural) successfully precompiled in $(seconds_elapsed) seconds")
                if n_already_precomp > 0 || !isempty(circular_deps)
                    n_already_precomp > 0 && (print(iostr, ". $n_already_precomp already precompiled"))
                    !isempty(circular_deps) && (print(iostr, ". $(length(circular_deps)) skipped due to circular dependency"))
                    print(iostr, ".")
                end
                if n_loaded > 0
                    plural1 = length(configs) > 1 ? "dependency configurations" : n_loaded == 1 ? "dependency" : "dependencies"
                    plural2 = n_loaded == 1 ? "a different version is" : "different versions are"
                    plural3 = n_loaded == 1 ? "" : "s"
                    plural4 = n_loaded == 1 ? "this package" : "these packages"
                    print(iostr, "\n  ",
                        color_string(string(n_loaded), Base.warn_color()),
                        " $(plural1) precompiled but ",
                        color_string("$(plural2) currently loaded", Base.warn_color()),
                        ". Restart julia to access the new version$(plural3). \
                        Otherwise, loading dependents of $(plural4) may trigger further precompilation to work with the unexpected version$(plural3)."
                    )
                end
                if !isempty(precomperr_deps)
                    pluralpc = length(configs) > 1 ? "dependency configurations" : precomperr_deps == 1 ? "dependency" : "dependencies"
                    print(iostr, "\n  ",
                        color_string(string(length(precomperr_deps)), Base.warn_color()),
                        " $(pluralpc) failed but may be precompilable after restarting julia"
                    )
                end
            end
            # show any stderr output, even if Pkg.precompile has been interrupted (quick_exit=true), given user may be
            # interrupting a hanging precompile job with stderr output. julia#48371
            let std_outputs = Tuple{PkgConfig,SubString{String}}[(pkg_config, strip(String(take!(io)))) for (pkg_config,io) in std_outputs]
                filter!(kv -> !isempty(last(kv)), std_outputs)
                if !isempty(std_outputs)
                    plural1 = length(std_outputs) == 1 ? "y" : "ies"
                    plural2 = length(std_outputs) == 1 ? "" : "s"
                    print(iostr, "\n  ", color_string("$(length(std_outputs))", Base.warn_color()), " dependenc$(plural1) had output during precompilation:")
                    for (pkg_config, err) in std_outputs
                        pkg, config = pkg_config
                        err = if pkg == pkg_liveprinted
                            "[Output was shown above]"
                        else
                            join(split(err, "\n"), color_string("\n│  ", Base.warn_color()))
                        end
                        name = full_name(ext_to_parent, pkg)
                        print(iostr, color_string("\n┌ ", Base.warn_color()), name, color_string("\n│  ", Base.warn_color()), err, color_string("\n└  ", Base.warn_color()))
                    end
                end
            end
        end
        let str=str
            lock(print_lock) do
                println(io, str)
            end
        end
        quick_exit && return
        err_str = IOBuffer()
        n_direct_errs = 0
        for (pkg_config, err) in failed_deps
            dep, config = pkg_config
            if strict || (dep in project_deps)
                print(err_str, "\n", dep.name, " ")
                for cfg in config[1]
                    print(err_str, cfg, " ")
                end
                print(err_str, "\n\n", err)
                n_direct_errs > 0 && write(err_str, "\n")
                n_direct_errs += 1
            end
        end
        if position(err_str) > 0
            skip(err_str, -1)
            truncate(err_str, position(err_str))
            pluralde = n_direct_errs == 1 ? "y" : "ies"
            direct = strict ? "" : "direct "
            err_msg = "The following $n_direct_errs $(direct)dependenc$(pluralde) failed to precompile:\n$(String(take!(err_str)))"
            if internal_call # aka. auto-precompilation
                if isinteractive()
                    plural1 = length(failed_deps) == 1 ? "y" : "ies"
                    println(io, "  ", color_string("$(length(failed_deps))", Base.error_color()), " dependenc$(plural1) errored.")
                    println(io, "  For a report of the errors see `julia> err`. To retry use `pkg> precompile`")
                    setglobal!(Base.MainInclude, :err, PkgPrecompileError(err_msg))
                else
                    # auto-precompilation shouldn't throw but if the user can't easily access the
                    # error messages, just show them
                    print(io, "\n", err_msg)
                end
            else
                println(io)
                throw(PkgPrecompileError(err_msg))
            end
        end
    end
    nothing
end

_timing_string(t) = string(lpad(round(t * 1e3, digits = 1), 9), " ms")

function _color_string(cstr::String, col::Union{Int64, Symbol}, hascolor)
    if hascolor
        enable_ansi  = get(Base.text_colors, col, Base.text_colors[:default])
        disable_ansi = get(Base.disable_text_style, col, Base.text_colors[:default])
        return string(enable_ansi, cstr, disable_ansi)
    else
        return cstr
    end
end

# Can be merged with `maybe_cachefile_lock` in loading?
function precompile_pkgs_maybe_cachefile_lock(f, io::IO, print_lock::ReentrantLock, fancyprint::Bool, pkg_config, pkgspidlocked, hascolor, parallel_limiter::Base.Semaphore, ignore_loaded::Bool)
    if !(isdefined(Base, :mkpidlock_hook) && isdefined(Base, :trymkpidlock_hook) && Base.isdefined(Base, :parse_pidfile_hook))
        return f()
    end
    pkg, config = pkg_config
    flags, cacheflags = config
    stale_age = Base.compilecache_pidlock_stale_age
    pidfile = Base.compilecache_pidfile_path(pkg, flags=cacheflags)
    cachefile = @invokelatest Base.trymkpidlock_hook(f, pidfile; stale_age)
    if cachefile === false
        pid, hostname, age = @invokelatest Base.parse_pidfile_hook(pidfile)
        pkgspidlocked[pkg_config] = if isempty(hostname) || hostname == gethostname()
            if pid == getpid()
                "an async task in this process (pidfile: $pidfile)"
            else
                "another process (pid: $pid, pidfile: $pidfile)"
            end
        else
            "another machine (hostname: $hostname, pid: $pid, pidfile: $pidfile)"
        end
        !fancyprint && lock(print_lock) do
            println(io, "    ", pkg.name, _color_string(" Being precompiled by $(pkgspidlocked[pkg_config])", Base.info_color(), hascolor))
        end
        Base.release(parallel_limiter) # release so other work can be done while waiting
        try
            # wait until the lock is available
            @invokelatest Base.mkpidlock_hook(() -> begin
                    # double-check in case the other process crashed or the lock expired
                    if Base.isprecompiled(pkg; ignore_loaded, flags=cacheflags) # don't use caches for this as the env state will have changed
                        return nothing # returning nothing indicates a process waited for another
                    else
                        delete!(pkgspidlocked, pkg_config)
                        Base.acquire(f, parallel_limiter) # precompile
                    end
                end,
                pidfile; stale_age)
        finally
            Base.acquire(parallel_limiter) # re-acquire so the outer release is balanced
        end
    end
    return cachefile
end

end
