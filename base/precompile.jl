#using Base: PkgId, UUID, SHA1, parsed_toml, project_file_name_uuid, project_names,
#            project_file_manifest_path, get_deps, preferences_names, isaccessibledir, isfile_casesensitive,

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
            uuid = UUID(_uuid)
            v[name] = uuid
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

    if proj_name !== nothing && proj_uuid !== nothing
        # TODO: Error on missing uuid?
        project_deps[proj_name] = UUID(proj_uuid)
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
        deps_expanded[proj_uuid] = filter!(!=(proj_uuid), collect(values(project_deps)))
        extensions_expanded[proj_uuid] = project_extensions
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


#import Base: StaleCacheKey

function precompile(ctx::Context, pkgs::Vector{PackageSpec}; internal_call::Bool=false,
                    strict::Bool=false, warn_loaded = true, already_instantiated = false, timing::Bool = false,
                    _from_loading::Bool=false, kwargs...)
    Context!(ctx; kwargs...)
    if !already_instantiated
        instantiate(ctx; allow_autoprecomp=false, kwargs...)
        @debug "precompile: instantiated"
    end

    time_start = time_ns()

    # Windows sometimes hits a ReadOnlyMemoryError, so we halve the default number of tasks. Issue #2323
    # TODO: Investigate why this happens in windows and restore the full task limit
    default_num_tasks = Sys.iswindows() ? div(Sys.CPU_THREADS::Int, 2) + 1 : Sys.CPU_THREADS::Int + 1
    default_num_tasks = min(default_num_tasks, 16) # limit for better stability on shared resource systems

    num_tasks = parse(Int, get(ENV, "JULIA_NUM_PRECOMPILE_TASKS", string(default_num_tasks)))
    parallel_limiter = Base.Semaphore(num_tasks)
    io = ctx.io
    if io isa UnstableIO
        # precompile does quite a bit of output and using the UnstableIO can cause
        # some slowdowns, the important part here is to not specialize the whole
        # precompile function on the io
        io = io.io
    end
    fancyprint = can_fancyprint(io) && !timing

    hascolor = get(io, :color, false)::Bool
    color_string(cstr::String, col::Union{Int64, Symbol}) = _color_string(cstr, col, hascolor)

    recall_precompile_state() # recall suspended and force-queued packages
    !internal_call && precomp_unsuspend!() # when manually called, unsuspend all packages that were suspended due to precomp errors

    direct_deps = [
        Base.PkgId(uuid, name)
        for (name, uuid) in ctx.env.project.deps if !Base.in_sysimage(Base.PkgId(uuid, name))
    ]
    stale_cache = Dict{StaleCacheKey, Bool}()
    exts = Dict{Base.PkgId, String}() # ext -> parent
    # make a flat map of each dep and its direct deps
    depsmap = Dict{Base.PkgId, Vector{Base.PkgId}}()
    pkg_specs = PackageSpec[]
    pkg_exts_map = Dict{Base.PkgId, Vector{Base.PkgId}}()
    for dep in ctx.env.manifest
        pkg = Base.PkgId(first(dep), last(dep).name)
        Base.in_sysimage(pkg) && continue
        deps = [Base.PkgId(last(x), first(x)) for x in last(dep).deps]
        depsmap[pkg] = filter!(!Base.in_sysimage, deps)
        # add any extensions
        weakdeps = last(dep).weakdeps
        pkg_exts = Dict{Base.PkgId, Vector{Base.PkgId}}()
        prev_ext = nothing
        for (ext_name, extdep_names) in last(dep).exts
            ext_deps = Base.PkgId[]
            push!(ext_deps, pkg) # depends on parent package
            all_extdeps_available = true
            extdep_names = extdep_names isa String ? String[extdep_names] : extdep_names
            for extdep_name in extdep_names
                extdep_uuid = weakdeps[extdep_name]
                if extdep_uuid in keys(ctx.env.manifest.deps) || Base.in_sysimage(Base.PkgId(extdep_uuid, extdep_name))
                    push!(ext_deps, Base.PkgId(extdep_uuid, extdep_name))
                else
                    all_extdeps_available = false
                    break
                end
            end
            all_extdeps_available || continue
            if prev_ext isa Base.PkgId
                # also make the exts depend on eachother sequentially to avoid race
                push!(ext_deps, prev_ext)
            end
            ext_uuid = Base.uuid5(pkg.uuid, ext_name)
            ext = Base.PkgId(ext_uuid, ext_name)
            prev_ext = ext
            push!(pkg_specs, PackageSpec(uuid = ext_uuid, name = ext_name)) # create this here as the name cannot be looked up easily later via the uuid
            filter!(!Base.in_sysimage, ext_deps)
            depsmap[ext] = ext_deps
            exts[ext] = pkg.name
            pkg_exts[ext] = ext_deps
        end
        if !isempty(pkg_exts)
            pkg_exts_map[pkg] = collect(keys(pkg_exts))
        end
    end
    @debug "precompile: deps collected"
    # this loop must be run after the full depsmap has been populated
    for (pkg, pkg_exts) in pkg_exts_map
        # find any packages that depend on the extension(s)'s deps and replace those deps in their deps list with the extension(s),
        # basically injecting the extension into the precompile order in the graph, to avoid race to precompile extensions
        for (_pkg, deps) in depsmap # for each manifest dep
            if !in(_pkg, keys(exts)) && pkg in deps # if not an extension and depends on pkg
                append!(deps, pkg_exts) # add the package extensions to deps
                filter!(!isequal(pkg), deps) # remove the pkg from deps
            end
        end
    end
    @debug "precompile: extensions collected"

    # if the active environment is a package, add that
    ctx_env_pkg = ctx.env.pkg
    if ctx_env_pkg !== nothing && isfile(joinpath(dirname(ctx.env.project_file), "src", "$(ctx_env_pkg.name).jl"))
        project_pkgid = Base.PkgId(ctx_env_pkg.uuid, ctx_env_pkg.name)
        depsmap[project_pkgid] = [
            Base.PkgId(last(x), first(x))
            for x in ctx.env.project.deps if !Base.in_sysimage(Base.PkgId(last(x), first(x)))
        ]
        push!(direct_deps, Base.PkgId(ctx_env_pkg.uuid, ctx_env_pkg.name))
    else
        project_pkgid = nothing
    end

    # return early if no deps
    if isempty(depsmap)
        if isempty(pkgs)
            return
        elseif _from_loading
            # if called from loading precompilation it may be a package from another environment stack so
            # don't error and allow serial precompilation to try
            # TODO: actually handle packages from other envs in the stack
            return
        else
            pkgerror("No direct dependencies outside of the sysimage found matching $(repr([p.name for p in pkgs]))")
        end
    end

    # initialize signalling
    started = Dict{Base.PkgId,Bool}()
    was_processed = Dict{Base.PkgId,Base.Event}()
    was_recompiled = Dict{Base.PkgId,Bool}()
    for pkgid in keys(depsmap)
        started[pkgid] = false
        was_processed[pkgid] = Base.Event()
        was_recompiled[pkgid] = false
        push!(pkg_specs, get_or_make_pkgspec(pkg_specs, ctx, pkgid.uuid))
    end
    @debug "precompile: signalling initialized"

    # remove packages that are suspended because they errored before
    # note that when `Pkg.precompile` is manually called, all suspended packages are unsuspended
    precomp_prune_suspended!(pkg_specs)

    # find and guard against circular deps
    circular_deps = Base.PkgId[]
    # Three states
    # !haskey -> never visited
    # true -> cannot be compiled due to a cycle (or not yet determined)
    # false -> not depending on a cycle
    could_be_cycle = Dict{Base.PkgId, Bool}()
    function scan_pkg!(pkg, dmap)
        did_visit_dep = true
        inpath = get!(could_be_cycle, pkg) do
            did_visit_dep = false
            return true
        end
        if did_visit_dep ? inpath : scan_deps!(pkg, dmap)
            # Found a cycle. Delete this and all parents
            return true
        end
        return false
    end
    function scan_deps!(pkg, dmap)
        for dep in dmap[pkg]
            scan_pkg!(dep, dmap) && return true
        end
        could_be_cycle[pkg] = false
        return false
    end
    for pkg in keys(depsmap)
        if scan_pkg!(pkg, depsmap)
            push!(circular_deps, pkg)
            notify(was_processed[pkg])
        end
    end
    if !isempty(circular_deps)
        @warn """Circular dependency detected. Precompilation will be skipped for:\n  $(join(string.(circular_deps), "\n  "))"""
    end
    @debug "precompile: circular dep check done"

    # if a list of packages is given, restrict to dependencies of given packages
    if !isempty(pkgs)
        pkgs_names = [p.name for p in pkgs]
        function collect_all_deps(depsmap, dep, alldeps=Set{Base.PkgId}())
            for _dep in depsmap[dep]
                if !(_dep in alldeps)
                    push!(alldeps, _dep)
                    collect_all_deps(depsmap, _dep, alldeps)
                end
            end
            return alldeps
        end
        keep = Set{Base.PkgId}()
        for dep in depsmap
            dep_pkgid = first(dep)
            if dep_pkgid.name in pkgs_names
                push!(keep, dep_pkgid)
                collect_all_deps(depsmap, dep_pkgid, keep)
            end
        end
        for ext in keys(exts)
            if issubset(collect_all_deps(depsmap, ext), keep) # if all extension deps are kept
                push!(keep, ext)
            end
        end
        filter!(d->in(first(d), keep), depsmap)
        if isempty(depsmap)
            if _from_loading
                # if called from loading precompilation it may be a package from another environment stack so
                # don't error and allow serial precompilation to try
                # TODO: actually handle packages from other envs in the stack
                return
            else
                pkgerror("No direct dependencies outside of the sysimage found matching $(repr(pkgs_names))")
            end
        end
        target = join(pkgs_names, ", ")
    else
        target = "project..."
    end
    @debug "precompile: packages filtered"

    pkg_queue = Base.PkgId[]
    failed_deps = Dict{Base.PkgId, String}()
    skipped_deps = Base.PkgId[]
    precomperr_deps = Base.PkgId[] # packages that may succeed after a restart (i.e. loaded packages with no cache file)

    print_lock = ctx.io isa Base.LibuvStream ? ctx.io.lock::ReentrantLock : ReentrantLock()
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
                println(io, " Interrupted: Exiting precompilation...")
            end
            interrupted = true
            return true
        else
            return false
        end
    end

    std_outputs = Dict{Base.PkgId,String}()
    taskwaiting = Set{Base.PkgId}()
    pkgspidlocked = Dict{Base.PkgId,String}()
    pkg_liveprinted = nothing

    function monitor_std(pkg, pipe; single_requested_pkg=false)
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
                std_outputs[pkg] = string(get(std_outputs, pkg, ""), str)
                if !in(pkg, taskwaiting) && occursin("waiting for IO to finish", str)
                    !fancyprint && lock(print_lock) do
                        println(io, pkg.name, color_string(" Waiting for background task / IO / timer.", Base.warn_color()))
                    end
                    push!(taskwaiting, pkg)
                end
                if !fancyprint && in(pkg, taskwaiting)
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
            fancyprint && lock(print_lock) do
                printpkgstyle(io, :Precompiling, target)
                print(io, ansi_disablecursor)
            end
            t = Timer(0; interval=1/10)
            anim_chars = ["◐","◓","◑","◒"]
            i = 1
            last_length = 0
            bar = MiniProgressBar(; indent=2, header = "Progress", color = Base.info_color(), percentage=false, always_reprint=true)
            n_total = length(depsmap)
            bar.max = n_total - n_already_precomp
            final_loop = false
            n_print_rows = 0
            while !printloop_should_exit
                lock(print_lock) do
                    term_size = Base.displaysize(ctx.io)::Tuple{Int,Int}
                    num_deps_show = term_size[1] - 3
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
                        for dep in pkg_queue_show
                            loaded = warn_loaded && haskey(Base.loaded_modules, dep)
                            _name = haskey(exts, dep) ? string(exts[dep], " → ", dep.name) : dep.name
                            name = dep in direct_deps ? _name : string(color_string(_name, :light_black))
                            line = if dep in precomperr_deps
                                string(color_string("  ? ", Base.warn_color()), name)
                            elseif haskey(failed_deps, dep)
                                string(color_string("  ✗ ", Base.error_color()), name)
                            elseif was_recompiled[dep]
                                !loaded && interrupted_or_done.set && continue
                                loaded || @async begin # keep successful deps visible for short period
                                    sleep(1);
                                    filter!(!isequal(dep), pkg_queue)
                                end
                                string(color_string("  ✓ ", loaded ? Base.warn_color() : :green), name)
                            elseif started[dep]
                                # Offset each spinner animation using the first character in the package name as the seed.
                                # If not offset, on larger terminal fonts it looks odd that they all sync-up
                                anim_char = anim_chars[(i + Int(dep.name[1])) % length(anim_chars) + 1]
                                anim_char_colored = dep in direct_deps ? anim_char : color_string(anim_char, :light_black)
                                waiting = if haskey(pkgspidlocked, dep)
                                    who_has_lock = pkgspidlocked[dep]
                                    color_string(" Being precompiled by $(who_has_lock)", Base.info_color())
                                elseif dep in taskwaiting
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
    @debug "precompile: starting precompilation loop"
    ## precompilation loop
    for (pkg, deps) in depsmap
        cachepaths = Base.find_all_in_cache_path(pkg)
        sourcepath = Base.locate_package(pkg)
        if sourcepath === nothing
            failed_deps[pkg] = "Error: Missing source file for $(pkg)"
            notify(was_processed[pkg])
            continue
        end
        # Heuristic for when precompilation is disabled
        if occursin(r"\b__precompile__\(\s*false\s*\)", read(sourcepath, String))
            notify(was_processed[pkg])
            continue
        end

        single_requested_pkg = if length(pkgs) == 1
            only(pkgs).name == pkg.name
        elseif project_pkgid isa Base.PkgId
            pkg == project_pkgid # if a package project is being precompiled, consider the package requested
        else
            false
        end

        task = @async begin
            try
                loaded = haskey(Base.loaded_modules, pkg)
                for dep in deps # wait for deps to finish
                    wait(was_processed[dep])
                end

                pkgspec = get_or_make_pkgspec(pkg_specs, ctx, pkg.uuid)
                suspended = precomp_suspended(pkgspec)
                queued = precomp_queued(pkgspec)

                circular = pkg in circular_deps
                is_stale = true
                if !circular && (queued || (!suspended && (is_stale = !Base.isprecompiled(pkg; ignore_loaded=true, stale_cache, cachepaths, sourcepath))))
                    Base.acquire(parallel_limiter)
                    is_direct_dep = pkg in direct_deps

                    # std monitoring
                    std_pipe = Base.link_pipe!(Pipe(); reader_supports_async=true, writer_supports_async=true)
                    t_monitor = @async monitor_std(pkg, std_pipe; single_requested_pkg)

                    _name = haskey(exts, pkg) ? string(exts[pkg], " → ", pkg.name) : pkg.name
                    name = is_direct_dep ? _name : string(color_string(_name, :light_black))
                    !fancyprint && lock(print_lock) do
                        isempty(pkg_queue) && printpkgstyle(io, :Precompiling, target)
                    end
                    push!(pkg_queue, pkg)
                    started[pkg] = true
                    fancyprint && notify(first_started)
                    if interrupted_or_done.set
                        notify(was_processed[pkg])
                        Base.release(parallel_limiter)
                        return
                    end
                    try
                        # allows processes to wait if another process is precompiling a given package to
                        # a functionally identical package cache (except for preferences, which may differ)
                        t = @elapsed ret = maybe_cachefile_lock(io, print_lock, fancyprint, pkg, pkgspidlocked, hascolor) do
                            Logging.with_logger(Logging.NullLogger()) do
                                # The false here means we ignore loaded modules, so precompile for a fresh session
                                Base.compilecache(pkg, sourcepath, std_pipe, std_pipe, false)
                            end
                        end
                        t_str = timing ? string(lpad(round(t * 1e3, digits = 1), 9), " ms") : ""
                        if ret isa Base.PrecompilableError
                            push!(precomperr_deps, pkg)
                            precomp_queue!(get_or_make_pkgspec(pkg_specs, ctx, pkg.uuid))
                            !fancyprint && lock(print_lock) do
                                println(io, t_str, color_string("  ? ", Base.warn_color()), name)
                            end
                        else
                            queued && precomp_dequeue!(get_or_make_pkgspec(pkg_specs, ctx, pkg.uuid))
                            !fancyprint && lock(print_lock) do
                                println(io, t_str, color_string("  ✓ ", loaded ? Base.warn_color() : :green), name)
                            end
                            was_recompiled[pkg] = true
                        end
                        loaded && (n_loaded += 1)
                    catch err
                        close(std_pipe.in) # close pipe to end the std output monitor
                        wait(t_monitor)
                        if err isa ErrorException || (err isa ArgumentError && startswith(err.msg, "Invalid header in cache file"))
                            failed_deps[pkg] = (strict || is_direct_dep) ? string(sprint(showerror, err), "\n", strip(get(std_outputs, pkg, ""))) : ""
                            delete!(std_outputs, pkg) # so it's not shown as warnings, given error report
                            !fancyprint && lock(print_lock) do
                                println(io, timing ? " "^9 : "", color_string("  ✗ ", Base.error_color()), name)
                            end
                            queued && precomp_dequeue!(get_or_make_pkgspec(pkg_specs, ctx, pkg.uuid))
                            precomp_suspend!(get_or_make_pkgspec(pkg_specs, ctx, pkg.uuid))
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
                    suspended && push!(skipped_deps, pkg)
                end
                n_done += 1
                notify(was_processed[pkg])
            catch err_outer
                handle_interrupt(err_outer) || rethrow()
                notify(was_processed[pkg])
            finally
                filter!(!istaskdone, tasks)
                length(tasks) == 1 && notify(interrupted_or_done)
            end
        end
        push!(tasks, task)
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
    save_precompile_state() # save lists to scratch space
    fancyprint && wait(t_print)
    quick_exit = !all(istaskdone, tasks) || interrupted # if some not finished internal error is likely
    seconds_elapsed = round(Int, (time_ns() - time_start) / 1e9)
    ndeps = count(values(was_recompiled))
    if ndeps > 0 || !isempty(failed_deps) || (quick_exit && !isempty(std_outputs))
        str = sprint() do iostr
            if !quick_exit
                plural = ndeps == 1 ? "y" : "ies"
                print(iostr, "  $(ndeps) dependenc$(plural) successfully precompiled in $(seconds_elapsed) seconds")
                if n_already_precomp > 0 || !isempty(circular_deps) || !isempty(skipped_deps)
                    n_already_precomp > 0 && (print(iostr, ". $n_already_precomp already precompiled"))
                    !isempty(circular_deps) && (print(iostr, ". $(length(circular_deps)) skipped due to circular dependency"))
                    !isempty(skipped_deps) && (print(iostr, ". $(length(skipped_deps)) skipped during auto due to previous errors"))
                    print(iostr, ".")
                end
                if n_loaded > 0
                    plural1 = n_loaded == 1 ? "y" : "ies"
                    plural2 = n_loaded == 1 ? "a different version is" : "different versions are"
                    plural3 = n_loaded == 1 ? "" : "s"
                    print(iostr, "\n  ",
                        color_string(string(n_loaded), Base.warn_color()),
                        " dependenc$(plural1) precompiled but $(plural2) currently loaded. Restart julia to access the new version$(plural3)"
                    )
                end
                if !isempty(precomperr_deps)
                    pluralpc = length(precomperr_deps) == 1 ? "y" : "ies"
                    print(iostr, "\n  ",
                        color_string(string(length(precomperr_deps)), Base.warn_color()),
                        " dependenc$(pluralpc) failed but may be precompilable after restarting julia"
                    )
                end
            end
            # show any stderr output, even if Pkg.precompile has been interrupted (quick_exit=true), given user may be
            # interrupting a hanging precompile job with stderr output. julia#48371
            filter!(kv -> !isempty(strip(last(kv))), std_outputs) # remove empty output
            if !isempty(std_outputs)
                plural1 = length(std_outputs) == 1 ? "y" : "ies"
                plural2 = length(std_outputs) == 1 ? "" : "s"
                print(iostr, "\n  ", color_string("$(length(std_outputs))", Base.warn_color()), " dependenc$(plural1) had output during precompilation:")
                for (pkgid, err) in std_outputs
                    err = if pkgid == pkg_liveprinted
                        "[Output was shown above]"
                    else
                        join(split(strip(err), "\n"), color_string("\n│  ", Base.warn_color()))
                    end
                    name = haskey(exts, pkgid) ? string(exts[pkgid], " → ", pkgid.name) : pkgid.name
                    print(iostr, color_string("\n┌ ", Base.warn_color()), name, color_string("\n│  ", Base.warn_color()), err, color_string("\n└  ", Base.warn_color()))
                end
            end
        end
        let str=str
            lock(print_lock) do
                println(io, str)
            end
        end
        quick_exit && return
        err_str = ""
        n_direct_errs = 0
        for (dep, err) in failed_deps
            if strict || (dep in direct_deps)
                err_str = string(err_str, "\n$dep\n\n$err", (n_direct_errs > 0 ? "\n" : ""))
                n_direct_errs += 1
            end
        end
        if err_str != ""
            pluralde = n_direct_errs == 1 ? "y" : "ies"
            direct = strict ? "" : "direct "
            err_msg = "The following $n_direct_errs $(direct)dependenc$(pluralde) failed to precompile:\n$(err_str[1:end-1])"
            if internal_call # aka. auto-precompilation
                if isinteractive() && !get(ENV, "CI", false)
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
                pkgerror(err_msg)
            end
        end
    end
    nothing
end

function _color_string(cstr::String, col::Union{Int64, Symbol}, hascolor)
    if hascolor
        enable_ansi  = get(Base.text_colors, col, Base.text_colors[:default])
        disable_ansi = get(Base.disable_text_style, col, Base.text_colors[:default])
        return string(enable_ansi, cstr, disable_ansi)
    else
        return cstr
    end
end

function maybe_cachefile_lock(f, io::IO, print_lock::ReentrantLock, fancyprint::Bool, pkg::Base.PkgId, pkgspidlocked::Dict{Base.PkgId,String}, hascolor)
    stale_age = Base.compilecache_pidlock_stale_age
    pidfile = Base.compilecache_pidfile_path(pkg)
    cachefile = FileWatching.trymkpidlock(f, pidfile; stale_age)
    if cachefile === false
        pid, hostname, age = FileWatching.Pidfile.parse_pidfile(pidfile)
        pkgspidlocked[pkg] = if isempty(hostname) || hostname == gethostname()
            if pid == getpid()
                "an async task in this process (pidfile: $pidfile)"
            else
                "another process (pid: $pid, pidfile: $pidfile)"
            end
        else
            "another machine (hostname: $hostname, pid: $pid, pidfile: $pidfile)"
        end
        !fancyprint && lock(print_lock) do
            println(io, "    ", pkg.name, _color_string(" Being precompiled by $(pkgspidlocked[pkg])", Base.info_color(), hascolor))
        end
        # wait until the lock is available
        FileWatching.mkpidlock(pidfile; stale_age) do
            # double-check in case the other process crashed or the lock expired
            if Base.isprecompiled(pkg; ignore_loaded=true) # don't use caches for this as the env state will have changed
                return nothing # returning nothing indicates a process waited for another
            else
                delete!(pkgspidlocked, pkg)
                return f() # precompile
            end
        end
    end
    return cachefile
end

const pkgs_precompile_suspended = PackageSpec[] # packages that shouldn't be retried during autoprecomp
const pkgs_precompile_pending = PackageSpec[] # packages that need to be retried after restart
function save_precompile_state()
    path = Operations.pkg_scratchpath()
    for (prefix, store) in (("suspend_cache_", pkgs_precompile_suspended), ("pending_cache_", pkgs_precompile_pending))
        fpath = joinpath(path, string(prefix, hash(string(Base.active_project(), Base.VERSION))))
        if isempty(store)
            Base.rm(fpath, force=true)
        else
            mkpath(path); Base.rm(fpath, force=true)
            open(fpath, "w") do io
                serialize(io, store)
            end
        end
    end
    return nothing
end
function recall_precompile_state()
    for (prefix, store) in (("suspend_cache_", pkgs_precompile_suspended), ("pending_cache_", pkgs_precompile_pending))
        fpath = joinpath(Operations.pkg_scratchpath(), string(prefix, hash(string(Base.active_project(), Base.VERSION))))
        if isfile(fpath)
            open(fpath) do io
                try
                    pkgspecs = deserialize(io)::Vector{PackageSpec}
                    append!(empty!(store), pkgspecs)
                catch
                    empty!(store)
                end
            end
            Base.rm(fpath, force=true)
        else
            empty!(store)
        end
    end
    return nothing
end
function precomp_suspend!(pkg::PackageSpec)
    precomp_suspended(pkg) || push!(pkgs_precompile_suspended, pkg)
    return
end
precomp_unsuspend!() = empty!(pkgs_precompile_suspended)
precomp_suspended(pkg::PackageSpec) = pkg in pkgs_precompile_suspended
function precomp_prune_suspended!(pkgs::Vector{PackageSpec})
    filter!(in(pkgs), pkgs_precompile_suspended)
    unique!(pkgs_precompile_suspended)
    return
end

function precomp_queue!(pkg::PackageSpec)
    precomp_suspended(pkg) || push!(pkgs_precompile_pending, pkg)
    return
end
precomp_dequeue!(pkg::PackageSpec) = filter!(!isequal(pkg), pkgs_precompile_pending)
precomp_queued(pkg::PackageSpec) = pkg in pkgs_precompile_pending
