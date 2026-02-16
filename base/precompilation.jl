module Precompilation

using Base: CoreLogging, PkgId, UUID, SHA1, parsed_toml, project_file_name_uuid, project_names,
            project_file_manifest_path, get_deps, preferences_names, isaccessibledir, isfile_casesensitive,
            base_project, isdefined

const Config = Pair{Cmd, Base.CacheFlags}
const PkgConfig = Tuple{PkgId,Config}

struct PrecompileRequest
    pkgs::Vector{String}
    internal_call::Bool
    strict::Bool
    warn_loaded::Bool
    timing::Bool
    _from_loading::Bool
    configs::Vector{Config}
    io::IOContext
    fancyprint::Bool
    manifest::Bool
    ignore_loaded::Bool
    detachable::Bool
    result::Channel{Any}
end

# Background precompilation state
mutable struct BackgroundPrecompileState
    task::Union{Nothing, Task}
    interrupt_requested::Bool
    cancel_requested::Bool
    monitoring::Bool
    completed_at::Union{Nothing, Float64}
    result::Union{Nothing, String}
    return_value::Any
    exception::Any
    lock::ReentrantLock
    task_done::Threads.Condition
    signal_channels::Vector{Channel{Int32}}  # channels for broadcasting signals to all subprocesses
    pending_pkgids::Set{PkgId}  # packages queued and currently being precompiled
    pkg_done::Threads.Condition  # notified when a package finishes precompiling
    work_channel::Channel{PrecompileRequest}  # channel for injecting requests into running task
    verbose::Bool  # show PIDs and CPU% for each worker
    detachable::Bool  # whether the monitor can be detached
    cancel_confirming::Bool  # waiting for Enter to confirm cancel
    cancel_confirm_deadline::Float64  # time() deadline for cancel confirmation
end
Base.lock(f, bg::BackgroundPrecompileState) = lock(f, bg.lock)
Base.lock(bg::BackgroundPrecompileState) = lock(bg.lock)
Base.unlock(bg::BackgroundPrecompileState) = unlock(bg.lock)

const BG = BackgroundPrecompileState(nothing, false, false, false, nothing, nothing, nothing, nothing, ReentrantLock(), Threads.Condition(), Channel{Int32}[], Set{PkgId}(), Threads.Condition(), Channel{PrecompileRequest}(Inf), false, false, false, 0.0)

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

ExplicitEnv() = ExplicitEnv(Base.active_project())
function ExplicitEnv(::Nothing, envpath::String="")
    ExplicitEnv(envpath,
        Dict{String, UUID}(),     # project_deps
        Dict{String, UUID}(),     # project_weakdeps
        Dict{String, UUID}(),     # project_extras
        Dict{String, Vector{UUID}}(), # project_extensions
        Dict{UUID, Vector{UUID}}(),   # deps
        Dict{UUID, Vector{UUID}}(),   # weakdeps
        Dict{UUID, Dict{String, Vector{UUID}}}(), # extensions
        Dict{UUID, String}(),     # names
        Dict{UUID, Union{SHA1, String, Nothing, Missing}}())
end
function ExplicitEnv(envpath::String)
    # Handle missing project file by creating an empty environment
    if !isfile(envpath) || project_file_manifest_path(envpath) === nothing
        envpath = abspath(envpath)
        return ExplicitEnv(nothing, envpath)
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
            names[uuid] = name
            project_uuid_to_name[name] = uuid
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
        project_deps[proj_name] = proj_uuid
        names[proj_uuid] = proj_name
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
    max::Int = 1
    header::String = ""
    color::Symbol = :nothing
    width::Int = 40
    current::Int = 0
    prev::Int = 0
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
    termwidth = @something termwidth (displaysize(io)::Tuple{Int,Int})[2]
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

can_fancyprint(io::IO) = @something(get(io, :force_fancyprint, nothing), (io isa Base.TTY && (get(ENV, "CI", nothing) != "true")))

function printpkgstyle(io, header, msg; color=:green)
    return @lock io begin
        printstyled(io, header; color, bold=true)
        println(io, " ", msg)
    end
end

# name or parent → ext
function full_name(ext_to_parent::Dict{PkgId, PkgId}, pkg::PkgId)
    if haskey(ext_to_parent, pkg)
        return string(ext_to_parent[pkg].name, " → ", pkg.name)
    else
        return pkg.name
    end
end

function excluded_circular_deps_explanation(io::IOContext, ext_to_parent::Dict{PkgId, PkgId}, circular_deps, cycles)
    outer_deps = copy(circular_deps)
    cycles_names = ""
    hascolor = get(io, :color, false)::Bool
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
            line = color_string(line, :light_black, hascolor) * full_name(ext_to_parent, pkg) * "\n"
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


function scan_pkg!(stack, could_be_cycle, cycles, pkg, dmap)
    if haskey(could_be_cycle, pkg)
        return could_be_cycle[pkg]
    else
        return scan_deps!(stack, could_be_cycle, cycles, pkg, dmap)
    end
end
function scan_deps!(stack, could_be_cycle, cycles, pkg, dmap)
    push!(stack, pkg)
    cycle = nothing
    for dep in dmap[pkg]
        if dep in stack
            # Created fresh cycle
            cycle′ = stack[findlast(==(dep), stack):end]
            if cycle === nothing || length(cycle′) < length(cycle)
                cycle = cycle′ # try to report smallest cycle possible
            end
        elseif scan_pkg!(stack, could_be_cycle, cycles, dep, dmap)
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

function is_precompiling_in_background()
    lock(BG) do
        BG.task !== nothing && !istaskdone(BG.task)
    end
end

# Check if `pkg` is currently being precompiled by a background task.
# This should be called while holding require_lock to avoid races.
# Returns true if the package is pending.
function is_package_pending(pkg::PkgId)
    @lock BG begin
        BG.task === nothing && return false
        istaskdone(BG.task) && return false
        return pkg ∈ BG.pending_pkgids
    end
end

# Monitor the precompile progress for `pkg` until that package finishes.
# This should be called after unlocking require_lock.
function wait_for_pending_package(pkg::PkgId)
    is_package_pending(pkg) || return false
    printpkgstyle(stderr, :Info, "$(pkg.name) is currently being precompiled in the background. Waiting for it to finish...", color = Base.info_color())
    monitor_background_precompile(stderr, false, pkg)
    return true
end

# Broadcast a signal to all active precompilation subprocesses.
function broadcast_signal(sig::Int32)
    lock(BG) do
        for ch in BG.signal_channels
            try; isopen(ch) && put!(ch, sig); catch e; e isa InvalidStateException || rethrow(); end
        end
    end
end
broadcast_signal(sig::Integer) = broadcast_signal(Int32(sig))

function stop_background_precompile(; graceful::Bool = true)
    return lock(BG) do
        if BG.task !== nothing && !istaskdone(BG.task)
            if graceful
                BG.interrupt_requested = true
            else
                BG.cancel_requested = true
                broadcast_signal(Base.SIGKILL)
            end
            try; close(BG.work_channel); catch; end
            return true
        end
        return false
    end
end

const register_atexit_hook = Base.OncePerProcess{Nothing}() do
    Base.atexit() do
        task = @lock BG BG.task
        task === nothing && return
        istaskdone(task) && return
        stop_background_precompile(; graceful=false)
        wait(task)
    end
    nothing
end

function keyboard_tip(s::BackgroundPrecompileState)
    s.monitoring || return "", :default
    color = s.cancel_confirming ? Base.info_color() : :default
    if s.cancel_confirming
        remaining = max(0, ceil(Int, s.cancel_confirm_deadline - time()))
        return "Press Enter to cancel precompilation (ignoring in $(remaining)s)", color
    end
    if s.detachable
        return "Press `?` for help, `c` to cancel, `d` to detach.", color
    else
        return "Press `?` for help, `c` to cancel.", color
    end
end

function monitor_background_precompile(io::IO = stderr, detachable::Bool = true, wait_for_pkg::Union{Nothing, PkgId} = nothing)
    local completed_at::Union{Nothing, Float64}
    local task

    @lock BG begin
        completed_at = BG.completed_at
        task = BG.task
    end

    if task === nothing || istaskdone(task)
        if completed_at !== nothing
            elapsed = time() - completed_at
            time_str = if elapsed < 60
                "$(round(Int, elapsed)) seconds ago"
            elseif elapsed < 3600
                "$(round(Int, elapsed / 60)) minutes ago"
            elseif elapsed < 86400
                "$(round(Int, elapsed / 3600)) hours ago"
            else
                "$(round(Int, elapsed / 86400)) days ago"
            end
            printpkgstyle(io, :Info, "Background precompilation completed $time_str", color = Base.info_color())
            result = @lock BG BG.result
            if result !== nothing
                println(io, "  ", result)
            end
        else
            printpkgstyle(io, :Info, "No background precompilation is running or has been run in this session", color = Base.info_color())
        end
        return
    end

    # Enable output from do_precompile
    @lock BG BG.monitoring = true

    exit_requested = Ref(false)
    cancel_requested = Ref(false)
    interrupt_requested = Ref(false)

    # Start a task to listen for keypresses (only if stdin isn't already being
    # consumed in raw mode by another reader, e.g. runtests.jl's stdin_monitor)
    key_task = if stdin isa Base.TTY
        Threads.@spawn :samepool try
            trylock(stdin.raw_lock) || return
            @lock BG begin
                BG.detachable = detachable
                BG.cancel_confirming = false
            end
            try
                term = Base.Terminals.TTYTerminal(get(ENV, "TERM", "dumb"), stdin, stdout, stderr)
                Base.Terminals.raw!(term, true)
                try
                    while true
                        completed = @lock BG (BG.completed_at !== nothing)
                        if completed || exit_requested[] || cancel_requested[] || interrupt_requested[]
                            break
                        end
                        Base.wait_readnb(stdin, 1)
                        completed = @lock BG (BG.completed_at !== nothing)
                        if completed || exit_requested[] || cancel_requested[] || interrupt_requested[]
                            break
                        end
                        bytesavailable(stdin) > 0 || continue
                        c = read(stdin, Char)
                        # If waiting for cancel confirmation, Enter confirms, anything else aborts
                        cancel_confirmed = @lock BG begin
                            if BG.cancel_confirming
                                BG.cancel_confirming = false
                                c in ('\r', '\n')
                            else
                                false
                            end
                        end
                        if cancel_confirmed
                            cancel_requested[] = true
                            println(io)
                            @lock BG BG.cancel_requested = true
                            broadcast_signal(Base.SIGKILL)
                            break
                        end
                        if c in ('c', 'C')
                            @lock BG begin
                                BG.cancel_confirming = true
                                BG.cancel_confirm_deadline = time() + 5.0
                            end
                        elseif detachable && c in ('d', 'D', 'q', 'Q', ']')
                            exit_requested[] = true
                            println(io)  # newline after keypress
                            break
                        elseif c == '\x03'  # Ctrl-C
                            interrupt_requested[] = true
                            println(io)  # newline after keypress
                            @lock BG BG.interrupt_requested = true
                            broadcast_signal(Base.SIGINT)
                            break
                        elseif c in ('i', 'I')
                            broadcast_signal(Sys.isapple() ? Base.SIGINFO : Base.SIGUSR1)
                        elseif c in ('v', 'V')
                            @lock BG BG.verbose = !BG.verbose
                        elseif c in ('?', 'h', 'H')
                            @lock io begin
                                println(io)
                                println(io, "  Keyboard shortcuts:")
                                println(io, "    c       Cancel precompilation via killing subprocesses (press Enter to confirm)")
                                if detachable
                                    println(io, "    d/q/]   Detach (precompilation continues in background)")
                                end
                                println(io, "    i       Send profiling signal to subprocesses")
                                fields = Sys.iswindows() ? "elapsed time and PID" : "elapsed time, PID, CPU% and memory"
                                println(io, "    v       Toggle verbose mode (show $(fields) for each worker)")
                                println(io, "    Ctrl-C  Interrupt (sends SIGINT, shows output)")
                                println(io, "    ?/h     Show this help")
                            end
                        end
                    end
                finally
                    Base.Terminals.raw!(term, false)
                end
            catch err
                err isa EOFError && return
                exit_requested[] = true
                rethrow()
            finally
                Base.reseteof(stdin)
                @lock BG begin
                    BG.cancel_confirming = false
                end
                unlock(stdin.raw_lock)
            end
        finally
            @lock BG.task_done notify(BG.task_done)
        end
    else
        nothing
    end

    # Wake up key_task by signaling EOF on stdin so wait_readnb returns
    wake_key_task = () -> begin
        if key_task !== nothing && !istaskdone(key_task)
            lock(stdin.cond)
            try
                stdin.status = Base.StatusEOF
                notify(stdin.cond)
            finally
                unlock(stdin.cond)
            end
        end
    end

    # If waiting for a specific package, spawn a watcher that exits when it's done
    pkg_watcher = if wait_for_pkg !== nothing
        Threads.@spawn :samepool begin
            @lock BG.pkg_done begin
                while wait_for_pkg ∈ BG.pending_pkgids
                    wait(BG.pkg_done)
                end
            end
            exit_requested[] = true
            @lock BG.task_done notify(BG.task_done)
        end
    else
        nothing
    end

    return try
        # Wait for task completion or user action
        while !exit_requested[] && !cancel_requested[] && !interrupt_requested[]
            completed = @lock BG (BG.completed_at !== nothing)
            completed && break
            @lock BG.task_done wait(BG.task_done)
        end

        # If user requested cancel, stop the background task
        if cancel_requested[]
            @lock BG BG.monitoring = false
            key_task !== nothing && wait(key_task)
            print(io, "\e[?25h\e[J")  # enable cursor + clear to end
            printpkgstyle(io, :Info, "Canceling precompilation...\e[J", color = Base.info_color())
            wait(task; throw=false)
            return
        end

        # If user requested interrupt, wait for background task to finish cleanly
        if interrupt_requested[]
            key_task !== nothing && wait(key_task)
            # Escalate to SIGKILL if background task doesn't finish promptly
            escalation = Timer(5) do _
                broadcast_signal(Base.SIGKILL)
            end
            wait(task; throw=false)
            close(escalation)
            return
        end

        # If we were waiting for a specific package and it finished, clean up silently
        if exit_requested[] && wait_for_pkg !== nothing
            @lock BG BG.monitoring = false
            if key_task !== nothing
                wake_key_task()
                wait(key_task)
            end
            print(io, "\e[?25h\e[J")  # enable cursor + clear to end
            return
        end

        # If user requested exit, clean up and return
        if exit_requested[]
            @lock BG BG.monitoring = false
            key_task !== nothing && wait(key_task)
            print(io, "\e[?25h\e[J")  # enable cursor + clear to end
            printpkgstyle(io, :Precompiling, "detached. Precompilation will continue in the background. Monitor with `precompile --monitor`.\e[J", color = Base.info_color())
            return
        end

        # Normal completion - signal key_task to exit and wait
        if key_task !== nothing
            wake_key_task()
            wait(key_task)
        end

        wait(task; throw=false)
    catch e
        # Clean up on error
        @lock BG BG.monitoring = false
        if key_task !== nothing
            exit_requested[] = true
            wake_key_task()
            try; wait(key_task); catch; end
        end
        rethrow()
    end
end


"""
    precompilepkgs(pkgs; kwargs...)

Precompile packages and their dependencies, with support for parallel compilation,
progress tracking, and various compilation configurations.

`pkgs::Union{Vector{String}, Vector{PkgId}}`: Packages to precompile. When
empty (default), precompiles all project dependencies. When specified,
precompiles only the given packages and their dependencies (unless
`manifest=true`).

!!! note
    Errors will only throw when precompiling the top-level dependencies, given that
    not all manifest dependencies may be loaded by the top-level dependencies on the given system.
    This can be overridden to make errors in all dependencies throw by setting the kwarg `strict` to `true`

# Keyword Arguments
- `internal_call::Bool`: Indicates this is an automatic precompilation call
  from somewhere external (e.g. Pkg). Do not use this parameter.

- `strict::Bool`: Controls error reporting scope. When `false` (default), only reports
  errors for direct project dependencies. Only relevant when `manifest=true`.

- `warn_loaded::Bool`: When `true` (default), checks for and warns about packages that are
  precompiled but already loaded with a different version. Displays a warning that Julia
  needs to be restarted to use the newly precompiled versions.

- `timing::Bool`: When `true` (not default), displays timing information for
  each package compilation, but only if compilation might have succeeded.
  Disables fancy progress bar output (timing is shown in simple text mode).

- `_from_loading::Bool`: Internal flag indicating the call originated from the
  package loading system. When `true` (not default): returns early instead of
  throwing when packages are not found; suppresses progress messages when not
  in an interactive session; allows packages outside the current environment to
  be added as serial precompilation jobs; skips LOADING_CACHE initialization;
  and changes cachefile locking behavior.

- `configs::Union{Config,Vector{Config}}`: Compilation configurations to use. Each Config
  is a `Pair{Cmd, Base.CacheFlags}` specifying command flags and cache flags. When
  multiple configs are provided, each package is precompiled for each configuration.

- `io::IO`: Output stream for progress messages, warnings, and errors. Can be
  redirected (e.g., to `devnull` when called from loading in non-interactive mode).

- `fancyprint::Bool`: Controls output format. When `true`, displays an animated progress
  bar with spinners. When `false`, instead enables `timing` mode. Automatically
  disabled when `timing=true` or when called from loading in non-interactive mode.

- `manifest::Bool`: Controls the scope of packages to precompile. When `false` (default),
  precompiles only packages specified in `pkgs` and their dependencies. When `true`,
  precompiles all packages in the manifest (workspace mode), typically used by Pkg for
  workspace precompile requests.

- `ignore_loaded::Bool`: Controls whether already-loaded packages affect cache
  freshness checks. When `false` (not default), loaded package versions are considered when
  determining if cache files are fresh.

- `detachable::Bool`: When `true` (not default), allows detaching from the
  precompilation monitor with the `d` key, letting precompilation continue in
  the background. The monitor can be reattached later via
  [`Base.Precompilation.monitor_background_precompile`](@ref). Pkg.jl passes
  `detachable=true` in interactive sessions.

# Keyboard Controls

When running interactively in a TTY, the following keys are available during
precompilation:

  - **`c`** — Cancel precompilation. Prompts for Enter to confirm; ignored after
    5 seconds or if any other key is pressed.
  - **`d`/`q`/`]`** — Detach (only when `detachable=true`). Returns to the REPL while
    precompilation continues in the background.
  - **`i`** — Info. Sends a profiling signal (SIGINFO on macOS/BSD, SIGUSR1 on
    Linux) to subprocesses, triggering a profile peek without interrupting
    compilation.
  - **`v`** — Toggle verbose mode. Shows elapsed time and worker PID for each actively
    compiling package, plus CPU% and memory (RSS) on Linux and macOS.
  - **`?`/`h`** — Show keyboard shortcut help.
  - **Ctrl-C** — Interrupt. Sends SIGINT to subprocesses and displays their output.

# Return
- `Vector{String}`: Paths to cache files for the requested packages.
- `Nothing`: precompilation should be skipped

# Notes
- Packages in circular dependency cycles are skipped with a warning.
- Packages with `__precompile__(false)` are skipped if they are from loading to
  avoid repeated work on every session.
- Parallel compilation is controlled by `JULIA_NUM_PRECOMPILE_TASKS` environment variable
  (defaults to CPU_THREADS + 1, capped at 16, halved on Windows).
- Extensions are precompiled when all their triggers are available in the environment.
"""
function precompilepkgs(pkgs::Union{Vector{String}, Vector{PkgId}}=String[];
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
                        ignore_loaded::Bool=true,
                        detachable::Bool=false)
    @debug "precompilepkgs called with" pkgs internal_call strict warn_loaded timing _from_loading configs fancyprint manifest ignore_loaded detachable
    # monomorphize this to avoid latency problems
    _precompilepkgs(pkgs, internal_call, strict, warn_loaded, timing, _from_loading,
                   configs isa Vector{Config} ? configs : [configs],
                   io isa IOContext ? io : IOContext(io), fancyprint, manifest, ignore_loaded, detachable)
end

function launch_background_precompile(pkgs::Union{Vector{String}, Vector{PkgId}},
                                       internal_call::Bool,
                                       strict::Bool,
                                       warn_loaded::Bool,
                                       timing::Bool,
                                       _from_loading::Bool,
                                       configs::Vector{Config},
                                       io::IOContext,
                                       fancyprint::Bool,
                                       manifest::Bool,
                                       ignore_loaded::Bool,
                                       detachable::Bool)
    # Stop any existing background precompilation
    lock(BG) do
        if BG.task !== nothing && !istaskdone(BG.task)
            BG.interrupt_requested = true
            @lock BG.task_done notify(BG.task_done)
        end
    end

    # Wait for previous task to complete
    old_task = lock(() -> BG.task, BG)
    if old_task !== nothing
        wait(old_task)
    end

    lock(BG) do
        BG.interrupt_requested = false
        BG.cancel_requested = false
        empty!(BG.signal_channels)
        BG.monitoring = true
        BG.completed_at = nothing
        BG.result = nothing
        BG.return_value = nothing
        BG.exception = nothing
        empty!(BG.pending_pkgids)
        BG.work_channel = Channel{PrecompileRequest}(Inf)
    end

    # Capture necessary context for background task
    pkg_names = pkgs isa Vector{String} ? copy(pkgs) : String[pkg.name for pkg in pkgs]

    # Register an atexit hook (once) to cleanly shut down background precompilation
    # before the event loop is torn down.
    register_atexit_hook()

    # Launch new background precompilation
    lock(BG) do
        wc = BG.work_channel
        BG.task = Threads.@spawn :samepool begin
            try
                ret = do_precompile(pkg_names, internal_call, strict, warn_loaded, timing, _from_loading,
                                    configs, io, fancyprint, manifest, ignore_loaded, detachable, wc)

                @lock BG begin
                    BG.return_value = ret
                end
            catch e
                @lock BG begin
                    if BG.interrupt_requested || BG.cancel_requested
                        BG.result = "Background precompilation was interrupted"
                    else
                        BG.exception = e
                        BG.result = "Background precompilation failed: $(sprint(showerror, e))"
                    end
                end
            finally
                close(wc)
                # Drain pending requests with error
                while isready(wc)
                    req = try; take!(wc); catch; break; end
                    try; put!(req.result, InterruptException()); catch; end
                end
                @lock BG begin
                    BG.task = nothing
                    BG.interrupt_requested = false
                    BG.cancel_requested = false
                    foreach(close, BG.signal_channels)
                    empty!(BG.signal_channels)
                    empty!(BG.pending_pkgids)
                    @lock BG.pkg_done notify(BG.pkg_done)
                    BG.monitoring = false
                    BG.completed_at = time()
                    @lock BG.task_done notify(BG.task_done)
                end
            end
        end
    end

    return nothing
end

function visit_indirect_deps!(direct_deps::Dict{PkgId, Vector{PkgId}}, visited::Set{PkgId},
                               node::PkgId, all_deps::Set{PkgId})
    if node in visited
        return
    end
    push!(visited, node)
    for dep in get(Set{PkgId}, direct_deps, node)
        if !(dep in all_deps)
            push!(all_deps, dep)
            visit_indirect_deps!(direct_deps, visited, dep, all_deps)
        end
    end
    return
end

# Build dependency graph from an ExplicitEnv.
# Returns a NamedTuple of (direct_deps, ext_to_parent, parent_to_exts, triggers, project_deps, serial_deps).
function build_dep_graph(env::ExplicitEnv, _from_loading::Bool, requested_pkgids::Vector{PkgId})
    direct_deps = Dict{PkgId, Vector{PkgId}}()
    parent_to_exts = Dict{PkgId, Vector{PkgId}}()
    ext_to_parent = Dict{PkgId, PkgId}()
    triggers = Dict{PkgId, Vector{PkgId}}()

    for (dep, deps) in env.deps
        pkg = PkgId(dep, env.names[dep])
        Base.in_sysimage(pkg) && continue
        deps = [PkgId(x, env.names[x]) for x in deps]
        direct_deps[pkg] = filter!(!Base.in_sysimage, deps)
        for (ext_name, trigger_uuids) in env.extensions[dep]
            ext_uuid = Base.uuid5(pkg.uuid, ext_name)
            ext = PkgId(ext_uuid, ext_name)
            triggers[ext] = PkgId[pkg]
            all_triggers_available = true
            for trigger_uuid in trigger_uuids
                trigger_name = PkgId(trigger_uuid, env.names[trigger_uuid])
                if trigger_uuid in keys(env.deps) || Base.in_sysimage(trigger_name)
                    push!(triggers[ext], trigger_name)
                else
                    all_triggers_available = false
                    break
                end
            end
            all_triggers_available || continue
            ext_to_parent[ext] = pkg
            direct_deps[ext] = filter(!Base.in_sysimage, triggers[ext])
            if !haskey(parent_to_exts, pkg)
                parent_to_exts[pkg] = PkgId[ext]
            else
                push!(parent_to_exts[pkg], ext)
            end
        end
    end

    project_deps = [
        PkgId(uuid, name)
        for (name, uuid) in env.project_deps if !Base.in_sysimage(PkgId(uuid, name))
    ]
    # consider exts of project deps to be project deps so that errors are reported
    append!(project_deps, keys(filter(d->last(d).name in keys(env.project_deps), ext_to_parent)))

    # An extension effectively depends on another extension if it has a strict superset of its triggers
    for ext_a in keys(ext_to_parent)
        for ext_b in keys(ext_to_parent)
            if triggers[ext_a] ⊋ triggers[ext_b]
                push!(triggers[ext_a], ext_b)
                push!(direct_deps[ext_a], ext_b)
            end
        end
    end

    # A package depends on an extension if it (indirectly) depends on all extension triggers
    indirect_deps = Dict{PkgId, Set{PkgId}}()
    for package in keys(direct_deps)
        all_deps = Set{PkgId}()
        visited = Set{PkgId}()
        visit_indirect_deps!(direct_deps, visited, package, all_deps)
        indirect_deps[package] = all_deps
    end
    for ext in keys(ext_to_parent)
        ext_loadable_in_pkg = Dict{PkgId,Bool}()
        for pkg in keys(direct_deps)
            is_trigger = in(pkg, direct_deps[ext])
            is_extension = in(pkg, keys(ext_to_parent))
            has_triggers = issubset(direct_deps[ext], indirect_deps[pkg])
            ext_loadable_in_pkg[pkg] = !is_extension && has_triggers && !is_trigger
        end
        for (pkg, ext_loadable) in ext_loadable_in_pkg
            if ext_loadable && !any((dep)->ext_loadable_in_pkg[dep], direct_deps[pkg])
                push!(direct_deps[pkg], ext)
            end
        end
    end

    serial_deps = PkgId[]
    if _from_loading
        for pkgid in requested_pkgids
            pkgid === nothing && continue
            if !haskey(direct_deps, pkgid)
                @debug "precompile: package `$(pkgid)` is outside of the environment, so adding as single package serial job"
                direct_deps[pkgid] = PkgId[]
                push!(project_deps, pkgid)
                push!(serial_deps, pkgid)
            end
        end
    end

    return (; direct_deps, ext_to_parent, parent_to_exts, triggers, project_deps, serial_deps)
end

# Detect circular dependencies and notify their Events so waiting tasks can skip them.
# Returns the list of packages involved in or dependent on cycles.
function detect_circular_deps!(direct_deps, serial_deps, was_processed, io, ext_to_parent)
    cycles = Vector{PkgId}[]
    could_be_cycle = Dict{PkgId, Bool}()
    stack = PkgId[]
    circular_deps = PkgId[]
    for pkg in keys(direct_deps)
        @assert isempty(stack)
        pkg in serial_deps && continue
        if scan_pkg!(stack, could_be_cycle, cycles, pkg, direct_deps)
            push!(circular_deps, pkg)
            for (pkg_config, evt) in was_processed
                pkg_config[1] == pkg && notify(evt)
            end
        end
    end
    if !isempty(circular_deps)
        printpkgstyle(io, :Warning, excluded_circular_deps_explanation(io, ext_to_parent, circular_deps, cycles), color=Base.warn_color())
    end
    return circular_deps
end

# Filter the dependency graph to only include requested packages and their transitive deps.
# Returns true if the graph became empty (caller should return early).
function filter_dep_graph!(direct_deps, pkg_names, manifest, project_deps, ext_to_parent, requested_pkgids)
    manifest && return false
    if isempty(pkg_names)
        append!(pkg_names, [pkg.name for pkg in project_deps])
    end
    keep = Set{PkgId}()
    for dep_pkgid in keys(direct_deps)
        if dep_pkgid.name in pkg_names
            push!(keep, dep_pkgid)
            collect_all_deps(direct_deps, dep_pkgid, keep)
        end
    end
    for requested_pkgid in requested_pkgids
        if haskey(direct_deps, requested_pkgid)
            push!(keep, requested_pkgid)
            collect_all_deps(direct_deps, requested_pkgid, keep)
        end
    end
    for ext in keys(ext_to_parent)
        if issubset(collect_all_deps(direct_deps, ext), keep)
            push!(keep, ext)
        end
    end
    filter!(d->in(first(d), keep), direct_deps)
    return isempty(direct_deps)
end

function _precompilepkgs(pkgs::Union{Vector{String}, Vector{PkgId}},
                         internal_call::Bool,
                         strict::Bool,
                         warn_loaded::Bool,
                         timing::Bool,
                         _from_loading::Bool,
                         configs::Vector{Config},
                         io::IOContext,
                         fancyprint′::Bool,
                         manifest::Bool,
                         ignore_loaded::Bool,
                         detachable::Bool)
    # Try to inject into a running background task
    local req = nothing
    injected = @lock BG begin
        if BG.task !== nothing && !istaskdone(BG.task) &&
                isopen(BG.work_channel)
            pkg_names = pkgs isa Vector{String} ? copy(pkgs) : String[pkg.name for pkg in pkgs]
            req = PrecompileRequest(pkg_names, internal_call, strict, warn_loaded, timing, _from_loading,
                                    configs, io, fancyprint′, manifest, ignore_loaded, detachable,
                                    Channel{Any}(1))
            try
                put!(BG.work_channel, req)
                true
            catch
                req = nothing
                false
            end
        else
            false
        end
    end
    if injected
        printpkgstyle(io, :Precompiling, "Merging precompilation request into existing run...", color = Base.info_color())
    else
        launch_background_precompile(pkgs, internal_call, strict, warn_loaded, timing, _from_loading,
                                     configs, io, fancyprint′, manifest, ignore_loaded, detachable)
    end

    if req !== nothing
        # Injected into existing task — monitor until our package finishes, then read result
        wait_for_pkg = if _from_loading && length(pkgs) == 1
            pkgs[1] isa PkgId ? pkgs[1] : Base.identify_package(pkgs[1])
        else
            nothing
        end
        monitor_background_precompile(io.io, detachable, wait_for_pkg)
        if _from_loading
            # _from_loading: package just left pending_pkgids, waiter will put result shortly
            result = take!(req.result)
            result isa Exception && throw(result)
            return result
        end
        # Interactive: if result is ready (task completed), use it; if detached, return nothing
        if isready(req.result)
            result = take!(req.result)
            result isa Exception && throw(result)
            return result
        end
        return nothing
    end

    # Launched new task — wait for full completion
    monitor_background_precompile(io.io, detachable)

    local ret_val, ret_ex
    @lock BG begin
        ret_val = BG.return_value
        ret_ex = BG.exception
    end
    ret_ex !== nothing && throw(ret_ex)
    return ret_val
end

# Shared mutable state for a precompilation session.
Base.@kwdef mutable struct PrecompileSession
    # Configuration (set once, read-only after construction)
    configs::Vector{Config}
    io::IOContext
    logio::IOContext
    logcalls::Union{Nothing, CoreLogging.LogLevel}
    fancyprint::Bool
    hascolor::Bool
    warn_loaded::Bool
    ignore_loaded::Bool
    internal_call::Bool
    strict::Bool
    _from_loading::Bool
    time_start::UInt64
    print_lock::ReentrantLock
    parallel_limiter::Base.Semaphore
    start_loaded_modules::Base.KeySet{PkgId, Dict{PkgId, Module}}
    requested_pkgids::Vector{PkgId}

    # Dependency graph (built by setup, extended by drainer)
    ext_to_parent::Dict{PkgId, PkgId}
    parent_to_exts::Dict{PkgId, Vector{PkgId}}
    triggers::Dict{PkgId, Vector{PkgId}}
    project_deps::Vector{PkgId}
    serial_deps::Vector{PkgId}
    circular_deps::Vector{PkgId}

    # Progress counters and flags
    n_done::Ref{Int}                            = Ref(0)
    n_already_precomp::Ref{Int}                 = Ref(0)
    n_loaded::Ref{Int}                          = Ref(0)
    n_total::Ref{Int}
    n_batches::Ref{Int}                         = Ref(1)
    interrupted::Ref{Bool}                      = Ref(false)
    interrupted_or_done::Ref{Bool}              = Ref(false)
    printloop_should_exit::Ref{Bool}
    target::Ref{Union{Nothing, String}}
    pkg_liveprinted::Ref{Union{Nothing, PkgId}} = Ref{Union{Nothing,PkgId}}(nothing)

    # Per-package tracking
    started::Dict{PkgConfig, Float64}
    was_processed::Dict{PkgConfig, Base.Event}
    was_recompiled::Dict{PkgConfig, Bool}
    failed_deps::Dict{PkgConfig, String}           = Dict{PkgConfig,String}()
    precomperr_deps::Vector{PkgConfig}             = PkgConfig[]
    stale_cache::Dict{StaleCacheKey, Bool}         = Dict{StaleCacheKey,Bool}()
    cachepath_cache::Dict{PkgId, Vector{String}}   = Dict{PkgId,Vector{String}}()
    std_outputs::Dict{PkgConfig, IOBuffer}         = Dict{PkgConfig,IOBuffer}()
    pkg_queue::Vector{PkgConfig}                   = PkgConfig[]
    pkgspidlocked::Dict{PkgConfig, String}         = Dict{PkgConfig,String}()
    taskwaiting::Set{PkgConfig}                    = Set{PkgConfig}()
    active_pids::Dict{PkgConfig, Int32}            = Dict{PkgConfig,Int32}()
    prev_cpu_times::Dict{Int32, UInt64}            = Dict{Int32,UInt64}()

    # Synchronization
    first_started::Base.Event                      = Base.Event()

    # Task tracking
    tasks::Vector{Task}                            = Task[]
    injected_tasks::Vector{Task}                   = Task[]
    result_waiters::Vector{Task}                   = Task[]
end

const ansi_movecol1 = "\e[1G"
const ansi_cleartoend = "\e[0J"
const ansi_cleartoendofline = "\e[0K"
const ansi_enablecursor = "\e[?25h"
const ansi_disablecursor = "\e[?25l"
ansi_moveup(n::Int) = string("\e[", n, "A")

# Mach timebase info for converting Mach absolute time → nanoseconds on macOS.
# Queried once per process at runtime (not at sysimage build time).
@static if Sys.isapple()
    const _mach_timebase = Base.OncePerProcess{Tuple{UInt64,UInt64}}() do
        buf = zeros(UInt32, 2)
        ccall(:mach_timebase_info, Cvoid, (Ptr{UInt32},), buf)
        (UInt64(buf[1]), UInt64(buf[2]))
    end
end

# Read cumulative CPU time (user+system) in nanoseconds and RSS in bytes for a process.
# Returns (cpu_ns=0, rss_bytes=0) if the process no longer exists or on unsupported platforms.
function process_stats(pid::Int32)
    @static if Sys.islinux()
        try
            stat = read("/proc/$(pid)/stat", String)
            # Field 2 (comm) is in parens and may contain spaces; skip past it
            i = findlast(')', stat)
            i === nothing && return (cpu_ns=UInt64(0), rss_bytes=UInt64(0))
            fields = split(@view(stat[nextind(stat, i):end]))
            # fields[1] = state (field 3), so utime=field 14 is at index 12,
            # stime=field 15 at index 13, rss=field 24 at index 22
            length(fields) >= 22 || return (cpu_ns=UInt64(0), rss_bytes=UInt64(0))
            utime = parse(UInt64, fields[12])
            stime = parse(UInt64, fields[13])
            rss_pages = parse(UInt64, fields[22])
            # CLK_TCK is almost always 100 on Linux; 1 tick = 10ms = 10_000_000 ns
            cpu_ns = (utime + stime) * UInt64(10_000_000)
            rss_bytes = rss_pages * UInt64(ccall(:getpagesize, Cint, ()))
            return (; cpu_ns, rss_bytes)
        catch
            return (cpu_ns=UInt64(0), rss_bytes=UInt64(0))
        end
    elseif Sys.isapple()
        try
            buf = Vector{UInt8}(undef, 96)  # sizeof(struct proc_taskinfo)
            ret = ccall((:proc_pidinfo, "libproc"), Cint,
                        (Cint, Cint, UInt64, Ptr{UInt8}, Cint),
                        pid, Cint(4), UInt64(0), buf, Cint(96))
            ret <= 0 && return (cpu_ns=UInt64(0), rss_bytes=UInt64(0))
            # pti_resident_size at offset 8 (bytes)
            rss_bytes = GC.@preserve buf unsafe_load(Ptr{UInt64}(pointer(buf) + 8))
            # pti_total_user at offset 16, pti_total_system at offset 24 (Mach absolute time)
            user = GC.@preserve buf unsafe_load(Ptr{UInt64}(pointer(buf) + 16))
            sys  = GC.@preserve buf unsafe_load(Ptr{UInt64}(pointer(buf) + 24))
            # Convert Mach absolute time to nanoseconds via mach_timebase_info
            numer, denom = _mach_timebase()
            cpu_ns = div((user + sys) * numer, denom)
            return (; cpu_ns, rss_bytes)
        catch
            return (cpu_ns=UInt64(0), rss_bytes=UInt64(0))
        end
    else
        return (cpu_ns=UInt64(0), rss_bytes=UInt64(0))
    end
end

# Compute CPU% and RSS for all active PIDs since last poll.
# Mutates cpu_pcts and rss in place.
function poll_process_stats!(cpu_pcts::Dict{Int32, Float64}, rss::Dict{Int32, UInt64},
                             prev_cpu_times::Dict{Int32, UInt64}, active_pids::Dict{PkgConfig, Int32}, dt::Float64)
    empty!(cpu_pcts)
    empty!(rss)
    @static if !(Sys.islinux() || Sys.isapple())
        return
    end
    for (_, pid) in active_pids
        haskey(cpu_pcts, pid) && continue
        stats = process_stats(pid)
        stats.cpu_ns == 0 && continue
        prev = get(prev_cpu_times, pid, stats.cpu_ns)
        delta = stats.cpu_ns >= prev ? stats.cpu_ns - prev : UInt64(0)
        prev_cpu_times[pid] = stats.cpu_ns
        pct = dt > 0 ? (delta / 1.0e9) / dt * 100.0 : 0.0
        cpu_pcts[pid] = min(pct, 999.9)
        stats.rss_bytes > 0 && (rss[pid] = stats.rss_bytes)
    end
    pids_set = Set(values(active_pids))
    for pid in keys(prev_cpu_times)
        pid in pids_set || delete!(prev_cpu_times, pid)
    end
    return
end

function should_stop(s::PrecompileSession)
    ir, cr = @lock BG begin
        BG.interrupt_requested, BG.cancel_requested
    end
    if ir || cr
        s.interrupted[] = s.interrupted[] || ir
        if !s.interrupted_or_done[]
            s.interrupted_or_done[] = true
            foreach(notify, values(s.was_processed))
            notify(s.first_started)
        end
        return true
    end
    return s.interrupted_or_done[]
end

function make_signal_channel()
    ch = Channel{Int32}(32)
    @lock BG push!(BG.signal_channels, ch)
    return ch
end

function describe_pkg(s::PrecompileSession, pkg::PkgId, is_project_dep::Bool, is_serial_dep::Bool, flags::Cmd, cacheflags::Base.CacheFlags)
    name = full_name(s.ext_to_parent, pkg)
    name = is_project_dep ? name : color_string(name, :light_black, s.hascolor)
    if is_serial_dep
        name *= color_string(" (serial)", :light_black, s.hascolor)
    end
    if length(s.configs) > 1 && !isempty(flags)
        config_str = join(flags, " ")
        name *= color_string(" `$config_str`", :light_black, s.hascolor)
    end
    if length(s.configs) > 1
        config_str = join(Base.translate_cache_flags(cacheflags, Base.DefaultCacheFlags), " ")
        name *= color_string(" $config_str", :light_black, s.hascolor)
    end
    return name
end

function spawn_print_loop!(s::PrecompileSession)
    Threads.@spawn :samepool begin
        try
            wait(s.first_started)
            (isempty(s.pkg_queue) || s.interrupted_or_done[]) && return
            @lock s.print_lock begin
                if BG.monitoring && s.target[] !== nothing
                    printpkgstyle(s.logio, :Precompiling, something(s.target[], ""))
                end
            end
            cursor_disabled = false
            t = Timer(0; interval=1/10)
            anim_chars = ["◐","◓","◑","◒"]
            i = 1
            last_length = 0
            bar = MiniProgressBar(; indent=0, header = "Precompiling packages ", color = :green, percentage=false, always_reprint=true)
            bar.max = s.n_total[] - s.n_already_precomp[]
            final_loop = false
            n_print_rows = 0
            last_poll_time = time()
            cpu_pcts = Dict{Int32, Float64}()
            rss_bytes = Dict{Int32, UInt64}()
            while !s.printloop_should_exit[]
                @lock s.print_lock begin
                    verbose = BG.verbose
                    now_time = time()
                    dt = now_time - last_poll_time
                    if verbose && dt >= 0.5
                        poll_process_stats!(cpu_pcts, rss_bytes, s.prev_cpu_times, s.active_pids, dt)
                        last_poll_time = now_time
                    end
                    term_size = displaysize(s.logio)::Tuple{Int, Int}
                    num_deps_show = max(term_size[1] - 3, 2) # show at least 2 deps
                    pkg_queue_show = if !s.interrupted_or_done[] && length(s.pkg_queue) > num_deps_show
                        last(s.pkg_queue, num_deps_show)
                    else
                        s.pkg_queue
                    end
                    i_local = i
                    final_loop_local = final_loop
                    tip, tip_color = @lock BG begin
                        if BG.cancel_confirming && time() >= BG.cancel_confirm_deadline
                            BG.cancel_confirming = false
                        end
                        keyboard_tip(BG)
                    end
                    str_ = sprint(; context=s.logio) do iostr
                        if i_local > 1
                            print(iostr, ansi_cleartoend)
                        end
                        bar.current = s.n_done[] - s.n_already_precomp[]
                        bar.max = s.n_total[] - s.n_already_precomp[]
                        termwidth = (displaysize(s.logio)::Tuple{Int,Int})[2]
                        if !final_loop_local
                            tip_width = isempty(tip) ? 0 : textwidth(tip) + 1
                            bar_termwidth = termwidth - tip_width
                            s_bar = sprint(io -> show_progress(io, bar; termwidth=bar_termwidth, carriagereturn=false); context=s.logio)
                            if !isempty(tip)
                                s_bar = string(s_bar, " ", color_string(tip, tip_color, s.hascolor))
                            end
                            print(iostr, Base._truncate_at_width_or_chars(true, s_bar, termwidth), "\n")
                        end
                        for pkg_config in pkg_queue_show
                            dep, config = pkg_config
                            loaded = s.warn_loaded && (dep in s.start_loaded_modules)
                            flags, cacheflags = config
                            name = describe_pkg(s, dep, dep in s.project_deps, dep in s.serial_deps, flags, cacheflags)
                            line = if pkg_config in s.precomperr_deps
                                string(color_string("  ? ", Base.warn_color(), s.hascolor), name)
                            elseif haskey(s.failed_deps, pkg_config)
                                string(color_string("  ✗ ", Base.error_color(), s.hascolor), name)
                            elseif s.was_recompiled[pkg_config]
                                !loaded && s.interrupted_or_done[] && continue
                                loaded || Base.errormonitor(Threads.@spawn :samepool begin
                                    sleep(1);
                                    filter!(!isequal(pkg_config), s.pkg_queue)
                                end)
                                string(color_string("  ✓ ", loaded ? Base.warn_color() : :green, s.hascolor), name)
                            elseif s.started[pkg_config] > 0
                                anim_char = anim_chars[(i_local + Int(dep.name[1])) % length(anim_chars) + 1]
                                anim_char_colored = dep in s.project_deps ? anim_char : color_string(anim_char, :light_black, s.hascolor)
                                waiting = if haskey(s.pkgspidlocked, pkg_config)
                                    who_has_lock = s.pkgspidlocked[pkg_config]
                                    color_string(" Being precompiled by $(who_has_lock)", Base.info_color(), s.hascolor)
                                elseif pkg_config in s.taskwaiting
                                    color_string(" Waiting for background task / IO / timer. Interrupt to inspect", Base.warn_color(), s.hascolor)
                                else
                                    ""
                                end

                                pid_info = if verbose && haskey(s.active_pids, pkg_config)
                                    elapsed_str = string(round(Int, now_time - s.started[pkg_config]), "s")
                                    pid = s.active_pids[pkg_config]
                                    pct = get(cpu_pcts, pid, -1.0)
                                    pct_str = pct >= 0 ? string(" | cpu ", round(Int, pct), "%") : ""
                                    mem = get(rss_bytes, pid, UInt64(0))
                                    mem_str = mem > 0 ? string(" | ", Base.format_bytes(mem)) : ""
                                    color_string(" $(elapsed_str) | pid $(pid)$(pct_str)$(mem_str)", Base.info_color(), s.hascolor)
                                else
                                    ""
                                end
                                string("  ", anim_char_colored, " ", name, pid_info, waiting)
                            else
                                string("    ", name)
                            end
                            println(iostr, Base._truncate_at_width_or_chars(true, line, termwidth))
                        end
                    end
                    last_length = length(pkg_queue_show)
                    n_print_rows = count("\n", str_)
                    s.printloop_should_exit[] = s.interrupted_or_done[] && final_loop
                    final_loop = s.interrupted_or_done[]
                    i += 1
                    if BG.monitoring
                        if s.fancyprint && !cursor_disabled
                            print(s.logio, ansi_disablecursor)
                            cursor_disabled = true
                        end
                        if s.printloop_should_exit[]
                            print(s.logio, str_)
                        else
                            print(s.logio, str_, ansi_moveup(n_print_rows), ansi_movecol1)
                        end
                    elseif cursor_disabled
                        print(s.logio, ansi_enablecursor)
                        cursor_disabled = false
                    end
                end
                wait(t)
            end
        finally
            @isdefined(cursor_disabled) && cursor_disabled && print(s.logio, ansi_enablecursor)
        end
    end
end

function spawn_precompile_tasks!(s::PrecompileSession, batch_dd, batch_wp, batch_configs, batch_circular,
                                  batch_reqids, batch_names, batch_reqpkgs, batch_from_loading)
    batch_tasks = Task[]
    for (pkg, deps) in batch_dd
        cachepaths = Base.find_all_in_cache_path(pkg)
        freshpaths = String[]
        s.cachepath_cache[pkg] = freshpaths
        sourcespec = Base.locate_package_load_spec(pkg)
        single_requested_pkg = length(batch_reqpkgs) == 1 &&
            (pkg in batch_reqids || pkg.name in batch_names)
        for config in batch_configs
            pkg_config = (pkg, config)
            if sourcespec === nothing
                s.failed_deps[pkg_config] = "Error: Missing source file for $(pkg)"
                notify(batch_wp[pkg_config])
                continue
            end
            if batch_from_loading && single_requested_pkg && occursin(r"\b__precompile__\(\s*false\s*\)", read(sourcespec.path, String))
                @lock s.print_lock begin
                    Base.@logmsg s.logcalls "Disabled precompiling $(repr("text/plain", pkg)) since the text `__precompile__(false)` was found in file."
                end
                notify(batch_wp[pkg_config])
                continue
            end
            @lock BG push!(BG.pending_pkgids, pkg)
            flags, cacheflags = config
            task = Threads.@spawn :samepool try
                loaded = s.warn_loaded && (pkg in s.start_loaded_modules)
                for dep in deps
                    wait(batch_wp[(dep,config)])
                    if should_stop(s)
                        return
                    end
                end
                circular = pkg in batch_circular
                freshpath = Base.compilecache_freshest_path(pkg; ignore_loaded=s.ignore_loaded,
                    stale_cache=s.stale_cache, cachepath_cache=s.cachepath_cache, cachepaths, sourcespec, flags=cacheflags)
                is_stale = freshpath === nothing
                if !is_stale
                    push!(freshpaths, freshpath)
                end
                if !circular && is_stale
                    Base.acquire(s.parallel_limiter)
                    is_serial_dep = pkg in s.serial_deps
                    is_project_dep = pkg in s.project_deps

                    std_pipe = Base.link_pipe!(Pipe(); reader_supports_async=true, writer_supports_async=true)
                    t_monitor = Threads.@spawn :samepool precompilepkgs_monitor_std(pkg_config, std_pipe,
                        single_requested_pkg, s.ext_to_parent, s.hascolor, s.std_outputs, s.taskwaiting,
                        s.pkg_liveprinted, s.print_lock, s.io, s.fancyprint, ansi_cleartoendofline)

                    try
                        name = describe_pkg(s, pkg, is_project_dep, is_serial_dep, flags, cacheflags)
                        @lock s.print_lock begin
                            if !s.fancyprint && isempty(s.pkg_queue) && BG.monitoring
                                printpkgstyle(s.logio, :Precompiling, something(s.target[], "packages..."))
                            end
                        end
                        push!(s.pkg_queue, pkg_config)
                        s.started[pkg_config] = time()
                        s.fancyprint && notify(s.first_started)
                        if should_stop(s)
                            return
                        end
                        loadable_exts = haskey(s.ext_to_parent, pkg) ? filter((dep)->haskey(s.ext_to_parent, dep), s.triggers[pkg]) : nothing

                        flags_ = if !isempty(deps)
                            `$flags --compiled-modules=strict`
                        else
                            flags
                        end

                        pid_ch = Channel{Int32}(1)
                        if batch_from_loading && pkg in batch_reqids
                            Base.errormonitor(Threads.@spawn :samepool begin
                                pid = try; take!(pid_ch); catch; Int32(0); end
                                pid > 0 && @lock s.print_lock (s.active_pids[pkg_config] = pid)
                            end)
                            t = @elapsed ret = begin
                                Base.compilecache(pkg, sourcespec, std_pipe, std_pipe, !s.ignore_loaded;
                                                  flags=flags_, cacheflags, loadable_exts, signal_channel=make_signal_channel(),
                                                  pid_channel=pid_ch)
                            end
                        else
                            fullname = full_name(s.ext_to_parent, pkg)
                            Base.errormonitor(Threads.@spawn :samepool begin
                                pid = try; take!(pid_ch); catch; Int32(0); end
                                pid > 0 && @lock s.print_lock (s.active_pids[pkg_config] = pid)
                            end)
                            t = @elapsed ret = precompile_pkgs_maybe_cachefile_lock(s.io, s.print_lock, s.fancyprint, pkg_config, s.pkgspidlocked, s.hascolor, s.parallel_limiter, fullname) do
                                if should_stop(s)
                                    return ErrorException("canceled")
                                end
                                local cachepaths = Base.find_all_in_cache_path(pkg)
                                local freshpath = Base.compilecache_freshest_path(pkg; ignore_loaded=s.ignore_loaded,
                                    stale_cache=s.stale_cache, cachepath_cache=s.cachepath_cache, cachepaths, sourcespec, flags=cacheflags)
                                local is_stale = freshpath === nothing
                                if !is_stale
                                    push!(freshpaths, freshpath)
                                    return nothing
                                end
                                s.logcalls === CoreLogging.Debug && @lock s.print_lock begin
                                    @debug "Precompiling $(repr("text/plain", pkg))"
                                end
                                Base.compilecache(pkg, sourcespec, std_pipe, std_pipe, !s.ignore_loaded;
                                                  flags=flags_, cacheflags, loadable_exts, signal_channel=make_signal_channel(),
                                                  pid_channel=pid_ch)
                            end
                        end
                        if ret isa Exception
                            push!(s.precomperr_deps, pkg_config)
                            !s.fancyprint && BG.monitoring && @lock s.print_lock begin
                                println(s.logio, timing_string(t), color_string("  ? ", Base.warn_color(), s.hascolor), name)
                            end
                        else
                            !s.fancyprint && BG.monitoring && @lock s.print_lock begin
                                println(s.logio, timing_string(t), color_string("  ✓ ", loaded ? Base.warn_color() : :green, s.hascolor), name)
                            end
                            if ret !== nothing
                                s.was_recompiled[pkg_config] = true
                                cachefile, _ = ret::Tuple{String, Union{Nothing, String}}
                                push!(freshpaths, cachefile)
                                build_id, _ = Base.parse_cache_buildid(cachefile)
                                stale_cache_key = (pkg, build_id, sourcespec, cachefile, s.ignore_loaded, cacheflags)::StaleCacheKey
                                s.stale_cache[stale_cache_key] = false
                            end
                        end
                        loaded && (s.n_loaded[] += 1)
                    catch err
                        close(std_pipe.in)
                        wait(t_monitor)
                        err isa InterruptException && rethrow()
                        s.failed_deps[pkg_config] = sprint(showerror, err)
                        !s.fancyprint && BG.monitoring && @lock s.print_lock begin
                            println(s.logio, " "^12, color_string("  ✗ ", Base.error_color(), s.hascolor), name)
                        end
                    finally
                        isopen(std_pipe.in) && close(std_pipe.in)
                        wait(t_monitor)
                        try; close(pid_ch); catch; end
                        @lock s.print_lock delete!(s.active_pids, pkg_config)
                        Base.release(s.parallel_limiter)
                        @lock BG begin
                            delete!(BG.pending_pkgids, pkg)
                            @lock BG.pkg_done notify(BG.pkg_done)
                        end
                    end
                else
                    is_stale || (s.n_already_precomp[] += 1)
                end
            finally
                s.n_done[] += 1
                notify(batch_wp[pkg_config])
            end
            push!(batch_tasks, task)
        end
    end
    return batch_tasks
end

function drain_work_channel!(s::PrecompileSession, work_channel::Channel{PrecompileRequest})
    drainer = Threads.@spawn :samepool begin
        try
            while true
                isopen(work_channel) || break
                should_stop(s) && break
                request = try; take!(work_channel); catch; break; end
                try
                    new_env = ExplicitEnv()
                    req_pkgids = PkgId[]
                    for name in request.pkgs
                        pkgid = Base.identify_package(name)
                        pkgid !== nothing && push!(req_pkgids, pkgid)
                    end
                    new_graph = build_dep_graph(new_env, request._from_loading, req_pkgids)
                    @lock s.print_lock begin
                        merge!(s.ext_to_parent, new_graph.ext_to_parent)
                        merge!(s.parent_to_exts, new_graph.parent_to_exts)
                        merge!(s.triggers, new_graph.triggers)
                        for p in new_graph.project_deps
                            p in s.project_deps || push!(s.project_deps, p)
                        end
                        for dep in new_graph.serial_deps
                            dep in s.serial_deps || push!(s.serial_deps, dep)
                        end
                    end
                    new_pkg_names = copy(request.pkgs)
                    new_dd = new_graph.direct_deps
                    filter_dep_graph!(new_dd, new_pkg_names, request.manifest, new_graph.project_deps, new_graph.ext_to_parent, req_pkgids)
                    skip_pkgs = Set{PkgId}()
                    @lock BG begin
                        for pkgid in keys(new_dd)
                            if pkgid in BG.pending_pkgids
                                push!(skip_pkgs, pkgid)
                            end
                        end
                    end
                    for pkgid in skip_pkgs
                        delete!(new_dd, pkgid)
                    end
                    for (_, deps) in new_dd
                        setdiff!(deps, skip_pkgs)
                    end
                    new_wp = Dict{PkgConfig, Base.Event}()
                    for config in request.configs
                        for pkgid in keys(new_dd)
                            pkg_config = (pkgid, config)
                            @lock s.print_lock begin
                                get!(s.started, pkg_config, 0.0)
                                get!(s.was_recompiled, pkg_config, false)
                            end
                            new_wp[pkg_config] = Base.Event()
                        end
                    end
                    new_circular = detect_circular_deps!(new_dd, new_graph.serial_deps, new_wp, s.io, s.ext_to_parent)
                    s.n_total[] += length(new_dd) * length(request.configs)
                    new_tasks = spawn_precompile_tasks!(s, new_dd, new_wp, request.configs, new_circular,
                                                         req_pkgids, request.pkgs, request.pkgs, request._from_loading)
                    append!(s.injected_tasks, new_tasks)
                    s.n_batches[] += 1
                    waiter = Threads.@spawn :samepool begin
                        waitall(new_tasks; failfast=false, throw=false)
                        paths = collect(String, Iterators.flatten((v for (pkgid, v) in s.cachepath_cache if pkgid in req_pkgids)))
                        try; put!(request.result, paths); catch; end
                    end
                    push!(s.result_waiters, waiter)
                catch e
                    try; put!(request.result, e); catch; end
                end
            end
        catch
        end
    end
    return drainer
end

function report_precompile_results!(s::PrecompileSession)
    s.interrupted_or_done[] = true
    @lock Base.require_lock begin
        Base.LOADING_CACHE[] = nothing
    end
    notify(s.first_started) # in cases of no-op or !fancyprint

    quick_exit = any(t -> !istaskdone(t) || istaskfailed(t), s.tasks) || s.interrupted[]
    seconds_elapsed = round(Int, (s.time_start > 0 ? (time_ns() - s.time_start) : 0) / 1e9)
    ndeps = count(values(s.was_recompiled))

    requested_errs = false
    for ((dep, _), _) in s.failed_deps
        if dep in s.requested_pkgids
            requested_errs = true
            break
        end
    end
    if !s.strict && !requested_errs && !s.interrupted[]
        for (pkg_config, _) in s.failed_deps
            delete!(s.std_outputs, pkg_config)
        end
        empty!(s.failed_deps)
    end
    if ndeps > 0 || !isempty(s.failed_deps)
        if !quick_exit
            logstr = sprint(context=s.logio) do iostr
                if s.fancyprint
                    what = if s.n_batches[] > 1
                        "done."
                    elseif isempty(s.requested_pkgids)
                        "packages finished."
                    else
                        "$(join((full_name(s.ext_to_parent, p) for p in s.requested_pkgids), ", ", " and ")) finished."
                    end
                    printpkgstyle(iostr, :Precompiling, what)
                end
                plural = length(s.configs) > 1 ? "dependency configurations" : ndeps == 1 ? "dependency" : "dependencies"
                print(iostr, "  $(ndeps) $(plural) successfully precompiled in $(seconds_elapsed) seconds")
                if s.n_already_precomp[] > 0 || !isempty(s.circular_deps)
                    s.n_already_precomp[] > 0 && (print(iostr, ". $(s.n_already_precomp[]) already precompiled"))
                    !isempty(s.circular_deps) && (print(iostr, ". $(length(s.circular_deps)) skipped due to circular dependency"))
                    print(iostr, ".")
                end
                if s.n_loaded[] > 0
                    plural1 = length(s.configs) > 1 ? "dependency configurations" : s.n_loaded[] == 1 ? "dependency" : "dependencies"
                    plural2 = s.n_loaded[] == 1 ? "a different version is" : "different versions are"
                    plural3 = s.n_loaded[] == 1 ? "" : "s"
                    plural4 = s.n_loaded[] == 1 ? "this package" : "these packages"
                    print(iostr, "\n  ",
                        color_string(string(s.n_loaded[]), Base.warn_color(), s.hascolor),
                        " $(plural1) precompiled but ",
                        color_string("$(plural2) currently loaded", Base.warn_color(), s.hascolor),
                        ". Restart julia to access the new version$(plural3). \
                        Otherwise, loading dependents of $(plural4) may trigger further precompilation to work with the unexpected version$(plural3)."
                    )
                end
                if !isempty(s.precomperr_deps)
                    pluralpc = length(s.configs) > 1 ? "dependency configurations" : length(s.precomperr_deps) == 1 ? "dependency" : "dependencies"
                    print(iostr, "\n  ",
                        color_string(string(length(s.precomperr_deps)), Base.warn_color(), s.hascolor),
                        " $(pluralpc) failed but may be precompilable after restarting julia"
                    )
                end
            end
            @lock BG BG.result = logstr
            BG.monitoring && @lock s.print_lock begin
                println(s.logio, logstr)
            end
        elseif s.interrupted[]
            istr = sprint(context=s.logio) do iostr
                if s.fancyprint
                    printpkgstyle(iostr, :Precompiling, "interrupted.")
                end
                n_failed = length(s.failed_deps)
                print(iostr, "  $(ndeps) dependenc$(ndeps == 1 ? "y" : "ies") precompiled, ",
                      color_string("$(n_failed)", Base.error_color(), s.hascolor),
                      " interrupted after $(seconds_elapsed) seconds")
            end
            @lock BG BG.result = istr
            BG.monitoring && @lock s.print_lock begin
                println(s.logio, istr)
            end
        end
    end
    if !isempty(s.std_outputs)
        str = sprint(context=s.io) do iostr
            let std_outputs = Tuple{PkgConfig,SubString{String}}[(pkg_config, strip(String(take!(io)))) for (pkg_config,io) in s.std_outputs]
                filter!(!isempty∘last, std_outputs)
                if !isempty(std_outputs)
                    plural1 = length(std_outputs) == 1 ? "y" : "ies"
                    plural2 = length(std_outputs) == 1 ? "" : "s"
                    print(iostr, "\n  ", color_string("$(length(std_outputs))", Base.warn_color(), s.hascolor), " dependenc$(plural1) had output during precompilation:")
                    for (pkg_config, err) in std_outputs
                        pkg, config = pkg_config
                        err = if pkg == s.pkg_liveprinted[]
                            "[Output was shown above]"
                        else
                            join(split(err, "\n"), color_string("\n│  ", Base.warn_color(), s.hascolor))
                        end
                        name = full_name(s.ext_to_parent, pkg)
                        print(iostr, color_string("\n┌ ", Base.warn_color(), s.hascolor), name, color_string("\n│  ", Base.warn_color(), s.hascolor), err, color_string("\n└  ", Base.warn_color(), s.hascolor))
                    end
                end
            end
        end
        isempty(str) || BG.monitoring && @lock s.print_lock begin
            println(s.io, str)
        end
    end
    if !isempty(s.failed_deps)
        err_str = IOBuffer()
        for ((dep, config), err) in s.failed_deps
            write(err_str, "\n")
            print(err_str, "\n", full_name(s.ext_to_parent, dep), " ")
            join(err_str, config[1], " ")
            print(err_str, "\n", err)
        end
        n_errs = length(s.failed_deps)
        pluraled = n_errs == 1 ? "" : "s"
        err_msg = "The following $n_errs package$(pluraled) failed to precompile:$(String(take!(err_str)))\n"
        if s.internal_call
            print(s.io, err_msg)
        else
            throw(PkgPrecompileError(err_msg))
        end
    end
    if s.interrupted[]
        throw(InterruptException())
    end
    return collect(String, Iterators.flatten((v for (pkgid, v) in s.cachepath_cache if pkgid in s.requested_pkgids)))
end

# The actual precompilation implementation (mode-agnostic)
function do_precompile(pkgs::Union{Vector{String}, Vector{PkgId}},
                        internal_call::Bool,
                        strict::Bool,
                        warn_loaded::Bool,
                        timing::Bool,
                        _from_loading::Bool,
                        configs::Vector{Config},
                        io::IOContext,
                        fancyprint′::Bool,
                        manifest::Bool,
                        ignore_loaded::Bool,
                        detachable::Bool,
                        work_channel::Channel{PrecompileRequest})
    requested_pkgs = copy(pkgs)
    pkg_names = pkgs isa Vector{String} ? copy(pkgs) : String[pkg.name for pkg in pkgs]
    if pkgs isa Vector{PkgId}
        requested_pkgids = copy(pkgs)
    else
        requested_pkgids = PkgId[]
        for name in pkgs
            pkgid = Base.identify_package(name)
            if pkgid === nothing
                _from_loading && return
                throw(PkgPrecompileError("Unknown package: $name"))
            end
            push!(requested_pkgids, pkgid)
        end
    end

    time_start = time_ns()
    env = ExplicitEnv()

    # Windows sometimes hits a ReadOnlyMemoryError, so we halve the default number of tasks. Issue #2323
    # TODO: Investigate why this happens in windows and restore the full task limit
    default_num_tasks = Sys.iswindows() ? div(Sys.EFFECTIVE_CPU_THREADS::Int, 2) + 1 : Sys.EFFECTIVE_CPU_THREADS::Int + 1
    default_num_tasks = min(default_num_tasks, 16) # limit for better stability on shared resource systems
    num_tasks = max(1, something(tryparse(Int, get(ENV, "JULIA_NUM_PRECOMPILE_TASKS", string(default_num_tasks))), 1))

    # Suppress precompilation progress messages when precompiling for loading packages, except during
    # interactive sessions, since the complicated IO can have disastrous consequences in the background (#59599)
    logio = io
    logcalls = nothing
    if _from_loading
        if isinteractive()
            logcalls = CoreLogging.Info
        else
            logio = IOContext{IO}(devnull)
            fancyprint′ = false
            logcalls = CoreLogging.Debug
        end
    end
    fancyprint = fancyprint′
    hascolor = get(logio, :color, false)::Bool

    # Build dependency graph
    graph = build_dep_graph(env, _from_loading, requested_pkgids)

    # Return early if no deps
    if isempty(graph.direct_deps)
        isempty(pkgs) && return
        error("No direct dependencies outside of the sysimage found matching $(pkgs)")
    end

    # Initialize signalling
    was_processed = Dict{PkgConfig,Base.Event}()
    started = Dict{PkgConfig,Bool}()
    was_recompiled = Dict{PkgConfig,Bool}()
    for config in configs
        for pkgid in keys(graph.direct_deps)
            pkg_config = (pkgid, config)
            started[pkg_config] = 0.0
            was_processed[pkg_config] = Base.Event()
            was_recompiled[pkg_config] = false
        end
    end

    circular_deps = detect_circular_deps!(graph.direct_deps, graph.serial_deps, was_processed, io, graph.ext_to_parent)

    if filter_dep_graph!(graph.direct_deps, pkg_names, manifest, graph.project_deps, graph.ext_to_parent, requested_pkgids)
        return
    end

    nconfigs = length(configs)
    target = Ref{Union{Nothing, String}}(nothing)
    if nconfigs == 1
        !isempty(only(configs)[1]) && (target[] = "for configuration $(join(only(configs)[1], " "))")
    else
        target[] = "for $nconfigs compilation configurations"
    end

    print_lock = io.io isa Base.LibuvStream ? io.io.lock::ReentrantLock : ReentrantLock()

    s = PrecompileSession(;
        configs, io, logio, logcalls, fancyprint, hascolor,
        warn_loaded, ignore_loaded, internal_call, strict, _from_loading,
        time_start, print_lock, parallel_limiter=Base.Semaphore(num_tasks),
        start_loaded_modules=keys(Base.loaded_modules), requested_pkgids,
        ext_to_parent=graph.ext_to_parent, parent_to_exts=graph.parent_to_exts,
        triggers=graph.triggers, project_deps=graph.project_deps,
        serial_deps=graph.serial_deps, circular_deps,
        n_total=Ref(length(graph.direct_deps) * nconfigs),
        printloop_should_exit=Ref{Bool}(!fancyprint), target,
        started, was_processed, was_recompiled,
    )

    # Start print loop
    t_print = spawn_print_loop!(s)

    if !_from_loading
        @lock Base.require_lock begin
            Base.LOADING_CACHE[] = Base.LoadingCache()
        end
    end
    @debug "precompile: starting precompilation loop" graph.direct_deps graph.project_deps

    # Initial pass
    initial_tasks = spawn_precompile_tasks!(s, graph.direct_deps, was_processed, configs, circular_deps,
                                             requested_pkgids, pkg_names, requested_pkgs, _from_loading)
    append!(s.tasks, initial_tasks)

    # Concurrent drainer for injected requests
    drainer = drain_work_channel!(s, work_channel)

    waitall(initial_tasks; failfast=false, throw=false)
    @lock BG close(work_channel)
    wait(drainer)
    append!(s.tasks, s.injected_tasks)
    waitall(s.injected_tasks; failfast=false, throw=false)
    waitall(s.result_waiters; failfast=false, throw=false)

    # Final output
    s.interrupted_or_done[] = true
    fancyprint && wait(t_print)
    return report_precompile_results!(s)
end

function precompilepkgs_monitor_std(pkg_config, pipe, single_requested_pkg::Bool,
    ext_to_parent, hascolor::Bool, std_outputs, taskwaiting, pkg_liveprinted, print_lock,
    io::IOContext, fancyprint::Bool, ansi_cleartoendofline::String)
    pkg, config = pkg_config
    liveprinting = false
    thistaskwaiting = false
    while !eof(pipe)
        str = readline(pipe, keep=true)
        if single_requested_pkg && (liveprinting || !isempty(str))
            BG.monitoring && @lock print_lock begin
                if !liveprinting
                    liveprinting = true
                    pkg_liveprinted[] = pkg
                end
                print(io, ansi_cleartoendofline, str)
            end
        end
        write(get!(IOBuffer, std_outputs, pkg_config), str)
        if !thistaskwaiting && occursin("Waiting for background task / IO / timer", str)
            thistaskwaiting = true
            !liveprinting && !fancyprint && BG.monitoring && @lock print_lock begin
                println(io, full_name(ext_to_parent, pkg), color_string(str, Base.warn_color(), hascolor))
            end
            push!(taskwaiting, pkg_config)
        elseif !thistaskwaiting
            # XXX: don't just re-enable IO for random packages without printing the context for them first
            !liveprinting && !fancyprint && BG.monitoring && @lock print_lock begin
                print(io, ansi_cleartoendofline, str)
            end
        end
    end
end

timing_string(t) = string(lpad(round(t * 1e3, digits = 1), 9), " ms")

function color_string(cstr::String, col::Union{Int64, Symbol}, hascolor)
    if hascolor
        enable_ansi  = get(Base.text_colors, col, Base.text_colors[:default])
        disable_ansi = get(Base.disable_text_style, col, Base.text_colors[:default])
        return string(enable_ansi, cstr, disable_ansi)
    else
        return cstr
    end
end

# Can be merged with `maybe_cachefile_lock` in loading?
# Wraps the precompilation function `f` with cachefile lock handling.
# Returns the result from `f()`, which can be:
#   - `nothing`: cache already existed
#   - `Tuple{String, Union{Nothing, String}}`: this process just compiled
#   - `Exception`: compilation failed
function precompile_pkgs_maybe_cachefile_lock(f, io::IO, print_lock::ReentrantLock, fancyprint::Bool, pkg_config, pkgspidlocked, hascolor, parallel_limiter::Base.Semaphore, fullname)
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
        !fancyprint && BG.monitoring && @lock print_lock begin
            println(io, "    ", fullname, color_string(" Being precompiled by $(pkgspidlocked[pkg_config])", Base.info_color(), hascolor))
        end
        Base.release(parallel_limiter) # release so other work can be done while waiting
        try
            # wait until the lock is available
            cachefile = @invokelatest Base.mkpidlock_hook(() -> begin
                    delete!(pkgspidlocked, pkg_config)
                    Base.acquire(f, parallel_limiter)
                end,
                pidfile; stale_age)
        finally
            Base.acquire(parallel_limiter) # re-acquire so the outer release is balanced
        end
    end
    return cachefile
end

end
