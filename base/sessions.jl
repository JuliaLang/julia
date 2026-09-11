# This file is a part of Julia. License is MIT: https://julialang.org/license

# Saving and restoring sessions (see doc/src/devdocs/sessions.md).
# EXPERIMENTAL proof of concept.

# Main-owned bindings and `using` scope, projected for saving.
function _session_state()
    state = Any[]
    for n in names(Main; all=true, imported=false, usings=false)
        isdefined(Main, n) || continue
        (n === :Main || n === :Base || n === :Core) && continue
        startswith(String(n), "#") && continue
        binding_module(Main, n) === Main || continue
        push!(state, (n, isconst(Main, n), getglobal(Main, n)))
    end
    usings = Any[]
    for m in ccall(:jl_module_usings, Any, (Any,), Main)::Vector{Any}
        (m === Base || m === Core || m === MainInclude) && continue
        push!(usings, fullname(m))
    end
    return state, usings
end

"""
    session_save_report() -> Vector{String}

Scan the state `save_session` would capture for objects that cannot survive a
restore (open handles, running tasks, raw pointers) and describe them. An
empty result means the session restores cleanly. isbits data is skipped
wholesale, so large numeric arrays cost nothing to scan.
"""
function session_save_report(state::Vector{Any} = _session_state()[1])
    counts = Dict{String,Int}()
    seen = IdSet{Any}()
    budget = Ref(2_000_000)
    for (_, _, val) in state
        _scan_session_value!(counts, seen, budget, val)
    end
    report = sort!(String[string(v, "x ", k) for (k, v) in counts])
    budget[] <= 0 && push!(report, "(scan truncated: session too large to check fully)")
    return report
end

function _scan_session_value!(counts::Dict{String,Int}, seen::IdSet{Any}, budget::Ref{Int},
                              @nospecialize(x))
    budget[] <= 0 && return
    bump(k) = (counts[k] = get(counts, k, 0) + 1; nothing)
    if x isa Ptr
        (UInt(x) == 0 || UInt(x) == typemax(UInt)) ||
            bump("raw pointer (Ptr): reset to NULL on save")
        return
    end
    T = typeof(x)
    (isbitstype(T) || x isa Symbol || x isa String || x isa Type || x isa Module) && return
    x in seen && return
    push!(seen, x)
    budget[] -= 1
    if x isa Task
        istaskdone(x) ||
            bump("running or waiting Task: restores as a completed placeholder")
        return
    elseif x isa IOStream
        isopen(x) && bump("open IOStream: dead after restore")
        return
    elseif x isa Timer
        isopen(x) && bump("active Timer: dead after restore")
        return
    elseif x isa Base.LibuvStream
        (x.status == StatusUninit || x.status == StatusClosed) ||
            bump("open stream or socket ($(nameof(T))): dead after restore")
        return
    elseif x isa Base.LibuvServer
        (x.status == StatusUninit || x.status == StatusClosed) ||
            bump("listening server ($(nameof(T))): dead after restore")
        return
    elseif x isa Base.Process
        process_running(x) && bump("running Process: dead after restore")
        return
    end
    if x isa Union{Array, GenericMemory}
        isbitstype(eltype(x)) && return
        for i in eachindex(x)
            @inbounds isassigned(x, i) && _scan_session_value!(counts, seen, budget, x[i])
        end
    else
        for i in 1:fieldcount(T)
            isdefined(x, i) && _scan_session_value!(counts, seen, budget, getfield(x, i))
        end
    end
    return
end

"""
    save_session(target::AbstractString = "")

Save the session's state to a session overlay file that a new process can
restore with `julia --restore`: everything in `Main` (globals, functions,
types, submodules), the modules brought into scope with `using`, and every
method defined during the session, including methods added to `Base` or
package functions. The overlay references the sysimage and loaded package
images instead of copying them, so restore boots a normal `julia` and loads
the overlay the way a package image loads.

`target` may be a path (ending in `.ji` or containing a path separator), a
session name (letters, digits, `_`, `.`, `-`), or empty for an anonymous
session. Named and anonymous sessions are stored under
`<depot>/sessions/v#.#/` with a timestamp; `julia --restore=<name>` finds the
newest session whose name matches exactly, by prefix, or as a substring.

State that cannot survive a restore (open handles, unfinished tasks, raw
pointers; see [`Base.session_save_report`](@ref)) is reported as a warning,
and the save proceeds. Finished tasks keep their results across a restore;
unfinished ones restore as a single completed placeholder. Saving is a
terminal act: writing the image damages the live heap, so the process exits
once the file is written.

!!! warning
    This is an experimental proof of concept. A session file can only be
    restored by the identical build of Julia, with the session's packages
    available at the same versions.
"""
function save_session(target::AbstractString = "")
    local path
    if isempty(target)
        path = new_session_path()
    elseif endswith(target, ".ji") || '/' in target || '\\' in target
        path = String(target)
    else
        occursin(r"^[A-Za-z0-9_][A-Za-z0-9_.-]*$", target) ||
            error("invalid session name ", repr(target),
                  " (use letters, digits, '_', '.', '-', or pass a path ending in .ji)")
        path = new_session_path(target)
    end
    state, usings = _session_state()
    issues = session_save_report(state)
    if !isempty(issues)
        @warn "This session contains state that will not survive a restore:\n" *
              join(("    " * m for m in issues), '\n')
    end
    mkpath(dirname(path))
    # Project Main's state into a fresh module: runtime-created modules are
    # fully serializable as incremental worklist roots, unlike Main itself,
    # which lives in the sysimage.
    sess = Module(:var"#session#", false, false)
    Core.eval(sess, Expr(:const, GlobalRef(sess, :SESSION_STATE),
                         (bindings = state, usings = usings)))
    flush(stdout)
    flush(stderr)
    ccall(:jl_save_session_overlay, Cvoid, (Cstring, Any), path, sess)
    println(stderr, "session saved to ", repr(path))
    exit(0)
end

function new_session_path(name::AbstractString = "")
    isempty(DEPOT_PATH) &&
        error("DEPOT_PATH is empty; pass an explicit path to `save_session`")
    dir = joinpath(DEPOT_PATH[1], "sessions", "v$(VERSION.major).$(VERSION.minor)")
    stamp = Libc.strftime("%Y-%m-%dT%H-%M-%S", time())
    return joinpath(dir, (isempty(name) ? stamp : stamp * "-" * name) * ".ji")
end

# Newest session file across all depots, or nothing. With a query, prefer an
# exact name match, then a name prefix, then a substring of the file name;
# newest wins within the best tier.
function newest_session_path(query::Union{AbstractString,Nothing} = nothing)
    tail = joinpath("sessions", "v$(VERSION.major).$(VERSION.minor)")
    best = nothing
    best_key = (-1, -Inf)
    for depot in DEPOT_PATH
        dir = joinpath(depot, tail)
        isdir(dir) || continue
        for file in readdir(dir; join=true)
            endswith(file, ".ji") || continue
            base = basename(file)[1:end-3]
            name = length(base) > 20 ? base[21:end] : "" # after "yyyy-mm-ddTHH-MM-SS-"
            tier = query === nothing ? 0 :
                   name == query ? 3 :
                   startswith(name, query) ? 2 :
                   occursin(query, base) ? 1 : -1
            tier < 0 && continue
            key = (tier, mtime(file))
            if key > best_key
                best = file
                best_key = key
            end
        end
    end
    return best
end

# Restored IOStreams carry a dead C-side buffer; replace top-level ones with
# a closed stream (fd -1), which behaves exactly like a stream the user
# closed, instead of crashing or reading from a hijacked descriptor.
function _sanitize_restored(@nospecialize(val))
    val isa IOStream &&
        return fdio("<dead stream from restored session: " * val.name * ">", -1, false)
    return val
end

# `julia --restore` driver, run from `exec_options` after startup.jl: boot was
# a normal sysimage boot; load the overlay like a package image and replay the
# projected state into Main.
function restore_session(target::Union{AbstractString,Nothing} = nothing)
    local path
    if target === nothing
        path = newest_session_path()
        path === nothing &&
            error("no saved session images found in <depot>/sessions/v$(VERSION.major).$(VERSION.minor)")
    elseif isfile(target)
        path = String(target)
    else
        path = newest_session_path(String(target))
        path === nothing &&
            error("no saved session matching ", repr(target),
                  " in <depot>/sessions/v$(VERSION.major).$(VERSION.minor)")
    end
    sv = @lock require_lock begin
        io = open(path, "r")
        local depmodnames
        try
            isvalid_cache_header(io) !== nothing ||
                error(repr(path), " is not a session file for this build of Julia")
            hdr = parse_cache_header(io, path)
            depmodnames = hdr[3]::Vector{Pair{PkgId,UInt128}}
            isempty(hdr[6]) ||
                error("session file unexpectedly contains native code targets")
            isvalid_file_crc(io) ||
                error("session file ", repr(path), " is corrupt")
        finally
            close(io)
        end
        # the overlay references loaded images by identity, so bring up the
        # same dependencies at the same build ids first
        depmods = Vector{Any}(undef, length(depmodnames))
        for i in eachindex(depmodnames)
            modkey, build_id = depmodnames[i]
            dep = _tryrequire_from_serialized(modkey, build_id)
            dep isa Module ||
                throw(ErrorException("failed to load session dependency $modkey:\n$dep"))
            depmods[i] = dep
        end
        # arbitrary code runs during restore (method insertion, invalidation),
        # so drop the lock around the ccall, as _include_from_serialized does
        unlock(require_lock)
        try
            ccall(:jl_restore_incremental, Any, (Cstring, Any, Cint, Cstring),
                  path, depmods, #=completeinfo=#false, "session")
        finally
            lock(require_lock)
        end
    end
    sv isa Exception && throw(sv)
    sv = sv::SimpleVector
    ReinferUtils.insert_backedges_typeinf(sv[3]::Vector{Any})
    restored = sv[1]::Vector{Any}
    sess = restored[end]::Module
    st = invokelatest(getglobal, sess, :SESSION_STATE)
    for (n, wasconst, val) in st.bindings
        val = _sanitize_restored(val)
        try
            if wasconst
                Core.eval(Main, Expr(:const, GlobalRef(Main, n), val))
            else
                Core.eval(Main, Expr(:global, n))
                invokelatest(setglobal!, Main, n, val)
            end
        catch err
            # e.g. startup.jl already declared the name with a conflicting kind
            @warn "could not restore binding `Main.$n`: $(sprint(showerror, err))"
        end
    end
    # after the bindings, so that `using` of modules defined in Main resolves
    for pathsyms in st.usings
        try
            Core.eval(Main, Expr(:using, Expr(:., pathsyms...)))
        catch
            @warn "could not restore `using $(join(pathsyms, '.'))` in Main"
        end
    end
    nothing
end
