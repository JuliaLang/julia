# This file is a part of Julia. License is MIT: https://julialang.org/license

# Saving and restoring sessions (see doc/src/devdocs/sessions.md).
# EXPERIMENTAL proof of concept.

"""
    save_session(path::AbstractString = <depot>/sessions/v#.#/<timestamp>.ji)

Save the session's state to a session overlay file that a new process can
restore with `julia --restore`: everything in `Main` (globals, functions,
types, submodules), the modules brought into scope with `using`, and every
method defined during the session, including methods added to `Base` or
package functions. The overlay references the sysimage and loaded package
images instead of copying them, so restore boots a normal `julia` and loads
the overlay the way a package image loads.

Saving is a terminal act: writing the image damages the live heap, so the
process exits once the file is written.

Not preserved: running tasks (task references are restored as a single
completed placeholder task), open files/sockets/timers, memory-mapped
arrays, raw pointers, new or rebound globals in `Base` or package modules,
and in-place mutations of package-owned objects. Native code is not saved,
so session-defined code recompiles on first use after restore.

!!! warning
    This is an experimental proof of concept. A session file can only be
    restored by the identical build of Julia, with the session's packages
    available at the same versions.
"""
function save_session(path::AbstractString = new_session_path())
    mkpath(dirname(path))
    # Project Main's state into a fresh module: runtime-created modules are
    # fully serializable as incremental worklist roots, unlike Main itself,
    # which lives in the sysimage.
    sess = Module(:var"#session#", false, false)
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
    Core.eval(sess, Expr(:const, GlobalRef(sess, :SESSION_STATE),
                         (bindings = state, usings = usings)))
    flush(stdout)
    flush(stderr)
    ccall(:jl_save_session_overlay, Cvoid, (Cstring, Any), path, sess)
    println(stderr, "session saved to ", repr(path))
    exit(0)
end

function new_session_path()
    isempty(DEPOT_PATH) &&
        error("DEPOT_PATH is empty; pass an explicit path to `save_session`")
    dir = joinpath(DEPOT_PATH[1], "sessions", "v$(VERSION.major).$(VERSION.minor)")
    return joinpath(dir, Libc.strftime("%Y-%m-%dT%H-%M-%S", time()) * ".ji")
end

# Newest session file across all depots, or nothing.
function newest_session_path()
    tail = joinpath("sessions", "v$(VERSION.major).$(VERSION.minor)")
    best = nothing
    best_mtime = -Inf
    for depot in DEPOT_PATH
        dir = joinpath(depot, tail)
        isdir(dir) || continue
        for file in readdir(dir; join=true)
            endswith(file, ".ji") || continue
            m = mtime(file)
            if m > best_mtime
                best = file
                best_mtime = m
            end
        end
    end
    return best
end

# `julia --restore` driver, run from `exec_options` after startup.jl: boot was
# a normal sysimage boot; load the overlay like a package image and replay the
# projected state into Main.
function restore_session(path::Union{AbstractString,Nothing} = nothing)
    if path === nothing
        path = newest_session_path()
        path === nothing &&
            error("no saved session images found in <depot>/sessions/v$(VERSION.major).$(VERSION.minor)")
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
    for path in st.usings
        try
            Core.eval(Main, Expr(:using, Expr(:., path...)))
        catch
            @warn "could not restore `using $(join(path, '.'))` in Main"
        end
    end
    nothing
end
