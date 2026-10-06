# This file is a part of Julia. License is MIT: https://julialang.org/license

# A package's builds for one project and configuration share a slot, the name returned by
# `compilecache_path`. Each build of a package with a uuid gets its own file in that slot,
# `<slot>_<build id>.ji`, so no file is ever replaced. A session holds a shared lock on each
# cache file it loaded until it exits, and a process takes an exclusive lock on a cache file
# before removing it, so files in use stay.

const loaded_cachefile_locks = Dict{Tuple{UInt,UInt},Filesystem.File}() # guarded by require_lock

# per slot, a lock on the newest build that only suits this session, so other processes don't
# remove it before this session loads it
const session_build_locks = Dict{String,Filesystem.File}() # guarded by require_lock

# old builds of a slot to keep where file locks do not work, as another session may use them
const MAX_NUM_UNLOCKABLE_OLD_BUILDS = 2

function cachefile_with_build_id(slot_cachefile::String, path::String)
    build_id = parse_cache_buildid(path)[1] % UInt64
    return string(chopsuffix(slot_cachefile, ".ji"), "_", string(build_id; base=16, pad=16), ".ji")
end

# a package without a uuid has one file that is overwritten in place (an open file would block
# that on Windows), and nothing prunes the bundled depot once Julia is built
function cachefile_needs_lock(pkg::PkgId, path::String)
    pkg.uuid === nothing && return false
    bundled = joinpath(dirname(dirname(Sys.STDLIB)), "compiled") * Filesystem.path_separator
    return !startswith(path, bundled)
end

function open_and_trylock(path::String, flags::Integer, shared::Bool)
    f = try
        Filesystem.open(path, flags)
    catch e
        e isa IOError || rethrow()
        # a file that is gone was removed by another process
        return ispath(path) ? nothing : false
    end
    locked = try
        Filesystem.trylockfile(f; shared)
    catch e
        e isa SystemError || rethrow()
        nothing
    end
    locked === true || close(f)
    return locked === true ? f : locked
end

# returns the open file, `false` if it is being removed, or `nothing` if it cannot be opened or
# locked here
function lock_cachefile_shared(cachefile::String)
    f = open_and_trylock(cachefile, Filesystem.JL_O_RDONLY, true)
    f isa Filesystem.File || return f
    # the file may have been removed after we opened it
    if !samefile(stat(f), stat(cachefile))
        release_cachefile_lock(f)
        return false
    end
    return f
end

# returns the open file, `false` if a session may use it or it is gone, or `nothing` if it
# cannot be opened or locked here
function lock_cachefile_exclusive(cachefile::String)
    # on Linux NFS, an exclusive lock needs write access
    f = open_and_trylock(cachefile, Filesystem.JL_O_RDWR, false)
    f === nothing && (f = open_and_trylock(cachefile, Filesystem.JL_O_RDONLY, false))
    return f
end

# unlock before closing, since on Windows closing may release the lock late
function release_cachefile_lock(f::Filesystem.File)
    try
        Filesystem.unlockfile(f)
    catch e
        e isa SystemError || rethrow()
    end
    close(f)
end

cachefile_key(st::StatStruct) = (UInt(st.device), UInt(st.inode))

function keep_cachefile_lock(f)
    f isa Filesystem.File || return
    key = cachefile_key(stat(f))
    @lock require_lock haskey(loaded_cachefile_locks, key) ? release_cachefile_lock(f) : (loaded_cachefile_locks[key] = f)
    return
end

function keep_session_build_lock(slot_cachefile::String, f)
    @lock require_lock begin
        old = pop!(session_build_locks, slot_cachefile, nothing)
        old === nothing || release_cachefile_lock(old)
        f isa Filesystem.File && (session_build_locks[slot_cachefile] = f)
    end
    return
end

# Windows may release the locks of an exiting process late
function release_cachefile_locks()
    trylock(require_lock) || return # another thread may hold it while we exit
    try
        foreach(release_cachefile_lock, values(loaded_cachefile_locks))
        foreach(release_cachefile_lock, values(session_build_locks))
        empty!(loaded_cachefile_locks)
        empty!(session_build_locks)
    finally
        unlock(require_lock)
    end
end

# some locks (NFS) don't conflict within one process, so also check what this session holds
function is_loaded_cachefile(pkg::PkgId, cachefile::String)
    st = stat(cachefile)
    ispath(st) || return false
    @lock require_lock begin
        haskey(loaded_cachefile_locks, cachefile_key(st)) && return true
        any(f -> cachefile_key(stat(f)) == cachefile_key(st), values(session_build_locks)) && return true
        origin = get(pkgorigins, pkg, nothing)
        origin === nothing && return false
        path = origin.cachepath
        return path !== nothing && samefile(path, cachefile)
    end
end

function lock_unused_cachefile(pkg::PkgId, cachefile::String)
    is_loaded_cachefile(pkg, cachefile) && return false
    return lock_cachefile_exclusive(cachefile)
end

# Remove the slot's other builds that no session holds, then trim the directory to its size
# limit. A build that only suits this session keeps the slot's other builds, as the next
# session will want the environment's build. Where locks don't work, the size limit can
# still remove the older builds kept for other sessions.
function prune_cachefiles(pkg::PkgId, slot_cachefile::String, cachefile::String; session_only::Bool)
    dir = dirname(cachefile)
    _, entryfile = cache_file_entry(pkg)
    cachefiles = filter!(x -> startswith(x, entryfile * "_") && endswith(x, ".ji"), readdir(dir))
    slot = basename(slot_cachefile)
    slot_prefix = chopsuffix(slot, ".ji") * "_"
    if !session_only
        unknown = String[]
        nloaded = 0
        for x in cachefiles
            path = joinpath(dir, x)
            # also the bare slot name, used before builds were named by id
            (x == slot || startswith(x, slot_prefix)) && path != cachefile || continue
            is_loaded_cachefile(pkg, path) && (nloaded += 1; continue)
            guard = lock_cachefile_exclusive(path)
            if guard === nothing
                push!(unknown, path)
            elseif guard !== false
                evict_cachefile(path, guard)
            end
        end
        sort!(unknown; by=mtime, rev=true)
        nkeep = max(0, MAX_NUM_UNLOCKABLE_OLD_BUILDS - nloaded)
        foreach(evict_cachefile, @view unknown[nkeep+1:end])
    end
    filter!(x -> isfile(joinpath(dir, x)), cachefiles)
    excess = length(cachefiles) - MAX_NUM_PRECOMPILE_FILES[]
    excess > 0 || return
    sort!(cachefiles; by = x -> mtime(joinpath(dir, x)))
    for x in cachefiles
        excess > 0 || break
        path = joinpath(dir, x)
        path == cachefile && continue
        session_only && (x == slot || startswith(x, slot_prefix)) && continue
        guard = lock_unused_cachefile(pkg, path)
        guard === false && continue
        evict_cachefile(path, guard)
        excess -= 1
    end
end

function evict_cachefile(cachefile::String, guard::Union{Nothing,Filesystem.File}=nothing)
    @debug "Evicting file from cache" cachefile
    try
        # remove the .ji first, so a failure leaves nothing that loads; the next build retries
        rm(cachefile; force=true)
        rm(ocachefile_from_cachefile(cachefile); force=true)
        @static if Sys.isapple()
            rm(ocachefile_from_cachefile(cachefile) * ".dSYM"; force=true, recursive=true)
        end
    catch e
        e isa IOError || rethrow()
        @debug "Could not evict a cache file" cachefile exception=e
    finally
        guard === nothing || release_cachefile_lock(guard)
    end
end

# whether a build used a loaded package that a new session would not load, because the
# environment now finds it at another path or its source changed since it was built
function built_against_other_sources(required_modules::Vector{Pair{PkgId,UInt128}})
    @lock require_lock for (dep, build_id) in required_modules
        m = get(loaded_modules, dep, nothing)
        (m === nothing || module_build_id(m) != build_id) && continue
        origin = get(pkgorigins, dep, nothing)
        # the system image's copy is used whatever the environment says
        (origin === nothing || origin.path === nothing || origin.cachepath === nothing) && continue
        path = locate_package(dep)
        path === nothing || samefile(path, origin.path) || return true
        loaded_build_sources_changed(origin.cachepath) && return true
    end
    return false
end

function loaded_build_sources_changed(cachefile::String)
    includes = try
        parse_cache_header(cachefile)[2][1]
    catch e
        e isa InterruptException && rethrow()
        return true # can't tell, so keep the environment's build
    end
    return any_includes_stale(includes, cachefile)
end
