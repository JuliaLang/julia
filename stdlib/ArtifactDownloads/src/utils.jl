# This file is a part of Julia. License is MIT: https://julialang.org/license

# Small helpers the artifact code used to take from Pkg. They stay Pkg-free so that Pkg
# can build on this module rather than the other way round.

depots() = Base.DEPOT_PATH

function depots1()
    isempty(Base.DEPOT_PATH) && error("no depots provided")
    return Base.DEPOT_PATH[1]
end

logdir(depot::AbstractString = depots1()) = joinpath(depot, "logs")

"""
    pkg_server()

The package server artifacts are downloaded from before their listed URLs are tried,
taken from `JULIA_PKG_SERVER` and defaulting to `https://pkg.julialang.org`. Returns
`nothing` when the variable is set but empty, which disables package server downloads.
"""
function pkg_server()
    server = get(ENV, "JULIA_PKG_SERVER", "https://pkg.julialang.org")
    isempty(server) && return nothing
    startswith(server, r"\w+://") || (server = "https://$server")
    return rstrip(server, '/')
end

# Print through one io type so the download path is compiled once rather than once per
# stream kind (see JuliaLang/julia#52249).
function unstableio(@nospecialize(io::IO))
    _io = Base.inferencebarrier(io)
    return IOContext{IO}(
        _io,
        get(_io, :color, false) ? Base.ImmutableDict{Symbol, Any}(:color, true) : Base.ImmutableDict{Symbol, Any}()
    )
end
stderr_f() = unstableio(stderr)

usable_io(io) = (io isa Base.TTY) || (io isa IOContext{IO} && io.io isa Base.TTY)
can_fancyprint(io::IO) = usable_io(io) && (get(ENV, "CI", nothing) != "true")

# "Precompiling" is the longest of Pkg's status words; align with it
const pkgstyle_indent = textwidth(string(:Precompiling))

function printpkgstyle(io::IO, cmd::Symbol, text::String, ignore_indent::Bool = false; color = :green)
    indent = ignore_indent ? 0 : pkgstyle_indent
    return @lock io begin
        printstyled(io, lpad(string(cmd), indent), color = color, bold = true)
        println(io, " ", text)
    end
end

function set_readonly(path)
    for (root, dirs, files) in walkdir(path)
        for file in files
            filepath = joinpath(root, file)
            # `chmod` on a link would change the permissions of the target.  If
            # the link points to a file within the same root, it will be
            # chmod'ed anyway, but we don't want to make directories read-only.
            # It's better not to mess with the other cases (links to files
            # outside of the root, links to non-file/non-directories, etc...)
            islink(filepath) && continue
            fmode = filemode(filepath)
            @static if Sys.iswindows()
                if Sys.isexecutable(filepath)
                    fmode |= 0o111
                end
            end
            try
                chmod(filepath, fmode & (typemax(fmode) ⊻ 0o222))
            catch
            end
        end
    end
    return nothing
end
set_readonly(::Nothing) = nothing

function mv_temp_dir_retries(temp_dir::String, new_path::String; set_permissions::Bool = true)::Nothing
    # Sometimes a rename can fail because the temp_dir is locked by
    # anti-virus software scanning the new files.
    # In this case we want to sleep and try again.
    # I am using the list of error codes to retry from:
    # https://github.com/isaacs/node-graceful-fs/blob/234379906b7d2f4c9cfeb412d2516f42b0fb4953/polyfills.js#L87
    # Retry for up to about 60 seconds by retrying 20 times with exponential backoff.
    retry = 0
    max_num_retries = 20 # maybe this should be configurable?
    sleep_amount = 0.01 # seconds
    max_sleep_amount = 5.0 # seconds
    while true
        isdir(new_path) && return
        # This next step is like
        # `mv(temp_dir, new_path)`.
        # However, `mv` defaults to `cp` if `rename` returns an error.
        # `cp` is not atomic, so avoid the potential of calling it.
        err = ccall(:jl_fs_rename, Int32, (Cstring, Cstring), temp_dir, new_path)
        if err ≥ 0
            if set_permissions
                # rename worked
                new_path_mode = filemode(dirname(new_path))
                if Sys.iswindows()
                    # If this is Windows, ensure the directory mode is executable,
                    # as `filemode()` is incomplete.  Some day, that may not be the
                    # case, there exists a test that will fail if this is changes.
                    new_path_mode |= 0o111
                end
                chmod(new_path, new_path_mode)
                set_readonly(new_path)
            end
            return
        else
            # Ignore rename error if `new_path` exists.
            isdir(new_path) && return
            if retry < max_num_retries && err ∈ (Base.UV_EACCES, Base.UV_EPERM, Base.UV_EBUSY)
                sleep(sleep_amount)
                sleep_amount = min(sleep_amount * 2.0, max_sleep_amount)
                retry += 1
            else
                Base.uv_error("rename of $(repr(temp_dir)) to $(repr(new_path))", err)
            end
        end
    end
    return
end

# Write `data` as TOML to `path` through a temporary file in the same directory, so a
# reader never sees a partially written file.
function atomic_toml_write(path::String, data; kws...)
    dir = dirname(path)
    isempty(dir) && (dir = pwd())

    temp_path, temp_io = mktemp(dir)
    return try
        TOML.print(temp_io, data; kws...)
        close(temp_io)
        mv(temp_path, path; force = true)
    catch
        close(temp_io)
        rm(temp_path; force = true)
        rethrow()
    end
end

function create_cachedir_tag(cache_dir::AbstractString)
    return try
        tag_file = joinpath(cache_dir, "CACHEDIR.TAG")
        if !isfile(tag_file)
            write(tag_file, "Signature: 8a477f597d28d172789f06886806bc55\n# This file is a cache directory tag created by Julia Pkg.\n# See https://bford.info/cachedir/\n")
        end
    catch
        # Ignore errors to avoid failing operations on read-only filesystems
    end
end

# Record that `source_file` (an `Artifacts.toml`) was used, in the depot's usage log that
# `Pkg.gc()` reads to decide which artifacts are still referenced.
write_env_usage(source_file::AbstractString, usage_filepath::AbstractString) =
    write_env_usage([source_file], usage_filepath)

function write_env_usage(source_files, usage_filepath::AbstractString)
    # Don't record ghost usage
    source_files = filter(isfile, source_files)
    isempty(source_files) && return

    # Ensure that log dir exists
    !ispath(logdir()) && mkpath(logdir())

    usage_file = joinpath(logdir(), usage_filepath)
    timestamp = now()

    ## Atomically write usage file using process id locking
    FileWatching.mkpidlock(usage_file * ".pid", stale_age = 3) do
        usage = if isfile(usage_file)
            try
                TOML.parsefile(usage_file)
            catch err
                @warn "Failed to parse usage file `$usage_file`, ignoring." err
                Dict{String, Any}()
            end
        else
            Dict{String, Any}()
        end

        # record new usage
        for source_file in source_files
            usage[source_file] = [Dict("time" => timestamp)]
        end

        # keep only latest usage info
        for k in keys(usage)
            times = map(usage[k]) do d
                if haskey(d, "time")
                    Dates.DateTime(d["time"])
                else
                    # if there's no time entry because of a write failure be conservative and mark it as being used now
                    @debug "Usage file `$usage_filepath` has a missing `time` entry for `$k`. Marking as used `now()`"
                    Dates.now()
                end
            end
            usage[k] = [Dict("time" => maximum(times))]
        end

        try
            atomic_toml_write(usage_file, usage, sorted = true)
        catch err
            @error "Failed to write valid usage file `$usage_file`" exception = err
        end
    end
    return
end
