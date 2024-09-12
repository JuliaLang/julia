module Utils

import FileWatching, TOML, Dates

struct ArtifactsError <: Exception
    msg::String
end
artifactserror(msg::String...) = throw(ArtifactsError(join(msg)))
Base.showerror(io::IO, err::ArtifactsError) = print(io, err.msg)

function pkg_server()
    server = get(ENV, "JULIA_PKG_SERVER", "https://pkg.julialang.org")
    isempty(server) && return nothing
    startswith(server, r"\w+://") || (server = "https://$server")
    return rstrip(server, '/')
end

can_fancyprint(io::IO) = ((io isa Base.TTY) || (io isa IOContext{IO} && io.io isa Base.TTY)) && (get(ENV, "CI", nothing) != "true")

function printpkgstyle(io::IO, cmd::Symbol, text::String, ignore_indent::Bool=false; color=:green)
    indent = textwidth(string(:Precompiling)) # "Precompiling" is the longest operation
    ignore_indent && (indent = 0)
    printstyled(io, lpad(string(cmd), indent), color=color, bold=true)
    println(io, " ", text)
end

# For globally overriding in e.g. tests
const DEFAULT_IO = Ref{Union{IO,Nothing}}(nothing)

# See discussion in https://github.com/JuliaLang/julia/pull/52249
function unstableio(@nospecialize(io::IO))
    # Needed to prevent specialization https://github.com/JuliaLang/julia/pull/52249#discussion_r1401199265
    _io = Base.inferencebarrier(io)
    IOContext{IO}(
        _io,
        get(_io,:color,false) ? Base.ImmutableDict{Symbol,Any}(:color, true) : Base.ImmutableDict{Symbol,Any}()
    )
end
stderr_f() = something(DEFAULT_IO[], unstableio(stderr))
stdout_f() = something(DEFAULT_IO[], unstableio(stdout))

function write_env_usage(source_file::AbstractString, usage_filepath::AbstractString)
    # Don't record ghost usage
    !isfile(source_file) && return

    # Ensure that log dir exists
    !ispath(logdir()) && mkpath(logdir())

    usage_file = joinpath(logdir(), usage_filepath)
    timestamp = now()

    ## Atomically write usage file using process id locking
    FileWatching.mkpidlock(usage_file * ".pid", stale_age = 3) do
        usage = if isfile(usage_file)
            TOML.parsefile(usage_file)
        else
            Dict{String, Any}()
        end

        # record new usage
        usage[source_file] = [Dict("time" => timestamp)]

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

        open(usage_file, "w") do io
            TOML.print(io, usage, sorted=true)
        end
    end
    return
end

depots() = Base.DEPOT_PATH
function depots1()
    d = depots()
    isempty(d) && artifactserror("no depots found in DEPOT_PATH")
    return d[1]
end

logdir(depot = depots1()) = joinpath(depot, "logs")

function can_symlink(dir::AbstractString)
    # guaranteed to be an empty directory
    link_path = joinpath(dir, "link")
    return try
        symlink("target", link_path)
        true
    catch err
        err isa Base.IOError || rethrow()
        false
    finally
        rm(link_path, force=true)
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

"""
    contains_files(root::AbstractString)

Helper function to determine whether a directory contains files; e.g. it is a
direct parent of a file or it contains some other directory that itself is a
direct parent of a file. This is used to exclude directories from tree hashing.
"""
function contains_files(path::AbstractString)
    st = lstat(path)
    ispath(st) || throw(ArgumentError("non-existent path: $(repr(path))"))
    isdir(st) || return true
    for p in readdir(path)
        contains_files(joinpath(path, p)) && return true
    end
    return false
end

# See loading.jl
const TOML_CACHE = Base.TOMLCache(Base.TOML.Parser{Dates}())
const TOML_LOCK = ReentrantLock()
# Some functions mutate the returning Dict so return a copy of the cached value here
parse_toml(toml_file::AbstractString) =
    Base.invokelatest(deepcopy_toml, Base.parsed_toml(toml_file, TOML_CACHE, TOML_LOCK))::Dict{String, Any}

end
