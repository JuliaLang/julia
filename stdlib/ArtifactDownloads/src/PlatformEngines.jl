# This file is a part of Julia. License is MIT: https://julialang.org/license

# Content in this file is extracted from BinaryProvider.jl, see LICENSE.method

module PlatformEngines

import TOML # parsefile, print
import Tar # several
import SHA # sha256
import Downloads # download
import ..Utils: pkg_server, depots1, can_fancyprint, stderr_f
import ..MiniProgressBars: MiniProgressBar, start_progress, end_progress, show_progress

import Base.BinaryPlatforms: HostPlatform, triplet
import p7zip_jll

export verify, unpack, package, download_verify_unpack

const EXE7Z_LOCK = ReentrantLock()
const EXE7Z = Ref{String}()

function exe7z()
    # If the JLL is available, use the wrapper function defined in there
    if p7zip_jll.is_available()
        return p7zip_jll.p7zip()
    end

    lock(EXE7Z_LOCK) do
        if !isassigned(EXE7Z)
            EXE7Z[] = find7z()
        end
        return Cmd([EXE7Z[]])
    end
end

function find7z()
    name = "7z"
    Sys.iswindows() && (name = "$name.exe")
    for dir in (joinpath("..", "libexec"), ".")
        path = normpath(Sys.BINDIR::String, dir, name)
        isfile(path) && return path
    end
    path = Sys.which(name)
    path !== nothing && return path
    error("7z binary not found")
end

is_secure_url(url::AbstractString) =
    occursin(r"^(https://|\w+://(127\.0\.0\.1|localhost)(:\d+)?($|/))"i, url)

function get_server_dir(
    url :: AbstractString,
    server :: Union{AbstractString, Nothing} = pkg_server(),
)
    server === nothing && return
    url == server || startswith(url, "$server/") || return
    m = match(r"^\w+://([^\\/]+)(?:$|/)", server)
    if m === nothing
        @warn "malformed Pkg server value" server
        return
    end
    isempty(Base.DEPOT_PATH) && return
    invalid_filename_chars = [':', '/', '<', '>', '"', '/', '\\', '|', '?', '*']
    dir = join(replace(c -> c in invalid_filename_chars ? '_' : c, collect(String(m[1]))))
    return joinpath(depots1(), "servers", dir)
end

const AUTH_ERROR_HANDLERS = Pair{Union{String, Regex},Any}[]

function handle_auth_error(url, err; verbose::Bool = false)
    handled, should_retry = false, false
    for (scheme, handler) in AUTH_ERROR_HANDLERS
        occursin(scheme, url) || continue
        handled, should_retry = handler(url, pkg_server(), err)
        handled && break
    end
    handled && should_retry && return get_auth_header(url; verbose = verbose)
    return nothing
end

"""
    register_auth_error_handler(urlscheme::Union{AbstractString, Regex}, f)

Registers `f` as the topmost handler for failures in package server authentication.

A handler is only invoked if `occursin(urlscheme, url)` is true (where `url` is the URL Pkg
is currently trying to download.)

`f` must be a function that takes three input arguments `(url, pkgserver, err)`, where `url` is the
URL currently being downloaded, `pkgserver = Pkg.pkg_server()` the current package server, and
`err` is one of `no-auth-file`, `insecure-connection`, `malformed-file`, `no-access-token`,
`no-refresh-key` or `insecure-refresh-url`.

The handler `f` needs to return a tuple of `Bool`s `(handled, should_retry)`. If `handled` is `false`,
the next handler in the stack will be called, otherwise handling terminates; `get_auth_header` is called again if `should_retry`
is `true`.

`register_auth_error_handler` returns a zero-arg function that can be called to deregister the handler.
"""
function register_auth_error_handler(urlscheme::Union{AbstractString, Regex}, @nospecialize(f))
    unique!(pushfirst!(AUTH_ERROR_HANDLERS, urlscheme => f))
    return () -> deregister_auth_error_handler(urlscheme, f)
end

"""
    deregister_auth_error_handler(urlscheme::Union{AbstractString, Regex}, f)

Removes `f` from the stack of authentication error handlers.
"""
function deregister_auth_error_handler(urlscheme::Union{String, Regex}, @nospecialize(f))
    filter!(handler -> !(handler.first == urlscheme && handler.second === f), AUTH_ERROR_HANDLERS)
    return nothing
end

function get_auth_header(url::AbstractString; verbose::Bool = false)
    server_dir = get_server_dir(url)
    server_dir === nothing && return
    auth_file = joinpath(server_dir, "auth.toml")
    isfile(auth_file) || return handle_auth_error(url, "no-auth-file"; verbose=verbose)
    # TODO: check for insecure auth file permissions
    if !is_secure_url(url)
        @warn "refusing to send auth info over insecure connection" url=url
        return handle_auth_error(url, "insecure-connection"; verbose=verbose)
    end
    # parse the auth file
    auth_info = try
        TOML.parsefile(auth_file)
    catch err
        @error "malformed auth file" file=auth_file err=err
        return handle_auth_error(url, "malformed-file"; verbose=verbose)
    end
    # check for an auth token
    if !haskey(auth_info, "access_token")
        @warn "auth file without access_token field" file=auth_file
        return handle_auth_error(url, "no-access-token"; verbose=verbose)
    end
    auth_token = auth_info["access_token"]::String
    auth_header = "Authorization" => "Bearer $auth_token"
    # handle token expiration and refresh
    expires_at = Inf
    if haskey(auth_info, "expires_at")
        expires_at = min(expires_at, Float64(auth_info["expires_at"])::Float64)
    end
    if haskey(auth_info, "expires_in")
        expires_at = min(expires_at, mtime(auth_file) + Float64(auth_info["expires_in"])::Float64)
    end
    # if token is good until ten minutes from now, use it
    time_now = time()
    if expires_at ≥ time_now + 10*60 # ten minutes
        return auth_header
    end
    if !haskey(auth_info, "refresh_url") || !haskey(auth_info, "refresh_token")
        if expires_at ≤ time_now
            @warn "expired auth without refresh keys" file=auth_file
        end
        # try it anyway since we can't refresh
        return something(handle_auth_error(url, "no-refresh-key"; verbose=verbose), auth_header)
    end
    refresh_url = auth_info["refresh_url"]::String
    if !is_secure_url(refresh_url)
        @warn "ignoring insecure auth refresh URL" url=refresh_url
        return something(handle_auth_error(url, "insecure-refresh-url"; verbose=verbose), auth_header)
    end
    verbose && @info "Refreshing expired auth token..." file=auth_file
    tmp = tempname()
    refresh_token = auth_info["refresh_token"]::String
    refresh_auth = "Authorization" => "Bearer $refresh_token"
    try download(refresh_url, tmp, auth_header=refresh_auth, verbose=verbose)
    catch err
        @warn "token refresh failure" file=auth_file url=refresh_url err=err
        rm(tmp, force=true)
        return handle_auth_error(url, "token-refresh-failed"; verbose=verbose)
    end
    auth_info = try TOML.parsefile(tmp)
    catch err
        @warn "discarding malformed auth file" url=refresh_url err=err
        rm(tmp, force=true)
        return something(handle_auth_error(url, "malformed-file"; verbose=verbose), auth_header)
    end
    if !haskey(auth_info, "access_token")
        if haskey(auth_info, "refresh_token")
            auth_info["refresh_token"] = "*"^64
        end
        @warn "discarding auth file without access token" auth=auth_info
        rm(tmp, force=true)
        return something(handle_auth_error(url, "no-access-token"; verbose=verbose), auth_header)
    end
    if haskey(auth_info, "expires_in")
        expires_in = auth_info["expires_in"]
        if expires_in isa Number
            expires_at = floor(time_now + Float64(expires_in)::Float64)
            # overwrite expires_at (avoids clock skew issues)
            auth_info["expires_at"] = expires_at
        end
    end
    let auth_info = auth_info
        open(tmp, write=true) do io
            TOML.print(io, auth_info, sorted=true)
        end
    end
    mv(tmp, auth_file, force=true)
    access_token = auth_info["access_token"]::String
    return "Authorization" => "Bearer $access_token"
end

# based on information in this post:
# https://github.community/t5/GitHub-Actions/Have-the-CI-environment-variable-set-by-default/m-p/32358/highlight/true#M1097
const CI_VARIABLES = [
    "APPVEYOR",
    "CI",
    "CI_SERVER",
    "CIRCLECI",
    "CONTINUOUS_INTEGRATION",
    "GITHUB_ACTIONS",
    "GITLAB_CI",
    "JULIA_CI",
    "JULIA_PKGEVAL",
    "JULIA_REGISTRYCI_AUTOMERGE",
    "TF_BUILD",
    "TRAVIS",
]

function get_metadata_headers(url::AbstractString)
    headers = Pair{String,String}[]
    server = pkg_server()
    server_dir = get_server_dir(url, server)
    server_dir === nothing && return headers
    push!(headers, "Julia-Pkg-Protocol" => "1.0")
    push!(headers, "Julia-Pkg-Server" => server)
    push!(headers, "Julia-Version" => string(VERSION))
    system = triplet(HostPlatform())
    push!(headers, "Julia-System" => system)
    ci_info = String[]
    for var in CI_VARIABLES
        val = get(ENV, var, nothing)
        state = val === nothing ? "n" :
            lowercase(val) in ("true", "t", "1", "yes", "y") ? "t" :
            lowercase(val) in ("false", "f", "0", "no", "n") ? "f" : "o"
        push!(ci_info, "$var=$state")
    end
    push!(headers, "Julia-CI-Variables" => join(ci_info, ';'))
    push!(headers, "Julia-Interactive" => string(isinteractive()))
    for (key, val) in ENV
        m = match(r"^JULIA_PKG_SERVER_([A-Z0-9_]+)$"i, key)
        m === nothing && continue
        val = strip(val)
        isempty(val) && continue
        words = split(m.captures[1], '_', keepempty=false)
        isempty(words) && continue
        hdr = "Julia-" * join(map(titlecase, words), '-')
        any(hdr == k for (k, v) in headers) && continue
        push!(headers, hdr => val)
    end
    return headers
end

function download(
    url::AbstractString,
    dest::AbstractString;
    verbose::Bool = false,
    headers::Vector{Pair{String,String}} = Pair{String,String}[],
    auth_header::Union{Pair{String,String}, Nothing} = nothing,
    io::IO=stderr_f(),
    progress::Union{Nothing,Function} = nothing, # (total, now) -> nothing
)
    if auth_header === nothing
        auth_header = get_auth_header(url, verbose=verbose)
    end
    if auth_header !== nothing
        push!(headers, auth_header)
    end
    for header in get_metadata_headers(url)
        push!(headers, header)
    end

    do_fancy = verbose && can_fancyprint(io)
    progress = if !isnothing(progress)
        progress
    elseif do_fancy
        bar = MiniProgressBar(header="Downloading", color=Base.info_color())
        start_progress(io, bar)
        let bar=bar
            (total, now) -> begin
                bar.max = total
                bar.current = now
                # Downloads.download attaches the progress indicator to the header request too
                # which is only ~100 bytes, and will report as 0 - 100% progress immediately
                # then dip down to 0 before the actual download starts. So we only show the
                # progress bar once the real download starts.
                total > 1000 && show_progress(io, bar)
            end
        end
    else
        nothing
    end
    try
        Downloads.download(url, dest; headers, progress)
    finally
        do_fancy && end_progress(io, bar)
    end
end

"""
    download_verify(
        url::AbstractString,
        hash::Union{AbstractString, Nothing},
        dest::AbstractString;
        verbose::Bool = false,
        force::Bool = false,
        quiet_download::Bool = false,
    )

Download file located at `url`, verify it matches the given `hash`, and throw
an error if anything goes wrong.  If `dest` already exists, just verify it. If
`force` is set to `true`, overwrite the given file if it exists but does not
match the given `hash`.

This method returns `true` if the file was downloaded successfully, `false`
if an existing file was removed due to the use of `force`, and throws an error
if `force` is not set and the already-existent file fails verification, or if
`force` is set, verification fails, and then verification fails again after
redownloading the file.

If `quiet_download` is set to `false`, this method will print to
stdout when downloading a new file.  If it is set to `true` (default, and `verbose` is
set to `false`) the downloading process will be completely silent.  If
`verbose` is set to `true`, messages about integrity verification will be
printed in addition to messages regarding downloading.
"""
function download_verify(
    url::AbstractString,
    hash::Union{AbstractString, Nothing},
    dest::AbstractString;
    verbose::Bool = false,
    force::Bool = false,
    quiet_download::Bool = false,
    progress::Union{Nothing,Function} = nothing, # (total, now) -> nothing
)
    # Whether the file existed in the first place
    file_existed = false

    if isfile(dest)
        file_existed = true
        if verbose
            @info("Destination file $(dest) already exists, verifying...")
        end

        # verify download, if it passes, return happy.  If it fails, (and
        # `force` is `true`, re-download!)
        if hash !== nothing && verify(dest, hash; verbose=verbose)
            return true
        elseif !force
            error("Verification failed, not overwriting $(dest)")
        end
    end

    # Make sure the containing folder exists
    mkpath(dirname(dest))

    # Download the file, optionally continuing
    attempts = 3
    for i in 1:attempts
        try
            download(url, dest; verbose=verbose || !quiet_download, progress)
            break
        catch err
            @debug "download and verify failed on attempt $i/$attempts" url dest err
            # for system errors like `no space left on device` exit after first try
            if err isa SystemError || i == attempts
                rethrow()
            else
                sleep(1)
            end
        end
    end
    details = String[]
    if hash !== nothing && !verify(dest, hash; verbose, details)
        # If the file already existed, it's possible the initially downloaded chunk
        # was bad.  If verification fails after downloading, auto-delete the file
        # and start over from scratch.
        if file_existed
            if verbose
                @info("Continued download didn't work, restarting from scratch")
            end
            Base.rm(dest; force=true)

            # Download and verify from scratch
            download(url, dest; verbose=verbose || !quiet_download)
            if hash !== nothing && !verify(dest, hash; verbose, details)
                @goto verification_failed
            end
        else
            @label verification_failed
            # If it didn't verify properly and we didn't resume, something is
            # very wrong and we must complain mightily.
            details_indented = join(map(s -> "      $s", split(join(details, "\n"), '\n')), "\n")
            error("Verification failed:\n" * details_indented)
        end
    end

    # If the file previously existed, this means we removed it (due to `force`)
    # and redownloaded, so return `false`.  If it didn't exist, then this means
    # that we successfully downloaded it, so return `true`.
    return !file_existed
end

# TODO: can probably delete this, only affects tests
function copy_symlinks()
    var = get(ENV, "BINARYPROVIDER_COPYDEREF", "")
    lowercase(var) in ("true", "t", "yes", "y", "1") ? true :
    lowercase(var) in ("false", "f", "no", "n", "0") ? false : nothing
end

function unpack(
    tarball_path::AbstractString,
    dest::AbstractString;
    verbose::Bool = false,
)
    Tar.extract(`$(exe7z()) x $tarball_path -so`, dest, copy_symlinks = copy_symlinks())
end

"""
    package(src_dir::AbstractString, tarball_path::AbstractString)

Compress `src_dir` into a tarball located at `tarball_path`.
"""
function package(src_dir::AbstractString, tarball_path::AbstractString; io=stderr_f())
    rm(tarball_path, force=true)
    cmd = `$(exe7z()) a -si -tgzip -mx9 $tarball_path`
    open(pipeline(cmd, stdout=devnull, stderr=io), write=true) do io
        Tar.create(src_dir, io)
    end
end

"""
    download_verify_unpack(
        url::AbstractString,
        hash::Union{AbstractString, Nothing},
        dest::AbstractString;
        tarball_path = nothing,
        ignore_existence::Bool = false,
        force::Bool = false,
        verbose::Bool = false,
        quiet_download::Bool = false,
        io::IO=stderr,
    )

Helper method to download tarball located at `url`, verify it matches the
given `hash`, then unpack it into folder `dest`.  In general, the method
`install()` should be used to download and install tarballs into a `Prefix`;
this method should only be used if the extra functionality of `install()` is
undesired.

If `tarball_path` is specified, the given `url` will be downloaded to
`tarball_path`, and it will not be removed after downloading and verification
is complete.  If it is not specified, the tarball will be downloaded to a
temporary location, and removed after verification is complete.

If `force` is specified, a verification failure will cause `tarball_path` to be
deleted (if it exists), the `dest` folder to be removed (if it exists) and the
tarball to be redownloaded and reverified.  If the verification check is failed
a second time, an exception is raised.  If `force` is not specified, a
verification failure will result in an immediate raised exception.

If `ignore_existence` is set, the tarball is unpacked even if the destination
directory already exists.

Returns `true` if a tarball was actually unpacked, `false` if nothing was
changed in the destination prefix.
"""
function download_verify_unpack(
    url::AbstractString,
    hash::Union{AbstractString, Nothing},
    dest::AbstractString;
    tarball_path = nothing,
    ignore_existence::Bool = false,
    force::Bool = false,
    verbose::Bool = false,
    quiet_download::Bool = false,
    io::IO=stderr_f(),
    progress::Union{Nothing,Function} = nothing, # (total, now) -> nothing
)
    # First, determine whether we should keep this tarball around
    remove_tarball = false
    if tarball_path === nothing
        remove_tarball = true

        function url_ext(url)
            url = basename(url)

            # Chop off urlparams
            qidx = findfirst(isequal('?'), url)
            if qidx !== nothing
                url = url[1:qidx]
            end

            # Try to detect extension
            dot_idx = findlast(isequal('.'), url)
            if dot_idx === nothing
                return nothing
            end

            return url[dot_idx+1:end]
        end

        # If extension of url contains a recognized extension, use it, otherwise use ".gz"
        ext = url_ext(url)
        if !(ext in ["tar", "gz", "tgz", "bz2", "xz"])
            ext = "gz"
        end

        # Work around windows limitations regarding tempname()
        tarball_path = "$(tempname())-download.$(ext)"
        tries = 0
        while isfile(tarball_path) && tries < 100
            tarball_path = "$(tempname())-download.$(ext)"
            tries += 1
        end
        if tries >= 100
            error("Unable to generate unused tempname! Clean up your temporary folder $(dirname(tempname())) and try again.")
        end
    end

    # Download the tarball; if it already existed and we needed to remove it
    # then we should remove the unpacked path as well
    should_delete = !download_verify(url, hash, tarball_path; force, verbose, quiet_download, progress)
    if should_delete
        if verbose
            @info("Removing dest directory $(dest) as source tarball changed")
        end
        Base.rm(dest; recursive=true, force=true)
    end

    # If the destination path already exists, don't bother to unpack
    if !ignore_existence && isdir(dest)
        if verbose
            @info("Destination directory $(dest) already exists, returning")
        end

        # Signify that we didn't do any unpacking
        return false
    end

    try
        if verbose
            @info("Unpacking $(tarball_path) into $(dest)...")
        end
        isnothing(progress) || progress(10000, 10000; status="unpacking")
        open(`$(exe7z()) x $tarball_path -so`) do io
            Tar.extract(io, dest, copy_symlinks = copy_symlinks())
        end
    finally
        if remove_tarball
            Base.rm(tarball_path)
            # Remove cached tarball hash, if it exists.
            Base.rm(string(tarball_path, ".sha256"); force=true)
        end
    end

    # Signify that we did some unpacking!
    return true
end


"""
    verify(path::AbstractString, hash::AbstractString;
           verbose::Bool = false, report_cache_status::Bool = false,
           details::Union{Vector{String},Nothing} = nothing)

Given a file `path` and a `hash`, calculate the SHA256 of the file and compare
it to `hash`.  This method caches verification results in a `"\$(path).sha256"`
file to accelerate reverification of files that have been previously verified.
If no `".sha256"` file exists, a full verification will be done and the file
will be created, with the calculated hash being stored within the `".sha256"`
file.  If a `".sha256"` file does exist, its contents are checked to ensure
that the hash contained within matches the given `hash` parameter, and its
modification time shows that the file located at `path` has not been modified
since the last verification.

If `report_cache_status` is set to `true`, then the return value will be a
`Symbol` giving a granular status report on the state of the hash cache, in
addition to the `true`/`false` signifying whether verification completed
successfully.

If `details` is provided, any pertinent detail will be pushed to it rather than logged.
"""
function verify(path::AbstractString, hash::AbstractString; verbose::Bool = false,
                report_cache_status::Bool = false, hash_path::AbstractString="$(path).sha256",
                details::Union{Vector{String},Nothing} = nothing)

    # Check hash string format
    if !occursin(r"^[0-9a-f]{64}$"i, hash)
        msg = "Hash value must be 64 hexadecimal characters (256 bits), "
        if !isascii(hash)
            msg *= "given hash value is non-ASCII"
        elseif occursin(r"^[0-9a-f]*$"i, hash)
            msg *= "given hash value has the wrong length ($(length(hash)))"
        else
            msg *= "given hash value contains non-hexadecimal characters"
        end
        msg *= ": $(repr(hash))"
        error(msg)
    end
    hash = lowercase(hash)

    # Check to see if the hash cache is consistent
    status = :hash_consistent

    # First, it must exist
    if isfile(hash_path)
        # Next, it must contain the same hash as what we're verifying against
        if read(hash_path, String) == hash
            # Next, it must be no older than the actual path
            if stat(hash_path).mtime >= stat(path).mtime
                # If all of that is true, then we're good!
                if verbose
                    @info("Hash cache is consistent, returning true")
                end
                status = :hash_cache_consistent

                # If we're reporting our status, then report it!
                if report_cache_status
                    return true, status
                else
                    return true
                end
            else
                if verbose
                    @info("File has been modified, hash cache invalidated")
                end
                status = :file_modified
            end
        else
            if verbose
                @info("Verification hash mismatch, hash cache invalidated")
            end
            status = :hash_cache_mismatch
        end
    else
        if verbose
            @info("No hash cache found")
        end
        status = :hash_cache_missing
    end

    calc_hash = open(path) do file
        bytes2hex(SHA.sha256(file))
    end
    @assert occursin(r"^[0-9a-f]{64}$", calc_hash)

    if verbose
        @info("Calculated hash $calc_hash for file $path")
    end

    if calc_hash != hash
        msg  = "Hash Mismatch!\n"
        msg *= "  Expected sha256:   $hash\n"
        msg *= "  Calculated sha256: $calc_hash"
        if isnothing(details)
            @error(msg)
        else
            push!(details, msg)
        end
        if report_cache_status
            return false, :hash_mismatch
        else
            return false
        end
    end

    # Try to save a hash cache if everything worked out fine
    try
        open(hash_path, "w") do file
            write(file, hash)
        end
    catch e
        if isa(e, InterruptException)
            rethrow(e)
        end

        if verbose
            @warn("Unable to create hash cache file $(hash_path)")
        end
    end

    if report_cache_status
        return true, status
    else
        return true
    end
end

# Verify the git-tree-sha1 hash of a compressed archive.
function verify_archive_tree_hash(tar_gz::AbstractString, expected_hash::Base.SHA1)
    # This can fail because unlike sha256 verification of the downloaded
    # tarball, tree hash verification requires that the file can i) be
    # decompressed and ii) is a proper archive.
    calc_hash = try
        Base.SHA1(open(Tar.tree_hash, `$(exe7z()) x $tar_gz -so`))
    catch err
        @warn "unable to decompress and read archive" exception=err
        return false
    end
    if calc_hash != expected_hash
        @warn "tarball content does not match expected git-tree-sha1"
        return false
    end
    return true
end

end # module PlatformEngines
