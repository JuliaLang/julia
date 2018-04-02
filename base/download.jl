# This file is a part of Julia. License is MIT: https://julialang.org/license

##############################################################################
# file downloading

downloadcmd = nothing
if Sys.iswindows()
    downloadcmd = :powershell
    function download(url::AbstractString, filename::AbstractString; sha=nothing)
        ps = "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"
        tls12 = "[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12"
        client = "New-Object System.Net.Webclient"
        # in the following we escape ' with '' (see https://ss64.com/ps/syntax-esc.html)
        downloadfile = "($client).DownloadFile('$(replace(url, "'" => "''"))', '$(replace(filename, "'" => "''"))')"
        run(`$ps -NoProfile -Command "$tls12; $downloadfile"`)
        shacheck(filename, sha)
        filename
    end
else
    function download(url::AbstractString, filename::AbstractString; sha=nothing)
        global downloadcmd
        if downloadcmd === nothing
            for checkcmd in (:curl, :wget, :fetch)
                if success(pipeline(`which $checkcmd`, devnull))
                    downloadcmd = checkcmd
                    break
                end
            end
        end
        if downloadcmd == :wget
            try
                run(`wget -O $filename $url`)
            catch
                rm(filename)  # wget always creates a file
                rethrow()
            end
        elseif downloadcmd == :curl
            run(`curl -g -L -f -o $filename $url`)
        elseif downloadcmd == :fetch
            run(`fetch -f $filename $url`)
        else
            error("no download agent available; install curl, wget, or fetch")
        end
        shacheck(filename, sha)
        filename
    end
end
function download(url::AbstractString; sha=nothing)
    filename = tempname()
    download(url, filename, sha=sha)
end

"""
    download(url::AbstractString, [localfile::AbstractString]; sha=nothing)

Download a file from the given url, optionally renaming it to the given local file name.

If the optional `sha` keyword is specified, it should be the expected SHA-256
hash of the downloaded file (a 64-character hexadecimal string). An error is
thrown if the downloaded file does not have this hash.

Note that this function relies on the availability of external tools such as `curl`, `wget`
or `fetch` to download the file and is provided for convenience. For production use or
situations in which more options are needed, please use a package that provides the desired
functionality instead.
"""
download(url, filename)

##############################################################################
# SHA-256 hash validation, via mbedtls.

# If sha==nothing is supplied, then no check is performed.  However,
# if Base._downloadsecurity[] is set to a non-empty string, a
# deprecation message is emitted.  This is so that in certain
# security-sensitive contexts, future versions of Julia can disallow
# non-validated downloads.

const _downloadsecurity = Ref("") # non-empty in secure download contexts

function shacheck(filename::AbstractString, sha::Nothing)
    if !isempty(_downloadsecurity[])
        depwarn("Calling download without an sha=\"...\" argument for validation is deprecated in the security context: $(_downloadsecurity[])", shacheck)
    end
end

_updatehash!(ctx, data, nb) =
    ccall((:mbedtls_sha256_update,:libmbedtls), Cvoid, (Ptr{UInt8},Ptr{UInt8},Csize_t), ctx,data,nb%Csize_t)

# check that contents of filename match the given SHA-256 hash `sha`
function shacheck(filename::AbstractString, sha::AbstractString)
    (length(sha) == 64 && all(c -> c in '0':'9' || lowercase(c) in 'a':'f', sha)) ||
        throw(ArgumentError("invalid SHA-256 hash $sha"))
    ctx = Vector{UInt8}(undef, 10*sizeof(UInt32) + 64 + sizeof(Cint)) # mbedtls_sha256_context
    ccall((:mbedtls_sha256_init,:libmbedtls), Cvoid, (Ptr{UInt8},), ctx)
    try
        # todo: switch to mbedtls_sha256_starts_ret etcetera for mbedtls ≥ v2.7
        ccall((:mbedtls_sha256_starts,:libmbedtls), Cvoid, (Ptr{UInt8},Cint), ctx,0)
        open(filename, "r") do io
            nb = filesize(io)-position(io)
            buf = Vector{UInt8}(undef, min(nb, 32768))
            while !eof(io) && nb > 32768
                n = readbytes!(io, buf)
                _updatehash!(ctx, buf, n)
                nb -= n
            end
            _updatehash!(ctx, buf, readbytes!(io, buf, min(nb, length(buf))))
        end
        h = Vector{UInt8}(undef, 32)
        ccall((:mbedtls_sha256_finish,:libmbedtls), Cvoid, (Ptr{UInt8},Ptr{UInt8}), ctx,h)
        i = firstindex(sha)
        for hbyte in h
            j = nextind(sha, i)
            hbyte == parse(UInt8, sha[i], base=16)<<4 + parse(UInt8, sha[j], base=16) ||
                error("downloaded file ", filename, " had incorrect SHA256 hash")
            i = nextind(sha, j)
        end
    finally
        ccall((:mbedtls_sha256_free,:libmbedtls), Cvoid, (Ptr{UInt8},), ctx)
    end
end
