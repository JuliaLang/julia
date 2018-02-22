# This file is a part of Julia. License is MIT: https://julialang.org/license

module Cache

import LibGit2
import ..Dir, ..PkgError
using ..Types

rewrite_url_to = "https"

const GITHUB_REGEX =
    r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

path(pkg::AbstractString) = abspath(".cache", pkg)

function mkcachedir()
    cache = joinpath(realpath("."), ".cache")
    if isdir(cache)
        return
    end

    @static if Sys.isunix()
        if Dir.isversioned(pwd())
            rootcache = joinpath(realpath(".."), ".cache")
            if !isdir(rootcache)
                mkdir(rootcache)
            end
            symlink(rootcache, cache)
            return
        end
    end
    mkdir(cache)
end

function prefetch(pkg::AbstractString, url::AbstractString, sha1s::Vector)
    isdir(".cache") || mkcachedir()

    cache = path(pkg)
    normalized_url = normalize_url(url)

    repo = if isdir(cache)
        LibGit2.GitRepo(cache) # open repo, free it at the end
    else
        @info "Cloning cache of $pkg from $normalized_url"
        try
            # clone repo, free it at the end
            LibGit2.clone(normalized_url, cache, isbare = true, remote_cb = LibGit2.mirror_cb())
        catch err
            errmsg = if isa(err, LibGit2.Error.GitError)
                "Cannot clone $pkg from $normalized_url. $(err.msg)"
            elseif isa(err, InterruptException)
                "Package `$pkg` prefetching was interrupted."
            else
                "Unknown error: $err"
            end
            isdir(cache) && rm(cache, recursive=true)
            throw(PkgError(errmsg))
        end
    end
    try
        LibGit2.set_remote_url(repo, "origin", normalized_url)
        in_cache = BitVector(map(sha1->LibGit2.iscommit(sha1, repo), sha1s))
        if !all(in_cache)
            @info "Updating cache of $pkg..."
            LibGit2.fetch(repo)
            in_cache = BitVector(map(sha1->LibGit2.iscommit(sha1, repo), sha1s))
        end
        sha1s[.!in_cache]
    finally
        close(repo) # closing repo opened/created above
    end
end
prefetch(pkg::AbstractString, url::AbstractString, sha1::AbstractString...) =
    prefetch(pkg, url, AbstractString[sha1...])

function setprotocol!(proto::AbstractString)
    global rewrite_url_to

    if length(proto) == 0
        rewrite_url_to = nothing
    else
        rewrite_url_to = proto
    end
end

function normalize_url(url::AbstractString)
    global rewrite_url_to

    m = match(GITHUB_REGEX,url)
    (m === nothing || rewrite_url_to === nothing) ?
        url : "$rewrite_url_to://github.com/$(m.captures[1]).git"
end

end # module
