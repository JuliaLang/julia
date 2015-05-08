# This file is a part of Julia. License is MIT: http://julialang.org/license

module Cache

import ..LibGit2, ..Dir
using ..Types

path(pkg::AbstractString) = abspath(".cache", pkg)

function mkcachedir()
    cache = joinpath(realpath("."), ".cache")
    if isdir(cache)
        return
    end

    @windows_only if Base.windows_version() < Base.WINDOWS_VISTA_VER
        mkdir(cache)
        return
    end
    if Dir.isversioned(pwd())
        rootcache = joinpath(realpath(".."), ".cache")
        if !isdir(rootcache)
            mkdir(rootcache)
        end
        symlink(rootcache, cache)
        return
    end
    mkdir(cache)
end

function prefetch(pkg::AbstractString, url::AbstractString, sha1s::Vector)
    isdir(".cache") || mkcachedir()
    cache = path(pkg)
    if !isdir(cache)
        info("Cloning cache of $pkg from $url")
        repo = LibGit2.clone(url, path, bare = true, remote_cb = Pkg.LibGit2.mirror_cb)
        if repo == noting
            rm(cache, recursive=true)
            error("Cannot clone $pkg from $url")
        end
    else
        repo = LibGit2.GitRepo(cache)
    end
    LibGit2.set_remote_url(repo, url)
    if !all(sha1->LibGit2.iscommit(sha1, repo), sha1s)
        info("Updating cache of $pkg...")
        isa(LibGit2.fetch(repo), LibGit2.GitError) &&
            error("couldn't update $cache using `git remote update`")
    end
    filter(sha1->!LibGit2.iscommit(sha1, repo), sha1s)
end
prefetch(pkg::AbstractString, url::AbstractString, sha1::AbstractString...) = prefetch(pkg, url, AbstractString[sha1...])

end # module
