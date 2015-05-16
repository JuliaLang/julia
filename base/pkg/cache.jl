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
    repo = if isdir(cache)
        LibGit2.GitRepo(cache) # open repo, free it at the end
    else
        info("Cloning cache of $pkg from $url")
        try
            # clone repo, free it at the end
            LibGit2.clone(url, cache, bare = true, remote_cb = LibGit2.mirror_cb)
        catch err
            isdir(cache) && rm(cache, recursive=true)
            error("Cannot clone $pkg from $url\n", err)
        end
    end
    try
        LibGit2.set_remote_url(repo, url)
        if !all(sha1->LibGit2.iscommit(sha1, repo), sha1s)
            info("Updating cache of $pkg...")
            try
                LibGit2.fetch(repo)
            catch
                error("couldn't update $cache using `git remote update`")
            end
        end
        filter(sha1->!LibGit2.iscommit(sha1, repo), sha1s)
    finally
        LibGit2.finalize(repo) # closing repo opened/created above
    end
end
prefetch(pkg::AbstractString, url::AbstractString, sha1::AbstractString...) = prefetch(pkg, url, AbstractString[sha1...])

end # module
