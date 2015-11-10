# This file is a part of Julia. License is MIT: http://julialang.org/license

module Cache

import ..Git, ..Dir
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
        try
            symlink(rootcache, cache)
            return
        end
    end
    mkdir(cache)
end


function prefetch(pkg::AbstractString, url::AbstractString, sha1s::Vector)
    isdir(".cache") || mkcachedir()
    cache = path(pkg)
    if !isdir(cache)
        info("Cloning cache of $pkg from $url")
        try Git.run(`clone -q --mirror $url $cache`)
        catch
            rm(cache, recursive=true)
            rethrow()
        end
    end
    Git.set_remote_url(url, dir=cache)
    in_cache = Git.iscommit(sha1s, dir=cache)
    if !all(in_cache)
        info("Updating cache of $pkg...")
        Git.success(`remote update`, dir=cache) ||
        error("couldn't update $cache using `git remote update`")
        in_cache = Git.iscommit(sha1s, dir=cache)
    end
    sha1s[!in_cache]
end
prefetch(pkg::AbstractString, url::AbstractString, sha1::AbstractString...) = prefetch(pkg, url, AbstractString[sha1...])

end # module
