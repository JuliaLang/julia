module Cache

import ..Git, ..Dir
using ..Types

path(pkg::AbstractString) = abspath(".cache", pkg)

function mkcachedir()
    cache = joinpath(realpath("."), ".cache")
    if isdir(cache)
        return
    end

    @windowsxp_only mkdir(cache)
    @non_windowsxp_only begin
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
end


function prefetch{S<:AbstractString}(pkg::AbstractString, url::AbstractString, sha1s::Vector{S})
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
    if !all(sha1->Git.iscommit(sha1, dir=cache), sha1s)
        info("Updating cache of $pkg...")
        Git.success(`remote update`, dir=cache) ||
        error("couldn't update $cache using `git remote update`")
    end
    filter(sha1->!Git.iscommit(sha1, dir=cache), sha1s)
end
prefetch(pkg::AbstractString, url::AbstractString, sha1::AbstractString...) = prefetch(pkg, url, AbstractString[sha1...])

end # module
