module Cache

import ..Git, ..Dir
using ..Types

path(pkg::String) = abspath(".cache", pkg)

function mkcachedir()
    cache = joinpath(realpath("."), ".cache")
    if isdir(cache)
        return
    end

    @windows_only mkdir(cache)
    @unix_only begin
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


function prefetch{S<:String}(pkg::String, url::String, sha1s::Vector{S})
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
prefetch(pkg::String, url::String, sha1::String...) = prefetch(pkg, url, String[sha1...])

end # module
