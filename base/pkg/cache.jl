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
            run(`rm -rf $cache`)
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
prefetch(pkg::String, url::String, sha1::String...) = prefetch(pkg, url, String[sha1...])

end # module
