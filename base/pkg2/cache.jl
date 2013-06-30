module Cache

using Base.Git, ..Types, ..Read

path(pkg::String) = abspath(".cache", pkg)

function prefetch{S<:String}(pkg::String, sha1s::Vector{S})
	url = Read.url(pkg)
    isdir(".cache") || mkdir(".cache")
    cache = path(pkg)
    if !isdir(cache)
        info("Cloning $pkg from $url")
        try Git.run(`clone -q --mirror $url $cache`)
        catch
            run(`rm -rf $cache`)
            rethrow()
        end
    else
        Git.run(`config remote.origin.url $url`, dir=cache)
    end
    if !all(sha1->Git.iscommit(sha1, dir=cache), sha1s)
	    Git.run(`fetch -q $url`, dir=cache)
	end
    filter(sha1->!Git.iscommit(sha1, dir=cache), sha1s)
end
function prefetch(pkg::String, vers::Vector{VersionNumber})
	prefetch(pkg, map(ver->Read.sha1(pkg,ver), vers))
end
prefetch(pkg::String, sha1::String) = prefetch(pkg,[sha1])
prefetch(pkg::String, ver::VersionNumber) = prefetch(pkg,[ver])

end # module
