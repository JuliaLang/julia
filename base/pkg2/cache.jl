module Cache

using ..Types, Base.Git

path(pkg::String) = abspath(".cache", pkg)
origin(pkg::String) = Git.readchomp(`config remote.origin.url`, dir=path(pkg))

function prefetch(pkg::String, url::String, vers::Dict{String,VersionNumber})
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
    # ensure that desired versions exist
    sha1s = collect(keys(vers))
    all(sha1->Git.iscommit(sha1, dir=cache), sha1s) && return
    Git.run(`fetch -q $url`, dir=cache)
    all(sha1->Git.iscommit(sha1, dir=cache), sha1s) && return
    unfound = filter(sha1->!Git.iscommit(sha1, dir=cache), sha1s)
    msg = "unfound versions of $pkg (possible metadata misconfiguration):"
    for sha1 in sha1s
        ver = vers[sha1]
        msg *= "  $(ver[sha1]) [$sha1[1:10]]\n"
    end
    error(msg)
end
prefetch(pkg::String, url::String, sha1::String, ver::VersionNumber) =
    prefetch(pkg, url, (String=>VersionNumber)[sha1=>ver])

function prefetch(pkg::String, url::String, avail::Dict{VersionNumber,Available})
    vers = Dict{String,VersionNumber}()
    for (v,a) in avail
        vers[a.sha1] = v
    end
    prefetch(pkg, url, vers)
end

end # module
