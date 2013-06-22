module Cache

using Base.Git

path(pkg::String) = abspath(".cache", pkg)
origin(pkg::String) = Git.readchomp(`config remote.origin.url`, dir=path(pkg))

function prefetch(pkg::String, url::String, vers::Dict{String,VersionNumber})
    cache = path(pkg)
    isdir(".cache") || mkpath(".cache")
    if !isdir(cache)
        from = ispath(pkg,".git") ? abspath(pkg) : url
        info("Cloning $pkg from $from...")
        try Git.run(`clone --bare $from $cache`)
        catch
            run(`rm -rf $cache`)
            rethrow()
        end
    elseif ispath(pkg,".git")
        Git.run(`fetch -q $pkg`, dir=cache)
    end
    Git.run(`config remote.origin.url $url`, dir=cache)
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

end # module
