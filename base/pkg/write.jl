module Write

import ..Git, ..Cache, ..Read

function prefetch(pkg::String, sha1::String)
    isempty(Cache.prefetch(pkg, Read.url(pkg), sha1)) && return
    error("$pkg: couldn't find commit $(sha1[1:10])")
end

function fetch(pkg::String, sha1::String)
    refspec = "+refs/*:refs/remotes/cache/*"
    # Git.run(`fetch -q $(Cache.path(pkg)) $refspec`, dir=pkg)
    repo = Git.get_repo(pkg)
    # Add a new remote with the given refspec and fetch from it
    remote = if "cache" in Base.LibGit2.remote_names(repo)
        Base.LibGit2.lookup_remote(repo, "cache")
    else
        Base.LibGit2.remote_add!(repo, "cache", Cache.path(pkg), refspec)
    end
    Base.LibGit2.remote_fetch(remote)
    Git.iscommit(sha1, dir=pkg) && return
    f = Git.iscommit(sha1, dir=Cache.path(pkg)) ? "fetch" : "prefetch"
    error("$pkg: $f failed to get commit $(sha1[1:10]), please file a bug")
end

function checkout(pkg::String, sha1::String)
    Git.set_remote_url(Read.url(pkg), dir=pkg)
    Git.checkout(sha1, dir=pkg)
    # Git.run(`checkout -q $sha1`, dir=pkg)
end

function install(pkg::String, sha1::String)
    prefetch(pkg, sha1)
    if isdir(".trash/$pkg")
        mv(".trash/$pkg", "./$pkg")
    else
        # Git.run(`clone -q $(Cache.path(pkg)) $pkg`)
        Git.clone(Cache.path(pkg), pkg)
    end
    fetch(pkg, sha1)
    checkout(pkg, sha1)
end

function update(pkg::String, sha1::String)
    prefetch(pkg, sha1)
    fetch(pkg, sha1)
    checkout(pkg, sha1)
end

function remove(pkg::String)
    isdir(".trash") || mkdir(".trash")
    ispath(".trash/$pkg") && rm(".trash/$pkg", recursive=true)
    mv(pkg, ".trash/$pkg")
end

end # module
