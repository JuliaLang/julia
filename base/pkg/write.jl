# This file is a part of Julia. License is MIT: http://julialang.org/license

module Write

import ..LibGit2, ..Cache, ..Read
importall ..LibGit2

function prefetch(pkg::AbstractString, sha1::AbstractString)
    isempty(Cache.prefetch(pkg, Read.url(pkg), sha1)) && return
    error("$pkg: couldn't find commit $(sha1[1:10])")
end

function fetch(pkg::AbstractString, sha1::AbstractString)
    refspec = "+refs/*:refs/remotes/cache/*"
    cache = Cache.path(pkg)
    with(GitRepo, pkg) do repo
        LibGit2.fetch(repo, cache, refspec)
        LibGit2.iscommit(sha1, repo)
    end && return
    f = with(GitRepo, cache) do repo
         LibGit2.iscommit(sha1, repo)
    end ? "fetch" : "prefetch"
    url = Read.issue_url(pkg)
    if isempty(url)
        error("$pkg: $f failed to get commit $(sha1[1:10]), please file a bug report with the package author.")
    else
        error("$pkg: $f failed to get commit $(sha1[1:10]), please file an issue at $url")
    end
end

function checkout(pkg::AbstractString, sha1::AbstractString)
    with(GitRepo, pkg) do repo
        LibGit2.set_remote_url(repo, Read.url(pkg))
        LibGit2.checkout(repo, sha1)
    end
end

function install(pkg::AbstractString, sha1::AbstractString)
    prefetch(pkg, sha1)
    if isdir(".trash/$pkg")
        mv(".trash/$pkg", "./$pkg")
    else
        LibGit2.clone(Cache.path(pkg), pkg)
    end
    fetch(pkg, sha1)
    checkout(pkg, sha1)
end

function update(pkg::AbstractString, sha1::AbstractString)
    prefetch(pkg, sha1)
    fetch(pkg, sha1)
    checkout(pkg, sha1)
end

function remove(pkg::AbstractString)
    isdir(".trash") || mkdir(".trash")
    ispath(".trash/$pkg") && rm(".trash/$pkg", recursive=true)
    mv(pkg, ".trash/$pkg")
end

end # module
