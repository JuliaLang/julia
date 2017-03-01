# This file is a part of Julia. License is MIT: http://julialang.org/license

module Write

import ...LibGit2, ..Cache, ..Read, ...Pkg.PkgError
importall ...LibGit2

function prefetch(pkg::AbstractString, sha1::AbstractString)
    isempty(Cache.prefetch(pkg, Read.url(pkg), sha1)) && return
    throw(PkgError("$pkg: couldn't find commit $(sha1[1:10])"))
end

function fetch(repo::GitRepo, pkg::AbstractString, sha1::AbstractString)
    cache = Cache.path(pkg)
    LibGit2.fetch(repo, remoteurl=cache, refspecs=["+refs/*:refs/remotes/cache/*"])
    LibGit2.need_update(repo)
    LibGit2.iscommit(sha1, repo) && return
    f = LibGit2.iscommit(sha1, GitRepo(cache)) ? "fetch" : "prefetch"
    url = Read.issue_url(pkg)
    if isempty(url)
        throw(PkgError("$pkg: $f failed to get commit $(sha1[1:10]), please file a bug report with the package author."))
    else
        throw(PkgError("$pkg: $f failed to get commit $(sha1[1:10]), please file an issue at $url"))
    end
end

function checkout(repo::GitRepo, pkg::AbstractString, sha1::AbstractString)
    LibGit2.set_remote_url(repo, Cache.normalize_url(Read.url(pkg)))
    LibGit2.checkout!(repo, sha1)
end

function install(pkg::AbstractString, sha1::AbstractString)
    prefetch(pkg, sha1)
    repo = if isdir(".trash/$pkg")
        mv(".trash/$pkg", "./$pkg") #TODO check for newer version in cache before moving
        GitRepo(pkg)
    else
        LibGit2.clone(Cache.path(pkg), pkg)
    end
    fetch(repo, pkg, sha1)
    checkout(repo, pkg, sha1)
end

function update(pkg::AbstractString, sha1::AbstractString)
    prefetch(pkg, sha1)
    repo = GitRepo(pkg)
    fetch(repo, pkg, sha1)
    checkout(repo, pkg, sha1)
end

function remove(pkg::AbstractString)
    isdir(".trash") || mkdir(".trash")
    ispath(".trash/$pkg") && rm(".trash/$pkg", recursive=true)
    mv(pkg, ".trash/$pkg")
end

end # module
