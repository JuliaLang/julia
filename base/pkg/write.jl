module Write

import ..Git, ..Cache, ..Read

function prefetch(pkg::AbstractString, sha1::AbstractString)
    isempty(Cache.prefetch(pkg, Read.url(pkg), sha1)) && return
    error("$pkg: couldn't find commit $(sha1[1:10])")
end

function fetch(pkg::AbstractString, sha1::AbstractString)
    refspec = "+refs/*:refs/remotes/cache/*"
    Git.run(`fetch -q $(Cache.path(pkg)) $refspec`, dir=pkg)
    Git.iscommit(sha1, dir=pkg) && return
    f = Git.iscommit(sha1, dir=Cache.path(pkg)) ? "fetch" : "prefetch"
    url = Read.issue_url(pkg)
    if isempty(url)
        error("$pkg: $f failed to get commit $(sha1[1:10]), please file a bug report with the package author.")
    else
        error("$pkg: $f failed to get commit $(sha1[1:10]), please file an issue at $url")
    end
end

function checkout(pkg::AbstractString, sha1::AbstractString)
    Git.set_remote_url(Read.url(pkg), dir=pkg)
    Git.run(`checkout -q $sha1`, dir=pkg)
end

function install(pkg::AbstractString, sha1::AbstractString)
    prefetch(pkg, sha1)
    if isdir(".trash/$pkg")
        mv(".trash/$pkg", "./$pkg")
    else
        Git.run(`clone -q $(Cache.path(pkg)) $pkg`)
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
