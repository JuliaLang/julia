# This file is a part of Julia. License is MIT: http://julialang.org/license

module Read

import ..LibGit2, ..Cache, ..Reqs
using ..Types

readstrip(path...) = strip(readall(joinpath(path...)))

url(pkg::AbstractString) = readstrip("METADATA", pkg, "url")
sha1(pkg::AbstractString, ver::VersionNumber) = readstrip("METADATA", pkg, "versions", string(ver), "sha1")

function available(names=readdir("METADATA"))
    pkgs = Dict{ByteString,Dict{VersionNumber,Available}}()
    for pkg in names
        isfile("METADATA", pkg, "url") || continue
        versdir = joinpath("METADATA", pkg, "versions")
        isdir(versdir) || continue
        for ver in readdir(versdir)
            ismatch(Base.VERSION_REGEX, ver) || continue
            isfile(versdir, ver, "sha1") || continue
            haskey(pkgs,pkg) || (pkgs[pkg] = Dict{VersionNumber,Available}())
            pkgs[pkg][convert(VersionNumber,ver)] = Available(
                readchomp(joinpath(versdir,ver,"sha1")),
                Reqs.parse(joinpath(versdir,ver,"requires"))
            )
        end
    end
    return pkgs
end
available(pkg::AbstractString) = get(available([pkg]),pkg,Dict{VersionNumber,Available}())

function latest(names=readdir("METADATA"))
    pkgs = Dict{ByteString,Available}()
    for pkg in names
        isfile("METADATA", pkg, "url") || continue
        versdir = joinpath("METADATA", pkg, "versions")
        isdir(versdir) || continue
        pkgversions = VersionNumber[]
        for ver in readdir(versdir)
            ismatch(Base.VERSION_REGEX, ver) || continue
            isfile(versdir, ver, "sha1") || continue
            push!(pkgversions, convert(VersionNumber,ver))
        end
        isempty(pkgversions) && continue
        ver = string(maximum(pkgversions))
        pkgs[pkg] = Available(
                readchomp(joinpath(versdir,ver,"sha1")),
                Reqs.parse(joinpath(versdir,ver,"requires"))
            )
    end
    return pkgs
end

isinstalled(pkg::AbstractString) =
    pkg != "METADATA" && pkg != "REQUIRE" && pkg[1] != '.' && isdir(pkg)

function isfixed(pkg::AbstractString, prepo::LibGit2.GitRepo, avail::Dict=available(pkg))
    isinstalled(pkg) || error("$pkg is not an installed package.")
    isfile("METADATA", pkg, "url") || return true
    ispath(pkg, ".git") || return true

    LibGit2.isdirty(prepo) && return true
    LibGit2.isattached(prepo) && return true
    LibGit2.revparse(prepo, "HEAD:REQUIRE") == nothing && isfile(pkg,"REQUIRE") && return true

    head = string(LibGit2.head_oid(prepo))
    for (ver,info) in avail
        head == info.sha1 && return false
    end

    cache = Cache.path(pkg)
    cache_has_head = if isdir(cache)
        crepo = LibGit2.GitRepo(cache)
        LibGit2.iscommit(head, crepo)
    else
        false
    end
    res = true
    for (ver,info) in avail
        if cache_has_head && LibGit2.iscommit(info.sha1, crepo)
            if LibGit2.is_ancestor_of(head, info.sha1, crepo)
                res = false
                break
            end
        elseif LibGit2.iscommit(info.sha1, prepo)
            if LibGit2.is_ancestor_of(head, info.sha1, prepo)
                res = false
                break
            end
        else
            Base.warn_once("unknown $pkg commit $(info.sha1[1:8]), metadata may be ahead of package cache")
        end
    end
    cache_has_head && LibGit2.free!(crepo)
    return res
end

function installed_version(pkg::AbstractString, prepo::LibGit2.GitRepo, avail::Dict=available(pkg))
    ispath(pkg,".git") || return typemin(VersionNumber)

    # get package repo head hash
    head = string(LibGit2.head_oid(prepo))
    isempty(head) && return typemin(VersionNumber)

    vers = collect(keys(filter((ver,info)->info.sha1==head, avail)))
    !isempty(vers) && return maximum(vers)

    cache = Cache.path(pkg)
    cache_has_head = if isdir(cache)
        crepo = LibGit2.GitRepo(cache)
        LibGit2.iscommit(head, crepo)
    else
        false
    end
    ancestors = VersionNumber[]
    descendants = VersionNumber[]
    for (ver,info) in avail
        sha1 = info.sha1
        base = if cache_has_head && LibGit2.iscommit(sha1, crepo)
            LibGit2.merge_base(head, sha1, crepo)
        elseif LibGit2.iscommit(sha1, prepo)
            LibGit2.merge_base(head, sha1, prepo)
        else
            Base.warn_once("unknown $pkg commit $(sha1[1:8]), metadata may be ahead of package cache")
            continue
        end
        base == sha1 && push!(ancestors,ver)
        base == head && push!(descendants,ver)
    end
    cache_has_head && LibGit2.free!(crepo)

    both = sort!(intersect(ancestors,descendants))
    isempty(both) || warn("$pkg: some versions are both ancestors and descendants of head: $both")
    if !isempty(descendants)
        v = minimum(descendants)
        return VersionNumber(v.major, v.minor, v.patch, ("",), ())
    elseif !isempty(ancestors)
        v = maximum(ancestors)
        return VersionNumber(v.major, v.minor, v.patch, (), ("",))
    else
        return typemin(VersionNumber)
    end
end

function requires_path(pkg::AbstractString, avail::Dict=available(pkg))
    pkgreq = joinpath(pkg,"REQUIRE")
    ispath(pkg,".git") || return pkgreq
    repo = LibGit2.GitRepo(pkg)
    LibGit2.isdirty(repo, "REQUIRE") && return pkgreq
    LibGit2.revparse(prepo, "HEAD:REQUIRE") == nothing && isfile(pkgreq) && return pkgreq
    head = string(LibGit2.head_oid(prepo))
    LibGit2.free!(repo)
    for (ver,info) in avail
        if head == info.sha1
            return joinpath("METADATA", pkg, "versions", string(ver), "requires")
        end
    end
    return pkgreq
end

requires_list(pkg::AbstractString, avail::Dict=available(pkg)) =
    collect(keys(Reqs.parse(requires_path(pkg,avail))))

requires_dict(pkg::AbstractString, avail::Dict=available(pkg)) =
    Reqs.parse(requires_path(pkg,avail))

function installed(avail::Dict=available())
    pkgs = Dict{ByteString,Tuple{VersionNumber,Bool}}()
    for pkg in readdir()
        isinstalled(pkg) || continue
        ap = get(avail,pkg,Dict{VersionNumber,Available}())
        try
            prepo = LibGit2.GitRepo(pkg)
            try
                ver = installed_version(pkg, prepo, ap)
                fixed = isfixed(pkg, prepo, ap)
                pkgs[pkg] = (ver, fixed)
            catch e
                pkgs[pkg] = (typemin(VersionNumber), true)
            finally
                LibGit2.free!(prepo)
            end
        catch
            pkgs[pkg] = (typemin(VersionNumber), true)
        end
    end
    return pkgs
end

function fixed(avail::Dict=available(), inst::Dict=installed(avail),
    julia_version::VersionNumber=VERSION)
    pkgs = Dict{ByteString,Fixed}()
    for (pkg,(ver,fix)) in inst
        fix || continue
        ap = get(avail,pkg,Dict{VersionNumber,Available}())
        pkgs[pkg] = Fixed(ver,requires_dict(pkg,ap))
    end
    pkgs["julia"] = Fixed(julia_version)
    return pkgs
end

function free(inst::Dict=installed())
    pkgs = Dict{ByteString,VersionNumber}()
    for (pkg,(ver,fix)) in inst
        fix && continue
        pkgs[pkg] = ver
    end
    return pkgs
end

function issue_url(pkg::AbstractString)
    ispath(pkg,".git") || return ""
    m = match(LibGit2.GITHUB_REGEX, url(pkg))
    m == nothing && return ""
    return "https://github.com/" * m.captures[1] * "/issues"
end

end # module
