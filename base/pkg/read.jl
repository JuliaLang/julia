module Read

import ..Git, ..Cache, ..Reqs
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
            haskey(pkgs,pkg) || (pkgs[pkg] = eltype(pkgs)[2]())
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

function isfixed(pkg::AbstractString, avail::Dict=available(pkg))
    isinstalled(pkg) || error("$pkg is not an installed package.")
    isfile("METADATA", pkg, "url") || return true
    ispath(pkg, ".git") || return true
    Git.dirty(dir=pkg) && return true
    Git.attached(dir=pkg) && return true
    !Git.success(`cat-file -e HEAD:REQUIRE`, dir=pkg) && isfile(pkg,"REQUIRE") && return true
    head = Git.head(dir=pkg)
    for (ver,info) in avail
        head == info.sha1 && return false
    end
    cache = Cache.path(pkg)
    cache_has_head = isdir(cache) && Git.iscommit(head, dir=cache)
    for (ver,info) in avail
        if cache_has_head && Git.iscommit(info.sha1, dir=cache)
            Git.is_ancestor_of(head, info.sha1, dir=cache) && return false
        elseif Git.iscommit(info.sha1, dir=pkg)
            Git.is_ancestor_of(head, info.sha1, dir=pkg) && return false
        else
            Base.warn_once("unknown $pkg commit $(info.sha1[1:8]), metadata may be ahead of package cache")
        end
    end
    return true
end

function installed_version(pkg::AbstractString, avail::Dict=available(pkg))
    ispath(pkg,".git") || return typemin(VersionNumber)
    head = Git.head(dir=pkg)
    vers = [keys(filter((ver,info)->info.sha1==head, avail))...]
    !isempty(vers) && return maximum(vers)
    cache = Cache.path(pkg)
    cache_has_head = isdir(cache) && Git.iscommit(head, dir=cache)
    ancestors = VersionNumber[]
    descendants = VersionNumber[]
    for (ver,info) in avail
        sha1 = info.sha1
        base = if cache_has_head && Git.iscommit(sha1, dir=cache)
            Git.readchomp(`merge-base $head $sha1`, dir=cache)
        elseif Git.iscommit(sha1, dir=pkg)
            Git.readchomp(`merge-base $head $sha1`, dir=pkg)
        else
            Base.warn_once("unknown $pkg commit $(sha1[1:8]), metadata may be ahead of package cache")
            continue
        end
        base == sha1 && push!(ancestors,ver)
        base == head && push!(descendants,ver)
    end
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
    Git.dirty("REQUIRE", dir=pkg) && return pkgreq
    !Git.success(`cat-file -e HEAD:REQUIRE`, dir=pkg) && isfile(pkgreq) && return pkgreq
    head = Git.head(dir=pkg)
    for (ver,info) in avail
        if head == info.sha1
            return joinpath("METADATA", pkg, "versions", string(ver), "requires")
        end
    end
    return pkgreq
end

function requires_list(pkg::AbstractString, avail::Dict=available(pkg))
    reqs = filter!(Reqs.read(requires_path(pkg,avail))) do line
        isa(line,Reqs.Requirement)
    end
    map(req->req.package, reqs)
end
requires_dict(pkg::AbstractString, avail::Dict=available(pkg)) =
    Reqs.parse(requires_path(pkg,avail))

function installed(avail::Dict=available())
    pkgs = Dict{ByteString,(VersionNumber,Bool)}()
    for pkg in readdir()
        isinstalled(pkg) || continue
        ap = get(avail,pkg,Dict{VersionNumber,Available}())
        pkgs[pkg] = (installed_version(pkg,ap),isfixed(pkg,ap))
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
    m = match(Git.GITHUB_REGEX, url(pkg))
    m == nothing && return ""
    return "https://github.com/" * m.captures[1] * "/issues"
end

end # module
