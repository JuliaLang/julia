module Read

using ..Types, ..Reqs, Base.Git

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
                readchomp(joinpath(versdir, ver, "sha1")),
                Reqs.parse(joinpath(versdir, ver, "requires"))
            )
        end
    end
    return pkgs
end
available(pkg::String) = available([pkg])[pkg]

isinstalled(pkg::String) =
    pkg != "METADATA" && pkg != "REQUIRE" && isfile(pkg, "src", "$pkg.jl")

function isfixed(pkg::String, avail::Dict=available(pkg))
    isinstalled(pkg) || error("$pkg is not an installed package.")
    isfile("METADATA", pkg, "url") || return true
    ispath(pkg, ".git") || return true
    cd(pkg) do
        Git.dirty() && return true
        Git.attached() && return true
        head = Git.head()
        for (ver,info) in avail
            if Git.iscommit(info.sha1)
                Git.is_ancestor_of(head, info.sha1) && return false
            else
                Base.warn_once("unknown $pkg commit $(info.sha1[1:10]) (METADATA may be ahead of package repo).")
            end
        end
        return true
    end
end

function installed_version(pkg::String, avail::Dict=available(pkg))
    head = cd(Git.head,pkg)
    lo = typemin(VersionNumber)
    hi = typemin(VersionNumber)
    for (ver,info) in avail
        head == info.sha1 && return ver
        cd(pkg) do
            if Git.iscommit(info.sha1)
                base = readchomp(`git merge-base $head $(info.sha1)`)
                if base == head # Git.is_ancestor_of(head, info.sha1)
                    lo = max(lo,ver)
                elseif base == info.sha1 # Git.is_ancestor_of(info.sha1, head)
                    hi = max(hi,ver)
                end
            else
                Base.warn_once("unknown $pkg commit $(info.sha1[1:10]) (METADATA may be ahead of package repo).")
            end
        end
    end
    typemin(VersionNumber) < lo ?
        VersionNumber(lo.major, lo.minor, lo.patch, ("",), ()) :
        VersionNumber(hi.major, hi.minor, hi.patch, (), ("",))
end

function requires_path(pkg::String, avail::Dict=available(pkg))
    cd(pkg) do
        Git.dirty("REQUIRE") && return joinpath(pkg, "REQUIRE")
        head = Git.head()
        for (ver,info) in avail
            if head == info.sha1
                return joinpath("METADATA", pkg, "versions", string(ver), "requires")
            end
        end
        return joinpath(pkg, "REQUIRE")
    end
end
requires_dict(pkg::String, avail::Dict=available(pkg)) = Reqs.parse(requires_path(pkg,avail))

function fixed(avail::Dict=available())
    pkgs = Dict{ByteString,Fixed}()
    for pkg in readdir()
        isinstalled(pkg) || continue
        ap = avail[pkg]
        isfixed(pkg,ap) || continue
        pkgs[pkg] = Fixed(installed_version(pkg,ap),requires_dict(pkg,ap))
    end
    pkgs["julia"] = Fixed(VERSION)
    return pkgs
end

end # module
