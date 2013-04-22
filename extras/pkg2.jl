# module Pkg2

function parse_requires(readable)
    reqs = Requires()
    for line in eachline(readable)
        ismatch(r"^\s*(?:#|$)", line) && continue
        fields = split(replace(line, r"#.*$", ""))
        pkg = shift!(fields)
        if !all(_->ismatch(Base.VERSION_REGEX,_), fields)
            error("invalid requires entry for $pkg: $fields")
        end
        vers = [convert(VersionNumber,_) for _ in fields]
        if !issorted(vers)
            error("invalid requires entry for $pkg: $vers")
        end
        ivals = VersionInterval[]
        if isempty(vers)
            push!(ivals, VersionInterval(typemin(VersionNumber),typemax(VersionNumber)))
        else
            isodd(length(vers)) && push!(vers, typemax(VersionNumber))
            while !isempty(vers)
                push!(ivals, VersionInterval(shift!(vers), shift!(vers)))
            end
        end
        reqs[pkg] = has(reqs,pkg) ? intersect(reqs[pkg], ivals) : ivals
    end
    return reqs
end
parse_requires(file::String) = isfile(file) ? open(parse_requires,file) : Requires()

function merge_requires!(A::Requires, B::Requires)
    for (pkg, ivals) in B
        A[pkg] = has(A,pkg) ? intersect(A[pkg], ivals) : ivals
    end
    return A
end

function available()
    pkgs = Dict{ByteString,Dict{VersionNumber,Available}}()
    for pkg in readdir("METADATA")
        isfile("METADATA", pkg, "url") || continue
        versdir = joinpath("METADATA", pkg, "versions")
        isdir(versdir) || continue
        for ver in readdir(versdir)
            ismatch(Base.VERSION_REGEX, ver) || continue
            isfile(versdir, ver, "sha1") || continue
            has(pkgs,pkg) || (pkgs[pkg] = eltype(pkgs)[2]())
            pkgs[pkg][convert(VersionNumber,ver)] = Available(
                readchomp(joinpath(versdir, ver, "sha1")),
                parse_requires(joinpath(versdir, ver, "requires"))
            )
        end
    end
    return pkgs
end

isinstalled(pkg::String) =
    pkg != "METADATA" && pkg != "REQUIRE" && isfile(pkg, "src", "$pkg.jl")

function isfixed(pkg::String, avail::Dict=available())
    isinstalled(pkg) || error("$pkg is not an installed package.")
    isfile("METADATA", pkg, "url") || return true
    ispath(pkg, ".git") || return true
    cd(pkg) do
        Git.dirty() && return true
        Git.attached() && return true
        head = Git.head()
        for (ver,info) in avail[pkg]
            Git.is_ancestor_of(head, info.sha1) && return false
        end
        return true
    end
end

function installed_version(pkg::String, avail::Dict=available())
    head = cd(Git.head,pkg)
    lo = typemin(VersionNumber)
    hi = typemin(VersionNumber)
    for (ver,info) in avail[pkg]
        head == info.sha1 && return ver
        base = cd(()->readchomp(`git merge-base $head $(info.sha1)`), pkg)
        if base == head # Git.is_ancestor_of(head, info.sha1)
            lo = max(lo,ver)
        elseif base == info.sha1 # Git.is_ancestor_of(info.sha1, head)
            hi = max(hi,ver)
        end
    end
    typemin(VersionNumber) < lo ?
        VersionNumber(lo.major, lo.minor, lo.patch, ("",), ()) :
        VersionNumber(hi.major, hi.minor, hi.patch, (), ("",))
end

function requires_path(pkg::String, avail::Dict=available())
    cd(pkg) do
        Git.dirty("REQUIRE") && return joinpath(pkg, "REQUIRE")
        head = Git.head()
        for (ver,info) in avail[pkg]
            if head == info.sha1
                return joinpath("METADATA", pkg, "versions", string(ver), "requires")
            end
        end
        return joinpath(pkg, "REQUIRE")
    end
end
requires_dict(pkg::String, avail::Dict=available()) =
    parse_requires(requires_path(pkg,avail))

function installed(avail::Dict=available())
    pkgs = Dict{ByteString,Installed}()
    for pkg in readdir()
        isinstalled(pkg) || continue
        pkgs[pkg] = !isfixed(pkg,avail) ? Free() :
            Fixed(installed_version(pkg,avail), requires_dict(pkg,avail))
    end
    pkgs["julia"] = Fixed(VERSION)
    return pkgs
end

# end # module
