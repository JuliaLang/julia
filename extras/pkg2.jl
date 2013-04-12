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

function merge!(A::Requires, B::Requires)
    for (pkg, ivals) in B
        A[pkg] = has(A,pkg) ? intersect(A[pkg], ivals) : ivals
    end
    return A
end

function isfixed(pkg::String)
    isfile("METDATA", pkg, "url") || return true
    ispath(pkg, ".git") || return true
    cd(pkg) do
        Git.dirty() && return true
        Git.attached() && return true
        for line in eachline(`git branch -r --contains`)
            return false
        end
        return true
    end
end

function installed()
    pkgs = Dict{ByteString,(Bool,Requires)}()
    for pkg in readdir()
        pkg == "METADATA" && continue
        pkg == "REQUIRE" && continue
        isfile(pkg, "src", "$pkg.jl") || continue
        pkgs[pkg] = (isfixed(pkg), parse_requires(joinpath(pkg, "REQUIRE")))
    end
    return pkgs
end

function available()
    pkgs = Dict{ByteString,Dict{VersionNumber,Requires}}()
    for pkg in readdir("METADATA")
        isfile("METADATA", pkg, "url") || continue
        versions = joinpath("METADATA", pkg, "versions")
        isdir(versions) || continue
        for ver in readdir(versions)
            ismatch(Base.VERSION_REGEX, ver) || continue
            isfile(versions, ver, "sha1") || continue
            if !has(pkgs,pkg) pkgs[pkg] = Dict{VersionNumber,Requires}() end
            pkgs[pkg][convert(VersionNumber,ver)] = parse_requires(joinpath(versions, ver, "requires"))
        end
    end
    return pkgs
end

# end # module
