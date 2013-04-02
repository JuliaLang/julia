# module Pkg2
import Base: Git, isequal, isless, hash, intersect, contains

### VersionSet ###

abstract VersionInterval
immutable NoVersions <: VersionInterval end
immutable AllVersions <: VersionInterval end
immutable VersionsGreater <: VersionInterval
    lo::VersionNumber
end
immutable VersionsBetween <: VersionInterval
    lo::VersionNumber
    hi::VersionNumber
end

interval(lo::VersionNumber) = VersionsGreater(lo)
interval(lo::VersionNumber, hi::VersionNumber) = lo < hi ? VersionsBetween(lo, hi) : NoVersions()

intersect(x::NoVersions,      y::VersionInterval) = x
intersect(x::AllVersions,     y::VersionInterval) = y
intersect(x::VersionInterval, y::VersionInterval) = intersect(y,x)
intersect(x::VersionsGreater, y::VersionsGreater) = interval(max(x.lo,y.lo))
intersect(x::VersionsGreater, y::VersionsBetween) = interval(max(x.lo,y.lo),y.hi)
intersect(x::VersionsBetween, y::VersionsBetween) = interval(max(x.lo,y.lo),min(x.hi,y.hi))

contains(i::NoVersions, v::VersionNumber) = false
contains(i::AllVersions, v::VersionNumber) = true
contains(i::VersionsGreater, v::VersionNumber) = i.lo <= v
contains(i::VersionsBetween, v::VersionNumber) = i.lo <= v < i.hi

immutable VersionSet
    package::ByteString
    versions::Vector{VersionNumber}
    function VersionSet(pkg::ByteString, vers::Vector{VersionNumber})
        if !issorted(vers)
            error("version numbers must be sorted")
        end
        new(pkg, vers)
    end
end
VersionSet(pkg::ByteString) = VersionSet(pkg, VersionNumber[])

isequal(a::VersionSet, b::VersionSet) =
    a.package == b.package && a.versions == b.versions
isless(a::VersionSet, b::VersionSet) = a.package < b.package
hash(s::VersionSet) = hash([s.(n) for n in VersionSet.names])

function parse_requires(readable)
    reqs = VersionSet[]
    for line in eachline(readable)
        if ismatch(r"^\s*(?:#|$)", line) continue end
        line = replace(line, r"#.*$", "")
        fields = split(line)
        pkg = shift!(fields)
        vers = [ convert(VersionNumber,x) for x in fields ]
        if !issorted(vers)
            error("invalid requires entry for $pkg: $vers")
        end
        # TODO: merge version sets instead of appending?
        push!(reqs,VersionSet(pkg,vers))
    end
    sort!(reqs)
end
parse_requires(file::String) = open(parse_requires,file)

each_installed() = @task begin
    for line in eachline(`ls`)
        name = chomp(line)
        name == "METADATA" && continue
        name == "REQUIRE" && continue
        isfile(name, "src", "$name.jl") && produce(name)
    end
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

function requirements()
    reqs = parse_requires("REQUIRE")
    for pkg in each_installed()
        isfixed(pkg) || continue
        isfile(pkg, "REQUIRE") || continue
        append!(reqs, parse_requires(joinpath(pkg, "REQUIRE")))
    end
    sort!(reqs)
end

# end # module
