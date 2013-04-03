# module Pkg2
import Base: Git, isequal, isless, hash, isempty, contains

### VersionSet ###

immutable VersionInterval
    lower::VersionNumber
    upper::VersionNumber
end

isempty(i::VersionInterval) = i.upper <= i.lower
contains(i::VersionInterval, v::VersionNumber) = a.lower <= v < a.upper
intersect(a::VersionInterval, b::VersionInterval) = VersionInterval(max(a.lower,b.lower), min(a.upper,b.upper))
intersect(A::Vector{VersionInterval}, B::Vector{VersionInterval}) =
    sortby!(filter!(i->!isempty(i), vec([ intersect(a,b) for a in A, b in B ])), i->i.lower)

typealias Requires Dict{ByteString,Vector{VersionInterval}}

function parse_requires(readable)
    reqs = Requires()
    for line in eachline(readable)
        ismatch(r"^\s*(?:#|$)", line) && continue
        fields = split(replace(line, r"#.*$", ""))
        pkg = shift!(fields)
        if !all(x->ismatch(Base.VERSION_REGEX,x), fields)
            error("invalid requires entry for $pkg: $fields")
        end
        vers = [ convert(VersionNumber,x) for x in fields ]
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
parse_requires(file::String) = open(parse_requires,file)

function merge!(A::Requires, B::Requires)
    for (pkg, ivals) in B
        A[pkg] = has(A,pkg) ? intersect(A[pkg], ivals) : ivals
    end
    return A
end

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
        merge!(reqs, parse_requires(joinpath(pkg, "REQUIRE")))
    end
    return reqs
end

# end # module
