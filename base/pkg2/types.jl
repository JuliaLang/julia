module Types

export VersionInterval, VersionSet, Requires, Available, Fixed,
       merge_requires!, satisfies, @recover

immutable VersionInterval
    lower::VersionNumber
    upper::VersionNumber
end
VersionInterval(lower::VersionNumber) = VersionInterval(lower,typemax(VersionNumber))
VersionInterval() = VersionInterval(typemin(VersionNumber))

Base.show(io::IO, i::VersionInterval) = print(io, "[$(i.lower),$(i.upper))")
Base.isempty(i::VersionInterval) = i.upper <= i.lower
Base.contains(i::VersionInterval, v::VersionNumber) = i.lower <= v < i.upper
Base.intersect(a::VersionInterval, b::VersionInterval) = VersionInterval(max(a.lower,b.lower), min(a.upper,b.upper))
Base.isequal(a::VersionInterval, b::VersionInterval) = (a.lower == b.lower) & (a.upper == b.upper)

immutable VersionSet
    intervals::Vector{VersionInterval}
end
function VersionSet(versions::Vector{VersionNumber})
    intervals = VersionInterval[]
    if isempty(versions)
        push!(intervals, VersionInterval())
    else
        isodd(length(versions)) && push!(versions, typemax(VersionNumber))
        while !isempty(versions)
            push!(intervals, VersionInterval(shift!(versions), shift!(versions)))
        end
    end
    VersionSet(intervals)
end
VersionSet(versions::VersionNumber...) = VersionSet(VersionNumber[versions...])

Base.show(io::IO, s::VersionSet) = print_joined(io, s.intervals, " ∪ ")
Base.isempty(s::VersionSet) = all(i->isempty(i), s.intervals)
Base.contains(s::VersionSet, v::VersionNumber) = any(i->contains(i,v), s.intervals)
function Base.intersect(A::VersionSet, B::VersionSet)
    ivals = vec([ intersect(a,b) for a in A.intervals, b in B.intervals ])
    filter!(i->!isempty(i), ivals)
    sort!(ivals, by=i->i.lower)
    VersionSet(ivals)
end
Base.isequal(A::VersionSet, B::VersionSet) = (A.intervals == B.intervals)
Base.hash(s::VersionSet) = hash(s.intervals)

typealias Requires Dict{ByteString,VersionSet}

function merge_requires!(A::Requires, B::Requires)
    for (pkg,vers) in B
        A[pkg] = haskey(A,pkg) ? intersect(A[pkg],vers) : vers
    end
    return A
end

satisfies(pkg::String, ver::VersionNumber, reqs::Requires) =
    !haskey(reqs, pkg) || contains(reqs[pkg], ver)

immutable Available
    sha1::ASCIIString
    requires::Requires
end

Base.isequal(a::Available, b::Available) = (a.sha1 == b.sha1 && a.requires == b.requires)

Base.show(io::IO, a::Available) = isempty(a.requires) ?
    print(io, "Available(", repr(a.sha1), ")") :
    print(io, "Available(", repr(a.sha1), ",", a.requires, ")")

immutable Fixed
    version::VersionNumber
    requires::Requires
end
Fixed(v::VersionNumber) = Fixed(v,Requires())

Base.isequal(a::Fixed, b::Fixed) = (a.version == b.version && a.requires == b.requires)

Base.show(io::IO, f::Fixed) = isempty(f.requires) ?
    print(io, "Fixed(", repr(f.version), ")") :
    print(io, "Fixed(", repr(f.version), ",", f.requires, ")")

# TODO: Available & Fixed are almost the same – merge them?
# Free could include the same information too, it just isn't
# required by anything that processes these things.

macro recover(ex)
    quote
        try $(esc(ex))
        catch err
            show(err)
        end
    end
end

end # module
