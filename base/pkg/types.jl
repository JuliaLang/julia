# This file is a part of Julia. License is MIT: http://julialang.org/license

module Types

export VersionInterval, VersionSet, Requires, Available, Fixed, merge_requires!, satisfies
import Base: show, isempty, in, intersect, ==, hash, deepcopy_internal

immutable VersionInterval
    lower::VersionNumber
    upper::VersionNumber
end
VersionInterval(lower::VersionNumber) = VersionInterval(lower,typemax(VersionNumber))
VersionInterval() = VersionInterval(typemin(VersionNumber))

show(io::IO, i::VersionInterval) = print(io, "[$(i.lower),$(i.upper))")
isempty(i::VersionInterval) = i.upper <= i.lower
in(v::VersionNumber, i::VersionInterval) = i.lower <= v < i.upper
intersect(a::VersionInterval, b::VersionInterval) = VersionInterval(max(a.lower,b.lower), min(a.upper,b.upper))
==(a::VersionInterval, b::VersionInterval) = a.lower == b.lower && a.upper == b.upper
hash(i::VersionInterval, h::UInt) = hash((i.lower, i.upper), h + (0x0f870a92db508386 % UInt))

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

show(io::IO, s::VersionSet) = print_joined(io, s.intervals, " ∪ ")
isempty(s::VersionSet) = all(i->isempty(i), s.intervals)
in(v::VersionNumber, s::VersionSet) = any(i->in(v,i), s.intervals)
function intersect(A::VersionSet, B::VersionSet)
    ivals = vec([ intersect(a,b) for a in A.intervals, b in B.intervals ])
    ivals = copy(ivals) # temporary bandaid for issue #4592
    filter!(i->!isempty(i), ivals)
    sort!(ivals, by=i->i.lower)
    VersionSet(ivals)
end
==(A::VersionSet, B::VersionSet) = A.intervals == B.intervals
hash(s::VersionSet, h::UInt) = hash(s.intervals, h + (0x2fd2ca6efa023f44 % UInt))
deepcopy_internal(vs::VersionSet, ::ObjectIdDict) = VersionSet(copy(vs.intervals))

typealias Requires Dict{ByteString,VersionSet}

function merge_requires!(A::Requires, B::Requires)
    for (pkg,vers) in B
        A[pkg] = haskey(A,pkg) ? intersect(A[pkg],vers) : vers
    end
    return A
end

satisfies(pkg::AbstractString, ver::VersionNumber, reqs::Requires) =
    !haskey(reqs, pkg) || in(ver, reqs[pkg])

immutable Available
    sha1::UTF8String
    requires::Requires
end

==(a::Available, b::Available) = a.sha1 == b.sha1 && a.requires == b.requires
hash(a::Available, h::UInt) = hash((a.sha1, a.requires), h + (0xbc8ae0de9d11d972 % UInt))

show(io::IO, a::Available) = isempty(a.requires) ?
    print(io, "Available(", repr(a.sha1), ")") :
    print(io, "Available(", repr(a.sha1), ",", a.requires, ")")

immutable Fixed
    version::VersionNumber
    requires::Requires
end
Fixed(v::VersionNumber) = Fixed(v,Requires())

==(a::Fixed, b::Fixed) = a.version == b.version && a.requires == b.requires
hash(f::Fixed, h::UInt) = hash((f.version, f.requires), h + (0x68628b809fd417ca % UInt))

show(io::IO, f::Fixed) = isempty(f.requires) ?
    print(io, "Fixed(", repr(f.version), ")") :
    print(io, "Fixed(", repr(f.version), ",", f.requires, ")")

# TODO: Available & Fixed are almost the same – merge them?
# Free could include the same information too, it just isn't
# required by anything that processes these things.

end # module
