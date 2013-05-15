import Base: Git,  show, isempty, contains, intersect

immutable VersionInterval
    lower::VersionNumber
    upper::VersionNumber
end

show(io::IO, i::VersionInterval) = print(io, "[$(i.lower),$(i.upper))")
isempty(i::VersionInterval) = i.upper <= i.lower
contains(i::VersionInterval, v::VersionNumber) = i.lower <= v < i.upper
intersect(a::VersionInterval, b::VersionInterval) = VersionInterval(max(a.lower,b.lower), min(a.upper,b.upper))

immutable VersionSet
	intervals::Vector{VersionInterval}
end

show(io::IO, s::VersionSet) = print_joined(io, s.intervals, " ∪ ")
isempty(s::VersionSet) = all(i->isempty(i), s.intervals)
contains(s::VersionSet, v::VersionNumber) = any(i->contains(i,v), s.intervals)
function intersect(A::VersionSet, B::VersionSet)
	ivals = vec([ intersect(a,b) for a in A.intervals, b in B.intervals ])
	filter!(i->!isempty(i), ivals)
    sortby!(ivals, i->i.lower)
    VersionSet(ivals)
end

typealias Requires Dict{ByteString,VersionSet}

satisfies(pkg::String, ver::VersionNumber, reqs::Requires) =
	!haskey(reqs, pkg) || contains(reqs[pkg], ver)

immutable Available
	sha1::ASCIIString
	requires::Requires
end

show(io::IO, a::Available) = isempty(a.requires) ?
	print(io, "Available(", repr(a.sha1), ")") :
	print(io, "Available(", repr(a.sha1), ",", a.requires, ")")

abstract Installed
immutable Free <: Installed end
immutable Fixed <: Installed
	version::VersionNumber
	requires::Requires
end
Fixed(v::VersionNumber) = Fixed(v,Requires())

show(io::IO, f::Fixed) = isempty(f.requires) ?
	print(io, "Fixed(", repr(f.version), ")") :
	print(io, "Fixed(", repr(f.version), ",", f.requires, ")")

# TODO: Available & Fixed are almost the same – merge them?
# Free could include the same information too, it just isn't
# required by anything that processes these things.
