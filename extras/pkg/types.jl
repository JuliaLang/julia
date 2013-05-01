import Base: Git, isequal, isless, hash, isempty, contains, show

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

immutable Available
	sha1::ASCIIString
	requires::Requires
end

abstract Installed
immutable Free <: Installed end
immutable Fixed <: Installed
	version::VersionNumber
	requires::Requires
end
Fixed(v::VersionNumber) = Fixed(v,Requires())
