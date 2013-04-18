import Base: Git, isequal, isless, hash, isempty, contains

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
