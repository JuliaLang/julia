module Types

using Base.Random: UUID
using Base.Pkg.Types: VersionSet, Available

export SHA1, VersionRange, VersionSpec, Addition, Removal

## ordering of UUIDs ##

Base.isless(a::UUID, b::UUID) = a.value < b.value
Base.convert(::Type{String}, u::UUID) = string(u)

## SHA1 ##

struct SHA1
    bytes::Vector{UInt8}
    function SHA1(bytes::Vector{UInt8})
        length(bytes) == 20 ||
            throw(ArgumentError("wrong number of bytes for SHA1 hash: $(length(bytes))"))
        return new(bytes)
    end
end

Base.convert(::Type{SHA1}, s::String) = SHA1(hex2bytes(s))
Base.convert(::Type{Vector{UInt8}}, hash::SHA1) = hash.bytes
Base.convert(::Type{String}, hash::SHA1) = bytes2hex(Vector{UInt8}(hash))

Base.string(hash::SHA1) = String(hash)
Base.show(io::IO, hash::SHA1) = print(io, "SHA1(", String(hash), ")")
Base.isless(a::SHA1, b::SHA1) = lexless(a.bytes, b.bytes)
Base.hash(a::SHA1, h::UInt) = hash((SHA1, a.bytes), h)
Base.:(==)(a::SHA1, b::SHA1) = a.bytes == b.bytes

## VersionRange ##

struct VersionBound{n}
    t::NTuple{n,Int}
    function VersionBound{n}(t::NTuple{n,Integer}) where n
        n <= 3 || throw(ArgumentError("VersionBound: you can only specify major, minor and patch versions"))
        return new(t)
    end
end
VersionBound(t::Integer...) = VersionBound{length(t)}(t)

Base.convert(::Type{VersionBound}, v::VersionNumber) =
    VersionBound(v.major, v.minor, v.patch)

Base.getindex(b::VersionBound, i::Int) = b.t[i]

≲(v::VersionNumber, b::VersionBound{0}) = true
≲(v::VersionNumber, b::VersionBound{1}) = v.major <= b[1]
≲(v::VersionNumber, b::VersionBound{2}) = (v.major, v.minor) <= (b[1], b[2])
≲(v::VersionNumber, b::VersionBound{3}) = (v.major, v.minor, v.patch) <= (b[1], b[2], b[3])

≲(b::VersionBound{0}, v::VersionNumber) = true
≲(b::VersionBound{1}, v::VersionNumber) = v.major >= b[1]
≲(b::VersionBound{2}, v::VersionNumber) = (v.major, v.minor) >= (b[1], b[2])
≲(b::VersionBound{3}, v::VersionNumber) = (v.major, v.minor, v.patch) >= (b[1], b[2], b[3])

≳(v::VersionNumber, b::VersionBound) = v ≲ b
≳(b::VersionBound, v::VersionNumber) = b ≲ v

Base.convert(::Type{VersionBound}, s::AbstractString) =
    VersionBound(map(x->parse(Int, x), split(s, '.'))...)

struct VersionRange{m,n}
    lower::VersionBound{m}
    upper::VersionBound{n}
    # TODO: check non-emptiness of range?
end
VersionRange(b::VersionBound=VersionBound()) = VersionRange(b, b)

Base.convert(::Type{VersionRange}, v::VersionNumber) =
    VersionRange(VersionBound(v))

function Base.convert(::Type{VersionRange}, s::AbstractString)
    ismatch(r"^\s*\*\s*$", s) && return VersionRange()
    m = match(r"^\s*(\d+(?:\.\d+)?(?:\.\d+)?)(?:\s*-\s*(\d+(?:\.\d+)?(?:\.\d+)?))?\s*$", s)
    m == nothing && throw(ArgumentError("invalid version range: $(repr(s))"))
    lower = VersionBound(m.captures[1])
    upper = m.captures[2] != nothing ? VersionBound(m.captures[2]) : lower
    return VersionRange(lower, upper)
end

function Base.print(io::IO, r::VersionRange)
    join(io, r.lower.t, '.')
    if r.lower != r.upper
        print(io, '-')
        join(io, r.upper.t, '.')
    end
end
Base.print(io::IO, ::VersionRange{0,0}) = print(io, "*")
Base.show(io::IO, r::VersionRange) = print(io, "VersionRange(\"", r, "\")")

Base.in(v::VersionNumber, r::VersionRange) = r.lower ≲ v ≲ r.upper
Base.in(v::VersionNumber, r::VersionNumber) = v == r

struct VersionSpec
    ranges::Vector{VersionRange}
    VersionSpec(r::Vector{<:VersionRange}) = new(r)
end
VersionSpec() = VersionSpec(VersionRange())

Base.in(v::VersionNumber, s::VersionSpec) = any(v in r for r in s.ranges)

Base.convert(::Type{VersionSpec}, v::VersionNumber) = VersionSpec(VersionRange(v))
Base.convert(::Type{VersionSpec}, r::VersionRange) = VersionSpec(VersionRange[r])
Base.convert(::Type{VersionSpec}, s::AbstractString) = VersionSpec(VersionRange(s))
Base.convert(::Type{VersionSpec}, v::AbstractVector) = VersionSpec(map(VersionRange, v))

function Base.print(io::IO, s::VersionSpec)
    length(s.ranges) == 1 && return print(io, s.ranges[1])
    print(io, '[')
    for i = 1:length(s.ranges)
        1 < i && print(io, ", ")
        print(io, s.ranges[i])
    end
    print(io, ']')
end
Base.show(io::IO, s::VersionSpec) = print(io, "VersionSpec(\"", s, "\")")

Base.convert(::Type{VersionSet}, v::VersionNumber) = VersionSet(v, Base.nextpatch(v))
Base.convert(::Type{VersionSet}, r::VersionRange{0,0}) = VersionSet()
Base.convert(::Type{VersionSet}, r::VersionRange{m,1}) where {m} =
    VersionSet(VersionNumber(r.lower.t...), VersionNumber(r.upper[1]+1))
Base.convert(::Type{VersionSet}, r::VersionRange{m,2}) where {m} =
    VersionSet(VersionNumber(r.lower.t...), VersionNumber(r.upper[1], r.upper[2]+1))
Base.convert(::Type{VersionSet}, r::VersionRange{m,3}) where {m} =
    VersionSet(VersionNumber(r.lower.t...), VersionNumber(r.upper[1], r.upper[2], r.upper[3]+1))
Base.convert(::Type{VersionSet}, s::VersionSpec) = mapreduce(VersionSet, ∪, s.ranges)
Base.convert(::Type{Available}, t::Tuple{SHA1,Dict{UUID,VersionSpec}}) = Available(t...)

## type for expressing operations ##

mutable struct Addition
    name::String
    uuid::UUID
    vers::VersionRange
    Addition(
        name::AbstractString = "",
        uuid::UUID = UUID(zero(UInt128)),
        vers::VersionRange = VersionRange("*"),
    ) = new(name, uuid, vers)
end
Addition(uuid::UUID) = Addition("", uuid)

mutable struct Removal
    name::String
    uuid::UUID
    Removal(
        name::AbstractString = "",
        uuid::UUID = UUID(zero(UInt128)),
    ) = new(name, uuid)
end
Removal(uuid::UUID) = Removal("", uuid)

end # module
