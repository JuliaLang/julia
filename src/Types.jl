module Types

using Base.Random: UUID

export SHA1, VersionRange, VersionSet, @vr_str

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

macro vr_str(s::String); VersionRange(s); end

function Base.show(io::IO, r::VersionRange)
    print(io, "vr\"")
    join(io, r.lower.t, '.')
    if r.lower != r.upper
        print(io, '-')
        join(io, r.upper.t, '.')
    end
    print(io, '"')
end
Base.show(io::IO, ::VersionRange{0,0}) = print(io, "vr\"*\"")

Base.in(v::VersionNumber, r::VersionRange) = r.lower ≲ v ≲ r.upper
Base.in(v::VersionNumber, r::VersionNumber) = v == r

struct VersionSet
    ranges::Vector{VersionRange}
    VersionSet(r::Vector{<:VersionRange}) = new(r)
end
VersionSet() = VersionSet(VersionRange())

Base.in(v::VersionNumber, s::VersionSet) = any(v in r for r in s.ranges)

Base.convert(::Type{VersionSet}, v::VersionNumber) = VersionSet(VersionRange(v))
Base.convert(::Type{VersionSet}, r::VersionRange) = VersionSet(VersionRange[r])
Base.convert(::Type{VersionSet}, s::AbstractString) = VersionSet(VersionRange(s))
Base.convert(::Type{VersionSet}, v::AbstractVector) = VersionSet(map(VersionRange, v))

function Base.show(io::IO, s::VersionSet)
    length(s.ranges) == 1 && return print(io, s.ranges[1])
    print(io, '[')
    for i = 1:length(s.ranges)
        1 < i && print(io, ", ")
        print(io, s.ranges[i])
    end
    print(io, ']')
end

let VS = Pkg.Types.VersionSet, Av = Pkg.Types.Available
    Base.convert(::Type{VS}, v::VersionNumber) = VS(v, Base.nextpatch(v))
    Base.convert(::Type{VS}, r::VersionRange{0,0}) = VS()
    Base.convert(::Type{VS}, r::VersionRange{m,1}) where {m} =
        VS(VersionNumber(r.lower.t...), VersionNumber(r.upper[1]+1))
    Base.convert(::Type{VS}, r::VersionRange{m,2}) where {m} =
        VS(VersionNumber(r.lower.t...), VersionNumber(r.upper[1], r.upper[2]+1))
    Base.convert(::Type{VS}, r::VersionRange{m,3}) where {m} =
        VS(VersionNumber(r.lower.t...), VersionNumber(r.upper[1], r.upper[2], r.upper[3]+1))
    Base.convert(::Type{VS}, s::VersionSet) = mapreduce(VS, ∪, s.ranges)
    Base.convert(::Type{Av}, t::Tuple{SHA1,Dict{UUID,VersionSet}}) = Av(t...)
end

end # module
