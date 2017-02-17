module Types

using Base.Random: UUID
using Base.Pkg.Types: VersionSet

export SHA1, VersionSpec, @vs_str

## ordering of UUIDs ##

Base.isless(a::UUID, b::UUID) = a.value < b.value

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

## VersionSpec ##

struct VersionSpec{n}
	spec::NTuple{n,Int}
	function VersionSpec{n}(spec::NTuple{n,Integer}) where n
		n <= 3 || throw(ArgumentError("VersionSpec: you can only specify major, minor and patch versions"))
		return new(spec)
	end
end
VersionSpec(spec::Integer...) = VersionSpec{length(spec)}(spec)

function Base.convert(::Type{VersionSpec}, s::AbstractString)
	isempty(s) || s == "*" ? VersionSpec() :
	VersionSpec(map(x->parse(Int, x), split(s, '.'))...)
end

macro vs_str(s::String); VersionSpec(s); end

function Base.show(io::IO, s::VersionSpec)
	print(io, "vs\"")
	join(io, s.spec, '.')
	print(io, '"')
end
Base.show(io::IO, ::VersionSpec{0}) = print(io, "vs\"*\"")
Base.getindex(s::VersionSpec, i::Int) = s.spec[i]

Base.in(v::VersionNumber, s::VersionSpec{0}) = true
Base.in(v::VersionNumber, s::VersionSpec{1}) = v.major == s.spec[1]
Base.in(v::VersionNumber, s::VersionSpec{2}) = v.major == s.spec[1] && v.minor == s.spec[2]
Base.in(v::VersionNumber, s::VersionSpec{3}) = v.major == s.spec[1] && v.minor == s.spec[2] && v.patch == s.spec[3]

Base.convert(::Type{VersionSet}, s::VersionSpec{0}) = VersionSet()
Base.convert(::Type{VersionSet}, s::VersionSpec{1}) =
	VersionSet(VersionNumber(s[1]), VersionNumber(s[1]+1))
Base.convert(::Type{VersionSet}, s::VersionSpec{2}) =
	VersionSet(VersionNumber(s[1], s[2]), VersionNumber(s[1], s[2]+1))
Base.convert(::Type{VersionSet}, s::VersionSpec{3}) =
	VersionSet(VersionNumber(s[1], s[2], s[3]), VersionNumber(s[1], s[2], s[3]+1))
Base.convert(::Type{VersionSet}, v::VersionNumber) =
	VersionSet(v, Base.nextpatch(v))

end # module
