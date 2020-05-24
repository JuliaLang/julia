struct PkgId
    uuid::Union{UUID,Nothing}
    name::String

    PkgId(u::UUID, name::AbstractString) = new(UInt128(u) == 0 ? nothing : u, name)
    PkgId(::Nothing, name::AbstractString) = new(nothing, name)
end
PkgId(name::AbstractString) = PkgId(nothing, name)

function PkgId(m::Module, name::String = String(nameof(moduleroot(m))))
    uuid = UUID(ccall(:jl_module_uuid, NTuple{2, UInt64}, (Any,), m))
    UInt128(uuid) == 0 ? PkgId(name) : PkgId(uuid, name)
end

==(a::PkgId, b::PkgId) = a.uuid == b.uuid && a.name == b.name

function hash(pkg::PkgId, h::UInt)
    h += 0xc9f248583a0ca36c % UInt
    h = hash(pkg.uuid, h)
    h = hash(pkg.name, h)
    return h
end

show(io::IO, pkg::PkgId) =
    print(io, pkg.name, " [", pkg.uuid === nothing ? "top-level" : pkg.uuid, "]")

function binpack(pkg::PkgId)
    io = IOBuffer()
    write(io, UInt8(0))
    uuid = pkg.uuid
    write(io, uuid === nothing ? UInt128(0) : UInt128(uuid))
    write(io, pkg.name)
    return String(take!(io))
end

function binunpack(s::String)
    io = IOBuffer(s)
    @assert read(io, UInt8) === 0x00
    uuid = read(io, UInt128)
    name = read(io, String)
    return PkgId(UUID(uuid), name)
end

