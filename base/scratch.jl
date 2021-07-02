const PRINT_SCRATCH_LENGTH = 256
struct ScratchBuf
    data::NTuple{PRINT_SCRATCH_LENGTH,UInt8}

    ScratchBuf(::UndefInitializer) = new()
end

struct Scratch
    p::Ptr{UInt8}
    length::UInt64
end

function with_scratch(f, n)
    buf = Ref(ScratchBuf(undef))
    GC.@preserve buf f(Scratch(pointer_from_objref(buf), unsafe_trunc(UInt64, n)))
end

@propagate_inbounds function setindex!(a::Scratch, v, i)
    @boundscheck (i <= a.length || throw(BoundsError(a, i)))
    unsafe_store!(a.p, convert(UInt8, v), i)
    a
end

write(io::IO, a::Scratch) = unsafe_write(io, a.p, a.length)

String(s::Scratch) = unsafe_string(s.p, s.length)
