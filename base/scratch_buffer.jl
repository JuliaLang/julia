# NOTE: ryu/Ryu.jl: neededdigits(::Type{Float64}) = 309 + 17
const SCRATCH_BUFFER_INLINE_LENGTH = 309 + 17
struct ScratchBufferInline
    data::NTuple{SCRATCH_BUFFER_INLINE_LENGTH,UInt8}

    ScratchBufferInline() = new()
end

struct ScratchBuffer
    p::Ptr{UInt8}
    length::UInt64
end

function with_scratch_buffer(f, n)
    if n <= SCRATCH_BUFFER_INLINE_LENGTH
        buf = Ref(ScratchBufferInline())
        GC.@preserve buf f(ScratchBuffer(pointer_from_objref(buf), unsafe_trunc(UInt64, n)))
    else
        tls = task_local_storage()
        buf = get!(tls, :SCRATCH_BUFFER) do
            Vector{UInt8}(undef, n)
        end::Vector{UInt8}
        resize!(buf, n)
        GC.@preserve buf f(ScratchBuffer(pointer(buf), length(buf)))
    end
end

@propagate_inbounds function setindex!(a::ScratchBuffer, v, i)
    @boundscheck (1 <= i <= a.length || throw(BoundsError(a, i)))
    unsafe_store!(a.p, convert(UInt8, v), i)
    a
end

@propagate_inbounds function getindex(a::ScratchBuffer, i)
    @boundscheck (1 <= i <= a.length || throw(BoundsError(a, i)))
    unsafe_load(a.p, i)
end

@propagate_inbounds function view(a::ScratchBuffer, index::UnitRange)
    @boundscheck ((index.start > 0 && index.stop <= a.length) || throw(BoundsError(a, index)))
    index.start > index.stop && return ScratchBuffer(Ptr{UInt8}(), 0)
    ScratchBuffer(a.p + index.start - 1, unsafe_trunc(UInt64, index.stop - index.start + 1))
end

pointer(a::ScratchBuffer) = a.p
pointer(a::ScratchBuffer, pos) = a.p + pos - 1
length(a::ScratchBuffer) = a.length
@inline function unsafe_copyto!(dst::ScratchBuffer, dst_pos, src::Vector{UInt8}, src_pos, N)
    GC.@preserve dst, src, begin
        unsafe_copyto!(pointer(dst, dst_pos), pointer(src, src_pos), N)
    end
end

write(io::IO, a::ScratchBuffer) = unsafe_write(io, a.p, a.length)

String(s::ScratchBuffer) = unsafe_string(s.p, s.length)
