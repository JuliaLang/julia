# NOTE: ryu/Ryu.jl: neededdigits(::Type{Float64}) = 309 + 17
const PRINT_SCRATCH_LENGTH = 309 + 17
struct ScratchBuf
    data::NTuple{PRINT_SCRATCH_LENGTH,UInt8}

    ScratchBuf(::UndefInitializer) = new()
end

struct Scratch
    p::Ptr{UInt8}
    length::UInt64
end

function with_scratch(f, n)
    if n <= PRINT_SCRATCH_LENGTH
        buf = Ref(ScratchBuf(undef))
        GC.@preserve buf f(Scratch(pointer_from_objref(buf), unsafe_trunc(UInt64, n)))
    else
        tls = task_local_storage()
        buf = get!(tls, :PRINT_SCRATCH) do
            Vector{UInt8}(undef, n)
        end::Vector{UInt8}
        resize!(buf, n)
        GC.@preserve buf f(Scratch(pointer(buf), length(buf)))
    end
end

@propagate_inbounds function setindex!(a::Scratch, v, i)
    @boundscheck (1 <= i <= a.length || throw(BoundsError(a, i)))
    unsafe_store!(a.p, convert(UInt8, v), i)
    a
end

@propagate_inbounds function getindex(a::Scratch, i)
    @boundscheck (1 <= i <= a.length || throw(BoundsError(a, i)))
    unsafe_load(a.p, i)
end

@propagate_inbounds function view(a::Scratch, index::UnitRange)
    @boundscheck ((index.start > 0 && index.stop <= a.length) || throw(BoundsError(a, index)))
    index.start > index.stop && return Scratch(Ptr{UInt8}(), 0)
    Scratch(a.p + index.start - 1, unsafe_trunc(UInt64, index.stop - index.start + 1))
end

pointer(a::Scratch) = a.p
pointer(a::Scratch, pos) = a.p + pos - 1
length(a::Scratch) = a.length
@inline function unsafe_copyto!(dst::Scratch, dst_pos, src::Vector{UInt8}, src_pos, N)
    GC.@preserve dst, src, begin
        unsafe_copyto!(pointer(dst, dst_pos), pointer(src, src_pos), N)
    end
end

write(io::IO, a::Scratch) = unsafe_write(io, a.p, a.length)

String(s::Scratch) = unsafe_string(s.p, s.length)
