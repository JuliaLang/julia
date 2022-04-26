# NOTE: ryu/Ryu.jl: neededdigits(::Type{Float64}) = 309 + 17
const SCRATCH_BUFFER_MIN_LENGTH = 309 + 17

struct ScratchBuffer
    p::Ptr{UInt8}
    length::UInt64
end

# NOTE: similar to pcre.jl, Threads module isn't available at the time of loading
_tid() = Int(ccall(:jl_threadid, Int16, ())) + 1
_nth() = Int(unsafe_load(cglobal(:jl_n_threads, Cint)))

const SCRATCH_BUFFERS = map(1:_nth()) do _
    Vector{UInt8}(undef, SCRATCH_BUFFER_MIN_LENGTH), false
end

function init_scratch_buffers()
    while length(SCRATCH_BUFFERS) < _nth()
        push!(SCRATCH_BUFFERS, (Vector{UInt8}(undef, SCRATCH_BUFFER_MIN_LENGTH), false))
    end
    nothing
end

function with_scratch_buffer(f, n::Int)
    buf, buf_inuse = @inbounds SCRATCH_BUFFERS[_tid()]
    if buf_inuse
        buf = Vector{UInt8}(undef, max(SCRATCH_BUFFER_MIN_LENGTH, n))
    else
        @inbounds SCRATCH_BUFFERS[_tid()] = (buf, true)
    end

    resize!(buf, n)
    # NOTE: we could just `f(buf)`, but String(buf) doesn't play well and causes allocation on the next `resize!(buf)`
    res = GC.@preserve buf f(ScratchBuffer(pointer(buf), length(buf)))

    # The intent is to reuse buffers when it's cheap and reallocate when synchronization/exception handling is required.
    # We'll "waste" a buffer when:
    # * task is migrated while `f()` is running
    # * exception is thrown from `f()`
    # * multiple tasks are using `with_scratch_buffer()` from the same thread (e.g. f() is blocking)
    # Those shouldn't happen often.
    # NOTE: _tid() could be different here if task migrated.
    @inbounds SCRATCH_BUFFERS[_tid()] = (buf, false)

    res
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
