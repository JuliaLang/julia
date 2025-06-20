# This file is a part of Julia. License is MIT: https://julialang.org/license

# Also see `work-stealing-queue.h` this is a pure Julia re-implementation

# =======
# Chase and Lev's work-stealing queue, optimized for
# weak memory models by Le et al.
#
# * Chase D., Lev Y. Dynamic Circular Work-Stealing queue
# * Le N. M. et al. Correct and Efficient Work-Stealing for
#   Weak Memory Models
# =======

module CLL


if Sys.ARCH == :x86_64
    # https://github.com/llvm/llvm-project/pull/106555
    fence() = Base.llvmcall(
        (raw"""
        define void @fence() #0 {
        entry:
            tail call void asm sideeffect "lock orq $$0 , (%rsp)", ""(); should this have ~{memory}
            ret void
        }
        attributes #0 = { alwaysinline }
        """, "fence"), Nothing, Tuple{})
else
    fence() = Core.Intrinsics.atomic_fence(:sequentially_consistent)
end

# mutable so that we don't get a mutex in WSQueue
mutable struct WSBuffer{T}
    const buffer::AtomicMemory{T}
    const capacity::Int64
    const mask::Int64
    @noinline function WSBuffer{T}(capacity::Int64) where T
        if __unlikely(capacity == 0)
            throw(ArgumentError("Capacity can't be zero"))
        end
        if __unlikely(count_ones(capacity) != 1)
            throw(ArgumentError("Capacity must be a power of two"))
        end
        buffer = AtomicMemory{T}(undef, capacity)
        mask = capacity - 1
        return new(buffer, capacity, mask)
    end
end

function Base.getindex_atomic(buf::WSBuffer{T}, order::Symbol, idx::Int64) where T
    @inbounds Base.getindex_atomic(buf.buffer, order, ((idx - 1) & buf.mask) + 1)
end

function Base.setindex_atomic!(buf::WSBuffer{T}, order::Symbol, val::T, idx::Int64) where T
    @inbounds Base.setindex_atomic!(buf.buffer, order, val,((idx - 1) & buf.mask) + 1)
end

function Base.modifyindex_atomic!(buf::WSBuffer{T}, order::Symbol, op, val::T, idx::Int64) where T
    @inbounds Base.modifyindex_atomic!(buf.buffer, order, op, val, ((idx - 1) & buf.mask) + 1)
end

function Base.swapindex_atomic!(buf::WSBuffer{T}, order::Symbol, val::T, idx::Int64) where T
    @inbounds Base.swapindex_atomic!(buf.buffer, order, val, ((idx - 1) & buf.mask) + 1)
end

function Base.replaceindex_atomic!(buf::WSBuffer{T}, success_order::Symbol, fail_order::Symbol, expected::T, desired::T, idx::Int64) where T
    @inbounds Base.replaceindex_atomic!(buf.buffer, success_order, fail_order, expected, desired, ((idx - 1) & buf.mask) + 1)
end

function Base.copyto!(dst::WSBuffer{T}, src::WSBuffer{T}, top, bottom) where T
    # must use queue indexes. When the queue is in state top=3, bottom=18, capacity=16
    # the real index of element 18 in the queue is 2, after growing in the new buffer it must be 18
    @assert dst.capacity >= src.capacity
    @assert top <= bottom
    # TODO overflow of bottom?
    for i in top:bottom
        @atomic :monotonic dst[i] = @atomic :monotonic src[i]
    end
end

const CACHE_LINE=64 # hardware_destructive_interference


"""
    WSQueue{T}

Work-stealing queue after Chase & Le.

!!! note
    popfirst! and push! are only allowed to be called from owner.
"""
mutable struct WSQueue{T}
    @atomic top::Int64 # 8 bytes
    __align::NTuple{CACHE_LINE-sizeof(Int64), UInt8}
    @atomic bottom::Int64
    __align2::NTuple{CACHE_LINE-sizeof(Int64), UInt8}
    @atomic buffer::WSBuffer{T}
    function WSQueue{T}(capacity = 64) where T
        new(1, ntuple(Returns(UInt8(0)), Val(CACHE_LINE-sizeof(Int64))),
            1, ntuple(Returns(UInt8(0)), Val(CACHE_LINE-sizeof(Int64))),
            WSBuffer{T}(capacity))
    end
end
@assert Base.fieldoffset(WSQueue{Int64}, 1) == 0
@assert Base.fieldoffset(WSQueue{Int64}, 3) == CACHE_LINE
@assert Base.fieldoffset(WSQueue{Int64}, 5) == 2*CACHE_LINE

@noinline function grow!(q::WSQueue{T}, buffer, top, bottom) where T
    new_buffer = WSBuffer{T}(2*buffer.capacity)
    copyto!(new_buffer, buffer, top, bottom)
    @atomic :release q.buffer = new_buffer
    return new_buffer
end

# accessing q.buffer requires a GC frame :/

# pushBottom
function Base.push!(q::WSQueue{T}, v::T) where T
    bottom = @atomic :monotonic q.bottom
    top    = @atomic :acquire   q.top
    buffer = @atomic :monotonic q.buffer

    size = bottom-top
    if __unlikely(size > (buffer.capacity - 1)) # Chase-Lev has size >= (buf.capacity - 1) || Le has size > (buf.capacity - 1)
        buffer = grow!(q, buffer, top, bottom) # Le does buffer = @atomic :monotonic q.buffer
    end
    @atomic :monotonic buffer[bottom] = v
    fence()
    @atomic :monotonic q.bottom = bottom + 1
    return nothing
end

# popBottom / take
function Base.popfirst!(q::WSQueue{T}) where T
    bottom = (@atomic :monotonic q.bottom) - 1
    buffer =  @atomic :monotonic q.buffer
    @atomic :monotonic q.bottom = bottom
    fence()
    top = @atomic :monotonic q.top

    size = bottom - top + 1
    if __likely(size > 0)
        # Non-empty queue
        v = @atomic :monotonic buffer[bottom]
        if size == 1
            # Single last element in queue
            _, success = @atomicreplace :sequentially_consistent :monotonic q.top top => top + 1
            @atomic :monotonic q.bottom = bottom + 1
            if !success
                # Failed race
                return nothing
            end
        end
        return v
    else
        # Empty queue
        @atomic :monotonic q.bottom = bottom + 1
        return nothing
    end
end

function steal!(q::WSQueue{T}) where T
    top    = @atomic :acquire q.top
    fence()
    bottom = @atomic :acquire q.bottom
    size = bottom - top
    if __likely(size > 0)
        # Non-empty queue
        buffer = @atomic :acquire q.buffer # consume in Le
        v = @atomic :monotonic buffer[top]
        _, success = @atomicreplace :sequentially_consistent :monotonic q.top top => top+1
        if !success
            # Failed race
            return nothing
        end
        return v
    end
    return nothing # failed
end

Base.pop!(q::WSQueue{T}) where T = popfirst!(q)
@inline __likely(cond::Bool) = ccall("llvm.expect", llvmcall, Bool, (Bool, Bool), cond, true)
@inline __unlikely(cond::Bool) = ccall("llvm.expect", llvmcall, Bool, (Bool, Bool), cond, false)
Base.isempty(q::WSQueue) = (q.bottom - q.top) == 0

const Queue = WSQueue

end #module