# This file is a part of Julia. License is MIT: https://julialang.org/license

module CLL
# Also see `work-stealing-queue.h` this is a pure Julia re-implementation

# =======
# Chase and Lev's work-stealing queue, optimized for
# weak memory models by Le et al.
#
# * Chase D., Lev Y. Dynamic Circular Work-Stealing queue
# * Le N. M. et al. Correct and Efficient Work-Stealing for
#   Weak Memory Models
# =======
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

function Base.copyto!(dst::WSBuffer{T}, src::WSBuffer{T}) where T
    @assert dst.capacity >= src.capacity
    for i in eachindex(src.buffer)
       @inbounds @atomic :monotonic dst.buffer[i] = src.buffer[i]
    end
end

"""
    WSQueue{T}

Work-stealing queue after Chase & Le.

!!! note
    popfirst! and push! are only allowed to be called from owner.
"""
mutable struct WSQueue{T}
    @atomic top::Int64
    @atomic bottom::Int64
    @atomic buffer::WSBuffer{T}
    function WSQueue{T}(capacity = 64) where T
        new(1, 1, WSBuffer{T}(capacity))
    end
end

# pushBottom
function Base.push!(q::WSQueue{T}, v::T) where T
    bottom = @atomic :monotonic q.bottom
    top    = @atomic :acquire   q.top
    buffer = @atomic :monotonic q.buffer

    size = bottom - top
    if __unlikely(size > (buffer.capacity - 1)) # Chase-Lev has size >= (buf.capacity - 1) || Le has size > (buf.capacity - 1)
        new_buffer = WSBuffer{T}(2*buffer.capacity)
        copyto!(new_buffer, buffer) # TODO only copy active range?
        @atomic :release q.buffer = new_buffer
        buffer = new_buffer # Le does buffer = @atomic :monotonic q.buffer
    end
    @atomic :monotonic buffer[bottom] = v
    Core.Intrinsics.atomic_fence(:release)
    @atomic :monotonic q.bottom = bottom + 1
    return nothing
end

# popBottom / take
function Base.popfirst!(q::WSQueue{T}) where T
    bottom = (@atomic :monotonic q.bottom) - 1
    buffer =  @atomic :monotonic q.buffer
    @atomic :monotonic q.bottom = bottom

    Core.Intrinsics.atomic_fence(:sequentially_consistent) # TODO slow on AMD

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
    top = @atomic :acquire q.top
    Core.Intrinsics.atomic_fence(:sequentially_consistent)
    bottom = @atomic :acquire q.bottom
    size = bottom - top
    if size > 0
        # Non-empty queue
        buffer = @atomic :acquire q.buffer # consume in Le
        v = @atomic :monotonic buffer[top]
        _, success = @atomicreplace :sequentially_consistent :monotonic q.top top => top + 1
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
Base.isempty(q::WSQueue) = q.top == q.bottom

const Queue = WSQueue

end #module
