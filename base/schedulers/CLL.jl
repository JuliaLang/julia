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

function Base.push!(q::WSQueue{T}, v::T) where T
    bottom = @atomic :monotonic q.bottom
    top    = @atomic :acquire   q.top
    buffer = @atomic :monotonic q.buffer

    # add unlikely
    if __unlikely(bottom - top > (buffer.capacity - 1))
        # @debug "Growing WS buffer" bottom top capacity = buffer.capacity
        new_buffer = WSBuffer{T}(2*buffer.capacity)
        copyto!(new_buffer, buffer) # TODO only copy active range?
        @atomic :release q.buffer = new_buffer
        buffer = new_buffer
    end
    # @show bottom
    @atomic :monotonic buffer[bottom] = v
    Core.Intrinsics.atomic_fence(:release)
    @atomic :monotonic q.bottom = bottom + 1
    return nothing
end

function Base.popfirst!(q::WSQueue{T}) where T
    bottom = (@atomic :monotonic q.bottom) - 1
    buffer =  @atomic :monotonic q.buffer
    @atomic :monotonic q.bottom = bottom

    Core.Intrinsics.atomic_fence(:sequentially_consistent) # TODO slow on AMD

    top = @atomic :monotonic q.top

    if __likely(top <= bottom)
        v = @atomic :monotonic buffer[bottom]
        if top == bottom
            _, success = @atomicreplace q.top top => top+1
            @atomic :monotonic q.bottom = bottom + 1
            if !success
                return nothing # failed
            end
        end
        return v
    else
        @atomic :monotonic q.bottom = bottom + 1
        return nothing # failed
    end
end

function steal!(q::WSQueue{T}) where T
    top    = @atomic :acquire q.top
    Core.Intrinsics.atomic_fence(:sequentially_consistent)
    bottom = @atomic :acquire q.bottom
    if top < bottom
        buffer = @atomic :monotonic q.buffer
        v = @atomic :monotonic buffer[top]
        _, success = @atomicreplace q.top top => top+1
        if !success
            return nothing # failed
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
