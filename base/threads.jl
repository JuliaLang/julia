# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Experimental multithreading support.
"""
module Threads

global Condition # we'll define this later, make sure we don't import Base.Condition

if Base.DISABLE_THREADS
    threadid() = 1
    nthreads() = 1
else
    include("threadingconstructs.jl")
    include("atomics.jl")
    include("locks-mt.jl")
end

"""
    resize_nthreads!(A, copyvalue=A[1])

Resize the array `A` to length [`nthreads()`](@ref).   Any new
elements that are allocated are initialized to `deepcopy(copyvalue)`,
where `copyvalue` defaults to `A[1]`.

This is typically used to allocate per-thread variables, and
should be called in `__init__` if `A` is a global constant.
"""
function resize_nthreads!(A::AbstractVector, copyvalue=A[1])
    nthr = nthreads()
    nold = length(A)
    resize!(A, nthr)
    for i = nold+1:nthr
        A[i] = deepcopy(copyvalue)
    end
    return A
end

end
