# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Experimental multithreading support.
"""
module Threads

include("threadingconstructs.jl")
include("atomics.jl")
include("locks.jl")

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

"""
	set_nthreads!(A, origarray::AbstracVector)
Similar to `resize_nthreads!` but makes a deepcopy per value of the origin array.

If `length(origarray)` is not equal to [`nthreads()`](@ref) it throws an error.
This function in intended to be used to allocate `origarray` values per-thread,
and should be called at `__init__` if `A` is a global constant.
"""
function set_nthreads!(A::AbstractVector, origarray::AbstractVector)
	nthr = nthreads()
	nthr > length(origarray) && throw(ArgumentError("Origin Array must have at least nthreads() elements!"))
    resize!(A, nthr)
	@threads for i = 1:nthr
		A[i] = deepcopy(origarray[i])
	end
	return A
end

end
