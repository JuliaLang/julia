.. _devdocs-subarrays:

.. currentmodule:: Base

**************************
Bounds checking
**************************

Like many modern programming languages, Julia uses bounds checking to ensure
program safety when accessing arrays. In tight inner loops or other performance
critical situations, you may wish to skip these bounds checks to improve runtime
performance. For instance, in order to emit vectorized (SIMD) instructions, your
loop body cannot contain branches, and thus cannot contain bounds checks.
Consequently, Julia includes an ``@inbounds(...)`` macro to tell the compiler to
skip such bounds checks within the given block. For the built-in ``Array`` type,
the magic happens inside the ``arrayref`` and ``arrayset`` intrinsics.
User-defined array types instead use the ``@boundscheck(...)`` macro to achieve
context-sensitive code selection.

Eliding bounds checks
---------------------

The ``@boundscheck(...)`` macro marks blocks of code that perform bounds
checking. When such blocks appear inside of an ``@inbounds(...)`` block, the
compiler removes these blocks. When the ``@boundscheck(...)`` is nested inside
of a calling function containing an ``@inbounds(...)``, the compiler will remove
the ``@boundscheck`` block *only if it is inlined* into the calling function.
For example, you might write the method ``sum`` as::

    function sum(A::AbstractArray)
        r = zero(eltype(A))
        for i = 1:length(A)
            @inbounds r += A[i]
        end
        return r
    end

With a custom array-like type ``MyArray`` having::

    @inline getindex(A::MyArray, i::Real) = (@boundscheck checkbounds(A,i); A.data[to_index(i)])

Then when ``getindex`` is inlined into ``sum``, the call to ``checkbounds(A,i)``
will be elided. If your function contains multiple layers of inlining, only
``@boundscheck`` blocks at most one level of inlining deeper are eliminated. The
rule prevents unintended changes in program behavior from code further up the
stack.


Propagating inbounds
--------------------

There may be certain scenarios where for code-organization reasons you want more
than one layer between the ``@inbounds`` and ``@boundscheck`` declarations. For
instance, the default ``getindex`` methods have the chain
``getindex(A::AbstractArray, i::Real)`` calls
``getindex(linearindexing(A), A, i)`` calls
``_getindex(::LinearFast, A, i)``.

To override the "one layer of inlining" rule, a function may be marked with
``@propagate_inbounds`` to propagate an inbounds context (or out of bounds
context) through one additional layer of inlining.
