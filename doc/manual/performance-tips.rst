.. _man-performance-tips:

.. currentmodule:: Base

******************
 Performance Tips
******************

In the following sections, we briefly go through a few techniques that
can help make your Julia code run as fast as possible.

Avoid global variables
----------------------

A global variable might have its value, and therefore its type, change
at any point. This makes it difficult for the compiler to optimize code
using global variables. Variables should be local, or passed as
arguments to functions, whenever possible.

Any code that is performance critical or being benchmarked should be
inside a function.

We find that global names are frequently constants, and declaring them
as such greatly improves performance::

    const DEFAULT_VAL = 0

Uses of non-constant globals can be optimized by annotating their types
at the point of use::

    global x
    y = f(x::Int + 1)

Writing functions is better style. It leads to more reusable code and
clarifies what steps are being done, and what their inputs and outputs
are.

**NOTE:**  All code in the REPL is evaluated in global scope, so a variable
defined and assigned at toplevel will be a **global** variable.

In the following REPL session::

    julia> x = 1.0

is equivalent to::

    julia> global x = 1.0

so all the performance issues discussed previously apply.

Measure performance with :obj:`@time` and pay attention to memory allocation
----------------------------------------------------------------------------

The most useful tool for measuring performance is the :obj:`@time` macro.
The following example illustrates good working style::

    julia> function f(n)
               s = 0
               for i = 1:n
                   s += i/2
               end
               s
           end
    f (generic function with 1 method)

    julia> @time f(1)
    elapsed time: 0.004710563 seconds (93504 bytes allocated)
    0.5

    julia> @time f(10^6)
    elapsed time: 0.04123202 seconds (32002136 bytes allocated)
    2.5000025e11

On the first call (``@time f(1)``), ``f`` gets compiled.  (If you've
not yet used :obj:`@time` in this session, it will also compile functions
needed for timing.)  You should not take the results of this run
seriously. For the second run, note that in addition to reporting the
time, it also indicated that a large amount of memory was allocated.
This is the single biggest advantage of :obj:`@time` vs. functions like
:func:`tic` and :func:`toc`, which only report time.

Unexpected memory allocation is almost always a sign of some problem
with your code, usually a problem with type-stability. Consequently,
in addition to the allocation itself, it's very likely that the code
generated for your function is far from optimal. Take such indications
seriously and follow the advice below.

As a teaser, note that an improved version of this function allocates
no memory (except to pass back the result back to the REPL) and has
an order of magnitude faster execution after the first call::

    julia> @time f_improved(1)   # first call
    elapsed time: 0.003702172 seconds (78944 bytes allocated)
    0.5

    julia> @time f_improved(10^6)
    elapsed time: 0.004313644 seconds (112 bytes allocated)
    2.5000025e11

Below you'll learn how to spot the problem with ``f`` and how to fix it.

In some situations, your function may need to allocate memory as part
of its operation, and this can complicate the simple picture above. In
such cases, consider using one of the :ref:`tools
<man-performance-tools>` below to diagnose problems, or write a
version of your function that separates allocation from its
algorithmic aspects (see :ref:`man-preallocation`).


.. _man-performance-tools:

Tools
-----

Julia and its package ecosystem includes tools that may help you
diagnose problems and improve the performance of your code:

- :ref:`stdlib-profiling` allows you to measure the performance of
  your running code and identify lines that serve as bottlenecks.  For
  complex projects, the `ProfileView
  <https://github.com/timholy/ProfileView.jl>`_ package can help you
  visualize your profiling results.

- Unexpectedly-large memory allocations---as reported by :obj:`@time`,
  :obj:`@allocated`, or the profiler (through calls to the
  garbage-collection routines)---hint that there might be issues with
  your code.  If you don't see another reason for the allocations,
  suspect a type problem.  You can also start Julia with the
  ``--track-allocation=user`` option and examine the resulting
  ``*.mem`` files to see information about where those allocations
  occur.  See :ref:`man-track-allocation`.

- ``@code_warntype`` generates a representation of your code that can
  be helpful in finding expressions that result in type uncertainty.
  See :ref:`man-code-warntype` below.

- The `Lint <https://github.com/tonyhffong/Lint.jl>`_ and `TypeCheck
  <https://github.com/astrieanna/TypeCheck.jl>`_ packages can also
  warn you of certain types of programming errors.



Avoid containers with abstract type parameters
----------------------------------------------

When working with parameterized types, including arrays, it is best to
avoid parameterizing with abstract types where possible.

Consider the following::

    a = Real[]    # typeof(a) = Array{Real,1}
    if (f = rand()) < .8
        push!(a, f)
    end

Because ``a`` is a an array of abstract type :class:`Real`, it must be able
to hold any Real value.  Since :class:`Real` objects can be of arbitrary
size and structure, ``a`` must be represented as an array of pointers to
individually allocated :class:`Real` objects.  Because ``f`` will always be
a :class:`Float64`, we should instead, use::

    a = Float64[] # typeof(a) = Array{Float64,1}

which will create a contiguous block of 64-bit floating-point values
that can be manipulated efficiently.

See also the discussion under :ref:`man-parametric-types`.





Type declarations
-----------------

In many languages with optional type declarations, adding declarations
is the principal way to make code run faster. This is *not* the case
in Julia. In Julia, the compiler generally knows the types of all function
arguments, local variables, and expressions.
However, there are a few specific instances where declarations are
helpful.

.. _man-abstract-fields:
Avoid fields with abstract type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Types can be declared without specifying the types of their fields:

.. doctest::

    julia> type MyAmbiguousType
               a
           end

This allows ``a`` to be of any type. This can often be useful, but it
does have a downside: for objects of type ``MyAmbiguousType``, the
compiler will not be able to generate high-performance code.  The
reason is that the compiler uses the types of objects, not their
values, to determine how to build code. Unfortunately, very little can
be inferred about an object of type ``MyAmbiguousType``:

.. doctest::

    julia> b = MyAmbiguousType("Hello")
    MyAmbiguousType("Hello")

    julia> c = MyAmbiguousType(17)
    MyAmbiguousType(17)

    julia> typeof(b)
    MyAmbiguousType

    julia> typeof(c)
    MyAmbiguousType

``b`` and ``c`` have the same type, yet their underlying
representation of data in memory is very different. Even if you stored
just numeric values in field ``a``, the fact that the memory
representation of a ``UInt8`` differs from a ``Float64`` also means
that the CPU needs to handle them using two different kinds of
instructions.  Since the required information is not available in the
type, such decisions have to be made at run-time. This slows
performance.

You can do better by declaring the type of ``a``. Here, we are focused
on the case where ``a`` might be any one of several types, in which
case the natural solution is to use parameters. For example:

.. doctest::

    julia> type MyType{T<:AbstractFloat}
             a::T
           end

This is a better choice than

.. doctest::

    julia> type MyStillAmbiguousType
             a::AbstractFloat
           end

because the first version specifies the type of ``a`` from the type of
the wrapper object.  For example:

.. doctest::

    julia> m = MyType(3.2)
    MyType{Float64}(3.2)

    julia> t = MyStillAmbiguousType(3.2)
    MyStillAmbiguousType(3.2)

    julia> typeof(m)
    MyType{Float64}

    julia> typeof(t)
    MyStillAmbiguousType

The type of field ``a`` can be readily determined from the type of
``m``, but not from the type of ``t``.  Indeed, in ``t`` it's possible
to change the type of field ``a``:

.. doctest::

    julia> typeof(t.a)
    Float64

    julia> t.a = 4.5f0
    4.5f0

    julia> typeof(t.a)
    Float32

In contrast, once ``m`` is constructed, the type of ``m.a`` cannot
change:

.. doctest::

    julia> m.a = 4.5f0
    4.5

    julia> typeof(m.a)
    Float64

The fact that the type of ``m.a`` is known from ``m``'s type---coupled
with the fact that its type cannot change mid-function---allows the
compiler to generate highly-optimized code for objects like ``m`` but
not for objects like ``t``.

Of course, all of this is true only if we construct ``m`` with a
concrete type.  We can break this by explicitly constructing it with
an abstract type:

.. doctest::

    julia> m = MyType{AbstractFloat}(3.2)
    MyType{AbstractFloat}(3.2)

    julia> typeof(m.a)
    Float64

    julia> m.a = 4.5f0
    4.5f0

    julia> typeof(m.a)
    Float32

For all practical purposes, such objects behave identically to those
of ``MyStillAmbiguousType``.

It's quite instructive to compare the sheer amount code generated for
a simple function
::

    func(m::MyType) = m.a+1

using
::

    code_llvm(func,(MyType{Float64},))
    code_llvm(func,(MyType{AbstractFloat},))
    code_llvm(func,(MyType,))

For reasons of length the results are not shown here, but you may wish
to try this yourself. Because the type is fully-specified in the first
case, the compiler doesn't need to generate any code to resolve the
type at run-time.  This results in shorter and faster code.


.. _man-abstract-container-type:

Avoid fields with abstract containers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The same best practices also work for container types:

.. doctest::

    julia> type MySimpleContainer{A<:AbstractVector}
             a::A
           end

    julia> type MyAmbiguousContainer{T}
             a::AbstractVector{T}
           end

For example:

.. doctest::

    julia> c = MySimpleContainer(1:3);

    julia> typeof(c)
    MySimpleContainer{UnitRange{Int64}}

    julia> c = MySimpleContainer([1:3;]);

    julia> typeof(c)
    MySimpleContainer{Array{Int64,1}}

    julia> b = MyAmbiguousContainer(1:3);

    julia> typeof(b)
    MyAmbiguousContainer{Int64}

    julia> b = MyAmbiguousContainer([1:3;]);

    julia> typeof(b)
    MyAmbiguousContainer{Int64}

For ``MySimpleContainer``, the object is fully-specified by its type
and parameters, so the compiler can generate optimized functions. In
most instances, this will probably suffice.

While the compiler can now do its job perfectly well, there are cases
where *you* might wish that your code could do different things
depending on the *element type* of ``a``.  Usually the best way to
achieve this is to wrap your specific operation (here, ``foo``) in a
separate function::

    function sumfoo(c::MySimpleContainer)
        s = 0
    for x in c.a
        s += foo(x)
    end
    s
    end

    foo(x::Integer) = x
    foo(x::AbstractFloat) = round(x)

This keeps things simple, while allowing the compiler to generate
optimized code in all cases.

However, there are cases where you may need to declare different
versions of the outer function for different element types of
``a``. You could do it like this::

    function myfun{T<:AbstractFloat}(c::MySimpleContainer{Vector{T}})
        ...
    end
    function myfun{T<:Integer}(c::MySimpleContainer{Vector{T}})
        ...
    end

This works fine for ``Vector{T}``, but we'd also have to write
explicit versions for ``UnitRange{T}`` or other abstract types. To
prevent such tedium, you can use two parameters in the declaration of
``MyContainer``::

    type MyContainer{T, A<:AbstractVector}
        a::A
    end
    MyContainer(v::AbstractVector) = MyContainer{eltype(v), typeof(v)}(v)

    julia> b = MyContainer(1.3:5);

    julia> typeof(b)
    MyContainer{Float64,UnitRange{Float64}}

Note the somewhat surprising fact that ``T`` doesn't appear in the
declaration of field ``a``, a point that we'll return to in a moment.
With this approach, one can write functions such as::

    function myfunc{T<:Integer, A<:AbstractArray}(c::MyContainer{T,A})
        return c.a[1]+1
    end
    # Note: because we can only define MyContainer for
    # A<:AbstractArray, and any unspecified parameters are arbitrary,
    # the previous could have been written more succinctly as
    #     function myfunc{T<:Integer}(c::MyContainer{T})

    function myfunc{T<:AbstractFloat}(c::MyContainer{T})
        return c.a[1]+2
    end

    function myfunc{T<:Integer}(c::MyContainer{T,Vector{T}})
        return c.a[1]+3
    end

    julia> myfunc(MyContainer(1:3))
    2

    julia> myfunc(MyContainer(1.0:3))
    3.0

    julia> myfunc(MyContainer([1:3]))
    4

As you can see, with this approach it's possible to specialize on both
the element type ``T`` and the array type ``A``.

However, there's one remaining hole: we haven't enforced that ``A``
has element type ``T``, so it's perfectly possible to construct an
object like this::

  julia> b = MyContainer{Int64, UnitRange{Float64}}(1.3:5);

  julia> typeof(b)
  MyContainer{Int64,UnitRange{Float64}}

To prevent this, we can add an inner constructor::

    type MyBetterContainer{T<:Real, A<:AbstractVector}
        a::A

        MyBetterContainer(v::AbstractVector{T}) = new(v)
    end
    MyBetterContainer(v::AbstractVector) = MyBetterContainer{eltype(v),typeof(v)}(v)


    julia> b = MyBetterContainer(1.3:5);

    julia> typeof(b)
    MyBetterContainer{Float64,UnitRange{Float64}}

    julia> b = MyBetterContainer{Int64, UnitRange{Float64}}(1.3:5);
    ERROR: no method MyBetterContainer(UnitRange{Float64},)

The inner constructor requires that the element type of ``A`` be ``T``.

Annotate values taken from untyped locations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is often convenient to work with data structures that may contain
values of any type, such as cell
arrays (arrays of type ``Array{Any}``). But, if you're using one of
these structures and happen to know the type of an element, it helps to
share this knowledge with the compiler::

    function foo(a::Array{Any,1})
        x = a[1]::Int32
        b = x+1
        ...
    end

Here, we happened to know that the first element of ``a`` would be an
:class:`Int32`. Making an annotation like this has the added benefit that it
will raise a run-time error if the value is not of the expected type,
potentially catching certain bugs earlier.

Declare types of keyword arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Keyword arguments can have declared types::

    function with_keyword(x; name::Int = 1)
        ...
    end

Functions are specialized on the types of keyword arguments, so these
declarations will not affect performance of code inside the function.
However, they will reduce the overhead of calls to the function that
include keyword arguments.

Functions with keyword arguments have near-zero overhead for call sites
that pass only positional arguments.

Passing dynamic lists of keyword arguments, as in ``f(x; keywords...)``,
can be slow and should be avoided in performance-sensitive code.

Break functions into multiple definitions
-----------------------------------------

Writing a function as many small definitions allows the compiler to
directly call the most applicable code, or even inline it.

Here is an example of a "compound function" that should really be
written as multiple definitions::

    function norm(A)
        if isa(A, Vector)
            return sqrt(real(dot(A,A)))
        elseif isa(A, Matrix)
            return max(svd(A)[2])
        else
            error("norm: invalid argument")
        end
    end

This can be written more concisely and efficiently as::

    norm(x::Vector) = sqrt(real(dot(x,x)))
    norm(A::Matrix) = max(svd(A)[2])

Write "type-stable" functions
-----------------------------

When possible, it helps to ensure that a function always returns a value
of the same type. Consider the following definition::

    pos(x) = x < 0 ? 0 : x

Although this seems innocent enough, the problem is that ``0`` is an
integer (of type :class:`Int`) and ``x`` might be of any type. Thus,
depending on the value of ``x``, this function might return a value of
either of two types. This behavior is allowed, and may be desirable in
some cases. But it can easily be fixed as follows::

    pos(x) = x < 0 ? zero(x) : x

There is also a :func:`one` function, and a more general :func:`oftype(x,y) <oftype>`
function, which returns ``y`` converted to the type of ``x``.

Avoid changing the type of a variable
-------------------------------------

An analogous "type-stability" problem exists for variables used
repeatedly within a function::

    function foo()
        x = 1
        for i = 1:10
            x = x/bar()
        end
        return x
    end

Local variable ``x`` starts as an integer, and after one loop iteration
becomes a floating-point number (the result of :obj:`/` operator). This
makes it more difficult for the compiler to optimize the body of the
loop. There are several possible fixes:

-  Initialize ``x`` with ``x = 1.0``
-  Declare the type of ``x``: ``x::Float64 = 1``
-  Use an explicit conversion: ``x = one(T)``

Separate kernel functions
-------------------------

Many functions follow a pattern of performing some set-up work, and then
running many iterations to perform a core computation. Where possible,
it is a good idea to put these core computations in separate functions.
For example, the following contrived function returns an array of a
randomly-chosen type::

    function strange_twos(n)
        a = Array(rand(Bool) ? Int64 : Float64, n)
        for i = 1:n
            a[i] = 2
        end
        return a
    end

This should be written as::

    function fill_twos!(a)
        for i=1:length(a)
            a[i] = 2
        end
    end

    function strange_twos(n)
        a = Array(rand(Bool) ? Int64 : Float64, n)
        fill_twos!(a)
        return a
    end

Julia's compiler specializes code for argument types at function
boundaries, so in the original implementation it does not know the type
of ``a`` during the loop (since it is chosen randomly). Therefore the
second version is generally faster since the inner loop can be
recompiled as part of ``fill_twos!`` for different types of ``a``.

The second form is also often better style and can lead to more code
reuse.

This pattern is used in several places in the standard library. For
example, see ``hvcat_fill`` in
`abstractarray.jl <https://github.com/JuliaLang/julia/blob/master/base/abstractarray.jl>`_,
or the ``fill!`` function, which we could have used instead of writing
our own ``fill_twos!``.

Functions like ``strange_twos`` occur when dealing with data of
uncertain type, for example data loaded from an input file that might
contain either integers, floats, strings, or something else.

Access arrays in memory order, along columns
--------------------------------------------

Multidimensional arrays in Julia are stored in column-major order. This
means that arrays are stacked one column at a time. This can be verified
using the ``vec`` function or the syntax ``[:]`` as shown below (notice
that the array is ordered ``[1 3 2 4]``, not ``[1 2 3 4]``):

.. doctest::

    julia> x = [1 2; 3 4]
    2x2 Array{Int64,2}:
     1  2
     3  4

    julia> x[:]
    4-element Array{Int64,1}:
     1
     3
     2
     4

This convention for ordering arrays is common in many languages like
Fortran, Matlab, and R (to name a few). The alternative to column-major
ordering is row-major ordering, which is the convention adopted by C and
Python (``numpy``) among other languages. Remembering the ordering of
arrays can have significant performance effects when looping over
arrays. A rule of thumb to keep in mind is that with column-major
arrays, the first index changes most rapidly. Essentially this means
that looping will be faster if the inner-most loop index is the first to
appear in a slice expression.

Consider the following contrived example. Imagine we wanted to write a
function that accepts a :obj:`Vector` and returns a square :obj:`Matrix`
with either the rows or the columns filled with copies of the input
vector. Assume that it is not important whether rows or columns are
filled with these copies (perhaps the rest of the code can be easily
adapted accordingly). We could conceivably do this in at least four ways
(in addition to the recommended call to the built-in :func:`repmat`)::

    function copy_cols{T}(x::Vector{T})
        n = size(x, 1)
        out = Array(eltype(x), n, n)
        for i=1:n
            out[:, i] = x
        end
        out
    end

    function copy_rows{T}(x::Vector{T})
        n = size(x, 1)
        out = Array(eltype(x), n, n)
        for i=1:n
            out[i, :] = x
        end
        out
    end

    function copy_col_row{T}(x::Vector{T})
        n = size(x, 1)
        out = Array(T, n, n)
        for col=1:n, row=1:n
            out[row, col] = x[row]
        end
        out
    end

    function copy_row_col{T}(x::Vector{T})
        n = size(x, 1)
        out = Array(T, n, n)
        for row=1:n, col=1:n
            out[row, col] = x[col]
        end
        out
    end

Now we will time each of these functions using the same random ``10000``
by ``1`` input vector::

    julia> x = randn(10000);

    julia> fmt(f) = println(rpad(string(f)*": ", 14, ' '), @elapsed f(x))

    julia> map(fmt, Any[copy_cols, copy_rows, copy_col_row, copy_row_col]);
    copy_cols:    0.331706323
    copy_rows:    1.799009911
    copy_col_row: 0.415630047
    copy_row_col: 1.721531501

Notice that ``copy_cols`` is much faster than ``copy_rows``. This is
expected because ``copy_cols`` respects the column-based memory layout
of the :obj:`Matrix` and fills it one column at a time. Additionally,
``copy_col_row`` is much faster than ``copy_row_col`` because it follows
our rule of thumb that the first element to appear in a slice expression
should be coupled with the inner-most loop.

.. _man-preallocation:

Pre-allocating outputs
----------------------

If your function returns an Array or some other complex
type, it may have to allocate memory.  Unfortunately, oftentimes
allocation and its converse, garbage collection, are substantial
bottlenecks.

Sometimes you can circumvent the need to allocate memory on each
function call by preallocating the output.  As a
trivial example, compare
::

    function xinc(x)
        return [x, x+1, x+2]
    end

    function loopinc()
        y = 0
        for i = 1:10^7
            ret = xinc(i)
            y += ret[2]
        end
        y
    end

with
::

    function xinc!{T}(ret::AbstractVector{T}, x::T)
        ret[1] = x
        ret[2] = x+1
        ret[3] = x+2
        nothing
    end

    function loopinc_prealloc()
        ret = Array(Int, 3)
        y = 0
        for i = 1:10^7
            xinc!(ret, i)
            y += ret[2]
        end
        y
    end

Timing results::

    julia> @time loopinc()
    elapsed time: 1.955026528 seconds (1279975584 bytes allocated)
    50000015000000

    julia> @time loopinc_prealloc()
    elapsed time: 0.078639163 seconds (144 bytes allocated)
    50000015000000

Preallocation has other advantages, for example by allowing the
caller to control the "output" type from an algorithm.  In the example
above, we could have passed a :class:`SubArray` rather than an :class:`Array`,
had we so desired.

Taken to its extreme, pre-allocation can make your code uglier, so
performance measurements and some judgment may be required.


Avoid string interpolation for I/O
----------------------------------

When writing data to a file (or other I/O device), forming extra
intermediate strings is a source of overhead. Instead of::

    println(file, "$a $b")

use::

    println(file, a, " ", b)

The first version of the code forms a string, then writes it
to the file, while the second version writes values directly
to the file. Also notice that in some cases string interpolation can
be harder to read. Consider::

    println(file, "$(f(a))$(f(b))")

versus::

    println(file, f(a), f(b))


Optimize network I/O during parallel execution
----------------------------------------------

When executing a remote function in parallel::

    responses = cell(nworkers())
    @sync begin
        for (idx, pid) in enumerate(workers())
            @async responses[idx] = remotecall_fetch(pid, foo, args...)
        end
    end

is faster than::

    refs = cell(nworkers())
    for (idx, pid) in enumerate(workers())
        refs[idx] = @spawnat pid foo(args...)
    end
    responses = [fetch(r) for r in refs]

The former results in a single network round-trip to every worker, while the
latter results in two network calls - first by the ``@spawnat`` and the
second due to the ``fetch`` (or even a ``wait``). The ``fetch``/``wait``
is also being executed serially resulting in an overall poorer performance.


Fix deprecation warnings
------------------------

A deprecated function internally performs a lookup in order to
print a relevant warning only once. This extra lookup can cause a
significant slowdown, so all uses of deprecated functions should be
modified as suggested by the warnings.

Tweaks
------

These are some minor points that might help in tight inner loops.

-  Avoid unnecessary arrays. For example, instead of :func:`sum([x,y,z]) <sum>`
   use ``x+y+z``.
-  Use :func:`abs2(z) <abs2>` instead of :func:`abs(z)^2 <abs>` for complex ``z``. In general,
   try to rewrite code to use :func:`abs2` instead of :func:`abs` for complex arguments.
-  Use :func:`div(x,y) <div>` for truncating division of integers instead of
   :func:`trunc(x/y) <trunc>`, :func:`fld(x,y) <fld>` instead of :func:`floor(x/y) <floor>`, and
   :func:`cld(x,y) <cld>` instead of :func:`ceil(x/y) <ceil>`.

Performance Annotations
-----------------------

Sometimes you can enable better optimization by promising certain program
properties.

-  Use :obj:`@inbounds` to eliminate array bounds checking within expressions.
   Be certain before doing this. If the subscripts are ever out of bounds,
   you may suffer crashes or silent corruption.
-  Use :obj:`@fastmath` to allow floating point optimizations that are
   correct for real numbers, but lead to differences for IEEE numbers.
   Be careful when doing this, as this may change numerical results.
   This corresponds to the ``-ffast-math`` option of clang.
-  Write :obj:`@simd` in front of ``for`` loops that are amenable to vectorization.
   **This feature is experimental** and could change or disappear in future
   versions of Julia.

Note: While :obj:`@simd` needs to be placed directly in front of a
loop, both :obj:`@inbounds` and :obj:`@fastmath` can be applied to
several statements at once, e.g. using ``begin`` ... ``end``, or even
to a whole function.

Here is an example with both :obj:`@inbounds` and :obj:`@simd` markup::

    function inner( x, y )
        s = zero(eltype(x))
        for i=1:length(x)
            @inbounds s += x[i]*y[i]
        end
        s
    end

    function innersimd( x, y )
        s = zero(eltype(x))
        @simd for i=1:length(x)
            @inbounds s += x[i]*y[i]
        end
        s
    end

    function timeit( n, reps )
        x = rand(Float32,n)
        y = rand(Float32,n)
        s = zero(Float64)
        time = @elapsed for j in 1:reps
            s+=inner(x,y)
        end
        println("GFlop        = ",2.0*n*reps/time*1E-9)
        time = @elapsed for j in 1:reps
            s+=innersimd(x,y)
        end
        println("GFlop (SIMD) = ",2.0*n*reps/time*1E-9)
    end

    timeit(1000,1000)

On a computer with a 2.4GHz Intel Core i5 processor, this produces::

    GFlop        = 1.9467069505224963
    GFlop (SIMD) = 17.578554163920018

The range for a ``@simd for`` loop should be a one-dimensional range.
A variable used for accumulating, such as ``s`` in the example, is called
a *reduction variable*. By using :obj:`@simd`, you are asserting several
properties of the loop:

-  It is safe to execute iterations in arbitrary or overlapping order,
   with special consideration for reduction variables.
-  Floating-point operations on reduction variables can be reordered,
   possibly causing different results than without :obj:`@simd`.
-  No iteration ever waits on another iteration to make forward progress.

A loop containing ``break``, ``continue``, or :obj:`@goto` will cause a
compile-time error.

Using :obj:`@simd` merely gives the compiler license to vectorize. Whether
it actually does so depends on the compiler. To actually benefit from the
current implementation, your loop should have the following additional
properties:

-  The loop must be an innermost loop.
-  The loop body must be straight-line code. This is why :obj:`@inbounds` is
   currently needed for all array accesses. The compiler can sometimes turn
   short ``&&``, ``||``, and ``?:`` expressions into straight-line code,
   if it is safe to evaluate all operands unconditionally. Consider using
   :func:`ifelse` instead of ``?:`` in the loop if it is safe to do so.
-  Accesses must have a stride pattern and cannot be "gathers" (random-index reads)
   or "scatters" (random-index writes).
-  The stride should be unit stride.
-  In some simple cases, for example with 2-3 arrays accessed in a loop, the
   LLVM auto-vectorization may kick in automatically, leading to no further
   speedup with :obj:`@simd`.

Here is an example with all three kinds of markup. This program first
calculates the finite difference of a one-dimensional array, and then
evaluates the L2-norm of the result::

    function init!(u)
        n = length(u)
        dx = 1.0 / (n-1)
        @fastmath @inbounds @simd for i in 1:n
            u[i] = sin(2pi*dx*i)
        end
    end

    function deriv!(u, du)
        n = length(u)
        dx = 1.0 / (n-1)
        @fastmath @inbounds du[1] = (u[2] - u[1]) / dx
        @fastmath @inbounds @simd for i in 2:n-1
            du[i] = (u[i+1] - u[i-1]) / (2*dx)
        end
        @fastmath @inbounds du[n] = (u[n] - u[n-1]) / dx
    end

    function norm(u)
        n = length(u)
        T = eltype(u)
        s = zero(T)
        @fastmath @inbounds @simd for i in 1:n
            s += u[i]^2
        end
        @fastmath @inbounds return sqrt(s/n)
    end

    function main()
        n = 2000
        u = Array(Float64, n)
        init!(u)
        du = similar(u)

        deriv!(u, du)
        nu = norm(du)

        @time for i in 1:10^6
            deriv!(u, du)
            nu = norm(du)
        end

        println(nu)
    end

    main()

On a computer with a 2.7 GHz Intel Core i7 processor, this produces::

    $ julia wave.jl
    elapsed time: 1.207814709 seconds (0 bytes allocated)
    4.443986180758243

    $ julia --math-mode=ieee wave.jl
    elapsed time: 4.487083643 seconds (0 bytes allocated)
    4.443986180758243

Here, the option ``--math-mode=ieee`` disables the :obj:`@fastmath`
macro, so that we can compare results.

In this case, the speedup due to :obj:`@fastmath` is a factor of about
3.7. This is unusually large -- in general, the speedup will be
smaller. (In this particular example, the working set of the benchmark
is small enough to fit into the L1 cache of the processor, so that
memory access latency does not play a role, and computing time is
dominated by CPU usage. In many real world programs this is not the
case.) Also, in this case this optimization does not change the result
-- in general, the result will be slightly different. In some cases,
especially for numerically unstable algorithms, the result can be very
different.

The annotation :obj:`@fastmath` re-arranges floating point
expressions, e.g. changing the order of evaluation, or assuming that
certain special cases (inf, nan) cannot occur. In this case (and on
this particular computer), the main difference is that the expression
``1 / (2*dx)`` in the function ``deriv`` is hoisted out of the loop
(i.e. calculated outside the loop), as if one had written ``idx = 1 /
(2*dx)``. In the loop, the expression ``... / (2*dx)`` then becomes
``... * idx``, which is much faster to evaluate. Of course, both the
actual optimization that is applied by the compiler as well as the
resulting speedup depend very much on the hardware. You can examine
the change in generated code by using Julia's :func:`code_native`
function.

Treat Subnormal Numbers as Zeros
--------------------------------

Subnormal numbers, formerly called `denormal numbers <https://en.wikipedia.org/wiki/Denormal_number>`_,
are useful in many contexts, but incur a performance penalty on some hardware.
A call :func:`set_zero_subnormals(true) <set_zero_subnormals>`
grants permission for floating-point operations to treat subnormal
inputs or outputs as zeros, which may improve performance on some hardware.
A call :func:`set_zero_subnormals(false) <set_zero_subnormals>`
enforces strict IEEE behavior for subnormal numbers.

Below is an example where subnormals noticeably impact performance on some hardware::

    function timestep{T}( b::Vector{T}, a::Vector{T}, Δt::T )
        @assert length(a)==length(b)
        n = length(b)
        b[1] = 1                            # Boundary condition
        for i=2:n-1
            b[i] = a[i] + (a[i-1] - T(2)*a[i] + a[i+1]) * Δt
        end
        b[n] = 0                            # Boundary condition
    end

    function heatflow{T}( a::Vector{T}, nstep::Integer )
        b = similar(a)
        for t=1:div(nstep,2)                # Assume nstep is even
            timestep(b,a,T(0.1))
            timestep(a,b,T(0.1))
        end
    end

    heatflow(zeros(Float32,10),2)           # Force compilation
    for trial=1:6
        a = zeros(Float32,1000)
        set_zero_subnormals(iseven(trial))  # Odd trials use strict IEEE arithmetic
        @time heatflow(a,1000)
    end

This example generates many subnormal numbers because the values in ``a`` become
an exponentially decreasing curve, which slowly flattens out over time.

Treating subnormals as zeros should be used with caution, because doing so
breaks some identities, such as ``x-y==0`` implies ``x==y``::

    julia> x=3f-38; y=2f-38;

    julia> set_zero_subnormals(false); (x-y,x==y)
    (1.0000001f-38,false)

    julia> set_zero_subnormals(true); (x-y,x==y)
    (0.0f0,false)

In some applications, an alternative to zeroing subnormal numbers is
to inject a tiny bit of noise.  For example, instead of
initializing ``a`` with zeros, initialize it with::

     a = rand(Float32,1000) * 1.f-9

.. _man-code-warntype:

:obj:`@code_warntype`
---------------------

The macro :obj:`@code_warntype` (or its function variant :func:`code_warntype`)
can sometimes be helpful in diagnosing type-related problems. Here's an
example::

    pos(x) = x < 0 ? 0 : x

    function f(x)
        y = pos(x)
        sin(y*x+1)
    end

    julia> @code_warntype f(3.2)
    Variables:
      x::Float64
      y::UNION(INT64,FLOAT64)
      _var0::Float64
      _var3::Tuple{Int64}
      _var4::UNION(INT64,FLOAT64)
      _var1::Float64
      _var2::Float64

    Body:
      begin  # none, line 2:
          _var0 = (top(box))(Float64,(top(sitofp))(Float64,0))
          unless (top(box))(Bool,(top(or_int))((top(lt_float))(x::Float64,_var0::Float64)::Bool,(top(box))(Bool,(top(and_int))((top(box))(Bool,(top(and_int))((top(eq_float))(x::Float64,_var0::Float64)::Bool,(top(lt_float))(_var0::Float64,9.223372036854776e18)::Bool)),(top(slt_int))((top(box))(Int64,(top(fptosi))(Int64,_var0::Float64)),0)::Bool)))) goto 1
          _var4 = 0
          goto 2
          1:
          _var4 = x::Float64
          2:
          y = _var4::UNION(INT64,FLOAT64) # line 3:
          _var1 = y::UNION(INT64,FLOAT64) * x::Float64::Float64
          _var2 = (top(box))(Float64,(top(add_float))(_var1::Float64,(top(box))(Float64,(top(sitofp))(Float64,1))))
          return (GlobalRef(Base.Math,:nan_dom_err))((top(ccall))($(Expr(:call1, :(top(tuple)), "sin", GlobalRef(Base.Math,:libm))),Float64,$(Expr(:call1, :(top(tuple)), :Float64)),_var2::Float64,0)::Float64,_var2::Float64)::Float64
      end::Float64

Interpreting the output of :obj:`@code_warntype`, like that of its cousins
:obj:`@code_lowered`, :obj:`@code_typed`, :obj:`@code_llvm`, and
:obj:`@code_native`, takes a little practice. Your
code is being presented in form that has been partially digested on
its way to generating compiled machine code.  Most of the expressions
are annotated by a type, indicated by the ``::T`` (where ``T`` might
be :obj:`Float64`, for example). The most important characteristic of
:obj:`@code_warntype` is that non-concrete types are displayed in red; in
the above example, such output is shown in all-caps.

The top part of the output summarizes the type information for the different
variables internal to the function. You can see that ``y``, one of the
variables you created, is a ``Union{Int64,Float64}``, due to the
type-instability of ``pos``.  There is another variable, ``_var4``, which you
can see also has the same type.

The next lines represent the body of ``f``. The lines starting with a
number followed by a colon (``1:``, ``2:``) are labels, and represent
targets for jumps (via ``goto``) in your code.  Looking at the body,
you can see that ``pos`` has been *inlined* into ``f``---everything
before ``2:`` comes from code defined in ``pos``.

Starting at ``2:``, the variable ``y`` is defined, and again annotated
as a :obj:`Union` type.  Next, we see that the compiler created the
temporary variable ``_var1`` to hold the result of ``y*x``. Because
a :obj:`Float64` times *either* an :obj:`Int64` or :obj:`Float64` yields a
:obj:`Float64`, all type-instability ends here. The net result is that
``f(x::Float64)`` will not be type-unstable in its output, even if some of the
intermediate computations are type-unstable.

How you use this information is up to you. Obviously, it would be far
and away best to fix ``pos`` to be type-stable: if you did so, all of
the variables in ``f`` would be concrete, and its performance would be
optimal.  However, there are circumstances where this kind of
*ephemeral* type instability might not matter too much: for example,
if ``pos`` is never used in isolation, the fact that ``f``\'s output
is type-stable (for :obj:`Float64` inputs) will shield later code from
the propagating effects of type instability.  This is particularly
relevant in cases where fixing the type instability is difficult or
impossible: for example, currently it's not possible to infer the
return type of an anonymous function.  In such cases, the tips above
(e.g., adding type annotations and/or breaking up functions) are your
best tools to contain the "damage" from type instability.

The following examples may help you interpret expressions marked as
containing non-leaf types:

- Function body ending in ``end::Union{T1,T2})``

  + Interpretation: function with unstable return type

  + Suggestion: make the return value type-stable, even if you have to annotate it

- ``f(x::T)::Union{T1,T2}``

  + Interpretation: call to a type-unstable function

  + Suggestion: fix the function, or if necessary annotate the return value

- ``(top(arrayref))(A::Array{Any,1},1)::Any``

  + Interpretation: accessing elements of poorly-typed arrays

  + Suggestion: use arrays with better-defined types, or if necessary annotate the type of individual element accesses

- ``(top(getfield))(A::ArrayContainer{Float64},:data)::Array{Float64,N}``

  + Interpretation: getting a field that is of non-leaf type. In this case, ``ArrayContainer`` had a field ``data::Array{T}``. But ``Array`` needs the dimension ``N``, too, to be a concrete type.

  + Suggestion: use concrete types like ``Array{T,3}`` or ``Array{T,N}``, where ``N`` is now a parameter of ``ArrayContainer``
