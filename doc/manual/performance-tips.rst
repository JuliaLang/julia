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
    elapsed time: 0.008217942 seconds (93784 bytes allocated)
    0.5

    julia> @time f(10^6)
    elapsed time: 0.063418472 seconds (32002136 bytes allocated)
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
thirty-fold faster execution::

    julia> @time f_improved(10^6)
    elapsed time: 0.00253829 seconds (112 bytes allocated)
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
  occur.  See :ref:`stdlib-track-allocation`.

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

Declare specific types for fields of composite types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a user-defined type like the following::

    type Foo
        field
    end

the compiler will not generally know the type of ``foo.field``, since it
might be modified at any time to refer to a value of a different type.
It will help to declare the most specific type possible, such as
``field::Float64`` or ``field::Array{Int64,1}``.

Annotate values taken from untyped locations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is often convenient to work with data structures that may contain
values of any type, such as the original ``Foo`` type above, or cell
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
        a = Array(randbool() ? Int64 : Float64, n)
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
        a = Array(randbool() ? Int64 : Float64, n)
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

    julia> map(fmt, {copy_cols, copy_rows, copy_col_row, copy_row_col});
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
-  Write :obj:`@simd` in front of ``for`` loops that are amenable to vectorization.
   **This feature is experimental** and could change or disappear in future
   versions of Julia.

Here is an example with both forms of markup::

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
   speedup with ``@simd``.

.. raw:: html
    <style> .red {color:red} </style>

.. _man-code-warntype:

``@code_warntype``
------------------

The macro ``@code_warntype`` (or its function variant) can sometimes be helpful in diagnosing type-related problems.  Here's an example::

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
      _var3::(Int64,)
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
          return (GetfieldNode(Base.Math,:nan_dom_err,Any))((top(ccall))($(Expr(:call1, :(top(tuple)), "sin", GetfieldNode(Base.Math,:libm,Any))),Float64,$(Expr(:call1, :(top(tuple)), :Float64)),_var2::Float64,0)::Float64,_var2::Float64)::Float64
      end::Float64

Interpreting the output of ``@code_warntype``, like that of its cousins
``@code_lowered``, ``@code_typed``, ``@code_llvm``, and
``@code_native``, takes a little practice. Your
code is being presented in form that has been partially-digested on
its way to generating compiled machine code.  Most of the expressions
are annotated by a type, indicated by the ``::T`` (where ``T`` might
be ``Float64``, for example). The most important characteristic of
``@code_warntype`` is that non-concrete types are displayed in red; in
the above example, such output is shown in all-caps.

The top part of the output summarizes the type information for the different variables internal to the function. You can see that ``y``, one
of the variables you created, is a ``Union(Int64,Float64)``, due to
the type-instability of ``pos``.  There is another variable,
``_var4``, which you can see also has the same type.

The next lines represent the body of ``f``. The lines starting with a
number followed by a colon (``1:``, ``2:``) are labels, and represent
targets for jumps (via ``goto``) in your code.  Looking at the body,
you can see that ``pos`` has been *inlined* into ``f``---everything
before ``2:`` comes from code defined in ``pos``.

Starting at ``2:``, the variable ``y`` is defined, and again annotated
as a ``Union`` type.  Next, we see that the compiler created the
temporary variable ``_var1`` to hold the result of ``y*x``. Because
a ``Float64`` times *either* an ``Int64`` or ``Float64`` yields a ``Float64``,
all type-instability ends here.  The net result is that
``f(x::Float64)`` will not be type-unstable in its output, even if
some of the intermediate computations are type-unstable.

How you use this information is up to you.  Obviously, it would be far
and away best to fix ``pos`` to be type-stable: if you did so, all of
the variables in ``f`` would be concrete, and its performance would be
optimal.  However, there are circumstances where this kind of
*ephemeral* type instability might not matter too much: for example,
if ``pos`` is never used in isolation, the fact that ``f``\'s output
is type-stable (for ``Float64`` inputs) will shield later code from
the propagating effects of type instability.  This is particularly
relevant in cases where fixing the type instability is difficult or
impossible: for example, currently it's not possible to infer the
return type of an anonymous function.  In such cases, the tips above
(e.g., adding type annotations and/or breaking up functions) are your
best tools to contain the "damage" from type instability.

Here is a table which can help you interpret expressions marked as
containing non-leaf types:

.. |I0| replace:: Interpretation
.. |F0| replace:: Possible fix
.. |I1| replace:: Function with unstable return type
.. |F1| replace:: Make the return value type-stable, even if you have to annotate it
.. |I2| replace:: Call to a type-unstable function
.. |F2| replace:: Fix the function, or annotate the return value
.. |I3| replace:: Accessing elements of poorly-typed arrays
.. |F3| replace:: Use arrays with better-defined types, or annotate the type of individual element accesses
.. |I4| replace:: Getting a field that is of non-leaf type. In this case, ``ArrayContainer`` had a field ``data::Array{T}``. But ``Array`` needs the dimension ``N``, too.
.. |F4| replace:: Use concrete types like ``Array{T,3}`` or ``Array{T,N}``, where ``N`` is now a parameter of ``ArrayContainer``

+-------------------------------------------------------------------------+------+------+
|  Marked expression                                                      | |I0| | |F0| |
+=========================================================================+======+======+
| Function ending in ``end::Union(T1,T2)))``                              | |I1| | |F1| |
+-------------------------------------------------------------------------+------+------+
| ``f(x::T)::Union(T1,T2)``                                               | |I2| | |F2| |
+-------------------------------------------------------------------------+------+------+
| ``(top(arrayref))(A::Array{Any,1},1)::Any``                             | |I3| | |F3| |
+-------------------------------------------------------------------------+------+------+
| ``(top(getfield))(A::ArrayContainer{Float64},:data)::Array{Float64,N}`` | |I4| | |F4| |
+-------------------------------------------------------------------------+------+------+
