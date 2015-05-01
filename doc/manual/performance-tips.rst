.. _man-performance-tips:

.. currentmodule:: Base

******************
 Performance Tips
******************

Some users reported that their Julia programs did not run as fast as expected, maybe even slower than their Python counterparts, contrast to the high performance claimed of Julia project. However, this inefficiency is largely due to the failure of conformity with the Julia programming norm. To help users better benefit from the high performance of Julia, this page presents some tips, by following which, users may see a significant speed-up (around 100x ~ 1000x) in their programs.

Why Julia *can* be fast
-----------------

The first thing is to understand why Julia *can* be fast. This is the basic to understand all following sections. The speed of a language depends on whether its corresponding compiler is able to generate efficient binary (machine code). The advantage of Julia compiler over its competitors lies on its capacity of inferring, during the *compile time*, the *runtime* type of a variable. By employing this technique, Julia is able to achieve nearly static language (C/Fortran) speed.

In other words, Julia is fast when the compiler can infer the runtime type of a variable, and it loses its efficiency when it fails to do so. Julia can infer the type of lots of variables automatically. But there are cases where it cannot. Therefore, whenever you help the compiler infer the runtime type of a variable which it cannot infer otherwise, you improve the speed of your program. By reading through the following sections, readers are invited to discover how the proposed code-writing skills help the compiler to infer the type.


Be cautious to the use of global variables, especially in a loop
----------------------------------------------------------------

The most significant trap in Julia is to use global variable everywhere, whose consequence is that its speed is even worse than the Python equivalent. A global variable is one defined in the global scope. Typically, the following are examples of global scopes:

* The top level of a script (Note: the begin-end, if-else block do not introduce a scope, and something directly inside these blocks are still in the global scope);

* The top level of a module (Note: module scope is global scope, and different modules have separted global scopes);

* REPL (also known as prompt);

* The top level of a cell in IJulia interactive environment.

In contrast to the local variables, whose types can mostly be inferred by the compiler automatically during the compile time, global variables cannot get their types inferred in the compile time, because their types can change at any point during the runtime. Every time a global variable is referred directly, the program has to do some extra work to determine its runtime type. If a single use of global variable is not much an issue, **involving a global variable in a loop** can significantly degrade the performance. The following are some examples.

Example 1 (bad code)::

	x = zeros(100000)
	for i = 1:length(x)
	    x[i] = 1
	end

This is bad, because the global variable ``x`` are referred 100000 times, and thus has its type checked 100000 times during the runtime, which takes a lot of time.

Example 2 (good code)::

	function main()
	    x = zeros(100000)
	    for i = 1:length(x)
	        x[i] = 1
	    end
	end
	main()

By simply putting all code in a function, ``x`` becomes local, and thus has its type inferred automatically during the compile time. So there will be no type-check cost during the runtime. The ``for`` loop in this example is 200x faster than the one in Example 1.

If some user does not like the idea of putting everything in a function, he has still another option: passing global variables as parameters to a function. Consider the following code:

Example 3 (good code)::

    function assign1!(a)
        for i = 1:length(a)
            a[i] = 1
        end
    end
    x = zeros(100000)
    assign1!(x)

The objective of this code is to assign ``1`` to each component of ``x``. This is not done in a loop in the global scope. On the contrary, a function named ``assign1!`` assumes the responsiblity, accepts a variable as parameter, and gets the heavy work done. With this technique, the ``for`` loop enjoys exactly the same speed as in Example 2, despite of the use of global variables.

To understand this, let us have a careful analysis on the type inference mechanism in this code. During the runtime, the global variable ``x`` is initialized as an array of type ``Float64``. Then, the program calls the function ``assign1!`` with the parameter ``x``. Here, ``x``, as a global variable, has its type checked, but only once. Then, the program runs a specific version of ``assign1!``: ``assign1!(a::Array{Float64})``. Therefore, in the function and thus in the ``for`` loop, the type of ``a`` is actually known, so without any type-check cost.

To better understand this: although ``assign1!`` modifies the global variable ``x`` with the local representative ``a``, ``a`` and ``x`` is not the same thing -- ``a`` is a *simplified* view of ``x``. Consider the following example:

Example 4 (some explanation)::

    change_x() = global x = "string"
    function assign1!(a)
        change_x()
        for i = 1:length(a)
            a[i] = 1
        end
    end
    x = zeros(100000)
    assign1!(x)

In the beginning of the function ``assign1!``, there is a function ``change_x``, which modifies the global variable ``x``. So, before the ``for`` loop, the definition of ``x`` has changed. ``x`` is now associated with a new address in the main memory, while ``a`` keeps associated with the original address. Therefore, during the ``for`` loop, the original address is assigned, while the new address remains untouched. Needless to say, the final value of ``x`` is "string". So, in this example, we did not "involve a global variable in a loop".

Sometimes, "involve a global variable in a loop" may happen in a less obvious way. Consider the following example:

Example 5 (bad code)::

    N = 100000
    function f()
        for i = 1:N
        end
    end
    f()

Here, ``N`` in ``f()`` refers to a global variable and has its type, as well as value, checked during each iteration. Although it does not appear in the loop body, it is still involved in the loop. The consequent cost is very high. To improve this code, while we can again adopt the technique in Example 3, here we will introduce another technique: ``const`` keyword.

``const`` is a modifier, which defines a "constant-value variable". A constant-value variable is a variable that cannot have its value, thus its type, changed during the runtime. Its type is determined when it is initialized. Of course, the compiler can use this type information to generate efficient binary. Consider the following example:

Example 6 (good code)::

    const N = 100000
    function f()
        for i = 1:N
        end
    end
    f()

It is 1000x faster than Example 5.

With the same logic, we can rewrite the Example 1 with the ``const`` keyword:

Example 7 (good code)::

	const x = zeros(100000)
	for i = 1:length(x)
	    x[i] = 1
	end

Though a constant-value variable may be a bit restricted -- all we need here is a "constant-type variable", this "constant-type variable", however, is not available currently. Though in the local scope, it is allowed to write ``x::Int = 1`` to restrict the type of a local variable, in the global scope, it is not allowed. The good news is that "constant-type variable" is a planned feature in future Julia version. Needless to say, it will be a powerful tool to improve the speed when using global variables.

Currently, all we have is the "constant-value variable". But the users can use it to imitate the behavior of "constant-type variable". In fact, the modification of the value of "constant-value variable" is actually allowed except that a warning will be produced. And the modification of the type of "constant-value variable" is allowed in no case.



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
