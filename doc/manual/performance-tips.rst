.. _man-performance-tips:

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

We find that global names are frequently constants, and declaring them
as such greatly improves performance::

    const DEFAULT_VAL = 0

Uses of non-constant globals can be optimized by annotating their types
at the point of use::

    global x
    y = f(x::Int + 1)

Type declarations
-----------------

In many languages with optional type declarations, adding declarations
is the principal way to make code run faster. In Julia, the compiler
generally knows the types of all function arguments and local variables.
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
``Int32``. Making an annotation like this has the added benefit that it
will raise a run-time error if the value is not of the expected type,
potentially catching certain bugs earlier.

Break functions into multiple definitions
-----------------------------------------

Writing a function as many small definitions allows the compiler to
directly call the most applicable code, or even inline it.

Here is an example of a "compound function" that should really be
written as multiple definitions::

    function norm(A)
        if isa(A, Vector)
            return sqrt(real(dot(x,x)))
        elseif isa(A, Matrix)
            return max(svd(A)[2])
        else
            error("norm: invalid argument")
        end
    end

This can be written more concisely and efficiently as::

    norm(A::Vector) = sqrt(real(dot(x,x)))
    norm(A::Matrix) = max(svd(A)[2])

Write "type-stable" functions
-----------------------------

When possible, it helps to ensure that a function always returns a value
of the same type. Consider the following definition::

    pos(x) = x < 0 ? 0 : x

Although this seems innocent enough, the problem is that ``0`` is an
integer (of type ``Int``) and ``x`` might be of any type. Thus,
depending on the value of ``x``, this function might return a value of
either of two types. This behavior is allowed, and may be desirable in
some cases. But it can easily be fixed as follows::

    pos(x) = x < 0 ? zero(x) : x

There is also a ``one`` function, and a more general ``oftype(x,y)``
function, which returns ``y`` converted to the type of ``x``. The first
argument to any of these functions can be either a value or a type.

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
becomes a floating-point number (the result of the ``/`` operator). This
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
        for i=1:numel(a)
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
example, see ``_jl_hvcat_fill`` in
`abstractarray.jl <https://github.com/JuliaLang/julia/blob/master/base/abstractarray.jl>`_,
or the ``fill!`` function, which we could have used instead of writing
our own ``fill_twos!``.

Functions like ``strange_twos`` occur when dealing with data of
uncertain type, for example data loaded from an input file that might
contain either integers, floats, strings, or something else.

Tweaks
------

These are some minor points that might help in tight inner loops.

-  Use ``size(A,n)`` when possible instead of ``size(A)``.
-  Avoid unnecessary arrays. For example, instead of ``sum([x,y,z])``
   use ``x+y+z``.

