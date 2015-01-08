.. _man-functions:

.. currentmodule:: Base

***********
 Functions  
***********

In Julia, a function is an object that maps a tuple of argument values
to a return value. Julia functions are not pure mathematical functions,
in the sense that functions can alter and be affected by the global
state of the program. The basic syntax for defining functions in Julia
is:

.. testcode::

    function f(x,y)
      x + y
    end

.. testoutput::
    :hide:

    f (generic function with 1 method)

There is a second, more terse syntax for defining a function in Julia.
The traditional function declaration syntax demonstrated above is
equivalent to the following compact "assignment form"::

    f(x,y) = x + y

In the assignment form, the body of the function must be a single
expression, although it can be a compound expression (see
:ref:`man-compound-expressions`). Short, simple
function definitions are common in Julia. The short function syntax is
accordingly quite idiomatic, considerably reducing both typing and
visual noise.

A function is called using the traditional parenthesis syntax:

.. doctest::

    julia> f(2,3)
    5

Without parentheses, the expression ``f`` refers to the function object,
and can be passed around like any value:

.. doctest::

    julia> g = f;

    julia> g(2,3)
    5

There are two other ways that functions can be applied: using special
operator syntax for certain function names (see `Operators Are
Functions <#operators-are-functions>`_ below), or with the :func:`apply`
function:

.. doctest::

    julia> apply(f,2,3)
    5

:func:`apply` applies its first argument — a function object —
to its remaining arguments.

As with variables, Unicode can also be used for function names::

    julia> ∑(x,y) = x + y
    ∑ (generic function with 1 method)


Argument Passing Behavior
-------------------------

Julia function arguments follow a convention sometimes called "pass-by-sharing",
which means that values are not copied when they are passed to functions.
Function arguments themselves act as new variable *bindings* (new locations that
can refer to values), but the values they refer to are identical to the passed
values. Modifications to mutable values (such as Arrays) made within a function
will be visible to the caller. This is the same behavior found in Scheme, most
Lisps, Python, Ruby and Perl, among other dynamic languages.

.. _man-return-keyword:

The ``return`` Keyword
----------------------

The value returned by a function is the value of the last expression
evaluated, which, by default, is the last expression in the body of the
function definition. In the example function, ``f``, from the previous
section this is the value of the expression ``x + y``. As in C and most
other imperative or functional languages, the ``return`` keyword causes
a function to return immediately, providing an expression whose value is
returned::

    function g(x,y)
      return x * y
      x + y
    end

Since function definitions can be entered into interactive sessions, it
is easy to compare these definitions::

    f(x,y) = x + y

    function g(x,y)
      return x * y
      x + y
    end

    julia> f(2,3)
    5

    julia> g(2,3)
    6

Of course, in a purely linear function body like ``g``, the usage of
``return`` is pointless since the expression ``x + y`` is never
evaluated and we could simply make ``x * y`` the last expression in the
function and omit the ``return``. In conjunction with other control
flow, however, ``return`` is of real use. Here, for example, is a
function that computes the hypotenuse length of a right triangle with
sides of length *x* and *y*, avoiding overflow::

    function hypot(x,y)
      x = abs(x)
      y = abs(y)
      if x > y
        r = y/x
        return x*sqrt(1+r*r)
      end
      if y == 0
        return zero(x)
      end
      r = x/y
      return y*sqrt(1+r*r)
    end

There are three possible points of return from this function, returning
the values of three different expressions, depending on the values of
*x* and *y*. The ``return`` on the last line could be omitted since it
is the last expression.

Operators Are Functions
-----------------------

In Julia, most operators are just functions with support for special
syntax. The exceptions are operators with special evaluation semantics
like ``&&`` and ``||``. These operators cannot be functions since
:ref:`short-circuit evaluation <man-short-circuit-evaluation>` requires that
their operands are not evaluated before evaluation of the operator.
Accordingly, you can also apply them using parenthesized argument lists,
just as you would any other function:

.. doctest::

    julia> 1 + 2 + 3
    6

    julia> +(1,2,3)
    6

The infix form is exactly equivalent to the function application form —
in fact the former is parsed to produce the function call internally.
This also means that you can assign and pass around operators such as
:func:`+` and :func:`*` just like you would with other function values:

.. doctest:: f-plus

    julia> f = +;

    julia> f(1,2,3)
    6

Under the name ``f``, the function does not support infix notation,
however.

Operators With Special Names
----------------------------

A few special expressions correspond to calls to functions with non-obvious
names. These are:

=================== ==================
Expression          Calls
=================== ==================
``[A B C ...]``     :func:`hcat`
``[A, B, C, ...]``  :func:`vcat`
``[A B; C D; ...]`` :func:`hvcat`
``A'``              :func:`ctranspose`
``A.'``             :func:`transpose`
``1:n``             :func:`colon`
``A[i]``            :func:`getindex`
``A[i]=x``          :func:`setindex!`
=================== ==================

These functions are included in the ``Base.Operators`` module even
though they do not have operator-like names.

.. _man-anonymous-functions:

Anonymous Functions
-------------------

Functions in Julia are `first-class objects
<http://en.wikipedia.org/wiki/First-class_citizen>`_: they can be assigned to
variables, called using the standard function call syntax from the
variable they have been assigned to. They can be used as arguments, and
they can be returned as values. They can also be created anonymously,
without being given a name:

.. doctest::

    julia> x -> x^2 + 2x - 1
    (anonymous function)

This creates an unnamed function taking one argument *x* and returning the
value of the polynomial *x*\ ^2 + 2\ *x* - 1 at that value. The primary
use for anonymous functions is passing them to functions which take
other functions as arguments. A classic example is :func:`map`,
which applies a function to each value of an array and returns a new
array containing the resulting values:

.. doctest::

    julia> map(round, [1.2,3.5,1.7])
    3-element Array{Float64,1}:
     1.0
     4.0
     2.0

This is fine if a named function effecting the transform one wants
already exists to pass as the first argument to :func:`map`. Often, however,
a ready-to-use, named function does not exist. In these situations, the
anonymous function construct allows easy creation of a single-use
function object without needing a name:

.. doctest::

    julia> map(x -> x^2 + 2x - 1, [1,3,-1])
    3-element Array{Int64,1}:
      2
     14
     -2

An anonymous function accepting multiple arguments can be written using
the syntax ``(x,y,z)->2x+y-z``. A zero-argument anonymous function is
written as ``()->3``. The idea of a function with no arguments may seem
strange, but is useful for "delaying" a computation. In this usage, a
block of code is wrapped in a zero-argument function, which is later
invoked by calling it as ``f()``.

Multiple Return Values
----------------------

In Julia, one returns a tuple of values to simulate returning multiple
values. However, tuples can be created and destructured without needing
parentheses, thereby providing an illusion that multiple values are
being returned, rather than a single tuple value. For example, the
following function returns a pair of values:

.. doctest::

    julia> function foo(a,b)
             a+b, a*b
           end;

If you call it in an interactive session without assigning the return
value anywhere, you will see the tuple returned:

.. doctest::

    julia> foo(2,3)
    (5,6)

A typical usage of such a pair of return values, however, extracts each
value into a variable. Julia supports simple tuple "destructuring" that
facilitates this:

.. doctest::

    julia> x, y = foo(2,3);

    julia> x
    5

    julia> y
    6

You can also return multiple values via an explicit usage of the
``return`` keyword::

    function foo(a,b)
      return a+b, a*b
    end

This has the exact same effect as the previous definition of ``foo``.

Varargs Functions
-----------------

It is often convenient to be able to write functions taking an arbitrary
number of arguments. Such functions are traditionally known as "varargs"
functions, which is short for "variable number of arguments". You can
define a varargs function by following the last argument with an
ellipsis:

.. doctest::

    julia> bar(a,b,x...) = (a,b,x)
    bar (generic function with 1 method)

The variables ``a`` and ``b`` are bound to the first two argument values
as usual, and the variable ``x`` is bound to an iterable collection of
the zero or more values passed to ``bar`` after its first two arguments:

.. doctest::

    julia> bar(1,2)
    (1,2,())

    julia> bar(1,2,3)
    (1,2,(3,))

    julia> bar(1,2,3,4)
    (1,2,(3,4))

    julia> bar(1,2,3,4,5,6)
    (1,2,(3,4,5,6))

In all these cases, ``x`` is bound to a tuple of the trailing values
passed to ``bar``.

On the flip side, it is often handy to "splice" the values contained in
an iterable collection into a function call as individual arguments. To
do this, one also uses ``...`` but in the function call instead:

.. doctest::

    julia> x = (3,4)
    (3,4)

    julia> bar(1,2,x...)
    (1,2,(3,4))

In this case a tuple of values is spliced into a varargs call precisely
where the variable number of arguments go. This need not be the case,
however:

.. doctest::

    julia> x = (2,3,4)
    (2,3,4)

    julia> bar(1,x...)
    (1,2,(3,4))

    julia> x = (1,2,3,4)
    (1,2,3,4)

    julia> bar(x...)
    (1,2,(3,4))

Furthermore, the iterable object spliced into a function call need not
be a tuple:

.. doctest::

    julia> x = [3,4]
    2-element Array{Int64,1}:
     3
     4

    julia> bar(1,2,x...)
    (1,2,(3,4))

    julia> x = [1,2,3,4]
    4-element Array{Int64,1}:
     1
     2
     3
     4

    julia> bar(x...)
    (1,2,(3,4))

Also, the function that arguments are spliced into need not be a varargs
function (although it often is)::

    baz(a,b) = a + b

    julia> args = [1,2]
    2-element Array{Int64,1}:
     1
     2

    julia> baz(args...)
    3

    julia> args = [1,2,3]
    3-element Array{Int64,1}:
     1
     2
     3

    julia> baz(args...)
    no method baz(Int64,Int64,Int64)

As you can see, if the wrong number of elements are in the spliced
container, then the function call will fail, just as it would if too
many arguments were given explicitly.

Optional Arguments
------------------

In many cases, function arguments have sensible default values and therefore
might not need to be passed explicitly in every call. For example, the
library function :func:`parseint(num,base) <parseint>` interprets a string as a number
in some base. The ``base`` argument defaults to ``10``. This behavior can be
expressed concisely as::

    function parseint(num, base=10)
        ###
    end

With this definition, the function can be called with either one or two
arguments, and ``10`` is automatically passed when a second argument is not
specified:

.. doctest::

    julia> parseint("12",10)
    12

    julia> parseint("12",3)
    5

    julia> parseint("12")
    12

Optional arguments are actually just a convenient syntax for writing
multiple method definitions with different numbers of arguments
(see :ref:`man-methods`).


Keyword Arguments
-----------------

Some functions need a large number of arguments, or have a large number of
behaviors. Remembering how to call such functions can be difficult. Keyword
arguments can make these complex interfaces easier to use and extend by
allowing arguments to be identified by name instead of only by position.

For example, consider a function ``plot`` that
plots a line. This function might have many options, for controlling line
style, width, color, and so on. If it accepts keyword arguments, a possible
call might look like ``plot(x, y, width=2)``, where we have chosen to
specify only line width. Notice that this serves two purposes. The call is
easier to read, since we can label an argument with its meaning. It also
becomes possible to pass any subset of a large number of arguments, in
any order.

Functions with keyword arguments are defined using a semicolon in the
signature::

    function plot(x, y; style="solid", width=1, color="black")
        ###
    end

When the function is called, the semicolon is optional: one can
either call ``plot(x, y, width=2)`` or ``plot(x, y; width=2)``, but
the former style is more common.  An explicit semicolon is required only
for passing varargs or computed keywords as described below.

Keyword argument default values are evaluated only when necessary
(when a corresponding keyword argument is not passed), and in
left-to-right order. Therefore default expressions may refer to
prior keyword arguments.

Extra keyword arguments can be collected using ``...``, as in varargs
functions::

    function f(x; y=0, args...)
        ###
    end

Inside ``f``, ``args`` will be a collection of ``(key,value)`` tuples,
where each ``key`` is a symbol. Such collections can be passed as keyword
arguments using a semicolon in a call, e.g. ``f(x, z=1; args...)``.
Dictionaries can also be used for this purpose.

In addition, one can also pass ``(key,value)`` tuples, or any iterable
expression (such as a ``=>`` pair) that can be assigned to such a
tuple, explicitly after a semicolon.  For example, ``plot(x, y;
(:width,2))`` and ``plot(x, y; :width => 2)`` are equivalent to
``plot(x, y, width=2)``.  This is useful in situations where the
keyword name is computed at runtime.


Evaluation Scope of Default Values
----------------------------------

Optional and keyword arguments differ slightly in how their default
values are evaluated. When optional argument default expressions are
evaluated, only *previous* arguments are in scope. In contrast, *all*
the arguments are in scope when keyword arguments default expressions
are evaluated. For example, given this definition::

    function f(x, a=b, b=1)
        ###
    end

the ``b`` in ``a=b`` refers to a ``b`` in an outer scope, not the
subsequent argument ``b``. However, if ``a`` and ``b`` were keyword
arguments instead, then both would be created in the same scope and
the ``b`` in ``a=b`` would refer the the subsequent argument ``b``
(shadowing any ``b`` in an outer scope), which would result in an
undefined variable error (since the default expressions are evaluated
left-to-right, and ``b`` has not been assigned yet).


Block Syntax for Function Arguments
-----------------------------------

Passing functions as arguments to other functions is a powerful technique,
but the syntax for it is not always convenient. Such calls are especially
awkward to write when the function argument requires multiple lines. As
an example, consider calling :func:`map` on a function with several cases::

    map(x->begin
               if x < 0 && iseven(x)
                   return 0
               elseif x == 0
                   return 1
               else
                   return x
               end
           end,
        [A, B, C])

Julia provides a reserved word ``do`` for rewriting this code more clearly::

    map([A, B, C]) do x
        if x < 0 && iseven(x)
            return 0
        elseif x == 0
            return 1
        else
            return x
        end
    end

The ``do x`` syntax creates an anonymous function with argument ``x``
and passes it as the first argument to :func:`map`. Similarly, ``do a,b``
would create a two-argument anonymous function, and a plain ``do``
would declare that what follows is an anonymous function of the form
``() -> ...``.

How these arguments are initialized depends on the "outer" function;
here, :func:`map` will sequentially set ``x`` to ``A``, ``B``, ``C``,
calling the anonymous function on each, just as would happen in the
syntax ``map(func, [A, B, C])``.

This syntax makes it easier to use functions to effectively extend the
language, since calls look like normal code blocks. There are many
possible uses quite different from :func:`map`, such as managing system
state. For example, there is a version of :func:`open` that runs code
ensuring that the opened file is eventually closed::

    open("outfile", "w") do io
        write(io, data)
    end

This is accomplished by the following definition::

    function open(f::Function, args...)
        io = open(args...)
        try
            f(io)
        finally
            close(io)
        end
    end

In contrast to the :func:`map` example, here ``io`` is initialized by the
*result* of ``open("outfile", "w")``.  The stream is then passed to
your anonymous function, which performs the writing; finally, the
:func:`open` function ensures that the stream is closed after your
function exits.  The ``try/finally`` construct will be described in
:ref:`man-control-flow`.

With the ``do`` block syntax, it helps to check the documentation or
implementation to know how the arguments of the user function are
initialized.

Further Reading
---------------

We should mention here that this is far from a complete picture of
defining functions. Julia has a sophisticated type system and allows
multiple dispatch on argument types. None of the examples given here
provide any type annotations on their arguments, meaning that they are
applicable to all types of arguments. The type system is described in
:ref:`man-types` and defining a function in terms of methods chosen
by multiple dispatch on run-time argument types is described in
:ref:`man-methods`.
