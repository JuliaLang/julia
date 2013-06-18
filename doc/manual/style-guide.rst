.. _man-style-guide:

*************
 Style Guide
*************

The following sections explain a few aspects of idiomatic Julia coding style.
None of these rules are absolute; they are only suggestions to help familiarize
you with the language and to help you choose among alternative designs.

Avoid writing overly-specific types
-----------------------------------

Code should be as generic as possible. Instead of writing:

    convert(Complex{Float64}, x)

it's better to use available generic functions:

    complex(float(x))

The second version will convert ``x`` to an appropriate type, instead of
always the same type.

This style point is especially relevant to function arguments. Don't declare
an argument to be of type ``Int`` or ``Int32`` if it really could be
any integer, expressed with the abstract type ``Integer``.

Handle excess argument diversity in the caller
----------------------------------------------

Instead of:

    function foo(x, y)
        x = int(x); y = int(y)
        ...
    end
    foo(x, y)

use:

    function foo(x::Int, y::Int)
        ...
    end
    foo(int(x), int(y))

This is better style because ``foo`` does not really accept numbers of all
types; it really needs ``Int`` s.

Avoid strange type Unions
-------------------------

Types such as ``Union(Function,String)`` are often a sign that some design
could be cleaner.

Try to avoid nullable fields
----------------------------

When using ``x::Union(Nothing,T)``, ask whether the option for ``x`` to be
``nothing`` is really necessary. Here are some alternatives to consider:

- Find a safe default value to initialize ``x`` with
- Introduce another type that lacks ``x``
- If there are many fields like ``x``, store them in a dictionary
- Determine whether there is a simple rule for when ``x`` is ``nothing``.
  For example, often the field will start as ``nothing`` but get initialized at
  some well-defined point. In that case, consider leaving it undefined at first.

Avoid elaborate container types
-------------------------------

It is usually not much help to construct arrays like the following:

    a = Array(Union(Int,String,Tuple,Array), n)

In this case ``cell(n)`` is better. It is also more helpful to the compiler
to annotate specific uses (e.g. ``a[i]::Int``) than to try to pack many
alternatives into one type.

Avoid underscores in names
--------------------------

If a function name requires multiple words, it might represent more than one
concept. It is better to keep identifier names concise.

Don't overuse try-catch
-----------------------

It is better to avoid errors than to rely on catching them.

Don't parenthesize conditions
-----------------------------

Julia doesn't require parens around conditions in ``if`` and ``while``.
Write

    if a == b

instead of

    if (a == b)

Don't overuse ...
-----------------

Splicing function arguments can be addictive. Instead of ``[a..., b...]``,
use simply ``[a, b]``, which already concatenates arrays.
``collect(a)`` is better than ``[a...]``, but since ``a`` is already iterable
it is often even better to leave it alone, and not convert it to an array.
