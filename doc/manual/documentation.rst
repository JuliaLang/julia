.. _man-documentation:

***************
 Documentation
***************

Julia enables package developers and users to document functions, types and
other objects easily, either via the built-in documentation system in Julia 0.4
or the `Docile.jl <https://github.com/MichaelHatherly/Docile.jl>`_ package in
Julia 0.3.

In 0.4:

.. doctest::

    "Tells you if there are too foo items in the array."
    foo(xs::Array) = ...

Documentation is interpreted as `Markdown <https://en.wikipedia.org/wiki/Markdown>`_,
so you can use indentation and code fences to delimit code examples from text.

.. doctest::

    """
    The `@bar` macro will probably make your code 2x faster or something. Use
    it like this:

        @bar buy_drink_for("Jiahao")
    """
    macro bar(ex) ...

Documentation is very free-form; there are no set formatting
restrictions or strict conventions. It's hoped that best practices will
emerge fairly naturally as package developers learn to use the new
system.

Technically, any object can be associated with any other as metadata;
Markdown happens to be the default, but one can construct other string
macros and pass them to the ``@doc`` macro just as well.

Accessing Documentation
-----------------------

Documentation can be accessed at the REPL or in IJulia by typing ``?``
followed by the name of a function or macro, and pressing ``Enter``. For
example,

.. doctest::

    ?fft
    ?@time
    ?r""

will bring up docs for the relevant function, macro or string macro
respectively. In `Juno <http://junolab.org>`_ using ``Ctrl-D`` will
bring up documentation for the object under the cursor.

Functions & Methods
-------------------

Functions in Julia may have multiple implementations, known as methods.
While it's good practice for generic functions to have a single purpose,
Julia allows methods to be documented individually if necessary. For
example:

.. doctest::

    """
    Multiplication operator. `x*y*z*...` calls this function with multiple
    arguments, i.e. `*(x,y,z...)`.
    """
    function *(x, y)
      # ... [implementation sold seperately] ...
    end

    "When applied to strings, concatenates them."
    function *(x::AbstractString, y::AbstractString)
      # ... [insert secret sauce here] ...
    end

    help?>*
    Multiplication operator. `x*y*z*...` calls this function with multiple
    arguments, i.e. `*(x,y,z...)`.

    When applied to strings, concatenates them.

When retrieving documentation for a generic function, the metadata for
each method is concatenated with the ``catdoc`` function, which can of
course be overridden for custom types.

Advanced Usage
--------------

The ``@doc`` macro associates its first argument with its second in a
per-module dictionary called ``META``. By default, documentation is
expected to be written in Markdown, and the ``doc""`` string macro simply
creates an object representing the Markdown content. In the future it is
likely to do more advanced things such as allowing for relative image or
link paths.

When used for retrieving documentation, the ``@doc`` macro (or equally,
the ``doc`` function) will search all ``META`` dictionaries for metadata
relevant to the given object and return it. The returned object (some
Markdown content, for example) will by default display itself
intelligently. This design also makes it easy to use the doc system in a
programmatic way; for example, to re-use documentation between different
versions of a function:

.. doctest::

    @doc "..." foo!
    @doc (@doc foo!) foo

Or for use with Julia's metaprogramming functionality:

.. doctest::

    for (f, op) in ((:add, :+), (:subtract, :-), (:multiply, :*), (:divide, :/))
        @eval begin
            $f(a,b) = $op(a,b)
        end
    end
    @doc "`add(a,b)` adds `a` and `b` together" add
    @doc "`subtract(a,b)` subtracts `b` from `a`" subtract

Documentation written in non-toplevel blocks, such as ``if``, ``for``, and ``let``, are not
automatically added to the documentation system. ``@doc`` must be used in these cases. For
example:

.. code-block:: julia

    if VERSION > v"0.4"
        "..."
        f(x) = x
    end

will not add any documentation to ``f`` even when the condition is ``true`` and must instead
be written as:

.. code-block:: julia

    if VERSION > v"0.4"
        @doc "..." ->
        f(x) = x
    end

Syntax Guide
------------

A comprehensive overview of all documentable Julia syntax.

In the following examples ``"..."`` is used to illustrate an arbitrary docstring which may
be one of the follow four variants and contain arbitrary text:

.. code-block:: julia

    "..."

    doc"..."

    """
    ...
    """

    doc"""
    ...
    """

``@doc_str`` should only be used when the docstring contains ``$`` or ``\`` characters that
should not be parsed by Julia such as LaTeX syntax or Julia source code examples containing
interpolation.

Functions and Methods
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: julia

    "..."
    function f end

    "..."
    f

Adds docstring ``"..."`` to ``Function`` ``f``. The first version is the preferred syntax,
however both are equivalent.

.. code-block:: julia

    "..."
    f(x) = x

    "..."
    function f(x)
        x
    end

    "..."
    f(x)

Adds docstring ``"..."`` to ``Method`` ``f(::Any)``.

.. code-block:: julia

    "..."
    f(x, y = 1) = x + y

Adds docstring ``"..."`` to two ``Method``\ s, namely ``f(::Any)`` and ``f(::Any, ::Any)``.

Types
~~~~~

.. code-block:: julia

    "..."
    abstract T

    "..."
    type T end

    "..."
    immutable T end

Adds the docstring ``"..."`` to type ``T``.

.. code-block:: julia

    "..."
    type T
        "x"
        x
        "y"
        y
    end

Adds docstring ``"..."`` to type ``T``, ``"x"`` to field ``T.x`` and ``"y"`` to field
``T.y``. Also applicable to ``immutable`` types.

.. code-block:: julia

    "..."
    typealias A T

Adds docstring ``"..."`` to the ``Binding`` ``A``.

``Binding``\ s are used to store a reference to a particular ``Symbol`` in a ``Module``
without storing the referenced value itself.

Macros
~~~~~~

.. code-block:: julia

    "..."
    macro m() end

    "..."
    :(@m)

Adds docstring ``"..."`` to the ``Binding`` ``@m``. Adding documentation at the definition
is the preferred approach.

Modules
~~~~~~~

.. code-block:: julia

    "..."
    module M end

    module M

    "..."
    M

    end

Adds docstring ``"..."`` to the ``Module`` ``M``. Adding the docstring above the ``Module``
is the preferred syntax, however both are equivalent.

.. code-block:: julia

    "..."
    baremodule M
    # ...
    end

    baremodule M

    import Base: call, @doc

    "..."
    f(x) = x

    end

Documenting a ``baremodule`` by placing a docstring above the expression automatically
imports ``call`` and ``@doc`` into the module. These imports must be done manually when the
module expression is not documented. Empty ``baremodule``\ s cannot be documented.

Global Variables
~~~~~~~~~~~~~~~~

.. code-block:: julia

    "..."
    const a = 1

    "..."
    b = 2

    "..."
    global c = 3

Adds docstring ``"..."`` to the ``Binding``\ s ``a``, ``b``, and ``c``.

.. code-block:: julia

    "..."
    sym

Adds docstring ``"..."`` to the value associated with ``sym``. Users should prefer
documenting ``sym`` at it's definition.

Multiple Objects
~~~~~~~~~~~~~~~~

.. code-block:: julia

    "..."
    a, b

Adds docstring ``"..."`` to ``a`` and ``b`` each of which should be a documentable
expression. This syntax is equivalent to

.. code-block:: julia

    "..."
    a

    "..."
    b

Any number of expressions many be documented together in this way. This syntax can be useful
when two functions are related, such as non-mutating and mutating versions ``f`` and ``f!``.

Macro-generated code
~~~~~~~~~~~~~~~~~~~~

.. code-block:: julia

    "..."
    @m expression

Adds docstring ``"..."`` to expression generated by expanding ``@m expression``. This syntax
is only valid if expanding ``@m`` results in a single expression that can be documented.
This allows for functions decorated with ``@inline``, ``@noinline``, and ``@generated`` to
be documented in the same way as other functions.

Markdown Syntax Notes
---------------------

Julia's Markdown parser supports most of the basic Markdown elements,
including paragraphs, code blocks, bulleted lists and basic links. It's
also a work in progress, however, and support for more advanced things
like tables is in the works.

Markdown.jl supports interpolation in a very similar way to basic string
literals, with the difference that it will store the object itself in
the Markdown tree (as opposed to converting it to a string). When the
Markdown content is rendered the usual ``writemime`` methods will be
called, and these can be overridden as usual. This design allows the
Markdown to be extended with arbitrarily complex features (such as
references) without cluttering the basic syntax.

In principle, the Markdown parser itself can also be arbitrarily
extended by packages, or an entirely custom flavour of Markdown can be
used, but this should generally be unnecessary.
