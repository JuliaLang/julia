.. _man-documentation:

***************
 Documentation
***************

Julia enables package developers and users to document functions, types and
other objects easily, either via the built-in documentation system in Julia 0.4
or the `Docile.jl <http://github.com/MichaelHatherly/Docile.jl>`_ package in
Julia 0.3.

.. doctest::

    VERSION < v"0.4-" && using Docile

    @doc doc"Tells you if there are too foo items in the array." ->
    foo(xs::Array) = ...

Documentation is interpreted as `Markdown <http://en.wikipedia.org/wiki/Markdown>`_,
so you can use indentation and code fences to delimit code examples from text.

.. doctest::

    @doc doc"""
      The `@bar` macro will probably make your code 2x faster or something. Use
      it like this:

          @bar by_drink_for("Mike")
      """ ->
    macro bar(ex) ...

Documentation is free-form; there are no set formatting restrictions or
strict conventions.

Technically, any object can be associated with any other as metadata; Markdown
happens to be the default, but one can construct other string macros and pass
them to the ``@doc`` macro just as well.

Accessing Documentation
-----------------------

Documentation can be accessed at the REPL or in IJulia by typing ``?`` followed
by the name of a function or macro, and pressing ``Enter``. For example,

.. doctest::

    ?fft
    ?@time
    ?r""

will bring up docs for the relevant function, macro or string macro respectively.
In Juno using ``Ctrl-D`` will bring up documentation for the object under the
cursor.

Functions & Methods
-------------------

Functions in Julia may have multiple implementations, known as methods. While
it's good practice for generic functions to have a single purpose, Julia
allows methods to be documented individually if necessary. For example:

.. doctest::

    @doc doc"""
      Multiplication operator. `x*y*z*...` calls this function with multiple
      arguments, i.e. `*(x,y,z...)`.
      """ ->
    function *(x, y)
      # ... [implementation sold seperately] ...
    end

    @doc doc"""
      When applied to strings, concatenates them.
      """
    function *(x::String, y::String)
      # ... [insert secret sauce here] ...
    end

    help?>*
    Multiplication operator. `x*y*z*...` calls this function with multiple
    arguments, i.e. `*(x,y,z...)`.

    When applied to strings, concatenates them.

When retrieving documentation for a generic function, the metadata for each
method is concatenated with the ``catdoc`` function.

Notes
-----

Julia 0.4 will introduce the more convenient syntax

.. doctest::

    "..."
    f(x) = ...

but this is not yet implemented.
