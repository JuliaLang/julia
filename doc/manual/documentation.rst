.. _man-documentation:

***************
 Documentation
***************

Julia enables package developers and users to document functions, types and
other objects easily via a built-in documentation system since Julia 0.4.

.. tip::

    This documentation system can also be used in Julia 0.3 via the
    `Docile.jl <https://github.com/MichaelHatherly/Docile.jl>`_ package; see
    the documentation for that package for more details.

The basic syntax is very simple: any string appearing at the top-level right
before an object (function, macro, type or instance) will be interpreted as
documenting it (these are called *docstrings*). Here is a very simple example:

.. doctest::

    "Tell whether there are too foo items in the array."
    foo(xs::Array) = ...

Documentation is interpreted as `Markdown <https://en.wikipedia.org/wiki/Markdown>`_,
so you can use indentation and code fences to delimit code examples from text.
Technically, any object can be associated with any other as metadata;
Markdown happens to be the default, but one can construct other string
macros and pass them to the ``@doc`` macro just as well.

Here is a more complex example, still using Markdown:

    """
        bar(x[, y])

    Compute the Bar index between `x` and `y`. If `y` is missing, compute
    the Bar index between all pairs of columns of `x`.

    # Examples
    ```julia
    julia> bar([1, 2], [1, 2])
    1
    ```
    """
    function bar(x, y) ...

As in the example above, we recommend following some simple conventions when writing
documentation:

1. Always show the signature of a function at the top of the documentation,
with a four-space indent so that it is printed as Julia code.

  This can be identical to the signature present in the Julia code
  (like ``mean(x::AbstractArray)``), or a simplified form.
  Optional arguments should be represented with their default values (i.e. ``f(x, y=1)``)
  when possible, following the actual Julia syntax. Optional arguments which
  do not have a default value should be put in brackets (i.e. ``f(x[, y])`` and
  ``f(x[, y[, z]])``). An alternative solution is to use several lines: one without
  optional arguments, the other(s) with them. This solution can also be used to document
  several related methods of a given function. When a function accepts many keyword
  arguments, only include a ``<keyword arguments>`` placeholder in the signature (i.e.
  ``f(x; <keyword arguments>)``), and give the complete list under an ``# Arguments``
  section (see point 4 below).

2. Include a single one-line sentence describing what the function does or what the
object represents after the simplified signature block. If needed, provide more details
in a second paragraph, after a blank line.

  The one-line sentence should use the imperative form ("Do this", "Return that") instead
  of the third person (do not write "Returns the length...") when documenting functions.
  It should end with a period. If the meaning of a function cannot be summarized easily,
  splitting it into separate composable parts could be beneficial (this should not be
  taken as an absolute requirement for every single case though).

3. Do not repeat yourself.

  Since the function name is given by the signature, there is no need to
  start the documentation with "The function ``bar``...": go straight to the point.
  Similarly, if the signature specifies the types of the arguments, mentioning them
  in the description is redundant.

4. Only provide an argument list when really necessary.

  For simple functions, it is often clearer to mention the role of the arguments directly
  in the description of the function's purpose. An argument list would only repeat
  information already provided elsewhere. However, providing an argument list can be a good
  idea for complex functions with many arguments (in particular keyword arguments).
  In that case, insert it after the general description of the function, under
  an ``# Arguments`` header, with one ``*`` bullet for each argument. The list should
  mention the types and default values (if any) of the arguments::

    """
    ...
    # Arguments
    * `n::Integer`: the number of elements to compute.
    * `dim::Integer=1`: the dimensions along which to perform the computation.
    ...
    """

5. Group examples under an ``# Examples`` section and use ````julia`` blocks instead of
standard text.

  Examples should consist of verbatim copies of the Julia REPL, including the ``julia>``
  prompt (see example above). This will be used in the future to allow running examples
  automatically and checking that their actual output is consistent with that presented
  in the documentation (a feature called *doctests*). This way, the code will be tested and
  examples won't get out of date without notice.

6. Use backticks to identify code and equations.

  Julia identifiers and code excerpts should always appear between backticks `````
  to enable highlighting. Equations in the LaTeX syntax can be inserted between
  double backticks ``````. Use Unicode characters rather than their LaTeX escape sequence,
  i.e. ````α = 1```` rather than :samp:`\`\`\\\\alpha = 1\`\``.

7. Place the starting and ending ``"""`` characters on lines by themselves.

  That is, write::

    """
    ...

    ...
    """
    f(x, y) = ...

  rather than::

    """...

    ..."""
    f(x, y) = ...

  This makes it more clear where docstrings start and end.

8. Respect the line length limit used in the surrounding code.

  Docstrings are edited using the same tools as code. Therefore, the same conventions
  should apply. It it advised to add line breaks after 92 characters.

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
Julia allows methods to be documented individually if necessary.
In general, only the most generic method should be documented, or even the
function itself (i.e. the object created without any methods by
``function bar end``). Specific methods should only be documented if their
behaviour differs from the more generic ones. In any case, they should not
repeat the information provided elsewhere. For example:

.. doctest::

    """
    Multiplication operator. `x*y*z*...` calls this function with multiple
    arguments, i.e. `*(x,y,z...)`.
    """
    function *(x, y)
      # ... [implementation sold separately] ...
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

Adds docstring ``"..."`` to expression generated by expanding ``@m expression``. This allows
for expressions decorated with ``@inline``, ``@noinline``, ``@generated``, or any other
macro to be documented in the same way as undecorated expressions.

Macro authors should take note that only macros that generate a single expression will
automatically support docstrings. If a macro returns a block containing multiple
subexpressions then the subexpression that should be documented must be marked using the
:func:`@__doc__` macro.

The ``@enum`` macro makes use of ``@__doc__`` to allow for documenting ``Enum``\ s.
Examining it's definition should serve as an example of how to use ``@__doc__`` correctly.

.. function:: @__doc__(ex)

   .. Docstring generated from Julia source

   Low-level macro used to mark expressions returned by a macro that should be documented. If more than one expression is marked then the same docstring is applied to each expression.

   .. code-block:: julia

       macro example(f)
           quote
               $(f)() = 0
               @__doc__ $(f)(x) = 1
               $(f)(x, y) = 2
           end |> esc
       end

   ``@__doc__`` has no effect when a macro that uses it is not documented.

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
