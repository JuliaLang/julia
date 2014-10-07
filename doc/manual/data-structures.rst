
***********
 Data Structures
***********

There are a number of basic data structures built into Julia. These include
variables, arrays (1- and multi-dimensional), tuples, types (similar to
structures), Dicts (associative collections), and Sets.

Variables
=========

These typically contain scalar values, as explained in the Variables section.

.. doctest:: array-rand

    julia> const x = rand(8)
    8-element Array{Float64,1}:
     0.843025
     0.869052
     0.365105
     0.699456
     0.977653
     0.994953
     0.41084
     0.809411

    julia> [ 0.25*x[i-1] + 0.5*x[i] + 0.25*x[i+1] for i=2:length(x)-1 ]
    6-element Array{Float64,1}:
     0.736559
     0.57468
     0.685417
     0.912429
     0.8446
     0.656511

Arrays (1-dimensional)
======================

Julia has first-class arrays, which may be accessed and assigned using square
bracket notion. Arrays must be pre-allocated or allocated by assignment, and are
not resizeable. For example,

.. doctest::

    # Assign an array to x, automatically allocates memory
    julia> x = [1, 2, 10]
    3-element Array{Int64,1}:
     1
     2
    10

    # Access entries using square brackets
    julia> x[1] + x[3]
    11

    # Can't assign an additional entry, which hasn't been allocated
    julia> x[4] = 1
    ERROR: BoundsError()

Arrays usually have a type, which may be assigned explicitly

    x = Float64[1, 2, 10]

One-dimensional arrays may be regarded as having column orientation,
which may be convenient when multiplying by a two-dimensional array.



Arrays (multi-dimensional)
==========================

Arrays may have multiple dimensions. In the two-dimensional case, a
Matlab-like notation can be used, in which case the square brackets are
used with space between elements across a row, and rows demarcated by
semicolons or carriage returns:

.. doctest::

    # 2D array with spaces and semi-colons
    julia> A = [1 2 3; 4 5 6]
    2x3 Array{Int64,2}:
     1  2  3
     4  5  6

    # with carriage return
    julia> B = [1 2 3
    4 5 6]
    2x3 Array{Int64,2}:
     1  2  3
     4  5  6

    # a row vector is actually a 2D array (1 x N)
    julia> y = [1 1 1]
    1x3 Array{Int64,2}:
     1  1  1

    # transpose a 1D array to make it a row
    julia> x = [1, 2, 3]
    x'
    1x3 Array{Int64,2}:
     1  2  3

    # warning: tranposing a row returns a column array, not a 1D array
    julia> x''
    3x1 Array{Int64,2}:
     1
     2
     3

    julia> x'' == x
    false

Multi-dimensional arrays may also be constructed. The colon operator can be
used to access slices. Similar to Matlab, the keyword ``end`` is recognized.

.. doctest::

    # allocate and initialize a three-dimensional array
    julia> A = ones(Int, 2, 3, 1)
    2x3x1 Array{Int64,3}:
    [:, :, 1] =
     1  1  1
     1  1  1

    julia> A[1,:,:]
    1x3x1 Array{Int64,3}:
    [:, :, 1] =
     1  1  1

    julia> A[:,1,:]
    2x1x1 Array{Int64,3}:
    [:, :, 1] =
     1
     1

    julia> A[:,2:end,1]
    2x2 Array{Int64,2}:
     1  1
     1  1



Arrays of Arrays
================

Tuples
======

Types (data structures)
=======================

Dicts (associative collections)
===============================

Sets (unordered lists)
======================

Unicode names (in UTF-8 encoding) are allowed:



Julia will even let you redefine built-in constants and functions if needed:

.. doctest::

    julia> pi
    π = 3.1415926535897...

    julia> pi = 3
    Warning: imported binding for pi overwritten in module Main
    3

    julia> pi
    3

    julia> sqrt(100)
    10.0

    julia> sqrt = 4
    Warning: imported binding for sqrt overwritten in module Main
    4

However, this is obviously not recommended to avoid potential confusion.

Allowed Variable Names
======================

Variable names must begin with a letter (A-Z or a-z), underscore, or a
subset of Unicode code points greater than 00A0; in particular, `Unicode character categories`_ Lu/Ll/Lt/Lm/Lo/Nl (letters), Sc/So (currency and
other symbols), and a few other letter-like characters (e.g. a subset
of the Sm math symbols) are allowed. Subsequent characters may also
include ! and digits (0-9 and other characters in categories Nd/No),
as well as other Unicode code points: diacritics and other modifying
marks (categories Mn/Mc/Me/Sk), some punctuation connectors (category
Pc), primes, and a few other characters.

.. _Unicode character categories: http://www.fileformat.info/info/unicode/category/index.htm

Operators like ``+`` are also valid identifiers, but are parsed specially. In
some contexts, operators can be used just like variables; for example
``(+)`` refers to the addition function, and ``(+) = f`` will reassign
it.  Most of the Unicode infix operators (in category Sm),
such as ``⊕``, are parsed as infix operators and are available for
user-defined methods (e.g. you can use ``const ⊗ = kron`` to define
``⊗`` as an infix Kronecker product).

The only explicitly disallowed names for variables are the names of built-in
statements:

.. doctest::

    julia> else = false
    ERROR: syntax: unexpected "else"

    julia> try = "No"
    ERROR: syntax: unexpected "="


Stylistic Conventions
=====================

While Julia imposes few restrictions on valid names, it has become useful to
adopt the following conventions:

- Names of variables are in lower case.
- Word separation can be indicated by underscores (``'_'``), but use of
  underscores is discouraged unless the name would be hard to read otherwise.
- Names of ``Type``\ s begin with a capital letter and word separation is
  shown with CamelCase instead of underscores.
- Names of ``function``\ s and ``macro``\s are in lower case, without
  underscores.
- Functions that modify their inputs have names that end in ``!``. These
  functions are sometimes called mutating functions or in-place functions.
