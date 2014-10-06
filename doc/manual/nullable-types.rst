.. _man-nullable-types:

*******************************************
Nullable Types: Representing Missing Values
*******************************************

In many settings, you need to interact with a value of type ``T`` that may or
may not exist. To handle these settings, Julia provides a parametric type
called ``Nullable{T}``, which can be thought of as a specialized container
type that can contain either zero or one values. ``Nullable{T}`` provides a
minimal interface designed to ensure that interactions with missing values
are safe. At present, the interface consists of four possible interactions:

- Construct a ``Nullable`` object.
- Check if an ``Nullable`` object has a missing value.
- Access the value of a ``Nullable`` object with a guarantee that a
  ``NullException`` will be thrown if the object's value is missing.
- Access the value of a ``Nullable`` object with a guarantee that a default
  value of type ``T`` will be returned if the object's value is missing.

Constructing ``Nullable`` objects
---------------------------------

To construct an object representing a missing value of type ``T``, use the
``Nullable{T}()`` function:

.. doctest::

    x1 = Nullable{Int}()
    x2 = Nullable{Float64}()
    x3 = Nullable{Vector{Int}}()

To construct an object representing a non-missing value of type ``T``, use the
``Nullable(x::T)`` function:

.. doctest::

    x1 = Nullable(1)
    x2 = Nullable(1.0)
    x3 = Nullable([1, 2, 3])

Note the core distinction between these two ways of constructing a ``Nullable``
object: in one style, you provide a type, ``T``, as a function parameter; in
the other style, you provide a single value of type ``T`` as an argument.

Checking if an ``Nullable`` object has a value
----------------------------------------------

You can check if a ``Nullable`` object has any value using the ``isnull``
function:

.. doctest::

    isnull(Nullable{Float64}())
    isnull(Nullable(0.0))

Safely accessing the value of an ``Nullable`` object
----------------------------------------------------

You can safely access the value of an ``Nullable`` object using the ``get``
function:

.. doctest::

    get(Nullable{Float64}())
    get(Nullable(1.0))

If the value is not present, as it would be for ``Nullable{Float64}``, a
``NullException`` error will be thrown. The error-throwing nature of the
``get`` function ensures that any attempt to access a missing value immediately
fails.

In cases for which a reasonable default value exists that could be used
when a ``Nullable`` object's value turns out to be missing, you can provide this
default value as a second argument to ``get``:

.. doctest::

    get(Nullable{Float64}(), 0)
    get(Nullable(1.0), 0)

Note that this default value will automatically be converted to the type of
the ``Nullable`` object that you attempt to access using the ``get`` function.
For example, in the code shown above the value ``0`` would be automatically
converted to a ``Float64`` value before being returned. The presence of default
replacement values makes it easy to use the ``get`` function to write
type-stable code that interacts with sources of potentially missing values.
