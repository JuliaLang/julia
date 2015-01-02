.. currentmodule:: Base

**********************
 The Standard Library
**********************

Introduction
------------

The Julia standard library contains a range of functions and macros appropriate for performing scientific and numerical computing, but is also as broad as those of many general purpose programming languages.  Additional functionality is available from a growing collection of available packages. Functions are grouped by topic below.

Some general notes:

* Except for functions in built-in modules (:mod:`~Base.Pkg`, :mod:`~Base.Collections`, :mod:`~Base.Graphics`,
  :mod:`~Base.Test` and :mod:`~Base.Profile`), all functions documented here are directly available for use in programs.
* To use module functions, use ``import Module`` to import the module, and ``Module.fn(x)`` to use the functions.
* Alternatively, ``using Module`` will import all exported ``Module`` functions into the current namespace.
* By convention, function names ending with an exclamation point (``!``) modify their arguments.  Some functions have both modifying (e.g., ``sort!``) and non-modifying (``sort``) versions.

Getting Around
--------------

.. function:: exit([code])

   Quit (or control-D at the prompt). The default exit code is zero, indicating that the processes completed successfully.

.. function:: quit()

   Quit the program indicating that the processes completed successfully. This function calls ``exit(0)`` (see :func:`exit`).

.. function:: atexit(f)

   Register a zero-argument function to be called at exit.

.. function:: isinteractive() -> Bool

   Determine whether Julia is running an interactive session.

.. function:: whos([Module,] [pattern::Regex])

   Print information about exported global variables in a module, optionally restricted
   to those matching ``pattern``.

.. function:: edit(file::AbstractString, [line])

   Edit a file optionally providing a line number to edit at. Returns to the julia prompt when you quit the editor.

.. function:: edit(function, [types])

   Edit the definition of a function, optionally specifying a tuple of types to indicate which method to edit.

.. function:: @edit

   Evaluates the arguments to the function call, determines their types, and calls the ``edit`` function on the resulting expression

.. function:: less(file::AbstractString, [line])

   Show a file using the default pager, optionally providing a starting line number. Returns to the julia prompt when you quit the pager.

.. function:: less(function, [types])

   Show the definition of a function using the default pager, optionally specifying a tuple of types to indicate which method to see.

.. function:: @less

   Evaluates the arguments to the function call, determines their types, and calls the ``less`` function on the resulting expression

.. function:: clipboard(x)

   Send a printed form of ``x`` to the operating system clipboard ("copy").

.. function:: clipboard() -> AbstractString

   Return a string with the contents of the operating system clipboard ("paste").

.. function:: require(file::AbstractString...)

   Load source files once, in the context of the ``Main`` module, on every active node, searching standard locations for files. ``require`` is considered a top-level operation, so it sets the current ``include`` path but does not use it to search for files (see help for ``include``). This function is typically used to load library code, and is implicitly called by ``using`` to load packages.

   When searching for files, ``require`` first looks in the current working directory, then looks for package code under ``Pkg.dir()``, then tries paths in the global array ``LOAD_PATH``.

.. function:: reload(file::AbstractString)

   Like ``require``, except forces loading of files regardless of whether they have been loaded before. Typically used when interactively developing libraries.

.. function:: include(path::AbstractString)

   Evaluate the contents of a source file in the current context. During including, a task-local include path is set to the directory containing the file. Nested calls to ``include`` will search relative to that path. All paths refer to files on node 1 when running in parallel, and files will be fetched from node 1. This function is typically used to load source interactively, or to combine files in packages that are broken into multiple source files.

.. function:: include_string(code::AbstractString)

   Like ``include``, except reads code from the given string rather than from a file. Since there is no file path involved, no path processing or fetching from node 1 is done.

.. function:: help(name)

   Get help for a function. ``name`` can be an object or a string.

.. function:: apropos(string)

   Search documentation for functions related to ``string``.

.. function:: which(f, types)

   Return the method of ``f`` (a ``Method`` object) that will be called for arguments with the given types.

.. function:: @which

   Evaluates the arguments to the function call, determines their types, and calls the ``which`` function on the resulting expression

.. function:: methods(f, [types])

   Show all methods of ``f`` with their argument types.

   If ``types`` is specified, an array of methods whose types match is returned.

.. function:: methodswith(typ[, module or function][, showparents])

   Return an array of methods with an argument of type ``typ``. If optional
   ``showparents`` is ``true``, also return arguments with a parent type
   of ``typ``, excluding type ``Any``.

   The optional second argument restricts the search to a particular module
   or function.

.. function:: @show

   Show an expression and result, returning the result

.. function:: versioninfo([verbose::Bool])

   Print information about the version of Julia in use. If the ``verbose`` argument
   is true, detailed system information is shown as well.

.. function:: workspace()

   Replace the top-level module (``Main``) with a new one, providing a clean workspace.
   The previous ``Main`` module is made available as ``LastMain``. A previously-loaded
   package can be accessed using a statement such as ``using LastMain.Package``.

   This function should only be used interactively.

All Objects
-----------

.. function:: is(x, y) -> Bool
              ===(x,y) -> Bool
              ≡(x,y) -> Bool

   Determine whether ``x`` and ``y`` are identical, in the sense that no program could distinguish them. Compares mutable objects by address in memory, and compares immutable objects (such as numbers) by contents at the bit level. This function is sometimes called ``egal``.

.. function:: isa(x, type) -> Bool

   Determine whether ``x`` is of the given ``type``.

.. function:: isequal(x, y)

   Similar to ``==``, except treats all floating-point ``NaN`` values as equal to each other,
   and treats ``-0.0`` as unequal to ``0.0``.
   The default implementation of ``isequal`` calls ``==``, so if you have a type that doesn't have these floating-point subtleties then you probably only need to define ``==``.

   ``isequal`` is the comparison function used by hash tables (``Dict``).
   ``isequal(x,y)`` must imply that ``hash(x) == hash(y)``.

   This typically means that if you define your own ``==`` function then you must define a corresponding ``hash`` (and vice versa).  Collections typically implement ``isequal`` by calling ``isequal`` recursively on
   all contents.

   Scalar types generally do not need to implement ``isequal`` separate from ``==``, unless they
   represent floating-point numbers amenable to a more efficient implementation
   than that provided as a generic fallback (based on ``isnan``, ``signbit``, and ``==``).

.. function:: isless(x, y)

   Test whether ``x`` is less than ``y``, according to a canonical total order.
   Values that are normally unordered, such as ``NaN``, are ordered in an arbitrary but consistent fashion. This is the default comparison used by ``sort``. Non-numeric types with a canonical total order should implement this function. Numeric types only need to implement it if they have special values such as ``NaN``.

.. function:: ifelse(condition::Bool, x, y)

   Return ``x`` if ``condition`` is true, otherwise return ``y``. This
   differs from ``?`` or ``if`` in that it is an ordinary function, so
   all the arguments are evaluated first. In some cases, using
   ``ifelse`` instead of an ``if`` statement can eliminate the branch
   in generated code and provide higher performance in tight loops.

.. function:: lexcmp(x, y)

   Compare ``x`` and ``y`` lexicographically and return -1, 0, or 1 depending on whether ``x`` is less than, equal to, or greater than ``y``, respectively.
   This function should be defined for lexicographically comparable types, and ``lexless`` will call ``lexcmp`` by default.

.. function:: lexless(x, y)

   Determine whether ``x`` is lexicographically less than ``y``.

.. function:: typeof(x)

   Get the concrete type of ``x``.

.. function:: tuple(xs...)

   Construct a tuple of the given objects.

.. function:: ntuple(n, f::Function)

   Create a tuple of length ``n``, computing each element as ``f(i)``, where ``i`` is the index of the element.

.. function:: object_id(x)

   Get a unique integer id for ``x``. ``object_id(x)==object_id(y)`` if and only if ``is(x,y)``.

.. function:: hash(x[, h])

   Compute an integer hash code such that ``isequal(x,y)`` implies ``hash(x)==hash(y)``.
   The optional second argument ``h`` is a hash code to be mixed with the result.

   New types should implement the 2-argument form, typically  by calling the 2-argument ``hash`` method recursively in order to mix hashes of the contents with each other (and with ``h``).   Typically, any type that implements ``hash`` should also implement its own ``==`` (hence ``isequal``) to guarantee the property mentioned above.

.. function:: finalizer(x, function)

   Register a function ``f(x)`` to be called when there are no program-accessible references to ``x``. The behavior of this function is unpredictable if ``x`` is of a bits type.

.. function:: finalize(x)

   Immediately run finalizers registered for object ``x``.

.. function:: copy(x)

   Create a shallow copy of ``x``: the outer structure is copied, but not all internal values. For example, copying an array produces a new array with identically-same elements as the original.

.. function:: deepcopy(x)

   Create a deep copy of ``x``: everything is copied recursively, resulting in a fully independent object. For example, deep-copying an array produces a new array whose elements are deep copies of the original elements. Calling `deepcopy` on an object should generally have the same effect as serializing and then deserializing it.

   As a special case, functions can only be actually deep-copied if they are anonymous, otherwise they are just copied. The difference is only relevant in the case of closures, i.e. functions which may contain hidden internal references.

   While it isn't normally necessary, user-defined types can override the default ``deepcopy`` behavior by defining a specialized version of the function ``deepcopy_internal(x::T, dict::ObjectIdDict)`` (which shouldn't otherwise be used), where ``T`` is the type to be specialized for, and ``dict`` keeps track of objects copied so far within the recursion. Within the definition, ``deepcopy_internal`` should be used in place of ``deepcopy``, and the ``dict`` variable should be updated as appropriate before returning.

.. function:: isdefined([object,] index | symbol)

   Tests whether an assignable location is defined. The arguments can be an
   array and index, a composite object and field name (as a symbol), or a
   module and a symbol.
   With a single symbol argument, tests whether a global variable with that
   name is defined in ``current_module()``.

.. function:: convert(T, x)

   Convert ``x`` to a value of type ``T``.

   If ``T`` is an ``Integer`` type, an ``InexactError`` will be raised if
   ``x`` is not representable by ``T``, for example if ``x`` is not
   integer-valued, or is outside the range supported by ``T``.

   .. doctest::

      julia> convert(Int, 3.0)
      3

      julia> convert(Int, 3.5)
      ERROR: InexactError()
       in convert at int.jl:185

   If ``T`` is a ``FloatingPoint`` or ``Rational`` type, then it will return
   the closest value to ``x`` representable by ``T``.

   .. doctest::

      julia> x = 1/3
      0.3333333333333333

      julia> convert(Float32, x)
      0.33333334f0

      julia> convert(Rational{Int32}, x)
      1//3

      julia> convert(Rational{Int64}, x)
      6004799503160661//18014398509481984

.. function:: promote(xs...)

   Convert all arguments to their common promotion type (if any), and return them all (as a tuple).

.. function:: oftype(x, y)

   Convert ``y`` to the type of ``x`` (``convert(typeof(x), y)``).

.. function:: widen(type | x)

   If the argument is a type, return a "larger" type (for numeric types, this will be
   a type with at least as much range and precision as the argument, and usually more).
   Otherwise the argument ``x`` is converted to ``widen(typeof(x))``.

   .. doctest::

	   julia> widen(Int32)
	   Int64

   .. doctest::

	   julia> widen(1.5f0)
	   1.5

.. function:: identity(x)

   The identity function. Returns its argument.

Types
-----

.. function:: super(T::DataType)

   Return the supertype of DataType T

.. function:: issubtype(type1, type2)

   True if and only if all values of ``type1`` are also of ``type2``. Can also be written using the ``<:`` infix operator as ``type1 <: type2``.

.. function:: <:(T1, T2)

   Subtype operator, equivalent to ``issubtype(T1,T2)``.

.. function:: subtypes(T::DataType)

   Return a list of immediate subtypes of DataType T.  Note that all currently loaded subtypes are included, including those not visible in the current module.

.. function:: subtypetree(T::DataType)

   Return a nested list of all subtypes of DataType T.  Note that all currently loaded subtypes are included, including those not visible in the current module.

.. function:: typemin(type)

   The lowest value representable by the given (real) numeric type.

.. function:: typemax(type)

   The highest value representable by the given (real) numeric type.

.. function:: realmin(type)

   The smallest in absolute value non-subnormal value representable by the given floating-point type

.. function:: realmax(type)

   The highest finite value representable by the given floating-point type

.. function:: maxintfloat(type)

   The largest integer losslessly representable by the given floating-point type

.. function:: sizeof(type)

   Size, in bytes, of the canonical binary representation of the given type, if any.

.. function:: eps([type])

   The distance between 1.0 and the next larger representable floating-point value of ``type``. Only floating-point types are sensible arguments. If ``type`` is omitted, then ``eps(Float64)`` is returned.

.. function:: eps(x)

   The distance between ``x`` and the next larger representable floating-point value of the same type as ``x``.

.. function:: promote_type(type1, type2)

   Determine a type big enough to hold values of each argument type without loss, whenever possible. In some cases, where no type exists to which both types can be promoted losslessly, some loss is tolerated; for example, ``promote_type(Int64,Float64)`` returns ``Float64`` even though strictly, not all ``Int64`` values can be represented exactly as ``Float64`` values.

.. function:: promote_rule(type1, type2)

   Specifies what type should be used by ``promote`` when given values of types
   ``type1`` and ``type2``. This function should not be called directly, but
   should have definitions added to it for new types as appropriate.

.. function:: getfield(value, name::Symbol)

   Extract a named field from a value of composite type. The syntax ``a.b`` calls
   ``getfield(a, :b)``, and the syntax ``a.(b)`` calls ``getfield(a, b)``.

.. function:: setfield!(value, name::Symbol, x)

   Assign ``x`` to a named field in ``value`` of composite type.
   The syntax ``a.b = c`` calls ``setfield!(a, :b, c)``, and the syntax ``a.(b) = c``
   calls ``setfield!(a, b, c)``.

.. function:: fieldoffsets(type)

   The byte offset of each field of a type relative to the data start. For example, we could use it
   in the following manner to summarize information about a struct type:

   .. doctest::

      julia> structinfo(T) = [zip(fieldoffsets(T),names(T),T.types)...];

      julia> structinfo(StatStruct)
      12-element Array{(Int64,Symbol,DataType),1}:
       (0,:device,UInt64)
       (8,:inode,UInt64)
       (16,:mode,UInt64)
       (24,:nlink,Int64)
       (32,:uid,UInt64)
       (40,:gid,UInt64)
       (48,:rdev,UInt64)
       (56,:size,Int64)
       (64,:blksize,Int64)
       (72,:blocks,Int64)
       (80,:mtime,Float64)
       (88,:ctime,Float64)

.. function:: fieldtype(type, name::Symbol | index::Int)

   Determine the declared type of a field (specified by name or index) in a composite type.

.. function:: isimmutable(v)

   True if value ``v`` is immutable.  See :ref:`man-immutable-composite-types` for a discussion of immutability.
   Note that this function works on values, so if you give it a type, it will tell you that a value of ``DataType`` is mutable.

.. function:: isbits(T)

   True if ``T`` is a "plain data" type, meaning it is immutable and contains no references to other values. Typical examples are numeric types such as ``UInt8``, ``Float64``, and ``Complex{Float64}``.

   .. doctest::

      julia> isbits(Complex{Float64})
      true

      julia> isbits(Complex)
      false

.. function:: isleaftype(T)

   Determine whether ``T`` is a concrete type that can have instances, meaning
   its only subtypes are itself and ``None`` (but ``T`` itself is not
   ``None``).

.. function:: typejoin(T, S)

   Compute a type that contains both ``T`` and ``S``.

.. function:: typeintersect(T, S)

   Compute a type that contains the intersection of ``T`` and ``S``. Usually this will be the smallest such type or one close to it.

Generic Functions
-----------------

.. function:: apply(f, x...)

   Accepts a function and several arguments, each of which must be iterable.
   The elements generated by all the arguments are appended into a single
   list, which is then passed to ``f`` as its argument list.

   .. doctest::

   	julia> function f(x, y) # Define a function f
   	           x + y
   	       end;

   	julia> apply(f, [1 2]) # Apply f with 1 and 2 as arguments
   	3

   ``apply`` is called to implement the ``...`` argument splicing syntax,
   and is usually not called directly: ``apply(f,x) === f(x...)``

.. function:: method_exists(f, tuple) -> Bool

   Determine whether the given generic function has a method matching the given tuple of argument types.

   .. doctest::

   	julia> method_exists(length, (Array,))
   	true

.. function:: applicable(f, args...) -> Bool

   Determine whether the given generic function has a method applicable to the given arguments.

   .. doctest::

   	julia> function f(x, y)
   	           x + y
   	       end;

   	julia> applicable(f, 1)
   	false

   	julia> applicable(f, 1, 2)
   	true

.. function:: invoke(f, (types...), args...)

   Invoke a method for the given generic function matching the specified types (as a tuple), on the specified arguments. The arguments must be compatible with the specified types. This allows invoking a method other than the most specific matching method, which is useful when the behavior of a more general definition is explicitly needed (often as part of the implementation of a more specific method of the same function).

.. function:: |>(x, f)

   Applies a function to the preceding argument. This allows for easy function chaining.

   .. doctest::

      julia> [1:5] |> x->x.^2 |> sum |> inv
      0.01818181818181818

Syntax
------

.. function:: eval([m::Module], expr::Expr)

   Evaluate an expression in the given module and return the result.
   Every module (except those defined with ``baremodule``) has its own 1-argument definition
   of ``eval``, which evaluates expressions in that module.

.. function:: @eval

   Evaluate an expression and return the value.

.. function:: evalfile(path::AbstractString)

   Evaluate all expressions in the given file, and return the value of the last one. No other processing (path searching, fetching from node 1, etc.) is performed.

.. function:: esc(e::ANY)

   Only valid in the context of an Expr returned from a macro. Prevents the macro hygiene pass from turning embedded variables into gensym variables. See the :ref:`man-macros`
   section of the Metaprogramming chapter of the manual for more details and examples.

.. function:: gensym([tag])

   Generates a symbol which will not conflict with other variable names.

.. function:: @gensym

   Generates a gensym symbol for a variable. For example, ``@gensym x y`` is transformed into ``x = gensym("x"); y = gensym("y")``.

.. function:: parse(str, start; greedy=true, raise=true)

   Parse the expression string and return an expression (which could later be passed to eval for execution). Start is the index of the first character to start parsing. If ``greedy`` is true (default), ``parse`` will try to consume as much input as it can; otherwise, it will stop as soon as it has parsed a valid expression. If ``raise`` is true (default), syntax errors will raise an error; otherwise, ``parse`` will return an expression that will raise an error upon evaluation.

.. function:: parse(str; raise=true)

   Parse the whole string greedily, returning a single expression.  An error is thrown if there are additional characters after the first expression. If ``raise`` is true (default), syntax errors will raise an error; otherwise, ``parse`` will return an expression that will raise an error upon evaluation.

Iteration
---------

Sequential iteration is implemented by the methods ``start``, ``done``, and
``next``. The general ``for`` loop::

    for i = I   # or  "for i in I"
      # body
    end

is translated to::

    state = start(I)
    while !done(I, state)
      (i, state) = next(I, state)
      # body
    end

The ``state`` object may be anything, and should be chosen appropriately for each iterable type.

.. function:: start(iter) -> state

   Get initial iteration state for an iterable object

.. function:: done(iter, state) -> Bool

   Test whether we are done iterating

.. function:: next(iter, state) -> item, state

   For a given iterable object and iteration state, return the current item and the next iteration state

.. function:: zip(iters...)

   For a set of iterable objects, returns an iterable of tuples, where the ``i``\ th tuple contains the ``i``\ th component of each input iterable.

   Note that ``zip`` is its own inverse: ``[zip(zip(a...)...)...] == [a...]``.

.. function:: enumerate(iter)

   Return an iterator that yields ``(i, x)`` where ``i`` is an index starting at 1, and ``x`` is the ``i``\ th value from the given iterator. It's useful when you need not only the values ``x`` over which you are iterating, but also the index ``i`` of the iterations.

   .. doctest::

   	julia> a = ["a", "b", "c"];

   	julia> for (index, value) in enumerate(a)
                   println("$index $value")
               end
        1 a
        2 b
        3 c


Fully implemented by: ``Range``, ``UnitRange``, ``NDRange``, ``Tuple``, ``Real``, ``AbstractArray``, ``IntSet``, ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``, ``EachLine``, ``AbstractString``, ``Set``, ``Task``.

General Collections
-------------------

.. function:: isempty(collection) -> Bool

   Determine whether a collection is empty (has no elements).

   .. doctest::

   	julia> isempty([])
   	true

   	julia> isempty([1 2 3])
   	false

.. function:: empty!(collection) -> collection

   Remove all elements from a ``collection``.

.. function:: length(collection) -> Integer

   For ordered, indexable collections, the maximum index ``i`` for which ``getindex(collection, i)`` is valid. For unordered collections, the number of elements.

.. function:: endof(collection) -> Integer

   Returns the last index of the collection.

   .. doctest::

   	julia> endof([1,2,4])
   	3

Fully implemented by: ``Range``, ``UnitRange``, ``Tuple``, ``Number``, ``AbstractArray``, ``IntSet``, ``Dict``, ``WeakKeyDict``, ``AbstractString``, ``Set``.

Iterable Collections
--------------------

.. function:: in(item, collection) -> Bool
              ∈(item,collection) -> Bool
              ∋(collection,item) -> Bool
              ∉(item,collection) -> Bool
              ∌(collection,item) -> Bool

   Determine whether an item is in the given collection, in the sense that it is
   ``==`` to one of the values generated by iterating over the collection.
   Some collections need a slightly different definition; for example Sets
   check whether the item is ``isequal`` to one of the elements. Dicts look for
   ``(key,value)`` pairs, and the key is compared using ``isequal``. To test
   for the presence of a key in a dictionary, use ``haskey`` or
   ``k in keys(dict)``.

.. function:: eltype(collection)

   Determine the type of the elements generated by iterating ``collection``.
   For associative collections, this will be a ``(key,value)`` tuple type.

.. function:: indexin(a, b)

   Returns a vector containing the highest index in ``b``
   for each value in ``a`` that is a member of ``b`` .
   The output vector contains 0 wherever ``a`` is not a member of ``b``.

.. function:: findin(a, b)

   Returns the indices of elements in collection ``a`` that appear in collection ``b``

.. function:: unique(itr[, dim])

   Returns an array containing only the unique elements of the iterable ``itr``, in
   the order that the first of each set of equivalent elements originally appears.
   If ``dim`` is specified, returns unique regions of the array ``itr`` along ``dim``.

.. function:: reduce(op, v0, itr)

   Reduce the given collection ``ìtr`` with the given binary operator
   ``op``. ``v0`` must be a neutral element for ``op`` that will be
   returned for empty collections. It is unspecified whether ``v0`` is
   used for non-empty collections.

   Reductions for certain commonly-used operators have special
   implementations which should be used instead: ``maximum(itr)``,
   ``minimum(itr)``, ``sum(itr)``, ``prod(itr)``, ``any(itr)``,
   ``all(itr)``.

   The associativity of the reduction is implementation-dependent.
   This means that you can't use non-associative operations like ``-``
   because it is undefined whether ``reduce(-,[1,2,3])`` should be
   evaluated as ``(1-2)-3`` or ``1-(2-3)``. Use ``foldl`` or ``foldr``
   instead for guaranteed left or right associativity.

   Some operations accumulate error, and parallelism will also be
   easier if the reduction can be executed in groups. Future versions
   of Julia might change the algorithm. Note that the elements are not
   reordered if you use an ordered collection.

.. function:: reduce(op, itr)

   Like ``reduce(op, v0, itr)``. This cannot be used with empty
   collections, except for some special cases (e.g. when ``op`` is one
   of ``+``, ``*``, ``max``, ``min``, ``&``, ``|``) when Julia can
   determine the neutral element of ``op``.

.. function:: foldl(op, v0, itr)

   Like ``reduce``, but with guaranteed left associativity. ``v0``
   will be used exactly once.

.. function:: foldl(op, itr)

   Like ``foldl(op, v0, itr)``, but using the first element of ``itr``
   as ``v0``. In general, this cannot be used with empty collections
   (see ``reduce(op, itr)``).

.. function:: foldr(op, v0, itr)

   Like ``reduce``, but with guaranteed right associativity. ``v0``
   will be used exactly once.

.. function:: foldr(op, itr)

   Like ``foldr(op, v0, itr)``, but using the last element of ``itr``
   as ``v0``. In general, this cannot be used with empty collections
   (see ``reduce(op, itr)``).

.. function:: maximum(itr)

   Returns the largest element in a collection.

.. function:: maximum(A, dims)

   Compute the maximum value of an array over the given dimensions.

.. function:: maximum!(r, A)

   Compute the maximum value of ``A`` over the singleton dimensions of ``r``,
   and write results to ``r``.

.. function:: minimum(itr)

   Returns the smallest element in a collection.

.. function:: minimum(A, dims)

   Compute the minimum value of an array over the given dimensions.

.. function:: minimum!(r, A)

   Compute the minimum value of ``A`` over the singleton dimensions of ``r``,
   and write results to ``r``.

.. function:: extrema(itr)

    Compute both the minimum and maximum element in a single pass, and
    return them as a 2-tuple.

.. function:: indmax(itr) -> Integer

   Returns the index of the maximum element in a collection.

.. function:: indmin(itr) -> Integer

   Returns the index of the minimum element in a collection.

.. function:: findmax(itr) -> (x, index)

   Returns the maximum element and its index.

.. function:: findmax(A, dims) -> (maxval, index)

   For an array input, returns the value and index of the maximum over
   the given dimensions.

.. function:: findmin(itr) -> (x, index)

   Returns the minimum element and its index.

.. function:: findmin(A, dims) -> (minval, index)

   For an array input, returns the value and index of the minimum over
   the given dimensions.

.. function:: maxabs(itr)

   Compute the maximum absolute value of a collection of values.

.. function:: maxabs(A, dims)

   Compute the maximum absolute values over given dimensions.

.. function:: maxabs!(r, A)

   Compute the maximum absolute values over the singleton dimensions of ``r``,
   and write values to ``r``.

.. function:: minabs(itr)

   Compute the minimum absolute value of a collection of values.

.. function:: minabs(A, dims)

   Compute the minimum absolute values over given dimensions.

.. function:: minabs!(r, A)

   Compute the minimum absolute values over the singleton dimensions of ``r``,
   and write values to ``r``.

.. function:: sum(itr)

   Returns the sum of all elements in a collection.

.. function:: sum(A, dims)

   Sum elements of an array over the given dimensions.

.. function:: sum!(r, A)

   Sum elements of ``A`` over the singleton dimensions of ``r``,
   and write results to ``r``.

.. function:: sum(f, itr)

   Sum the results of calling function ``f`` on each element of ``itr``.

.. function:: sumabs(itr)

   Sum absolute values of all elements in a collection. This is
   equivalent to `sum(abs(itr))` but faster.

.. function:: sumabs(A, dims)

   Sum absolute values of elements of an array over the given
   dimensions.

.. function:: sumabs!(r, A)

   Sum absolute values of elements of ``A`` over the singleton
   dimensions of ``r``, and write results to ``r``.

.. function:: sumabs2(itr)

   Sum squared absolute values of all elements in a collection. This
   is equivalent to `sum(abs2(itr))` but faster.

.. function:: sumabs2(A, dims)

   Sum squared absolute values of elements of an array over the given
   dimensions.

.. function:: sumabs2!(r, A)

   Sum squared absolute values of elements of ``A`` over the singleton
   dimensions of ``r``, and write results to ``r``.

.. function:: prod(itr)

   Returns the product of all elements of a collection.

.. function:: prod(A, dims)

   Multiply elements of an array over the given dimensions.

.. function:: prod!(r, A)

   Multiply elements of ``A`` over the singleton dimensions of ``r``,
   and write results to ``r``.

.. function:: any(itr) -> Bool

   Test whether any elements of a boolean collection are true.

.. function:: any(A, dims)

   Test whether any values along the given dimensions of an array are true.

.. function:: any!(r, A)

   Test whether any values in ``A`` along the singleton dimensions of ``r`` are true,
   and write results to ``r``.

.. function:: all(itr) -> Bool

   Test whether all elements of a boolean collection are true.

.. function:: all(A, dims)

   Test whether all values along the given dimensions of an array are true.

.. function:: all!(r, A)

   Test whether all values in ``A`` along the singleton dimensions of ``r`` are true,
   and write results to ``r``.

.. function:: count(p, itr) -> Integer

   Count the number of elements in ``itr`` for which predicate ``p`` returns true.

.. function:: any(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for any elements of ``itr``.

.. function:: all(p, itr) -> Bool

   Determine whether predicate ``p`` returns true for all elements of ``itr``.

   .. doctest::

   	julia> all(i->(4<=i<=6), [4,5,6])
   	true

.. function:: map(f, c...) -> collection

   Transform collection ``c`` by applying ``f`` to each element.
   For multiple collection arguments, apply ``f`` elementwise.

   .. doctest::

      julia> map((x) -> x * 2, [1, 2, 3])
      3-element Array{Int64,1}:
       2
       4
       6

      julia> map(+, [1, 2, 3], [10, 20, 30])
      3-element Array{Int64,1}:
       11
       22
       33

.. function:: map!(function, collection)

   In-place version of :func:`map`.

.. function:: map!(function, destination, collection...)

   Like :func:`map()`, but stores the result in ``destination`` rather than a
   new collection. ``destination`` must be at least as large as the first
   collection.

.. function:: mapreduce(f, op, v0, itr)

   Apply function ``f`` to each element in ``itr``, and then reduce
   the result using the binary function ``op``. ``v0`` must be a
   neutral element for ``op`` that will be returned for empty
   collections. It is unspecified whether ``v0`` is used for non-empty
   collections.

   ``mapreduce`` is functionally equivalent to calling ``reduce(op,
   v0, map(f, itr))``, but will in general execute faster since no
   intermediate collection needs to be created. See documentation for
   ``reduce`` and ``map``.

   .. doctest::

      julia> mapreduce(x->x^2, +, [1:3]) # == 1 + 4 + 9
      14

   The associativity of the reduction is implementation-dependent. Use
   ``mapfoldl`` or ``mapfoldr`` instead for guaranteed left or right
   associativity.

.. function:: mapreduce(f, op, itr)

   Like ``mapreduce(f, op, v0, itr)``. In general, this cannot be used
   with empty collections (see ``reduce(op, itr)``).

.. function:: mapfoldl(f, op, v0, itr)

   Like ``mapreduce``, but with guaranteed left associativity. ``v0``
   will be used exactly once.

.. function:: mapfoldl(f, op, itr)

   Like ``mapfoldl(f, op, v0, itr)``, but using the first element of
   ``itr`` as ``v0``. In general, this cannot be used with empty
   collections (see ``reduce(op, itr)``).

.. function:: mapfoldr(f, op, v0, itr)

   Like ``mapreduce``, but with guaranteed right associativity. ``v0``
   will be used exactly once.

.. function:: mapfoldr(f, op, itr)

   Like ``mapfoldr(f, op, v0, itr)``, but using the first element of
   ``itr`` as ``v0``. In general, this cannot be used with empty
   collections (see ``reduce(op, itr)``).

.. function:: first(coll)

   Get the first element of an iterable collection. Returns the start point of a ``Range``
   even if it is empty.

.. function:: last(coll)

   Get the last element of an ordered collection, if it can be computed in O(1) time.
   This is accomplished by calling ``endof`` to get the last index.
   Returns the end point of a ``Range`` even if it is empty.

.. function:: step(r)

   Get the step size of a ``Range`` object.

.. function:: collect(collection)

   Return an array of all items in a collection. For associative collections, returns (key, value) tuples.

.. function:: collect(element_type, collection)

   Return an array of type ``Array{element_type,1}`` of all items in a collection.

.. function:: issubset(a, b)
              ⊆(A,S) -> Bool
              ⊈(A,S) -> Bool
              ⊊(A,S) -> Bool

   Determine whether every element of ``a`` is also in ``b``, using the
   ``in`` function.

.. function:: filter(function, collection)

   Return a copy of ``collection``, removing elements for which ``function`` is false.
   For associative collections, the function is passed two arguments (key and value).

.. function:: filter!(function, collection)

   Update ``collection``, removing elements for which ``function`` is false.
   For associative collections, the function is passed two arguments (key and value).


Indexable Collections
---------------------

.. function:: getindex(collection, key...)

   Retrieve the value(s) stored at the given key or index within a collection.
   The syntax ``a[i,j,...]`` is converted by the compiler to
   ``getindex(a, i, j, ...)``.

.. function:: setindex!(collection, value, key...)

   Store the given value at the given key or index within a collection.
   The syntax ``a[i,j,...] = x`` is converted by the compiler to
   ``setindex!(a, x, i, j, ...)``.

Fully implemented by: ``Array``, ``DArray``, ``BitArray``, ``AbstractArray``, ``SubArray``, ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``, ``AbstractString``.

Partially implemented by: ``Range``, ``UnitRange``, ``Tuple``.

Associative Collections
-----------------------

``Dict`` is the standard associative collection. Its implementation uses the ``hash(x)`` as the hashing function for the key, and ``isequal(x,y)`` to determine equality. Define these two functions for custom types to override how they are stored in a hash table.

``ObjectIdDict`` is a special hash table where the keys are always object identities. ``WeakKeyDict`` is a hash table implementation where the keys are weak references to objects, and thus may be garbage collected even when referenced in a hash table.

Dicts can be created by passing pair objects constructed with ``=>`` to a ``Dict`` constructor: ``Dict("A"=>1, "B"=>2)``. This call will attempt to infer type information from the keys and values (i.e. this example creates a ``Dict{ASCIIString, Int64}``).
To explicitly specify types use the syntax ``Dict{KeyType,ValueType}(...)``.
For example, ``Dict{ASCIIString,Int32}("A"=>1, "B"=>2)``.

As with arrays, ``Dicts`` may be created with comprehensions. For example,
``[i => f(i) for i = 1:10]``.

Given a dictionary ``D``, the syntax ``D[x]`` returns the value of key ``x`` (if it exists) or throws an error, and ``D[x] = y`` stores the key-value pair ``x => y`` in ``D`` (replacing any existing value for the key ``x``).  Multiple arguments to ``D[...]`` are converted to tuples; for example, the syntax ``D[x,y]``  is equivalent to ``D[(x,y)]``, i.e. it refers to the value keyed by the tuple ``(x,y)``.

.. function:: Dict([itr])

   ``Dict{K,V}()`` constructs a hash table with keys of type K and values of type V.
   Given a single iterable argument, constructs a ``Dict`` whose key-value pairs
   are taken from 2-tuples ``(key,value)`` generated by the argument.

   Alternatively, a sequence of pair arguments may be passed:
   ``Dict{K,V}("A"=>1, "B"=>2)``.

.. function:: haskey(collection, key) -> Bool

   Determine whether a collection has a mapping for a given key.

.. function:: get(collection, key, default)

   Return the value stored for the given key, or the given default value if no mapping for the key is present.

.. function:: get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the key is present, return ``f()``.  Use ``get!`` to also store the default value in the dictionary.

   This is intended to be called using ``do`` block syntax::

     get(dict, key) do
         # default value calculated here
	      time()
     end

.. function:: get!(collection, key, default)

   Return the value stored for the given key, or if no mapping for the key is present, store ``key => default``, and return ``default``.

.. function:: get!(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the key is present, store ``key => f()``, and return ``f()``.

   This is intended to be called using ``do`` block syntax::

     get!(dict, key) do
         # default value calculated here
	      time()
     end

.. function:: getkey(collection, key, default)

   Return the key matching argument ``key`` if one exists in ``collection``, otherwise return ``default``.

.. function:: delete!(collection, key)

   Delete the mapping for the given key in a collection, and return the collection.

.. function:: pop!(collection, key[, default])

   Delete and return the mapping for ``key`` if it exists in ``collection``, otherwise return ``default``, or throw an error if default is not specified.

.. function:: keys(collection)

   Return an iterator over all keys in a collection. ``collect(keys(d))`` returns an array of keys.

.. function:: values(collection)

   Return an iterator over all values in a collection. ``collect(values(d))`` returns an array of values.

.. function:: merge(collection, others...)

   Construct a merged collection from the given collections. If necessary, the types of the resulting collection will be promoted to accommodate the types of the merged collections::

     julia> a = Dict("foo" => 0.0, "bar" => 42.0)
     Dict{ASCIIString,Float64} with 2 entries:
       "bar" => 42.0
       "foo" => 0.0

     julia> b = Dict(utf8("baz") => 17, utf8("qux") => 4711)
     Dict{UTF8String,Int64} with 2 entries:
       "baz" => 17
       "qux" => 4711

     julia> merge(a, b)
     Dict{UTF8String,Float64} with 4 entries:
       "qux" => 4711.0
       "bar" => 42.0
       "baz" => 17.0
       "foo" => 0.0

.. function:: merge!(collection, others...)

   Update collection with pairs from the other collections

.. function:: sizehint!(s, n)

   Suggest that collection ``s`` reserve capacity for at least ``n`` elements. This can improve performance.

Fully implemented by: ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``.

Partially implemented by: ``IntSet``, ``Set``, ``EnvHash``, ``Array``, ``BitArray``.

Set-Like Collections
--------------------

.. function:: Set([itr])

   Construct a ``Set`` of the values generated by the given iterable object, or an empty set.
   Should be used instead of ``IntSet`` for sparse integer sets, or for sets of arbitrary objects.

.. function:: IntSet([itr])

   Construct a sorted set of the integers generated by the given iterable object, or an empty set. Implemented as a bit string, and therefore designed for dense integer sets. Only non-negative integers can be stored. If the set will be sparse (for example holding a single very large integer), use ``Set`` instead.

.. function:: union(s1,s2...)
              ∪(s1,s2)

   Construct the union of two or more sets. Maintains order with arrays.

.. function:: union!(s, iterable)

   Union each element of ``iterable`` into set ``s`` in-place.

.. function:: intersect(s1,s2...)
              ∩(s1,s2)

   Construct the intersection of two or more sets. Maintains order and multiplicity of the first argument for arrays and ranges.

.. function:: setdiff(s1,s2)

   Construct the set of elements in ``s1`` but not ``s2``. Maintains order with arrays.
   Note that both arguments must be collections, and both will be iterated over.
   In particular, ``setdiff(set,element)`` where ``element`` is a potential member of
   ``set``, will not work in general.

.. function:: setdiff!(s, iterable)

   Remove each element of ``iterable`` from set ``s`` in-place.

.. function:: symdiff(s1,s2...)

   Construct the symmetric difference of elements in the passed in sets or arrays. Maintains order with arrays.

.. function:: symdiff!(s, n)

   IntSet s is destructively modified to toggle the inclusion of integer ``n``.

.. function:: symdiff!(s, itr)

   For each element in ``itr``, destructively toggle its inclusion in set ``s``.

.. function:: symdiff!(s1, s2)

   Construct the symmetric difference of IntSets ``s1`` and ``s2``, storing the result in ``s1``.

.. function:: complement(s)

   Returns the set-complement of IntSet ``s``.

.. function:: complement!(s)

   Mutates IntSet ``s`` into its set-complement.

.. function:: intersect!(s1, s2)

   Intersects IntSets ``s1`` and ``s2`` and overwrites the set ``s1`` with the result. If needed, s1 will be expanded to the size of ``s2``.

.. function:: issubset(A, S) -> Bool
              ⊆(A,S) -> Bool

   True if A is a subset of or equal to S.

Fully implemented by: ``IntSet``, ``Set``.

Partially implemented by: ``Array``.

Dequeues
--------

.. function:: push!(collection, items...) -> collection

   Insert items at the end of a collection.

.. function:: pop!(collection) -> item

   Remove the last item in a collection and return it.

.. function:: unshift!(collection, items...) -> collection

   Insert items at the beginning of a collection.

.. function:: shift!(collection) -> item

   Remove the first item in a collection.

.. function:: insert!(collection, index, item)

   Insert an item at the given index.

.. function:: deleteat!(collection, index)

   Remove the item at the given index, and return the modified collection. Subsequent items
   are shifted to fill the resulting gap.

.. function:: deleteat!(collection, itr)

   Remove the items at the indices given by ``itr``, and return the modified collection. Subsequent
   items are shifted to fill the resulting gap.  ``itr`` must be sorted and unique.

.. function:: splice!(collection, index, [replacement]) -> item

   Remove the item at the given index, and return the removed item. Subsequent items
   are shifted down to fill the resulting gap. If specified, replacement values from
   an ordered collection will be spliced in place of the removed item.

   To insert ``replacement`` before an index ``n`` without removing any items, use ``splice!(collection, n:n-1, replacement)``.

.. function:: splice!(collection, range, [replacement]) -> items

   Remove items in the specified index range, and return a collection containing the
   removed items. Subsequent items are shifted down to fill the resulting gap.
   If specified, replacement values from an ordered collection will be spliced in place
   of the removed items.

   To insert ``replacement`` before an index ``n`` without removing any items, use ``splice!(collection, n:n-1, replacement)``.

.. function:: resize!(collection, n) -> collection

   Resize collection to contain ``n`` elements. If ``n`` is smaller than the current
   collection length, the first ``n`` elements will be retained. If ``n`` is larger,
   the new elements will not be initialized.

.. function:: append!(collection, items) -> collection.

   Add the elements of ``items`` to the end of a collection.

   .. doctest::

      julia> append!([1],[2,3])
      3-element Array{Int64,1}:
       1
       2
       3

.. function:: prepend!(collection, items) -> collection

   Insert the elements of ``items`` to the beginning of a collection.

   .. doctest::

      julia> prepend!([3],[1,2])
      3-element Array{Int64,1}:
       1
       2
       3

Fully implemented by: ``Vector`` (aka 1-d ``Array``), ``BitVector`` (aka 1-d ``BitArray``).

Nullables
---------

.. function:: get(x)

   Attempt to access the value of the ``Nullable`` object, ``x``. Returns the
   value if it is present; otherwise, throws a ``NullException``.

.. function:: get(x, y)

   Attempt to access the value of the ``Nullable{T}`` object, ``x``. Returns
   the value if it is present; otherwise, returns ``convert(T, y)``.

.. function:: isnull(x)

   Is the ``Nullable`` object ``x`` null, i.e. missing a value?

Strings
-------

.. function:: length(s)

   The number of characters in string ``s``.

.. function:: sizeof(s::AbstractString)

   The number of bytes in string ``s``.

.. function:: *(s, t)

   Concatenate strings. The ``*`` operator is an alias to this function.

   .. doctest::

	   julia> "Hello " * "world"
	   "Hello world"

.. function:: ^(s, n)

   Repeat ``n`` times the string ``s``. The ``^`` operator is an alias to this function.

   .. doctest::

   	julia> "Test "^3
   	"Test Test Test "

.. function:: string(xs...)

   Create a string from any values using the ``print`` function.

.. function:: repr(x)

   Create a string from any value using the ``showall`` function.

.. function:: bytestring(::Ptr{UInt8}, [length])

   Create a string from the address of a C (0-terminated) string encoded in ASCII or UTF-8. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

.. function:: bytestring(s)

   Convert a string to a contiguous byte array representation appropriate for passing it to C functions. The string will be encoded as either ASCII or UTF-8.

.. function:: ascii(::Array{UInt8,1})

   Create an ASCII string from a byte array.

.. function:: ascii(s)

   Convert a string to a contiguous ASCII string (all characters must be valid ASCII characters).

.. function:: utf8(::Array{UInt8,1})

   Create a UTF-8 string from a byte array.

.. function:: utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).

.. function:: normalize_string(s, normalform::Symbol)

   Normalize the string ``s`` according to one of the four "normal
   forms" of the Unicode standard: ``normalform`` can be ``:NFC``,
   ``:NFD``, ``:NFKC``, or ``:NFKD``.  Normal forms C (canonical
   composition) and D (canonical decomposition) convert different
   visually identical representations of the same abstract string into
   a single canonical form, with form C being more compact.  Normal
   forms KC and KD additionally canonicalize "compatibility
   equivalents": they convert characters that are abstractly similar
   but visually distinct into a single canonical choice (e.g. they expand
   ligatures into the individual characters), with form KC being more compact.

   Alternatively, finer control and additional transformations may be
   be obtained by calling `normalize_string(s; keywords...)`, where
   any number of the following boolean keywords options (which all default
   to ``false`` except for ``compose``) are specified:

   * ``compose=false``: do not perform canonical composition
   * ``decompose=true``: do canonical decomposition instead of canonical composition (``compose=true`` is ignored if present)
   * ``compat=true``: compatibility equivalents are canonicalized
   * ``casefold=true``: perform Unicode case folding, e.g. for case-insensitive string comparison
   * ``newline2lf=true``, ``newline2ls=true``, or ``newline2ps=true``: convert various newline sequences (LF, CRLF, CR, NEL) into a linefeed (LF), line-separation (LS), or paragraph-separation (PS) character, respectively
   * ``stripmark=true``: strip diacritical marks (e.g. accents)
   * ``stripignore=true``: strip Unicode's "default ignorable" characters (e.g. the soft hyphen or the left-to-right marker)
   * ``stripcc=true``: strip control characters; horizontal tabs and form feeds are converted to spaces; newlines are also converted to spaces unless a newline-conversion flag was specified
   * ``rejectna=true``: throw an error if unassigned code points are found
   * ``stable=true``: enforce Unicode Versioning Stability

   For example, NFKC corresponds to the options ``compose=true, compat=true, stable=true``.

.. function:: graphemes(s) -> iterator over substrings of s

   Returns an iterator over substrings of ``s`` that correspond to
   the extended graphemes in the string, as defined by Unicode UAX #29.
   (Roughly, these are what users would perceive as single characters,
   even though they may contain more than one codepoint; for example
   a letter combined with an accent mark is a single grapheme.)

.. function:: is_valid_ascii(s) -> Bool

   Returns true if the argument (``ASCIIString``, ``UTF8String``, or byte vector) is valid ASCII, false otherwise.

.. function:: is_valid_utf8(s) -> Bool

   Returns true if the argument (``ASCIIString``, ``UTF8String``, or byte vector) is valid UTF-8, false otherwise.

.. function:: is_valid_char(c) -> Bool

   Returns true if the given char or integer is a valid Unicode code point.

.. function:: is_assigned_char(c) -> Bool

   Returns true if the given char or integer is an assigned Unicode code point.

.. function:: ismatch(r::Regex, s::AbstractString) -> Bool

   Test whether a string contains a match of the given regular expression.

.. function:: match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

   Search for the first match of the regular expression ``r`` in ``s`` and return a RegexMatch object containing the match, or nothing if the match failed. The matching substring can be retrieved by accessing ``m.match`` and the captured sequences can be retrieved by accessing ``m.captures`` The optional ``idx`` argument specifies an index at which to start the search.

.. function:: eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

   Search for all matches of a the regular expression ``r`` in ``s`` and return a iterator over the matches. If overlap is true, the matching sequences are allowed to overlap indices in the original string, otherwise they must be from distinct character ranges.

.. function:: matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

   Return a vector of the matching substrings from eachmatch.

.. function:: lpad(string, n, p)

   Make a string at least ``n`` characters long by padding on the left with copies of ``p``.

.. function:: rpad(string, n, p)

   Make a string at least ``n`` characters long by padding on the right with copies of ``p``.

.. function:: search(string, chars, [start])

   Search for the first occurrence of the given characters within the given string. The second argument may be a single character, a vector or a set of characters, a string, or a regular expression (though regular expressions are only allowed on contiguous strings, such as ASCII or UTF-8 strings). The third argument optionally specifies a starting index. The return value is a range of indexes where the matching sequence is found, such that ``s[search(s,x)] == x``:

   ``search(string, "substring")`` = ``start:end`` such that ``string[start:end] == "substring"``, or ``0:-1`` if unmatched.

   ``search(string, 'c')``         = ``index`` such that ``string[index] == 'c'``, or ``0`` if unmatched.

.. function:: rsearch(string, chars, [start])

   Similar to ``search``, but returning the last occurrence of the given characters within the given string, searching in reverse from ``start``.

.. function:: searchindex(string, substring, [start])

   Similar to ``search``, but return only the start index at which the substring is found, or 0 if it is not.

.. function:: rsearchindex(string, substring, [start])

   Similar to ``rsearch``, but return only the start index at which the substring is found, or 0 if it is not.

.. function:: contains(haystack, needle)

   Determine whether the second argument is a substring of the first.

.. function:: replace(string, pat, r[, n])

   Search for the given pattern ``pat``, and replace each occurrence with ``r``. If ``n`` is provided, replace at most ``n`` occurrences.  As with search, the second argument may be a single character, a vector or a set of characters, a string, or a regular expression. If ``r`` is a function, each occurrence is replaced with ``r(s)`` where ``s`` is the matched substring.

.. function:: split(string, [chars]; limit=0, keep=true)

   Return an array of substrings by splitting the given string on occurrences of the given character delimiters, which may be specified in any of the formats allowed by ``search``'s second argument (i.e. a single character, collection of characters, string, or regular expression). If ``chars`` is omitted, it defaults to the set of all space characters, and ``keep`` is taken to be false. The two keyword arguments are optional: they are are a maximum size for the result and a flag determining whether empty fields should be kept in the result.

.. function:: rsplit(string, [chars]; limit=0, keep=true)

   Similar to ``split``, but starting from the end of the string.

.. function:: strip(string, [chars])

   Return ``string`` with any leading and trailing whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: lstrip(string, [chars])

   Return ``string`` with any leading whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: rstrip(string, [chars])

   Return ``string`` with any trailing whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: beginswith(string, prefix | chars)

   Returns ``true`` if ``string`` starts with ``prefix``. If the second argument is a vector or set of characters, tests whether the first character of ``string`` belongs to that set.

.. function:: endswith(string, suffix | chars)

   Returns ``true`` if ``string`` ends with ``suffix``. If the second argument is a vector or set of characters, tests whether the last character of ``string`` belongs to that set.

.. function:: uppercase(string)

   Returns ``string`` with all characters converted to uppercase.

.. function:: lowercase(string)

   Returns ``string`` with all characters converted to lowercase.

.. function:: ucfirst(string)

   Returns ``string`` with the first character converted to uppercase.

.. function:: lcfirst(string)

   Returns ``string`` with the first character converted to lowercase.

.. function:: join(strings, delim, [last])

   Join an array of ``strings`` into a single string, inserting the given delimiter between adjacent strings.
   If ``last`` is given, it will be used instead of ``delim`` between the last two strings.
   For example, ``join(["apples", "bananas", "pineapples"], ", ", " and ") == "apples, bananas and pineapples"``.

   ``strings`` can be any iterable over elements ``x`` which are convertible to strings via ``print(io::IOBuffer, x)``.

.. function:: chop(string)

   Remove the last character from a string

.. function:: chomp(string)

   Remove a trailing newline from a string

.. function:: ind2chr(string, i)

   Convert a byte index to a character index

.. function:: chr2ind(string, i)

   Convert a character index to a byte index

.. function:: isvalid(str, i)

   Tells whether index ``i`` is valid for the given string

.. function:: nextind(str, i)

   Get the next valid string index after ``i``. Returns a value greater than ``endof(str)``
   at or after the end of the string.

.. function:: prevind(str, i)

   Get the previous valid string index before ``i``. Returns a value less than ``1`` at
   the beginning of the string.

.. function:: randstring(len)

   Create a random ASCII string of length ``len``, consisting of upper- and lower-case letters and the digits 0-9

.. function:: charwidth(c)

   Gives the number of columns needed to print a character.

.. function:: strwidth(s)

   Gives the number of columns needed to print a string.

.. function:: isalnum(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is alphanumeric, or whether this
   is true for all elements of a string.  A character is classified as alphabetic
   if it belongs to the Unicode general category Letter or Number, i.e. a character whose
   category code begins with 'L' or 'N'.

.. function:: isalpha(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is alphabetic, or whether this
   is true for all elements of a string. A character is classified as alphabetic
   if it belongs to the Unicode general category Letter, i.e. a character whose
   category code begins with 'L'.

.. function:: isascii(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character belongs to the ASCII character set, or whether this
   is true for all elements of a string.

.. function:: iscntrl(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is a control character, or whether this
   is true for all elements of a string.  Control characters are the
   non-printing characters of the Latin-1 subset of Unicode.

.. function:: isdigit(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is a numeric digit (0-9), or whether this
   is true for all elements of a string.

.. function:: isgraph(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is printable, and not a space, or whether this
   is true for all elements of a string.  Any character that would cause a printer
   to use ink should be classified with isgraph(c)==true.

.. function:: islower(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is a lowercase letter, or whether this
   is true for all elements of a string.  A character is classified as lowercase
   if it belongs to Unicode category Ll, Letter: Lowercase.

.. function:: isnumber(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is numeric, or whether this
   is true for all elements of a string.   A character is classified as numeric
   if it belongs to the Unicode general category Number, i.e. a character whose
   category code begins with 'N'.

.. function:: isprint(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is printable, including spaces, but not a control character. For strings, tests whether this is true for all elements of the string.

.. function:: ispunct(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character belongs to the Unicode general category Punctuation, i.e. a character whose category code begins with 'P'. For strings, tests whether this is true for all elements of the string.

.. function:: isspace(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is any whitespace character.  Includes ASCII characters '\\t', '\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode category Zs.  For strings, tests whether this    is true for all elements of the string.

.. function:: isupper(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is an uppercase letter, or whether this
   is true for all elements of a string.    A character is classified as uppercase
   if it belongs to Unicode category Lu, Letter: Uppercase, or Lt, Letter: Titlecase.

.. function:: isxdigit(c::Union(Char,AbstractString)) -> Bool

   Tests whether a character is a valid hexadecimal digit, or whether this
   is true for all elements of a string.

.. function:: symbol(str) -> Symbol

   Convert a string to a ``Symbol``.

.. function:: escape_string(str::AbstractString) -> AbstractString

   General escaping of traditional C and Unicode escape sequences. See :func:`print_escaped` for more general escaping.

.. function:: unescape_string(s::AbstractString) -> AbstractString

   General unescaping of traditional C and Unicode escape sequences. Reverse of :func:`escape_string`. See also :func:`print_unescaped`.

.. function:: utf16(s)

   Create a UTF-16 string from a byte array, array of ``UInt16``, or
   any other string type.  (Data must be valid UTF-16.  Conversions of
   byte arrays check for a byte-order marker in the first two bytes,
   and do not include it in the resulting string.)

   Note that the resulting ``UTF16String`` data is terminated by the NUL
   codepoint (16-bit zero), which is not treated as a character in the
   string (so that it is mostly invisible in Julia); this allows the
   string to be passed directly to external functions requiring
   NUL-terminated data.  This NUL is appended automatically by the
   `utf16(s)` conversion function.  If you have a ``UInt16`` array
   ``A`` that is already NUL-terminated valid UTF-16 data, then you
   can instead use `UTF16String(A)`` to construct the string without
   making a copy of the data and treating the NUL as a terminator
   rather than as part of the string.

.. function:: utf16(::Union(Ptr{UInt16},Ptr{Int16}) [, length])

   Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the pointer can be safely freed. If ``length`` is specified, the string does not have to be NUL-terminated.

.. function:: is_valid_utf16(s) -> Bool

   Returns true if the argument (``UTF16String`` or ``UInt16`` array) is valid UTF-16.

.. function:: utf32(s)

   Create a UTF-32 string from a byte array, array of ``UInt32``, or
   any other string type.  (Conversions of byte arrays check for a
   byte-order marker in the first four bytes, and do not include it in
   the resulting string.)

   Note that the resulting ``UTF32String`` data is terminated by the NUL
   codepoint (32-bit zero), which is not treated as a character in the
   string (so that it is mostly invisible in Julia); this allows the
   string to be passed directly to external functions requiring
   NUL-terminated data.  This NUL is appended automatically by the
   `utf32(s)` conversion function.  If you have a ``UInt32`` array
   ``A`` that is already NUL-terminated UTF-32 data, then you
   can instead use `UTF32String(A)`` to construct the string without
   making a copy of the data and treating the NUL as a terminator
   rather than as part of the string.

.. function:: utf32(::Union(Ptr{Char},Ptr{UInt32},Ptr{Int32}) [, length])

   Create a string from the address of a NUL-terminated UTF-32 string. A copy is made; the pointer can be safely freed. If ``length`` is specified, the string does not have to be NUL-terminated.

.. function:: wstring(s)

   This is a synonym for either ``utf32(s)`` or ``utf16(s)``,
   depending on whether ``Cwchar_t`` is 32 or 16 bits, respectively.
   The synonym ``WString`` for ``UTF32String`` or ``UTF16String``
   is also provided.

I/O
---

.. data:: STDOUT

   Global variable referring to the standard out stream.

.. data:: STDERR

   Global variable referring to the standard error stream.

.. data:: STDIN

   Global variable referring to the standard input stream.

.. function:: open(file_name, [read, write, create, truncate, append]) -> IOStream

   Open a file in a mode specified by five boolean arguments. The default is to open files for reading only. Returns a stream for accessing the file.

.. function:: open(file_name, [mode]) -> IOStream

   Alternate syntax for open, where a string-based mode specifier is used instead of the five booleans. The values of ``mode`` correspond to those from ``fopen(3)`` or Perl ``open``, and are equivalent to setting the following boolean groups:

   ==== =================================
    r    read
    r+   read, write
    w    write, create, truncate
    w+   read, write, create, truncate
    a    write, create, append
    a+   read, write, create, append
   ==== =================================


.. function:: open(f::function, args...)

   Apply the function ``f`` to the result of ``open(args...)`` and close the resulting file descriptor upon completion.

   **Example**: ``open(readall, "file.txt")``

.. function:: IOBuffer() -> IOBuffer

   Create an in-memory I/O stream.

.. function:: IOBuffer(size::Int)

   Create a fixed size IOBuffer. The buffer will not grow dynamically.

.. function:: IOBuffer(string)

   Create a read-only IOBuffer on the data underlying the given string

.. function:: IOBuffer([data,],[readable,writable,[maxsize]])

   Create an IOBuffer, which may optionally operate on a pre-existing array. If the readable/writable arguments are given,
   they restrict whether or not the buffer may be read from or written to respectively. By default the buffer is readable
   but not writable. The last argument optionally specifies a size beyond which the buffer may not be grown.

.. function:: takebuf_array(b::IOBuffer)

   Obtain the contents of an ``IOBuffer`` as an array, without copying. Afterwards, the IOBuffer is reset to its initial state.

.. function:: takebuf_string(b::IOBuffer)

   Obtain the contents of an ``IOBuffer`` as a string, without copying. Afterwards, the IOBuffer is reset to its initial state.

.. function:: fdio([name::AbstractString, ]fd::Integer[, own::Bool]) -> IOStream

   Create an ``IOStream`` object from an integer file descriptor. If ``own`` is true, closing this object will close the underlying descriptor. By default, an ``IOStream`` is closed when it is garbage collected. ``name`` allows you to associate the descriptor with a named file.

.. function:: flush(stream)

   Commit all currently buffered writes to the given stream.

.. function:: flush_cstdio()

   Flushes the C ``stdout`` and ``stderr`` streams (which may have been
   written to by external C code).

.. function:: close(stream)

   Close an I/O stream. Performs a ``flush`` first.

.. function:: write(stream, x)

   Write the canonical binary representation of a value to the given stream.

.. function:: read(stream, type)

   Read a value of the given type from a stream, in canonical binary representation.

.. function:: read(stream, type, dims)

   Read a series of values of the given type from a stream, in canonical binary representation. ``dims`` is either a tuple or a series of integer arguments specifying the size of ``Array`` to return.

.. function:: read!(stream, array::Array)

   Read binary data from a stream, filling in the argument ``array``.

.. function:: readbytes!(stream, b::Vector{UInt8}, nb=length(b))

   Read at most ``nb`` bytes from the stream into ``b``, returning the
   number of bytes read (increasing the size of ``b`` as needed).

.. function:: readbytes(stream, nb=typemax(Int))

   Read at most ``nb`` bytes from the stream, returning a
   ``Vector{UInt8}`` of the bytes read.

.. function:: position(s)

   Get the current position of a stream.

.. function:: seek(s, pos)

   Seek a stream to the given position.

.. function:: seekstart(s)

   Seek a stream to its beginning.

.. function:: seekend(s)

   Seek a stream to its end.

.. function:: skip(s, offset)

   Seek a stream relative to the current position.

.. function:: mark(s)

   Add a mark at the current position of stream ``s``.  Returns the marked position.

   See also :func:`unmark`, :func:`reset`, :func:`ismarked`

.. function:: unmark(s)

   Remove a mark from stream ``s``.
   Returns ``true`` if the stream was marked, ``false`` otherwise.

   See also :func:`mark`, :func:`reset`, :func:`ismarked`

.. function:: reset(s)

   Reset a stream ``s`` to a previously marked position, and remove the mark.
   Returns the previously marked position.
   Throws an error if the stream is not marked.

   See also :func:`mark`, :func:`unmark`, :func:`ismarked`

.. function:: ismarked(s)

   Returns true if stream ``s`` is marked.

   See also :func:`mark`, :func:`unmark`, :func:`reset`

.. function:: eof(stream) -> Bool

   Tests whether an I/O stream is at end-of-file. If the stream is not yet
   exhausted, this function will block to wait for more data if necessary, and
   then return ``false``. Therefore it is always safe to read one byte after
   seeing ``eof`` return ``false``. ``eof`` will return ``false`` as long
   as buffered data is still available, even if the remote end of a
   connection is closed.

.. function:: isreadonly(stream) -> Bool

   Determine whether a stream is read-only.

.. function:: isopen(stream) -> Bool

   Determine whether a stream is open (i.e. has not been closed yet).
   If the connection has been closed remotely (in case of e.g. a socket),
   ``isopen`` will return ``false`` even though buffered data may still be
   available. Use ``eof`` to check if necessary.

.. function:: ntoh(x)

   Converts the endianness of a value from Network byte order (big-endian) to
   that used by the Host.

.. function:: hton(x)

   Converts the endianness of a value from that used by the Host to Network
   byte order (big-endian).

.. function:: ltoh(x)

   Converts the endianness of a value from Little-endian to that used by the
   Host.

.. function:: htol(x)

   Converts the endianness of a value from that used by the Host to
   Little-endian.

.. data:: ENDIAN_BOM

   The 32-bit byte-order-mark indicates the native byte order of the host machine. Little-endian machines will contain the value 0x04030201. Big-endian machines will contain the value 0x01020304.

.. function:: serialize(stream, value)

   Write an arbitrary value to a stream in an opaque format, such that it can
   be read back by ``deserialize``. The read-back value will be as identical as
   possible to the original. In general, this process will not work if the
   reading and writing are done by different versions of Julia, or
   an instance of Julia with a different system image.

.. function:: deserialize(stream)

   Read a value written by ``serialize``.

.. function:: print_escaped(io, str::AbstractString, esc::AbstractString)

   General escaping of traditional C and Unicode escape sequences, plus any characters in esc are also escaped (with a backslash).

.. function:: print_unescaped(io, s::AbstractString)

   General unescaping of traditional C and Unicode escape sequences. Reverse of :func:`print_escaped`.

.. function:: print_joined(io, items, delim, [last])

   Print elements of ``items`` to ``io`` with ``delim`` between them. If ``last`` is specified, it is used as the final delimiter instead of ``delim``.

.. function:: print_shortest(io, x)

   Print the shortest possible representation of number ``x`` as a floating point number, ensuring that it would parse to the exact same number.

.. function:: fd(stream)

   Returns the file descriptor backing the stream or file. Note that this function only applies to synchronous `File`'s and `IOStream`'s
   not to any of the asynchronous streams.

.. function:: redirect_stdout()

   Create a pipe to which all C and Julia level STDOUT output will be redirected. Returns a tuple (rd,wr)
   representing the pipe ends. Data written to STDOUT may now be read from the rd end of the pipe. The
   wr end is given for convenience in case the old STDOUT object was cached by the user and needs to be
   replaced elsewhere.

.. function:: redirect_stdout(stream)

   Replace STDOUT by stream for all C and julia level output to STDOUT. Note that `stream` must be a TTY, a Pipe or a
   TcpSocket.

.. function:: redirect_stderr([stream])

   Like redirect_stdout, but for STDERR

.. function:: redirect_stdin([stream])

   Like redirect_stdout, but for STDIN. Note that the order of the return tuple is still (rd,wr), i.e. data to be read
   from STDIN, may be written to wr.

.. function:: readchomp(x)

   Read the entirety of x as a string but remove trailing newlines. Equivalent to chomp(readall(x)).

.. function:: readdir([dir]) -> Vector{ByteString}

   Returns the files and directories in the directory `dir` (or the current working directory if not given).

.. function:: truncate(file,n)

   Resize the file or buffer given by the first argument to exactly `n` bytes, filling previously unallocated space with '\0'
   if the file or buffer is grown

.. function:: skipchars(stream, predicate; linecomment::Char)

   Advance the stream until before the first character for which ``predicate`` returns false. For example ``skipchars(stream, isspace)`` will skip all whitespace. If keyword argument ``linecomment`` is specified, characters from that character through the end of a line will also be skipped.

.. function:: countlines(io,[eol::Char])

   Read io until the end of the stream/file and count the number of non-empty lines. To specify a file pass the filename as the first
   argument. EOL markers other than '\n' are supported by passing them as the second argument.

.. function:: PipeBuffer()

   An IOBuffer that allows reading and performs writes by appending. Seeking and truncating are not supported. See IOBuffer for the available constructors.

.. function:: PipeBuffer(data::Vector{UInt8},[maxsize])

   Create a PipeBuffer to operate on a data vector, optionally specifying a size beyond which the underlying Array may not be grown.

.. function:: readavailable(stream)

   Read all available data on the stream, blocking the task only if no data is available.

.. function:: stat(file)

   Returns a structure whose fields contain information about the file. The fields of the structure are:

   ========= ======================================================================
    size      The size (in bytes) of the file
    device    ID of the device that contains the file
    inode     The inode number of the file
    mode      The protection mode of the file
    nlink     The number of hard links to the file
    uid       The user id of the owner of the file
    gid       The group id of the file owner
    rdev      If this file refers to a device, the ID of the device it refers to
    blksize   The file-system preferred block size for the file
    blocks    The number of such blocks allocated
    mtime     Unix timestamp of when the file was last modified
    ctime     Unix timestamp of when the file was created
   ========= ======================================================================

.. function:: lstat(file)

   Like stat, but for symbolic links gets the info for the link itself rather than the file it refers to. This function must be called on a file path rather than a file object or a file descriptor.

.. function:: ctime(file)

   Equivalent to stat(file).ctime

.. function:: mtime(file)

   Equivalent to stat(file).mtime

.. function:: filemode(file)

   Equivalent to stat(file).mode

.. function:: filesize(path...)

   Equivalent to stat(file).size

.. function:: uperm(file)

   Gets the permissions of the owner of the file as a bitfield of

   ==== =====================
    01   Execute Permission
    02   Write Permission
    04   Read Permission
   ==== =====================

   For allowed arguments, see ``stat``.

.. function:: gperm(file)

   Like uperm but gets the permissions of the group owning the file

.. function:: operm(file)

   Like uperm but gets the permissions for people who neither own the file nor are a
   member of the group owning the file

.. function:: cp(src::AbstractString,dst::AbstractString)

   Copy a file from `src` to `dest`.

.. function:: download(url,[localfile])

   Download a file from the given url, optionally renaming it to the given local file name.
   Note that this function relies on the availability of external tools such as ``curl``,
   ``wget`` or ``fetch`` to download the file and is provided for convenience. For production
   use or situations in which more options are need, please use a package that provides the
   desired functionality instead.

.. function:: mv(src::AbstractString,dst::AbstractString)

   Move a file from `src` to `dst`.

.. function:: rm(path::AbstractString; recursive=false)

   Delete the file, link, or empty directory at the given path. If ``recursive=true`` is
   passed and the path is a directory, then all contents are removed recursively.

.. function:: touch(path::AbstractString)

   Update the last-modified timestamp on a file to the current time.


Network I/O
-----------

.. function:: connect([host],port) -> TcpSocket

   Connect to the host ``host`` on port ``port``

.. function:: connect(path) -> Pipe

   Connect to the Named Pipe/Domain Socket at ``path``

.. function:: listen([addr,]port) -> TcpServer

   Listen on port on the address specified by ``addr``. By default this listens on localhost only.
   To listen on all interfaces pass, ``IPv4(0)`` or ``IPv6(0)`` as appropriate.

.. function:: listen(path) -> PipeServer

   Listens on/Creates a Named Pipe/Domain Socket

.. function:: getaddrinfo(host)

   Gets the IP address of the ``host`` (may have to do a DNS lookup)

.. function:: parseip(addr)

   Parse a string specifying an IPv4 or IPv6 ip address.

.. function:: IPv4(host::Integer) -> IPv4

   Returns IPv4 object from ip address formatted as Integer

.. function:: IPv6(host::Integer) -> IPv6

   Returns IPv6 object from ip address formatted as Integer

.. function:: nb_available(stream)

   Returns the number of bytes available for reading before a read from this stream or buffer will block.

.. function:: accept(server[,client])

   Accepts a connection on the given server and returns a connection to the client. An uninitialized client
   stream may be provided, in which case it will be used instead of creating a new stream.

.. function:: listenany(port_hint) -> (UInt16,TcpServer)

   Create a TcpServer on any port, using hint as a starting point. Returns a tuple of the actual port that the server
   was created on and the server itself.

.. function:: watch_file(cb=false, s; poll=false)

   Watch file or directory ``s`` and run callback ``cb`` when ``s`` is modified. The ``poll`` parameter specifies whether to use file system event monitoring or polling. The callback function ``cb`` should accept 3 arguments: ``(filename, events, status)`` where ``filename`` is the name of file that was modified, ``events`` is an object with boolean fields ``changed`` and ``renamed`` when using file system event monitoring, or ``readable`` and ``writable`` when using polling, and ``status`` is always 0. Pass ``false`` for ``cb`` to not use a callback function.

.. function:: poll_fd(fd, seconds::Real; readable=false, writable=false)

   Poll a file descriptor fd for changes in the read or write availability and with a timeout given by the second argument.
   If the timeout is not needed, use ``wait(fd)`` instead. The keyword arguments determine which of read and/or write status
   should be monitored and at least one of them needs to be set to true.
   The returned value is an object with boolean fields ``readable``, ``writable``, and
   ``timedout``, giving the result of the polling.

.. function:: poll_file(s, interval_seconds::Real, seconds::Real)

   Monitor a file for changes by polling every `interval_seconds` seconds for `seconds` seconds. A return value of true indicates
   the file changed, a return value of false indicates a timeout.

.. function:: bind(socket::Union(UDPSocket, TCPSocket), host::IPv4, port::Integer)

   Bind ``socket`` to the given ``host:port``. Note that `0.0.0.0` will listen on all devices.

.. function:: send(socket::UDPSocket, host::IPv4, port::Integer, msg)

   Send ``msg`` over ``socket to ``host:port``.

.. function:: recv(socket::UDPSocket)

   Read a UDP packet from the specified socket, and return the bytes received. This call blocks.

.. function:: setopt(sock::UDPSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)

   Set UDP socket options. ``multicast_loop``: loopback for multicast packets (default: true). ``multicast_ttl``: TTL for multicast packets. ``enable_broadcast``: flag must be set to true if socket will be used for broadcast messages, or else the UDP system will return an access error (default: false). ``ttl``: Time-to-live of packets sent on the socket.


Text I/O
--------

.. function:: show(x)

   Write an informative text representation of a value to the current output stream. New types should overload ``show(io, x)`` where the first argument is a stream.
   The representation used by ``show`` generally includes Julia-specific formatting and type information.

.. function:: showcompact(x)

   Show a more compact representation of a value. This is used for printing
   array elements. If a new type has a different compact representation, it
   should overload ``showcompact(io, x)`` where the first argument is a stream.

.. function:: showall(x)

   Similar to ``show``, except shows all elements of arrays.

.. function:: summary(x)

   Return a string giving a brief description of a value. By default returns
   ``string(typeof(x))``. For arrays, returns strings like "2x2 Float64 Array".

.. function:: print(x)

   Write (to the default output stream) a canonical (un-decorated) text representation of a value if there is one, otherwise call ``show``.
   The representation used by ``print`` includes minimal formatting and tries to avoid Julia-specific details.

.. function:: println(x)

   Print (using :func:`print`) ``x`` followed by a newline.

.. function:: print_with_color(color::Symbol, [io], strings...)

   Print strings in a color specified as a symbol, for example ``:red`` or ``:blue``.

.. function:: info(msg)

   Display an informational message.

.. function:: warn(msg)

   Display a warning.

.. function:: @printf([io::IOStream], "%Fmt", args...)

   Print arg(s) using C ``printf()`` style format specification string. Optionally, an IOStream may be passed as the first argument to redirect output.

.. function:: @sprintf("%Fmt", args...)

   Return ``@printf`` formatted output as string.

.. function:: sprint(f::Function, args...)

   Call the given function with an I/O stream and the supplied extra arguments.
   Everything written to this I/O stream is returned as a string.

.. function:: showerror(io, e)

   Show a descriptive representation of an exception object.

.. function:: dump(x)

   Show all user-visible structure of a value.

.. function:: xdump(x)

   Show all structure of a value, including all fields of objects.

.. function:: readall(stream::IO)

   Read the entire contents of an I/O stream as a string.

.. function:: readall(filename::AbstractString)

   Open ``filename``, read the entire contents as a string, then close the file.
   Equivalent to ``open(readall, filename)``.

.. function:: readline(stream=STDIN)

   Read a single line of text, including a trailing newline character (if one is reached before the end of the input), from the given ``stream`` (defaults to ``STDIN``),

.. function:: readuntil(stream, delim)

   Read a string, up to and including the given delimiter byte.

.. function:: readlines(stream)

   Read all lines as an array.

.. function:: eachline(stream)

   Create an iterable object that will yield each line from a stream.

.. function:: readdlm(source, delim::Char, T::Type, eol::Char; header=false, skipstart=0, skipblanks=true, use_mmap, ignore_invalid_chars=false, quotes=true, dims, comments=true, comment_char='#')

   Read a matrix from the source where each line (separated by ``eol``) gives one row, with elements separated by the given delimeter. The source can be a text file, stream or byte array. Memory mapped files can be used by passing the byte array representation of the mapped segment as source.

   If ``T`` is a numeric type, the result is an array of that type, with any non-numeric elements as ``NaN`` for floating-point types, or zero. Other useful values of ``T`` include ``ASCIIString``, ``AbstractString``, and ``Any``.

   If ``header`` is ``true``, the first row of data will be read as header and the tuple ``(data_cells, header_cells)`` is returned instead of only ``data_cells``.

   Specifying ``skipstart`` will ignore the corresponding number of initial lines from the input.

   If ``skipblanks`` is ``true``, blank lines in the input will be ignored.

   If ``use_mmap`` is ``true``, the file specified by ``source`` is memory mapped for potential speedups. Default is ``true`` except on Windows. On Windows, you may want to specify ``true`` if the file is large, and is only read once and not written to.

   If ``ignore_invalid_chars`` is ``true``, bytes in ``source`` with invalid character encoding will be ignored. Otherwise an error is thrown indicating the offending character position.

   If ``quotes`` is ``true``, column enclosed within double-quote (``) characters are allowed to contain new lines and column delimiters. Double-quote characters within a quoted field must be escaped with another double-quote.

   Specifying ``dims`` as a tuple of the expected rows and columns (including header, if any) may speed up reading of large files.

   If ``comments`` is ``true``, lines beginning with ``comment_char`` and text following ``comment_char`` in any line are ignored.

.. function:: readdlm(source, delim::Char, eol::Char; options...)

   If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: readdlm(source, delim::Char, T::Type; options...)

   The end of line delimiter is taken as ``\n``.

.. function:: readdlm(source, delim::Char; options...)

   The end of line delimiter is taken as ``\n``. If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: readdlm(source, T::Type; options...)

   The columns are assumed to be separated by one or more whitespaces. The end of line delimiter is taken as ``\n``.

.. function:: readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces. The end of line delimiter is taken as ``\n``. If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: writedlm(f, A, delim='\t')

   Write ``A`` (a vector, matrix or an iterable collection of iterable rows) as text to ``f`` (either a filename string or an ``IO`` stream) using the given delimeter ``delim`` (which defaults to tab, but can be any printable Julia object, typically a ``Char`` or ``AbstractString``).

   For example, two vectors ``x`` and ``y`` of the same length can
   be written as two columns of tab-delimited text to ``f`` by
   either ``writedlm(f, [x y])`` or by ``writedlm(f, zip(x, y))``.

.. function:: readcsv(source, [T::Type]; options...)

   Equivalent to ``readdlm`` with ``delim`` set to comma.

.. function:: writecsv(filename, A)

   Equivalent to ``writedlm`` with ``delim`` set to comma.

.. function:: Base64EncodePipe(ostream)

   Returns a new write-only I/O stream, which converts any bytes written
   to it into base64-encoded ASCII bytes written to ``ostream``.  Calling
   ``close`` on the ``Base64Pipe`` stream is necessary to complete the
   encoding (but does not close ``ostream``).

.. function:: Base64DecodePipe(istream)

   Returns a new read-only I/O stream, which decodes base64-encoded data
   read from ``istream``.

.. function:: base64encode(writefunc, args...)
              base64encode(args...)

   Given a ``write``-like function ``writefunc``, which takes an I/O
   stream as its first argument, ``base64(writefunc, args...)``
   calls ``writefunc`` to write ``args...`` to a base64-encoded string,
   and returns the string.  ``base64(args...)`` is equivalent to
   ``base64(write, args...)``: it converts its arguments into bytes
   using the standard ``write`` functions and returns the base64-encoded
   string.

.. function:: base64decode(string)

   Decodes the base64-encoded ``string`` and returns the obtained bytes.

Multimedia I/O
--------------

Just as text output is performed by ``print`` and user-defined types
can indicate their textual representation by overloading ``show``,
Julia provides a standardized mechanism for rich multimedia output
(such as images, formatted text, or even audio and video), consisting
of three parts:

* A function ``display(x)`` to request the richest available multimedia
  display of a Julia object ``x`` (with a plain-text fallback).
* Overloading ``writemime`` allows one to indicate arbitrary multimedia
  representations (keyed by standard MIME types) of user-defined types.
* Multimedia-capable display backends may be registered by subclassing
  a generic ``Display`` type and pushing them onto a stack of display
  backends via ``pushdisplay``.

The base Julia runtime provides only plain-text display, but richer
displays may be enabled by loading external modules or by using graphical
Julia environments (such as the IPython-based IJulia notebook).

.. function:: display(x)
              display(d::Display, x)
              display(mime, x)
              display(d::Display, mime, x)

   Display ``x`` using the topmost applicable display in the display stack,
   typically using the richest supported multimedia output for ``x``, with
   plain-text ``STDOUT`` output as a fallback.  The ``display(d, x)`` variant
   attempts to display ``x`` on the given display ``d`` only, throwing
   a ``MethodError`` if ``d`` cannot display objects of this type.

   There are also two variants with a ``mime`` argument (a MIME type
   string, such as ``"image/png"``), which attempt to display ``x`` using the
   requested MIME type *only*, throwing a ``MethodError`` if this type
   is not supported by either the display(s) or by ``x``.   With these
   variants, one can also supply the "raw" data in the requested MIME
   type by passing ``x::AbstractString`` (for MIME types with text-based storage,
   such as text/html or application/postscript) or ``x::Vector{UInt8}``
   (for binary MIME types).

.. function:: redisplay(x)
              redisplay(d::Display, x)
              redisplay(mime, x)
              redisplay(d::Display, mime, x)

   By default, the ``redisplay`` functions simply call ``display``.  However,
   some display backends may override ``redisplay`` to modify an existing
   display of ``x`` (if any).   Using ``redisplay`` is also a hint to the
   backend that ``x`` may be redisplayed several times, and the backend
   may choose to defer the display until (for example) the next interactive
   prompt.

.. function:: displayable(mime) -> Bool
              displayable(d::Display, mime) -> Bool

   Returns a boolean value indicating whether the given ``mime`` type (string)
   is displayable by any of the displays in the current display stack, or
   specifically by the display ``d`` in the second variant.

.. function:: writemime(stream, mime, x)

   The ``display`` functions ultimately call ``writemime`` in order to
   write an object ``x`` as a given ``mime`` type to a given I/O
   ``stream`` (usually a memory buffer), if possible.  In order to
   provide a rich multimedia representation of a user-defined type
   ``T``, it is only necessary to define a new ``writemime`` method for
   ``T``, via: ``writemime(stream, ::MIME"mime", x::T) = ...``, where
   ``mime`` is a MIME-type string and the function body calls
   ``write`` (or similar) to write that representation of ``x`` to
   ``stream``. (Note that the ``MIME""`` notation only supports literal
   strings; to construct ``MIME`` types in a more flexible manner use
   ``MIME{symbol("")}``.)

   For example, if you define a ``MyImage`` type and know how to write
   it to a PNG file, you could define a function ``writemime(stream,
   ::MIME"image/png", x::MyImage) = ...``` to allow your images to
   be displayed on any PNG-capable ``Display`` (such as IJulia).
   As usual, be sure to ``import Base.writemime`` in order to add
   new methods to the built-in Julia function ``writemime``.

   Technically, the ``MIME"mime"`` macro defines a singleton type for
   the given ``mime`` string, which allows us to exploit Julia's
   dispatch mechanisms in determining how to display objects of any
   given type.

.. function:: mimewritable(mime, x)

   Returns a boolean value indicating whether or not the object ``x``
   can be written as the given ``mime`` type.  (By default, this
   is determined automatically by the existence of the corresponding
   ``writemime`` function for ``typeof(x)``.)

.. function:: reprmime(mime, x)

   Returns a ``AbstractString`` or ``Vector{UInt8}`` containing the
   representation of ``x`` in the requested ``mime`` type, as written
   by ``writemime`` (throwing a ``MethodError`` if no appropriate
   ``writemime`` is available).  A ``AbstractString`` is returned for MIME
   types with textual representations (such as ``"text/html"`` or
   ``"application/postscript"``), whereas binary data is returned as
   ``Vector{UInt8}``.  (The function ``istext(mime)`` returns whether
   or not Julia treats a given ``mime`` type as text.)

   As a special case, if ``x`` is a ``AbstractString`` (for textual MIME types)
   or a ``Vector{UInt8}`` (for binary MIME types), the ``reprmime`` function
   assumes that ``x`` is already in the requested ``mime`` format and
   simply returns ``x``.

.. function:: stringmime(mime, x)

   Returns a ``AbstractString`` containing the representation of ``x`` in the
   requested ``mime`` type.  This is similar to ``reprmime`` except
   that binary data is base64-encoded as an ASCII string.

As mentioned above, one can also define new display backends. For
example, a module that can display PNG images in a window can register
this capability with Julia, so that calling ``display(x)`` on types
with PNG representations will automatically display the image using
the module's window.

In order to define a new display backend, one should first create a
subtype ``D`` of the abstract class ``Display``.  Then, for each MIME
type (``mime`` string) that can be displayed on ``D``, one should
define a function ``display(d::D, ::MIME"mime", x) = ...`` that
displays ``x`` as that MIME type, usually by calling ``reprmime(mime,
x)``.  A ``MethodError`` should be thrown if ``x`` cannot be displayed
as that MIME type; this is automatic if one calls ``reprmime``.
Finally, one should define a function ``display(d::D, x)`` that
queries ``mimewritable(mime, x)`` for the ``mime`` types supported by
``D`` and displays the "best" one; a ``MethodError`` should be thrown
if no supported MIME types are found for ``x``.  Similarly, some
subtypes may wish to override ``redisplay(d::D, ...)``.  (Again, one
should ``import Base.display`` to add new methods to ``display``.)
The return values of these functions are up to the implementation
(since in some cases it may be useful to return a display "handle" of
some type).  The display functions for ``D`` can then be called
directly, but they can also be invoked automatically from
``display(x)`` simply by pushing a new display onto the display-backend
stack with:

.. function:: pushdisplay(d::Display)

   Pushes a new display ``d`` on top of the global display-backend
   stack.  Calling ``display(x)`` or ``display(mime, x)`` will display
   ``x`` on the topmost compatible backend in the stack (i.e., the
   topmost backend that does not throw a ``MethodError``).

.. function:: popdisplay()
   	      popdisplay(d::Display)

   Pop the topmost backend off of the display-backend stack, or the
   topmost copy of ``d`` in the second variant.

.. function:: TextDisplay(stream)

   Returns a ``TextDisplay <: Display``, which can display any object
   as the text/plain MIME type (only), writing the text representation
   to the given I/O stream.  (The text representation is the same
   as the way an object is printed in the Julia REPL.)

.. function:: istext(m::MIME)

   Determine whether a MIME type is text data.

Memory-mapped I/O
-----------------

.. function:: mmap_array(type, dims, stream, [offset])

   Create an ``Array`` whose values are linked to a file, using memory-mapping. This provides a convenient way of working with data too large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted. Note that the file must be stored in binary format, and no format conversions are possible (this is a limitation of operating systems, not Julia).

   ``dims`` is a tuple specifying the size of the array.

   The file is passed via the stream argument.  When you initialize the stream, use ``"r"`` for a "read-only" array, and ``"w+"`` to create a new array used to write values to disk.

   Optionally, you can specify an offset (in bytes) if, for example, you want to skip over a header in the file. The default value for the offset is the current stream position.

   For example, the following code::

      # Create a file for mmapping
      # (you could alternatively use mmap_array to do this step, too)
      A = rand(1:20, 5, 30)
      s = open("/tmp/mmap.bin", "w+")
      # We'll write the dimensions of the array as the first two Ints in the file
      write(s, size(A,1))
      write(s, size(A,2))
      # Now write the data
      write(s, A)
      close(s)

      # Test by reading it back in
      s = open("/tmp/mmap.bin")   # default is read-only
      m = read(s, Int)
      n = read(s, Int)
      A2 = mmap_array(Int, (m,n), s)

   creates a ``m``-by-``n`` ``Matrix{Int}``, linked to the file associated with stream ``s``.

   A more portable file would need to encode the word size---32 bit or 64 bit---and endianness information in the header. In practice, consider encoding binary data using standard formats like HDF5 (which can be used with memory-mapping).

.. function:: mmap_bitarray([type,] dims, stream, [offset])

   Create a ``BitArray`` whose values are linked to a file, using memory-mapping; it has the same purpose, works in the same way, and has the same arguments, as :func:`mmap_array`, but the byte representation is different. The ``type`` parameter is optional, and must be ``Bool`` if given.

   **Example**:  ``B = mmap_bitarray((25,30000), s)``

   This would create a 25-by-30000 ``BitArray``, linked to the file associated with stream ``s``.

.. function:: msync(array)

   Forces synchronization between the in-memory version of a memory-mapped ``Array`` or ``BitArray`` and the on-disk version.

.. function:: msync(ptr, len, [flags])

   Forces synchronization of the :func:`mmap`\ ped memory region from ``ptr`` to ``ptr+len``. Flags defaults to ``MS_SYNC``, but can be a combination of ``MS_ASYNC``, ``MS_SYNC``, or ``MS_INVALIDATE``. See your platform man page for specifics. The flags argument is not valid on Windows.

   You may not need to call ``msync``, because synchronization is performed at intervals automatically by the operating system. However, you can call this directly if, for example, you are concerned about losing the result of a long-running calculation.

.. data:: MS_ASYNC

   Enum constant for :func:`msync`. See your platform man page for details. (not available on Windows).

.. data:: MS_SYNC

   Enum constant for :func:`msync`. See your platform man page for details. (not available on Windows).

.. data:: MS_INVALIDATE

   Enum constant for :func:`msync`. See your platform man page for details. (not available on Windows).

.. function:: mmap(len, prot, flags, fd, offset)

   Low-level interface to the ``mmap`` system call. See the man page.

.. function:: munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With :func:`mmap_array` you do not need to call this directly; the memory is unmapped for you when the array goes out of scope.

Standard Numeric Types
----------------------

``Bool`` ``Int8`` ``UInt8`` ``Int16`` ``UInt16`` ``Int32`` ``UInt32`` ``Int64`` ``UInt64`` ``Int128`` ``UInt128`` ``Float16`` ``Float32`` ``Float64`` ``Complex64`` ``Complex128``

.. _mathematical-operators:

Mathematical Operators
----------------------

.. function:: -(x)

   Unary minus operator.

.. _+:
.. function:: +(x, y...)

   Addition operator. ``x+y+z+...`` calls this function with all arguments, i.e.
   ``+(x, y, z, ...)``.

.. _-:
.. function:: -(x, y)

   Subtraction operator.

.. _*:
.. function:: *(x, y...)

   Multiplication operator. ``x*y*z*...`` calls this function with all arguments, i.e.
   ``*(x, y, z, ...)``.

.. _/:
.. function:: /(x, y)

   Right division operator: multiplication of ``x`` by the inverse of ``y`` on the right.
   Gives floating-point results for integer arguments.

.. _\\:
.. function:: \\(x, y)

   Left division operator: multiplication of ``y`` by the inverse of ``x`` on the left.
   Gives floating-point results for integer arguments.

.. _^:
.. function:: ^(x, y)

   Exponentiation operator.

.. _.+:
.. function:: .+(x, y)

   Element-wise addition operator.

.. _.-:
.. function:: .-(x, y)

   Element-wise subtraction operator.

.. _.*:
.. function:: .*(x, y)

   Element-wise multiplication operator.

.. _./:
.. function:: ./(x, y)

   Element-wise right division operator.

.. _.\\:
.. function:: .\\(x, y)

   Element-wise left division operator.

.. _.^:
.. function:: .^(x, y)

   Element-wise exponentiation operator.

.. function:: div(x, y)
              ÷(x, y)

   The quotient from Euclidean division. Computes ``x/y``, truncated to an integer.

.. function:: fld(x, y)

   Largest integer less than or equal to ``x/y``.

.. function:: cld(x, y)

   Smallest integer larger than or equal to ``x/y``.

.. function:: mod(x, y)

   Modulus after division, returning in the range [0,``y``), if ``y`` is
   positive, or (``y``,0] if ``y`` is negative.

.. function:: mod2pi(x)

   Modulus after division by 2pi, returning in the range [0,2pi).

   This function computes a floating point representation of the modulus after
   division by numerically exact 2pi, and is therefore not exactly the same as
   mod(x,2pi), which would compute the modulus of x relative to division by the
   floating-point number 2pi.

.. function:: rem(x, y)
              %(x, y)

   Remainder from Euclidean division, returning a value of the same sign
   as``x``, and smaller in magnitude than ``y``. This value is always exact.

.. function:: divrem(x, y)

   The quotient and remainder from Euclidean division. Equivalent to ``(x÷y, x%y)``.

.. function:: mod1(x,m)

   Modulus after division, returning in the range (0,m]

.. function:: rem1(x,m)

   Remainder after division, returning in the range (0,m]

.. _//:
.. function:: //(num, den)

   Divide two integers or rational numbers, giving a ``Rational`` result.

.. function:: rationalize([Type=Int,] x; tol=eps(x))

   Approximate floating point number ``x`` as a Rational number with components of the given
   integer type. The result will differ from ``x`` by no more than ``tol``.

.. function:: num(x)

   Numerator of the rational representation of ``x``

.. function:: den(x)

   Denominator of the rational representation of ``x``

.. _<<:
.. function:: <<(x, n)

   Left bit shift operator.

.. _>>:
.. function:: >>(x, n)

   Right bit shift operator, preserving the sign of ``x``.

.. _>>>:
.. function:: >>>(x, n)

   Unsigned right bit shift operator.

.. _\::
.. function:: \:(start, [step], stop)

   Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1,
   and ``a:s:b`` is similar but uses a step size of ``s``. These syntaxes call the
   function ``colon``.
   The colon is also used in indexing to select whole dimensions.

.. function:: colon(start, [step], stop)

   Called by ``:`` syntax for constructing ranges.

.. function:: range(start, [step], length)

   Construct a range by length, given a starting value and optional step (defaults to 1).

.. function:: linrange(start, end, length)

   Construct a range by length, given a starting and ending value.

.. _==:
.. function:: ==(x, y)

   Generic equality operator, giving a single ``Bool`` result. Falls back to ``===``.
   Should be implemented for all types with a notion of equality, based
   on the abstract value that an instance represents. For example, all numeric types are compared
   by numeric value, ignoring type. Strings are compared as sequences of characters, ignoring
   encoding.

   Follows IEEE semantics for floating-point numbers.

   Collections should generally implement ``==`` by calling ``==`` recursively on all contents.

   New numeric types should implement this function for two arguments of the new type, and handle
   comparison to other types via promotion rules where possible.

.. _!=:
.. function:: !=(x, y)
              ≠(x,y)

   Not-equals comparison operator. Always gives the opposite answer as ``==``.
   New types should generally not implement this, and rely on the fallback
   definition ``!=(x,y) = !(x==y)`` instead.

.. _===:
.. function:: ===(x, y)
              ≡(x,y)

   See the :func:`is` operator

.. _!==:
.. function:: !==(x, y)
              ≢(x,y)

   Equivalent to ``!is(x, y)``

.. _<:
.. function:: <(x, y)

   Less-than comparison operator. New numeric types should implement this function
   for two arguments of the new type.
   Because of the behavior of floating-point NaN values, ``<`` implements a
   partial order. Types with a canonical partial order should implement ``<``, and
   types with a canonical total order should implement ``isless``.

.. _<=:
.. function:: <=(x, y)
              ≤(x,y)

   Less-than-or-equals comparison operator.

.. _>:
.. function:: >(x, y)

   Greater-than comparison operator. Generally, new types should implement ``<``
   instead of this function, and rely on the fallback definition ``>(x,y) = y<x``.

.. _>=:
.. function:: >=(x, y)
              ≥(x,y)

   Greater-than-or-equals comparison operator.

.. _.==:
.. function:: .==(x, y)

   Element-wise equality comparison operator.

.. _.!=:
.. function:: .!=(x, y)
              .≠(x,y)

   Element-wise not-equals comparison operator.

.. _.<:
.. function:: .<(x, y)

   Element-wise less-than comparison operator.

.. _.<=:
.. function:: .<=(x, y)
              .≤(x,y)

   Element-wise less-than-or-equals comparison operator.

.. _.>:
.. function:: .>(x, y)

   Element-wise greater-than comparison operator.

.. _.>=:
.. function:: .>=(x, y)
              .≥(x,y)

   Element-wise greater-than-or-equals comparison operator.

.. function:: cmp(x,y)

   Return -1, 0, or 1 depending on whether ``x`` is less than, equal to, or greater
   than ``y``, respectively. Uses the total order implemented by ``isless``. For
   floating-point numbers, uses ``<`` but throws an error for unordered arguments.

.. _~:
.. function:: ~(x)

   Bitwise not

.. _&:
.. function:: &(x, y)

   Bitwise and

.. _|:
.. function:: |(x, y)

   Bitwise or

.. _$:
.. function:: $(x, y)

   Bitwise exclusive or

.. _!:
.. function:: !(x)

   Boolean not

.. _&&:
.. function:: x && y

   Short-circuiting boolean and

.. _||:
.. function:: x || y

   Short-circuiting boolean or

.. function:: A_ldiv_Bc(a,b)

   Matrix operator A \\ B\ :sup:`H`

.. function:: A_ldiv_Bt(a,b)

   Matrix operator A \\ B\ :sup:`T`

.. function:: A_mul_B(...)

   Matrix operator A B

.. function:: A_mul_Bc(...)

   Matrix operator A B\ :sup:`H`

.. function:: A_mul_Bt(...)

   Matrix operator A B\ :sup:`T`

.. function:: A_rdiv_Bc(...)

   Matrix operator A / B\ :sup:`H`

.. function:: A_rdiv_Bt(a,b)

   Matrix operator A / B\ :sup:`T`

.. function:: Ac_ldiv_B(...)

   Matrix operator A\ :sup:`H` \\ B

.. function:: Ac_ldiv_Bc(...)

   Matrix operator A\ :sup:`H` \\ B\ :sup:`H`

.. function:: Ac_mul_B(...)

   Matrix operator A\ :sup:`H` B

.. function:: Ac_mul_Bc(...)

   Matrix operator A\ :sup:`H` B\ :sup:`H`

.. function:: Ac_rdiv_B(a,b)

   Matrix operator A\ :sup:`H` / B

.. function:: Ac_rdiv_Bc(a,b)

   Matrix operator A\ :sup:`H` / B\ :sup:`H`

.. function:: At_ldiv_B(...)

   Matrix operator A\ :sup:`T` \\ B

.. function:: At_ldiv_Bt(...)

   Matrix operator A\ :sup:`T` \\ B\ :sup:`T`

.. function:: At_mul_B(...)

   Matrix operator A\ :sup:`T` B

.. function:: At_mul_Bt(...)

   Matrix operator A\ :sup:`T` B\ :sup:`T`

.. function:: At_rdiv_B(a,b)

   Matrix operator A\ :sup:`T` / B

.. function:: At_rdiv_Bt(a,b)

   Matrix operator A\ :sup:`T` / B\ :sup:`T`


Mathematical Functions
----------------------

.. function:: isapprox(x::Number, y::Number; rtol::Real=cbrt(maxeps), atol::Real=sqrt(maxeps))

   Inexact equality comparison - behaves slightly different depending on types of input args:

   * For ``FloatingPoint`` numbers, ``isapprox`` returns ``true`` if ``abs(x-y) <= atol + rtol*max(abs(x), abs(y))``.

   * For ``Integer`` and ``Rational`` numbers, ``isapprox`` returns ``true`` if ``abs(x-y) <= atol``. The `rtol` argument is ignored. If one of ``x`` and ``y`` is ``FloatingPoint``, the other is promoted, and the method above is called instead.

   * For ``Complex`` numbers, the distance in the complex plane is compared, using the same criterion as above.

   For default tolerance arguments, ``maxeps = max(eps(abs(x)), eps(abs(y)))``.

.. function:: sin(x)

   Compute sine of ``x``, where ``x`` is in radians

.. function:: cos(x)

   Compute cosine of ``x``, where ``x`` is in radians

.. function:: tan(x)

   Compute tangent of ``x``, where ``x`` is in radians

.. function:: sind(x)

   Compute sine of ``x``, where ``x`` is in degrees

.. function:: cosd(x)

   Compute cosine of ``x``, where ``x`` is in degrees

.. function:: tand(x)

   Compute tangent of ``x``, where ``x`` is in degrees

.. function:: sinpi(x)

   Compute :math:`\sin(\pi x)` more accurately than ``sin(pi*x)``, especially for large ``x``.

.. function:: cospi(x)

   Compute :math:`\cos(\pi x)` more accurately than ``cos(pi*x)``, especially for large ``x``.

.. function:: sinh(x)

   Compute hyperbolic sine of ``x``

.. function:: cosh(x)

   Compute hyperbolic cosine of ``x``

.. function:: tanh(x)

   Compute hyperbolic tangent of ``x``

.. function:: asin(x)

   Compute the inverse sine of ``x``, where the output is in radians

.. function:: acos(x)

   Compute the inverse cosine of ``x``, where the output is in radians

.. function:: atan(x)

   Compute the inverse tangent of ``x``, where the output is in radians

.. function:: atan2(y, x)

   Compute the inverse tangent of ``y/x``, using the signs of both ``x`` and ``y`` to determine the quadrant of the return value.

.. function:: asind(x)

   Compute the inverse sine of ``x``, where the output is in degrees

.. function:: acosd(x)

   Compute the inverse cosine of ``x``, where the output is in degrees

.. function:: atand(x)

   Compute the inverse tangent of ``x``, where the output is in degrees

.. function:: sec(x)

   Compute the secant of ``x``, where ``x`` is in radians

.. function:: csc(x)

   Compute the cosecant of ``x``, where ``x`` is in radians

.. function:: cot(x)

   Compute the cotangent of ``x``, where ``x`` is in radians

.. function:: secd(x)

   Compute the secant of ``x``, where ``x`` is in degrees

.. function:: cscd(x)

   Compute the cosecant of ``x``, where ``x`` is in degrees

.. function:: cotd(x)

   Compute the cotangent of ``x``, where ``x`` is in degrees

.. function:: asec(x)

   Compute the inverse secant of ``x``, where the output is in radians

.. function:: acsc(x)

   Compute the inverse cosecant of ``x``, where the output is in radians

.. function:: acot(x)

   Compute the inverse cotangent of ``x``, where the output is in radians

.. function:: asecd(x)

   Compute the inverse secant of ``x``, where the output is in degrees

.. function:: acscd(x)

   Compute the inverse cosecant of ``x``, where the output is in degrees

.. function:: acotd(x)

   Compute the inverse cotangent of ``x``, where the output is in degrees

.. function:: sech(x)

   Compute the hyperbolic secant of ``x``

.. function:: csch(x)

   Compute the hyperbolic cosecant of ``x``

.. function:: coth(x)

   Compute the hyperbolic cotangent of ``x``

.. function:: asinh(x)

   Compute the inverse hyperbolic sine of ``x``

.. function:: acosh(x)

   Compute the inverse hyperbolic cosine of ``x``

.. function:: atanh(x)

   Compute the inverse hyperbolic tangent of ``x``

.. function:: asech(x)

   Compute the inverse hyperbolic secant of ``x``

.. function:: acsch(x)

   Compute the inverse hyperbolic cosecant of ``x``

.. function:: acoth(x)

   Compute the inverse hyperbolic cotangent of ``x``

.. function:: sinc(x)

   Compute :math:`\sin(\pi x) / (\pi x)` if :math:`x \neq 0`, and :math:`1` if :math:`x = 0`.

.. function:: cosc(x)

   Compute :math:`\cos(\pi x) / x - \sin(\pi x) / (\pi x^2)` if :math:`x \neq 0`, and :math:`0`
   if :math:`x = 0`. This is the derivative of ``sinc(x)``.

.. function:: deg2rad(x)

   Convert ``x`` from degrees to radians

.. function:: rad2deg(x)

   Convert ``x`` from radians to degrees

.. function:: hypot(x, y)

   Compute the :math:`\sqrt{x^2+y^2}` avoiding overflow and underflow

.. function:: log(x)

   Compute the natural logarithm of ``x``. Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments instead.

.. function:: log(b,x)

   Compute the base ``b`` logarithm of ``x``. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log2(x)

   Compute the logarithm of ``x`` to base 2. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log10(x)

   Compute the logarithm of ``x`` to base 10. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log1p(x)

   Accurate natural logarithm of ``1+x``.  Throws ``DomainError`` for ``Real`` arguments less than -1.

.. function:: frexp(val)

   Return ``(x,exp)`` such that ``x`` has a magnitude in the interval ``[1/2, 1)`` or 0,
   and val = :math:`x \times 2^{exp}`.

.. function:: exp(x)

   Compute :math:`e^x`

.. function:: exp2(x)

   Compute :math:`2^x`

.. function:: exp10(x)

   Compute :math:`10^x`

.. function:: ldexp(x, n)

   Compute :math:`x \times 2^n`

.. function:: modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts of a
   number. Both parts have the same sign as the argument.

.. function:: expm1(x)

   Accurately compute :math:`e^x-1`

.. function:: round([T,] x, [digits, [base]], [r::RoundingMode])

   ``round(x)`` returns an integral value of the same type as ``x``
   to ``x``, according to the default rounding mode (see ``get_rounding``). By
   default, this will round to the nearest integer, with ties (fractional
   values of 0.5) being rounded to the even integer.

   .. doctest::

      julia> round(1.7)
      2.0

      julia> round(1.5)
      2.0

      julia> round(2.5)
      2.0

   The optional ``roundingmode`` argument will change this
   behaviour. Currently supported are

   * `RoundNearestTiesAway`: this emulates C-style ``round`` behaviour, by
     rounding ties away from zero.
   * `RoundNearestTiesUp`: this emulates Java-style ``round`` behaviour, by
     rounding ties toward positive infinity.
   * `RoundToZero`: an alias for `trunc`
   * `RoundUp`: an alias for `ceil`
   * `RoundDown`: an alias for `floor`

   ``round(T, x, [r::RoundingMode])`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``round(x, digits)`` rounds to the specified number of digits after the decimal place, or before if negative, e.g., ``round(pi,2)`` is ``3.14``. ``round(x, digits, base)`` rounds using a different base, defaulting to 10, e.g., ``round(pi, 1, 8)`` is ``3.125``.

.. function:: ceil([T,] x, [digits, [base]])

   ``ceil(x)`` returns the nearest integral value of the same type as ``x``
   that is greater than or equal to ``x``.

   ``ceil(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for ``round``.

.. function:: floor([T,] x, [digits, [base]])

   ``floor(x)`` returns the nearest integral value of the same type as ``x``
   that is less than or equal to ``x``.

   ``floor(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as above.

.. function:: trunc([T,] x, [digits, [base]])

   ``trunc(x)`` returns the nearest integral value of the same type as ``x`` whose absolute
   value is less than or equal to ``x``.

   ``trunc(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as above.

.. function:: unsafe_trunc(T, x)

   ``unsafe_trunc(T, x)`` returns the nearest integral value of type ``T`` whose absolute
   value is less than or equal to ``x``. If the value is not representable by
   ``T``, an arbitrary value will be returned.

.. function:: signif(x, digits, [base])

   Rounds (in the sense of ``round``) ``x`` so that there are ``digits`` significant digits, under a base ``base`` representation, default 10. E.g., ``signif(123.456, 2)`` is ``120.0``, and ``signif(357.913, 4, 2)`` is ``352.0``.

.. function:: min(x, y, ...)

   Return the minimum of the arguments. Operates elementwise over arrays.

.. function:: max(x, y, ...)

   Return the maximum of the arguments. Operates elementwise over arrays.

.. function:: minmax(x, y)

   Return ``(min(x,y), max(x,y))``.
   See also: :func:`extrema` that returns ``(minimum(x), maximum(x))``

.. function:: clamp(x, lo, hi)

   Return x if ``lo <= x <= hi``. If ``x < lo``, return ``lo``. If ``x > hi``, return ``hi``. Arguments are promoted to a common type. Operates elementwise over ``x`` if it is an array.

.. function:: abs(x)

   Absolute value of ``x``

.. function:: abs2(x)

   Squared absolute value of ``x``

.. function:: copysign(x, y)

   Return ``x`` such that it has the same sign as ``y``

.. function:: sign(x)

   Return ``+1`` if ``x`` is positive, ``0`` if ``x == 0``, and ``-1`` if ``x`` is negative.

.. function:: signbit(x)

   Returns ``true`` if the value of the sign of ``x`` is negative, otherwise ``false``.

.. function:: flipsign(x, y)

   Return ``x`` with its sign flipped if ``y`` is negative. For example ``abs(x) = flipsign(x,x)``.

.. function:: sqrt(x)

   Return :math:`\sqrt{x}`. Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments instead.  The prefix operator ``√`` is equivalent to ``sqrt``.

.. function:: isqrt(n)

   Integer square root: the largest integer ``m`` such that ``m*m <= n``.

.. function:: cbrt(x)

   Return :math:`x^{1/3}`.  The prefix operator ``∛`` is equivalent to ``cbrt``.

.. function:: erf(x)

   Compute the error function of ``x``, defined by
   :math:`\frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt`
   for arbitrary complex ``x``.

.. function:: erfc(x)

   Compute the complementary error function of ``x``,
   defined by :math:`1 - \operatorname{erf}(x)`.

.. function:: erfcx(x)

   Compute the scaled complementary error function of ``x``,
   defined by :math:`e^{x^2} \operatorname{erfc}(x)`.  Note
   also that :math:`\operatorname{erfcx}(-ix)` computes the
   Faddeeva function :math:`w(x)`.

.. function:: erfi(x)

   Compute the imaginary error function of ``x``,
   defined by :math:`-i \operatorname{erf}(ix)`.

.. function:: dawson(x)

   Compute the Dawson function (scaled imaginary error function) of ``x``,
   defined by :math:`\frac{\sqrt{\pi}}{2} e^{-x^2} \operatorname{erfi}(x)`.

.. function:: erfinv(x)

   Compute the inverse error function of a real ``x``,
   defined by :math:`\operatorname{erf}(\operatorname{erfinv}(x)) = x`.

.. function:: erfcinv(x)

   Compute the inverse error complementary function of a real ``x``,
   defined by :math:`\operatorname{erfc}(\operatorname{erfcinv}(x)) = x`.

.. function:: real(z)

   Return the real part of the complex number ``z``

.. function:: imag(z)

   Return the imaginary part of the complex number ``z``

.. function:: reim(z)

   Return both the real and imaginary parts of the complex number ``z``

.. function:: conj(z)

   Compute the complex conjugate of a complex number ``z``

.. function:: angle(z)

   Compute the phase angle of a complex number ``z``

.. function:: cis(z)

   Return :math:`\exp(iz)`.

.. function:: binomial(n,k)

   Number of ways to choose ``k`` out of ``n`` items

.. function:: factorial(n)

   Factorial of n

.. function:: factorial(n,k)

   Compute ``factorial(n)/factorial(k)``

.. function:: factor(n) -> Dict

   Compute the prime factorization of an integer ``n``. Returns a dictionary. The keys of the dictionary correspond to the factors, and hence are of the same type as ``n``. The value associated with each key indicates the number of times the factor appears in the factorization.

   .. doctest::

      julia> factor(100) # == 2*2*5*5
      Dict{Int64,Int64} with 2 entries:
        2 => 2
        5 => 2

.. function:: gcd(x,y)

   Greatest common (positive) divisor (or zero if x and y are both zero).

.. function:: lcm(x,y)

   Least common (non-negative) multiple.

.. function:: gcdx(x,y)

   Computes the greatest common (positive) divisor of ``x`` and ``y`` and their Bézout coefficients, i.e. the integer coefficients ``u`` and ``v`` that satisfy :math:`ux+vy = d = gcd(x,y)`.

   .. doctest::

      julia> gcdx(12, 42)
      (6,-3,1)

   .. doctest::

      julia> gcdx(240, 46)
      (2,-9,47)

   .. note::

      Bézout coefficients are *not* uniquely defined. ``gcdx`` returns the minimal Bézout coefficients that are computed by the extended Euclid algorithm. (Ref: D. Knuth, TAoCP, 2/e, p. 325, Algorithm X.) These coefficients ``u`` and ``v`` are minimal in the sense that :math:`|u| < |\frac y d` and :math:`|v| < |\frac x d`. Furthermore, the signs of ``u`` and ``v`` are chosen so that ``d`` is positive.

.. function:: ispow2(n) -> Bool

   Test whether ``n`` is a power of two

.. function:: nextpow2(n)

   The smallest power of two not less than ``n``. Returns 0 for ``n==0``, and returns
   ``-nextpow2(-n)`` for negative arguments.

.. function:: prevpow2(n)

   The largest power of two not greater than ``n``. Returns 0 for ``n==0``, and returns
   ``-prevpow2(-n)`` for negative arguments.

.. function:: nextpow(a, x)

   The smallest ``a^n`` not less than ``x``, where ``n`` is a non-negative integer.
   ``a`` must be greater than 1, and ``x`` must be greater than 0.

.. function:: prevpow(a, x)

   The largest ``a^n`` not greater than ``x``, where ``n`` is a non-negative integer.
   ``a`` must be greater than 1, and ``x`` must not be less than 1.

.. function:: nextprod([k_1,k_2,...], n)

   Next integer not less than ``n`` that can be written as :math:`\prod k_i^{p_i}` for integers :math:`p_1`, :math:`p_2`, etc.

.. function:: prevprod([k_1,k_2,...], n)

   Previous integer not greater than ``n`` that can be written as :math:`\prod k_i^{p_i}` for integers :math:`p_1`, :math:`p_2`, etc.

.. function:: invmod(x,m)

   Take the inverse of ``x`` modulo ``m``: ``y`` such that :math:`xy = 1 \pmod m`

.. function:: powermod(x, p, m)

   Compute :math:`x^p \pmod m`

.. function:: gamma(x)

   Compute the gamma function of ``x``

.. function:: lgamma(x)

   Compute the logarithm of absolute value of ``gamma(x)``

.. function:: lfact(x)

   Compute the logarithmic factorial of ``x``

.. function:: digamma(x)

   Compute the digamma function of ``x`` (the logarithmic derivative of ``gamma(x)``)

.. function:: invdigamma(x)

   Compute the inverse digamma function of ``x``.

.. function:: trigamma(x)

   Compute the trigamma function of ``x`` (the logarithmic second derivative of ``gamma(x)``)

.. function:: polygamma(m, x)

   Compute the polygamma function of order ``m`` of argument ``x`` (the ``(m+1)th`` derivative of the logarithm of ``gamma(x)``)

.. function:: airy(k,x)

   kth derivative of the Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyai(x)

   Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airyaiprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airybi(x)

   Airy function :math:`\operatorname{Bi}(x)`.

.. function:: airybiprime(x)

   Airy function derivative :math:`\operatorname{Bi}'(x)`.

.. function:: airyx(k,x)

   scaled kth derivative of the Airy function, return :math:`\operatorname{Ai}(x) e^{\frac{2}{3} x \sqrt{x}}` for ``k == 0 || k == 1``, and :math:`\operatorname{Ai}(x) e^{- \left| \operatorname{Re} \left( \frac{2}{3} x \sqrt{x} \right) \right|}` for ``k == 2 || k == 3``.

.. function:: besselj0(x)

   Bessel function of the first kind of order 0, :math:`J_0(x)`.

.. function:: besselj1(x)

   Bessel function of the first kind of order 1, :math:`J_1(x)`.

.. function:: besselj(nu, x)

   Bessel function of the first kind of order ``nu``, :math:`J_\nu(x)`.

.. function:: besseljx(nu, x)

   Scaled Bessel function of the first kind of order ``nu``, :math:`J_\nu(x) e^{- | \operatorname{Im}(x) |}`.

.. function:: bessely0(x)

   Bessel function of the second kind of order 0, :math:`Y_0(x)`.

.. function:: bessely1(x)

   Bessel function of the second kind of order 1, :math:`Y_1(x)`.

.. function:: bessely(nu, x)

   Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x)`.

.. function:: besselyx(nu, x)

   Scaled Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x) e^{- | \operatorname{Im}(x) |}`.

.. function:: hankelh1(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x)`.

.. function:: hankelh1x(nu, x)

   Scaled Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x) e^{-x i}`.

.. function:: hankelh2(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x)`.

.. function:: hankelh2x(nu, x)

   Scaled Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x) e^{x i}`.

.. function:: besselh(nu, k, x)

   Bessel function of the third kind of order ``nu`` (Hankel function).
   ``k`` is either 1 or 2, selecting ``hankelh1`` or ``hankelh2``, respectively.

.. function:: besseli(nu, x)

   Modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x)`.

.. function:: besselix(nu, x)

   Scaled modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x) e^{- | \operatorname{Re}(x) |}`.

.. function:: besselk(nu, x)

   Modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x)`.

.. function:: besselkx(nu, x)

   Scaled modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x) e^x`.

.. function:: beta(x, y)

   Euler integral of the first kind :math:`\operatorname{B}(x,y) = \Gamma(x)\Gamma(y)/\Gamma(x+y)`.

.. function:: lbeta(x, y)

   Natural logarithm of the absolute value of the beta function :math:`\log(|\operatorname{B}(x,y)|)`.

.. function:: eta(x)

   Dirichlet eta function :math:`\eta(s) = \sum^\infty_{n=1}(-)^{n-1}/n^{s}`.

.. function:: zeta(s)

   Riemann zeta function :math:`\zeta(s)`.

.. function:: zeta(s, z)

   Hurwitz zeta function :math:`\zeta(s, z)`.  (This is equivalent to
   the Riemann zeta function :math:`\zeta(s)` for the case of ``z=1``.)

.. function:: ndigits(n, b)

   Compute the number of digits in number ``n`` written in base ``b``.

.. function:: widemul(x, y)

   Multiply ``x`` and ``y``, giving the result as a larger type.

.. function:: @evalpoly(z, c...)

   Evaluate the polynomial :math:`\sum_k c[k] z^{k-1}` for the
   coefficients ``c[1]``, ``c[2]``, ...; that is, the coefficients are
   given in ascending order by power of ``z``.  This macro expands to
   efficient inline code that uses either Horner's method or, for
   complex ``z``, a more efficient Goertzel-like algorithm.

Data Formats
------------

.. function:: bin(n, [pad])

   Convert an integer to a binary string, optionally specifying a number of digits to pad to.

.. function:: hex(n, [pad])

   Convert an integer to a hexadecimal string, optionally specifying a number of digits to pad to.

.. function:: dec(n, [pad])

   Convert an integer to a decimal string, optionally specifying a number of digits to pad to.

.. function:: oct(n, [pad])

   Convert an integer to an octal string, optionally specifying a number of digits to pad to.

.. function:: base(base, n, [pad])

   Convert an integer to a string in the given base, optionally specifying a number of digits to pad to. The base can be specified as either an integer, or as a ``UInt8`` array of character values to use as digit symbols.

.. function:: digits(n, [base], [pad])

   Returns an array of the digits of ``n`` in the given base, optionally padded with
   zeros to a specified size. More significant digits are at higher indexes, such
   that ``n == sum([digits[k]*base^(k-1) for k=1:length(digits)])``.

.. function:: digits!(array, n, [base])

    Fills an array of the digits of ``n`` in the given base. More significant digits are at higher indexes.
    If the array length is insufficient, the least significant digits are filled up to the array length.
    If the array length is excessive, the excess portion is filled with zeros.

.. function:: bits(n)

   A string giving the literal bit representation of a number.

.. function:: parseint([type], str, [base])

   Parse a string as an integer in the given base (default 10), yielding a number of the specified type (default ``Int``).

.. function:: parsefloat([type], str)

   Parse a string as a decimal floating point number, yielding a number of the specified type.

.. function:: big(x)

   Convert a number to a maximum precision representation (typically ``BigInt`` or ``BigFloat``). See ``BigFloat`` for information about some pitfalls with floating-point numbers.

.. function:: bool(x)

   Convert a number or numeric array to boolean

.. function:: int(x)

   Convert a number or array to the default integer type on your platform. Alternatively, ``x`` can be a string, which is parsed as an integer.

.. function:: uint(x)

   Convert a number or array to the default unsigned integer type on your platform. Alternatively, ``x`` can be a string, which is parsed as an unsigned integer.

.. function:: integer(x)

   Convert a number or array to integer type. If ``x`` is already of integer type it is unchanged, otherwise it converts it to the default integer type on your platform.

.. function:: signed(x)

   Convert a number to a signed integer. If the argument is unsigned, it is reinterpreted as signed without checking for overflow.

.. function:: unsigned(x) -> Unsigned

   Convert a number to an unsigned integer. If the argument is signed, it is reinterpreted as unsigned without checking for negative values.

.. function:: int8(x)

   Convert a number or array to ``Int8`` data type

.. function:: int16(x)

   Convert a number or array to ``Int16`` data type

.. function:: int32(x)

   Convert a number or array to ``Int32`` data type

.. function:: int64(x)

   Convert a number or array to ``Int64`` data type

.. function:: int128(x)

   Convert a number or array to ``Int128`` data type

.. function:: uint8(x)

   Convert a number or array to ``UInt8`` data type

.. function:: uint16(x)

   Convert a number or array to ``UInt16`` data type

.. function:: uint32(x)

   Convert a number or array to ``UInt32`` data type

.. function:: uint64(x)

   Convert a number or array to ``UInt64`` data type

.. function:: uint128(x)

   Convert a number or array to ``UInt128`` data type

.. function:: float16(x)

   Convert a number or array to ``Float16`` data type

.. function:: float32(x)

   Convert a number or array to ``Float32`` data type

.. function:: float64(x)

   Convert a number or array to ``Float64`` data type

.. function:: float32_isvalid(x, out::Vector{Float32}) -> Bool

   Convert a number or array to ``Float32`` data type, returning true if successful. The result of the conversion is stored in ``out[1]``.

.. function:: float64_isvalid(x, out::Vector{Float64}) -> Bool

   Convert a number or array to ``Float64`` data type, returning true if successful. The result of the conversion is stored in ``out[1]``.

.. function:: float(x)

   Convert a number, array, or string to a ``FloatingPoint`` data type. For numeric data, the smallest suitable ``FloatingPoint`` type is used. Converts strings to ``Float64``.

   This function is not recommended for arrays. It is better to use a more specific function such as ``float32`` or ``float64``.

.. function:: significand(x)

   Extract the significand(s) (a.k.a. mantissa), in binary representation, of a floating-point number or array.

   .. doctest::

      julia> significand(15.2)/15.2
      0.125

      julia> significand(15.2)*8
      15.2

.. function:: exponent(x) -> Int

   Get the exponent of a normalized floating-point number.

.. function:: complex64(r, [i])

   Convert to ``r + i*im`` represented as a ``Complex64`` data type. ``i`` defaults to zero.

.. function:: complex128(r, [i])

   Convert to ``r + i*im`` represented as a ``Complex128`` data type. ``i`` defaults to zero.

.. function:: complex(r, [i])

   Convert real numbers or arrays to complex. ``i`` defaults to zero.

.. function:: char(x)

   Convert a number or array to ``Char`` data type

.. function:: bswap(n)

   Byte-swap an integer

.. function:: num2hex(f)

   Get a hexadecimal string of the binary representation of a floating point number

.. function:: hex2num(str)

   Convert a hexadecimal string to the floating point number it represents

.. function:: hex2bytes(s::ASCIIString)

   Convert an arbitrarily long hexadecimal string to its binary representation. Returns an Array{UInt8, 1}, i.e. an array of bytes.

.. function:: bytes2hex(bin_arr::Array{UInt8, 1})

   Convert an array of bytes to its hexadecimal representation. All characters are in lower-case. Returns an ASCIIString.


Numbers
-------

.. function:: one(x)

   Get the multiplicative identity element for the type of x (x can also specify the type itself). For matrices, returns an identity matrix of the appropriate size and type.

.. function:: zero(x)

   Get the additive identity element for the type of x (x can also specify the type itself).

.. data:: pi
          π

   The constant pi

.. data:: im

   The imaginary unit

.. data:: e

   The constant e

.. data:: catalan

   Catalan's constant

.. data:: γ

   Euler's constant

.. data:: φ

   The golden ratio

.. data:: Inf

   Positive infinity of type Float64

.. data:: Inf32

   Positive infinity of type Float32

.. data:: Inf16

   Positive infinity of type Float16

.. data:: NaN

   A not-a-number value of type Float64

.. data:: NaN32

   A not-a-number value of type Float32

.. data:: NaN16

   A not-a-number value of type Float16

.. function:: issubnormal(f) -> Bool

   Test whether a floating point number is subnormal

.. function:: isfinite(f) -> Bool

   Test whether a number is finite

.. function:: isinf(f) -> Bool

   Test whether a number is infinite

.. function:: isnan(f) -> Bool

   Test whether a floating point number is not a number (NaN)

.. function:: inf(f)

   Returns positive infinity of the floating point type ``f`` or of the same floating point type as ``f``

.. function:: nan(f)

   Returns NaN (not-a-number) of the floating point type ``f`` or of the same floating point type as ``f``

.. function:: nextfloat(f)

   Get the next floating point number in lexicographic order

.. function:: prevfloat(f) -> FloatingPoint

   Get the previous floating point number in lexicographic order

.. function:: isinteger(x) -> Bool

   Test whether ``x`` or all its elements are numerically equal to some integer

.. function:: isreal(x) -> Bool

   Test whether ``x`` or all its elements are numerically equal to some real number

.. function:: Float32(x [, mode::RoundingMode])

   Create a Float32 from ``x``. If ``x`` is not exactly representable then
   ``mode`` determines how ``x`` is rounded.

   .. doctest::

      julia> Float32(1/3, RoundDown)
      0.3333333f0

      julia> Float32(1/3, RoundUp)
      0.33333334f0

   See ``get_rounding`` for available rounding modes.

.. function:: Float64(x [, mode::RoundingMode])

   Create a Float64 from ``x``. If ``x`` is not exactly representable then
   ``mode`` determines how ``x`` is rounded.

   .. doctest::

      julia> Float64(pi, RoundDown)
      3.141592653589793

      julia> Float64(pi, RoundUp)
      3.1415926535897936

   See ``get_rounding`` for available rounding modes.

.. function:: BigInt(x)

   Create an arbitrary precision integer. ``x`` may be an ``Int`` (or anything that can be converted to an ``Int``) or a ``AbstractString``.
   The usual mathematical operators are defined for this type, and results are promoted to a ``BigInt``.

.. function:: BigFloat(x)

   Create an arbitrary precision floating point number. ``x`` may be
   an ``Integer``, a ``Float64``, a ``AbstractString`` or a ``BigInt``. The
   usual mathematical operators are defined for this type, and results
   are promoted to a ``BigFloat``. Note that because floating-point
   numbers are not exactly-representable in decimal notation,
   ``BigFloat(2.1)`` may not yield what you expect. You may prefer to
   initialize constants using strings, e.g., ``BigFloat("2.1")``.

.. function:: get_rounding(T)

   Get the current floating point rounding mode for type ``T``. Valid modes
   are ``RoundNearest``, ``RoundToZero``, ``RoundUp``, ``RoundDown``, and
   ``RoundFromZero`` (``BigFloat`` only).

.. function:: set_rounding(T, mode)

   Set the rounding mode of floating point type ``T``. Note that this may
   affect other types, for instance changing the rounding mode of ``Float64``
   will change the rounding mode of ``Float32``. See ``get_rounding`` for available modes

.. function:: with_rounding(f::Function, T, mode)

   Change the rounding mode of floating point type ``T`` for the duration of ``f``. It is logically equivalent to::

       old = get_rounding(T)
       set_rounding(T, mode)
       f()
       set_rounding(T, old)

   See ``get_rounding`` for available rounding modes.

Integers
~~~~~~~~

.. function:: count_ones(x::Integer) -> Integer

   Number of ones in the binary representation of ``x``.

   .. doctest::

      julia> count_ones(7)
      3

.. function:: count_zeros(x::Integer) -> Integer

   Number of zeros in the binary representation of ``x``.

   .. doctest::

      julia> count_zeros(int32(2 ^ 16 - 1))
      16

.. function:: leading_zeros(x::Integer) -> Integer

   Number of zeros leading the binary representation of ``x``.

   .. doctest::

      julia> leading_zeros(int32(1))
      31

.. function:: leading_ones(x::Integer) -> Integer

   Number of ones leading the binary representation of ``x``.

   .. doctest::

      julia> leading_ones(uint32(2 ^ 32 - 2))
      31

.. function:: trailing_zeros(x::Integer) -> Integer

   Number of zeros trailing the binary representation of ``x``.

   .. doctest::

      julia> trailing_zeros(2)
      1

.. function:: trailing_ones(x::Integer) -> Integer

   Number of ones trailing the binary representation of ``x``.

   .. doctest::

      julia> trailing_ones(3)
      2

.. function:: isprime(x::Integer) -> Bool

   Returns ``true`` if ``x`` is prime, and ``false`` otherwise.

   .. doctest::

   	julia> isprime(3)
   	true

.. function:: primes(n)

   Returns a collection of the prime numbers <= ``n``.

.. function:: isodd(x::Integer) -> Bool

   Returns ``true`` if ``x`` is odd (that is, not divisible by 2), and ``false`` otherwise.

   .. doctest::

   	julia> isodd(9)
   	true

   	julia> isodd(10)
   	false

.. function:: iseven(x::Integer) -> Bool

   Returns ``true`` is ``x`` is even (that is, divisible by 2), and ``false`` otherwise.

   .. doctest::

   	julia> iseven(9)
   	false

   	julia> iseven(10)
   	true

BigFloats
---------
The `BigFloat` type implements arbitrary-precision floating-point arithmetic using the `GNU MPFR library <http://www.mpfr.org/>`_.

.. function:: precision(num::FloatingPoint)

   Get the precision of a floating point number, as defined by the effective number of bits in the mantissa.

.. function:: get_bigfloat_precision()

   Get the precision (in bits) currently used for BigFloat arithmetic.

.. function:: set_bigfloat_precision(x::Int64)

   Set the precision (in bits) to be used to BigFloat arithmetic.

.. function:: with_bigfloat_precision(f::Function,precision::Integer)

   Change the BigFloat arithmetic precision (in bits) for the duration of ``f``. It is logically equivalent to::

       old = get_bigfloat_precision()
       set_bigfloat_precision(precision)
       f()
       set_bigfloat_precision(old)

Random Numbers
--------------

Random number generation in Julia uses the `Mersenne Twister library <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT>`_ via ``MersenneTwister`` objects.
Julia has a global RNG, which is used by default. Other RNG types can be plugged in by inheriting the ``AbstractRNG`` type;
they can then be used to have multiple streams of random numbers.
Besides ``MersenneTwister``, Julia also provides the ``RandomDevice`` RNG type, which is a wrapper over the OS provided entropy.

Most functions related to random generation accept an optional ``AbstractRNG`` as the first argument, ``rng`` , which defaults to the global one if not provided.
Morever, some of them accept optionally dimension specifications ``dims...`` (which can be given as a tuple) to generate arrays of random values.

A ``MersenneTwister`` or ``RandomDevice`` RNG can generate random numbers of the following types:
``Float16``, ``Float32``, ``Float64``, ``Bool``, ``Int8``, ``UInt8``, ``Int16``, ``UInt16``,
``Int32``, ``UInt32``, ``Int64``, ``UInt64``, ``Int128``, ``UInt128``, ``BigInt``
(or complex numbers of those types). Random floating point numbers are generated uniformly in [0,1).
As ``BigInt`` represents unbounded integers, the interval must be specified (e.g. ``rand(big(1:6))``).

.. function:: srand([rng], [seed])

   Reseed the random number generator. If a ``seed`` is provided, the RNG will give a reproducible sequence of numbers, otherwise Julia will get entropy from the system.
   For ``MersenneTwister``, the ``seed`` may be a non-negative integer, a vector of ``UInt32`` integers or a filename, in which case the seed is read from a file.
   ``RandomDevice`` does not support seeding.

.. function:: MersenneTwister([seed])

   Create a ``MersenneTwister`` RNG object. Different RNG objects can have their own seeds, which may be useful for generating different streams of random numbers.

.. function:: RandomDevice()

   Create a ``RandomDevice`` RNG object. Two such objects will always generate different streams of random numbers.

.. function:: rand([rng], [S], [dims...])

   Pick a random element or array of random elements from the set of values specified by ``S``; ``S`` can be

   * an indexable collection (for example ``1:n`` or ``['x','y','z']``), or

   * a type: the set of values to pick from is then equivalent to ``typemin(S):typemax(S)`` for integers (this is not applicable to ``BigInt``), and to [0,1) for floating point numbers;

   ``S`` defaults to ``Float64``.

.. function:: rand!([rng], A ,[coll])

   Populate the array A with random values. If the indexable collection ``coll`` is specified, the values are picked randomly from ``coll``. This is equivalent to ``copy!(A, rand(rng, coll, size(A)))`` or ``copy!(A, rand(rng, eltype(A), size(A)))`` but without allocating a new array.

.. function:: randbool([rng], [dims...])

   Generate a random boolean value. Optionally, generate a ``BitArray`` of random boolean values.

.. function:: randn([rng], [dims...])

   Generate a normally-distributed random number with mean 0 and standard deviation 1. Optionally generate an array of normally-distributed random numbers.

.. function:: randn!([rng], A::Array{Float64,N})

   Fill the array A with normally-distributed (mean 0, standard deviation 1) random numbers. Also see the rand function.

.. function:: randexp([rng], [dims...])

   Generate a random number according to the exponential distribution with scale 1. Optionally generate an array of such random numbers.

.. function:: randexp!([rng], A::Array{Float64,N})

   Fill the array A with random numbers following the exponential distribution (with scale 1).

Arrays
------

Basic functions
~~~~~~~~~~~~~~~

.. function:: ndims(A) -> Integer

   Returns the number of dimensions of A

.. function:: size(A, [dim...])

   Returns a tuple containing the dimensions of A. Optionally you can specify the dimension(s) you want the length of, and get the length of that dimension, or a tuple of the lengths of dimensions you asked for.::

    julia> A = rand(2,3,4);

    julia> size(A, 2)
    3

    julia> size(A,3,2)
    (4,3)

.. function:: iseltype(A,T)

   Tests whether A or its elements are of type T

.. function:: length(A) -> Integer

   Returns the number of elements in A

.. function:: eachindex(A)

   Creates an iterable object for visiting each multi-dimensional index of the AbstractArray ``A``.  Example for a 2-d array::

    julia> A = rand(2,3)
    2x3 Array{Float64,2}:
     0.960084  0.629326  0.625155
     0.432588  0.955903  0.991614

    julia> for iter in eachindex(A)
	       @show iter.I_1, iter.I_2
	       @show A[iter]
	   end
    (iter.I_1,iter.I_2) = (1,1)
    A[iter] = 0.9600836263003063
    (iter.I_1,iter.I_2) = (2,1)
    A[iter] = 0.4325878255452178
    (iter.I_1,iter.I_2) = (1,2)
    A[iter] = 0.6293256402775211
    (iter.I_1,iter.I_2) = (2,2)
    A[iter] = 0.9559027084099654
    (iter.I_1,iter.I_2) = (1,3)
    A[iter] = 0.6251548453735303
    (iter.I_1,iter.I_2) = (2,3)
    A[iter] = 0.9916142534546522

.. function:: countnz(A)

   Counts the number of nonzero values in array A (dense or sparse). Note that this is not a constant-time operation. For sparse matrices, one should usually use ``nnz``, which returns the number of stored values.

.. function:: conj!(A)

   Convert an array to its complex conjugate in-place

.. function:: stride(A, k)

   Returns the distance in memory (in number of elements) between adjacent elements in dimension k

.. function:: strides(A)

   Returns a tuple of the memory strides in each dimension

.. function:: ind2sub(dims, index) -> subscripts

   Returns a tuple of subscripts into an array with dimensions ``dims``, corresponding to the linear index ``index``

   **Example** ``i, j, ... = ind2sub(size(A), indmax(A))`` provides the indices of the maximum element

.. function:: ind2sub(a, index) -> subscripts

   Returns a tuple of subscripts into array ``a`` corresponding to the linear index ``index``

.. function:: sub2ind(dims, i, j, k...) -> index

   The inverse of ``ind2sub``, returns the linear index corresponding to the provided subscripts

Constructors
~~~~~~~~~~~~

.. function:: Array(type, dims)

   Construct an uninitialized dense array. ``dims`` may be a tuple or a series of integer arguments.

.. function:: getindex(type[, elements...])

   Construct a 1-d array of the specified type. This is usually called with the syntax ``Type[]``. Element values can be specified using ``Type[a,b,c,...]``.

.. function:: cell(dims)

   Construct an uninitialized cell array (heterogeneous array). ``dims`` can be either a tuple or a series of integer arguments.
.. function:: zeros(type, dims)

   Create an array of all zeros of specified type. The type defaults to Float64 if not specified.

.. function:: zeros(A)

   Create an array of all zeros with the same element type and shape as A.

.. function:: ones(type, dims)

   Create an array of all ones of specified type. The type defaults to Float64 if not specified.

.. function:: ones(A)

   Create an array of all ones with the same element type and shape as A.

.. function:: trues(dims)

   Create a ``BitArray`` with all values set to true

.. function:: falses(dims)

   Create a ``BitArray`` with all values set to false

.. function:: fill(x, dims)

   Create an array filled with the value ``x``.
   For example, ``fill(1.0, (10,10))`` returns a  10x10 array of floats, with each
   element initialized to 1.0.

   If ``x`` is an object reference, all elements will refer to the same object.
   ``fill(Foo(), dims)`` will return an array filled with the result of evaluating ``Foo()`` once.

.. function:: fill!(A, x)

   Fill array ``A`` with the value ``x``. If ``x`` is an object reference, all elements will refer to the same object.
   ``fill!(A, Foo())`` will return ``A`` filled with the result of evaluating ``Foo()`` once.

.. function:: reshape(A, dims)

   Create an array with the same data as the given array, but with different dimensions. An implementation for a particular type of array may choose whether the data is copied or shared.

.. function:: similar(array, element_type, dims)

   Create an uninitialized array of the same type as the given array, but with the specified element type and dimensions. The second and third arguments are both optional. The ``dims`` argument may be a tuple or a series of integer arguments.

.. function:: reinterpret(type, A)

   Change the type-interpretation of a block of memory. For example, ``reinterpret(Float32, uint32(7))`` interprets the 4 bytes corresponding to ``uint32(7)`` as a ``Float32``. For arrays, this constructs an array with the same binary data as the given array, but with the specified element type.

.. function:: eye(n)

   n-by-n identity matrix

.. function:: eye(m, n)

   m-by-n identity matrix

.. function:: eye(A)

   Constructs an identity matrix of the same dimensions and type as ``A``.

.. function:: linspace(start, stop, n)

   Construct a vector of ``n`` linearly-spaced elements from ``start`` to ``stop``.
   See also: :func:`linrange` that constructs a range object.

.. function:: logspace(start, stop, n)

   Construct a vector of ``n`` logarithmically-spaced numbers from ``10^start`` to ``10^stop``.

Mathematical operators and functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All mathematical operations and functions are supported for arrays

.. function:: broadcast(f, As...)

   Broadcasts the arrays ``As`` to a common size by expanding singleton dimensions, and returns an array of the results ``f(as...)`` for each position.

.. function:: broadcast!(f, dest, As...)

   Like ``broadcast``, but store the result of ``broadcast(f, As...)`` in the ``dest`` array.
   Note that ``dest`` is only used to store the result, and does not supply arguments to
   ``f`` unless it is also listed in the ``As``, as in ``broadcast!(f, A, A, B)`` to perform
   ``A[:] = broadcast(f, A, B)``.

.. function:: bitbroadcast(f, As...)

   Like ``broadcast``, but allocates a ``BitArray`` to store the result, rather then an ``Array``.

.. function:: broadcast_function(f)

   Returns a function ``broadcast_f`` such that ``broadcast_function(f)(As...) === broadcast(f, As...)``. Most useful in the form ``const broadcast_f = broadcast_function(f)``.

.. function:: broadcast!_function(f)

   Like ``broadcast_function``, but for ``broadcast!``.

Indexing, Assignment, and Concatenation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. function:: getindex(A, inds...)

   Returns a subset of array ``A`` as specified by ``inds``, where each ``ind`` may be an ``Int``, a ``Range``, or a ``Vector``.

.. function:: sub(A, inds...)

   Returns a SubArray, which stores the input ``A`` and ``inds`` rather than computing the result immediately. Calling ``getindex`` on a SubArray computes the indices on the fly.

.. function:: parent(A)

   Returns the "parent array" of an array view type (e.g., SubArray), or the array itself if it is not a view

.. function:: parentindexes(A)

   From an array view ``A``, returns the corresponding indexes in the parent

.. function:: slicedim(A, d, i)

   Return all the data of ``A`` where the index for dimension ``d`` equals ``i``. Equivalent to ``A[:,:,...,i,:,:,...]`` where ``i`` is in position ``d``.

.. function:: slice(A, inds...)

   Create a view of the given indexes of array ``A``, dropping dimensions indexed with
   scalars.

.. function:: setindex!(A, X, inds...)

   Store values from array ``X`` within some subset of ``A`` as specified by ``inds``.

.. function:: broadcast_getindex(A, inds...)

   Broadcasts the ``inds`` arrays to a common size like ``broadcast``, and returns an array of the results ``A[ks...]``, where ``ks`` goes over the positions in the broadcast.

.. function:: broadcast_setindex!(A, X, inds...)

   Broadcasts the ``X`` and ``inds`` arrays to a common size and stores the value from each position in ``X`` at the indices given by the same positions in ``inds``.

.. function:: cat(dims, A...)

   Concatenate the input arrays along the specified dimensions in the iterable ``dims``. For dimensions not in ``dims``, all input arrays should have the same size, which will also be the size of the output array along that dimension. For dimensions in ``dims``, the size of the output array is the sum of the sizes of the input arrays along that dimension. If ``dims`` is a single number, the different arrays are tightly stacked along that dimension. If ``dims`` is an iterable containing several dimensions, this allows to construct block diagonal matrices and their higher-dimensional analogues by simultaneously increasing several dimensions for every new input array and putting zero blocks elsewhere. For example, `cat([1,2], matrices...)` builds a block diagonal matrix, i.e. a block matrix with `matrices[1]`, `matrices[2]`, ... as diagonal blocks and matching zero blocks away from the diagonal.

.. function:: vcat(A...)

   Concatenate along dimension 1

.. function:: hcat(A...)

   Concatenate along dimension 2

.. function:: hvcat(rows::(Int...), values...)

   Horizontal and vertical concatenation in one call. This function is called for
   block matrix syntax. The first argument specifies the number of arguments to
   concatenate in each block row.
   For example, ``[a b;c d e]`` calls ``hvcat((2,3),a,b,c,d,e)``.

   If the first argument is a single integer ``n``, then all block rows are assumed to have ``n`` block columns.

.. function:: flipdim(A, d)

   Reverse ``A`` in dimension ``d``.

.. function:: flipud(A)

   Equivalent to ``flipdim(A,1)``.

.. function:: fliplr(A)

   Equivalent to ``flipdim(A,2)``.

.. function:: circshift(A,shifts)

   Circularly shift the data in an array. The second argument is a vector giving the amount to shift in each dimension.

.. function:: find(A)

   Return a vector of the linear indexes of the non-zeros in ``A``
   (determined by ``A[i]!=0``).  A common use of this is to convert a
   boolean array to an array of indexes of the ``true``
   elements.

.. function:: find(f,A)

   Return a vector of the linear indexes of  ``A`` where ``f`` returns true.

.. function:: findn(A)

   Return a vector of indexes for each dimension giving the locations of the non-zeros in ``A`` (determined by ``A[i]!=0``).

.. function:: findnz(A)

   Return a tuple ``(I, J, V)`` where ``I`` and ``J`` are the row and
   column indexes of the non-zero values in matrix ``A``, and ``V`` is
   a vector of the non-zero values.

.. function:: findfirst(A)

   Return the index of the first non-zero value in ``A`` (determined by ``A[i]!=0``).

.. function:: findfirst(A,v)

   Return the index of the first element equal to ``v`` in ``A``.

.. function:: findfirst(predicate, A)

   Return the index of the first element of ``A`` for which ``predicate`` returns true.

.. function:: findnext(A, i)

   Find the next index >= ``i`` of a non-zero element of ``A``, or ``0`` if not found.

.. function:: findnext(predicate, A, i)

   Find the next index >= ``i`` of an element of ``A`` for which ``predicate`` returns true, or ``0`` if not found.

.. function:: findnext(A, v, i)

   Find the next index >= ``i`` of an element of ``A`` equal to ``v`` (using ``==``),
   or ``0`` if not found.

.. function:: permutedims(A, perm)

   Permute the dimensions of array ``A``. ``perm`` is a vector specifying a permutation of length ``ndims(A)``. This is a generalization of transpose for multi-dimensional arrays. Transpose is equivalent to ``permutedims(A, [2,1])``.

.. function:: ipermutedims(A, perm)

   Like :func:`permutedims`, except the inverse of the given permutation is applied.

.. function:: permutedims!(dest, src, perm)

   Permute the dimensions of array ``src`` and store the result in the array ``dest``. ``perm`` is a vector specifying a permutation of length ``ndims(src)``. The preallocated array ``dest`` should have ``size(dest) == size(src)[perm]`` and is completely overwritten. No in-place permutation is supported and unexpected results will happen if `src` and `dest` have overlapping memory regions.

.. function:: squeeze(A, dims)

   Remove the dimensions specified by ``dims`` from array ``A``. Elements of
   ``dims`` must be unique and within the range ``1:ndims(A)``.

.. function:: vec(Array) -> Vector

   Vectorize an array using column-major convention.

.. function:: promote_shape(s1, s2)

   Check two array shapes for compatibility, allowing trailing singleton dimensions,
   and return whichever shape has more dimensions.

.. function:: checkbounds(array, indexes...)

   Throw an error if the specified indexes are not in bounds for the given array.

.. function:: randsubseq(A, p) -> Vector

   Return a vector consisting of a random subsequence of the given array ``A``,
   where each element of ``A`` is included (in order) with independent
   probability ``p``.   (Complexity is linear in ``p*length(A)``, so this
   function is efficient even if ``p`` is small and ``A`` is large.)  Technically,
   this process is known as "Bernoulli sampling" of ``A``.

.. function:: randsubseq!(S, A, p)

   Like ``randsubseq``, but the results are stored in ``S`` (which is
   resized as needed).


Array functions
~~~~~~~~~~~~~~~

.. function:: cumprod(A, [dim])

   Cumulative product along a dimension.
   The dimension defaults to 1.

.. function:: cumprod!(B, A, [dim])

   Cumulative product of ``A`` along a dimension, storing the result in ``B``.
   The dimension defaults to 1.

.. function:: cumsum(A, [dim])

   Cumulative sum along a dimension.
   The dimension defaults to 1.

.. function:: cumsum!(B, A, [dim])

   Cumulative sum of ``A`` along a dimension, storing the result in ``B``.
   The dimension defaults to 1.

.. function:: cumsum_kbn(A, [dim])

   Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier
   compensated summation algorithm for additional accuracy.
   The dimension defaults to 1.

.. function:: cummin(A, [dim])

   Cumulative minimum along a dimension.
   The dimension defaults to 1.

.. function:: cummax(A, [dim])

   Cumulative maximum along a dimension.
   The dimension defaults to 1.

.. function:: diff(A, [dim])

   Finite difference operator of matrix or vector.

.. function:: gradient(F, [h])

   Compute differences along vector ``F``, using ``h`` as the spacing between points.
   The default spacing is one.

.. function:: rot180(A)

   Rotate matrix ``A`` 180 degrees.

.. function:: rotl90(A)

   Rotate matrix ``A`` left 90 degrees.

.. function:: rotr90(A)

   Rotate matrix ``A`` right 90 degrees.

.. function:: reducedim(f, A, dims, initial)

   Reduce 2-argument function ``f`` along dimensions of ``A``. ``dims`` is a
   vector specifying the dimensions to reduce, and ``initial`` is the initial
   value to use in the reductions.

   The associativity of the reduction is implementation-dependent; if you
   need a particular associativity, e.g. left-to-right, you should write
   your own loop. See documentation for ``reduce``.

.. function:: mapslices(f, A, dims)

   Transform the given dimensions of array ``A`` using function ``f``. ``f``
   is called on each slice of ``A`` of the form ``A[...,:,...,:,...]``.
   ``dims`` is an integer vector specifying where the colons go in this
   expression. The results are concatenated along the remaining dimensions.
   For example, if ``dims`` is ``[1,2]`` and A is 4-dimensional, ``f`` is
   called on ``A[:,:,i,j]`` for all ``i`` and ``j``.

.. function:: sum_kbn(A)

   Returns the sum of all array elements, using the Kahan-Babuska-Neumaier compensated summation algorithm for additional accuracy.

.. function:: cartesianmap(f, dims)

   Given a ``dims`` tuple of integers ``(m, n, ...)``, call ``f`` on all combinations of
   integers in the ranges ``1:m``, ``1:n``, etc.

   .. doctest::

      julia> cartesianmap(println, (2,2))
      11
      21
      12
      22

BitArrays
~~~~~~~~~

.. function:: bitpack(A::AbstractArray{T,N}) -> BitArray

   Converts a numeric array to a packed boolean array

.. function:: bitunpack(B::BitArray{N}) -> Array{Bool,N}

   Converts a packed boolean array to an array of booleans

.. function:: flipbits!(B::BitArray{N}) -> BitArray{N}

   Performs a bitwise not operation on B. See :ref:`~ operator <~>`.

.. function:: rol(B::BitArray{1},i::Integer) -> BitArray{1}

   Left rotation operator.

.. function:: ror(B::BitArray{1},i::Integer) -> BitArray{1}

   Right rotation operator.


Combinatorics
-------------

.. function:: nthperm(v, k)

   Compute the kth lexicographic permutation of a vector.

.. function:: nthperm(p)

   Return the ``k`` that generated permutation ``p``.
   Note that ``nthperm(nthperm([1:n], k)) == k`` for ``1 <= k <= factorial(n)``.

.. function:: nthperm!(v, k)

   In-place version of :func:`nthperm`.

.. function:: randperm(n)

   Construct a random permutation of the given length.

.. function:: invperm(v)

   Return the inverse permutation of v.

.. function:: isperm(v) -> Bool

   Returns true if v is a valid permutation.

.. function:: permute!(v, p)

   Permute vector ``v`` in-place, according to permutation ``p``.  No
   checking is done to verify that ``p`` is a permutation.

   To return a new permutation, use ``v[p]``.  Note that this is
   generally faster than ``permute!(v,p)`` for large vectors.

.. function:: ipermute!(v, p)

   Like permute!, but the inverse of the given permutation is applied.

.. function:: randcycle(n)

   Construct a random cyclic permutation of the given length.

.. function:: shuffle(v)

   Return a randomly permuted copy of ``v``.

.. function:: shuffle!(v)

   In-place version of :func:`shuffle`.

.. function:: reverse(v [, start=1 [, stop=length(v) ]] )

   Return a copy of ``v`` reversed from start to stop.

.. function:: reverseind(v, i)

   Given an index ``i`` in ``reverse(v)``, return the corresponding
   index in ``v`` so that ``v[reverseind(v,i)] == reverse(v)[i]``.
   (This can be nontrivial in the case where ``v`` is a Unicode string.)

.. function:: reverse!(v [, start=1 [, stop=length(v) ]]) -> v

   In-place version of :func:`reverse`.

.. function:: combinations(array, n)

   Generate all combinations of ``n`` elements from an indexable
   object.  Because the number of combinations can be very large, this
   function returns an iterator object. Use
   ``collect(combinations(array,n))`` to get an array of all combinations.

.. function:: permutations(array)

   Generate all permutations of an indexable object.  Because the
   number of permutations can be very large, this function returns an
   iterator object. Use ``collect(permutations(array))`` to get an array
   of all permutations.

.. function:: partitions(n)

   Generate all integer arrays that sum to ``n``. Because the number of
   partitions can be very large, this function returns an iterator
   object. Use ``collect(partitions(n))`` to get an array of all
   partitions. The number of partitions to generate can be efficiently
   computed using ``length(partitions(n))``.

.. function:: partitions(n, m)

   Generate all arrays of ``m`` integers that sum to ``n``. Because
   the number of partitions can be very large, this function returns an
   iterator object. Use ``collect(partitions(n,m))`` to get an array of
   all partitions. The number of partitions to generate can be efficiently
   computed using ``length(partitions(n,m))``.

.. function:: partitions(array)

   Generate all set partitions of the elements of an array,
   represented as arrays of arrays. Because the number of partitions
   can be very large, this function returns an iterator object. Use
   ``collect(partitions(array))`` to get an array of all partitions.
   The number of partitions to generate can be efficiently
   computed using ``length(partitions(array))``.

.. function:: partitions(array, m)

   Generate all set partitions of the elements of an array into exactly m
   subsets, represented as arrays of arrays. Because the number of
   partitions can be very large, this function returns an iterator object.
   Use ``collect(partitions(array,m))`` to get an array of all partitions.
   The number of partitions into m subsets is equal to the Stirling number
   of the second kind and can be efficiently computed using
   ``length(partitions(array,m))``.

Statistics
----------

.. function:: mean(v[, region])

   Compute the mean of whole array ``v``, or optionally along the dimensions in ``region``.
   Note: Julia does not ignore ``NaN`` values in the computation.
   For applications requiring the handling of missing data, the ``DataArray``
   package is recommended.

.. function:: mean!(r, v)

   Compute the mean of ``v`` over the singleton dimensions of ``r``, and write results to ``r``.

.. function:: std(v[, region])

   Compute the sample standard deviation of a vector or array ``v``, optionally along dimensions in ``region``. The algorithm returns an estimator of the generative distribution's standard deviation under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sqrt(sum((v - mean(v)).^2) / (length(v) - 1))``.
   Note: Julia does not ignore ``NaN`` values in the computation.
   For applications requiring the handling of missing data, the ``DataArray``
   package is recommended.

.. function:: stdm(v, m)

   Compute the sample standard deviation of a vector ``v`` with known mean ``m``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: var(v[, region])

   Compute the sample variance of a vector or array ``v``, optionally along dimensions in ``region``. The algorithm will return an estimator of the generative distribution's variance under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sum((v - mean(v)).^2) / (length(v) - 1)``.
   Note: Julia does not ignore ``NaN`` values in the computation.
   For applications requiring the handling of missing data, the ``DataArray``
   package is recommended.

.. function:: varm(v, m)

   Compute the sample variance of a vector ``v`` with known mean ``m``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: middle(x)

   Compute the middle of a scalar value, which is equivalent to ``x`` itself,
   but of the type of ``middle(x, x)`` for consistency.

.. function:: middle(x, y)

   Compute the middle of two reals ``x`` and ``y``, which is equivalent
   in both value and type to computing their mean (``(x + y) / 2``).

.. function:: middle(range)

   Compute the middle of a range, which consists in computing the mean of its extrema.
   Since a range is sorted, the mean is performed with the first and last element.

.. function:: middle(array)

   Compute the middle of an array, which consists in finding its extrema and
   then computing their mean.

.. function:: median(v)

   Compute the median of a vector ``v``. ``NaN`` is returned if the data
   contains any ``NaN`` values. For applications requiring the handling of
   missing data, the ``DataArrays`` package is recommended.

.. function:: median!(v)

   Like ``median``, but may overwrite the input vector.

.. function:: hist(v[, n]) -> e, counts

   Compute the histogram of ``v``, optionally using approximately ``n``
   bins. The return values are a range ``e``, which correspond to the
   edges of the bins, and ``counts`` containing the number of elements of
   ``v`` in each bin.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist(v, e) -> e, counts

   Compute the histogram of ``v`` using a vector/range ``e`` as the edges for
   the bins. The result will be a vector of length ``length(e) - 1``, such that the
   element at location ``i`` satisfies ``sum(e[i] .< v .<= e[i+1])``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist!(counts, v, e) -> e, counts

   Compute the histogram of ``v``, using a vector/range ``e`` as the edges for the bins.
   This function writes the resultant counts to a pre-allocated array ``counts``.

.. function:: hist2d(M, e1, e2) -> (edge1, edge2, counts)

   Compute a "2d histogram" of a set of N points specified by N-by-2 matrix ``M``.
   Arguments ``e1`` and ``e2`` are bins for each dimension, specified either as
   integer bin counts or vectors of bin edges. The result is a tuple of
   ``edge1`` (the bin edges used in the first dimension), ``edge2`` (the bin edges
   used in the second dimension), and ``counts``, a histogram matrix of size
   ``(length(edge1)-1, length(edge2)-1)``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist2d!(counts, M, e1, e2) -> (e1, e2, counts)

   Compute a "2d histogram" with respect to the bins delimited by the edges given
   in ``e1`` and ``e2``. This function writes the results to a pre-allocated
   array ``counts``.

.. function:: histrange(v, n)

   Compute *nice* bin ranges for the edges of a histogram of ``v``, using
   approximately ``n`` bins. The resulting step sizes will be 1, 2 or 5
   multiplied by a power of 10.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: midpoints(e)

   Compute the midpoints of the bins with edges ``e``. The result is a
   vector/range of length ``length(e) - 1``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile(v, p)

   Compute the quantiles of a vector ``v`` at a specified set of probability values ``p``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile(v, p)

   Compute the quantile of a vector ``v`` at the probability ``p``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile!(v, p)

   Like ``quantile``, but overwrites the input vector.

.. function:: cov(v1[, v2][, vardim=1, corrected=true, mean=nothing])

   Compute the Pearson covariance between the vector(s) in ``v1`` and ``v2``.
   Here, ``v1`` and ``v2`` can be either vectors or matrices.

   This function accepts three keyword arguments:

   - ``vardim``: the dimension of variables. When ``vardim = 1``, variables
     are considered in columns while observations in rows; when ``vardim = 2``,
     variables are in rows while observations in columns. By default, it is
     set to ``1``.

   - ``corrected``: whether to apply Bessel's correction (divide by ``n-1``
     instead of ``n``). By default, it is set to ``true``.

   - ``mean``: allow users to supply mean values that are known. By default,
     it is set to ``nothing``, which indicates that the mean(s) are unknown,
     and the function will compute the mean. Users can use ``mean=0`` to
     indicate that the input data are centered, and hence there's no need to
     subtract the mean.

   The size of the result depends on the size of ``v1`` and ``v2``. When both
   ``v1`` and ``v2`` are vectors, it returns the covariance between them as a
   scalar. When either one is a matrix, it returns a covariance matrix of size
   ``(n1, n2)``, where ``n1`` and ``n2`` are the numbers of slices in ``v1`` and
   ``v2``, which depend on the setting of ``vardim``.

   Note: ``v2`` can be omitted, which indicates ``v2 = v1``.


.. function:: cor(v1[, v2][, vardim=1, mean=nothing])

   Compute the Pearson correlation between the vector(s) in ``v1`` and ``v2``.

   Users can use the keyword argument ``vardim`` to specify the variable
   dimension, and ``mean`` to supply pre-computed mean values.


Signal Processing
-----------------

Fast Fourier transform (FFT) functions in Julia are largely
implemented by calling functions from `FFTW
<http://www.fftw.org>`_. By default, Julia does not use multi-threaded
FFTW. Higher performance may be obtained by experimenting with
multi-threading. Use `FFTW.set_num_threads(np)` to use `np` threads.

.. function:: fft(A [, dims])

   Performs a multidimensional FFT of the array ``A``.  The optional ``dims``
   argument specifies an iterable subset of dimensions (e.g. an integer,
   range, tuple, or array) to transform along.  Most efficient if the
   size of ``A`` along the transformed dimensions is a product of small
   primes; see :func:`nextprod`.  See also :func:`plan_fft` for even
   greater efficiency.

   A one-dimensional FFT computes the one-dimensional discrete Fourier
   transform (DFT) as defined by

   .. math::

      \operatorname{DFT}(A)[k] = \sum_{n=1}^{\operatorname{length}(A)}
      \exp\left(-i\frac{2\pi (n-1)(k-1)}{\operatorname{length}(A)} \right)
      A[n].

   A multidimensional FFT simply performs this operation along each transformed
   dimension of ``A``.

   Higher performance is usually possible with multi-threading. Use
   `FFTW.set_num_threads(np)` to use `np` threads, if you have `np`
   processors.

.. function:: fft!(A [, dims])

   Same as :func:`fft`, but operates in-place on ``A``,
   which must be an array of complex floating-point numbers.

.. function:: ifft(A [, dims])

   Multidimensional inverse FFT.

   A one-dimensional inverse FFT computes

   .. math::

      \operatorname{IDFT}(A)[k] = \frac{1}{\operatorname{length}(A)}
      \sum_{n=1}^{\operatorname{length}(A)} \exp\left(+i\frac{2\pi (n-1)(k-1)}
      {\operatorname{length}(A)} \right) A[n].

   A multidimensional inverse FFT simply performs this operation along each
   transformed dimension of ``A``.

.. function:: ifft!(A [, dims])

   Same as :func:`ifft`, but operates in-place on ``A``.

.. function:: bfft(A [, dims])

   Similar to :func:`ifft`, but computes an unnormalized inverse (backward)
   transform, which must be divided by the product of the sizes of the
   transformed dimensions in order to obtain the inverse. (This is slightly
   more efficient than :func:`ifft` because it omits a scaling step, which in
   some applications can be combined with other computational steps elsewhere.)

   .. math::

      \operatorname{BDFT}(A)[k] = \operatorname{length}(A) \operatorname{IDFT}(A)[k]

.. function:: bfft!(A [, dims])

   Same as :func:`bfft`, but operates in-place on ``A``.

.. function:: plan_fft(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized FFT along given dimensions (``dims``) of arrays
   matching the shape and type of ``A``.  (The first two arguments have
   the same meaning as for :func:`fft`.)  Returns a function ``plan(A)``
   that computes ``fft(A, dims)`` quickly.

   The ``flags`` argument is a bitwise-or of FFTW planner flags, defaulting
   to ``FFTW.ESTIMATE``.  e.g. passing ``FFTW.MEASURE`` or ``FFTW.PATIENT``
   will instead spend several seconds (or more) benchmarking different
   possible FFT algorithms and picking the fastest one; see the FFTW manual
   for more information on planner flags.  The optional ``timelimit`` argument
   specifies a rough upper bound on the allowed planning time, in seconds.
   Passing ``FFTW.MEASURE`` or ``FFTW.PATIENT`` may cause the input array ``A``
   to be overwritten with zeros during plan creation.

   :func:`plan_fft!` is the same as :func:`plan_fft` but creates a plan
   that operates in-place on its argument (which must be an array of
   complex floating-point numbers).  :func:`plan_ifft` and so on
   are similar but produce plans that perform the equivalent of
   the inverse transforms :func:`ifft` and so on.

.. function:: plan_ifft(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_fft`, but produces a plan that performs inverse transforms
   :func:`ifft`.

.. function:: plan_bfft(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_fft`, but produces a plan that performs an unnormalized
   backwards transform :func:`bfft`.

.. function:: plan_fft!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_fft`, but operates in-place on ``A``.

.. function:: plan_ifft!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_ifft`, but operates in-place on ``A``.

.. function:: plan_bfft!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_bfft`, but operates in-place on ``A``.

.. function:: rfft(A [, dims])

   Multidimensional FFT of a real array A, exploiting the fact that
   the transform has conjugate symmetry in order to save roughly half
   the computational time and storage costs compared with :func:`fft`.
   If ``A`` has size ``(n_1, ..., n_d)``, the result has size
   ``(floor(n_1/2)+1, ..., n_d)``.

   The optional ``dims`` argument specifies an iterable subset of one or
   more dimensions of ``A`` to transform, similar to :func:`fft`.  Instead
   of (roughly) halving the first dimension of ``A`` in the result, the
   ``dims[1]`` dimension is (roughly) halved in the same way.

.. function:: irfft(A, d [, dims])

   Inverse of :func:`rfft`: for a complex array ``A``, gives the
   corresponding real array whose FFT yields ``A`` in the first half.
   As for :func:`rfft`, ``dims`` is an optional subset of dimensions
   to transform, defaulting to ``1:ndims(A)``.

   ``d`` is the length of the transformed real array along the ``dims[1]``
   dimension, which must satisfy ``d == floor(size(A,dims[1])/2)+1``.
   (This parameter cannot be inferred from ``size(A)`` due to the
   possibility of rounding by the ``floor`` function here.)

.. function:: brfft(A, d [, dims])

   Similar to :func:`irfft` but computes an unnormalized inverse transform
   (similar to :func:`bfft`), which must be divided by the product
   of the sizes of the transformed dimensions (of the real output array)
   in order to obtain the inverse transform.

.. function:: plan_rfft(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized real-input FFT, similar to :func:`plan_fft`
   except for :func:`rfft` instead of :func:`fft`.  The first two
   arguments, and the size of the transformed result, are the same as
   for :func:`rfft`.

.. function:: plan_brfft(A, d [, dims [, flags [, timelimit]]])

   Pre-plan an optimized real-input unnormalized transform, similar to
   :func:`plan_rfft` except for :func:`brfft` instead of :func:`rfft`.
   The first two arguments and the size of the transformed result, are
   the same as for :func:`brfft`.

.. function:: plan_irfft(A, d [, dims [, flags [, timelimit]]])

   Pre-plan an optimized inverse real-input FFT, similar to :func:`plan_rfft`
   except for :func:`irfft` and :func:`brfft`, respectively.  The first
   three arguments have the same meaning as for :func:`irfft`.

.. function:: dct(A [, dims])

   Performs a multidimensional type-II discrete cosine transform (DCT)
   of the array ``A``, using the unitary normalization of the DCT.
   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of ``A`` along the transformed
   dimensions is a product of small primes; see :func:`nextprod`.  See
   also :func:`plan_dct` for even greater efficiency.

.. function:: dct!(A [, dims])

   Same as :func:`dct!`, except that it operates in-place
   on ``A``, which must be an array of real or complex floating-point
   values.

.. function:: idct(A [, dims])

   Computes the multidimensional inverse discrete cosine transform (DCT)
   of the array ``A`` (technically, a type-III DCT with the unitary
   normalization).
   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of ``A`` along the transformed
   dimensions is a product of small primes; see :func:`nextprod`.  See
   also :func:`plan_idct` for even greater efficiency.

.. function:: idct!(A [, dims])

   Same as :func:`idct!`, but operates in-place on ``A``.

.. function:: plan_dct(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized discrete cosine transform (DCT), similar to
   :func:`plan_fft` except producing a function that computes :func:`dct`.
   The first two arguments have the same meaning as for :func:`dct`.

.. function:: plan_dct!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_dct`, but operates in-place on ``A``.

.. function:: plan_idct(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized inverse discrete cosine transform (DCT), similar to
   :func:`plan_fft` except producing a function that computes :func:`idct`.
   The first two arguments have the same meaning as for :func:`idct`.

.. function:: plan_idct!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_idct`, but operates in-place on ``A``.

.. function:: fftshift(x)

   Swap the first and second halves of each dimension of ``x``.

.. function:: fftshift(x,dim)

   Swap the first and second halves of the given dimension of array ``x``.

.. function:: ifftshift(x, [dim])

   Undoes the effect of ``fftshift``.

.. function:: filt(b, a, x, [si])

   Apply filter described by vectors ``a`` and ``b`` to vector ``x``, with an
   optional initial filter state vector ``si`` (defaults to zeros).

.. function:: filt!(out, b, a, x, [si])

   Same as :func:`filt` but writes the result into the ``out`` argument,
   which may alias the input ``x`` to modify it in-place.

.. function:: deconv(b,a)

   Construct vector ``c`` such that ``b = conv(a,c) + r``. Equivalent to polynomial division.

.. function:: conv(u,v)

   Convolution of two vectors. Uses FFT algorithm.

.. function:: conv2(u,v,A)

   2-D convolution of the matrix ``A`` with the 2-D separable kernel generated by
   the vectors ``u`` and ``v``.  Uses 2-D FFT algorithm

.. function:: conv2(B,A)

   2-D convolution of the matrix ``B`` with the matrix ``A``.  Uses 2-D FFT algorithm

.. function:: xcorr(u,v)

   Compute the cross-correlation of two vectors.

The following functions are defined within the ``Base.FFTW`` module.

.. currentmodule:: Base.FFTW

.. function:: r2r(A, kind [, dims])

   Performs a multidimensional real-input/real-output (r2r) transform
   of type ``kind`` of the array ``A``, as defined in the FFTW manual.
   ``kind`` specifies either a discrete cosine transform of various types
   (``FFTW.REDFT00``, ``FFTW.REDFT01``, ``FFTW.REDFT10``, or
   ``FFTW.REDFT11``), a discrete sine transform of various types
   (``FFTW.RODFT00``, ``FFTW.RODFT01``, ``FFTW.RODFT10``, or
   ``FFTW.RODFT11``), a real-input DFT with halfcomplex-format output
   (``FFTW.R2HC`` and its inverse ``FFTW.HC2R``), or a discrete
   Hartley transform (``FFTW.DHT``).  The ``kind`` argument may be
   an array or tuple in order to specify different transform types
   along the different dimensions of ``A``; ``kind[end]`` is used
   for any unspecified dimensions.  See the FFTW manual for precise
   definitions of these transform types, at http://www.fftw.org/doc.

   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along. ``kind[i]`` is then the transform type for ``dims[i]``,
   with ``kind[end]`` being used for ``i > length(kind)``.

   See also :func:`plan_r2r` to pre-plan optimized r2r transforms.

.. function:: r2r!(A, kind [, dims])

   Same as :func:`r2r`, but operates in-place on ``A``, which must be
   an array of real or complex floating-point numbers.

.. function:: plan_r2r(A, kind [, dims [, flags [, timelimit]]])

   Pre-plan an optimized r2r transform, similar to :func:`Base.plan_fft`
   except that the transforms (and the first three arguments)
   correspond to :func:`r2r` and :func:`r2r!`, respectively.

.. function:: plan_r2r!(A, kind [, dims [, flags [, timelimit]]])

   Similar to :func:`Base.plan_fft`, but corresponds to :func:`r2r!`.

.. currentmodule:: Base

Numerical Integration
---------------------

Although several external packages are available for numeric integration
and solution of ordinary differential equations, we also provide
some built-in integration support in Julia.

.. function:: quadgk(f, a,b,c...; reltol=sqrt(eps), abstol=0, maxevals=10^7, order=7, norm=vecnorm)

   Numerically integrate the function ``f(x)`` from ``a`` to ``b``,
   and optionally over additional intervals ``b`` to ``c`` and so on.
   Keyword options include a relative error tolerance ``reltol`` (defaults
   to ``sqrt(eps)`` in the precision of the endpoints), an absolute error
   tolerance ``abstol`` (defaults to 0), a maximum number of function
   evaluations ``maxevals`` (defaults to ``10^7``), and the ``order``
   of the integration rule (defaults to 7).

   Returns a pair ``(I,E)`` of the estimated integral ``I`` and an
   estimated upper bound on the absolute error ``E``.  If ``maxevals``
   is not exceeded then ``E <= max(abstol, reltol*norm(I))`` will hold.
   (Note that it is useful to specify a positive ``abstol`` in cases where
   ``norm(I)`` may be zero.)

   The endpoints ``a`` etcetera can also be complex (in which case the
   integral is performed over straight-line segments in the complex
   plane).  If the endpoints are ``BigFloat``, then the integration
   will be performed in ``BigFloat`` precision as well (note: it is
   advisable to increase the integration ``order`` in rough proportion
   to the precision, for smooth integrands).  More generally, the
   precision is set by the precision of the integration endpoints
   (promoted to floating-point types).

   The integrand ``f(x)`` can return any numeric scalar, vector, or matrix
   type, or in fact any type supporting ``+``, ``-``, multiplication
   by real values, and a ``norm`` (i.e., any normed vector space).
   Alternatively, a different norm can be specified by passing a `norm`-like
   function as the `norm` keyword argument (which defaults to `vecnorm`).

   The algorithm is an adaptive Gauss-Kronrod integration technique:
   the integral in each interval is estimated using a Kronrod rule
   (``2*order+1`` points) and the error is estimated using an embedded
   Gauss rule (``order`` points).   The interval with the largest
   error is then subdivided into two intervals and the process is repeated
   until the desired error tolerance is achieved.

   These quadrature rules work best for smooth functions within each
   interval, so if your function has a known discontinuity or other
   singularity, it is best to subdivide your interval to put the
   singularity at an endpoint.  For example, if ``f`` has a discontinuity
   at ``x=0.7`` and you want to integrate from 0 to 1, you should use
   ``quadgk(f, 0,0.7,1)`` to subdivide the interval at the point of
   discontinuity.  The integrand is never evaluated exactly at the endpoints
   of the intervals, so it is possible to integrate functions that diverge
   at the endpoints as long as the singularity is integrable (for example,
   a ``log(x)`` or ``1/sqrt(x)`` singularity).

   For real-valued endpoints, the starting and/or ending points may be
   infinite.  (A coordinate transformation is performed internally to
   map the infinite interval to a finite one.)

Parallel Computing
------------------

.. function:: addprocs(n::Integer; exeflags=``) -> List of process identifiers

   Launches workers using the in-built ``LocalManager`` which only launches workers on the local host.
   This can be used to take advantage of multiple cores. `addprocs(4)`` will add 4 processes on the local machine.

.. function:: addprocs() -> List of process identifiers

    Equivalent to ``addprocs(CPU_CORES)``

.. function:: addprocs(machines; tunnel=false, sshflags=``, max_parallel=10, exeflags=``) -> List of process identifiers

   Add processes on remote machines via SSH.
   Requires julia to be installed in the same location on each node, or to be available via a shared file system.

   ``machines`` is a vector of machine specifications.  Worker are started for each specification.

   A machine specification is either a string ``machine_spec`` or a tuple - ``(machine_spec, count)``

   ``machine_spec`` is a string of the form ``[user@]host[:port] [bind_addr[:port]]``. ``user`` defaults
   to current user, ``port`` to the standard ssh port. If ``[bind_addr[:port]]`` is specified, other
   workers will connect to this worker at the specified ``bind_addr`` and ``port``.

   ``count`` is the number of workers to be launched on the specified host. If specified as ``"auto"``
   or ``:auto`` it will launch as many workers as the number of cores on the specific host.


   Keyword arguments:

   ``tunnel`` : if ``true`` then SSH tunneling will be used to connect to the worker from the master process.

   ``sshflags`` : specifies additional ssh options, e.g. :literal:`sshflags=\`-i /home/foo/bar.pem\`` .

   ``max_parallel`` : specifies the maximum number of workers connected to in parallel at a host. Defaults to 10.

   ``dir`` :  specifies the location of the julia binaries on the worker nodes. Defaults to JULIA_HOME.

   ``exename`` :  name of the julia executable. Defaults to "./julia" or "./julia-debug" as the case may be.

   ``exeflags`` :  additional flags passed to the worker processes.


.. function:: addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager.

   For example Beowulf clusters are  supported via a custom cluster manager implemented
   in  package ``ClusterManagers``.

   See the documentation for package ``ClusterManagers`` for more information on how to
   write a custom cluster manager.


.. function:: nprocs()

   Get the number of available processes.

.. function:: nworkers()

   Get the number of available worker processes. This is one less than nprocs(). Equal to nprocs() if nprocs() == 1.

.. function:: procs()

   Returns a list of all process identifiers.

.. function:: workers()

   Returns a list of all worker process identifiers.

.. function:: rmprocs(pids...)

   Removes the specified workers.

.. function:: interrupt([pids...])

   Interrupt the current executing task on the specified workers. This is
   equivalent to pressing Ctrl-C on the local machine. If no arguments are given,
   all workers are interrupted.

.. function:: myid()

   Get the id of the current process.

.. function:: pmap(f, lsts...; err_retry=true, err_stop=false)

   Transform collections ``lsts`` by applying ``f`` to each element in parallel.
   If ``nprocs() > 1``, the calling process will be dedicated to assigning tasks.
   All other available processes will be used as parallel workers.

   If ``err_retry`` is true, it retries a failed application of ``f`` on a different worker.
   If ``err_stop`` is true, it takes precedence over the value of ``err_retry`` and ``pmap`` stops execution on the first error.


.. function:: remotecall(id, func, args...)

   Call a function asynchronously on the given arguments on the specified process. Returns a ``RemoteRef``.

.. function:: wait([x])

   Block the current task until some event occurs, depending on the type
   of the argument:

   * ``RemoteRef``: Wait for a value to become available for the specified remote reference.

   * ``Condition``: Wait for ``notify`` on a condition.

   * ``Process``: Wait for a process or process chain to exit. The ``exitcode`` field of a process can be used to determine success or failure.

   * ``Task``: Wait for a ``Task`` to finish, returning its result value.

   * ``RawFD``: Wait for changes on a file descriptor (see `poll_fd` for keyword arguments and return code)

   If no argument is passed, the task blocks for an undefined period. If the task's
   state is set to ``:waiting``, it can only be restarted by an explicit call to
   ``schedule`` or ``yieldto``. If the task's state is ``:runnable``, it might be
   restarted unpredictably.

   Often ``wait`` is called within a ``while`` loop to ensure a waited-for condition
   is met before proceeding.

.. function:: fetch(RemoteRef)

   Wait for and get the value of a remote reference.

.. function:: remotecall_wait(id, func, args...)

   Perform ``wait(remotecall(...))`` in one message.

.. function:: remotecall_fetch(id, func, args...)

   Perform ``fetch(remotecall(...))`` in one message.

.. function:: put!(RemoteRef, value)

   Store a value to a remote reference. Implements "shared queue of length 1" semantics: if a value is already present, blocks until the value is removed with ``take!``. Returns its first argument.

.. function:: take!(RemoteRef)

   Fetch the value of a remote reference, removing it so that the reference is empty again.

.. function:: isready(r::RemoteRef)

   Determine whether a ``RemoteRef`` has a value stored to it. Note that this function
   can cause race conditions, since by the time you receive its result it may
   no longer be true. It is recommended that this function only be used on a
   ``RemoteRef`` that is assigned once.

   If the argument ``RemoteRef`` is owned by a different node, this call will block to
   wait for the answer. It is recommended to wait for ``r`` in a separate task instead,
   or to use a local ``RemoteRef`` as a proxy::

       rr = RemoteRef()
       @async put!(rr, remotecall_fetch(p, long_computation))
       isready(rr)  # will not block

.. function:: RemoteRef()

   Make an uninitialized remote reference on the local machine.

.. function:: RemoteRef(n)

   Make an uninitialized remote reference on process ``n``.

.. function:: timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

   Waits till ``testcb`` returns ``true`` or for ``secs``` seconds, whichever is earlier.
   ``testcb`` is polled every ``pollint`` seconds.

.. function:: @spawn

   Execute an expression on an automatically-chosen process, returning a
   ``RemoteRef`` to the result.

.. function:: @spawnat

   Accepts two arguments, ``p`` and an expression, and runs the expression
   asynchronously on process ``p``, returning a ``RemoteRef`` to the result.

.. function:: @fetch

   Equivalent to ``fetch(@spawn expr)``.

.. function:: @fetchfrom

   Equivalent to ``fetch(@spawnat p expr)``.

.. function:: @async

   Schedule an expression to run on the local machine, also adding it to the
   set of items that the nearest enclosing ``@sync`` waits for.

.. function:: @sync

   Wait until all dynamically-enclosed uses of ``@async``, ``@spawn``,
   ``@spawnat`` and ``@parallel`` are complete.

.. function:: @parallel

   A parallel for loop of the form ::

        @parallel [reducer] for var = range
            body
        end

   The specified range is partitioned and locally executed across all workers.
   In case an optional reducer function is specified, @parallel performs local
   reductions on each worker with a final reduction on the calling process.

   Note that without a reducer function, @parallel executes asynchronously,
   i.e. it spawns independent tasks on all available workers and returns
   immediately without waiting for completion. To wait for completion, prefix
   the call with ``@sync``, like ::

        @sync @parallel for var = range
            body
        end



Distributed Arrays
------------------

.. function:: DArray(init, dims, [procs, dist])

   Construct a distributed array. The parameter ``init`` is a function that accepts a tuple of index ranges.
   This function should allocate a local chunk of the distributed array and initialize it for the specified indices.
   ``dims`` is the overall size of the distributed array. ``procs`` optionally specifies a vector of process IDs to use.
   If unspecified, the array is distributed over all worker processes only. Typically, when running in distributed mode,
   i.e., ``nprocs() > 1``, this would mean that no chunk of the distributed array exists on the process hosting the
   interactive julia prompt.
   ``dist`` is an integer vector specifying how many chunks the distributed array should be divided into in each dimension.

   For example, the ``dfill`` function that creates a distributed array and fills it with a value ``v`` is implemented as:

   ``dfill(v, args...) = DArray(I->fill(v, map(length,I)), args...)``

.. function:: dzeros(dims, ...)

   Construct a distributed array of zeros. Trailing arguments are the same as those accepted by :func:`DArray`.

.. function:: dones(dims, ...)

   Construct a distributed array of ones. Trailing arguments are the same as those accepted by :func:`DArray`.

.. function:: dfill(x, dims, ...)

   Construct a distributed array filled with value ``x``. Trailing arguments are the same as those accepted by :func:`DArray`.

.. function:: drand(dims, ...)

   Construct a distributed uniform random array. Trailing arguments are the same as those accepted by :func:`DArray`.

.. function:: drandn(dims, ...)

   Construct a distributed normal random array. Trailing arguments are the same as those accepted by :func:`DArray`.

.. function:: distribute(a)

   Convert a local array to distributed.

.. function:: localpart(d)

   Get the local piece of a distributed array. Returns an empty array if no local part exists on the calling process.

.. function:: localindexes(d)

   A tuple describing the indexes owned by the local process. Returns a tuple with empty ranges
   if no local part exists on the calling process.

.. function:: procs(d)

   Get the vector of processes storing pieces of ``d``.


Shared Arrays (Experimental, UNIX-only feature)
-----------------------------------------------

.. function:: SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])

    Construct a SharedArray of a bitstype ``T``  and size ``dims`` across the processes
    specified by ``pids`` - all of which have to be on the same host.

    If ``pids`` is left unspecified, the shared array will be mapped across all processes
    on the current host, including the master. But, ``localindexes`` and ``indexpids``
    will only refer to worker processes. This facilitates work distribution code to use
    workers for actual computation with the master process acting as a driver.

    If an ``init`` function of the type ``initfn(S::SharedArray)`` is specified,
    it is called on all the participating workers.

.. function:: procs(S::SharedArray)

   Get the vector of processes that have mapped the shared array

.. function:: sdata(S::SharedArray)

   Returns the actual ``Array`` object backing ``S``

.. function:: indexpids(S::SharedArray)

   Returns the index of the current worker into the ``pids`` vector, i.e., the list of workers mapping
   the SharedArray


System
------

.. function:: run(command)

   Run a command object, constructed with backticks. Throws an error if anything goes wrong, including the process exiting with a non-zero status.

.. function:: spawn(command)

   Run a command object asynchronously, returning the resulting ``Process`` object.

.. data:: DevNull

   Used in a stream redirect to discard all data written to it. Essentially equivalent to /dev/null on Unix or NUL on Windows.
   Usage: ``run(`cat test.txt` |> DevNull)``

.. function:: success(command)

   Run a command object, constructed with backticks, and tell whether it was successful (exited with a code of 0). An exception is raised if the process cannot be started.

.. function:: process_running(p::Process)

   Determine whether a process is currently running.

.. function:: process_exited(p::Process)

   Determine whether a process has exited.

.. function:: kill(p::Process, signum=SIGTERM)

   Send a signal to a process. The default is to terminate the process.

.. function:: open(command, mode::AbstractString="r", stdio=DevNull)

   Start running ``command`` asynchronously, and return a tuple
   ``(stream,process)``.  If ``mode`` is ``"r"``, then ``stream``
   reads from the process's standard output and ``stdio`` optionally
   specifies the process's standard input stream.  If ``mode`` is
   ``"w"``, then ``stream`` writes to the process's standard input
   and ``stdio`` optionally specifies the process's standard output
   stream.

.. function:: open(f::Function, command, mode::AbstractString="r", stdio=DevNull)

   Similar to ``open(command, mode, stdio)``, but calls ``f(stream)``
   on the resulting read or write stream, then closes the stream
   and waits for the process to complete.  Returns the value returned
   by ``f``.

.. function:: readandwrite(command)

   Starts running a command asynchronously, and returns a tuple (stdout,stdin,process) of the output stream and input stream of the process, and the process object itself.

.. function:: ignorestatus(command)

   Mark a command object so that running it will not throw an error if the
   result code is non-zero.

.. function:: detach(command)

   Mark a command object so that it will be run in a new process group,
   allowing it to outlive the julia process, and not have Ctrl-C interrupts
   passed to it.

.. function:: setenv(command, env; dir=working_dir)

   Set environment variables to use when running the given command. ``env`` is either
   a dictionary mapping strings to strings, or an array of strings of the form
   ``"var=val"``.

   The ``dir`` keyword argument can be used to specify a working directory for the
   command.

.. function:: |>(command, command)
              |>(command, filename)
              |>(filename, command)

   Redirect operator. Used for piping the output of a process into another (first form) or to redirect the standard output/input of a command to/from a file (second and third forms).

   **Examples**:
     * ``run(`ls` |> `grep xyz`)``
     * ``run(`ls` |> "out.txt")``
     * ``run("out.txt" |> `grep xyz`)``

.. function:: >>(command, filename)

   Redirect standard output of a process, appending to the destination file.

.. function:: .>(command, filename)

   Redirect the standard error stream of a process.

.. function:: gethostname() -> AbstractString

   Get the local machine's host name.

.. function:: getipaddr() -> AbstractString

   Get the IP address of the local machine, as a string of the form "x.x.x.x".

.. function:: pwd() -> AbstractString

   Get the current working directory.

.. function:: cd(dir::AbstractString)

   Set the current working directory.

.. function:: cd(f, [dir])

   Temporarily changes the current working directory (HOME if not specified) and applies function f before returning.

.. function:: mkdir(path, [mode])

   Make a new directory with name ``path`` and permissions ``mode``.
   ``mode`` defaults to 0o777, modified by the current file creation mask.

.. function:: mkpath(path, [mode])

   Create all directories in the given ``path``, with permissions ``mode``.
   ``mode`` defaults to 0o777, modified by the current file creation mask.

.. function:: symlink(target, link)

   Creates a symbolic link to ``target`` with the name ``link``.

   .. note::

      This function raises an error under operating systems that do not support
      soft symbolic links, such as Windows XP.

.. function:: chmod(path, mode)

   Change the permissions mode of ``path`` to ``mode``. Only integer ``mode``\ s
   (e.g. 0o777) are currently supported.

.. function:: getpid() -> Int32

   Get julia's process ID.

.. function:: time([t::TmStruct])

   Get the system time in seconds since the epoch, with fairly high (typically, microsecond) resolution. When passed a ``TmStruct``, converts it to a number of seconds since the epoch.

.. function:: time_ns()

   Get the time in nanoseconds. The time corresponding to 0 is undefined, and wraps every 5.8 years.

.. function:: strftime([format], time)

   Convert time, given as a number of seconds since the epoch or a ``TmStruct``, to a formatted string using the given format. Supported formats are the same as those in the standard C library.

.. function:: strptime([format], timestr)

   Parse a formatted time string into a ``TmStruct`` giving the seconds, minute, hour, date, etc. Supported formats are the same as those in the standard C library. On some platforms, timezones will not be parsed correctly. If the result of this function will be passed to ``time`` to convert it to seconds since the epoch, the ``isdst`` field should be filled in manually. Setting it to ``-1`` will tell the C library to use the current system settings to determine the timezone.

.. function:: TmStruct([seconds])

   Convert a number of seconds since the epoch to broken-down format, with fields ``sec``, ``min``, ``hour``, ``mday``, ``month``, ``year``, ``wday``, ``yday``, and ``isdst``.

.. function:: tic()

   Set a timer to be read by the next call to :func:`toc` or :func:`toq`. The macro call ``@time expr`` can also be used to time evaluation.

.. function:: toc()

   Print and return the time elapsed since the last :func:`tic`.

.. function:: toq()

   Return, but do not print, the time elapsed since the last :func:`tic`.

.. function:: @time

   A macro to execute an expression, printing the time it took to execute and the total number of bytes its execution caused to be allocated, before returning the value of the expression.

.. function:: @elapsed

   A macro to evaluate an expression, discarding the resulting value, instead returning the number of seconds it took to execute as a floating-point number.

.. function:: @allocated

   A macro to evaluate an expression, discarding the resulting value, instead returning the total number of bytes allocated during evaluation of the expression.

.. function:: EnvHash() -> EnvHash

   A singleton of this type provides a hash table interface to environment variables.

.. data:: ENV

   Reference to the singleton ``EnvHash``, providing a dictionary interface to system environment variables.

.. function:: @unix

   Given ``@unix? a : b``, do ``a`` on Unix systems (including Linux and OS X) and ``b`` elsewhere. See documentation
   for Handling Platform Variations in the Calling C and Fortran Code section of the manual.

.. function:: @osx

   Given ``@osx? a : b``, do ``a`` on OS X and ``b`` elsewhere. See documentation for Handling Platform Variations
   in the Calling C and Fortran Code section of the manual.

.. function:: @linux

   Given ``@linux? a : b``, do ``a`` on Linux and ``b`` elsewhere. See documentation for Handling Platform Variations
   in the Calling C and Fortran Code section of the manual.

.. function:: @windows

   Given ``@windows? a : b``, do ``a`` on Windows and ``b`` elsewhere. See documentation for Handling Platform Variations
   in the Calling C and Fortran Code section of the manual.

C Interface
-----------

.. function:: ccall((symbol, library) or fptr, RetType, (ArgType1, ...), ArgVar1, ...)

   Call function in C-exported shared library, specified by ``(function name, library)`` tuple, where each component is a AbstractString or :Symbol. Alternatively,
   ccall may be used to call a function pointer returned by dlsym, but note that this usage is generally discouraged to facilitate future static compilation.
   Note that the argument type tuple must be a literal tuple, and not a tuple-valued variable or expression.

.. function:: cglobal((symbol, library) or ptr [, Type=Void])

   Obtain a pointer to a global variable in a C-exported shared library, specified exactly as in ``ccall``.  Returns a ``Ptr{Type}``, defaulting to ``Ptr{Void}`` if no Type argument is supplied.  The values can be read or written by ``unsafe_load`` or ``unsafe_store!``, respectively.

.. function:: cfunction(fun::Function, RetType::Type, (ArgTypes...))

   Generate C-callable function pointer from Julia function. Type annotation of the return value in the
   callback function is a must for situations where Julia cannot infer the return type automatically.

   For example::

   	function foo()
   	    # body

   	    retval::Float64
   	end

   	bar = cfunction(foo, Float64, ())


.. function:: dlopen(libfile::AbstractString [, flags::Integer])

   Load a shared library, returning an opaque handle.

   The optional flags argument is a bitwise-or of zero or more of
   ``RTLD_LOCAL``, ``RTLD_GLOBAL``, ``RTLD_LAZY``, ``RTLD_NOW``, ``RTLD_NODELETE``,
   ``RTLD_NOLOAD``, ``RTLD_DEEPBIND``, and ``RTLD_FIRST``.  These are converted to
   the corresponding flags of the POSIX (and/or GNU libc and/or MacOS)
   dlopen command, if possible, or are ignored if the specified
   functionality is not available on the current platform.  The
   default is ``RTLD_LAZY|RTLD_DEEPBIND|RTLD_LOCAL``.  An important usage
   of these flags, on POSIX platforms, is to specify
   ``RTLD_LAZY|RTLD_DEEPBIND|RTLD_GLOBAL`` in order for the library's
   symbols to be available for usage in other shared libraries, in
   situations where there are dependencies between shared libraries.

.. function:: dlopen_e(libfile::AbstractString [, flags::Integer])

   Similar to :func:`dlopen`, except returns a ``NULL`` pointer instead of raising errors.

.. data:: RTLD_DEEPBIND

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_FIRST

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_GLOBAL

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_LAZY

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_LOCAL

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_NODELETE

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_NOLOAD

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_NOW

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. function:: dlsym(handle, sym)

   Look up a symbol from a shared library handle, return callable function pointer on success.

.. function:: dlsym_e(handle, sym)

   Look up a symbol from a shared library handle, silently return NULL pointer on lookup failure.

.. function:: dlclose(handle)

   Close shared library referenced by handle.

.. function:: find_library(names, locations)

   Searches for the first library in ``names`` in the paths in the ``locations`` list, ``DL_LOAD_PATH``, or system
   library paths (in that order) which can successfully be dlopen'd. On success, the return value will be one of
   the names (potentially prefixed by one of the paths in locations). This string can be assigned to a ``global const``
   and used as the library name in future ``ccall``'s. On failure, it returns the empty string.

.. data:: DL_LOAD_PATH

   When calling ``dlopen``, the paths in this list will be searched first, in order, before searching the
   system locations for a valid library handle.

.. function:: c_malloc(size::Integer) -> Ptr{Void}

   Call ``malloc`` from the C standard library.

.. function:: c_calloc(num::Integer, size::Integer) -> Ptr{Void}

   Call ``calloc`` from the C standard library.

.. function:: c_realloc(addr::Ptr, size::Integer) -> Ptr{Void}

   Call ``realloc`` from the C standard library.

.. function:: c_free(addr::Ptr)

   Call ``free`` from the C standard library.

.. function:: unsafe_load(p::Ptr{T},i::Integer)

   Load a value of type ``T`` from the address of the ith element (1-indexed)
   starting at ``p``. This is equivalent to the C expression ``p[i-1]``.

.. function:: unsafe_store!(p::Ptr{T},x,i::Integer)

   Store a value of type ``T`` to the address of the ith element (1-indexed)
   starting at ``p``. This is equivalent to the C expression ``p[i-1] = x``.

.. function:: unsafe_copy!(dest::Ptr{T}, src::Ptr{T}, N)

   Copy ``N`` elements from a source pointer to a destination, with no checking. The
   size of an element is determined by the type of the pointers.

.. function:: unsafe_copy!(dest::Array, do, src::Array, so, N)

   Copy ``N`` elements from a source array to a destination, starting at offset ``so``
   in the source and ``do`` in the destination (1-indexed).

.. function:: copy!(dest, src)

   Copy all elements from collection ``src`` to array ``dest``. Returns ``dest``.

.. function:: copy!(dest, do, src, so, N)

   Copy ``N`` elements from collection ``src`` starting at offset ``so``, to
   array ``dest`` starting at offset ``do``. Returns ``dest``.

.. function:: pointer(a[, index])

   Get the native address of an array or string element. Be careful to
   ensure that a julia reference to ``a`` exists as long as this
   pointer will be used.

.. function:: pointer(type, int)

   Convert an integer to a pointer of the specified element type.

.. function:: pointer_to_array(p, dims[, own])

   Wrap a native pointer as a Julia Array object. The pointer element type determines
   the array element type. ``own`` optionally specifies whether Julia should take
   ownership of the memory, calling ``free`` on the pointer when the array is no
   longer referenced.

.. function:: pointer_from_objref(obj)

   Get the memory address of a Julia object as a ``Ptr``. The existence of the resulting
   ``Ptr`` will not protect the object from garbage collection, so you must ensure
   that the object remains referenced for the whole time that the ``Ptr`` will be used.

.. function:: unsafe_pointer_to_objref(p::Ptr)

   Convert a ``Ptr`` to an object reference. Assumes the pointer refers to a
   valid heap-allocated Julia object. If this is not the case, undefined behavior
   results, hence this function is considered "unsafe" and should be used with care.

.. function:: disable_sigint(f::Function)

   Disable Ctrl-C handler during execution of a function, for calling
   external code that is not interrupt safe. Intended to be called using ``do``
   block syntax as follows::

    disable_sigint() do
        # interrupt-unsafe code
        ...
    end

.. function:: reenable_sigint(f::Function)

   Re-enable Ctrl-C handler during execution of a function. Temporarily
   reverses the effect of ``disable_sigint``.

.. function:: errno([code])

   Get the value of the C library's ``errno``. If an argument is specified, it is
   used to set the value of ``errno``.

   The value of ``errno`` is only valid immediately after a ``ccall`` to a C
   library routine that sets it. Specifically, you cannot call ``errno`` at the next
   prompt in a REPL, because lots of code is executed between prompts.

.. function:: systemerror(sysfunc, iftrue)

   Raises a ``SystemError`` for ``errno`` with the descriptive string ``sysfunc`` if ``bool`` is true

.. function:: strerror(n)

   Convert a system call error code to a descriptive string

.. data:: Cchar

   Equivalent to the native ``char`` c-type

.. data:: Cuchar

   Equivalent to the native ``unsigned char`` c-type (UInt8)

.. data:: Cshort

   Equivalent to the native ``signed short`` c-type (Int16)

.. data:: Cushort

   Equivalent to the native ``unsigned short`` c-type (UInt16)

.. data:: Cint

   Equivalent to the native ``signed int`` c-type (Int32)

.. data:: Cuint

   Equivalent to the native ``unsigned int`` c-type (UInt32)

.. data:: Clong

   Equivalent to the native ``signed long`` c-type

.. data:: Culong

   Equivalent to the native ``unsigned long`` c-type

.. data:: Clonglong

   Equivalent to the native ``signed long long`` c-type (Int64)

.. data:: Culonglong

   Equivalent to the native ``unsigned long long`` c-type (UInt64)

.. data:: Cintmax_t

   Equivalent to the native ``intmax_t`` c-type (Int64)

.. data:: Cuintmax_t

   Equivalent to the native ``uintmax_t`` c-type (UInt64)

.. data:: Csize_t

   Equivalent to the native ``size_t`` c-type (UInt)

.. data:: Cssize_t

   Equivalent to the native ``ssize_t`` c-type

.. data:: Cptrdiff_t

   Equivalent to the native ``ptrdiff_t`` c-type (Int)

.. data:: Coff_t

   Equivalent to the native ``off_t`` c-type

.. data:: Cwchar_t

   Equivalent to the native ``wchar_t`` c-type (Int32)

.. data:: Cfloat

   Equivalent to the native ``float`` c-type (Float32)

.. data:: Cdouble

   Equivalent to the native ``double`` c-type (Float64)


Errors
------

.. function:: error(message::AbstractString)

   Raise an error with the given message

.. function:: throw(e)

   Throw an object as an exception

.. function:: rethrow([e])

   Throw an object without changing the current exception backtrace.
   The default argument is the current exception (if called within a
   ``catch`` block).

.. function:: backtrace()

   Get a backtrace object for the current program point.

.. function:: catch_backtrace()

   Get the backtrace of the current exception, for use within ``catch``
   blocks.

.. function:: assert(cond, [text])

   Raise an error if ``cond`` is false. Also available as the macro ``@assert expr``.

.. function:: @assert

   Raise an error if ``cond`` is false. Preferred syntax for writings assertions.

.. data:: ArgumentError

   The parameters given to a function call are not valid.

.. data:: BoundsError

   An indexing operation into an array tried to access an out-of-bounds element.

.. data:: EOFError

   No more data was available to read from a file or stream.

.. data:: ErrorException

   Generic error type. The error message, in the `.msg` field, may provide more specific details.

.. data:: KeyError

   An indexing operation into an ``Associative`` (``Dict``) or ``Set`` like object tried to access or delete a non-existent element.

.. data:: LoadError

   An error occurred while `including`, `requiring`, or `using` a file. The error specifics should be available in the `.error` field.

.. data:: MethodError

   A method with the required type signature does not exist in the given generic function.

.. data:: ParseError

   The expression passed to the `parse` function could not be interpreted as a valid Julia expression.

.. data:: ProcessExitedException

   After a client Julia process has exited, further attempts to reference the dead child will throw this exception.

.. data:: SystemError

   A system call failed with an error code (in the ``errno`` global variable).

.. data:: TypeError

   A type assertion failure, or calling an intrinsic function with an incorrect argument type.


Tasks
-----

.. function:: Task(func)

   Create a ``Task`` (i.e. thread, or coroutine) to execute the given function (which must be callable with no arguments). The task exits when this function returns.

.. function:: yieldto(task, args...)

   Switch to the given task. The first time a task is switched to, the task's function is called with no arguments. On subsequent switches, ``args`` are returned from the task's last call to ``yieldto``. This is a low-level call that only switches tasks, not considering states or scheduling in any way.

.. function:: current_task()

   Get the currently running Task.

.. function:: istaskdone(task) -> Bool

   Tell whether a task has exited.

.. function:: consume(task, values...)

   Receive the next value passed to ``produce`` by the specified task.
   Additional arguments may be passed, to be returned from the last ``produce`` call
   in the producer.

.. function:: produce(value)

   Send the given value to the last ``consume`` call, switching to the consumer task.
   If the next ``consume`` call passes any values, they are returned by ``produce``.

.. function:: yield()

   Switch to the scheduler to allow another scheduled task to run. A task that calls this function is still runnable, and will be restarted immediately if there are no other runnable tasks.

.. function:: task_local_storage(symbol)

   Look up the value of a symbol in the current task's task-local storage.

.. function:: task_local_storage(symbol, value)

   Assign a value to a symbol in the current task's task-local storage.

.. function:: task_local_storage(body, symbol, value)

   Call the function ``body`` with a modified task-local storage, in which
   ``value`` is assigned to ``symbol``; the previous value of ``symbol``, or
   lack thereof, is restored afterwards. Useful for emulating dynamic scoping.

.. function:: Condition()

   Create an edge-triggered event source that tasks can wait for. Tasks
   that call ``wait`` on a ``Condition`` are suspended and queued.
   Tasks are woken up when ``notify`` is later called on the ``Condition``.
   Edge triggering means that only tasks waiting at the time ``notify`` is
   called can be woken up. For level-triggered notifications, you must
   keep extra state to keep track of whether a notification has happened.
   The ``RemoteRef`` type does this, and so can be used for level-triggered
   events.

.. function:: notify(condition, val=nothing; all=true, error=false)

   Wake up tasks waiting for a condition, passing them ``val``.
   If ``all`` is true (the default), all waiting tasks are woken, otherwise
   only one is. If ``error`` is true, the passed value is raised as an
   exception in the woken tasks.

.. function:: schedule(t::Task, [val]; error=false)

   Add a task to the scheduler's queue. This causes the task to run constantly
   when the system is otherwise idle, unless the task performs a blocking
   operation such as ``wait``.

   If a second argument is provided, it will be passed to the task (via the
   return value of ``yieldto``) when it runs again. If ``error`` is true,
   the value is raised as an exception in the woken task.

.. function:: @schedule

   Wrap an expression in a Task and add it to the scheduler's queue.

.. function:: @task

   Wrap an expression in a Task executing it, and return the Task. This
   only creates a task, and does not run it.

.. function:: sleep(seconds)

   Block the current task for a specified number of seconds. The minimum sleep
   time is 1 millisecond or input of ``0.001``.

Events
------

.. function:: Timer(f::Function)

   Create a timer to call the given callback function. The callback
   is passed one argument, the timer object itself. The timer can be
   started and stopped with ``start_timer`` and ``stop_timer``.

.. function:: start_timer(t::Timer, delay, repeat)

   Start invoking the callback for a ``Timer`` after the specified initial
   delay, and then repeating with the given interval. Times are in seconds.
   If ``repeat`` is ``0``, the timer is only triggered once.

.. function:: stop_timer(t::Timer)

   Stop invoking the callback for a timer.

Reflection
----------

.. function:: module_name(m::Module) -> Symbol

   Get the name of a module as a symbol.

.. function:: module_parent(m::Module) -> Module

   Get a module's enclosing module. ``Main`` is its own parent.

.. function:: current_module() -> Module

   Get the *dynamically* current module, which is the module code is currently being
   read from. In general, this is not the same as the module containing the call to
   this function.

.. function:: fullname(m::Module)

   Get the fully-qualified name of a module as a tuple of symbols. For example,
   ``fullname(Base.Pkg)`` gives ``(:Base,:Pkg)``, and ``fullname(Main)`` gives ``()``.

.. function:: names(x::Module[, all=false[, imported=false]])

   Get an array of the names exported by a module, with optionally more module
   globals according to the additional parameters.

.. function:: names(x::DataType)

   Get an array of the fields of a data type.

.. function:: isconst([m::Module], s::Symbol) -> Bool

   Determine whether a global is declared ``const`` in a given module.
   The default module argument is ``current_module()``.

.. function:: isgeneric(f::Function) -> Bool

   Determine whether a function is generic.

.. function:: function_name(f::Function) -> Symbol

   Get the name of a generic function as a symbol, or ``:anonymous``.

.. function:: function_module(f::Function, types) -> Module

   Determine the module containing a given definition of a generic function.

.. function:: functionloc(f::Function, types)

   Returns a tuple ``(filename,line)`` giving the location of a method definition.

.. function:: functionlocs(f::Function, types)

   Returns an array of the results of ``functionloc`` for all matching definitions.

Internals
---------

.. function:: gc()

   Perform garbage collection. This should not generally be used.

.. function:: gc_disable()

   Disable garbage collection. This should be used only with extreme
   caution, as it can cause memory use to grow without bound.

.. function:: gc_enable()

   Re-enable garbage collection after calling ``gc_disable``.

.. function:: macroexpand(x)

   Takes the expression x and returns an equivalent expression with all macros removed (expanded).

.. function:: expand(x)

   Takes the expression x and returns an equivalent expression in lowered form

.. function:: code_lowered(f, types)

   Returns an array of lowered ASTs for the methods matching the given generic function and type signature.

.. function:: @code_lowered

   Evaluates the arguments to the function call, determines their types, and calls the ``code_lowered`` function on the resulting expression

.. function:: code_typed(f, types)

   Returns an array of lowered and type-inferred ASTs for the methods matching the given generic function and type signature.

.. function:: @code_typed

   Evaluates the arguments to the function call, determines their types, and calls the ``code_typed`` function on the resulting expression

.. function:: code_warntype(f, types)

   Returns an array of lowered and type-inferred ASTs for the methods matching the given generic function and type signature. The ASTs are annotated in such a way as to cause "non-leaf" types to be displayed in red. This serves as a warning of potential type instability. Not all non-leaf types are particularly problematic for performance, so the results need to be used judiciously. See :ref:`man-code-warntype` for more information.

.. function:: @code_warntype

   Evaluates the arguments to the function call, determines their types, and calls the ``code_warntype`` function on the resulting expression

.. function:: code_llvm(f, types)

   Prints the LLVM bitcodes generated for running the method matching the given generic function and type signature to STDOUT.

.. function:: @code_llvm

   Evaluates the arguments to the function call, determines their types, and calls the ``code_llvm`` function on the resulting expression

.. function:: code_native(f, types)

   Prints the native assembly instructions generated for running the method matching the given generic function and type signature to STDOUT.

.. function:: @code_native

   Evaluates the arguments to the function call, determines their types, and calls the ``code_native`` function on the resulting expression

.. function:: precompile(f,args::(Any...,))

   Compile the given function ``f`` for the argument tuple (of types) ``args``, but do not execute it.
