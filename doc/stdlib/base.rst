.. currentmodule:: Base

Getting Around
--------------

.. function:: exit([code])

   Quit (or control-D at the prompt). The default exit code is zero, indicating that the processes completed successfully.

.. function:: whos([Module,] [pattern::Regex])

   Print information about global variables in a module, optionally restricted
   to those matching ``pattern``.

.. function:: edit(file::String, [line])

   Edit a file optionally providing a line number to edit at. Returns to the julia prompt when you quit the editor. If the file name ends in ".jl" it is reloaded when the editor closes the file.

.. function:: edit(function, [types])

   Edit the definition of a function, optionally specifying a tuple of types to indicate which method to edit. When the editor exits, the source file containing the definition is reloaded.

.. function:: require(file::String...)

   Load source files once, in the context of the ``Main`` module, on every active node, searching the system-wide ``LOAD_PATH`` for files. ``require`` is considered a top-level operation, so it sets the current ``include`` path but does not use it to search for files (see help for ``include``). This function is typically used to load library code, and is implicitly called by ``using`` to load packages.

.. function:: reload(file::String)

   Like ``require``, except forces loading of files regardless of whether they have been loaded before. Typically used when interactively developing libraries.

.. function:: include(path::String)

   Evaluate the contents of a source file in the current context. During including, a task-local include path is set to the directory containing the file. Nested calls to ``include`` will search relative to that path. All paths refer to files on node 1 when running in parallel, and files will be fetched from node 1. This function is typically used to load source interactively, or to combine files in packages that are broken into multiple source files.

.. function:: include_string(code::String)

   Like ``include``, except reads code from the given string rather than from a file. Since there is no file path involved, no path processing or fetching from node 1 is done.

.. function:: evalfile(path::String)

   Evaluate all expressions in the given file, and return the value of the last one. No other processing (path searching, fetching from node 1, etc.) is performed.

.. function:: help(name)

   Get help for a function. ``name`` can be an object or a string.

.. function:: apropos(string)

   Search documentation for functions related to ``string``.

.. function:: which(f, args...)

   Show which method of ``f`` will be called for the given arguments.

.. function:: methods(f)

   Show all methods of ``f`` with their argument types.

.. function:: methodswith(t)

   Show all methods with an argument of type ``typ``.

All Objects
-----------

.. function:: is(x, y)

   Determine whether ``x`` and ``y`` are identical, in the sense that no program could distinguish them.

.. function:: isa(x, type)

   Determine whether ``x`` is of the given type.

.. function:: isequal(x, y)

   True if and only if ``x`` and ``y`` have the same contents. Loosely speaking, this means ``x`` and ``y`` would look the same when printed.

.. function:: isless(x, y)

   Test whether ``x`` is less than ``y``. Provides a total order consistent with ``isequal``. Values that are normally unordered, such as ``NaN``, are ordered in an arbitrary but consistent fashion. This is the default comparison used by ``sort``. Non-numeric types that can be ordered should implement this function.

.. function:: typeof(x)

   Get the concrete type of ``x``.

.. function:: tuple(xs...)

   Construct a tuple of the given objects.

.. function:: ntuple(n, f::Function)

   Create a tuple of length ``n``, computing each element as ``f(i)``, where ``i`` is the index of the element.

.. function:: object_id(x)

   Get a unique integer id for ``x``. ``object_id(x)==object_id(y)`` if and only if ``is(x,y)``.

.. function:: hash(x)

   Compute an integer hash code such that ``isequal(x,y)`` implies ``hash(x)==hash(y)``.

.. function:: finalizer(x, function)

   Register a function ``f(x)`` to be called when there are no program-accessible references to ``x``. The behavior of this function is unpredictable if ``x`` is of a bits type.

.. function:: copy(x)

   Create a shallow copy of ``x``: the outer structure is copied, but not all internal values. For example, copying an array produces a new array with identically-same elements as the original.

.. function:: deepcopy(x)

   Create a deep copy of ``x``: everything is copied recursively, resulting in a fully independent object. For example, deep-copying an array produces a new array whose elements are deep-copies of the original elements.

   As a special case, functions can only be actually deep-copied if they are anonymous, otherwise they are just copied. The difference is only relevant in the case of closures, i.e. functions which may contain hidden internal references.

   While it isn't normally necessary, user-defined types can override the default ``deepcopy`` behavior by defining a specialized version of the function ``deepcopy_internal(x::T, dict::ObjectIdDict)`` (which shouldn't otherwise be used), where ``T`` is the type to be specialized for, and ``dict`` keeps track of objects copied so far within the recursion. Within the definition, ``deepcopy_internal`` should be used in place of ``deepcopy``, and the ``dict`` variable should be updated as appropriate before returning.

.. function:: convert(type, x)

   Try to convert ``x`` to the given type.

.. function:: promote(xs...)

   Convert all arguments to their common promotion type (if any), and return them all (as a tuple).

Types
-----

.. function:: subtype(type1, type2)

   True if and only if all values of ``type1`` are also of ``type2``. Can also be written using the ``<:`` infix operator as ``type1 <: type2``.

.. function:: typemin(type)

   The lowest value representable by the given (real) numeric type.

.. function:: typemax(type)

   The highest value representable by the given (real) numeric type.

.. function:: realmin(type)

   The smallest in absolute value non-denormal value representable by the given floating-point type

.. function:: realmax(type)

   The highest finite value representable by the given floating-point type

.. function:: maxintfloat(type)

   The largest integer losslessly representable by the given floating-point type

.. function:: sizeof(type)

   Size, in bytes, of the canonical binary representation of the given type, if any.

.. function:: eps([type])

   The distance between 1.0 and the next larger representable floating-point value of ``type``. The only types that are sensible arguments are ``Float32`` and ``Float64``. If ``type`` is omitted, then ``eps(Float64)`` is returned.

.. function:: eps(x)

   The distance between ``x`` and the next larger representable floating-point value of the same type as ``x``.

.. function:: promote_type(type1, type2)

   Determine a type big enough to hold values of each argument type without loss, whenever possible. In some cases, where no type exists which to which both types can be promoted losslessly, some loss is tolerated; for example, ``promote_type(Int64,Float64)`` returns ``Float64`` even though strictly, not all ``Int64`` values can be represented exactly as ``Float64`` values.

Generic Functions
-----------------

.. function:: method_exists(f, tuple) -> Bool

   Determine whether the given generic function has a method matching the given tuple of argument types.

   **Example**: ``method_exists(length, (Array,)) = true``

.. function:: applicable(f, args...)

   Determine whether the given generic function has a method applicable to the given arguments.

.. function:: invoke(f, (types...), args...)

   Invoke a method for the given generic function matching the specified types (as a tuple), on the specified arguments. The arguments must be compatible with the specified types. This allows invoking a method other than the most specific matching method, which is useful when the behavior of a more general definition is explicitly needed (often as part of the implementation of a more specific method of the same function).

.. function:: |(x, f)
   
   Applies a function to the preceding argument which allows for easy function chaining.

   **Example**: ``[1:5] | x->x.^2 | sum | inv``

Iteration
---------

Sequential iteration is implemented by the methods ``start``, ``done``, and
``next``. The general ``for`` loop::

    for i = I
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

   Note that ``zip`` is it's own inverse: ``[zip(zip(a...)...)...] == [a...]``.


Fully implemented by: ``Range``, ``Range1``, ``NDRange``, ``Tuple``, ``Real``, ``AbstractArray``, ``IntSet``, ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``, ``EachLine``, ``String``, ``Set``, ``Task``.

General Collections
-------------------

.. function:: isempty(collection) -> Bool

   Determine whether a collection is empty (has no elements).

.. function:: empty!(collection) -> collection

   Remove all elements from a collection.

.. function:: length(collection) -> Integer

   For ordered, indexable collections, the maximum index ``i`` for which ``getindex(collection, i)`` is valid. For unordered collections, the number of elements.

.. function:: endof(collection) -> Integer

   Returns the last index of the collection.
   
   **Example**: ``endof([1,2,4]) = 3``

Fully implemented by: ``Range``, ``Range1``, ``Tuple``, ``Number``, ``AbstractArray``, ``IntSet``, ``Dict``, ``WeakKeyDict``, ``String``, ``Set``.

Iterable Collections
--------------------

.. function:: contains(itr, x) -> Bool

   Determine whether a collection contains the given value, ``x``.

.. function:: findin(a, b)

   Returns the indices of elements in collection ``a`` that appear in collection ``b``

.. function:: unique(itr)

   Returns an array containing only the unique elements of the iterable ``itr``.

.. function:: reduce(op, v0, itr)

   Reduce the given collection with the given operator, i.e. accumulate ``v = op(v,elt)`` for each element, where ``v`` starts as ``v0``. Reductions for certain commonly-used operators are available in a more convenient 1-argument form: ``max(itr)``, ``min(itr)``, ``sum(itr)``, ``prod(itr)``, ``any(itr)``, ``all(itr)``.

.. function:: max(itr)

   Returns the largest element in a collection

.. function:: min(itr)

   Returns the smallest element in a collection

.. function:: indmax(itr) -> Integer

   Returns the index of the maximum element in a collection

.. function:: indmin(itr) -> Integer

   Returns the index of the minimum element in a collection

.. function:: findmax(itr) -> (x, index)

   Returns the maximum element and its index

.. function:: findmin(itr) -> (x, index)

   Returns the minimum element and its index

.. function:: sum(itr)

   Returns the sum of all elements in a collection

.. function:: prod(itr)

   Returns the product of all elements of a collection

.. function:: any(itr) -> Bool

   Test whether any elements of a boolean collection are true

.. function:: all(itr) -> Bool

   Test whether all elements of a boolean collection are true

.. function:: count(itr) -> Integer

   Count the number of boolean elements in ``itr`` which are true.

.. function:: countp(p, itr) -> Integer

   Count the number of elements in ``itr`` for which predicate ``p`` is true.

.. function:: any(p, itr) -> Bool

   Determine whether any element of ``itr`` satisfies the given predicate.

.. function:: all(p, itr) -> Bool

   Determine whether all elements of ``itr`` satisfy the given predicate.

.. function:: map(f, c) -> collection

   Transform collection ``c`` by applying ``f`` to each element.

   **Example**: ``map((x) -> x * 2, [1, 2, 3]) = [2, 4, 6]``

.. function:: map!(function, collection)

   In-place version of :func:`map`.

.. function:: mapreduce(f, op, itr)

   Applies function ``f`` to each element in ``itr`` and then reduces the result using the binary function ``op``.

   **Example**: ``mapreduce(x->x^2, +, [1:3]) == 1 + 4 + 9 == 14``

.. function:: first(coll)

   Get the first element of an ordered collection.

.. function:: last(coll)

   Get the last element of an ordered collection.

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

Fully implemented by: ``Array``, ``DArray``, ``AbstractArray``, ``SubArray``, ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``, ``String``.

Partially implemented by: ``Range``, ``Range1``, ``Tuple``.

Associative Collections
-----------------------

``Dict`` is the standard associative collection. Its implementation uses the ``hash(x)`` as the hashing function for the key, and ``isequal(x,y)`` to determine equality. Define these two functions for custom types to override how they are stored in a hash table.

``ObjectIdDict`` is a special hash table where the keys are always object identities. ``WeakKeyDict`` is a hash table implementation where the keys are weak references to objects, and thus may be garbage collected even when referenced in a hash table.

Dicts can be created using a literal syntax: ``{"A"=>1, "B"=>2}``. Use of curly brackets will create a ``Dict`` of type ``Dict{Any,Any}``. Use of square brackets will attempt to infer type information from the keys and values (i.e. ``["A"=>1, "B"=>2]`` creates a ``Dict{ASCIIString, Int64}``). To explicitly specify types use the syntax: ``(KeyType=>ValueType)[...]``. For example, ``(ASCIIString=>Int32)["A"=>1, "B"=>2]``.

As with arrays, ``Dicts`` may be created with comprehensions. For example,
``{i => f(i) for i = 1:10}``.

.. function:: Dict{K,V}()

   Construct a hashtable with keys of type K and values of type V

.. function:: has(collection, key)

   Determine whether a collection has a mapping for a given key.

.. function:: get(collection, key, default)

   Return the value stored for the given key, or the given default value if no mapping for the key is present.

.. function:: getkey(collection, key, default)

   Return the key matching argument ``key`` if one exists in ``collection``, otherwise return ``default``.

.. function:: delete!(collection, key)

   Delete the mapping for the given key in a collection.

.. function:: empty!(collection)

   Delete all keys from a collection.

.. function:: keys(collection)

   Return an array of all keys in a collection.

.. function:: values(collection)

   Return an array of all values in a collection.

.. function:: collect(collection)

   Return an array of all items in a collection. For associative collections, returns (key, value) tuples.

.. function:: merge(collection, others...)

   Construct a merged collection from the given collections.

.. function:: merge!(collection, others...)

   Update collection with pairs from the other collections

.. function:: filter(function, collection)

   Return a copy of collection, removing (key, value) pairs for which function is false.

.. function:: filter!(function, collection)

   Update collection, removing (key, value) pairs for which function is false.

.. function:: eltype(collection)

   Returns the type tuple of the (key,value) pairs contained in collection.

.. function:: sizehint(s, n)

   Suggest that collection ``s`` reserve capacity for at least ``n`` elements. This can improve performance.
   
Fully implemented by: ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``.

Partially implemented by: ``IntSet``, ``Set``, ``EnvHash``, ``Array``.

Set-Like Collections
--------------------

.. function:: add!(collection, key)

   Add an element to a set-like collection.

.. function:: add_each!(collection, iterable)

   Adds each element in iterable to the collection.

.. function:: Set(x...)

   Construct a ``Set`` with the given elements. Should be used instead of ``IntSet`` for sparse integer sets.

.. function:: IntSet(i...)

   Construct an ``IntSet`` of the given integers. Implemented as a bit string, and therefore good for dense integer sets.

.. function:: union(s1,s2...)

   Construct the union of two or more sets. Maintains order with arrays.

.. function:: union!(s1,s2)

   Constructs the union of IntSets s1 and s2, stores the result in ``s1``.

.. function:: intersect(s1,s2...)

   Construct the intersection of two or more sets. Maintains order with arrays.

.. function:: setdiff(s1,s2)

   Construct the set of elements in ``s1`` but not ``s2``. Maintains order with arrays.

.. function:: symdiff(s1,s2...)

   Construct the symmetric difference of elements in the passed in sets or arrays. Maintains order with arrays.

.. function:: symdiff!(s, n)

   IntSet s is destructively modified to toggle the inclusion of integer ``n``.

.. function:: symdiff!(s, itr)

   For each element in ``itr``, destructively toggle its inclusion in set ``s``.

.. function:: symdiff!(s1, s2)

   Construct the symmetric difference of IntSets ``s1`` and ``s2``, storing the result in ``s1``.

.. function:: complement(s)

   Returns the set-complement of IntSet s.

.. function:: complement!(s)

   Mutates IntSet s into its set-complement.

.. function:: del_each!(s, itr)

   Deletes each element of itr in set s in-place.

.. function:: intersect!(s1, s2)

   Intersects IntSets s1 and s2 and overwrites the set s1 with the result. If needed, s1 will be expanded to the size of s2.

Fully implemented by: ``IntSet``, ``Set``.

Partially implemented by: ``Array``.

Dequeues
--------

.. function:: push!(collection, item) -> collection

   Insert an item at the end of a collection.

.. function:: pop!(collection) -> item

   Remove the last item in a collection and return it.

.. function:: unshift!(collection, item) -> collection

   Insert an item at the beginning of a collection.

.. function:: shift!(collection) -> item

   Remove the first item in a collection.

.. function:: insert!(collection, index, item)

   Insert an item at the given index.

.. function:: delete!(collection, index) -> item

   Remove the item at the given index, and return the deleted item.

.. function:: delete!(collection, range) -> items
   
   Remove items at specified range, and return a collection containing the deleted items.

.. function:: resize!(collection, n) -> collection

   Resize collection to contain ``n`` elements.

.. function:: append!(collection, items) -> collection

   Add the elements of ``items`` to the end of a collection.

Fully implemented by: ``Vector`` (aka 1-d ``Array``).

Strings
-------

.. function:: length(s)

   The number of characters in string ``s``.

.. function:: collect(string)

   Return an array of the characters in ``string``.

.. function:: *(s, t)

   Concatenate strings.

   **Example**: ``"Hello " * "world" == "Hello world"``

.. function:: ^(s, n)

   Repeat string ``s`` ``n`` times.

   **Example**: ``"Julia "^3 == "Julia Julia Julia "``

.. function:: string(char...)

   Create a string with the given characters.

.. function:: string(x)

   Create a string from any value using the ``print`` function.

.. function:: repr(x)

   Create a string from any value using the ``show`` function.

.. function:: bytestring(::Ptr{Uint8})

   Create a string from the address of a C (0-terminated) string. A copy is made; the ptr can be safely freed.

.. function:: bytestring(s)

   Convert a string to a contiguous byte array representation appropriate for passing it to C functions.

.. function:: ascii(::Array{Uint8,1})

   Create an ASCII string from a byte array.

.. function:: ascii(s)

   Convert a string to a contiguous ASCII string (all characters must be valid ASCII characters).

.. function:: utf8(::Array{Uint8,1})

   Create a UTF-8 string from a byte array.

.. function:: utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).

.. function:: is_valid_ascii(s) -> Bool

   Returns true if the string is valid ASCII, false otherwise.

.. function:: is_valid_utf8(s) -> Bool

   Returns true if the string is valid UTF-8, false otherwise.

.. function:: check_ascii(s)

   Calls :func:`is_valid_ascii` on string. Throws error if it is not valid.

.. function:: check_utf8(s)

   Calls :func:`is_valid_utf8` on string. Throws error if it is not valid.

.. function:: byte_string_classify(s)

   Returns 0 if the string is neither valid ASCII nor UTF-8, 1 if it is valid ASCII, and 2 if it is valid UTF-8.

.. function:: search(string, char, [i])

   Return the index of ``char`` in ``string``, giving 0 if not found. The second argument may also be a vector or a set of characters. The third argument optionally specifies a starting index.

.. function:: ismatch(r::Regex, s::String)

   Test whether a string contains a match of the given regular expression.

.. function:: lpad(string, n, p)

   Make a string at least ``n`` characters long by padding on the left with copies of ``p``.

.. function:: rpad(string, n, p)

   Make a string at least ``n`` characters long by padding on the right with copies of ``p``.

.. function:: search(string, chars, [start])

   Search for the given characters within the given string. The second argument may be a single character, a vector or a set of characters, a string, or a regular expression (but regular expressions are only allowed on contiguous strings, such as ASCII or UTF-8 strings). The third argument optionally specifies a starting index. The return value is a range of indexes where the matching sequence is found, such that ``s[search(s,x)] == x``. The return value is ``0:-1`` if there is no match.

.. function:: replace(string, pat, r[, n])

   Search for the given pattern ``pat``, and replace each occurance with ``r``. If ``n`` is provided, replace at most ``n`` occurances.  As with search, the second argument may be a single character, a vector or a set of characters, a string, or a regular expression.

.. function:: replace(string, pat, f[, n])

   Search for the given pattern ``pat``, and replace each occurance with ``f(pat)``. If ``n`` is provided, replace at most ``n`` occurances.  As with search, the second argument may be a single character, a vector or a set of characters, a string, or a regular expression.

.. function:: split(string, [chars, [limit,] [include_empty]])

   Return an array of strings by splitting the given string on occurrences of the given character delimiters, which may be specified in any of the formats allowed by ``search``'s second argument (i.e. a single character, collection of characters, string, or regular expression). If ``chars`` is omitted, it defaults to the set of all space characters, and ``include_empty`` is taken to be false. The last two arguments are also optional: they are are a maximum size for the result and a flag determining whether empty fields should be included in the result.

.. function:: strip(string, [chars])

   Return ``string`` with any leading and trailing whitespace removed. If a string ``chars`` is provided, instead remove characters contained in that string.

.. function:: lstrip(string, [chars])

   Return ``string`` with any leading whitespace removed. If a string ``chars`` is provided, instead remove characters contained in that string.

.. function:: rstrip(string, [chars])

   Return ``string`` with any trailing whitespace removed. If a string ``chars`` is provided, instead remove characters contained in that string.

.. function:: begins_with(string, prefix)

   Returns ``true`` if ``string`` starts with ``prefix``.

.. function:: ends_with(string, suffix)

   Returns ``true`` if ``string`` ends with ``suffix``.

.. function:: uppercase(string)

   Returns ``string`` with all characters converted to uppercase.

.. function:: lowercase(string)

   Returns ``string`` with all characters converted to lowercase.

.. function:: join(strings, delim)

   Join an array of strings into a single string, inserting the given delimiter between adjacent strings.

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

   Get the next valid string index after ``i``. Returns ``endof(str)+1`` at
   the end of the string.

.. function:: prevind(str, i)

   Get the previous valid string index before ``i``. Returns ``0`` at
   the beginning of the string.

.. function:: thisind(str, i)

   Adjust ``i`` downwards until it reaches a valid index for the given string.

.. function:: randstring(len)

   Create a random ASCII string of length ``len``, consisting of upper- and lower-case letters and the digits 0-9

.. function:: charwidth(c)

   Gives the number of columns needed to print a character.

.. function:: strwidth(s)

   Gives the number of columns needed to print a string.

.. function:: isalnum(c::Char)

   Tests whether a character is alphanumeric.

.. function:: isalpha(c::Char)

   Tests whether a character is alphabetic.

.. function:: isascii(c::Char)

   Tests whether a character belongs to the ASCII character set.

.. function:: isblank(c::Char)

   Tests whether a character is a tab or space.

.. function:: iscntrl(c::Char)

   Tests whether a character is a control character.

.. function:: isdigit(c::Char)

   Tests whether a character is a numeric digit (0-9).

.. function:: isgraph(c::Char)

   Tests whether a character is printable, and not a space.

.. function:: islower(c::Char)

   Tests whether a character is a lowercase letter.

.. function:: isprint(c::Char)

   Tests whether a character is printable, including space.

.. function:: ispunct(c::Char)

   Tests whether a character is printable, and not a space or alphanumeric.

.. function:: isspace(c::Char)

   Tests whether a character is any whitespace character.

.. function:: isupper(c::Char)

   Tests whether a character is an uppercase letter.

.. function:: isxdigit(c::Char)

   Tests whether a character is a valid hexadecimal digit.

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


.. function:: open(file_name) -> IOStream

   Open a file in read mode.

.. function:: open(f::function, args...)

   Apply the function ``f`` to the result of ``open(args...)`` and close the resulting file descriptor upon completion.

   **Example**: ``open(readall, "file.txt")``

.. function:: memio([size[, finalize::Bool]]) -> IOStream

   Create an in-memory I/O stream, optionally specifying how much initial space is needed.

.. function:: fdio(fd::Integer, [own::Bool]) -> IOStream
              fdio(name::String, fd::Integer, [own::Bool]]) -> IOStream

   Create an ``IOStream`` object from an integer file descriptor. If ``own`` is true, closing this object will close the underlying descriptor. By default, an ``IOStream`` is closed when it is garbage collected. ``name`` allows you to associate the descriptor with a named file.

.. function:: flush(stream)

   Commit all currently buffered writes to the given stream.

.. function:: close(stream)

   Close an I/O stream. Performs a ``flush`` first.

.. function:: write(stream, x)

   Write the canonical binary representation of a value to the given stream.

.. function:: read(stream, type)

   Read a value of the given type from a stream, in canonical binary representation.

.. function:: read(stream, type, dims)

   Read a series of values of the given type from a stream, in canonical binary representation. ``dims`` is either a tuple or a series of integer arguments specifying the size of ``Array`` to return.

.. function:: position(s)

   Get the current position of a stream.

.. function:: seek(s, pos)

   Seek a stream to the given position.

.. function:: seek_end(s)

   Seek a stream to the end.

.. function:: skip(s, offset)

   Seek a stream relative to the current position.

.. function:: eof(stream)

   Tests whether an I/O stream is at end-of-file. If the stream is not yet exhausted, this function will block to wait for more data if necessary, and then return ``false``. Therefore it is always safe to read one byte after seeing ``eof`` return ``false``.

Text I/O
--------

.. function:: show(x)

   Write an informative text representation of a value to the current output stream. New types should overload ``show(io, x)`` where the first argument is a stream.

.. function:: print(x)

   Write (to the default output stream) a canonical (un-decorated) text representation of a value if there is one, otherwise call ``show``.

.. function:: println(x)

   Print (using :func:`print`) ``x`` followed by a newline

.. function:: showall(x)

   Show x, printing all elements of arrays

.. function:: dump(x)

   Write a thorough text representation of a value to the current output stream.

.. function:: readall(stream)

   Read the entire contents of an I/O stream as a string.

.. function:: readline(stream)

   Read a single line of text, including a trailing newline character (if one is reached before the end of the input).

.. function:: readuntil(stream, delim)

   Read a string, up to and including the given delimiter byte.

.. function:: readlines(stream)

   Read all lines as an array.

.. function:: each_line(stream)

   Create an iterable object that will yield each line from a stream.

.. function:: readdlm(filename, delim::Char)

   Read a matrix from a text file where each line gives one row, with elements separated by the given delimeter. If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: readdlm(filename, delim::Char, T::Type)

   Read a matrix from a text file with a given element type. If ``T`` is a numeric type, the result is an array of that type, with any non-numeric elements as ``NaN`` for floating-point types, or zero. Other useful values of ``T`` include ``ASCIIString``, ``String``, and ``Any``.

.. function:: writedlm(filename, array, delim::Char)

   Write an array to a text file using the given delimeter (defaults to comma).

.. function:: readcsv(filename, [T::Type])

   Equivalent to ``readdlm`` with ``delim`` set to comma.

.. function:: writecsv(filename, array)

   Equivalent to ``writedlm`` with ``delim`` set to comma.

Memory-mapped I/O
-----------------

.. function:: mmap_array(type, dims, stream, [offset])

   Create an array whose values are linked to a file, using memory-mapping. This provides a convenient way of working with data too large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted (no format conversions are possible), and dims is a tuple containing the size of the array.  

   The file is specified via the stream.  When you initialize the stream, use "r" for a "read-only" array, and "w+" to create a new array used to write values to disk. Optionally, you can specify an offset (in bytes) if, for example, you want to skip over a header in the file.

   **Example**:  A = mmap_array(Int64, (25,30000), s)

   This would create a 25-by-30000 array of Int64s, linked to the file associated with stream s.

.. function:: msync(array)

   Forces synchronization between the in-memory version of a memory-mapped array and the on-disk version. You may not need to call this function, because synchronization is performed at intervals automatically by the operating system. Hower, you can call this directly if, for example, you are concerned about losing the result of a long-running calculation.

.. function:: mmap(len, prot, flags, fd, offset)

   Low-level interface to the mmap system call. See the man page.

.. function:: munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With mmap_array you do not need to call this directly; the memory is unmapped for you when the array goes out of scope.

Standard Numeric Types
----------------------

``Bool`` ``Int8`` ``Uint8`` ``Int16`` ``Uint16`` ``Int32`` ``Uint32`` ``Int64`` ``Uint64`` ``Float32`` ``Float64`` ``Complex64`` ``Complex128``

Mathematical Functions
----------------------

.. function:: -(x)

   Unary minus operator.

.. function:: +(x, y)

   Binary addition operator.

.. function:: -(x, y)

   Binary subtraction operator.

.. function:: *(x, y)

   Binary multiplication operator.

.. function:: /(x, y)

   Binary left-division operator.

.. function:: \\(x, y)

   Binary right-division operator.

.. function:: ^(x, y)

   Binary exponentiation operator.

.. function:: .+(x, y)

   Element-wise binary addition operator.

.. function:: .-(x, y)

   Element-wise binary subtraction operator.

.. function:: .*(x, y)

   Element-wise binary multiplication operator.

.. function:: ./(x, y)

   Element-wise binary left division operator.

.. function:: .\\(x, y)

   Element-wise binary right division operator.

.. function:: .^(x, y)

   Element-wise binary exponentiation operator.

.. function:: div(a,b)

   Compute a/b, truncating to an integer

.. function:: fld(a,b)

   Largest integer less than or equal to a/b

.. function:: mod(x,m)

   Modulus after division, returning in the range [0,m)

.. function:: rem(x, m)

   Remainder after division

.. function:: %(x, m)

   Remainder after division. The operator form of ``rem``.

.. function:: mod1(x,m)

   Modulus after division, returning in the range (0,m]

.. function:: //(num, den)

   Rational division

.. function:: num(x)

   Numerator of the rational representation of ``x``

.. function:: den(x)

   Denominator of the rational representation of ``x``

.. function:: <<(x, n)

   Left shift operator.

.. function:: >>(x, n)

   Right shift operator.

.. function:: ==(x, y)

   Equality comparison operator.

.. function:: !=(x, y)

   Not-equals comparison operator.

.. function:: <(x, y)

   Less-than comparison operator.

.. function:: <=(x, y)

   Less-than-or-equals comparison operator.

.. function:: >(x, y)

   Greater-than comparison operator.

.. function:: >=(x, y)

   Greater-than-or-equals comparison operator.

.. function:: cmp(x,y)

   Return -1, 0, or 1 depending on whether ``x<y``, ``x==y``, or ``x>y``, respectively

.. function:: !(x)

   Boolean not

.. function:: ~(x)

   Bitwise not

.. function:: &(x, y)

   Bitwise and

.. function:: |(x, y)

   Bitwise or

.. function:: $(x, y)

   Bitwise exclusive or

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

   Compute the inverse hyperbolic cotangent of ``x``

.. function:: asech(x)

   Compute the inverse hyperbolic secant of ``x``

.. function:: acsch(x)

   Compute the inverse hyperbolic cosecant of ``x``

.. function:: acoth(x)

   Compute the inverse hyperbolic cotangent of ``x``

.. function:: sinc(x)

   Compute :math:`\sin(\pi x) / x`

.. function:: cosc(x)

   Compute :math:`\cos(\pi x) / x`

.. function:: degrees2radians(x)

   Convert ``x`` from degrees to radians

.. function:: radians2degrees(x)

   Convert ``x`` from radians to degrees

.. function:: hypot(x, y)

   Compute the :math:`\sqrt{x^2+y^2}` without undue overflow or underflow

.. function:: log(x)
   
   Compute the natural logarithm of ``x``

.. function:: log2(x)

   Compute the natural logarithm of ``x`` to base 2

.. function:: log10(x)

   Compute the natural logarithm of ``x`` to base 10

.. function:: log1p(x)

   Accurate natural logarithm of ``1+x``

.. function:: ilogb(x) 

   Return the exponent of x, represented as a signed integer value

.. function:: frexp(val, exp)

   Return a number ``x`` such that it has a magnitude in the interval ``[1/2, 1)`` or 0,
   and val = :math:`x \times 2^{exp}`.

.. function:: exp(x)

   Compute :math:`e^x`

.. function:: exp2(x)

   Compute :math:`2^x`

.. function:: ldexp(x, n)

   Compute :math:`x \times 2^n`

.. function:: modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts of a
   number. Both parts have the same sign as the argument.

.. function:: expm1(x)

   Accurately compute :math:`e^x-1`

.. function:: square(x)

   Compute :math:`x^2`

.. function:: round(x, [digits, [base]]) -> FloatingPoint

   ``round(x)`` returns the nearest integer to ``x``. ``round(x, digits)`` rounds to the specified number of digits after the decimal place, or before if negative, e.g., ``round(pi,2)`` is ``3.14``. ``round(x, digits, base)`` rounds using a different base, defaulting to 10, e.g., ``round(pi, 3, 2)`` is ``3.125``.

.. function:: ceil(x, [digits, [base]]) -> FloatingPoint

   Returns the nearest integer not less than ``x``. ``digits`` and ``base`` work as above.

.. function:: floor(x, [digits, [base]]) -> FloatingPoint

   Returns the nearest integer not greater than ``x``. ``digits`` and ``base`` work as above.

.. function:: trunc(x, [digits, [base]]) -> FloatingPoint

   Returns the nearest integer not greater in magnitude than ``x``. ``digits`` and ``base`` work as above.

.. function:: iround(x) -> Integer

   Returns the nearest integer to ``x``.

.. function:: iceil(x) -> Integer

   Returns the nearest integer not less than ``x``.

.. function:: ifloor(x) -> Integer

   Returns the nearest integer not greater than ``x``.

.. function:: itrunc(x) -> Integer

   Returns the nearest integer not greater in magnitude than ``x``.

.. function:: signif(x, digits, [base]) -> FloatingPoint

   Rounds (in the sense of ``round``) ``x`` so that there are ``digits`` significant digits, under a base ``base`` representation, default 10. E.g., ``signif(123.456, 2)`` is ``120.0``, and ``signif(357.913, 4, 2)`` is ``352.0``. 

.. function:: min(x, y)

   Return the minimum of ``x`` and ``y``

.. function:: max(x, y)

   Return the maximum of ``x`` and ``y``

.. function:: clamp(x, lo, hi)

   Return x if ``lo <= x <= y``. If ``x < lo``, return ``lo``. If ``x > hi``, return ``hi``.

.. function:: abs(x)

   Absolute value of ``x``

.. function:: abs2(x)

   Squared absolute value of ``x``

.. function:: copysign(x, y)

   Return ``x`` such that it has the same sign as ``y``

.. function:: sign(x)

   Return ``+1`` if ``x`` is positive, ``0`` if ``x == 0``, and ``-1`` if ``x`` is negative.

.. function:: signbit(x)

   Returns ``1`` if the value of the sign of ``x`` is negative, otherwise ``0``.

.. function:: flipsign(x, y)

   Return ``x`` with its sign flipped if ``y`` is negative. For example ``abs(x) = flipsign(x,x)``.

.. function:: sqrt(x)
   
   Return :math:`\sqrt{x}`

.. function:: cbrt(x)

   Return :math:`x^{1/3}`

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

   Return ``cos(z) + i*sin(z)`` if z is real. Return ``(cos(real(z)) + i*sin(real(z)))/exp(imag(z))`` if ``z`` is complex

.. function:: binomial(n,k)

   Number of ways to choose ``k`` out of ``n`` items

.. function:: factorial(n)

   Factorial of n

.. function:: factorial(n,k)

   Compute ``factorial(n)/factorial(k)``

.. function:: factor(n)

   Compute the prime factorization of an integer ``n``. Returns a dictionary. The keys of the dictionary correspond to the factors, and hence are of the same type as ``n``. The value associated with each key indicates the number of times the factor appears in the factorization.

   **Example**: :math:`100=2*2*5*5`; then, ``factor(100) -> [5=>2,2=>2]``

.. function:: gcd(x,y)

   Greatest common divisor

.. function:: lcm(x,y)

   Least common multiple

.. function:: gcdx(x,y)

   Greatest common divisor, also returning integer coefficients ``u`` and ``v`` that solve ``ux+vy == gcd(x,y)``

.. function:: ispow2(n)

   Test whether ``n`` is a power of two

.. function:: nextpow2(n)

   Next power of two not less than ``n``

.. function:: prevpow2(n)

   Previous power of two not greater than ``n``

.. function:: nextpow(a, n)

   Next power of ``a`` not less than ``n``

.. function:: prevpow(a, n)

   Previous power of ``a`` not greater than ``n``

.. function:: nextprod([a,b,c], n)

   Next integer not less than ``n`` that can be written ``a^i1 * b^i2 * c^i3`` for integers ``i1``, ``i2``, ``i3``.

.. function:: prevprod([a,b,c], n)

   Previous integer not greater than ``n`` that can be written ``a^i1 * b^i2 * c^i3`` for integers ``i1``, ``i2``, ``i3``.

.. function:: invmod(x,m)

   Inverse of ``x``, modulo ``m``

.. function:: powermod(x, p, m)

   Compute ``mod(x^p, m)``

.. function:: gamma(x)

   Compute the gamma function of ``x``

.. function:: lgamma(x)

   Compute the logarithm of ``gamma(x)``

.. function:: lfact(x)

   Compute the logarithmic factorial of ``x``

.. function:: digamma(x)

   Compute the digamma function of ``x`` (the logarithmic derivative of ``gamma(x)``)

.. function:: airy(x)
              airyai(x)

   Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyprime(x)
              airyaiprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airybi(x)

   Airy function :math:`\operatorname{Bi}(x)`.

.. function:: airybiprime(x)

   Airy function derivative :math:`\operatorname{Bi}'(x)`.

.. function:: besselj0(x)

   Bessel function of the first kind of order 0, :math:`J_0(x)`.

.. function:: besselj1(x)

   Bessel function of the first kind of order 1, :math:`J_1(x)`.

.. function:: besselj(nu, x)

   Bessel function of the first kind of order ``nu``, :math:`J_\nu(x)`.

.. function:: bessely0(x)

   Bessel function of the second kind of order 0, :math:`Y_0(x)`.

.. function:: bessely1(x)

   Bessel function of the second kind of order 1, :math:`Y_1(x)`.

.. function:: bessely(nu, x)

   Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x)`.

.. function:: hankelh1(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x)`.

.. function:: hankelh2(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x)`.

.. function:: besseli(nu, x)

   Modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x)`.

.. function:: besselk(nu, x)

   Modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x)`.

.. function:: beta(x, y)

   Euler integral of the first kind :math:`\operatorname{B}(x,y) = \Gamma(x)\Gamma(y)/\Gamma(x+y)`.

.. function:: lbeta(x, y)

   Natural logarithm of the beta function :math:`\log(\operatorname{B}(x,y))`.

.. function:: eta(x)

   Dirichlet eta function :math:`\eta(s) = \sum^\infty_{n=1}(-)^{n-1}/n^{s}`.

.. function:: zeta(x)

   Riemann zeta function :math:`\zeta(s)`.

.. function:: bitmix(x, y)

   Hash two integers into a single integer. Useful for constructing hash
   functions.

.. function:: ndigits(n, b)

   Compute the number of digits in number ``n`` written in base ``b``.

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

.. function:: base(b, n, [pad])

   Convert an integer to a string in the given base, optionally specifying a number of digits to pad to.

.. function:: bits(n)

   A string giving the literal bit representation of a number.

.. function:: parse_int(type, str, [base])

   Parse a string as an integer in the given base (default 10), yielding a number of the specified type.

.. function:: parse_bin(type, str)

   Parse a string as an integer in base 2, yielding a number of the specified type.

.. function:: parse_oct(type, str)

   Parse a string as an integer in base 8, yielding a number of the specified type.

.. function:: parse_hex(type, str)

   Parse a string as an integer in base 16, yielding a number of the specified type.

.. function:: parse_float(type, str)

   Parse a string as a decimal floating point number, yielding a number of the specified type.

.. function:: bool(x)

   Convert a number or numeric array to boolean

.. function:: isbool(x)

   Test whether number or array is boolean

.. function:: int(x)

   Convert a number or array to the default integer type on your platform. Alternatively, ``x`` can be a string, which is parsed as an integer.

.. function:: uint(x)

   Convert a number or array to the default unsigned integer type on your platform. Alternatively, ``x`` can be a string, which is parsed as an unsigned integer.

.. function:: integer(x)

   Convert a number or array to integer type. If ``x`` is already of integer type it is unchanged, otherwise it converts it to the default integer type on your platform.

.. function:: isinteger(x)

   Test whether a number or array is of integer type

.. function:: signed(x)

   Convert a number to a signed integer

.. function:: unsigned(x)

   Convert a number to an unsigned integer

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

   Convert a number or array to ``Uint8`` data type

.. function:: uint16(x)

   Convert a number or array to ``Uint16`` data type

.. function:: uint32(x)

   Convert a number or array to ``Uint32`` data type

.. function:: uint64(x)

   Convert a number or array to ``Uint64`` data type

.. function:: uint128(x)

   Convert a number or array to ``Uint128`` data type

.. function:: float32(x)

   Convert a number or array to ``Float32`` data type

.. function:: float64(x)

   Convert a number or array to ``Float64`` data type

.. function:: float(x)

   Convert a number, array, or string to a ``FloatingPoint`` data type. For numeric data, the smallest suitable ``FloatingPoint`` type is used. For strings, it converts to ``Float64``.

.. function:: significand(x)

   Extract the significand(s) (a.k.a. mantissa), in binary representation, of a floating-point number or array.
   
   For example, ``significand(15.2)/15.2 == 0.125``, and ``significand(15.2)*8 == 15.2``

.. function:: float64_valued(x::Rational)

   True if ``x`` can be losslessly represented as a ``Float64`` data type

.. function:: complex64(r,i)

   Convert to ``r+i*im`` represented as a ``Complex64`` data type

.. function:: complex128(r,i)

   Convert to ``r+i*im`` represented as a ``Complex128`` data type

.. function:: char(x)

   Convert a number or array to ``Char`` data type

.. function:: safe_char(x)

   Convert to ``Char``, checking for invalid code points

.. function:: complex(r,i)

   Convert real numbers or arrays to complex

.. function:: iscomplex(x) -> Bool

   Test whether a number or array is of a complex type

.. function:: isreal(x) -> Bool

   Test whether a number or array is of a real type

.. function:: bswap(n)

   Byte-swap an integer

.. function:: num2hex(f)

   Get a hexadecimal string of the binary representation of a floating point number

.. function:: hex2num(str)

   Convert a hexadecimal string to the floating point number it represents

Numbers
-------

.. function:: one(x)

   Get the multiplicative identity element for the type of x (x can also specify the type itself). For matrices, returns an identity matrix of the appropriate size and type.

.. function:: zero(x)

   Get the additive identity element for the type of x (x can also specify the type itself).

.. data:: pi

   The constant pi

.. function:: isdenormal(f) -> Bool

   Test whether a floating point number is denormal

.. function:: isfinite(f) -> Bool

   Test whether a number is finite

.. function:: isinf(f)

   Test whether a number is infinite

.. function:: isnan(f)

   Test whether a floating point number is not a number (NaN)

.. function:: inf(f)

   Returns infinity in the same floating point type as ``f`` (or ``f`` can by the type itself)

.. function:: nan(f)

   Returns NaN in the same floating point type as ``f`` (or ``f`` can by the type itself)

.. function:: nextfloat(f)

   Get the next floating point number in lexicographic order

.. function:: prevfloat(f) -> Float

   Get the previous floating point number in lexicographic order

.. function:: integer_valued(x)

   Test whether ``x`` is numerically equal to some integer

.. function:: real_valued(x)

   Test whether ``x`` is numerically equal to some real number

.. function:: exponent(f)

   Get the exponent of a floating-point number

.. function:: BigInt(x)

   Create an arbitrary precision integer. ``x`` may be an ``Int`` (or anything that can be converted to an ``Int``) or a ``String``. 
   The usual mathematical operators are defined for this type, and results are promoted to a ``BigInt``. 

.. function:: BigFloat(x)

   Create an arbitrary precision floating point number. ``x`` may be an ``Integer``, a ``Float64``, a ``String`` or a ``BigInt``. The 
   usual mathematical operators are defined for this type, and results are promoted to a ``BigFloat``.

Integers
~~~~~~~~

.. function:: count_ones(x::Integer) -> Integer

   Number of ones in the binary representation of ``x``.
   
   **Example**: ``count_ones(7) -> 3``

.. function:: count_zeros(x::Integer) -> Integer

   Number of zeros in the binary representation of ``x``.
   
   **Example**: ``count_zeros(int32(2 ^ 16 - 1)) -> 16``

.. function:: leading_zeros(x::Integer) -> Integer

   Number of zeros leading the binary representation of ``x``.
   
   **Example**: ``leading_zeros(int32(1)) -> 31``

.. function:: leading_ones(x::Integer) -> Integer

   Number of ones leading the binary representation of ``x``.
   
   **Example**: ``leading_ones(int32(2 ^ 32 - 2)) -> 31``

.. function:: trailing_zeros(x::Integer) -> Integer

   Number of zeros trailing the binary representation of ``x``.
   
   **Example**: ``trailing_zeros(2) -> 1``

.. function:: trailing_ones(x::Integer) -> Integer

   Number of ones trailing the binary representation of ``x``.
   
   **Example**: ``trailing_ones(3) -> 2``

.. function:: isprime(x::Integer) -> Bool

   Returns ``true`` if ``x`` is prime, and ``false`` otherwise.

   **Example**: ``isprime(3) -> true``

.. function:: isodd(x::Integer) -> Bool

   Returns ``true`` if ``x`` is odd (that is, not divisible by 2), and ``false`` otherwise.

   **Example**: ``isodd(9) -> false``

.. function:: iseven(x::Integer) -> Bool

   Returns ``true`` is ``x`` is even (that is, divisible by 2), and ``false`` otherwise.

   **Example**: ``iseven(1) -> false``


Random Numbers
--------------

Random number generateion in Julia uses the `Mersenne Twister library <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT>`_. Julia has a global RNG, which is used by default. Multiple RNGs can be plugged in using the ``AbstractRNG`` object, which can then be used to have multiple streams of random numbers. Currently, only ``MersenneTwister`` is supported.

.. function:: srand([rng], seed)

   Seed the RNG with a ``seed``, which may be an unsigned integer or a vector of unsigned integers. ``seed`` can even be a filename, in which case the seed is read from a file. If the argument ``rng`` is not provided, the default global RNG is seeded.

.. function:: MersenneTwister([seed])

   Create a ``MersenneTwister`` RNG object. Different RNG objects can have their own seeds, which may be useful for generating different streams of random numbers.

.. function:: rand()

   Generate a ``Float64`` random number in (0,1)

.. function:: rand!([rng], A)

   Populate the array A with random number generated from the specified RNG.

.. function:: rand(rng::AbstractRNG, [dims...])

   Generate a random ``Float64`` number or array of the size specified by dims, using the specified RNG object. Currently, ``MersenneTwister`` is the only available Random Number Generator (RNG), which may be seeded using srand.

.. function:: rand(dims...)

   Generate a random ``Float64`` array of the size specified by dims

.. function:: rand(Int32|Uint32|Int64|Uint64|Int128|Uint128, [dims...])

   Generate a random integer of the given type. Optionally, generate an array of random integers of the given type by specifying dims.

.. function:: rand(r, [dims...])

   Generate a random integer from ``1``:``n`` inclusive. Optionally, generate a random integer array.

.. function:: randbool([dims...])

   Generate a random boolean value. Optionally, generate an array of random boolean values.

.. function:: randbool!(A)

   Fill an array with random boolean values. A may be an ``Array`` or a ``BitArray``.   

.. function:: randn([dims...])

   Generate a normally-distributed random number with mean 0 and standard deviation 1. Optionally generate an array of normally-distributed random numbers.

Arrays
------

Basic functions
~~~~~~~~~~~~~~~

.. function:: ndims(A) -> Integer

   Returns the number of dimensions of A

.. function:: size(A)

   Returns a tuple containing the dimensions of A

.. function:: eltype(A)

   Returns the type of the elements contained in A

.. function:: length(A) -> Integer

   Returns the number of elements in A (note that this differs from MATLAB where ``length(A)`` is the largest dimension of ``A``)

.. function:: nnz(A)

   Counts the number of nonzero values in A

.. function:: scale!(A, k)

   Scale the contents of an array A with k (in-place)

.. function:: conj!(A)

   Convert an array to its complex conjugate in-place

.. function:: stride(A, k)

   Returns the distance in memory (in number of elements) between adjacent elements in dimension k

.. function:: strides(A)

   Returns a tuple of the memory strides in each dimension

Constructors
~~~~~~~~~~~~

.. function:: Array(type, dims)

   Construct an uninitialized dense array. ``dims`` may be a tuple or a series of integer arguments.

.. function:: getindex(type)

   Construct an empty 1-d array of the specified type. This is usually called with the syntax ``Type[]``. Element values can be specified using ``Type[a,b,c,...]``.

.. function:: cell(dims)

   Construct an uninitialized cell array (heterogeneous array). ``dims`` can be either a tuple or a series of integer arguments.  
.. function:: zeros(type, dims)

   Create an array of all zeros of specified type

.. function:: ones(type, dims)

   Create an array of all ones of specified type

.. function:: trues(dims)

   Create a Bool array with all values set to true

.. function:: falses(dims)

   Create a Bool array with all values set to false

.. function:: fill(v, dims)

   Create an array filled with ``v``

.. function:: fill!(A, x)

   Fill array ``A`` with value ``x``

.. function:: reshape(A, dims)

   Create an array with the same data as the given array, but with different dimensions. An implementation for a particular type of array may choose whether the data is copied or shared.

.. function:: copy(A)

   Create a copy of ``A``

.. function:: similar(array, element_type, dims)

   Create an uninitialized array of the same type as the given array, but with the specified element type and dimensions. The second and third arguments are both optional. The ``dims`` argument may be a tuple or a series of integer arguments.

.. function:: reinterpret(type, A)

   Construct an array with the same binary data as the given array, but with the specified element type

.. function:: rand(dims)

   Create a random array with Float64 random values in (0,1)

.. function:: randf(dims)

   Create a random array with Float32 random values in (0,1)

.. function:: randn(dims)

   Create a random array with Float64 normally-distributed random values with a mean of 0 and standard deviation of 1

.. function:: eye(n)

   n-by-n identity matrix

.. function:: eye(m, n)

   m-by-n identity matrix

.. function:: linspace(start, stop, n)

   Construct a vector of ``n`` linearly-spaced elements from ``start`` to ``stop``.

.. function:: logspace(start, stop, n)

   Construct a vector of ``n`` logarithmically-spaced numbers from ``10^start`` to ``10^stop``.

Mathematical operators and functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All mathematical operations and functions are supported for arrays

.. function:: bsxfun(fn, A, B[, C...])

   Apply binary function ``fn`` to two or more arrays, with singleton dimensions expanded.

Indexing, Assignment, and Concatenation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. function:: getindex(A, ind)

   Returns a subset of ``A`` as specified by ``ind``, which may be an ``Int``, a ``Range``, or a ``Vector``.

.. function:: sub(A, ind)

   Returns a SubArray, which stores the input ``A`` and ``ind`` rather than computing the result immediately. Calling ``getindex`` on a SubArray computes the indices on the fly.

.. function:: slicedim(A, d, i)

   Return all the data of ``A`` where the index for dimension ``d`` equals ``i``. Equivalent to ``A[:,:,...,i,:,:,...]`` where ``i`` is in position ``d``.

.. function:: setindex!(A, X, ind)

   Store an input array ``X`` within some subset of ``A`` as specified by ``ind``.

.. function:: cat(dim, A...)

   Concatenate the input arrays along the specified dimension

.. function:: vcat(A...)

   Concatenate along dimension 1

.. function:: hcat(A...)

   Concatenate along dimension 2

.. function:: hvcat

   Horizontal and vertical concatenation in one call

.. function:: flipdim(A, d)

   Reverse ``A`` in dimension ``d``.

.. function:: flipud(A)

   Equivalent to ``flipdim(A,1)``.

.. function:: fliplr(A)

   Equivalent to ``flipdim(A,2)``.

.. function:: circshift(A,shifts)

   Circularly shift the data in an array. The second argument is a vector giving the amount to shift in each dimension.

.. function:: find(A)

   Return a vector of the linear indexes of the non-zeros in ``A``.

.. function:: findn(A)

   Return a vector of indexes for each dimension giving the locations of the non-zeros in ``A``.

.. function:: nonzeros(A)

   Return a vector of the non-zero values in array ``A``.

.. function:: findfirst(A)

   Return the index of the first non-zero value in ``A``.

.. function:: findfirst(A,v)

   Return the index of the first element equal to ``v`` in ``A``.

.. function:: findfirst(predicate, A)

   Return the index of the first element that satisfies the given predicate in ``A``.

.. function:: permutedims(A,perm)

   Permute the dimensions of array ``A``. ``perm`` is a vector specifying a permutation of length ``ndims(A)``. This is a generalization of transpose for multi-dimensional arrays. Transpose is equivalent to ``permute(A,[2,1])``.

.. function:: ipermutedims(A,perm)

   Like :func:`permutedims`, except the inverse of the given permutation is applied.

.. function:: squeeze(A, dims)

   Remove the dimensions specified by ``dims`` from array ``A``

.. function:: vec(Array) -> Vector

   Vectorize an array using column-major convention.

Array functions
~~~~~~~~~~~~~~~

.. function:: cumprod(A, [dim])

   Cumulative product along a dimension.

.. function:: cumsum(A, [dim])

   Cumulative sum along a dimension.

.. function:: cumsum_kbn(A, [dim])

   Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier compensated summation algorithm for additional accuracy.

.. function:: cummin(A, [dim])

   Cumulative minimum along a dimension.

.. function:: cummax(A, [dim])

   Cumulative maximum along a dimension.

.. function:: diff(A, [dim])

   Finite difference operator of matrix or vector.

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

.. function:: sum_kbn(A)

   Returns the sum of all array elements, using the Kahan-Babuska-Neumaier compensated summation algorithm for additional accuracy.

Sparse Matrices
---------------

Sparse matrices support much of the same set of operations as dense matrices. The following functions are specific to sparse matrices.

.. function:: sparse(I,J,V,[m,n,combine])

   Create a sparse matrix ``S`` of dimensions ``m x n`` such that ``S[I[k], J[k]] = V[k]``. The ``combine`` function is used to combine duplicates. If ``m`` and ``n`` are not specified, they are set to ``max(I)`` and ``max(J)`` respectively. If the ``combine`` function is not supplied, duplicates are added by default.

.. function:: sparsevec(I, V, [m, combine])

   Create a sparse matrix ``S`` of size ``m x 1`` such that ``S[I[k]] = V[k]``. Duplicates are combined using the ``combine`` function, which defaults to `+` if it is not provided. In julia, sparse vectors are really just sparse matrices with one column. Given Julia's Compressed Sparse Columns (CSC) storage format, a sparse column matrix with one column is sparse, whereas a sparse row matrix with one row ends up being dense.

.. function:: sparsevec(D::Dict, [m])

   Create a sparse matrix of size ``m x 1`` where the row values are keys from the dictionary, and the nonzero values are the values from the dictionary.

.. function:: issparse(S)

   Returns ``true`` if ``S`` is sparse, and ``false`` otherwise.

.. function:: nnz(S)

   Return the number of nonzeros in ``S``.

.. function:: sparse(A)

   Convert a dense matrix ``A`` into a sparse matrix.

.. function:: sparsevec(A)

   Convert a dense vector ``A`` into a sparse matrix of size ``m x 1``. In julia, sparse vectors are really just sparse matrices with one column.

.. function:: dense(S)

   Convert a sparse matrix ``S`` into a dense matrix.   

.. function:: full(S)

   Convert a sparse matrix ``S`` into a dense matrix.   

.. function:: spzeros(m,n)

   Create an empty sparse matrix of size ``m x n``.

.. function:: speye(type,m[,n])

   Create a sparse identity matrix of specified type of size ``m x m``. In case ``n`` is supplied, create a sparse identity matrix of size ``m x n``.

.. function:: spones(S)

   Create a sparse matrix with the same structure as that of ``S``, but with every nonzero element having the value ``1.0``.

.. function:: sprand(m,n,density[,rng])

   Create a random sparse matrix with the specified density. Nonzeros are sampled from the distribution specified by ``rng``. The uniform distribution is used in case ``rng`` is not specified.

.. function:: sprandn(m,n,density)

   Create a random sparse matrix of specified density with nonzeros sampled from the normal distribution.

.. function:: sprandbool(m,n,density)

   Create a random sparse boolean matrix with the specified density.


Linear Algebra
--------------

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.

.. function:: *(A, B)

   Matrix multiplication

.. function:: \\(A, B)

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B``. For rectangular ``A``, QR factorization is used. For triangular ``A``, a triangular solve is performed. For square ``A``, Cholesky factorization is tried if the input is symmetric with a heavy diagonal. LU factorization is used in case Cholesky factorization fails or for general square inputs. If ``size(A,1) > size(A,2)``, the result is a least squares solution of ``A*X+eps=B`` using the singular value decomposition. ``A`` does not need to have full rank.

.. function:: dot(x, y)

   Compute the dot product

.. function:: cross(x, y)

   Compute the cross product of two 3-vectors

.. function:: norm(a)

   Compute the norm of a ``Vector`` or a ``Matrix``

.. function:: factors(F)

   Return the factors of a factorization ``F``. For example, in the case of an LU decomposition, factors(LU) -> L, U, P

.. function:: lu(A) -> L, U, P

   Compute the LU factorization of ``A``, such that ``A[P,:] = L*U``.

.. function:: lufact(A) -> LUDense

   Compute the LU factorization of ``A`` and return a ``LUDense`` object. ``factors(lufact(A))`` returns the triangular matrices containing the factorization. The following functions are available for ``LUDense`` objects: ``size``, ``factors``, ``\``, ``inv``, ``det``.

.. function:: lufact!(A) -> LUDense

   ``lufact!`` is the same as ``lufact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: chol(A, [LU]) -> F

   Compute Cholesky factorization of a symmetric positive-definite matrix ``A`` and return the matrix ``F``. If ``LU`` is ``L`` (Lower), ``A = L*L'``. If ``LU`` is ``U`` (Upper), ``A = R'*R``.

.. function:: cholfact(A, [LU]) -> CholeskyDense

   Compute the Cholesky factorization of a symmetric positive-definite matrix ``A`` and return a ``CholeskyDense`` object. ``LU`` may be 'L' for using the lower part or 'U' for the upper part. The default is to use 'U'. ``factors(cholfact(A))`` returns the triangular matrix containing the factorization. The following functions are available for ``CholeskyDense`` objects: ``size``, ``factors``, ``\``, ``inv``, ``det``. A ``LAPACK.PosDefException`` error is thrown in case the matrix is not positive definite.

.. function: cholfact!(A, [LU]) -> CholeskyDense

   ``cholfact!`` is the same as ``cholfact`` but saves space by overwriting the input A, instead of creating a copy.

..  function:: cholpfact(A, [LU]) -> CholeskyPivotedDense

   Compute the pivoted Cholesky factorization of a symmetric positive semi-definite matrix ``A`` and return a ``CholeskyDensePivoted`` object. ``LU`` may be 'L' for using the lower part or 'U' for the upper part. The default is to use 'U'. ``factors(cholpfact(A))`` returns the triangular matrix containing the factorization. The following functions are available for ``CholeskyDensePivoted`` objects: ``size``, ``factors``, ``\``, ``inv``, ``det``. A ``LAPACK.RankDeficientException`` error is thrown in case the matrix is rank deficient.

.. function:: cholpfact!(A, [LU]) -> CholeskyPivotedDense

   ``cholpfact!`` is the same as ``cholpfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: qr(A) -> Q, R

   Compute the QR factorization of ``A`` such that ``A = Q*R``. Also see ``qrd``.

.. function:: qrfact(A)

   Compute the QR factorization of ``A`` and return a ``QRDense`` object. ``factors(qrfact(A))`` returns ``Q`` and ``R``. The following functions are available for ``QRDense`` objects: ``size``, ``factors``, ``qmulQR``, ``qTmulQR``, ``\``. 

.. function:: qrfact!(A)

   ``qrfact!`` is the same as ``qrfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: qrp(A) -> Q, R, P

   Compute the QR factorization of ``A`` with pivoting, such that ``A*I[:,P] = Q*R``, where ``I`` is the identity matrix. Also see ``qrpfact``.

.. function:: qrpfact(A) -> QRPivotedDense

   Compute the QR factorization of ``A`` with pivoting and return a ``QRDensePivoted`` object. ``factors(qrpfact(A))`` returns ``Q`` and ``R``. The following functions are available for ``QRDensePivoted`` objects: ``size``, ``factors``, ``qmulQR``, ``qTmulQR``, ``\``. 

.. function:: qrpfact!(A) -> QRPivotedDense

   ``qrpfact!`` is the same as ``qrpfact`` but saves space by overwriting the input A, instead of creating a copy.

.. function:: qmulQR(QR, A)
   
   Perform ``Q*A`` efficiently, where Q is a an orthogonal matrix defined as the product of k elementary reflectors from the QR decomposition.

.. function:: qTmulQR(QR, A)

   Perform ``Q'*A`` efficiently, where Q is a an orthogonal matrix defined as the product of k elementary reflectors from the QR decomposition.

.. function:: sqrtm(A)

   Compute the matrix square root of ``A``. If ``B = sqrtm(A)``, then ``B*B == A`` within roundoff error.

.. function:: eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

.. function:: eigvals(A)

   Returns the eigenvalues of ``A``.

.. function:: svdfact(A, [thin]) -> SVDDense

   Compute the Singular Value Decomposition (SVD) of ``A`` and return an ``SVDDense`` object. ``factors(svdfact(A))`` returns ``U``, ``S``, and ``Vt``, such that ``A = U*diagm(S)*Vt``. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svdfact!(A, [thin]) -> SVDDense

   ``svdfact!`` is the same as ``svdfact`` but saves space by overwriting the input A, instead of creating a copy. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svd(A, [thin]) -> U, S, V

   Compute the SVD of A, returning ``U``, vector ``S``, and ``V`` such that ``A == U*diagm(S)*V'``. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svdt(A, [thin]) -> U, S, Vt

   Compute the SVD of A, returning ``U``, vector ``S``, and ``Vt`` such that ``A = U*diagm(S)*Vt``. If ``thin`` is ``true``, an economy mode decomposition is returned.

.. function:: svdvals(A)

   Returns the singular values of ``A``.

.. function:: svdvals!(A)

   Returns the singular values of ``A``, while saving space by overwriting the input.

.. function:: svdfact(A, B) -> GSVDDense

   Compute the generalized SVD of ``A`` and ``B``, returning a ``GSVDDense`` Factorization object. ``factors(svdfact(A,b))`` returns ``U``, ``V``, ``Q``, ``D1``, ``D2``, and ``R0`` such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.
   
.. function:: svd(A, B) -> U, V, Q, D1, D2, R0

   Compute the generalized SVD of ``A`` and ``B``, returning ``U``, ``V``, ``Q``, ``D1``, ``D2``, and ``R0`` such that ``A = U*D1*R0*Q'`` and ``B = V*D2*R0*Q'``.
 
.. function:: svdvals(A, B)

   Return only the singular values from the generalized singular value decomposition of ``A`` and ``B``.

.. function:: triu(M)

   Upper triangle of a matrix

.. function:: tril(M)

   Lower triangle of a matrix

.. function:: diag(M, [k])

   The ``k``-th diagonal of a matrix, as a vector

.. function:: diagm(v, [k])

   Construct a diagonal matrix and place ``v`` on the ``k``-th diagonal

.. function:: diagmm(matrix, vector)

   Multiply matrices, interpreting the vector argument as a diagonal matrix.
   The arguments may occur in the other order to multiply with the diagonal
   matrix on the left.

.. function:: Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal, and upper diagonal

.. function:: Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury matrix identity

.. function:: rank(M)

   Compute the rank of a matrix

.. function:: norm(A, [p])

   Compute the ``p``-norm of a vector or a matrix. ``p`` is ``2`` by default, if not provided. If ``A`` is a vector, ``norm(A, p)`` computes the ``p``-norm. ``norm(A, Inf)`` returns the largest value in ``abs(A)``, whereas ``norm(A, -Inf)`` returns the smallest. If ``A`` is a matrix, valid values for ``p`` are ``1``, ``2``, or ``Inf``. In order to compute the Frobenius norm, use ``normfro``.

.. function:: normfro(A)

   Compute the Frobenius norm of a matrix ``A``.

.. function:: cond(M, [p])

   Matrix condition number, computed using the p-norm. ``p`` is 2 by default, if not provided. Valid values for ``p`` are ``1``, ``2``, or ``Inf``.

.. function:: trace(M)

   Matrix trace

.. function:: det(M)

   Matrix determinant

.. function:: inv(M)

   Matrix inverse

.. function:: pinv(M)

   Moore-Penrose inverse

.. function:: null(M)

   Basis for null space of M.

.. function:: repmat(A, n, m)

   Construct a matrix by repeating the given matrix ``n`` times in dimension 1 and ``m`` times in dimension 2.

.. function:: kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

.. function:: linreg(x, y)

   Determine parameters ``[a, b]`` that minimize the squared error between ``y`` and ``a+b*x``.

.. function:: linreg(x, y, w)

   Weighted least-squares linear regression.

.. function:: expm(A)

   Matrix exponential.

.. function:: issym(A)

   Test whether a matrix is symmetric.

.. function:: isposdef(A)

   Test whether a matrix is positive-definite.

.. function:: istril(A)

   Test whether a matrix is lower-triangular.

.. function:: istriu(A)

   Test whether a matrix is upper-triangular.

.. function:: ishermitian(A)

   Test whether a matrix is hermitian.

.. function:: transpose(A)

   The transpose operator (.').

.. function:: ctranspose(A)

   The conjugate transpose operator (').

Combinatorics
-------------

.. function:: nthperm(v, k)

   Compute the kth lexicographic permutation of a vector.

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

   Randomly rearrange the elements of a vector.

.. function:: shuffle!(v)

   In-place version of :func:`shuffle`.

.. function:: reverse(v)

   Reverse vector ``v``.

.. function:: reverse!(v) -> v

   In-place version of :func:`reverse`.

.. function:: combinations(array, n)

   Generate all combinations of ``n`` elements from a given array. Because
   the number of combinations can be very large, this function runs inside
   a Task to produce values on demand. Write ``c = @task combinations(a,n)``,
   then iterate ``c`` or call ``consume`` on it.

.. function:: integer_partitions(n, m)

   Generate all arrays of ``m`` integers that sum to ``n``. Because
   the number of partitions can be very large, this function runs inside
   a Task to produce values on demand. Write
   ``c = @task integer_partitions(n,m)``, then iterate ``c`` or call
   ``consume`` on it.

.. function:: partitions(array)

   Generate all set partitions of the elements of an array, represented as
   arrays of arrays. Because the number of partitions can be very large, this
   function runs inside a Task to produce values on demand. Write
   ``c = @task partitions(a)``, then iterate ``c`` or call ``consume`` on it.

Statistics
----------

.. function:: mean(v, [dim])

   Compute the mean of whole array ``v``, or optionally along dimension ``dim``

.. function:: std(v, [corrected])

   Compute the sample standard deviation of a vector ``v``. If the optional argument ``corrected`` is either left unspecified or is explicitly set to the default value of ``true``, then the algorithm will return an estimator of the generative distribution's standard deviation under the assumption that each entry of ``v`` is an IID draw from that generative distribution. This computation is equivalent to calculating ``sqrt(sum((v .- mean(v)).^2) / (length(v) - 1))`` and involves an implicit correction term sometimes called the Bessel correction which insures that the estimator of the variance is unbiased. If, instead, the optional argument ``corrected`` is set to ``false``, then the algorithm will produce the equivalent of ``sqrt(sum((v .- mean(v)).^2) / length(v))``, which is the empirical standard deviation of the sample.

.. function:: std(v, m, [corrected])

   Compute the sample standard deviation of a vector ``v`` with known mean ``m``. If the optional argument ``corrected`` is either left unspecified or is explicitly set to the default value of ``true``, then the algorithm will return an estimator of the generative distribution's standard deviation under the assumption that each entry of ``v`` is an IID draw from that generative distribution. This computation is equivalent to calculating ``sqrt(sum((v .- m).^2) / (length(v) - 1))`` and involves an implicit correction term sometimes called the Bessel correction which insures that the estimator of the variance is unbiased. If, instead, the optional argument ``corrected`` is set to ``false``, then the algorithm will produce the equivalent of ``sqrt(sum((v .- m).^2) / length(v))``, which is the empirical standard deviation of the sample.

.. function:: var(v, [corrected])

   Compute the sample variance of a vector ``v``. If the optional argument ``corrected`` is either left unspecified or is explicitly set to the default value of ``true``, then the algorithm will return an unbiased estimator of the generative distribution's variance under the assumption that each entry of ``v`` is an IID draw from that generative distribution. This computation is equivalent to calculating ``sum((v .- mean(v)).^2) / (length(v) - 1)`` and involves an implicit correction term sometimes called the Bessel correction. If, instead, the optional argument ``corrected`` is set to ``false``, then the algorithm will produce the equivalent of ``sum((v .- mean(v)).^2) / length(v)``, which is the empirical variance of the sample.

.. function:: var(v, m, [corrected])

   Compute the sample variance of a vector ``v`` with known mean ``m``. If the optional argument ``corrected`` is either left unspecified or is explicitly set to the default value of ``true``, then the algorithm will return an unbiased estimator of the generative distribution's variance under the assumption that each entry of ``v`` is an IID draw from that generative distribution. This computation is equivalent to calculating ``sum((v .- m)).^2) / (length(v) - 1)`` and involves an implicit correction term sometimes called the Bessel correction. If, instead, the optional argument ``corrected`` is set to ``false``, then the algorithm will produce the equivalent of ``sum((v .- m)).^2) / length(v)``, which is the empirical variance of the sample.

.. function:: median(v)

   Compute the median of a vector ``v``

.. function:: hist(v, [n])

   Compute the histogram of ``v``, optionally using ``n`` bins

.. function:: hist(v, e)

   Compute the histogram of ``v`` using a vector ``e`` as the edges for the bins

.. function:: quantile(v, p)

   Compute the quantiles of a vector ``v`` at a specified set of probability values ``p``.

.. function:: quantile(v)

   Compute the quantiles of a vector ``v`` at the probability values ``[.0, .2, .4, .6, .8, 1.0]``.

.. function:: cov(v)

   Compute the Pearson covariance between two vectors ``v1`` and ``v2``.

.. function:: cor(v)

   Compute the Pearson correlation between two vectors ``v1`` and ``v2``.

Signal Processing
-----------------

FFT functions in Julia are largely implemented by calling functions from `FFTW <http://www.fftw.org>`_

.. function:: fft(A [, dims])

   Performs a multidimensional FFT of the array ``A``.  The optional ``dims``
   argument specifies an iterable subset of dimensions (e.g. an integer,
   range, tuple, or array) to transform along.  Most efficient if the
   size of ``A`` along the transformed dimensions is a product of small
   primes; see :func:`nextprod`.  See also :func:`plan_fft` for even
   greater efficiency.

   A one-dimensional FFT computes the one-dimensional discrete Fourier
   transform (DFT) as defined by :math:`\operatorname{DFT}[k] = \sum_{n=1}^{\operatorname{length}(A)} \exp\left(-i\frac{2\pi (n-1)(k-1)}{\operatorname{length}(A)} \right) A[n]`.  A multidimensional FFT simply performs this operation
   along each transformed dimension of ``A``.

.. function:: fft!(A [, dims])

   Same as :func:`fft`, but operates in-place on ``A``,
   which must be an array of complex floating-point numbers.

.. function:: ifft(A [, dims]), bfft, bfft!

   Multidimensional inverse FFT.

   A one-dimensional backward FFT computes
   :math:`\operatorname{BDFT}[k] =
   \sum_{n=1}^{\operatorname{length}(A)} \exp\left(+i\frac{2\pi
   (n-1)(k-1)}{\operatorname{length}(A)} \right) A[n]`.  A
   multidimensional backward FFT simply performs this operation along
   each transformed dimension of ``A``.  The inverse FFT computes
   the same thing divided by the product of the transformed dimensions.

.. function:: ifft!(A [, dims])

   Same as :func:`ifft`, but operates in-place on ``A``.

.. function:: bfft(A [, dims])

   Similar to :func:`ifft`, but computes an unnormalized inverse
   (backward) transform, which must be divided by the product of the sizes
   of the transformed dimensions in order to obtain the inverse.  (This is
   slightly more efficient than :func:`ifft` because it omits a scaling
   step, which in some applications can be combined with other
   computational steps elsewhere.)

.. function:: bfft!(A [, dims])

   Same as :func:`bfft`, but operates in-place on ``A``.

.. function:: plan_fft(A [, dims [, flags [, timelimit]]]),  plan_ifft, plan_bfft

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

.. function:: plan_irfft(A, d [, dims [, flags [, timelimit]]]), plan_bfft

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

.. function:: FFTW.r2r(A, kind [, dims]), FFTW.r2r!

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
   definitions of these transform types, at `<http://www.fftw.org/doc>`.

   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along. ``kind[i]`` is then the transform type for ``dims[i]``,
   with ``kind[end]`` being used for ``i > length(kind)``.

   See also :func:`FFTW.plan_r2r` to pre-plan optimized r2r transforms.

   :func:`FFTW.r2r!` is the same as :func:`FFTW.r2r`, but operates
   in-place on ``A``, which must be an array of real or complex 
   floating-point numbers.

.. function:: FFTW.plan_r2r(A, kind [, dims [, flags [, timelimit]]]), FFTW.plan_r2r!

   Pre-plan an optimized r2r transform, similar to :func:`plan_fft`
   except that the transforms (and the first three arguments)
   correspond to :func:`FFTW.r2r` and :func:`FFTW.r2r!`, respectively.

.. function:: fftshift(x)

   Swap the first and second halves of each dimension of ``x``.

.. function:: fftshift(x,dim)

   Swap the first and second halves of the given dimension of array ``x``.

.. function:: ifftshift(x, [dim])

   Undoes the effect of ``fftshift``.

.. function:: filt(b,a,x)

   Apply filter described by vectors ``a`` and ``b`` to vector ``x``.

.. function:: deconv(b,a)

   Construct vector ``c`` such that ``b = conv(a,c) + r``. Equivalent to polynomial division.

.. function:: conv(u,v)

   Convolution of two vectors. Uses FFT algorithm.

.. function:: xcorr(u,v)

   Compute the cross-correlation of two vectors.

Parallel Computing
------------------

.. function:: addprocs_local(n)

   Add processes on the local machine. Can be used to take advantage of multiple cores.

.. function:: addprocs_ssh({"host1","host2",...})

   Add processes on remote machines via SSH. Requires julia to be installed in the same location on each node, or to be available via a shared file system.

.. function:: addprocs_sge(n)

   Add processes via the Sun/Oracle Grid Engine batch queue, using ``qsub``.

.. function:: nprocs()

   Get the number of available processors.

.. function:: myid()

   Get the id of the current processor.

.. function:: pmap(f, c)

   Transform collection ``c`` by applying ``f`` to each element in parallel.

.. function:: remote_call(id, func, args...)

   Call a function asynchronously on the given arguments on the specified processor. Returns a ``RemoteRef``.

.. function:: wait(RemoteRef)

   Wait for a value to become available for the specified remote reference.

.. function:: fetch(RemoteRef)

   Wait for and get the value of a remote reference.

.. function:: remote_call_wait(id, func, args...)

   Perform ``wait(remote_call(...))`` in one message.

.. function:: remote_call_fetch(id, func, args...)

   Perform ``fetch(remote_call(...))`` in one message.

.. function:: put(RemoteRef, value)

   Store a value to a remote reference. Implements "shared queue of length 1" semantics: if a value is already present, blocks until the value is removed with ``take``.

.. function:: take(RemoteRef)

   Fetch the value of a remote reference, removing it so that the reference is empty again.

.. function:: RemoteRef()

   Make an uninitialized remote reference on the local machine.

.. function:: RemoteRef(n)

   Make an uninitialized remote reference on processor ``n``.

Distributed Arrays
------------------

.. function:: DArray(init, dims, [procs, dist])

   Construct a distributed array. ``init`` is a function accepting a tuple of index ranges. This function should return a chunk of the distributed array for the specified indexes. ``dims`` is the overall size of the distributed array. ``procs`` optionally specifies a vector of processor IDs to use. ``dist`` is an integer vector specifying how many chunks the distributed array should be divided into in each dimension.

.. function:: dzeros(dims, ...)

   Construct a distributed array of zeros. Trailing arguments are the same as those accepted by ``darray``.

.. function:: dones(dims, ...)

   Construct a distributed array of ones. Trailing arguments are the same as those accepted by ``darray``.

.. function:: dfill(x, dims, ...)

   Construct a distributed array filled with value ``x``. Trailing arguments are the same as those accepted by ``darray``.

.. function:: drand(dims, ...)

   Construct a distributed uniform random array. Trailing arguments are the same as those accepted by ``darray``.

.. function:: drandn(dims, ...)

   Construct a distributed normal random array. Trailing arguments are the same as those accepted by ``darray``.

.. function:: distribute(a)

   Convert a local array to distributed

.. function:: localize(d)

   Get the local piece of a distributed array

.. function:: myindexes(d)

   A tuple describing the indexes owned by the local processor

.. function:: procs(d)

   Get the vector of processors storing pieces of ``d``

System
------

.. function:: run(command)

   Run a command object, constructed with backticks. Throws an error if anything goes wrong, including the process exiting with a non-zero status.

.. function:: success(command)

   Run a command object, constructed with backticks, and tell whether it was successful (exited with a code of 0).

.. function:: readsfrom(command)

   Starts running a command asynchronously, and returns a tuple (stream,process). The first value is a stream reading from the process' standard output.

.. function:: writesto(command)

   Starts running a command asynchronously, and returns a tuple (stream,process). The first value is a stream writing to the process' standard input.

.. function:: readandwrite(command)

   Starts running a command asynchronously, and returns a tuple (stdout,stdin,process) of the output stream and input stream of the process, and the process object itself.

.. function:: > < >> .>

   ``>`` ``<`` and ``>>`` work exactly as in bash, and ``.>`` redirects STDERR.

   **Example**: ``run((`ls` > "out.log") .> "err.log")``

.. function:: gethostname() -> String

   Get the local machine's host name.

.. function:: getipaddr() -> String

   Get the IP address of the local machine, as a string of the form "x.x.x.x".

.. function:: pwd() -> String

   Get the current working directory.

.. function:: cd(dir::String)

   Set the current working directory. Returns the new current directory.

.. function:: cd(f, ["dir"])

   Temporarily changes the current working directory (HOME if not specified) and applies function f before returning. 

.. function:: mkdir(path, [mode])

   Make a new directory with name ``path`` and permissions ``mode``.
   ``mode`` defaults to 0o777, modified by the current file creation mask.

.. function:: rmdir(path)

   Remove the directory named ``path``.

.. function:: getpid() -> Int32

   Get julia's process ID.

.. function:: time()

   Get the system time in seconds since the epoch, with fairly high (typically, microsecond) resolution.

.. function:: time_ns()

   Get the time in nanoseconds. The time corresponding to 0 is undefined, and wraps every 5.8 years.

.. function:: tic()

   Set a timer to be read by the next call to :func:`toc` or :func:`toq`. The macro call ``@time expr`` can also be used to time evaluation.

.. function:: toc()

   Print and return the time elapsed since the last :func:`tic`.

.. function:: toq()

   Return, but do not print, the time elapsed since the last :func:`tic`.

.. function:: EnvHash() -> EnvHash

   A singleton of this type provides a hash table interface to environment variables.

.. data:: ENV

   Reference to the singleton ``EnvHash``.

C Interface
-----------

.. function:: ccall( (symbol, library), RetType, (ArgType1, ...), ArgVar1, ...)
              ccall( fptr::Ptr{Void}, RetType, (ArgType1, ...), ArgVar1, ...)

   Call function in C-exported shared library, specified by (function name, library) tuple (String or :Symbol). Alternatively, ccall may be used to call a function pointer returned by dlsym, but note that this usage is generally discouraged to facilitate future static compilation.

.. function:: cfunction(fun::Function, RetType::Type, (ArgTypes...))
   
   Generate C-callable function pointer from Julia function.

.. function:: dlopen(libfile::String [, flags::Integer])

   Load a shared library, returning an opaque handle.

   The optional flags argument is a bitwise-or of zero or more of
   RTLD_LOCAL, RTLD_GLOBAL, RTLD_LAZY, RTLD_NOW, RTLD_NODELETE,
   RTLD_NOLOAD, RTLD_DEEPBIND, and RTLD_FIRST.  These are converted to
   the corresponding flags of the POSIX (and/or GNU libc and/or MacOS)
   dlopen command, if possible, or are ignored if the specified
   functionality is not available on the current platform.  The
   default is RTLD_LAZY|RTLD_DEEPBIND|RTLD_LOCAL.  An important usage
   of these flags, on POSIX platforms, is to specify
   RTLD_LAZY|RTLD_DEEPBIND|RTLD_GLOBAL in order for the library's
   symbols to be available for usage in other shared libraries, in
   situations where there are dependencies between shared libraries.

.. function:: dlsym(handle, sym)

   Look up a symbol from a shared library handle, return callable function pointer on success.

.. function:: dlsym_e(handle, sym)
   
   Look up a symbol from a shared library handle, silently return NULL pointer on lookup failure.

.. function:: dlclose(handle)

   Close shared library referenced by handle.

.. function:: c_free(addr::Ptr)
  
   Call free() from C standard library.

.. function:: unsafe_ref(p::Ptr{T},i::Integer)

   Dereference the pointer ``p[i]`` or ``*p``, returning a copy of type T.

.. function:: unsafe_assign(p::Ptr{T},x,i::Integer)

   Assign to the pointer ``p[i] = x`` or ``*p = x``, making a copy of object x into the memory at p.


Errors
------

.. function:: error(message::String)
              error(Exception)

   Raise an error with the given message

.. function:: throw(e)

   Throw an object as an exception

.. function:: errno()

   Get the value of the C library's ``errno``

.. function:: strerror(n)

   Convert a system call error code to a descriptive string

.. function:: assert(cond)

   Raise an error if ``cond`` is false. Also available as the macro ``@assert expr``.

Tasks
-----

.. function:: Task(func)

   Create a ``Task`` (i.e. thread, or coroutine) to execute the given function. The task exits when this function returns.

.. function:: yieldto(task, args...)

   Switch to the given task. The first time a task is switched to, the task's function is called with ``args``. On subsequent switches, ``args`` are returned from the task's last call to ``yieldto``.

.. function:: current_task()

   Get the currently running Task.

.. function:: istaskdone(task)

   Tell whether a task has exited.

.. function:: consume(task)

   Receive the next value passed to ``produce`` by the specified task.

.. function:: produce(value)

   Send the given value to the last ``consume`` call, switching to the consumer task.

.. function:: make_scheduled(task)

   Register a task with the main event loop, so it will automatically run when possible.

.. function:: yield()

   For scheduled tasks, switch back to the scheduler to allow another scheduled task to run.

.. function:: tls(symbol)

   Look up the value of a symbol in the current task's task-local storage.

.. function:: tls(symbol, value)

   Assign a value to a symbol in the current task's task-local storage.
