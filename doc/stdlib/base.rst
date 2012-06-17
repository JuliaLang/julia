
Getting Around
--------------

.. function:: exit([code])

   Quit (or control-D at the prompt). The default exit code is zero, indicating that the processes completed successfully.

.. function:: whos()

   Print information about global user-defined variables.

.. function:: edit("file"[, line])

   Edit a file optionally providing a line number to edit at. Returns to the julia prompt when you quit the editor. If the file name ends in ".jl" it is reloaded when the editor closes the file.

.. function:: edit(function[, types])

   Edit the definition of a function, optionally specifying a tuple of types to indicate which method to edit. When the editor exits, the source file containing the definition is reloaded.

.. function:: load("file")

   Evaluate the contents of a source file

All Objects
-----------

.. function:: is(x, y)

   Determine whether ``x`` and ``y`` refer to the same object in memory.

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

.. function:: uid(x)

   Get a unique integer id for ``x``. ``uid(x)==uid(y)`` if and only if ``is(x,y)``.

.. function:: hash(x)

   Compute an integer hash code such that ``isequal(x,y)`` implies ``hash(x)==hash(y)``.

.. function:: finalizer(x, function)

   Register a function to be called on ``x`` when there are no program-accessible references to ``x``. The behavior of this function is unpredictable if ``x`` is of a bits type.

.. function:: copy(x)

   Create a deep copy of ``x``: i.e. ``copy`` is called recursively on all constituent parts of ``x``. If a user-defined type should be recursively copied, a ``copy`` method should be defined for it which implements deep copying of an instance.

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

.. function:: sizeof(type)

   Size, in bytes, of the canonical binary representation of the given type, if any.

.. function:: eps([type])

   The distance between 1.0 and the next largest representable floating-point value of ``type``. The only types that are sensible arguments are ``Float32`` and ``Float64``. If ``type`` is omitted, then ``eps(Float64)`` is returned.

.. function:: eps(x)

   The distance between ``x`` and the next largest representable floating-point value of the same type as ``x``.

.. function:: promote_type(type1, type2)

   Determine a type big enough to hold values of each argument type without loss, whenever possible. In some cases, where no type exists which to which both types can be promoted losslessly, some loss is tolerated; for example, ``promote_type(Int64,Float64)`` returns ``Float64`` even though strictly, not all ``Int64`` values can be represented exactly as ``Float64`` values.

Generic Functions
-----------------

.. function:: method_exists(f, tuple)

   Determine whether the given generic function has a method matching the given tuple of argument types.

.. function:: applicable(f, args...)

   Determine whether the given generic function has a method applicable to the given arguments.

.. function:: invoke(f, (types...), args...)

   Invoke a method for the given generic function matching the specified types (as a tuple), on the specified arguments. The arguments must be compatible with the specified types. This allows invoking a method other than the most specific matching method, which is useful when the behavior of a more general definition is explicitly needed (often as part of the implementation of a more specific method of the same function).

Iteration
---------

Sequential iteration is implemented by the methods ``start``, ``done``, and ``next``. The general ``for`` loop:

::

    for i = I
      # body
    end

is translated to:

::

    state = start(I)
    while !done(I, state)
      (i, state) = next(I, state)
      # body
    end

The ``state`` object may be anything, and should be chosen appropriately for each iterable type.

.. function:: start(iter)

   Get initial iteration state for an iterable object

.. function:: done(iter, state)

   Test whether we are done iterating

.. function:: next(iter, state) -> item, state

   For a given iterable object and iteration state, return the current item and the next iteration state

Fully implemented by: ``Range``, ``Range1``, ``NDRange``, ``Tuple``, ``Real``, ``AbstractArray``, ``IntSet``, ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``, ``EachLine``, ``String``, ``Set``, ``Task``.

General Collections
-------------------

.. function:: isempty(collection)

   Determine whether a collection is empty (has no elements).

.. function:: numel(collection)

   Return the number of elements in a collection.

.. function:: length(collection)

   For ordered, indexable collections, the maximum index ``i`` for which ``ref(collection, i)`` is valid.

Fully implemented by: ``Range``, ``Range1``, ``Tuple``, ``Number``, ``AbstractArray``, ``IntSet``, ``Dict``, ``WeakKeyDict``, ``String``, ``Set``.

Partially implemented by: ``FDSet``.

Iterable Collections
--------------------

.. function:: contains(itr, x)

   Determine whether a collection contains the given value, ``x``.

.. function:: reduce(op, v0, itr)

   Reduce the given collection with the given operator, i.e. accumulate ``v = op(v,elt)`` for each element, where ``v`` starts as ``v0``. Reductions for certain commonly-used operators are available in a more convenient 1-argument form: ``max(itr)``, ``min(itr)``, ``sum(itr)``, ``prod(itr)``, ``any(itr)``, ``all(itr)``.

.. function:: max(itr)

   Determine maximum element in a collection

.. function:: min(itr)

   Determine minimum element in a collection

.. function:: sum(itr)

   Sum elements of a collection

.. function:: prod(itr)

   Multiply elements of a collection

.. function:: any(itr)

   Test whether any elements of a boolean collection are true

.. function:: all(itr)

   Test whether all elements of a boolean collection are true

.. function:: count(itr)

   Count the number of boolean elements in ``itr`` which are ``true`` rather than ``false``.

.. function:: countp(p, itr)

   Count the number of elements in ``itr`` for which predicate ``p`` is true.

.. function:: anyp(p, itr)

   Determine whether any element of ``itr`` satisfies the given predicate.

.. function:: allp(p, itr)

   Determine whether all elements of ``itr`` satisfy the given predicate.

.. function:: map(f, c)

   Transform collection ``c`` by applying ``f`` to each element

Indexable Collections
---------------------

.. function:: ref(collection, key...)
              collection[key...]

   Retrieve the value(s) stored at the given key or index within a collection.

.. function:: assign(collection, value, key...)
              collection[key...] = value

   Store the given value at the given key or index within a collection.

Fully implemented by: ``Array``, ``DArray``, ``AbstractArray``, ``SubArray``, ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``, ``String``.

Partially implemented by: ``Range``, ``Range1``, ``Tuple``.

Associative Collections
-----------------------

``Dict`` is the standard associative collection. Its implementation uses the ``hash(x)`` as the hashing function for the key, and ``isequal(x,y)`` to determine equality. Define these two functions for custom types to override how they are stored in a hash table.

``ObjectIdDict`` is a special hash table where the keys are always object identities. ``WeakKeyDict`` is a hash table implementation where the keys are weak references to objects, and thus maybe garbage collected even when referenced in a hash table.

Dicts can be created using a literal syntax: ``{"A"=>1, "B"=>2}``

.. function:: Dict{K,V}(n)

   Construct a hashtable with keys of type K and values of type V and intial size of n

.. function:: has(collection, key)

   Determine whether a collection has a mapping for a given key.

.. function:: get(collection, key, default)

   Return the value stored for the given key, or the given default value if no mapping for the key is present.

.. function:: del(collection, key)

   Delete the mapping for the given key in a collection.

.. function:: del_all(collection)

   Delete all keys from a collection.

Fully implemented by: ``ObjectIdDict``, ``Dict``, ``WeakKeyDict``.

Partially implemented by: ``IntSet``, ``Set``, ``EnvHash``, ``FDSet``, ``Array``.

Set-Like Collections
--------------------

.. function:: add(collection, key)

   Add an element to a set-like collection.

.. function:: intset(i...)

   Construct an ``IntSet`` of the given integers.

.. function:: IntSet(n)

   Construct a set for holding integers up to ``n`` (larger integers may also be added later).

.. function:: choose(s)

   Pick an element of a set

.. function:: union(s1,s2)

   Construct the union of two sets

Fully implemented by: ``IntSet``, ``Set``, ``FDSet``.

Dequeues
--------

.. function:: push(collection, item)

   Insert an item at the end of a collection.

.. function:: pop(collection)

   Remove the last item in a collection and return it.

.. function:: enqueue(collection, item)

   Insert an item at the beginning of a collection. Also called ``unshift``.

.. function:: shift(collection)

   Remove the first item in a collection and return it.

.. function:: insert(collection, index, item)

   Insert an item at the given index.

.. function:: del(collection, index)

   Remove the item at the given index.

.. function:: grow(collection, n)

   Add uninitialized space for ``n`` elements at the end of a collection.

.. function:: append(collection, items)

   Construct an array composed of the elements of ``items`` added to the end of a collection. Does not modify collection.

.. function:: append!(collection, items)

   Add the elements of ``items`` to the end of a collection.

Fully implemented by: ``Vector`` (aka 1-d ``Array``).

Strings
-------

.. function:: strlen(s)

   The number of characters in string ``s``.

.. function:: length(s)

   The last valid index for string ``s``. Indexes are byte offsets and not character numbers.

.. function:: chars(string)

   Return an array of the characters in ``string``.

.. function:: strcat(strs...)

   Concatenate strings.

.. function:: string(char...)

   Create a string with the given characters.

.. function:: string(x)

   Create a string from any value using the ``show`` function.

.. function:: cstring(::Ptr{Uint8})

   Create a string from the address of a C (0-terminated) string.

.. function:: cstring(s)

   Convert a string to a contiguous byte array representation appropriate for passing it to C functions.

.. function:: ASCIIString(::Array{Uint8,1})

   Create an ASCII string from a byte array.

.. function:: UTF8String(::Array{Uint8,1})

   Create a UTF-8 string from a byte array.

.. function:: strchr(string, char[, i])

   Return the index of ``char`` in ``string``, giving an error if not found. The third argument optionally specifies a starting index.

.. function:: lpad(string, n, p)

   Make a string at least ``n`` characters long by padding on the left with copies of ``p``.

.. function:: rpad(string, n, p)

   Make a string at least ``n`` characters long by padding on the right with copies of ``p``.

.. function:: split(string, char, include_empty)

   Return an array of strings by splitting the given string on occurrences of the given character delimiter. The second argument may also be a set of character delimiters to use. The third argument specifies whether empty fields should be included.

.. function:: strip(string)

   Return ``string`` with any leading and trailing whitespace removed.

.. function:: lstrip(string)

   Return ``string`` with any leading whitespace removed.

.. function:: rstrip(string)

   Return ``string`` with any trailing whitespace removed.

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

I/O
---

.. data:: stdout_stream

   Global variable referring to the standard out stream.

.. data:: stderr_stream

   Global variable referring to the standard error stream.

.. data:: stdin_stream

   Global variable referring to the standard input stream.

.. function:: open(file_name[, read, write, create, truncate, append])

   Open a file in a mode specified by five boolean arguments. The default is to open files for reading only. Returns a stream for accessing the file.

.. function:: open(file_name[, mode])

   Alternate syntax for open, where a string-based mode specifier is used instead of the five booleans. The values of ``mode`` correspond to those from ``fopen(3)`` or Perl ``open``, and are equivalent to setting the following boolean groups:

   ==== =================================
    r    read
    r+   read, write
    w    write, create, truncate
    w+   read, write, create, truncate
    a    write, create, append
    a+   read, write, create, append
   ==== =================================

.. function:: memio([size])

   Create an in-memory I/O stream, optionally specifying how much initial space is needed.

.. function:: fdio(descriptor[, own])

   Create an ``IOStream`` object from an integer file descriptor. If ``own`` is true, closing this object will close the underlying descriptor. By default, an ``IOStream`` is closed when it is garbage collected.

.. function:: flush(stream)

   Commit all currently buffered writes to the given stream.

.. function:: close(stream)

   Close an I/O stream. Performs a ``flush`` first.

.. function:: with_output_stream(stream, f::Function, args...)

   Call ``f(args...)`` with the current output stream set to the given object. This is typically used to redirect the output of ``print`` and ``show``.

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

.. function:: skip(s, offset)

   Seek a stream relative to the current position.

.. function:: current_output_stream()

   Obtain the current default output stream (used by ``print`` and other output functions).

.. function:: set_current_output_stream(s)

   Set the current output stream.


Text I/O
--------

.. function:: show(x)

   Write an informative text representation of a value to the current output stream.

.. function:: print(x)

   Write (to the current output stream) a canonical (un-decorated) text representation of a value if there is one, otherwise call ``show``.

.. function:: println(x)

   Print (using ``print``) ``x`` followed by a newline

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

.. function:: EachLine(stream)

   Create an iterable object that will yield each line from a stream.

.. function:: dlmread(filename, delim::Char)

   Read a matrix from a text file where each line gives one row, with elements separated by the given delimeter. If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: dlmread(filename, delim::Char, T::Type)

   Read a matrix from a text file with a given element type. If ``T`` is a numeric type, the result is an array of that type, with any non-numeric elements as ``NaN`` for floating-point types, or zero. Other useful values of ``T`` include ``ASCIIString``, ``String``, and ``Any``.

.. function:: dlmwrite(filename, array, delim::Char)

   Write an array to a text file using the given delimeter (defaults to comma).

.. function:: csvread(filename[, T::Type])

   Equivalent to ``dlmread`` with ``delim`` set to comma.

.. function:: csvwrite(filename, array)

   Equivalent to ``dlmwrite`` with ``delim`` set to comma.

Memory-mapped I/O
-----------------

.. function:: mmap_array(type, dims, stream[, offset])

   Create an array whose values are linked to a file, using memory-mapping. This provides a convenient way of working with data too large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted (no format conversions are possible), and dims is a tuple containing the size of the array.  

   The file is specified via the stream.  When you initialize the stream, use "r" for a "read-only" array, and "w+" to create a new array used to write values to disk. Optionally, you can specify an offset (in bytes) if, for example, you want to skip over a header in the file.

   Example:  A = mmap_array(Int64, (25,30000), s)

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

.. function:: -

   Unary minus

``+`` ``-`` ``*`` ``.*`` ``/`` ``./`` ``\`` ``.\`` ``^`` ``.^``

.. function:: div

   Integer truncating division

.. function:: fld

   Integer floor division

``mod`` ``%``

.. function:: //

   Rational division

``<<`` ``>>`` ``>>>``

``==`` ``!=`` ``<`` ``<=`` ``>`` ``>=``

.. function:: !

   Boolean not

.. function:: ~

   Boolean or bitwise not

.. function:: &

   Bitwise and

.. function:: |

   Bitwise or

.. function:: $

   Bitwise exclusive or

``sin`` ``cos`` ``tan`` ``sinh`` ``cosh`` ``tanh`` ``asin`` ``acos`` ``atan`` ``atan2`` ``sec`` ``csc`` ``cot`` ``asec`` ``acsc`` ``acot`` ``sech`` ``csch`` ``coth`` ``acosh`` ``asinh`` ``atanh`` ``asech`` ``acsch`` ``acoth`` ``sinc`` ``cosc`` ``hypot`` ``log`` ``log2`` ``log10``

.. function:: log1p(x)

   Accurate natural logarithm of ``1+x``

``logb`` ``ilogb`` ``exp``

.. function:: expm1(x)

   Accurately compute ``exp(x)-1``

.. function:: ceil(x) -> Float

   Returns the nearest integer not less than ``x``.

.. function:: floor(x) -> Float

   Returns the nearest integer not greater than ``x``.

.. function:: trunc(x) -> Float

   Returns the nearest integer not greater in magnitude than ``x``.

.. function:: iceil(x) -> Integer

   Returns the nearest integer not less than ``x``.

.. function:: ifloor(x) -> Integer

   Returns the nearest integer not greater than ``x``.

.. function:: itrunc(x) -> Integer

   Returns the nearest integer not greater in magnitude than ``x``.

``exp2`` ``ldexp`` ``round`` ``iround`` ``ipart`` ``fpart`` ``min`` ``max`` ``clamp`` ``abs``

.. function:: abs2(x)

   Squared absolute value of ``x``

``copysign`` ``sign`` ``signbit`` ``pow`` ``sqrt`` ``cbrt`` ``erf`` ``erfc`` ``gamma`` ``lgamma`` ``lfact`` ``real`` ``imag`` ``conj`` ``angle`` ``cis(theta)``

.. function:: binomial(n,k)

   Number of ways to choose ``k`` out of ``n`` items

.. function:: factorial(n)

   Factorial of n

.. function:: factorial(n,k)

   Compute ``factorial(n)/factorial(k)``

.. function:: gcd(x,y)

   Greatest common divisor

.. function:: lcm(x,y)

   Least common multiple

.. function:: nextpow2(n)

   Next power of two not less than ``n``

.. function:: powermod(x, p, m)

   Compute ``mod(x^p, m)``

Data Formats
------------

.. function:: bin(n[, pad])

   Convert an integer to a binary string, optionally specifying a number of digits to pad to.

.. function:: hex(n[, pad])

   Convert an integer to a hexadecimal string, optionally specifying a number of digits to pad to.

.. function:: dec(n[, pad])

   Convert an integer to a decimal string, optionally specifying a number of digits to pad to.

.. function:: oct(n[, pad])

   Convert an integer to an octal string, optionally specifying a number of digits to pad to.

.. function:: int2str(n, base[, pad])

   Convert an integer to a string in the given base, optionally specifying a number of digits to pad to.

.. function:: parse_int(type, str, base)

   Parse a string as an integer in the given base, yielding a number of the specified type.

.. function:: bool(x)

   Convert a number or numeric array to boolean

.. function:: int8(x)

   Convert a number or array to ``Int8`` data type

.. function:: int16(x)

   Convert a number or array to ``Int16`` data type

.. function:: int32(x)

   Convert a number or array to ``Int32`` data type

.. function:: int64(x)

   Convert a number or array to ``Int64`` data type

.. function:: uint8(x)

   Convert a number or array to ``Uint8`` data type

.. function:: uint16(x)

   Convert a number or array to ``Uint16`` data type

.. function:: uint32(x)

   Convert a number or array to ``Uint32`` data type

.. function:: uint64(x)

   Convert a number or array to ``Uint64`` data type

.. function:: float32(x)

   Convert a number or array to ``Float32`` data type

.. function:: float64(x)

   Convert a number or array to ``Float64`` data type

.. function:: char(x)

   Convert a number or array to ``Char`` data type

.. function:: safe_char(x)

   Convert to ``Char``, checking for invalid code points

.. function:: complex(r,i)

   Convert real numbers or arrays to complex

.. function:: iscomplex(x)

   Test whether a number or array is of a complex type

.. function:: isreal(x)

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

.. function:: isdenormal(f)

   Test whether a floating point number is denormal

.. function:: isfinite(f)

   Test whether a number is finite

.. function:: isnan(f)

   Test whether a floating point number is not a number (NaN)

.. function:: nextfloat(f)

   Get the next floating point number in lexicographic order

.. function:: prevfloat(f)

   Get the previous floating point number in lexicographic order

.. function:: integer_valued(x)

   Test whether ``x`` is numerically equal to some integer

.. function:: real_valued(x)

   Test whether ``x`` is numerically equal to some real number

.. function:: exponent(f)

   Get the exponent of a floating-point number

.. function:: mantissa(f)

   Get the mantissa of a floating-point number

Random Numbers
--------------

Random numbers are generated in Julia by calling functions from the `Mersenne Twister library <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT>`_

.. function:: rand

   Generate a ``Float64`` random number in (0,1)

.. function:: randf

   Generate a ``Float32`` random number in (0,1)

.. function:: randi(Int32|Uint32|Int64|Uint64)

   Generate a random integer of the given type

.. function:: randi(n)

   Generate a random integer from 1 to ``n`` inclusive

.. function:: randi(n, dims...)

   Generate an array of random integers from 1 to ``n`` inclusive

.. function:: randi((a,b))

   Generate a random integer in the interval from ``a`` to ``b`` inclusive. The argument is a tuple.

.. function:: randi((a,b), dims...)

   Generate an array of random integers in the interval from ``a`` to ``b`` inclusive. The first argument is a tuple.

.. function:: randbit

   Generate ``1`` or ``0`` at random

.. function:: randbool

   Generate a random boolean value

.. function:: randn

   Generate a normally-distributed random number with mean 0 and standard deviation 1

.. function:: randg(a)

   Generate a sample from the gamma distribution with shape parameter ``a``

.. function:: randchi2(n)

   Generate a sample from the chi-squared distribution with ``n`` degrees of freedom (also available as ``chi2rnd``)

.. function:: randexp

   Generate samples from the exponential distribution

.. function:: srand

   Seed the RNG

Arrays
------

Basic functions
~~~~~~~~~~~~~~~

.. function:: ndims(A)

   Returns the number of dimensions of A

.. function:: size(A)

   Returns a tuple containing the dimensions of A

.. function:: eltype(A)

   Returns the type of the elements contained in A

.. function:: numel(A)

   Returns the number of elements in A

.. function:: length(A)

   Returns the number of elements in A (note that this differs from Matlab where ``length(A)`` is the largest dimension of ``A``)

.. function:: nnz(A)

   Counts the number of nonzero values in A

.. function:: stride(A, k)

   Returns the size of the stride along dimension k

.. function:: strides(A)

   Returns a tuple of the linear index distances between adjacent elements in each dimension

Constructors
~~~~~~~~~~~~

.. function:: Array(type, dims)

   Construct an uninitialized dense array. ``dims`` may be a tuple or a series of integer arguments.

.. function:: ref(type)

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

.. function:: empty(A)

   Construct an empty 1-d array similar to the given array

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

Mathematical operators and functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All mathematical operations and functions are supported for arrays

Indexing, Assignment, and Concatenation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. function:: ref(A, ind)

   Returns a subset of ``A`` as specified by ``ind``, which may be an ``Int``, a ``Range``, or a ``Vector``.

.. function:: sub(A, ind)

   Returns a SubArray, which stores the input ``A`` and ``ind`` rather than computing the result immediately. Calling ``ref`` on a SubArray computes the indices on the fly.

.. function:: slicedim(A, d, i)

   Return all the data of ``A`` where the index for dimension ``d`` equals ``i``. Equivalent to ``A[:,:,...,i,:,:,...]`` where ``i`` is in position ``d``.

.. function:: assign(A, X, ind)

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

.. function:: permute(A,perm)

   Permute the dimensions of array ``A``. ``perm`` is a vector specifying a permutation of length ``ndims(A)``. This is a generalization of transpose for multi-dimensional arrays. Transpose is equivalent to ``permute(A,[2,1])``.

.. function:: ipermute(A,perm)

   Like ``permute``, except the inverse of the given permutation is applied.

.. function:: squeeze(A)

   Remove singleton dimensions from the shape of array ``A``

.. function:: vec(A)

   Make a vector out of an array with only one non-singleton dimension.

.. function:: rowvec(A, i)

   Return the ith row of matrix A as a vector.

.. function:: colvec(A, i)

   Return the ith column of matrix A as a vector.

Linear Algebra
--------------

Linear algebra functions in Julia are largely implemented by calling functions from `LAPACK <http://www.netlib.org/lapack/>`_.

.. function:: *

   Matrix multiplication

.. function:: \

   Matrix division using a polyalgorithm. For input matrices ``A`` and ``B``, the result ``X`` is such that ``A*X == B``. For rectangular ``A``, QR factorization is used. For triangular ``A``, a triangular solve is performed. For square ``A``, Cholesky factorization is tried if the input is symmetric with a heavy diagonal. LU factorization is used in case Cholesky factorization fails or for general square inputs.

.. function:: dot

   Compute the dot product

.. function:: cross

   Compute the cross product of two 3-vectors

.. function:: norm

   Compute the norm of a ``Vector`` or a ``Matrix``

.. function:: chol(A)

   Compute Cholesky factorization

.. function:: lu(A) -> L, U, p

   Compute LU factorization

.. function:: qr(A) -> Q, R, p

   Compute QR factorization

.. function:: eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

.. function:: svd(A) -> U, S, V

   Compute the SVD of A

.. function:: triu(M)

   Upper triangle of a matrix

.. function:: tril(M)

   Lower triangle of a matrix

.. function:: diag(M)

   The diagonal of a matrix, as a vector

.. function:: diagm(v)

   Construct a diagonal matrix from a vector

.. function:: rank(M)

   Compute the rank of a matrix

.. function:: cond(M)

   Matrix condition number

.. function:: trace(M)

   Matrix trace

.. function:: det(M)

   Matrix determinant

.. function:: inv(M)

   Matrix inverse, or generalized ``1/M``

.. function:: repmat(A, n, m)

   Construct a matrix by repeating the given matrix ``n`` times in dimension 1 and ``m`` times in dimension 2.

.. function:: kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

.. function:: linreg(x, y)

   Determine parameters ``[a, b]`` that minimize the squared error between ``y`` and ``a+b*x``.

.. function:: linreg(x, y, w)

   Weighted least-squares linear regression

Combinatorics
-------------

.. function:: sort(v)

   Sort a vector in ascending order, according to ``isless``.

.. function:: sort!(v)

   In-place sort

.. function:: sortr(v)

   Sort a vector in descending order

.. function:: sortr!(v)

   In-place descending-order sort

.. function:: sort(a, dim)

   Sort an array along the given dimension

.. function:: sort(lessthan, a[, dim])

   Sort with a custom comparison function

.. function:: sortperm(v) -> s,p

   Sort a vector in ascending order, also constructing the permutation that sorts the vector

.. function:: issorted(v)

   Test whether a vector is in ascending sorted order

.. function:: nthperm(v, k)

   Compute the kth lexicographic permutation of a vector

.. function:: nthperm!(v, k)

   In-place version of ``nthperm``

.. function:: randperm(n)

   Construct a random permutation of the given length

.. function:: randcycle(n)

   Construct a random cyclic permutation of the given length

.. function:: shuffle(v)

   Randomly rearrange the elements of a vector

.. function:: shuffle!(v)

   In-place version of ``shuffle``

.. function:: reverse(v)

   Reverse vector ``v``

.. function:: reverse!(v)

   Reverse vector ``v`` in-place

.. function:: select(v, k)

   Find the element in position ``k`` in the sorted vector ``v`` without sorting

.. function:: select!(v, k)

   In-place version of ``select``

Statistics
----------

.. function:: mean(v[, dim])

   Compute the mean of whole array ``v``, or optionally along dimension ``dim``

.. function:: std(v)

   Compute the standard deviation of a vector ``v``

.. function:: median(v)

   Compute the median of a vector ``v``

.. function:: hist(v[, n])

   Compute the histogram of ``v``, optionally using ``n`` bins

.. function:: histc(v[, e])

   Compute the histogram of ``v``, optionally using a vector ``e`` as the edges for the bins

Signal Processing
-----------------

FFT functions in Julia are largely implemented by calling functions from `FFTW <http://www.fftw.org>`_

.. function:: fft(A, dim)

   One dimensional FFT if input is a ``Vector``. For n-d cases, compute fft of vectors along dimension ``dim``

.. function:: fft2

   2d FFT

.. function:: fft3

   3d FFT

.. function:: fftn

   N-d FFT

.. function:: ifft(A, dim)

   Inverse FFT. Same arguments as ``fft``.

.. function:: ifft2

   Inverse 2d FFT

.. function:: ifft3

   Inverse 3d FFT

.. function:: ifftn

   Inverse N-d FFT

.. function:: rfft(A [, dim=1])

   One-dimensional FFT of real array A along dimension dim. If A has size
   ``(..., n_dim, ...)``, the result has size ``(..., floor(n_dim/2)+1, ...)``.

.. function:: rfftn(A)

   N-d FFT of real array A. If A has size ``(n_1, ..., n_d)``, the result has size
   ``(floor(n_1/2)+1, ..., n_d)``.

.. function:: fftshift(x)

   Swap the first and second halves of each dimension of ``x``.

.. function:: fftshift(x,dim)

   Swap the first and second halves of the given dimension of array ``x``.

.. function:: ifftshift(x[, dim])

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

   Get the number of available processors

.. function:: myid()

   Get the id of the current processor

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

.. function:: darray(init, type, dims[, distdim, procs, dist])

   Construct a distributed array. ``init`` is a function of three arguments that will run on each processor, and should return an ``Array`` holding the local data for the current processor. Its arguments are ``(T,d,da)`` where ``T`` is the element type, ``d`` is the dimensions of the needed local piece, and ``da`` is the new ``DArray`` being constructed (though, of course, it is not fully initialized). ``type`` is the element type. ``dims`` is the dimensions of the entire ``DArray``. ``distdim`` is the dimension to distribute in. ``procs`` is a vector of processor ids to use. ``dist`` is a vector giving the first index of each contiguous distributed piece, such that the nth piece consists of indexes ``dist[n]`` through ``dist[n+1]-1``. If you have a vector ``v`` of the sizes of the pieces, ``dist`` can be computed as ``cumsum([1,v])``. Fortunately, all arguments after ``dims`` are optional.

.. function:: darray(f, A)

   Transform ``DArray`` ``A`` to another of the same type and distribution by applying function ``f`` to each block of ``A``.

.. function:: dzeros([type, ]dims, ...)

   Construct a distrbuted array of zeros. Trailing arguments are the same as those accepted by ``darray``.

.. function:: dones([type, ]dims, ...)

   Construct a distrbuted array of ones. Trailing arguments are the same as those accepted by ``darray``.

.. function:: dfill(x, dims, ...)

   Construct a distrbuted array filled with value ``x``. Trailing arguments are the same as those accepted by ``darray``.

.. function:: drand(dims, ...)

   Construct a distrbuted uniform random array. Trailing arguments are the same as those accepted by ``darray``.

.. function:: drandn(dims, ...)

   Construct a distrbuted normal random array. Trailing arguments are the same as those accepted by ``darray``.

.. function:: dcell(dims, ...)

   Construct a distrbuted cell array. Trailing arguments are the same as those accepted by ``darray``.

.. function:: distribute(a[, distdim])

   Convert a local array to distributed

.. function:: localize(d)

   Get the local piece of a distributed array

.. function:: changedist(d, distdim)

   Change the distributed dimension of a ``DArray``

.. function:: myindexes(d)

   A tuple describing the indexes owned by the local processor

.. function:: owner(d, i)

   Get the id of the processor holding index ``i`` in the distributed dimension

.. function:: procs(d)

   Get the vector of processors storing pieces of ``d``

.. function:: distdim(d)

   Get the distributed dimension of ``d``

System
------

.. function:: system("command")

   Run a shell command.

.. function:: gethostname()

   Get the local machine's host name.

.. function:: getipaddr()

   Get the IP address of the local machine, as a string of the form "x.x.x.x".

.. function:: getcwd()

   Get the current working directory.

.. function:: setcwd("dir")

   Set the current working directory. Returns the new current directory.

.. function:: getpid()

   Get julia's process ID.

.. function:: time()

   Get the time in seconds since the epoch, with fairly high resolution.

.. function:: tic()

   Set a timer to be read by the next call to ``toc`` or ``toq``. The macro call ``@time expr`` can also be used to time evaluation.

.. function:: toc()

   Print and return the time elapsed since the last ``tic``

.. function:: toq()

   Return, but do not print, the time elapsed since the last ``tic``

.. function:: EnvHash()

   A singleton of this type, ``ENV``, provides a hash table interface to environment variables.

.. function:: dlopen(libfile)

   Load a shared library, returning an opaque handle

.. function:: dlsym(handle, sym)

   Look up a symbol from a shared library handle

Errors
------

.. function:: error(message)

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
