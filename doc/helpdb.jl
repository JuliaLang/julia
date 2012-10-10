# automatically generated -- do not edit

{

(E"Getting Around",E"exit",E"exit([code])

   Quit (or control-D at the prompt). The default exit code is zero,
   indicating that the processes completed successfully.

"),

(E"Getting Around",E"whos",E"whos([Module][, pattern::Regex])

   Print information about global variables in a module, optionally
   restricted to those matching 'pattern'.

"),

(E"Getting Around",E"edit",E"edit('file'[, line])

   Edit a file optionally providing a line number to edit at. Returns
   to the julia prompt when you quit the editor. If the file name ends
   in '.jl' it is reloaded when the editor closes the file.

"),

(E"Getting Around",E"edit",E"edit(function[, types])

   Edit the definition of a function, optionally specifying a tuple of
   types to indicate which method to edit. When the editor exits, the
   source file containing the definition is reloaded.

"),

(E"Getting Around",E"load",E"load('file')

   Evaluate the contents of a source file

"),

(E"Getting Around",E"help",E"help('name' or object)

   Get help for a function

"),

(E"Getting Around",E"apropos",E"apropos('string')

   Search help for a substring

"),

(E"Getting Around",E"which",E"which(f, args...)

   Show which method of 'f' will be called for the given arguments

"),

(E"Getting Around",E"methods",E"methods(f)

   Show all methods of 'f' with their argument types

"),

(E"All Objects",E"is",E"is(x, y)

   Determine whether 'x' and 'y' are identical, in the sense that no
   program could distinguish them.

"),

(E"All Objects",E"isa",E"isa(x, type)

   Determine whether 'x' is of the given type.

"),

(E"All Objects",E"isequal",E"isequal(x, y)

   True if and only if 'x' and 'y' have the same contents. Loosely
   speaking, this means 'x' and 'y' would look the same when printed.

"),

(E"All Objects",E"isless",E"isless(x, y)

   Test whether 'x' is less than 'y'. Provides a total order
   consistent with 'isequal'. Values that are normally unordered, such
   as 'NaN', are ordered in an arbitrary but consistent fashion. This
   is the default comparison used by 'sort'. Non-numeric types that
   can be ordered should implement this function.

"),

(E"All Objects",E"typeof",E"typeof(x)

   Get the concrete type of 'x'.

"),

(E"All Objects",E"tuple",E"tuple(xs...)

   Construct a tuple of the given objects.

"),

(E"All Objects",E"ntuple",E"ntuple(n, f::Function)

   Create a tuple of length 'n', computing each element as 'f(i)',
   where 'i' is the index of the element.

"),

(E"All Objects",E"object_id",E"object_id(x)

   Get a unique integer id for 'x'. 'object_id(x)==object_id(y)' if
   and only if 'is(x,y)'.

"),

(E"All Objects",E"hash",E"hash(x)

   Compute an integer hash code such that 'isequal(x,y)' implies
   'hash(x)==hash(y)'.

"),

(E"All Objects",E"finalizer",E"finalizer(x, function)

   Register a function to be called on 'x' when there are no program-
   accessible references to 'x'. The behavior of this function is
   unpredictable if 'x' is of a bits type.

"),

(E"All Objects",E"copy",E"copy(x)

   Create a shallow copy of 'x': the outer structure is copied, but
   not all internal values. For example, copying an array produces a
   new array with identically-same elements as the original.

"),

(E"All Objects",E"deepcopy",E"deepcopy(x)

   Create a deep copy of 'x': everything is copied recursively,
   resulting in a fully independent object. For example, deep-copying
   an array produces a new array whose elements are deep-copies of the
   original elements.

   As a special case, functions can only be actually deep-copied if
   they are anonymous, otherwise they are just copied. The difference
   is only relevant in the case of closures, i.e. functions which may
   contain hidden internal references.

   While it isn't normally necessary, user-defined types can override
   the default 'deepcopy' behavior by defining a specialized version
   of the function 'deepcopy_internal(x::T, dict::ObjectIdDict)'
   (which shouldn't otherwise be used), where 'T' is the type to be
   specialized for, and 'dict' keeps track of objects copied so far
   within the recursion. Within the definition, 'deepcopy_internal'
   should be used in place of 'deepcopy', and the 'dict' variable
   should be updated as appropriate before returning.

"),

(E"All Objects",E"convert",E"convert(type, x)

   Try to convert 'x' to the given type.

"),

(E"All Objects",E"promote",E"promote(xs...)

   Convert all arguments to their common promotion type (if any), and
   return them all (as a tuple).

"),

(E"Types",E"subtype",E"subtype(type1, type2)

   True if and only if all values of 'type1' are also of 'type2'. Can
   also be written using the '<:' infix operator as 'type1 <: type2'.

"),

(E"Types",E"typemin",E"typemin(type)

   The lowest value representable by the given (real) numeric type.

"),

(E"Types",E"typemax",E"typemax(type)

   The highest value representable by the given (real) numeric type.

"),

(E"Types",E"realmin",E"realmin(type)

   The smallest in absolute value non-denormal value representable by
   the given floating-point type

"),

(E"Types",E"realmax",E"realmax(type)

   The highest finite value representable by the given floating-point
   type

"),

(E"Types",E"sizeof",E"sizeof(type)

   Size, in bytes, of the canonical binary representation of the given
   type, if any.

"),

(E"Types",E"eps",E"eps([type])

   The distance between 1.0 and the next larger representable
   floating-point value of 'type'. The only types that are sensible
   arguments are 'Float32' and 'Float64'. If 'type' is omitted, then
   'eps(Float64)' is returned.

"),

(E"Types",E"eps",E"eps(x)

   The distance between 'x' and the next larger representable
   floating-point value of the same type as 'x'.

"),

(E"Types",E"promote_type",E"promote_type(type1, type2)

   Determine a type big enough to hold values of each argument type
   without loss, whenever possible. In some cases, where no type
   exists which to which both types can be promoted losslessly, some
   loss is tolerated; for example, 'promote_type(Int64,Float64)'
   returns 'Float64' even though strictly, not all 'Int64' values can
   be represented exactly as 'Float64' values.

"),

(E"Generic Functions",E"method_exists",E"method_exists(f, tuple)

   Determine whether the given generic function has a method matching
   the given tuple of argument types.

"),

(E"Generic Functions",E"applicable",E"applicable(f, args...)

   Determine whether the given generic function has a method
   applicable to the given arguments.

"),

(E"Generic Functions",E"invoke",E"invoke(f, (types...), args...)

   Invoke a method for the given generic function matching the
   specified types (as a tuple), on the specified arguments. The
   arguments must be compatible with the specified types. This allows
   invoking a method other than the most specific matching method,
   which is useful when the behavior of a more general definition is
   explicitly needed (often as part of the implementation of a more
   specific method of the same function).

"),

(E"Iteration",E"start",E"start(iter)

   Get initial iteration state for an iterable object

"),

(E"Iteration",E"done",E"done(iter, state)

   Test whether we are done iterating

"),

(E"Iteration",E"next",E"next(iter, state) -> item, state

   For a given iterable object and iteration state, return the current
   item and the next iteration state

"),

(E"General Collections",E"isempty",E"isempty(collection)

   Determine whether a collection is empty (has no elements).

"),

(E"General Collections",E"length",E"length(collection)

   For ordered, indexable collections, the maximum index 'i' for which
   'ref(collection, i)' is valid. For unordered collections, the
   number of elements.

"),

(E"Iterable Collections",E"contains",E"contains(itr, x)

   Determine whether a collection contains the given value, 'x'.

"),

(E"Iterable Collections",E"reduce",E"reduce(op, v0, itr)

   Reduce the given collection with the given operator, i.e.
   accumulate 'v = op(v,elt)' for each element, where 'v' starts as
   'v0'. Reductions for certain commonly-used operators are available
   in a more convenient 1-argument form: 'max(itr)', 'min(itr)',
   'sum(itr)', 'prod(itr)', 'any(itr)', 'all(itr)'.

"),

(E"Iterable Collections",E"max",E"max(itr)

   Determine maximum element in a collection

"),

(E"Iterable Collections",E"min",E"min(itr)

   Determine minimum element in a collection

"),

(E"Iterable Collections",E"sum",E"sum(itr)

   Sum elements of a collection

"),

(E"Iterable Collections",E"prod",E"prod(itr)

   Multiply elements of a collection

"),

(E"Iterable Collections",E"any",E"any(itr)

   Test whether any elements of a boolean collection are true

"),

(E"Iterable Collections",E"all",E"all(itr)

   Test whether all elements of a boolean collection are true

"),

(E"Iterable Collections",E"count",E"count(itr)

   Count the number of boolean elements in 'itr' which are 'true'
   rather than 'false'.

"),

(E"Iterable Collections",E"countp",E"countp(p, itr)

   Count the number of elements in 'itr' for which predicate 'p' is
   true.

"),

(E"Iterable Collections",E"anyp",E"anyp(p, itr)

   Determine whether any element of 'itr' satisfies the given
   predicate.

"),

(E"Iterable Collections",E"allp",E"allp(p, itr)

   Determine whether all elements of 'itr' satisfy the given
   predicate.

"),

(E"Iterable Collections",E"map",E"map(f, c)

   Transform collection 'c' by applying 'f' to each element

"),

(E"Indexable Collections",E"collection[key...]",E"ref(collection, key...)
collection[key...]

   Retrieve the value(s) stored at the given key or index within a
   collection.

"),

(E"Indexable Collections",E"collection[key...] = value",E"assign(collection, value, key...)
collection[key...] = value

   Store the given value at the given key or index within a
   collection.

"),

(E"Associative Collections",E"Dict{K,V}(n)",E"Dict{K,V}(n)

   Construct a hashtable with keys of type K and values of type V and
   intial size of n

"),

(E"Associative Collections",E"has",E"has(collection, key)

   Determine whether a collection has a mapping for a given key.

"),

(E"Associative Collections",E"get",E"get(collection, key, default)

   Return the value stored for the given key, or the given default
   value if no mapping for the key is present.

"),

(E"Associative Collections",E"del",E"del(collection, key)

   Delete the mapping for the given key in a collection.

"),

(E"Associative Collections",E"del_all",E"del_all(collection)

   Delete all keys from a collection.

"),

(E"Set-Like Collections",E"add",E"add(collection, key)

   Add an element to a set-like collection.

"),

(E"Set-Like Collections",E"Set",E"Set(x...)

   Construct a 'Set' with the given elements. Should be used instead
   of 'IntSet' for sparse integer sets.

"),

(E"Set-Like Collections",E"IntSet",E"IntSet(i...)

   Construct an 'IntSet' of the given integers. Implemented as a bit
   string, and therefore good for dense integer sets.

"),

(E"Set-Like Collections",E"choose",E"choose(s)

   Pick an element of a set

"),

(E"Set-Like Collections",E"union",E"union(s1, s2)

   Construct the union of two sets

"),

(E"Dequeues",E"push",E"push(collection, item)

   Insert an item at the end of a collection.

"),

(E"Dequeues",E"pop",E"pop(collection)

   Remove the last item in a collection and return it.

"),

(E"Dequeues",E"enqueue",E"enqueue(collection, item)

   Insert an item at the beginning of a collection. Also called
   'unshift'.

"),

(E"Dequeues",E"shift",E"shift(collection)

   Remove the first item in a collection and return it.

"),

(E"Dequeues",E"insert",E"insert(collection, index, item)

   Insert an item at the given index.

"),

(E"Dequeues",E"del",E"del(collection, index)

   Remove the item at the given index.

"),

(E"Dequeues",E"grow",E"grow(collection, n)

   Add uninitialized space for 'n' elements at the end of a
   collection.

"),

(E"Dequeues",E"append!",E"append!(collection, items)

   Add the elements of 'items' to the end of a collection.

"),

(E"Strings",E"strlen",E"strlen(s)

   The number of characters in string 's'.

"),

(E"Strings",E"length",E"length(s)

   The last valid index for string 's'. Indexes are byte offsets and
   not character numbers.

"),

(E"Strings",E"chars",E"chars(string)

   Return an array of the characters in 'string'.

"),

(E"Strings",E"strcat",E"strcat(strs...)

   Concatenate strings.

"),

(E"Strings",E"string",E"string(char...)

   Create a string with the given characters.

"),

(E"Strings",E"string",E"string(x)

   Create a string from any value using the 'show' function.

"),

(E"Strings",E"bytestring",E"bytestring(::Ptr{Uint8})

   Create a string from the address of a C (0-terminated) string.

"),

(E"Strings",E"bytestring",E"bytestring(s)

   Convert a string to a contiguous byte array representation
   appropriate for passing it to C functions.

"),

(E"Strings",E"ascii",E"ascii(::Array{Uint8, 1})

   Create an ASCII string from a byte array.

"),

(E"Strings",E"ascii",E"ascii(s)

   Convert a string to a contiguous ASCII string (all characters must
   be valid ASCII characters).

"),

(E"Strings",E"utf8",E"utf8(::Array{Uint8, 1})

   Create a UTF-8 string from a byte array.

"),

(E"Strings",E"utf8",E"utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must
   be valid UTF-8 characters).

"),

(E"Strings",E"strchr",E"strchr(string, char[, i])

   Return the index of 'char' in 'string', giving 0 if not found. The
   second argument may also be a vector or a set of characters. The
   third argument optionally specifies a starting index.

"),

(E"Strings",E"lpad",E"lpad(string, n, p)

   Make a string at least 'n' characters long by padding on the left
   with copies of 'p'.

"),

(E"Strings",E"rpad",E"rpad(string, n, p)

   Make a string at least 'n' characters long by padding on the right
   with copies of 'p'.

"),

(E"Strings",E"search",E"search(string, chars[, start])

   Search for the given characters within the given string. The second
   argument may be a single character, a vector or a set of
   characters, a string, or a regular expression (but regular
   expressions are only allowed on contiguous strings, such as ASCII
   or UTF-8 strings). The third argument optionally specifies a
   starting index. The return value is a tuple with 2 integers: the
   index of the match and the first valid index past the match (or an
   index beyond the end of the string if the match is at the end); it
   returns '(0,0)' if no match was found, and '(start,start)' if
   'chars' is empty.

"),

(E"Strings",E"split",E"split(string, chars[, limit][, include_empty])

   Return an array of strings by splitting the given string on
   occurrences of the given character delimiters, which may be
   specified in any of the formats allowed by 'search''s second
   argument. The last two arguments are optional; they are are a
   maximum size for the result and a flag determining whether empty
   fields should be included in the result.

"),

(E"Strings",E"strip",E"strip(string)

   Return 'string' with any leading and trailing whitespace removed.

"),

(E"Strings",E"lstrip",E"lstrip(string)

   Return 'string' with any leading whitespace removed.

"),

(E"Strings",E"rstrip",E"rstrip(string)

   Return 'string' with any trailing whitespace removed.

"),

(E"Strings",E"begins_with",E"begins_with(string, prefix)

   Returns 'true' if 'string' starts with 'prefix'.

"),

(E"Strings",E"ends_with",E"ends_with(string, suffix)

   Returns 'true' if 'string' ends with 'suffix'.

"),

(E"Strings",E"uppercase",E"uppercase(string)

   Returns 'string' with all characters converted to uppercase.

"),

(E"Strings",E"lowercase",E"lowercase(string)

   Returns 'string' with all characters converted to lowercase.

"),

(E"Strings",E"join",E"join(strings, delim)

   Join an array of strings into a single string, inserting the given
   delimiter between adjacent strings.

"),

(E"Strings",E"chop",E"chop(string)

   Remove the last character from a string

"),

(E"Strings",E"chomp",E"chomp(string)

   Remove a trailing newline from a string

"),

(E"Strings",E"ind2chr",E"ind2chr(string, i)

   Convert a byte index to a character index

"),

(E"Strings",E"chr2ind",E"chr2ind(string, i)

   Convert a character index to a byte index

"),

(E"Strings",E"randstring",E"randstring(len)

   Create a random ASCII string of length 'len', consisting of upper-
   and lower-case letters and the digits 0-9

"),

(E"I/O",E"stdout_stream",E"stdout_stream

   Global variable referring to the standard out stream.

"),

(E"I/O",E"stderr_stream",E"stderr_stream

   Global variable referring to the standard error stream.

"),

(E"I/O",E"stdin_stream",E"stdin_stream

   Global variable referring to the standard input stream.

"),

(E"I/O",E"open",E"open(file_name[, read, write, create, truncate, append])

   Open a file in a mode specified by five boolean arguments. The
   default is to open files for reading only. Returns a stream for
   accessing the file.

"),

(E"I/O",E"open",E"open(file_name[, mode])

   Alternate syntax for open, where a string-based mode specifier is
   used instead of the five booleans. The values of 'mode' correspond
   to those from 'fopen(3)' or Perl 'open', and are equivalent to
   setting the following boolean groups:

   +------+-----------------------------------+
   | r    | read                              |
   +------+-----------------------------------+
   | r+   | read, write                       |
   +------+-----------------------------------+
   | w    | write, create, truncate           |
   +------+-----------------------------------+
   | w+   | read, write, create, truncate     |
   +------+-----------------------------------+
   | a    | write, create, append             |
   +------+-----------------------------------+
   | a+   | read, write, create, append       |
   +------+-----------------------------------+

"),

(E"I/O",E"memio",E"memio([size])

   Create an in-memory I/O stream, optionally specifying how much
   initial space is needed.

"),

(E"I/O",E"fdio",E"fdio(descriptor[, own])

   Create an 'IOStream' object from an integer file descriptor. If
   'own' is true, closing this object will close the underlying
   descriptor. By default, an 'IOStream' is closed when it is garbage
   collected.

"),

(E"I/O",E"flush",E"flush(stream)

   Commit all currently buffered writes to the given stream.

"),

(E"I/O",E"close",E"close(stream)

   Close an I/O stream. Performs a 'flush' first.

"),

(E"I/O",E"write",E"write(stream, x)

   Write the canonical binary representation of a value to the given
   stream.

"),

(E"I/O",E"read",E"read(stream, type)

   Read a value of the given type from a stream, in canonical binary
   representation.

"),

(E"I/O",E"read",E"read(stream, type, dims)

   Read a series of values of the given type from a stream, in
   canonical binary representation. 'dims' is either a tuple or a
   series of integer arguments specifying the size of 'Array' to
   return.

"),

(E"I/O",E"position",E"position(s)

   Get the current position of a stream.

"),

(E"I/O",E"seek",E"seek(s, pos)

   Seek a stream to the given position.

"),

(E"I/O",E"seek_end",E"seek_end(s)

   Seek a stream to the end.

"),

(E"I/O",E"skip",E"skip(s, offset)

   Seek a stream relative to the current position.

"),

(E"Text I/O",E"show",E"show(x)

   Write an informative text representation of a value to the current
   output stream. New types should overload 'show(io, x)' where the
   first argument is a stream.

"),

(E"Text I/O",E"print",E"print(x)

   Write (to the default output stream) a canonical (un-decorated)
   text representation of a value if there is one, otherwise call
   'show'.

"),

(E"Text I/O",E"println",E"println(x)

   Print (using 'print') 'x' followed by a newline

"),

(E"Text I/O",E"showall",E"showall(x)

   Show x, printing all elements of arrays

"),

(E"Text I/O",E"dump",E"dump(x)

   Write a thorough text representation of a value to the current
   output stream.

"),

(E"Text I/O",E"readall",E"readall(stream)

   Read the entire contents of an I/O stream as a string.

"),

(E"Text I/O",E"readline",E"readline(stream)

   Read a single line of text, including a trailing newline character
   (if one is reached before the end of the input).

"),

(E"Text I/O",E"readuntil",E"readuntil(stream, delim)

   Read a string, up to and including the given delimiter byte.

"),

(E"Text I/O",E"readlines",E"readlines(stream)

   Read all lines as an array.

"),

(E"Text I/O",E"EachLine",E"EachLine(stream)

   Create an iterable object that will yield each line from a stream.

"),

(E"Text I/O",E"dlmread",E"dlmread(filename, delim::Char)

   Read a matrix from a text file where each line gives one row, with
   elements separated by the given delimeter. If all data is numeric,
   the result will be a numeric array. If some elements cannot be
   parsed as numbers, a cell array of numbers and strings is returned.

"),

(E"Text I/O",E"dlmread",E"dlmread(filename, delim::Char, T::Type)

   Read a matrix from a text file with a given element type. If 'T' is
   a numeric type, the result is an array of that type, with any non-
   numeric elements as 'NaN' for floating-point types, or zero. Other
   useful values of 'T' include 'ASCIIString', 'String', and 'Any'.

"),

(E"Text I/O",E"dlmwrite",E"dlmwrite(filename, array, delim::Char)

   Write an array to a text file using the given delimeter (defaults
   to comma).

"),

(E"Text I/O",E"csvread",E"csvread(filename[, T::Type])

   Equivalent to 'dlmread' with 'delim' set to comma.

"),

(E"Text I/O",E"csvwrite",E"csvwrite(filename, array)

   Equivalent to 'dlmwrite' with 'delim' set to comma.

"),

(E"Memory-mapped I/O",E"mmap_array",E"mmap_array(type, dims, stream[, offset])

   Create an array whose values are linked to a file, using memory-
   mapping. This provides a convenient way of working with data too
   large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted (no
   format conversions are possible), and dims is a tuple containing
   the size of the array.

   The file is specified via the stream.  When you initialize the
   stream, use 'r' for a 'read-only' array, and 'w+' to create a new
   array used to write values to disk. Optionally, you can specify an
   offset (in bytes) if, for example, you want to skip over a header
   in the file.

   Example:  A = mmap_array(Int64, (25,30000), s)

   This would create a 25-by-30000 array of Int64s, linked to the file
   associated with stream s.

"),

(E"Memory-mapped I/O",E"msync",E"msync(array)

   Forces synchronization between the in-memory version of a memory-
   mapped array and the on-disk version. You may not need to call this
   function, because synchronization is performed at intervals
   automatically by the operating system. Hower, you can call this
   directly if, for example, you are concerned about losing the result
   of a long-running calculation.

"),

(E"Memory-mapped I/O",E"mmap",E"mmap(len, prot, flags, fd, offset)

   Low-level interface to the mmap system call. See the man page.

"),

(E"Memory-mapped I/O",E"munmap",E"munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With
   mmap_array you do not need to call this directly; the memory is
   unmapped for you when the array goes out of scope.

"),

(E"Mathematical Functions",E"-",E"-

   Unary minus

"),

(E"Mathematical Functions",E"div",E"div()

   Integer truncating division

"),

(E"Mathematical Functions",E"fld",E"fld()

   Integer floor division

"),

(E"Mathematical Functions",E"//",E"//

   Rational division

"),

(E"Mathematical Functions",E"!",E"!

   Boolean not

"),

(E"Mathematical Functions",E"~",E"~

   Boolean or bitwise not

"),

(E"Mathematical Functions",E"&",E"&

   Bitwise and

"),

(E"Mathematical Functions",E"|",E"|

   Bitwise or

"),

(E"Mathematical Functions",E"$",E"$

   Bitwise exclusive or

"),

(E"Mathematical Functions",E"log1p",E"log1p(x)

   Accurate natural logarithm of '1+x'

"),

(E"Mathematical Functions",E"expm1",E"expm1(x)

   Accurately compute 'exp(x)-1'

"),

(E"Mathematical Functions",E"round",E"round(x[, digits[, base]]) -> FloatingPoint

   'round(x)' returns the nearest integer to 'x'. 'round(x, digits)'
   rounds to the specified number of digits after the decimal place,
   or before if negative, e.g., 'round(pi,2)' is '3.14'. 'round(x,
   digits, base)' rounds using a different base, defaulting to 10,
   e.g., 'round(pi, 3, 2)' is '3.125'.

"),

(E"Mathematical Functions",E"ceil",E"ceil(x[, digits[, base]]) -> FloatingPoint

   Returns the nearest integer not less than 'x'. 'digits' and 'base'
   work as above.

"),

(E"Mathematical Functions",E"floor",E"floor(x[, digits[, base]]) -> FloatingPoint

   Returns the nearest integer not greater than 'x'. 'digits' and
   'base' work as above.

"),

(E"Mathematical Functions",E"trunc",E"trunc(x[, digits[, base]]) -> FloatingPoint

   Returns the nearest integer not greater in magnitude than 'x'.
   'digits' and 'base' work as above.

"),

(E"Mathematical Functions",E"iround",E"iround(x) -> Integer

   Returns the nearest integer to 'x'.

"),

(E"Mathematical Functions",E"iceil",E"iceil(x) -> Integer

   Returns the nearest integer not less than 'x'.

"),

(E"Mathematical Functions",E"ifloor",E"ifloor(x) -> Integer

   Returns the nearest integer not greater than 'x'.

"),

(E"Mathematical Functions",E"itrunc",E"itrunc(x) -> Integer

   Returns the nearest integer not greater in magnitude than 'x'.

"),

(E"Mathematical Functions",E"signif",E"signif(x, digits[, base]) -> FloatingPoint

   Rounds (in the sense of 'round') 'x' so that there are 'digits'
   significant digits, under a base 'base' representation, default 10.
   E.g., 'signif(123.456, 2)' is '120.0', and 'signif(357.913, 4, 2)'
   is '352.0'.

"),

(E"Mathematical Functions",E"abs2",E"abs2(x)

   Squared absolute value of 'x'

"),

(E"Mathematical Functions",E"binomial",E"binomial(n, k)

   Number of ways to choose 'k' out of 'n' items

"),

(E"Mathematical Functions",E"factorial",E"factorial(n)

   Factorial of n

"),

(E"Mathematical Functions",E"factorial",E"factorial(n, k)

   Compute 'factorial(n)/factorial(k)'

"),

(E"Mathematical Functions",E"gcd",E"gcd(x, y)

   Greatest common divisor

"),

(E"Mathematical Functions",E"lcm",E"lcm(x, y)

   Least common multiple

"),

(E"Mathematical Functions",E"nextpow2",E"nextpow2(n)

   Next power of two not less than 'n'

"),

(E"Mathematical Functions",E"nextpow",E"nextpow(a, n)

   Next power of 'a' not less than 'n'

"),

(E"Mathematical Functions",E"prevpow",E"prevpow(a, n)

   Previous power of 'a' not greater than 'n'

"),

(E"Mathematical Functions",E"nextprod",E"nextprod([a, b, c], n)

   Next integer not less than 'n' that can be written 'a^i1 * b^i2 *
   c^i3' for integers 'i1', 'i2', 'i3'.

"),

(E"Mathematical Functions",E"prevprod",E"prevprod([a, b, c], n)

   Previous integer not greater than 'n' that can be written 'a^i1 *
   b^i2 * c^i3' for integers 'i1', 'i2', 'i3'.

"),

(E"Mathematical Functions",E"powermod",E"powermod(x, p, m)

   Compute 'mod(x^p, m)'

"),

(E"Data Formats",E"bin",E"bin(n[, pad])

   Convert an integer to a binary string, optionally specifying a
   number of digits to pad to.

"),

(E"Data Formats",E"hex",E"hex(n[, pad])

   Convert an integer to a hexadecimal string, optionally specifying a
   number of digits to pad to.

"),

(E"Data Formats",E"dec",E"dec(n[, pad])

   Convert an integer to a decimal string, optionally specifying a
   number of digits to pad to.

"),

(E"Data Formats",E"oct",E"oct(n[, pad])

   Convert an integer to an octal string, optionally specifying a
   number of digits to pad to.

"),

(E"Data Formats",E"base",E"base(b, n[, pad])

   Convert an integer to a string in the given base, optionally
   specifying a number of digits to pad to.

"),

(E"Data Formats",E"parse_int",E"parse_int(type, str, base)

   Parse a string as an integer in the given base, yielding a number
   of the specified type.

"),

(E"Data Formats",E"bool",E"bool(x)

   Convert a number or numeric array to boolean

"),

(E"Data Formats",E"int8",E"int8(x)

   Convert a number or array to 'Int8' data type

"),

(E"Data Formats",E"int16",E"int16(x)

   Convert a number or array to 'Int16' data type

"),

(E"Data Formats",E"int32",E"int32(x)

   Convert a number or array to 'Int32' data type

"),

(E"Data Formats",E"int64",E"int64(x)

   Convert a number or array to 'Int64' data type

"),

(E"Data Formats",E"uint8",E"uint8(x)

   Convert a number or array to 'Uint8' data type

"),

(E"Data Formats",E"uint16",E"uint16(x)

   Convert a number or array to 'Uint16' data type

"),

(E"Data Formats",E"uint32",E"uint32(x)

   Convert a number or array to 'Uint32' data type

"),

(E"Data Formats",E"uint64",E"uint64(x)

   Convert a number or array to 'Uint64' data type

"),

(E"Data Formats",E"float32",E"float32(x)

   Convert a number or array to 'Float32' data type

"),

(E"Data Formats",E"float64",E"float64(x)

   Convert a number or array to 'Float64' data type

"),

(E"Data Formats",E"char",E"char(x)

   Convert a number or array to 'Char' data type

"),

(E"Data Formats",E"safe_char",E"safe_char(x)

   Convert to 'Char', checking for invalid code points

"),

(E"Data Formats",E"complex",E"complex(r, i)

   Convert real numbers or arrays to complex

"),

(E"Data Formats",E"iscomplex",E"iscomplex(x)

   Test whether a number or array is of a complex type

"),

(E"Data Formats",E"isreal",E"isreal(x)

   Test whether a number or array is of a real type

"),

(E"Data Formats",E"bswap",E"bswap(n)

   Byte-swap an integer

"),

(E"Data Formats",E"num2hex",E"num2hex(f)

   Get a hexadecimal string of the binary representation of a floating
   point number

"),

(E"Data Formats",E"hex2num",E"hex2num(str)

   Convert a hexadecimal string to the floating point number it
   represents

"),

(E"Numbers",E"one",E"one(x)

   Get the multiplicative identity element for the type of x (x can
   also specify the type itself). For matrices, returns an identity
   matrix of the appropriate size and type.

"),

(E"Numbers",E"zero",E"zero(x)

   Get the additive identity element for the type of x (x can also
   specify the type itself).

"),

(E"Numbers",E"pi",E"pi

   The constant pi

"),

(E"Numbers",E"isdenormal",E"isdenormal(f)

   Test whether a floating point number is denormal

"),

(E"Numbers",E"isfinite",E"isfinite(f)

   Test whether a number is finite

"),

(E"Numbers",E"isnan",E"isnan(f)

   Test whether a floating point number is not a number (NaN)

"),

(E"Numbers",E"nextfloat",E"nextfloat(f)

   Get the next floating point number in lexicographic order

"),

(E"Numbers",E"prevfloat",E"prevfloat(f)

   Get the previous floating point number in lexicographic order

"),

(E"Numbers",E"integer_valued",E"integer_valued(x)

   Test whether 'x' is numerically equal to some integer

"),

(E"Numbers",E"real_valued",E"real_valued(x)

   Test whether 'x' is numerically equal to some real number

"),

(E"Numbers",E"exponent",E"exponent(f)

   Get the exponent of a floating-point number

"),

(E"Numbers",E"mantissa",E"mantissa(f)

   Get the mantissa of a floating-point number

"),

(E"Random Numbers",E"rand",E"rand()

   Generate a 'Float64' random number in (0,1)

"),

(E"Random Numbers",E"randf",E"randf()

   Generate a 'Float32' random number in (0,1)

"),

(E"Random Numbers",E"randi",E"randi(Int32|Uint32|Int64|Uint64)

   Generate a random integer of the given type

"),

(E"Random Numbers",E"randi",E"randi(n)

   Generate a random integer from 1 to 'n' inclusive

"),

(E"Random Numbers",E"randi",E"randi(n, dims...)

   Generate an array of random integers from 1 to 'n' inclusive

"),

(E"Random Numbers",E"randi",E"randi((a, b))

   Generate a random integer in the interval from 'a' to 'b'
   inclusive. The argument is a tuple.

"),

(E"Random Numbers",E"randi",E"randi((a, b), dims...)

   Generate an array of random integers in the interval from 'a' to
   'b' inclusive. The first argument is a tuple.

"),

(E"Random Numbers",E"randbit",E"randbit()

   Generate '1' or '0' at random

"),

(E"Random Numbers",E"randbool",E"randbool()

   Generate a random boolean value

"),

(E"Random Numbers",E"randn",E"randn()

   Generate a normally-distributed random number with mean 0 and
   standard deviation 1

"),

(E"Random Numbers",E"randg",E"randg(a)

   Generate a sample from the gamma distribution with shape parameter
   'a'

"),

(E"Random Numbers",E"randchi2",E"randchi2(n)

   Generate a sample from the chi-squared distribution with 'n'
   degrees of freedom (also available as 'chi2rnd')

"),

(E"Random Numbers",E"randexp",E"randexp()

   Generate samples from the exponential distribution

"),

(E"Random Numbers",E"srand",E"srand()

   Seed the RNG

"),

(E"Arrays",E"ndims",E"ndims(A)

   Returns the number of dimensions of A

"),

(E"Arrays",E"size",E"size(A)

   Returns a tuple containing the dimensions of A

"),

(E"Arrays",E"eltype",E"eltype(A)

   Returns the type of the elements contained in A

"),

(E"Arrays",E"numel",E"numel(A)

   Returns the number of elements in A

"),

(E"Arrays",E"length",E"length(A)

   Returns the number of elements in A (note that this differs from
   Matlab where 'length(A)' is the largest dimension of 'A')

"),

(E"Arrays",E"nnz",E"nnz(A)

   Counts the number of nonzero values in A

"),

(E"Arrays",E"stride",E"stride(A, k)

   Returns the distance in memory (in number of elements) between
   adjacent elements in dimension k

"),

(E"Arrays",E"strides",E"strides(A)

   Returns a tuple of the memory strides in each dimension

"),

(E"Arrays",E"Array",E"Array(type, dims)

   Construct an uninitialized dense array. 'dims' may be a tuple or a
   series of integer arguments.

"),

(E"Arrays",E"ref",E"ref(type)

   Construct an empty 1-d array of the specified type. This is usually
   called with the syntax 'Type[]'. Element values can be specified
   using 'Type[a,b,c,...]'.

"),

(E"Arrays",E"cell",E"cell(dims)

   Construct an uninitialized cell array (heterogeneous array). 'dims'
   can be either a tuple or a series of integer arguments.

"),

(E"Arrays",E"zeros",E"zeros(type, dims)

   Create an array of all zeros of specified type

"),

(E"Arrays",E"ones",E"ones(type, dims)

   Create an array of all ones of specified type

"),

(E"Arrays",E"trues",E"trues(dims)

   Create a Bool array with all values set to true

"),

(E"Arrays",E"falses",E"falses(dims)

   Create a Bool array with all values set to false

"),

(E"Arrays",E"fill",E"fill(v, dims)

   Create an array filled with 'v'

"),

(E"Arrays",E"fill!",E"fill!(A, x)

   Fill array 'A' with value 'x'

"),

(E"Arrays",E"reshape",E"reshape(A, dims)

   Create an array with the same data as the given array, but with
   different dimensions. An implementation for a particular type of
   array may choose whether the data is copied or shared.

"),

(E"Arrays",E"copy",E"copy(A)

   Create a copy of 'A'

"),

(E"Arrays",E"similar",E"similar(array, element_type, dims)

   Create an uninitialized array of the same type as the given array,
   but with the specified element type and dimensions. The second and
   third arguments are both optional. The 'dims' argument may be a
   tuple or a series of integer arguments.

"),

(E"Arrays",E"reinterpret",E"reinterpret(type, A)

   Construct an array with the same binary data as the given array,
   but with the specified element type

"),

(E"Arrays",E"rand",E"rand(dims)

   Create a random array with Float64 random values in (0,1)

"),

(E"Arrays",E"randf",E"randf(dims)

   Create a random array with Float32 random values in (0,1)

"),

(E"Arrays",E"randn",E"randn(dims)

   Create a random array with Float64 normally-distributed random
   values with a mean of 0 and standard deviation of 1

"),

(E"Arrays",E"eye",E"eye(n)

   n-by-n identity matrix

"),

(E"Arrays",E"eye",E"eye(m, n)

   m-by-n identity matrix

"),

(E"Arrays",E"linspace",E"linspace(start, stop, n)

   Construct a vector of 'n' linearly-spaced elements from 'start' to
   'stop'.

"),

(E"Arrays",E"ref",E"ref(A, ind)

   Returns a subset of 'A' as specified by 'ind', which may be an
   'Int', a 'Range', or a 'Vector'.

"),

(E"Arrays",E"sub",E"sub(A, ind)

   Returns a SubArray, which stores the input 'A' and 'ind' rather
   than computing the result immediately. Calling 'ref' on a SubArray
   computes the indices on the fly.

"),

(E"Arrays",E"slicedim",E"slicedim(A, d, i)

   Return all the data of 'A' where the index for dimension 'd' equals
   'i'. Equivalent to 'A[:,:,...,i,:,:,...]' where 'i' is in position
   'd'.

"),

(E"Arrays",E"assign",E"assign(A, X, ind)

   Store an input array 'X' within some subset of 'A' as specified by
   'ind'.

"),

(E"Arrays",E"cat",E"cat(dim, A...)

   Concatenate the input arrays along the specified dimension

"),

(E"Arrays",E"vcat",E"vcat(A...)

   Concatenate along dimension 1

"),

(E"Arrays",E"hcat",E"hcat(A...)

   Concatenate along dimension 2

"),

(E"Arrays",E"hvcat",E"hvcat()

   Horizontal and vertical concatenation in one call

"),

(E"Arrays",E"flipdim",E"flipdim(A, d)

   Reverse 'A' in dimension 'd'.

"),

(E"Arrays",E"flipud",E"flipud(A)

   Equivalent to 'flipdim(A,1)'.

"),

(E"Arrays",E"fliplr",E"fliplr(A)

   Equivalent to 'flipdim(A,2)'.

"),

(E"Arrays",E"circshift",E"circshift(A, shifts)

   Circularly shift the data in an array. The second argument is a
   vector giving the amount to shift in each dimension.

"),

(E"Arrays",E"find",E"find(A)

   Return a vector of the linear indexes of the non-zeros in 'A'.

"),

(E"Arrays",E"findn",E"findn(A)

   Return a vector of indexes for each dimension giving the locations
   of the non-zeros in 'A'.

"),

(E"Arrays",E"permute",E"permute(A, perm)

   Permute the dimensions of array 'A'. 'perm' is a vector specifying
   a permutation of length 'ndims(A)'. This is a generalization of
   transpose for multi-dimensional arrays. Transpose is equivalent to
   'permute(A,[2,1])'.

"),

(E"Arrays",E"ipermute",E"ipermute(A, perm)

   Like 'permute', except the inverse of the given permutation is
   applied.

"),

(E"Arrays",E"squeeze",E"squeeze(A)

   Remove singleton dimensions from the shape of array 'A'

"),

(E"Arrays",E"vec",E"vec(A)

   Make a vector out of an array with only one non-singleton
   dimension.

"),

(E"Linear Algebra",E"*",E"*

   Matrix multiplication

"),

(E"Linear Algebra",E"\\",E"\\

   Matrix division using a polyalgorithm. For input matrices 'A' and
   'B', the result 'X' is such that 'A*X == B'. For rectangular 'A',
   QR factorization is used. For triangular 'A', a triangular solve is
   performed. For square 'A', Cholesky factorization is tried if the
   input is symmetric with a heavy diagonal. LU factorization is used
   in case Cholesky factorization fails or for general square inputs.

"),

(E"Linear Algebra",E"dot",E"dot()

   Compute the dot product

"),

(E"Linear Algebra",E"cross",E"cross()

   Compute the cross product of two 3-vectors

"),

(E"Linear Algebra",E"norm",E"norm()

   Compute the norm of a 'Vector' or a 'Matrix'

"),

(E"Linear Algebra",E"lu",E"lu(A) -> LU

   Compute LU factorization. LU is an 'LU factorization' type that can
   be used as an ordinary matrix.

"),

(E"Linear Algebra",E"chol",E"chol(A)

   Compute Cholesky factorization

"),

(E"Linear Algebra",E"qr",E"qr(A)

   Compute QR factorization

"),

(E"Linear Algebra",E"qrp",E"qrp(A)

   Compute QR factorization with pivoting

"),

(E"Linear Algebra",E"factors",E"factors(D)

   Return the factors of a decomposition D. For an LU decomposition,
   factors(LU) -> L, U, p

"),

(E"Linear Algebra",E"eig",E"eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

"),

(E"Linear Algebra",E"svd",E"svd(A) -> U, S, V

   Compute the SVD of A

"),

(E"Linear Algebra",E"triu",E"triu(M)

   Upper triangle of a matrix

"),

(E"Linear Algebra",E"tril",E"tril(M)

   Lower triangle of a matrix

"),

(E"Linear Algebra",E"diag",E"diag(M)

   The diagonal of a matrix, as a vector

"),

(E"Linear Algebra",E"diagm",E"diagm(v)

   Construct a diagonal matrix from a vector

"),

(E"Linear Algebra",E"Tridiagonal",E"Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal,
   and upper diagonal

"),

(E"Linear Algebra",E"Woodbury",E"Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury
   matrix identity

"),

(E"Linear Algebra",E"rank",E"rank(M)

   Compute the rank of a matrix

"),

(E"Linear Algebra",E"cond",E"cond(M)

   Matrix condition number

"),

(E"Linear Algebra",E"trace",E"trace(M)

   Matrix trace

"),

(E"Linear Algebra",E"det",E"det(M)

   Matrix determinant

"),

(E"Linear Algebra",E"inv",E"inv(M)

   Matrix inverse, or generalized '1/M'

"),

(E"Linear Algebra",E"repmat",E"repmat(A, n, m)

   Construct a matrix by repeating the given matrix 'n' times in
   dimension 1 and 'm' times in dimension 2.

"),

(E"Linear Algebra",E"kron",E"kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

"),

(E"Linear Algebra",E"linreg",E"linreg(x, y)

   Determine parameters '[a, b]' that minimize the squared error
   between 'y' and 'a+b*x'.

"),

(E"Linear Algebra",E"linreg",E"linreg(x, y, w)

   Weighted least-squares linear regression

"),

(E"Combinatorics",E"sort",E"sort(v)

   Sort a vector in ascending order, according to 'isless'.

"),

(E"Combinatorics",E"sort!",E"sort!(v)

   In-place sort

"),

(E"Combinatorics",E"sortr",E"sortr(v)

   Sort a vector in descending order

"),

(E"Combinatorics",E"sortr!",E"sortr!(v)

   In-place descending-order sort

"),

(E"Combinatorics",E"sort",E"sort(a, dim)

   Sort an array along the given dimension

"),

(E"Combinatorics",E"sort",E"sort(lessthan, a[, dim])

   Sort with a custom comparison function

"),

(E"Combinatorics",E"sortperm",E"sortperm(v) -> s,p

   Sort a vector in ascending order, also constructing the permutation
   that sorts the vector

"),

(E"Combinatorics",E"issorted",E"issorted(v)

   Test whether a vector is in ascending sorted order

"),

(E"Combinatorics",E"nthperm",E"nthperm(v, k)

   Compute the kth lexicographic permutation of a vector

"),

(E"Combinatorics",E"nthperm!",E"nthperm!(v, k)

   In-place version of 'nthperm'

"),

(E"Combinatorics",E"randperm",E"randperm(n)

   Construct a random permutation of the given length

"),

(E"Combinatorics",E"randcycle",E"randcycle(n)

   Construct a random cyclic permutation of the given length

"),

(E"Combinatorics",E"shuffle",E"shuffle(v)

   Randomly rearrange the elements of a vector

"),

(E"Combinatorics",E"shuffle!",E"shuffle!(v)

   In-place version of 'shuffle'

"),

(E"Combinatorics",E"reverse",E"reverse(v)

   Reverse vector 'v'

"),

(E"Combinatorics",E"reverse!",E"reverse!(v)

   Reverse vector 'v' in-place

"),

(E"Combinatorics",E"select",E"select(v, k)

   Find the element in position 'k' in the sorted vector 'v' without
   sorting

"),

(E"Combinatorics",E"select!",E"select!(v, k)

   In-place version of 'select'

"),

(E"Statistics",E"mean",E"mean(v[, dim])

   Compute the mean of whole array 'v', or optionally along dimension
   'dim'

"),

(E"Statistics",E"std",E"std(v)

   Compute the standard deviation of a vector 'v'

"),

(E"Statistics",E"median",E"median(v)

   Compute the median of a vector 'v'

"),

(E"Statistics",E"hist",E"hist(v[, n])

   Compute the histogram of 'v', optionally using 'n' bins

"),

(E"Statistics",E"histc",E"histc(v[, e])

   Compute the histogram of 'v', optionally using a vector 'e' as the
   edges for the bins

"),

(E"Signal Processing",E"fft",E"fft(A, dim)

   One dimensional FFT if input is a 'Vector'. For n-d cases, compute
   fft of vectors along dimension 'dim'. Most efficient if 'size(A,
   dim)' is a product of small primes; see 'nextprod()'.

"),

(E"Signal Processing",E"fft2",E"fft2()

   2d FFT

"),

(E"Signal Processing",E"fft3",E"fft3()

   3d FFT

"),

(E"Signal Processing",E"fftn",E"fftn()

   N-d FFT

"),

(E"Signal Processing",E"ifft",E"ifft(A, dim)

   Inverse FFT. Same arguments as 'fft'.

"),

(E"Signal Processing",E"ifft2",E"ifft2()

   Inverse 2d FFT

"),

(E"Signal Processing",E"ifft3",E"ifft3()

   Inverse 3d FFT

"),

(E"Signal Processing",E"ifftn",E"ifftn()

   Inverse N-d FFT

"),

(E"Signal Processing",E"rfft",E"rfft(A[, dim=1])

   One-dimensional FFT of real array A along dimension dim. If A has
   size '(..., n_dim, ...)', the result has size '(...,
   floor(n_dim/2)+1, ...)'.

"),

(E"Signal Processing",E"rfftn",E"rfftn(A)

   N-d FFT of real array A. If A has size '(n_1, ..., n_d)', the
   result has size '(floor(n_1/2)+1, ..., n_d)'.

"),

(E"Signal Processing",E"fftshift",E"fftshift(x)

   Swap the first and second halves of each dimension of 'x'.

"),

(E"Signal Processing",E"fftshift",E"fftshift(x, dim)

   Swap the first and second halves of the given dimension of array
   'x'.

"),

(E"Signal Processing",E"ifftshift",E"ifftshift(x[, dim])

   Undoes the effect of 'fftshift'.

"),

(E"Signal Processing",E"filt",E"filt(b, a, x)

   Apply filter described by vectors 'a' and 'b' to vector 'x'.

"),

(E"Signal Processing",E"deconv",E"deconv(b, a)

   Construct vector 'c' such that 'b = conv(a,c) + r'. Equivalent to
   polynomial division.

"),

(E"Signal Processing",E"conv",E"conv(u, v)

   Convolution of two vectors. Uses FFT algorithm.

"),

(E"Signal Processing",E"xcorr",E"xcorr(u, v)

   Compute the cross-correlation of two vectors.

"),

(E"Parallel Computing",E"addprocs_local",E"addprocs_local(n)

   Add processes on the local machine. Can be used to take advantage
   of multiple cores.

"),

(E"Parallel Computing",E"addprocs_ssh",E"addprocs_ssh({'host1', 'host2', ...})

   Add processes on remote machines via SSH. Requires julia to be
   installed in the same location on each node, or to be available via
   a shared file system.

"),

(E"Parallel Computing",E"addprocs_sge",E"addprocs_sge(n)

   Add processes via the Sun/Oracle Grid Engine batch queue, using
   'qsub'.

"),

(E"Parallel Computing",E"nprocs",E"nprocs()

   Get the number of available processors

"),

(E"Parallel Computing",E"myid",E"myid()

   Get the id of the current processor

"),

(E"Parallel Computing",E"remote_call",E"remote_call(id, func, args...)

   Call a function asynchronously on the given arguments on the
   specified processor. Returns a 'RemoteRef'.

"),

(E"Parallel Computing",E"wait",E"wait(RemoteRef)

   Wait for a value to become available for the specified remote
   reference.

"),

(E"Parallel Computing",E"fetch",E"fetch(RemoteRef)

   Wait for and get the value of a remote reference.

"),

(E"Parallel Computing",E"remote_call_wait",E"remote_call_wait(id, func, args...)

   Perform 'wait(remote_call(...))' in one message.

"),

(E"Parallel Computing",E"remote_call_fetch",E"remote_call_fetch(id, func, args...)

   Perform 'fetch(remote_call(...))' in one message.

"),

(E"Parallel Computing",E"put",E"put(RemoteRef, value)

   Store a value to a remote reference. Implements 'shared queue of
   length 1' semantics: if a value is already present, blocks until
   the value is removed with 'take'.

"),

(E"Parallel Computing",E"take",E"take(RemoteRef)

   Fetch the value of a remote reference, removing it so that the
   reference is empty again.

"),

(E"Parallel Computing",E"RemoteRef",E"RemoteRef()

   Make an uninitialized remote reference on the local machine.

"),

(E"Parallel Computing",E"RemoteRef",E"RemoteRef(n)

   Make an uninitialized remote reference on processor 'n'.

"),

(E"Distributed Arrays",E"darray",E"darray(init, type, dims[, distdim, procs, dist])

   Construct a distributed array. 'init' is a function of three
   arguments that will run on each processor, and should return an
   'Array' holding the local data for the current processor. Its
   arguments are '(T,d,da)' where 'T' is the element type, 'd' is the
   dimensions of the needed local piece, and 'da' is the new 'DArray'
   being constructed (though, of course, it is not fully initialized).
   'type' is the element type. 'dims' is the dimensions of the entire
   'DArray'. 'distdim' is the dimension to distribute in. 'procs' is a
   vector of processor ids to use. 'dist' is a vector giving the first
   index of each contiguous distributed piece, such that the nth piece
   consists of indexes 'dist[n]' through 'dist[n+1]-1'. If you have a
   vector 'v' of the sizes of the pieces, 'dist' can be computed as
   'cumsum([1,v])'. Fortunately, all arguments after 'dims' are
   optional.

"),

(E"Distributed Arrays",E"darray",E"darray(f, A)

   Transform 'DArray' 'A' to another of the same type and distribution
   by applying function 'f' to each block of 'A'.

"),

(E"Distributed Arrays",E"dzeros",E"dzeros([type], dims, ...)

   Construct a distributed array of zeros. Trailing arguments are the
   same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"dones",E"dones([type], dims, ...)

   Construct a distributed array of ones. Trailing arguments are the
   same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"dfill",E"dfill(x, dims, ...)

   Construct a distributed array filled with value 'x'. Trailing
   arguments are the same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"drand",E"drand(dims, ...)

   Construct a distributed uniform random array. Trailing arguments
   are the same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"drandn",E"drandn(dims, ...)

   Construct a distributed normal random array. Trailing arguments are
   the same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"dcell",E"dcell(dims, ...)

   Construct a distributed cell array. Trailing arguments are the same
   as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"distribute",E"distribute(a[, distdim])

   Convert a local array to distributed

"),

(E"Distributed Arrays",E"localize",E"localize(d)

   Get the local piece of a distributed array

"),

(E"Distributed Arrays",E"changedist",E"changedist(d, distdim)

   Change the distributed dimension of a 'DArray'

"),

(E"Distributed Arrays",E"myindexes",E"myindexes(d)

   A tuple describing the indexes owned by the local processor

"),

(E"Distributed Arrays",E"owner",E"owner(d, i)

   Get the id of the processor holding index 'i' in the distributed
   dimension

"),

(E"Distributed Arrays",E"procs",E"procs(d)

   Get the vector of processors storing pieces of 'd'

"),

(E"Distributed Arrays",E"distdim",E"distdim(d)

   Get the distributed dimension of 'd'

"),

(E"System",E"system",E"system('command')

   Run a shell command.

"),

(E"System",E"gethostname",E"gethostname()

   Get the local machine's host name.

"),

(E"System",E"getipaddr",E"getipaddr()

   Get the IP address of the local machine, as a string of the form
   'x.x.x.x'.

"),

(E"System",E"cwd",E"cwd()

   Get the current working directory.

"),

(E"System",E"cd",E"cd('dir')

   Set the current working directory. Returns the new current
   directory.

"),

(E"System",E"getpid",E"getpid()

   Get julia's process ID.

"),

(E"System",E"time",E"time()

   Get the time in seconds since the epoch, with fairly high
   (typically, microsecond) resolution.

"),

(E"System",E"time_ns",E"time_ns()

   Get the time in nanoseconds. The time corresponding to 0 is
   undefined, and wraps every 5.8 years.

"),

(E"System",E"tic",E"tic()

   Set a timer to be read by the next call to 'toc' or 'toq'. The
   macro call '@time expr' can also be used to time evaluation.

"),

(E"System",E"toc",E"toc()

   Print and return the time elapsed since the last 'tic'

"),

(E"System",E"toq",E"toq()

   Return, but do not print, the time elapsed since the last 'tic'

"),

(E"System",E"EnvHash",E"EnvHash()

   A singleton of this type, 'ENV', provides a hash table interface to
   environment variables.

"),

(E"System",E"dlopen",E"dlopen(libfile)

   Load a shared library, returning an opaque handle

"),

(E"System",E"dlsym",E"dlsym(handle, sym)

   Look up a symbol from a shared library handle

"),

(E"Errors",E"error",E"error(message)

   Raise an error with the given message

"),

(E"Errors",E"throw",E"throw(e)

   Throw an object as an exception

"),

(E"Errors",E"errno",E"errno()

   Get the value of the C library's 'errno'

"),

(E"Errors",E"strerror",E"strerror(n)

   Convert a system call error code to a descriptive string

"),

(E"Errors",E"assert",E"assert(cond)

   Raise an error if 'cond' is false. Also available as the macro
   '@assert expr'.

"),

(E"Tasks",E"Task",E"Task(func)

   Create a 'Task' (i.e. thread, or coroutine) to execute the given
   function. The task exits when this function returns.

"),

(E"Tasks",E"yieldto",E"yieldto(task, args...)

   Switch to the given task. The first time a task is switched to, the
   task's function is called with 'args'. On subsequent switches,
   'args' are returned from the task's last call to 'yieldto'.

"),

(E"Tasks",E"current_task",E"current_task()

   Get the currently running Task.

"),

(E"Tasks",E"istaskdone",E"istaskdone(task)

   Tell whether a task has exited.

"),

(E"Tasks",E"consume",E"consume(task)

   Receive the next value passed to 'produce' by the specified task.

"),

(E"Tasks",E"produce",E"produce(value)

   Send the given value to the last 'consume' call, switching to the
   consumer task.

"),

(E"Tasks",E"make_scheduled",E"make_scheduled(task)

   Register a task with the main event loop, so it will automatically
   run when possible.

"),

(E"Tasks",E"yield",E"yield()

   For scheduled tasks, switch back to the scheduler to allow another
   scheduled task to run.

"),

(E"Tasks",E"tls",E"tls(symbol)

   Look up the value of a symbol in the current task's task-local
   storage.

"),

(E"Tasks",E"tls",E"tls(symbol, value)

   Assign a value to a symbol in the current task's task-local
   storage.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.copy!(n, X, incx, Y, incy)

   Copy 'n' elements of array 'X' with stride 'incx' to array 'Y' with
   stride 'incy'.  Returns 'Y'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of 'n' elements of array 'X'
   with stride 'incx' and 'n' elements of array 'Y' with stride
   'incy'.  There are no 'BLAS.dot' methods for 'Complex' arrays.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.nrm2(n, X, incx)

   2-norm of a vector consisting of 'n' elements of array 'X' with
   stride 'incx'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.axpy!(n, a, X, incx, Y, incy)

   Overwrite 'Y' with 'a*X + Y'.  Returns 'Y'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix 'C' as 'alpha*A*A.' + beta*C'
   or 'alpha*A.'*A + beta*C' according to whether 'trans' is 'N' or
   'T'.  When 'uplo' is 'U' the upper triangle of 'C' is updated ('L'
   for lower triangle).  Returns 'C'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according
   to 'uplo' ('U' or 'L'), of 'alpha*A*A.'' or 'alpha*A.'*A',
   according to 'trans' ('N' or 'T').

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian
   matrix 'C' as 'alpha*A*A' + beta*C' or 'alpha*A'*A + beta*C'
   according to whether 'trans' is 'N' or 'T'.  When 'uplo' is 'U' the
   upper triangle of 'C' is updated ('L' for lower triangle). Returns
   'C'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle
   or the lower triangle, according to 'uplo' ('U' or 'L'), of
   'alpha*A*A'' or 'alpha*A'*A', according to 'trans' ('N' or 'T').

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector 'y' as 'alpha*A*x + beta*y' or 'alpha*A'*x + beta*y'
   according to 'trans' ('N' or 'T').  The matrix 'A' is a general
   band matrix of dimension 'm' by 'size(A,2)' with 'kl' sub-diagonals
   and 'ku' super-diagonals. Returns the updated 'y'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns 'alpha*A*x' or 'alpha*A'*x' according to 'trans' ('N' or
   'T'). The matrix 'A' is a general band matrix of dimension 'm' by
   'size(A,2)' with 'kl' sub-diagonals and 'ku' super-diagonals.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector 'y' as 'alpha*A*x + beta*y' where 'A' is a a
   symmetric band matrix of order 'size(A,2)' with 'k' super-diagonals
   stored in the argument 'A'.  The storage layout for 'A' is
   described the reference BLAS module, level-2 BLAS at
   *<http://www.netlib.org/lapack/explore-html/>*.

   Returns the updated 'y'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.sbmv(uplo, k, alpha, A, x)

   Returns 'alpha*A*x' where 'A' is a symmetric band matrix of order
   'size(A,2)' with 'k' super-diagonals stored in the argument 'A'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.gemm!(tA, tB, alpha, A, B, beta, C)

   Update 'C' as 'alpha*A*B + beta*C' or the other three variants
   according to 'tA' (transpose 'A') and 'tB'.  Returns the updated
   'C'.

"),

(E"blas.jl --- Basic Linear Algebra Subroutines",E"BLAS",E"BLAS.gemm(tA, tB, alpha, A, B)

   Returns 'alpha*A*B' or the other three variants according to 'tA'
   (transpose 'A') and 'tB'.

"),

(E"cpp.jl",E"cpp",E"cpp()

   Suppose you have a C++ shared library, 'libdemo', which contains a
   function 'timestwo':

      int timestwo(int x) {
        return 2*x;
      }

      double timestwo(double x) {
        return 2*x;
      }

   You can use these functions by placing the '@cpp' macro prior to a
   ccall, for example:

      mylib = dlopen('libdemo')
      x = 3.5
      x2 = @cpp ccall(dlsym(mylib, :timestwo), Float64, (Float64,), x)
      y = 3
      y2 = @cpp ccall(dlsym(mylib, :timestwo), Int, (Int,), y)

   The macro performs C++ ABI name-mangling (using the types of the
   parameters) to determine the correct library symbol.

   Like 'ccall', this performs library calls without overhead.
   However, currently it has a number of limitations:

   * It does not support pure-header libraries

   * The restrictions of 'ccall' apply here; for example, there is no
     support for 'struct'. Consequently it is not possible to use C++
     objects.

   * Currently there is no C++ namespace support

   * Currently there is no support for templated functions

   * Currently only g++ is supported

   The latter three may not be difficult to fix.

"),

(E"fitsio.jl",E"fits_create_file",E"fits_create_file(filename::String)

   Create and open a new empty output FITS file.

"),

(E"fitsio.jl",E"fits_clobber_file",E"fits_clobber_file(filename::String)

   Like fits_create_file, but overwrites 'filename' if it exists.

"),

(E"fitsio.jl",E"fits_open_file",E"fits_open_file(filename::String)

   Open an existing data file.

"),

(E"fitsio.jl",E"fits_close_file",E"fits_close_file(f::FITSFile)

   Close a previously opened FITS file.

"),

(E"fitsio.jl",E"fits_get_hdrspace",E"fits_get_hdrspace(f::FITSFile) -> (keysexist, morekeys)

   Return the number of existing keywords (not counting the END
   keyword) and the amount of space currently available for more
   keywords.

"),

(E"fitsio.jl",E"fits_read_keyword",E"fits_read_keyword(f::FITSFile, keyname::String) -> (value, comment)

   Return the specified keyword.

"),

(E"fitsio.jl",E"fits_read_record",E"fits_read_record(f::FITSFile, keynum::Int) -> String

   Return the nth header record in the CHU. The first keyword in the
   header is at keynum = 1.

"),

(E"fitsio.jl",E"fits_read_keyn",E"fits_read_keyn(f::FITSFile, keynum::Int) -> (name, value, comment)

   Return the nth header record in the CHU. The first keyword in the
   header is at keynum = 1.

"),

(E"fitsio.jl",E"fits_write_key",E"fits_write_key(f::FITSFile, keyname::String, value, comment::String)

   Write a keyword of the appropriate data type into the CHU.

"),

(E"fitsio.jl",E"fits_write_record",E"fits_write_record(f::FITSFile, card::String)

   Write a user specified keyword record into the CHU.

"),

(E"fitsio.jl",E"fits_delete_record",E"fits_delete_record(f::FITSFile, keynum::Int)

   Delete the keyword record at the specified index.

"),

(E"fitsio.jl",E"fits_delete_key",E"fits_delete_key(f::FITSFile, keyname::String)

   Delete the keyword named *keyname*.

"),

(E"fitsio.jl",E"fits_get_img_size",E"fits_get_img_size(f::FITSFile)

   Get the dimensions of the image.

"),

(E"fitsio.jl",E"fits_create_img",E"fits_create_img(f::FITSFile, t::Type, naxes::Vector{Int})

   Create a new primary array or IMAGE extension with a specified data
   type and size.

"),

(E"fitsio.jl",E"fits_write_pix",E"fits_write_pix(f::FITSFile, fpixel::Vector{Int}, nelements::Int, data::Array)

   Write pixels from *data* into the FITS file.

"),

(E"fitsio.jl",E"fits_read_pix",E"fits_read_pix(f::FITSFile, fpixel::Vector{Int}, nelements::Int, data::Array)

   Read pixels from the FITS file into 'data'.

"),

(E"glpk.jl",E"glp_set_prob_name",E"glp_set_prob_name(glp_prob, name)

   Assigns a name to the problem object (or deletes it if 'name' is
   empty or 'nothing').

"),

(E"glpk.jl",E"glp_set_obj_name",E"glp_set_obj_name(glp_prob, name)

   Assigns a name to the objective function (or deletes it if 'name'
   is empty or 'nothing').

"),

(E"glpk.jl",E"glp_set_obj_dir",E"glp_set_obj_dir(glp_prob, dir)

   Sets the optimization direction, 'GLP_MIN' (minimization) or
   'GLP_MAX' (maximization).

"),

(E"glpk.jl",E"glp_add_rows",E"glp_add_rows(glp_prob, rows)

   Adds the given number of rows (constraints) to the problem object;
   returns the number of the first new row added.

"),

(E"glpk.jl",E"glp_add_cols",E"glp_add_cols(glp_prob, cols)

   Adds the given number of columns (structural variables) to the
   problem object; returns the number of the first new column added.

"),

(E"glpk.jl",E"glp_set_row_name",E"glp_set_row_name(glp_prob, row, name)

   Assigns a name to the specified row (or deletes it if 'name' is
   empty or 'nothing').

"),

(E"glpk.jl",E"glp_set_col_name",E"glp_set_col_name(glp_prob, col, name)

   Assigns a name to the specified column (or deletes it if 'name' is
   empty or 'nothing').

"),

(E"glpk.jl",E"glp_set_row_bnds",E"glp_set_row_bnds(glp_prob, row, bounds_type, lb, ub)

   Sets the type and bounds on a row. 'type' must be one of 'GLP_FR'
   (free), 'GLP_LO' (lower bounded), 'GLP_UP' (upper bounded),
   'GLP_DB' (double bounded), 'GLP_FX' (fixed).

   At initialization, each row is free.

"),

(E"glpk.jl",E"glp_set_col_bnds",E"glp_set_col_bnds(glp_prob, col, bounds_type, lb, ub)

   Sets the type and bounds on a column. 'type' must be one of
   'GLP_FR' (free), 'GLP_LO' (lower bounded), 'GLP_UP' (upper
   bounded), 'GLP_DB' (double bounded), 'GLP_FX' (fixed).

   At initialization, each column is fixed at 0.

"),

(E"glpk.jl",E"glp_set_obj_coef",E"glp_set_obj_coef(glp_prob, col, coef)

   Sets the objective coefficient to a column ('col' can be 0 to
   indicate the constant term of the objective function).

"),

(E"glpk.jl",E"glp_set_mat_row",E"glp_set_mat_row(glp_prob, row[, len], ind, val)

   Sets (replaces) the content of a row. The content is specified in
   sparse format: 'ind' is a vector of indices, 'val' is the vector of
   corresponding values. 'len' is the number of vector elements which
   will be considered, and must be less or equal to the length of both
   'ind' and 'val'.  If 'len' is 0, 'ind' and/or 'val' can be
   'nothing'.

   In Julia, 'len' can be omitted, and then it is inferred from 'ind'
   and 'val' (which need to have the same length in such case).

"),

(E"glpk.jl",E"glp_set_mat_col",E"glp_set_mat_col(glp_prob, col[, len], ind, val)

   Sets (replaces) the content of a column. Everything else is like
   'glp_set_mat_row'.

"),

(E"glpk.jl",E"glp_load_matrix",E"glp_load_matrix(glp_prob[, numel], ia, ja, ar)
glp_load_matrix(glp_prob, A)

   Sets (replaces) the content matrix (i.e. sets all  rows/coluns at
   once). The matrix is passed in sparse format.

   In the first form (original C API), it's passed via 3 vectors: 'ia'
   and 'ja' are for rows/columns indices, 'ar' is for values. 'numel'
   is the number of elements which will be read and must be less or
   equal to the length of any of the 3 vectors. If 'numel' is 0, any
   of the vectors can be passed as 'nothing'.

   In Julia, 'numel' can be omitted, and then it is inferred from
   'ia', 'ja' and 'ar' (which need to have the same length in such
   case).

   Also, in Julia there's a second, simpler calling form, in which the
   matrix is passed as a 'SparseMatrixCSC' object.

"),

(E"glpk.jl",E"glp_check_dup",E"glp_check_dup(rows, cols[, numel], ia, ja)

   Check for duplicates in the indices vectors 'ia' and 'ja'. 'numel'
   has the same meaning and (optional) use as in 'glp_load_matrix'.
   Returns 0 if no duplicates/out-of-range indices are found, or a
   positive number indicating where a duplicate occurs, or a negative
   number indicating an out-of-bounds index.

"),

(E"glpk.jl",E"glp_sort_matrix",E"glp_sort_matrix(glp_prob)

   Sorts the elements of the problem object's matrix.

"),

(E"glpk.jl",E"glp_del_rows",E"glp_del_rows(glp_prob[, num_rows], rows_ids)

   Deletes rows from the problem object. Rows are specified in the
   'rows_ids' vector. 'num_rows' is the number of elements of
   'rows_ids' which will be considered, and must be less or equal to
   the length id 'rows_ids'. If 'num_rows' is 0, 'rows_ids' can be
   'nothing'. In Julia, 'num_rows' is optional (it's inferred from
   'rows_ids' if not given).

"),

(E"glpk.jl",E"glp_del_cols",E"glp_del_cols(glp_prob, cols_ids)

   Deletes columns from the problem object. See 'glp_del_rows'.

"),

(E"glpk.jl",E"glp_copy_prob",E"glp_copy_prob(glp_prob_dest, glp_prob, copy_names)

   Makes a copy of the problem object. The flag 'copy_names'
   determines if names are copied, and must be either 'GLP_ON' or
   'GLP_OFF'.

"),

(E"glpk.jl",E"glp_erase_prob",E"glp_erase_prob(glp_prob)

   Resets the problem object.

"),

(E"glpk.jl",E"glp_get_prob_name",E"glp_get_prob_name(glp_prob)

   Returns the problem object's name. Unlike the C version, if the
   problem has no assigned name, returns an empty string.

"),

(E"glpk.jl",E"glp_get_obj_name",E"glp_get_obj_name(glp_prob)

   Returns the objective function's name. Unlike the C version, if the
   objective has no assigned name, returns an empty string.

"),

(E"glpk.jl",E"glp_get_obj_dir",E"glp_get_obj_dir(glp_prob)

   Returns the optimization direction, 'GLP_MIN' (minimization) or
   'GLP_MAX' (maximization).

"),

(E"glpk.jl",E"glp_get_num_rows",E"glp_get_num_rows(glp_prob)

   Returns the current number of rows.

"),

(E"glpk.jl",E"glp_get_num_cols",E"glp_get_num_cols(glp_prob)

   Returns the current number of columns.

"),

(E"glpk.jl",E"glp_get_row_name",E"glp_get_row_name(glp_prob, row)

   Returns the name of the specified row. Unlike the C version, if the
   row has no assigned name, returns an empty string.

"),

(E"glpk.jl",E"glp_get_col_name",E"glp_get_col_name(glp_prob, col)

   Returns the name of the specified column. Unlike the C version, if
   the column has no assigned name, returns an empty string.

"),

(E"glpk.jl",E"glp_get_row_type",E"glp_get_row_type(glp_prob, row)

   Returns the type of the specified row: 'GLP_FR' (free), 'GLP_LO'
   (lower bounded), 'GLP_UP' (upper bounded), 'GLP_DB' (double
   bounded), 'GLP_FX' (fixed).

"),

(E"glpk.jl",E"glp_get_row_lb",E"glp_get_row_lb(glp_prob, row)

   Returns the lower bound of the specified row, '-DBL_MAX' if
   unbounded.

"),

(E"glpk.jl",E"glp_get_row_ub",E"glp_get_row_ub(glp_prob, row)

   Returns the upper bound of the specified row, '+DBL_MAX' if
   unbounded.

"),

(E"glpk.jl",E"glp_get_col_type",E"glp_get_col_type(glp_prob, col)

   Returns the type of the specified column: 'GLP_FR' (free), 'GLP_LO'
   (lower bounded), 'GLP_UP' (upper bounded), 'GLP_DB' (double
   bounded), 'GLP_FX' (fixed).

"),

(E"glpk.jl",E"glp_get_col_lb",E"glp_get_col_lb(glp_prob, col)

   Returns the lower bound of the specified column, '-DBL_MAX' if
   unbounded.

"),

(E"glpk.jl",E"glp_get_col_ub",E"glp_get_col_ub(glp_prob, col)

   Returns the upper bound of the specified column, '+DBL_MAX' if
   unbounded.

"),

(E"glpk.jl",E"glp_get_obj_coef",E"glp_get_obj_coef(glp_prob, col)

   Return the objective coefficient to a column ('col' can be 0 to
   indicate the constant term of the objective function).

"),

(E"glpk.jl",E"glp_get_num_nz",E"glp_get_num_nz(glp_prob)

   Return the number of non-zero elements in the constraint matrix.

"),

(E"glpk.jl",E"glp_get_mat_row",E"glp_get_mat_row(glp_prob, row, ind, val)
glp_get_mat_row(glp_prob, row)

   Returns the contents of a row. In the first form (original C API),
   it fills the 'ind' and 'val' vectors provided, which must be of
   type 'Vector{Int32}' and 'Vector{Float64}' respectively, and have a
   sufficient length to hold the result (or they can be empty or
   'nothing', and then they're not filled). It returns the length of
   the result.

   In Julia, there's a second, simpler calling form which allocates
   and returns the two vectors as '(ind, val)'.

"),

(E"glpk.jl",E"glp_get_mat_col",E"glp_get_mat_col(glp_prob, col, ind, val)
glp_get_mat_col(glp_prob, col)

   Returns the contents of a column. See 'glp_get_mat_row'.

"),

(E"glpk.jl",E"glp_create_index",E"glp_create_index(glp_prob)

   Creates the name index (used by 'glp_find_row', 'glp_find_col') for
   the problem object.

"),

(E"glpk.jl",E"glp_find_row",E"glp_find_row(glp_prob, name)

   Finds the numeric id of a row by name. Returns 0 if no row with the
   given name is found.

"),

(E"glpk.jl",E"glp_find_col",E"glp_find_col(glp_prob, name)

   Finds the numeric id of a column by name. Returns 0 if no column
   with the given name is found.

"),

(E"glpk.jl",E"glp_delete_index",E"glp_delete_index(glp_prob)

   Deletes the name index for the problem object.

"),

(E"glpk.jl",E"glp_set_rii",E"glp_set_rii(glp_prob, row, rii)

   Sets the rii scale factor for the specified row.

"),

(E"glpk.jl",E"glp_set_sjj",E"glp_set_sjj(glp_prob, col, sjj)

   Sets the sjj scale factor for the specified column.

"),

(E"glpk.jl",E"glp_get_rii",E"glp_get_rii(glp_prob, row)

   Returns the rii scale factor for the specified row.

"),

(E"glpk.jl",E"glp_get_sjj",E"glp_get_sjj(glp_prob, col)

   Returns the sjj scale factor for the specified column.

"),

(E"glpk.jl",E"glp_scale_prob",E"glp_scale_prob(glp_prob, flags)

   Performs automatic scaling of problem data for the problem object.
   The parameter 'flags' can be 'GLP_SF_AUTO' (automatic) or a bitwise
   OR of the forllowing: 'GLP_SF_GM' (geometric mean), 'GLP_SF_EQ'
   (equilibration), 'GLP_SF_2N' (nearest power of 2), 'GLP_SF_SKIP'
   (skip if well scaled).

"),

(E"glpk.jl",E"glp_unscale_prob",E"glp_unscale_prob(glp_prob)

   Unscale the problem data (cancels the scaling effect).

"),

(E"glpk.jl",E"glp_set_row_stat",E"glp_set_row_stat(glp_prob, row, stat)

   Sets the status of the specified row. 'stat' must be one of:
   'GLP_BS' (basic), 'GLP_NL' (non-basic lower bounded), 'GLP_NU'
   (non-basic upper-bounded), 'GLP_NF' (non-basic free), 'GLP_NS'
   (non-basic fixed).

"),

(E"glpk.jl",E"glp_set_col_stat",E"glp_set_col_stat(glp_prob, col, stat)

   Sets the status of the specified column. 'stat' must be one of:
   'GLP_BS' (basic), 'GLP_NL' (non-basic lower bounded), 'GLP_NU'
   (non-basic upper-bounded), 'GLP_NF' (non-basic free), 'GLP_NS'
   (non-basic fixed).

"),

(E"glpk.jl",E"glp_std_basis",E"glp_std_basis(glp_prob)

   Constructs the standard (trivial) initial LP basis for the problem
   object.

"),

(E"glpk.jl",E"glp_adv_basis",E"glp_adv_basis(glp_prob[, flags])

   Constructs an advanced initial LP basis for the problem object. The
   flag 'flags' is optional; it must be 0 if given.

"),

(E"glpk.jl",E"glp_cpx_basis",E"glp_cpx_basis(glp_prob)

   Constructs an initial LP basis for the problem object with the
   algorithm proposed by R. Bixby.

"),

(E"glpk.jl",E"glp_simplex",E"glp_simplex(glp_prob[, glp_param])

   The routine 'glp_simplex' is a driver to the LP solver based on the
   simplex method. This routine retrieves problem data from the
   specified problem object, calls the solver to solve the problem
   instance, and stores results of computations back into the problem
   object.

   The parameters are specified via the optional 'glp_param' argument,
   which is of type 'GLPSimplexParam' (or 'nothing' to use the default
   settings).

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: 'GLP_EBADB' (invalid base), 'GLP_ESING'
   (singular matrix), 'GLP_ECOND' (ill-conditioned matrix),
   'GLP_EBOUND' (incorrect bounds), 'GLP_EFAIL' (solver failure),
   'GLP_EOBJLL' (lower limit reached), 'GLP_EOBJUL' (upper limit
   reached), 'GLP_ITLIM' (iterations limit exceeded), 'GLP_ETLIM'
   (time limit exceeded), 'GLP_ENOPFS' (no primal feasible solution),
   'GLP_ENODFS' (no dual feasible solution).

"),

(E"glpk.jl",E"glp_exact",E"glp_exact(glp_prob[, glp_param])

   A tentative implementation of the primal two-phase simplex method
   based on exact (rational) arithmetic. Similar to 'glp_simplex'. The
   optional glp_param is of type 'GLPSimplexParam'.

   The possible return values are 0 (success) or 'GLP_EBADB',
   'GLP_ESING', 'GLP_EBOUND', 'GLP_EFAIL', 'GLP_ITLIM', 'GLP_ETLIM'
   (see 'glp_simplex').

"),

(E"glpk.jl",E"glp_init_smcp",E"glp_init_smcp(glp_param)

   Initializes a 'GLPSimplexParam' object with the default values. In
   Julia, this is done at object creation time; this function can be
   used to reset the object.

"),

(E"glpk.jl",E"glp_get_status",E"glp_get_status(glp_prob)

   Returns the generic status of the current basic solution: 'GLP_OPT'
   (optimal), 'GLP_FEAS' (feasible), 'GLP_INFEAS' (infeasible),
   'GLP_NOFEAS' (no feasible solution), 'GLP_UNBND' (unbounded
   solution), 'GLP_UNDEF' (undefined).

"),

(E"glpk.jl",E"glp_get_prim_stat",E"glp_get_prim_stat(glp_prob)

   Returns the status of the primal basic solution: 'GLP_FEAS',
   'GLP_INFEAS', 'GLP_NOFEAS', 'GLP_UNDEF' (see 'glp_get_status').

"),

(E"glpk.jl",E"glp_get_dual_stat",E"glp_get_dual_stat(glp_prob)

   Returns the status of the dual basic solution: 'GLP_FEAS',
   'GLP_INFEAS', 'GLP_NOFEAS', 'GLP_UNDEF' (see 'glp_get_status').

"),

(E"glpk.jl",E"glp_get_obj_val",E"glp_get_obj_val(glp_prob)

   Returns the current value of the objective function.

"),

(E"glpk.jl",E"glp_get_row_stat",E"glp_get_row_stat(glp_prob, row)

   Returns the status of the specified row: 'GLP_BS', 'GLP_NL',
   'GLP_NU', 'GLP_NF', 'GLP_NS' (see 'glp_set_row_stat').

"),

(E"glpk.jl",E"glp_get_row_prim",E"glp_get_row_prim(glp_prob, row)

   Returns the primal value of the specified row.

"),

(E"glpk.jl",E"glp_get_row_dual",E"glp_get_row_dual(glp_prob, row)

   Returns the dual value (reduced cost) of the specified row.

"),

(E"glpk.jl",E"glp_get_col_stat",E"glp_get_col_stat(glp_prob, col)

   Returns the status of the specified column: 'GLP_BS', 'GLP_NL',
   'GLP_NU', 'GLP_NF', 'GLP_NS' (see 'glp_set_row_stat').

"),

(E"glpk.jl",E"glp_get_col_prim",E"glp_get_col_prim(glp_prob, col)

   Returns the primal value of the specified column.

"),

(E"glpk.jl",E"glp_get_col_dual",E"glp_get_col_dual(glp_prob, col)

   Returns the dual value (reduced cost) of the specified column.

"),

(E"glpk.jl",E"glp_get_unbnd_ray",E"glp_get_unbnd_ray(glp_prob)

   Returns the number k of a variable, which causes primal or dual
   unboundedness (if 1 <= k <= rows it's row k; if rows+1 <= k <=
   rows+cols it's column k-rows, if k=0 such variable is not defined).

"),

(E"glpk.jl",E"glp_interior",E"glp_interior(glp_prob[, glp_param])

   The routine 'glp_interior' is a driver to the LP solver based on
   the primal-dual interior-point method. This routine retrieves
   problem data from the specified problem object, calls the solver to
   solve the problem instance, and stores results of computations back
   into the problem object.

   The parameters are specified via the optional 'glp_param' argument,
   which is of type 'GLPInteriorParam' (or 'nothing' to use the
   default settings).

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: 'GLP_EFAIL' (solver failure), 'GLP_ENOCVG'
   (very slow convergence, or divergence), 'GLP_ITLIM' (iterations
   limit exceeded), 'GLP_EINSTAB' (numerical instability).

"),

(E"glpk.jl",E"glp_init_iptcp",E"glp_init_iptcp(glp_param)

   Initializes a 'GLPInteriorParam' object with the default values. In
   Julia, this is done at object creation time; this function can be
   used to reset the object.

"),

(E"glpk.jl",E"glp_ipt_status",E"glp_ipt_status(glp_prob)

   Returns the status of the interior-point solution: 'GLP_OPT'
   (optimal), 'GLP_INFEAS' (infeasible), 'GLP_NOFEAS' (no feasible
   solution), 'GLP_UNDEF' (undefined).

"),

(E"glpk.jl",E"glp_ipt_obj_val",E"glp_ipt_obj_val(glp_prob)

   Returns the current value of the objective function for the
   interior-point solution.

"),

(E"glpk.jl",E"glp_ipt_row_prim",E"glp_ipt_row_prim(glp_prob, row)

   Returns the primal value of the specified row for the interior-
   point solution.

"),

(E"glpk.jl",E"glp_ipt_row_dual",E"glp_ipt_row_dual(glp_prob, row)

   Returns the dual value (reduced cost) of the specified row for the
   interior-point solution.

"),

(E"glpk.jl",E"glp_ipt_col_prim",E"glp_ipt_col_prim(glp_prob, col)

   Returns the primal value of the specified column for the interior-
   point solution.

"),

(E"glpk.jl",E"glp_ipt_col_dual",E"glp_ipt_col_dual(glp_prob, col)

   Returns the dual value (reduced cost) of the specified column for
   the interior-point solution.

"),

(E"glpk.jl",E"glp_set_col_kind",E"glp_set_col_kind(glp_prob, col, kind)

   Sets the kind for the specified column (for mixed-integer
   programming). 'kind' must be one of: 'GLP_CV' (continuous),
   'GLP_IV' (integer), 'GLP_BV' (binary, 0/1).

"),

(E"glpk.jl",E"glp_get_col_kind",E"glp_get_col_kind(glp_prob, col)

   Returns the kind for the specified column (see *glp_set_col_kind*).

"),

(E"glpk.jl",E"glp_get_num_int",E"glp_get_num_int(glp_prob)

   Returns the number of columns marked as integer (including binary).

"),

(E"glpk.jl",E"glp_get_num_bin",E"glp_get_num_bin(glp_prob)

   Returns the number of columns marked binary.

"),

(E"glpk.jl",E"glp_intopt",E"glp_intopt(glp_prob[, glp_param])

   The routine 'glp_intopt' is a driver to the mixed-integer-
   programming (MIP) solver based on the branch- and-cut method, which
   is a hybrid of branch-and-bound and cutting plane methods.

   The parameters are specified via the optional 'glp_param' argument,
   which is of type 'GLPIntoptParam' (or 'nothing' to use the default
   settings).

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: 'GLP_EBOUND' (incorrect bounds), 'GLP_EROOT'
   (no optimal LP basis given), 'GLP_ENOPFS' (no primal feasible LP
   solution), 'GLP_ENODFS' (no dual feasible LP solution), 'GLP_EFAIL'
   (solver failure), 'GLP_EMIPGAP' (mip gap tolearance reached),
   'GLP_ETLIM' (time limit exceeded), 'GLP_ESTOP' (terminated by
   application).

"),

(E"glpk.jl",E"glp_init_iocp",E"glp_init_iocp(glp_param)

   Initializes a 'GLPIntoptParam' object with the default values. In
   Julia, this is done at object creation time; this function can be
   used to reset the object.

"),

(E"glpk.jl",E"glp_mip_status",E"glp_mip_status(glp_prob)

   Returns the generic status of the MIP solution: 'GLP_OPT'
   (optimal), 'GLP_FEAS' (feasible), 'GLP_NOFEAS' (no feasible
   solution), 'GLP_UNDEF' (undefined).

"),

(E"glpk.jl",E"glp_mip_obj_val",E"glp_mip_obj_val(glp_prob)

   Returns the current value of the objective function for the MIP
   solution.

"),

(E"glpk.jl",E"glp_mip_row_val",E"glp_mip_row_val(glp_prob, row)

   Returns the value of the specified row for the MIP solution.

"),

(E"glpk.jl",E"glp_mip_col_val",E"glp_mip_col_val(glp_prob, col)

   Returns the value of the specified column for the MIP solution.

"),

(E"glpk.jl",E"glp_read_mps",E"glp_read_mps(glp_prob, format[, param], filename)

   Reads problem data in MPS format from a text file. 'format' must be
   one of 'GLP_MPS_DECK' (fixed, old) or 'GLP_MPS_FILE' (free,
   modern). 'param' is optional; if given it must be 'nothing'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_write_mps",E"glp_write_mps(glp_prob, format[, param], filename)

   Writes problem data in MPS format from a text file. See
   'glp_read_mps'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_read_lp",E"glp_read_lp(glp_prob[, param], filename)

   Reads problem data in CPLEX LP format from a text file. 'param' is
   optional; if given it must be 'nothing'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_write_lp",E"glp_write_lp(glp_prob[, param], filename)

   Writes problem data in CPLEX LP format from a text file. See
   'glp_read_lp'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_read_prob",E"glp_read_prob(glp_prob[, flags], filename)

   Reads problem data in GLPK LP/MIP format from a text file. 'flags'
   is optional; if given it must be 0.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_write_prob",E"glp_write_prob(glp_prob[, flags], filename)

   Writes problem data in GLPK LP/MIP format from a text file. See
   'glp_read_prob'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_mpl_read_model",E"glp_mpl_read_model(glp_tran, filename, skip)

   Reads the model section and, optionally, the data section, from a
   text file in MathProg format, and stores it in 'glp_tran', which is
   a 'GLPMathProgWorkspace' object. If 'skip' is nonzero, the data
   section is skipped if present.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_mpl_read_data",E"glp_mpl_read_data(glp_tran, filename)

   Reads data section from a text file in MathProg format and stores
   it in 'glp_tran', which is a 'GLPMathProgWorkspace' object. May be
   called more than once.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_mpl_generate",E"glp_mpl_generate(glp_tran[, filename])

   Generates the model using its description stored in the
   'GLPMathProgWorkspace' translator workspace 'glp_tran'. The
   optional 'filename' specifies an output file; if not given or
   'nothing', the terminal is used.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_mpl_build_prob",E"glp_mpl_build_prob(glp_tran, glp_prob)

   Transfer information from the 'GLPMathProgWorkspace' translator
   workspace 'glp_tran' to the 'GLPProb' problem object 'glp_prob'.

"),

(E"glpk.jl",E"glp_mpl_postsolve",E"glp_mpl_postsolve(glp_tran, glp_prob, sol)

   Copies the solution from the 'GLPProb' problem object 'glp_prob' to
   the 'GLPMathProgWorkspace' translator workspace 'glp_tran' and then
   executes all the remaining model statements, which follow the solve
   statement.

   The parameter 'sol' specifies which solution should be copied from
   the problem object to the workspace: 'GLP_SOL' (basic), 'GLP_IPT'
   (interior-point), 'GLP_MIP' (MIP).

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_print_sol",E"glp_print_sol(glp_prob, filename)

   Writes the current basic solution to a text file, in printable
   format.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_read_sol",E"glp_read_sol(glp_prob, filename)

   Reads the current basic solution from a text file, in the format
   used by 'glp_write_sol'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_write_sol",E"glp_write_sol(glp_prob, filename)

   Writes the current basic solution from a text file, in a format
   which can be read by 'glp_read_sol'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_print_ipt",E"glp_print_ipt(glp_prob, filename)

   Writes the current interior-point solution to a text file, in
   printable format.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_read_ipt",E"glp_read_ipt(glp_prob, filename)

   Reads the current interior-point solution from a text file, in the
   format used by 'glp_write_ipt'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_write_ipt",E"glp_write_ipt(glp_prob, filename)

   Writes the current interior-point solution from a text file, in a
   format which can be read by 'glp_read_ipt'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_print_mip",E"glp_print_mip(glp_prob, filename)

   Writes the current MIP solution to a text file, in printable
   format.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_read_mip",E"glp_read_mip(glp_prob, filename)

   Reads the current MIP solution from a text file, in the format used
   by 'glp_write_mip'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_write_mip",E"glp_write_mip(glp_prob, filename)

   Writes the current MIP solution from a text file, in a format which
   can be read by 'glp_read_mip'.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_print_ranges",E"glp_print_ranges(glp_prob, [[len,] list,] [flags,] filename)

   Performs sensitivity analysis of current optimal basic solution and
   writes the analysis report in human-readable format to a text file.
   'list' is a vector specifying the rows/columns to analyze (if 1 <=
   list[i] <= rows, analyzes row list[i]; if rows+1 <= list[i] <=
   rows+cols, analyzes column list[i]-rows). 'len' is the number of
   elements of 'list' which will be consideres, and must be smaller or
   equal to the length of the list. In Julia, 'len' is optional (it's
   inferred from 'len' if not given). 'list' can be empty of 'nothing'
   or not given at all, implying all indices will be analyzed. 'flags'
   is optional, and must be 0 if given.

   To call this function, the current basic solution must be optimal,
   and the basis factorization must exist.

   Returns 0 upon success, non-zero otherwise.

"),

(E"glpk.jl",E"glp_bf_exists",E"glp_bf_exists(glp_prob)

   Returns non-zero if the basis fatorization for the current basis
   exists, 0 otherwise.

"),

(E"glpk.jl",E"glp_factorize",E"glp_factorize(glp_prob)

   Computes the basis factorization for the current basis.

   Returns 0 if successful, otherwise: 'GLP_EBADB' (invalid matrix),
   'GLP_ESING' (singluar matrix), 'GLP_ECOND' (ill-conditioned
   matrix).

"),

(E"glpk.jl",E"glp_bf_updated",E"glp_bf_updated(glp_prob)

   Returns 0 if the basis factorization was computed from scratch,
   non-zero otherwise.

"),

(E"glpk.jl",E"glp_get_bfcp",E"glp_get_bfcp(glp_prob, glp_param)

   Retrieves control parameters, which are used on computing and
   updating the basis factorization associated with the problem
   object, and stores them in the 'GLPBasisFactParam' object
   'glp_param'.

"),

(E"glpk.jl",E"glp_set_bfcp",E"glp_set_bfcp(glp_prob[, glp_param])

   Sets the control parameters stored in the 'GLPBasisFactParam'
   object 'glp_param' into the problem object. If 'glp_param' is
   'nothing' or is omitted, resets the parameters to their defaults.

   The 'glp_param' should always be retreived via 'glp_get_bfcp'
   before changing its values and calling this function.

"),

(E"glpk.jl",E"glp_get_bhead",E"glp_get_bhead(glp_prob, k)

   Returns the basis header information for the current basis. 'k' is
   a row index.

   Returns either i such that 1 <= i <= rows, if 'k' corresponds to
   i-th auxiliary variable, or rows+j such that 1 <= j <= columns, if
   'k' corresponds to the j-th structural variable.

"),

(E"glpk.jl",E"glp_get_row_bind",E"glp_get_row_bind(glp_prob, row)

   Returns the index of the basic variable 'k' which is associated
   with the specified row, or 0 if the variable is non-basic. If
   'glp_get_bhead(glp_prob, k) = row', then 'glp_get_bind(glp_prob,
   row) = k'.

"),

(E"glpk.jl",E"glp_get_col_bind",E"glp_get_col_bind(glp_prob, col)

   Returns the index of the basic variable 'k' which is associated
   with the specified column, or 0 if the variable is non-basic. If
   'glp_get_bhead(glp_prob, k) = rows+col', then
   'glp_get_bind(glp_prob, col) = k'.

"),

(E"glpk.jl",E"glp_ftran",E"glp_ftran(glp_prob, v)

   Performs forward transformation (FTRAN), i.e. it solves the system
   Bx = b, where B is the basis matrix, x is the vector of unknowns to
   be computed, b is the vector of right-hand sides. At input, 'v'
   represents the vector b; at output, it contains the vector x. 'v'
   must be a 'Vector{Float64}' whose length is the number of rows.

"),

(E"glpk.jl",E"glp_btran",E"glp_btran(glp_prob, v)

   Performs backward transformation (BTRAN), i.e. it solves the system
   B'x = b, where B is the transposed of the basis matrix, x is the
   vector of unknowns to be computed, b is the vector of right-hand
   sides. At input, 'v' represents the vector b; at output, it
   contains the vector x. 'v' must be a 'Vector{Float64}' whose length
   is the number of rows.

"),

(E"glpk.jl",E"glp_warm_up",E"glp_warm_up(glp_prob)

   'Warms up' the LP basis using current statuses assigned to rows and
   columns, i.e. computes factorization of the basis matrix (if it
   does not exist), computes primal and dual components of basic
   solution, and determines the solution status.

   Returns 0 if successful, otherwise: 'GLP_EBADB' (invalid matrix),
   'GLP_ESING' (singluar matrix), 'GLP_ECOND' (ill-conditioned
   matrix).

"),

(E"glpk.jl",E"glp_eval_tab_row",E"glp_eval_tab_row(glp_prob, k, ind, val)
glp_eval_tab_row(glp_prob, k)

   Computes a row of the current simplex tableau which corresponds to
   some basic variable specified by the parameter 'k'. If 1 <= 'k' <=
   rows, uses 'k'-th auxiliary variable; if rows+1 <= 'k' <=
   rows+cols, uses ('k'-rows)-th structural variable. The basis
   factorization must exist.

   In the first form, stores the result in the provided vectors 'ind'
   and 'val', which must be of type 'Vector{Int32}' and
   'Vector{Float64}', respectively, and returns the length of the
   outcome; in Julia, the vectors will be resized as needed to hold
   the result.

   In the second, simpler form, 'ind' and 'val' are returned in a
   tuple as the output of the function.

"),

(E"glpk.jl",E"glp_eval_tab_col",E"glp_eval_tab_col(glp_prob, k, ind, val)
glp_eval_tab_col(glp_prob, k)

   Computes a column of the current simplex tableau which corresponds
   to some non-basic variable specified by the parameter 'k'. See
   'glp_eval_tab_row'.

"),

(E"glpk.jl",E"glp_transform_row",E"glp_transform_row(glp_prob[, len], ind, val)

   Performs the same operation as 'glp_eval_tab_row' with the
   exception that the row to be transformed is specified explicitly as
   a sparse vector. The parameter 'len' is the number of elements of
   'ind' and 'val' which will be used, and must be smaller or equal to
   the length of both vectors; in Julia it is optional (and the 'ind'
   and 'val' must have the same length). The vectors 'int' and 'val'
   must be of type 'Vector{Int32}' and 'Vector{Float64}',
   respectively, since they will also hold the result; in Julia, they
   will be resized to the resulting required length.

   Returns the length if the resulting vectors 'ind' and 'val'.

"),

(E"glpk.jl",E"glp_transform_col",E"glp_transform_col(glp_prob[, len], ind, val)

   Performs the same operation as 'glp_eval_tab_col' with the
   exception that the row to be transformed is specified explicitly as
   a sparse vector. See 'glp_transform_row'.

"),

(E"glpk.jl",E"glp_prim_rtest",E"glp_prim_rtest(glp_prob[, len], ind, val, dir, eps)

   Performs the primal ratio test using an explicitly specified column
   of the simplex table. The current basic solution must be primal
   feasible. The column is specified in sparse format by 'len' (length
   of the vector), 'ind' and 'val' (indices and values of the vector).
   'len' is the number of elements which will be considered and must
   be smaller or equal to the length of both 'ind' and 'val'; in
   Julia, it can be omitted (and then 'ind' and 'val' must have the
   same length). The indices in 'ind' must be between 1 and rows+cols;
   they must correspond to basic variables. 'dir' is a direction
   parameter which must be either +1 (increasing) or -1 (decreasing).
   'eps' is a tolerance parameter and must be positive. See the GLPK
   manual for a detailed explanation.

   Returns the position in 'ind' and 'val' which corresponds to the
   pivot element, or 0 if the choice cannot be made.

"),

(E"glpk.jl",E"glp_dual_rtest",E"glp_dual_rtest(glp_prob[, len], ind, val, dir, eps)

   Performs the dual ratio test using an explicitly specified row of
   the simplex table. The current basic solution must be dual
   feasible. The indices in 'ind' must correspond to non-basic
   variables. Everything else is like in 'glp_prim_rtest'.

"),

(E"glpk.jl",E"glp_analyze_bound",E"glp_analyze_bound(glp_prob, k)

   Analyzes the effect of varying the active bound of specified non-
   basic variable. See the GLPK manual for a detailed explanation. In
   Julia, this function has a different API then C. It returns
   '(limit1, var1, limit2, var2)' rather then taking them as pointers
   in the argument list.

"),

(E"glpk.jl",E"glp_analyze_coef",E"glp_analyze_coef(glp_prob, k)

   Analyzes the effect of varying the objective coefficient at
   specified basic variable. See the GLPK manual for a detailed
   explanation. In Julia, this function has a different API then C. It
   returns '(coef1, var1, value1, coef2, var2, value2)' rather then
   taking them as pointers in the argument list.

"),

(E"glpk.jl",E"glp_init_env",E"glp_init_env()

   Initializes the GLPK environment. Not normally needed.

   Returns 0 (initilization successful), 1 (environment already
   initialized), 2 (failed, insufficient memory) or 3 (failed,
   unsupported programming model).

"),

(E"glpk.jl",E"glp_version",E"glp_version()

   Returns the GLPK version number. In Julia, instead of returning a
   string as in C, it returns a tuple of integer values, containing
   the major and the minor number.

"),

(E"glpk.jl",E"glp_free_env",E"glp_free_env()

   Frees all resources used by GLPK routines (memory blocks, etc.)
   which are currently still in use. Not normally needed.

   Returns 0 if successful, 1 if envirnoment is inactive.

"),

(E"glpk.jl",E"glp_term_out",E"glp_term_out(flag)

   Enables/disables the terminal output of glpk routines. 'flag' is
   either 'GLP_ON' (output enabled) or 'GLP_OFF' (output disabled).

   Returns the previous status of the terminal output.

"),

(E"glpk.jl",E"glp_open_tee",E"glp_open_tee(filename)

   Starts copying all the terminal output to an output text file.

   Returns 0 if successful, 1 if already active, 2 if it fails
   creating the output file.

"),

(E"glpk.jl",E"glp_close_tee",E"glp_close_tee()

   Stops copying the terminal output to the output text file
   previously open by the 'glp_open_tee'.

   Return 0 if successful, 1 if copying terminal output was not
   started.

"),

(E"glpk.jl",E"glp_malloc",E"glp_malloc(size)

   Replacement of standard C 'malloc'. Allocates uninitialized memeory
   which must freed with 'glp_free'.

   Returns a pointer to the allocated memory.

"),

(E"glpk.jl",E"glp_calloc",E"glp_calloc(n, size)

   Replacement of standard C 'calloc', but does not initialize the
   memeory. Allocates uninitialized memeory which must freed with
   'glp_free'.

   Returns a pointer to the allocated memory.

"),

(E"glpk.jl",E"glp_free",E"glp_free(ptr)

   Deallocates a memory block previously allocated by 'glp_malloc' or
   'glp_calloc'.

"),

(E"glpk.jl",E"glp_mem_usage",E"glp_mem_usage()

   Reports some information about utilization of the memory by the
   routines 'glp_malloc', 'glp_calloc', and 'glp_free'. In Julia, this
   function has a different API then C. It returns '(count, cpeak,
   total, tpeak)' rather then taking them as pointers in the argument
   list.

"),

(E"glpk.jl",E"glp_mem_limit",E"glp_mem_limit(limit)

   Limits the amount of memory avaliable for dynamic allocation to a
   value in megabyes given by the integer parameter 'limit'.

"),

(E"glpk.jl",E"glp_time",E"glp_time()

   Returns the current universal time (UTC), in milliseconds.

"),

(E"glpk.jl",E"glp_difftime",E"glp_difftime(t1, t0)

   Returns the difference between two time values 't1' and 't0',
   expressed in seconds.

"),

(E"glpk.jl",E"glp_sdf_open_file",E"glp_sdf_open_file(filename)

   Opens a plain data file.

   If successful, returns a GLPData() object, otherwise throws an
   error.

"),

(E"glpk.jl",E"glp_sdf_read_int",E"glp_sdf_read_int(glp_data)

   Reads an integer number from the plain data file specified by the
   'GLPData' parameter 'glp_data', skipping initial whitespace.

"),

(E"glpk.jl",E"glp_sdf_read_num",E"glp_sdf_read_num(glp_data)

   Reads a floating point number from the plain data file specified by
   the 'GLPData' parameter 'glp_data', skipping initial whitespace.

"),

(E"glpk.jl",E"glp_sdf_read_item",E"glp_sdf_read_item(glp_data)

   Reads a data item (a String) from the plain data file specified by
   the 'GLPData' parameter 'glp_data', skipping initial whitespace.

"),

(E"glpk.jl",E"glp_sdf_read_text",E"glp_sdf_read_text(glp_data)

   Reads a line of text from the plain data file specified by the
   'GLPData' parameter 'glp_data', skipping initial and final
   whitespace.

"),

(E"glpk.jl",E"glp_sdf_line",E"glp_sdf_line(glp_data)

   Returns the current line in the GLPData object 'glp_data'

"),

(E"glpk.jl",E"glp_sdf_close_file",E"glp_sdf_close_file(glp_data)

   Closes the file associated to 'glp_data' and frees the resources.

"),

(E"glpk.jl",E"glp_read_cnfsat",E"glp_read_cnfsat(glp_prob, filename)

   Reads the CNF-SAT problem data in DIMACS format from a text file.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_check_cnfsat",E"glp_check_cnfsat(glp_prob)

   Checks if the problem object encodes a CNF-SAT problem instance, in
   which case it returns 0, otherwise returns non-zero.

"),

(E"glpk.jl",E"glp_write_cnfsat",E"glp_write_cnfsat(glp_prob, filename)

   Writes the CNF-SAT problem data in DIMACS format into a text file.

   Returns 0 upon success; throws an error in case of failure.

"),

(E"glpk.jl",E"glp_minisat1",E"glp_minisat1(glp_prob)

   The routine *glp_minisat1* is a driver to MiniSat, a CNF-SAT solver
   developed by Niklas Eén and Niklas Sörensson, Chalmers University
   of Technology, Sweden.

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: 'GLP_EDATA' (problem is not CNF-SAT),
   'GLP_EFAIL' (solver failure).

"),

(E"glpk.jl",E"glp_intfeas1",E"glp_intfeas1(glp_prob, use_bound, obj_bound)

   The routine glp_intfeas1 is a tentative implementation of an
   integer feasibility solver based on a CNF-SAT solver (currently
   MiniSat). 'use_bound' is a flag: if zero, any feasible solution is
   seeked, otherwise seraches for an integer feasible solution.
   'obj_bound' is used only if 'use_bound' is non-zero, and specifies
   an upper/lower bound (for maximization/minimazion respectively) to
   the objective function.

   All variables (columns) must either be binary or fixed. All
   constraint and objective coeffient must be integer.

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: 'GLP_EDATA' (problem data is not valid),
   'GLP_ERANGE' (integer overflow occurred), 'GLP_EFAIL' (solver
   failure).

"),


(E"options.jl",E"options",E"options()

   Use the '@options' macro to set the value of optional parameters
   for a function that has been written to use them (see 'defaults()'
   to learn how to write such functions).  The syntax is:

      opts = @options a=5 b=7

   For a function that uses optional parameters 'a' and 'b', this will
   override the default settings for these parameters. You would
   likely call that function in the following way:

      myfunc(requiredarg1, requiredarg2, ..., opts)

   Most functions written to use optional arguments will probably
   check to make sure that you are not supplying parameters that are
   never used by the function or its sub-functions. Typically,
   supplying unused parameters will result in an error. You can
   control the behavior this way:

      # throw an error if a or b is not used (the default)
      opts = @options CheckError a=5 b=2
      # issue a warning if a or b is not used
      opts = @options CheckWarn a=5 b=2
      # don't check whether a and b are used
      opts = @options CheckNone a=5 b=2

   As an alternative to the macro syntax, you can also say:

      opts = Options(CheckWarn, :a, 5, :b, 2)

   The check flag is optional.

"),

(E"options.jl",E"set_options",E"set_options()

   The '@set_options' macro lets you add new parameters to an existing
   options structure.  For example:

      @set_options opts d=99

   would add 'd' to the set of parameters in 'opts', or re-set its
   value if it was already supplied.

"),

(E"options.jl",E"defaults",E"defaults()

   The '@defaults' macro is for writing functions that take optional
   parameters.  The typical syntax of such functions is:

      function myfunc(requiredarg1, requiredarg2, ..., opts::Options)
          @defaults opts a=11 b=2a+1 c=a*b d=100
          # The function body. Use a, b, c, and d just as you would
          # any other variable. For example,
          k = a + b
          # You can pass opts down to subfunctions, which might supply
          # additional defaults for other variables aa, bb, etc.
          y = subfun(k, opts)
          # Terminate your function with check_used, then return values
          @check_used opts
          return y
      end

   Note the function calls 'check_used()' at the end.

   It is possible to have more than one Options parameter to a
   function, for example:

      function twinopts(x, plotopts::Options, calcopts::Options)
          @defaults plotopts linewidth=1
          @defaults calcopts n_iter=100
          # Do stuff
          @check_used plotopts
          @check_used calcopts
      end

   Within a given scope, you should only have one call to '@defaults'
   per options variable.

"),

(E"options.jl",E"check_used",E"check_used()

   The '@check_used' macro tests whether user-supplied parameters were
   ever accessed by the '@defaults' macro. The test is performed at
   the end of the function body, so that subfunction handling
   parameters not used by the parent function may be 'credited' for
   their usage. Each sub-function should also call '@check_used', for
   example:

      function complexfun(x, opts::Options)
          @defaults opts parent=3 both=7
          println(parent)
          println(both)
          subfun1(x, opts)
          subfun2(x, opts)
          @check_used opts
      end

      function subfun1(x, opts::Options)
          @defaults opts sub1='sub1 default' both=0
          println(sub1)
          println(both)
          @check_used opts
      end

      function subfun2(x, opts::Options)
          @defaults opts sub2='sub2 default' both=22
          println(sub2)
          println(both)
          @check_used opts
      end

"),

(E"options.jl",E"Options",E"type Options(OptionsChecking, param1, val1, param2, val2, ...)

   'Options' is the central type used for handling optional arguments.
   Its fields are briefly described below.

   key2index

      A 'Dict' that looks up an integer index, given the symbol for a
      variable (e.g., 'key2index[:a]' for the variable 'a')

   vals

      'vals[key2index[:a]]' is the value to be assigned to the
      variable 'a'

   used

      A vector of booleans, one per variable, with
      'used[key2index[:a]]' representing the value for variable 'a'.
      These all start as 'false', but access by a '@defaults' command
      sets the corresponding value to 'true'. This marks the variable
      as having been used in the function.

   check_lock

      A vector of booleans, one per variable. This is a 'lock' that
      prevents sub-functions from complaining that they did not access
      variables that were intended for the parent function.
      '@defaults' sets the lock to true for any options variables that
      have already been defined; new variables added through
      '@set_options' will start with their 'check_lock' set to
      'false', to be handled by a subfunction.

"),

(E"profile.jl",E"profile",E"profile()

   Profiling is controlled via the '@profile' macro. Your first step
   is to determine which code you want to profile and encapsulate it
   inside a '@profile begin ... end' block, like this:

      @profile begin
      function f1(x::Int)
        z = 0
        for j = 1:x
          z += j^2
        end
        return z
      end

      function f1(x::Float64)
        return x+2
      end

      function f1{T}(x::T)
        return x+5
      end

      f2(x) = 2*x
      end     # @profile begin

   Now load the file and execute the code you want to profile, e.g.:

      f1(215)
      for i = 1:100
        f1(3.5)
      end
      for i = 1:150
        f1(uint8(7))
      end
      for i = 1:125
        f2(11)
      end

   To view the execution times, type '@profile report'.  Each row of
   the output shows the number of times the line was executed, the
   cumulative time spent on that line, the estimated 'compensated'
   cumulative time (compensating for the overhead of profiling, see
   below), and the line number and filename.

   Here are the various options you have for controlling profiling:

   * '@profile report': display cumulative profiling results

   * '@profile clear': clear all timings accumulated thus far (start
     from zero)

   * '@profile off': turn profiling off (there is no need to remove
     '@profile begin ... end' blocks)

   * '@profile on': turn profiling back on

   Be aware that profiling adds a significant performance overhead.
   You can prevent a subsection of your code from being profiled by
   encapsulating it inside a 'begin ... end' block; in this case, the
   block as a whole is profiled, but the individual lines inside the
   block are not separately timed.

"),

(E"specfun.jl",E"airyai",E"airy(x)
airyai(x)

   Airy function \\operatorname{Ai}(x).

"),

(E"specfun.jl",E"airyaiprime",E"airyprime(x)
airyaiprime(x)

   Airy function derivative \\operatorname{Ai}'(x).

"),

(E"specfun.jl",E"airybi",E"airybi(x)

   Airy function \\operatorname{Bi}(x).

"),

(E"specfun.jl",E"airybiprime",E"airybiprime(x)

   Airy function derivative \\operatorname{Bi}'(x).

"),

(E"specfun.jl",E"besselj0",E"besselj0(x)

   Bessel function of the first kind of order 0, J_0(x).

"),

(E"specfun.jl",E"besselj1",E"besselj1(x)

   Bessel function of the first kind of order 1, J_1(x).

"),

(E"specfun.jl",E"besselj",E"besselj(nu, x)

   Bessel function of the first kind of order 'nu', J_\\nu(x).

"),

(E"specfun.jl",E"bessely0",E"bessely0(x)

   Bessel function of the second kind of order 0, Y_0(x).

"),

(E"specfun.jl",E"bessely1",E"bessely1(x)

   Bessel function of the second kind of order 1, Y_1(x).

"),

(E"specfun.jl",E"bessely",E"bessely(nu, x)

   Bessel function of the second kind of order 'nu', Y_\\nu(x).

"),

(E"specfun.jl",E"hankelh1",E"hankelh1(nu, x)

   Bessel function of the third kind of order 'nu', H^{(1)}_\\nu(x).

"),

(E"specfun.jl",E"hankelh2",E"hankelh2(nu, x)

   Bessel function of the third kind of order 'nu', H^{(2)}_\\nu(x).

"),

(E"specfun.jl",E"besseli",E"besseli(nu, x)

   Modified Bessel function of the first kind of order 'nu',
   I_\\nu(x).

"),

(E"specfun.jl",E"besselk",E"besselk(nu, x)

   Modified Bessel function of the second kind of order 'nu',
   K_\\nu(x).

"),

(E"specfun.jl",E"beta",E"beta(x, y)

   Euler integral of the first kind \\operatorname{B}(x,y) =
   \\Gamma(x)\\Gamma(y)/\\Gamma(x+y).

"),

(E"specfun.jl",E"lbeta",E"lbeta(x, y)

   Natural logarithm of the beta function
   \\log(\\operatorname{B}(x,y)).

"),

(E"specfun.jl",E"eta",E"eta(x)

   Dirichlet eta function \\eta(s) =
   \\sum^\\infty_{n=1}(-)^{n-1}/n^{s}.

"),

(E"specfun.jl",E"zeta",E"zeta(x)

   Riemann zeta function \\zeta(s).

"),

(E"strpack.jl",E"pack",E"pack(io, composite[, strategy])

   Create a packed buffer representation of 'composite' in stream
   'io', using data alignment coded by 'strategy'. This buffer is
   suitable to pass as a 'struct' argument in a 'ccall'.

"),

(E"strpack.jl",E"unpack",E"unpack(io, T[, strategy])

   Extract an instance of the Julia composite type 'T' from the packed
   representation in the stream 'io'. 'io' must be positioned at the
   beginning (using 'seek'). This allows you to read C 'struct'
   outputs from 'ccall'.

"),

(E"textwrap.jl",E"wrap",E"wrap(string[, options])

   Returns a string in which newlines are inserted as appropriate in
   order for each line to fit within a specified width.

   The options are passed via an 'Options' object (see the *options
   page*). The available options, and their default values, are:

   * 'width' (default = '70'): the maximum width of the wrapped text,
     including indentation.

   * 'initial_indent' (default = ''''): indentation of the first line.
     This can be any string (shorter than 'width'), or it can be an
     integer number (lower than 'width').

   * 'subsequent_indent' (default = ''''): indentation of all lines
     except the first. Works the same as 'initial_indent'.

   * 'break_on_hyphens' (default = 'true'): this flag determines
     whether words can be broken on hyphens, e.g. whether 'high-
     precision' can be split into 'high-' and 'precision'.

   * 'break_long_words' (default = 'true'): this flag determines what
     to do when a word is too long to fit in any line. If 'true', the
     word will be broken, otherwise it will go beyond the desired text
     width.

   * 'replace_whitespace' (default = 'true'): if this flag is true,
     all whitespace characters in the original text (including
     newlines) will be replaced by spaces.

   * 'expand_tabs' (default = 'true'): if this flag is true, tabs will
     be expanded in-place into spaces. The expansion happens before
     whitespace replacement.

   * 'fix_sentence_endings' (default = 'false'): if this flag is true,
     the wrapper will try to recognize sentence endings in the middle
     of a paragraph and put two spaces before the next sentence in
     case only one is present.

"),

(E"textwrap.jl",E"println_wrapped",E"print_wrapped(text...[, options])
print_wrapped(io, text...[, options])
println_wrapped(text...[, options])
println_wrapped(io, text...[, options])

   These are just like the standard 'print' and 'println' functions
   (they print multiple arguments and accept an optional 'IO' first
   argument), except that they wrap the result, and accept an optional
   last argument with the options to pass to 'wrap'.

"),

(E"zlib.jl --- Wrapper for zlib compress/uncompress",E"compress_bound",E"compress_bound(input_size)

   Returns the maximum size of the compressed output buffer for a
   given uncompressed input size.

"),

(E"zlib.jl --- Wrapper for zlib compress/uncompress",E"compress",E"compress(source[, level])

   Compresses source using the given compression level, and returns
   the compressed buffer ('Array{Uint8,1}').  'level' is an integer
   between 0 and 9, or one of 'Z_NO_COMPRESSION', 'Z_BEST_SPEED',
   'Z_BEST_COMPRESSION', or 'Z_DEFAULT_COMPRESSION'.  It defaults to
   'Z_DEFAULT_COMPRESSION'.

   If an error occurs, 'compress' throws a ZLibError with more
   information about the error.

"),

(E"zlib.jl --- Wrapper for zlib compress/uncompress",E"compress_to_buffer",E"compress_to_buffer(source, dest, level=Z_DEFAULT_COMPRESSION)

   Compresses the source buffer into the destination buffer, and
   returns the number of bytes written into dest.

   If an error occurs, 'uncompress' throws a ZLibError with more
   information about the error.

"),

(E"zlib.jl --- Wrapper for zlib compress/uncompress",E"uncompress",E"uncompress(source[, uncompressed_size])

   Allocates a buffer of size 'uncompressed_size', uncompresses source
   to this buffer using the given compression level, and returns the
   compressed buffer.  If 'uncompressed_size' is not given, the size
   of the output buffer is estimated as '2*length(source)'.  If the
   uncompressed_size is larger than uncompressed_size, the allocated
   buffer is grown and the uncompression is retried.

   If an error occurs, 'uncompress' throws a ZLibError with more
   information about the error.

"),

(E"zlib.jl --- Wrapper for zlib compress/uncompress",E"uncompress_to_buffer",E"uncompress_to_buffer(source, dest)

   Uncompresses the source buffer into the destination buffer. Returns
   the number of bytes written into dest.  An error is thrown if the
   destination buffer does not have enough space.

   If an error occurs, 'uncompress_to_buffer' throws a ZLibError with
   more information about the error.

"),


}
