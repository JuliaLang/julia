# automatically generated -- do not edit

function _jl_help_db() return [

(E"Getting Around",E"exit",E"exit([code])

   Quit (or control-D at the prompt). The default exit code is zero,
   indicating that the processes completed successfully.

"),

(E"Getting Around",E"whos",E"whos()

   Print information about global user-defined variables.

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

(E"All Objects",E"is",E"is(x, y)

   Determine whether 'x' and 'y' refer to the same object in memory.

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

(E"All Objects",E"uid",E"uid(x)

   Get a unique integer id for 'x'. 'uid(x)==uid(y)' if and only if
   'is(x,y)'.

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

   Create a deep copy of 'x': i.e. 'copy' is called recursively on all
   constituent parts of 'x'. If a user-defined type should be
   recursively copied, a 'copy' method should be defined for it which
   implements deep copying of an instance.

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

   The distance between 1.0 and the next largest representable
   floating-point value of 'type'. The only types that are sensible
   arguments are 'Float32' and 'Float64'. If 'type' is omitted, then
   'eps(Float64)' is returned.

"),

(E"Types",E"eps",E"eps(x)

   The distance between 'x' and the next largest representable
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

(E"General Collections",E"numel",E"numel(collection)

   Return the number of elements in a collection.

"),

(E"General Collections",E"length",E"length(collection)

   For ordered, indexable collections, the maximum index 'i' for which
   'ref(collection, i)' is valid.

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

(E"Set-Like Collections",E"intset",E"intset(i...)

   Construct an 'IntSet' of the given integers.

"),

(E"Set-Like Collections",E"IntSet",E"IntSet(n)

   Construct a set for holding integers up to 'n' (larger integers may
   also be added later).

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

(E"Dequeues",E"append",E"append(collection, items)

   Construct an array composed of the elements of 'items' added to the
   end of a collection. Does not modify collection.

"),

(E"Dequeues",E"append!(collection, items)",E"append!(collection, items)

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

(E"Strings",E"cstring",E"cstring(::Ptr{Uint8})

   Create a string from the address of a C (0-terminated) string.

"),

(E"Strings",E"cstring",E"cstring(s)

   Convert a string to a contiguous byte array representation
   appropriate for passing it to C functions.

"),

(E"Strings",E"ASCIIString",E"ASCIIString(::Array{Uint8, 1})

   Create an ASCII string from a byte array.

"),

(E"Strings",E"UTF8String",E"UTF8String(::Array{Uint8, 1})

   Create a UTF-8 string from a byte array.

"),

(E"Strings",E"strchr",E"strchr(string, char[, i])

   Return the index of 'char' in 'string', giving an error if not
   found. The third argument optionally specifies a starting index.

"),

(E"Strings",E"lpad",E"lpad(string, n, p)

   Make a string at least 'n' characters long by padding on the left
   with copies of 'p'.

"),

(E"Strings",E"rpad",E"rpad(string, n, p)

   Make a string at least 'n' characters long by padding on the right
   with copies of 'p'.

"),

(E"Strings",E"split",E"split(string, char, include_empty)

   Return an array of strings by splitting the given string on
   occurrences of the given character delimiter. The second argument
   may also be a set of character delimiters to use. The third
   argument specifies whether empty fields should be included.

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

(E"I/O",E"with_output_stream",E"with_output_stream(stream, f::Function, args...)

   Call 'f(args...)' with the current output stream set to the given
   object. This is typically used to redirect the output of 'print'
   and 'show'.

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

(E"I/O",E"skip",E"skip(s, offset)

   Seek a stream relative to the current position.

"),

(E"I/O",E"current_output_stream",E"current_output_stream()

   Obtain the current default output stream (used by 'print' and other
   output functions).

"),

(E"I/O",E"set_current_output_stream",E"set_current_output_stream(s)

   Set the current output stream.

"),

(E"Text I/O",E"show",E"show(x)

   Write an informative text representation of a value to the current
   output stream.

"),

(E"Text I/O",E"print",E"print(x)

   Write (to the current output stream) a canonical (un-decorated)
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

(E"Mathematical Functions",E"ceil",E"ceil(x) -> Float

   Returns the nearest integer not less than 'x'.

"),

(E"Mathematical Functions",E"floor",E"floor(x) -> Float

   Returns the nearest integer not greater than 'x'.

"),

(E"Mathematical Functions",E"trunc",E"trunc(x) -> Float

   Returns the nearest integer not greater in magnitude than 'x'.

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

   Returns the size of the stride along dimension k

"),

(E"Arrays",E"strides",E"strides(A)

   Returns a tuple of the linear index distances between adjacent
   elements in each dimension

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

(E"Arrays",E"fill!(A, x)",E"fill!(A, x)

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

(E"Arrays",E"empty",E"empty(A)

   Construct an empty 1-d array similar to the given array

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

(E"Arrays",E"rowvec",E"rowvec(A, i)

   Return the ith row of matrix A as a vector.

"),

(E"Arrays",E"colvec",E"colvec(A, i)

   Return the ith column of matrix A as a vector.

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

(E"Linear Algebra",E"chol",E"chol(A)

   Compute Cholesky factorization

"),

(E"Linear Algebra",E"lu",E"lu(A) -> L, U, p

   Compute LU factorization

"),

(E"Linear Algebra",E"qr",E"qr(A) -> Q, R, p

   Compute QR factorization

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

(E"Combinatorics",E"sort!(v)",E"sort!(v)

   In-place sort

"),

(E"Combinatorics",E"sortr",E"sortr(v)

   Sort a vector in descending order

"),

(E"Combinatorics",E"sortr!(v)",E"sortr!(v)

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

(E"Combinatorics",E"nthperm!(v, k)",E"nthperm!(v, k)

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

(E"Combinatorics",E"shuffle!(v)",E"shuffle!(v)

   In-place version of 'shuffle'

"),

(E"Combinatorics",E"reverse",E"reverse(v)

   Reverse vector 'v'

"),

(E"Combinatorics",E"reverse!(v)",E"reverse!(v)

   Reverse vector 'v' in-place

"),

(E"Combinatorics",E"select",E"select(v, k)

   Find the element in position 'k' in the sorted vector 'v' without
   sorting

"),

(E"Combinatorics",E"select!(v, k)",E"select!(v, k)

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
   fft of vectors along dimension 'dim'

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

   Construct a distrbuted array of zeros. Trailing arguments are the
   same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"dones",E"dones([type], dims, ...)

   Construct a distrbuted array of ones. Trailing arguments are the
   same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"dfill",E"dfill(x, dims, ...)

   Construct a distrbuted array filled with value 'x'. Trailing
   arguments are the same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"drand",E"drand(dims, ...)

   Construct a distrbuted uniform random array. Trailing arguments are
   the same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"drandn",E"drandn(dims, ...)

   Construct a distrbuted normal random array. Trailing arguments are
   the same as those accepted by 'darray'.

"),

(E"Distributed Arrays",E"dcell",E"dcell(dims, ...)

   Construct a distrbuted cell array. Trailing arguments are the same
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

(E"System",E"getcwd",E"getcwd()

   Get the current working directory.

"),

(E"System",E"setcwd",E"setcwd('dir')

   Set the current working directory. Returns the new current
   directory.

"),

(E"System",E"getpid",E"getpid()

   Get julia's process ID.

"),

(E"System",E"time",E"time()

   Get the time in seconds since the epoch, with fairly high
   resolution.

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

] end
