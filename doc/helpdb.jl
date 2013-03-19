# automatically generated -- do not edit

{

("Getting Around","Base","exit","exit([code])

   Quit (or control-D at the prompt). The default exit code is zero,
   indicating that the processes completed successfully.

"),

("Getting Around","Base","whos","whos([Module,] [pattern::Regex])

   Print information about global variables in a module, optionally
   restricted to those matching \"pattern\".

"),

("Getting Around","Base","edit","edit(file::String[, line])

   Edit a file optionally providing a line number to edit at. Returns
   to the julia prompt when you quit the editor. If the file name ends
   in \".jl\" it is reloaded when the editor closes the file.

"),

("Getting Around","Base","edit","edit(function[, types])

   Edit the definition of a function, optionally specifying a tuple of
   types to indicate which method to edit. When the editor exits, the
   source file containing the definition is reloaded.

"),

("Getting Around","Base","require","require(file::String...)

   Load source files once, in the context of the \"Main\" module, on
   every active node, searching the system-wide \"LOAD_PATH\" for
   files. \"require\" is considered a top-level operation, so it sets
   the current \"include\" path but does not use it to search for
   files (see help for \"include\"). This function is typically used
   to load library code, and is implicitly called by \"using\" to load
   packages.

"),

("Getting Around","Base","reload","reload(file::String)

   Like \"require\", except forces loading of files regardless of
   whether they have been loaded before. Typically used when
   interactively developing libraries.

"),

("Getting Around","Base","include","include(path::String)

   Evaluate the contents of a source file in the current context.
   During including, a task-local include path is set to the directory
   containing the file. Nested calls to \"include\" will search
   relative to that path. All paths refer to files on node 1 when
   running in parallel, and files will be fetched from node 1. This
   function is typically used to load source interactively, or to
   combine files in packages that are broken into multiple source
   files.

"),

("Getting Around","Base","include_string","include_string(code::String)

   Like \"include\", except reads code from the given string rather
   than from a file. Since there is no file path involved, no path
   processing or fetching from node 1 is done.

"),

("Getting Around","Base","evalfile","evalfile(path::String)

   Evaluate all expressions in the given file, and return the value of
   the last one. No other processing (path searching, fetching from
   node 1, etc.) is performed.

"),

("Getting Around","Base","help","help(name)

   Get help for a function. \"name\" can be an object or a string.

"),

("Getting Around","Base","apropos","apropos(string)

   Search documentation for functions related to \"string\".

"),

("Getting Around","Base","which","which(f, args...)

   Show which method of \"f\" will be called for the given arguments.

"),

("Getting Around","Base","methods","methods(f)

   Show all methods of \"f\" with their argument types.

"),

("Getting Around","Base","methodswith","methodswith(t)

   Show all methods with an argument of type \"typ\".

"),

("All Objects","Base","is","is(x, y)

   Determine whether \"x\" and \"y\" are identical, in the sense that
   no program could distinguish them.

"),

("All Objects","Base","isa","isa(x, type)

   Determine whether \"x\" is of the given type.

"),

("All Objects","Base","isequal","isequal(x, y)

   True if and only if \"x\" and \"y\" have the same contents. Loosely
   speaking, this means \"x\" and \"y\" would look the same when
   printed.

"),

("All Objects","Base","isless","isless(x, y)

   Test whether \"x\" is less than \"y\". Provides a total order
   consistent with \"isequal\". Values that are normally unordered,
   such as \"NaN\", are ordered in an arbitrary but consistent
   fashion. This is the default comparison used by \"sort\". Non-
   numeric types that can be ordered should implement this function.

"),

("All Objects","Base","typeof","typeof(x)

   Get the concrete type of \"x\".

"),

("All Objects","Base","tuple","tuple(xs...)

   Construct a tuple of the given objects.

"),

("All Objects","Base","ntuple","ntuple(n, f::Function)

   Create a tuple of length \"n\", computing each element as \"f(i)\",
   where \"i\" is the index of the element.

"),

("All Objects","Base","object_id","object_id(x)

   Get a unique integer id for \"x\". \"object_id(x)==object_id(y)\"
   if and only if \"is(x,y)\".

"),

("All Objects","Base","hash","hash(x)

   Compute an integer hash code such that \"isequal(x,y)\" implies
   \"hash(x)==hash(y)\".

"),

("All Objects","Base","finalizer","finalizer(x, function)

   Register a function \"f(x)\" to be called when there are no
   program-accessible references to \"x\". The behavior of this
   function is unpredictable if \"x\" is of a bits type.

"),

("All Objects","Base","copy","copy(x)

   Create a shallow copy of \"x\": the outer structure is copied, but
   not all internal values. For example, copying an array produces a
   new array with identically-same elements as the original.

"),

("All Objects","Base","deepcopy","deepcopy(x)

   Create a deep copy of \"x\": everything is copied recursively,
   resulting in a fully independent object. For example, deep-copying
   an array produces a new array whose elements are deep-copies of the
   original elements.

   As a special case, functions can only be actually deep-copied if
   they are anonymous, otherwise they are just copied. The difference
   is only relevant in the case of closures, i.e. functions which may
   contain hidden internal references.

   While it isn't normally necessary, user-defined types can override
   the default \"deepcopy\" behavior by defining a specialized version
   of the function \"deepcopy_internal(x::T, dict::ObjectIdDict)\"
   (which shouldn't otherwise be used), where \"T\" is the type to be
   specialized for, and \"dict\" keeps track of objects copied so far
   within the recursion. Within the definition, \"deepcopy_internal\"
   should be used in place of \"deepcopy\", and the \"dict\" variable
   should be updated as appropriate before returning.

"),

("All Objects","Base","convert","convert(type, x)

   Try to convert \"x\" to the given type.

"),

("All Objects","Base","promote","promote(xs...)

   Convert all arguments to their common promotion type (if any), and
   return them all (as a tuple).

"),

("Types","Base","subtype","subtype(type1, type2)

   True if and only if all values of \"type1\" are also of \"type2\".
   Can also be written using the \"<:\" infix operator as \"type1 <:
   type2\".

"),

("Types","Base","typemin","typemin(type)

   The lowest value representable by the given (real) numeric type.

"),

("Types","Base","typemax","typemax(type)

   The highest value representable by the given (real) numeric type.

"),

("Types","Base","realmin","realmin(type)

   The smallest in absolute value non-denormal value representable by
   the given floating-point type

"),

("Types","Base","realmax","realmax(type)

   The highest finite value representable by the given floating-point
   type

"),

("Types","Base","maxintfloat","maxintfloat(type)

   The largest integer losslessly representable by the given floating-
   point type

"),

("Types","Base","sizeof","sizeof(type)

   Size, in bytes, of the canonical binary representation of the given
   type, if any.

"),

("Types","Base","eps","eps([type])

   The distance between 1.0 and the next larger representable
   floating-point value of \"type\". The only types that are sensible
   arguments are \"Float32\" and \"Float64\". If \"type\" is omitted,
   then \"eps(Float64)\" is returned.

"),

("Types","Base","eps","eps(x)

   The distance between \"x\" and the next larger representable
   floating-point value of the same type as \"x\".

"),

("Types","Base","promote_type","promote_type(type1, type2)

   Determine a type big enough to hold values of each argument type
   without loss, whenever possible. In some cases, where no type
   exists which to which both types can be promoted losslessly, some
   loss is tolerated; for example, \"promote_type(Int64,Float64)\"
   returns \"Float64\" even though strictly, not all \"Int64\" values
   can be represented exactly as \"Float64\" values.

"),

("Generic Functions","Base","method_exists","method_exists(f, tuple) -> Bool

   Determine whether the given generic function has a method matching
   the given tuple of argument types.

   **Example**: \"method_exists(length, (Array,)) = true\"

"),

("Generic Functions","Base","applicable","applicable(f, args...)

   Determine whether the given generic function has a method
   applicable to the given arguments.

"),

("Generic Functions","Base","invoke","invoke(f, (types...), args...)

   Invoke a method for the given generic function matching the
   specified types (as a tuple), on the specified arguments. The
   arguments must be compatible with the specified types. This allows
   invoking a method other than the most specific matching method,
   which is useful when the behavior of a more general definition is
   explicitly needed (often as part of the implementation of a more
   specific method of the same function).

"),

("Generic Functions","Base","|","|(x, f)

   Applies a function to the preceding argument which allows for easy
   function chaining.

   **Example**: \"[1:5] | x->x.^2 | sum | inv\"

"),

("Iteration","Base","start","start(iter) -> state

   Get initial iteration state for an iterable object

"),

("Iteration","Base","done","done(iter, state) -> Bool

   Test whether we are done iterating

"),

("Iteration","Base","next","next(iter, state) -> item, state

   For a given iterable object and iteration state, return the current
   item and the next iteration state

"),

("Iteration","Base","zip","zip(iters...)

   For a set of iterable objects, returns an iterable of tuples, where
   the \"i\"th tuple contains the \"i\"th component of each input
   iterable.

   Note that \"zip\" is it's own inverse: \"[zip(zip(a...)...)...] ==
   [a...]\".

"),

("Iteration","Base","enumerate","enumerate(iter)

   Return an iterator that yields \"(i, x)\" where \"i\" is an index
   starting at 1, and \"x\" is the \"ith\" value from the given
   iterator.

"),

("General Collections","Base","isempty","isempty(collection) -> Bool

   Determine whether a collection is empty (has no elements).

"),

("General Collections","Base","empty!","empty!(collection) -> collection

   Remove all elements from a collection.

"),

("General Collections","Base","length","length(collection) -> Integer

   For ordered, indexable collections, the maximum index \"i\" for
   which \"getindex(collection, i)\" is valid. For unordered
   collections, the number of elements.

"),

("General Collections","Base","endof","endof(collection) -> Integer

   Returns the last index of the collection.

   **Example**: \"endof([1,2,4]) = 3\"

"),

("Iterable Collections","Base","contains","contains(itr, x) -> Bool

   Determine whether a collection contains the given value, \"x\".

"),

("Iterable Collections","Base","findin","findin(a, b)

   Returns the indices of elements in collection \"a\" that appear in
   collection \"b\"

"),

("Iterable Collections","Base","unique","unique(itr)

   Returns an array containing only the unique elements of the
   iterable \"itr\".

"),

("Iterable Collections","Base","reduce","reduce(op, v0, itr)

   Reduce the given collection with the given operator, i.e.
   accumulate \"v = op(v,elt)\" for each element, where \"v\" starts
   as \"v0\". Reductions for certain commonly-used operators are
   available in a more convenient 1-argument form: \"max(itr)\",
   \"min(itr)\", \"sum(itr)\", \"prod(itr)\", \"any(itr)\",
   \"all(itr)\".

"),

("Iterable Collections","Base","max","max(itr)

   Returns the largest element in a collection

"),

("Iterable Collections","Base","min","min(itr)

   Returns the smallest element in a collection

"),

("Iterable Collections","Base","indmax","indmax(itr) -> Integer

   Returns the index of the maximum element in a collection

"),

("Iterable Collections","Base","indmin","indmin(itr) -> Integer

   Returns the index of the minimum element in a collection

"),

("Iterable Collections","Base","findmax","findmax(itr) -> (x, index)

   Returns the maximum element and its index

"),

("Iterable Collections","Base","findmin","findmin(itr) -> (x, index)

   Returns the minimum element and its index

"),

("Iterable Collections","Base","sum","sum(itr)

   Returns the sum of all elements in a collection

"),

("Iterable Collections","Base","prod","prod(itr)

   Returns the product of all elements of a collection

"),

("Iterable Collections","Base","any","any(itr) -> Bool

   Test whether any elements of a boolean collection are true

"),

("Iterable Collections","Base","all","all(itr) -> Bool

   Test whether all elements of a boolean collection are true

"),

("Iterable Collections","Base","count","count(itr) -> Integer

   Count the number of boolean elements in \"itr\" which are true.

"),

("Iterable Collections","Base","countp","countp(p, itr) -> Integer

   Count the number of elements in \"itr\" for which predicate \"p\"
   is true.

"),

("Iterable Collections","Base","any","any(p, itr) -> Bool

   Determine whether any element of \"itr\" satisfies the given
   predicate.

"),

("Iterable Collections","Base","all","all(p, itr) -> Bool

   Determine whether all elements of \"itr\" satisfy the given
   predicate.

"),

("Iterable Collections","Base","map","map(f, c) -> collection

   Transform collection \"c\" by applying \"f\" to each element.

   **Example**: \"map((x) -> x * 2, [1, 2, 3]) = [2, 4, 6]\"

"),

("Iterable Collections","Base","map!","map!(function, collection)

   In-place version of \"map()\".

"),

("Iterable Collections","Base","mapreduce","mapreduce(f, op, itr)

   Applies function \"f\" to each element in \"itr\" and then reduces
   the result using the binary function \"op\".

   **Example**: \"mapreduce(x->x^2, +, [1:3]) == 1 + 4 + 9 == 14\"

"),

("Iterable Collections","Base","first","first(coll)

   Get the first element of an ordered collection.

"),

("Iterable Collections","Base","last","last(coll)

   Get the last element of an ordered collection.

"),

("Indexable Collections","Base","getindex","getindex(collection, key...)

   Retrieve the value(s) stored at the given key or index within a
   collection. The syntax \"a[i,j,...]\" is converted by the compiler
   to \"getindex(a, i, j, ...)\".

"),

("Indexable Collections","Base","setindex!","setindex!(collection, value, key...)

   Store the given value at the given key or index within a
   collection. The syntax \"a[i,j,...] = x\" is converted by the
   compiler to \"setindex!(a, x, i, j, ...)\".

"),

("Associative Collections","Base","Dict{K,V}","Dict{K,V}()

   Construct a hashtable with keys of type K and values of type V

"),

("Associative Collections","Base","has","has(collection, key)

   Determine whether a collection has a mapping for a given key.

"),

("Associative Collections","Base","get","get(collection, key, default)

   Return the value stored for the given key, or the given default
   value if no mapping for the key is present.

"),

("Associative Collections","Base","getkey","getkey(collection, key, default)

   Return the key matching argument \"key\" if one exists in
   \"collection\", otherwise return \"default\".

"),

("Associative Collections","Base","delete!","delete!(collection, key)

   Delete the mapping for the given key in a collection.

"),

("Associative Collections","Base","keys","keys(collection)

   Return an array of all keys in a collection.

"),

("Associative Collections","Base","values","values(collection)

   Return an array of all values in a collection.

"),

("Associative Collections","Base","collect","collect(collection)

   Return an array of all items in a collection. For associative
   collections, returns (key, value) tuples.

"),

("Associative Collections","Base","merge","merge(collection, others...)

   Construct a merged collection from the given collections.

"),

("Associative Collections","Base","merge!","merge!(collection, others...)

   Update collection with pairs from the other collections

"),

("Associative Collections","Base","filter","filter(function, collection)

   Return a copy of collection, removing (key, value) pairs for which
   function is false.

"),

("Associative Collections","Base","filter!","filter!(function, collection)

   Update collection, removing (key, value) pairs for which function
   is false.

"),

("Associative Collections","Base","eltype","eltype(collection)

   Returns the type tuple of the (key,value) pairs contained in
   collection.

"),

("Associative Collections","Base","sizehint","sizehint(s, n)

   Suggest that collection \"s\" reserve capacity for at least \"n\"
   elements. This can improve performance.

"),

("Set-Like Collections","Base","add!","add!(collection, key)

   Add an element to a set-like collection.

"),

("Set-Like Collections","Base","add_each!","add_each!(collection, iterable)

   Adds each element in iterable to the collection.

"),

("Set-Like Collections","Base","Set","Set(x...)

   Construct a \"Set\" with the given elements. Should be used instead
   of \"IntSet\" for sparse integer sets.

"),

("Set-Like Collections","Base","IntSet","IntSet(i...)

   Construct an \"IntSet\" of the given integers. Implemented as a bit
   string, and therefore good for dense integer sets.

"),

("Set-Like Collections","Base","union","union(s1, s2...)

   Construct the union of two or more sets. Maintains order with
   arrays.

"),

("Set-Like Collections","Base","union!","union!(s1, s2)

   Constructs the union of IntSets s1 and s2, stores the result in
   \"s1\".

"),

("Set-Like Collections","Base","intersect","intersect(s1, s2...)

   Construct the intersection of two or more sets. Maintains order
   with arrays.

"),

("Set-Like Collections","Base","setdiff","setdiff(s1, s2)

   Construct the set of elements in \"s1\" but not \"s2\". Maintains
   order with arrays.

"),

("Set-Like Collections","Base","symdiff","symdiff(s1, s2...)

   Construct the symmetric difference of elements in the passed in
   sets or arrays. Maintains order with arrays.

"),

("Set-Like Collections","Base","symdiff!","symdiff!(s, n)

   IntSet s is destructively modified to toggle the inclusion of
   integer \"n\".

"),

("Set-Like Collections","Base","symdiff!","symdiff!(s, itr)

   For each element in \"itr\", destructively toggle its inclusion in
   set \"s\".

"),

("Set-Like Collections","Base","symdiff!","symdiff!(s1, s2)

   Construct the symmetric difference of IntSets \"s1\" and \"s2\",
   storing the result in \"s1\".

"),

("Set-Like Collections","Base","complement","complement(s)

   Returns the set-complement of IntSet s.

"),

("Set-Like Collections","Base","complement!","complement!(s)

   Mutates IntSet s into its set-complement.

"),

("Set-Like Collections","Base","del_each!","del_each!(s, itr)

   Deletes each element of itr in set s in-place.

"),

("Set-Like Collections","Base","intersect!","intersect!(s1, s2)

   Intersects IntSets s1 and s2 and overwrites the set s1 with the
   result. If needed, s1 will be expanded to the size of s2.

"),

("Dequeues","Base","push!","push!(collection, item) -> collection

   Insert an item at the end of a collection.

"),

("Dequeues","Base","pop!","pop!(collection) -> item

   Remove the last item in a collection and return it.

"),

("Dequeues","Base","unshift!","unshift!(collection, item) -> collection

   Insert an item at the beginning of a collection.

"),

("Dequeues","Base","shift!","shift!(collection) -> item

   Remove the first item in a collection.

"),

("Dequeues","Base","insert!","insert!(collection, index, item)

   Insert an item at the given index.

"),

("Dequeues","Base","delete!","delete!(collection, index) -> item

   Remove the item at the given index, and return the deleted item.

"),

("Dequeues","Base","delete!","delete!(collection, range) -> items

   Remove items at specified range, and return a collection containing
   the deleted items.

"),

("Dequeues","Base","resize!","resize!(collection, n) -> collection

   Resize collection to contain \"n\" elements.

"),

("Dequeues","Base","append!","append!(collection, items) -> collection

   Add the elements of \"items\" to the end of a collection.

"),

("Strings","Base","length","length(s)

   The number of characters in string \"s\".

"),

("Strings","Base","*","*(s, t)

   Concatenate strings.

   **Example**: \"\"Hello \" * \"world\" == \"Hello world\"\"

"),

("Strings","Base","^","^(s, n)

   Repeat string \"s\" \"n\" times.

   **Example**: \"\"Julia \"^3 == \"Julia Julia Julia \"\"

"),

("Strings","Base","string","string(xs...)

   Create a string from any values using the \"print\" function.

"),

("Strings","Base","repr","repr(x)

   Create a string from any value using the \"show\" function.

"),

("Strings","Base","bytestring","bytestring(::Ptr{Uint8})

   Create a string from the address of a C (0-terminated) string. A
   copy is made; the ptr can be safely freed.

"),

("Strings","Base","bytestring","bytestring(s)

   Convert a string to a contiguous byte array representation
   appropriate for passing it to C functions.

"),

("Strings","Base","ascii","ascii(::Array{Uint8, 1})

   Create an ASCII string from a byte array.

"),

("Strings","Base","ascii","ascii(s)

   Convert a string to a contiguous ASCII string (all characters must
   be valid ASCII characters).

"),

("Strings","Base","utf8","utf8(::Array{Uint8, 1})

   Create a UTF-8 string from a byte array.

"),

("Strings","Base","utf8","utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must
   be valid UTF-8 characters).

"),

("Strings","Base","is_valid_ascii","is_valid_ascii(s) -> Bool

   Returns true if the string or byte vector is valid ASCII, false
   otherwise.

"),

("Strings","Base","is_valid_utf8","is_valid_utf8(s) -> Bool

   Returns true if the string or byte vector is valid UTF-8, false
   otherwise.

"),

("Strings","Base","is_valid_char","is_valid_char(c) -> Bool

   Returns true if the given char or integer is a valid Unicode code
   point.

"),

("Strings","Base","ismatch","ismatch(r::Regex, s::String)

   Test whether a string contains a match of the given regular
   expression.

"),

("Strings","Base","lpad","lpad(string, n, p)

   Make a string at least \"n\" characters long by padding on the left
   with copies of \"p\".

"),

("Strings","Base","rpad","rpad(string, n, p)

   Make a string at least \"n\" characters long by padding on the
   right with copies of \"p\".

"),

("Strings","Base","search","search(string, chars[, start])

   Search for the given characters within the given string. The second
   argument may be a single character, a vector or a set of
   characters, a string, or a regular expression (though regular
   expressions are only allowed on contiguous strings, such as ASCII
   or UTF-8 strings). The third argument optionally specifies a
   starting index. The return value is a range of indexes where the
   matching sequence is found, such that \"s[search(s,x)] == x\". The
   return value is \"0:-1\" if there is no match.

"),

("Strings","Base","replace","replace(string, pat, r[, n])

   Search for the given pattern \"pat\", and replace each occurance
   with \"r\". If \"n\" is provided, replace at most \"n\" occurances.
   As with search, the second argument may be a single character, a
   vector or a set of characters, a string, or a regular expression.
   If \"r\" is a function, each occurrence is replaced with \"r(s)\"
   where \"s\" is the matched substring.

"),

("Strings","Base","split","split(string, [chars, [limit,] [include_empty]])

   Return an array of strings by splitting the given string on
   occurrences of the given character delimiters, which may be
   specified in any of the formats allowed by \"search\"'s second
   argument (i.e. a single character, collection of characters,
   string, or regular expression). If \"chars\" is omitted, it
   defaults to the set of all space characters, and \"include_empty\"
   is taken to be false. The last two arguments are also optional:
   they are are a maximum size for the result and a flag determining
   whether empty fields should be included in the result.

"),

("Strings","Base","strip","strip(string[, chars])

   Return \"string\" with any leading and trailing whitespace removed.
   If a string \"chars\" is provided, instead remove characters
   contained in that string.

"),

("Strings","Base","lstrip","lstrip(string[, chars])

   Return \"string\" with any leading whitespace removed. If a string
   \"chars\" is provided, instead remove characters contained in that
   string.

"),

("Strings","Base","rstrip","rstrip(string[, chars])

   Return \"string\" with any trailing whitespace removed. If a string
   \"chars\" is provided, instead remove characters contained in that
   string.

"),

("Strings","Base","begins_with","begins_with(string, prefix)

   Returns \"true\" if \"string\" starts with \"prefix\".

"),

("Strings","Base","ends_with","ends_with(string, suffix)

   Returns \"true\" if \"string\" ends with \"suffix\".

"),

("Strings","Base","uppercase","uppercase(string)

   Returns \"string\" with all characters converted to uppercase.

"),

("Strings","Base","lowercase","lowercase(string)

   Returns \"string\" with all characters converted to lowercase.

"),

("Strings","Base","join","join(strings, delim)

   Join an array of strings into a single string, inserting the given
   delimiter between adjacent strings.

"),

("Strings","Base","chop","chop(string)

   Remove the last character from a string

"),

("Strings","Base","chomp","chomp(string)

   Remove a trailing newline from a string

"),

("Strings","Base","ind2chr","ind2chr(string, i)

   Convert a byte index to a character index

"),

("Strings","Base","chr2ind","chr2ind(string, i)

   Convert a character index to a byte index

"),

("Strings","Base","isvalid","isvalid(str, i)

   Tells whether index \"i\" is valid for the given string

"),

("Strings","Base","nextind","nextind(str, i)

   Get the next valid string index after \"i\". Returns
   \"endof(str)+1\" at the end of the string.

"),

("Strings","Base","prevind","prevind(str, i)

   Get the previous valid string index before \"i\". Returns \"0\" at
   the beginning of the string.

"),

("Strings","Base","thisind","thisind(str, i)

   Adjust \"i\" downwards until it reaches a valid index for the given
   string.

"),

("Strings","Base","randstring","randstring(len)

   Create a random ASCII string of length \"len\", consisting of
   upper- and lower-case letters and the digits 0-9

"),

("Strings","Base","charwidth","charwidth(c)

   Gives the number of columns needed to print a character.

"),

("Strings","Base","strwidth","strwidth(s)

   Gives the number of columns needed to print a string.

"),

("Strings","Base","isalnum","isalnum(c::Char)

   Tests whether a character is alphanumeric.

"),

("Strings","Base","isalpha","isalpha(c::Char)

   Tests whether a character is alphabetic.

"),

("Strings","Base","isascii","isascii(c::Char)

   Tests whether a character belongs to the ASCII character set.

"),

("Strings","Base","isblank","isblank(c::Char)

   Tests whether a character is a tab or space.

"),

("Strings","Base","iscntrl","iscntrl(c::Char)

   Tests whether a character is a control character.

"),

("Strings","Base","isdigit","isdigit(c::Char)

   Tests whether a character is a numeric digit (0-9).

"),

("Strings","Base","isgraph","isgraph(c::Char)

   Tests whether a character is printable, and not a space.

"),

("Strings","Base","islower","islower(c::Char)

   Tests whether a character is a lowercase letter.

"),

("Strings","Base","isprint","isprint(c::Char)

   Tests whether a character is printable, including space.

"),

("Strings","Base","ispunct","ispunct(c::Char)

   Tests whether a character is printable, and not a space or
   alphanumeric.

"),

("Strings","Base","isspace","isspace(c::Char)

   Tests whether a character is any whitespace character.

"),

("Strings","Base","isupper","isupper(c::Char)

   Tests whether a character is an uppercase letter.

"),

("Strings","Base","isxdigit","isxdigit(c::Char)

   Tests whether a character is a valid hexadecimal digit.

"),

("I/O","Base","STDOUT","STDOUT

   Global variable referring to the standard out stream.

"),

("I/O","Base","STDERR","STDERR

   Global variable referring to the standard error stream.

"),

("I/O","Base","STDIN","STDIN

   Global variable referring to the standard input stream.

"),

("I/O","Base","OUTPUT_STREAM","OUTPUT_STREAM

   The default stream used for text output, e.g. in the \"print\" and
   \"show\" functions.

"),

("I/O","Base","open","open(file_name[, read, write, create, truncate, append]) -> IOStream

   Open a file in a mode specified by five boolean arguments. The
   default is to open files for reading only. Returns a stream for
   accessing the file.

"),

("I/O","Base","open","open(file_name[, mode]) -> IOStream

   Alternate syntax for open, where a string-based mode specifier is
   used instead of the five booleans. The values of \"mode\"
   correspond to those from \"fopen(3)\" or Perl \"open\", and are
   equivalent to setting the following boolean groups:

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

("I/O","Base","open","open(f::function, args...)

   Apply the function \"f\" to the result of \"open(args...)\" and
   close the resulting file descriptor upon completion.

   **Example**: \"open(readall, \"file.txt\")\"

"),

("I/O","Base","memio","memio([size[, finalize::Bool]]) -> IOStream

   Create an in-memory I/O stream, optionally specifying how much
   initial space is needed.

"),

("I/O","Base","fdio","fdio([name::String], fd::Integer[, own::Bool]) -> IOStream

   Create an \"IOStream\" object from an integer file descriptor. If
   \"own\" is true, closing this object will close the underlying
   descriptor. By default, an \"IOStream\" is closed when it is
   garbage collected. \"name\" allows you to associate the descriptor
   with a named file.

"),

("I/O","Base","flush","flush(stream)

   Commit all currently buffered writes to the given stream.

"),

("I/O","Base","close","close(stream)

   Close an I/O stream. Performs a \"flush\" first.

"),

("I/O","Base","write","write(stream, x)

   Write the canonical binary representation of a value to the given
   stream.

"),

("I/O","Base","read","read(stream, type)

   Read a value of the given type from a stream, in canonical binary
   representation.

"),

("I/O","Base","read","read(stream, type, dims)

   Read a series of values of the given type from a stream, in
   canonical binary representation. \"dims\" is either a tuple or a
   series of integer arguments specifying the size of \"Array\" to
   return.

"),

("I/O","Base","position","position(s)

   Get the current position of a stream.

"),

("I/O","Base","seek","seek(s, pos)

   Seek a stream to the given position.

"),

("I/O","Base","seek_end","seek_end(s)

   Seek a stream to the end.

"),

("I/O","Base","skip","skip(s, offset)

   Seek a stream relative to the current position.

"),

("I/O","Base","eof","eof(stream)

   Tests whether an I/O stream is at end-of-file. If the stream is not
   yet exhausted, this function will block to wait for more data if
   necessary, and then return \"false\". Therefore it is always safe
   to read one byte after seeing \"eof\" return \"false\".

"),

("Text I/O","Base","show","show(x)

   Write an informative text representation of a value to the current
   output stream. New types should overload \"show(io, x)\" where the
   first argument is a stream.

"),

("Text I/O","Base","print","print(x)

   Write (to the default output stream) a canonical (un-decorated)
   text representation of a value if there is one, otherwise call
   \"show\".

"),

("Text I/O","Base","println","println(x)

   Print (using \"print()\") \"x\" followed by a newline

"),

("Text I/O","Base","@printf","@printf([io::IOStream], \"%Fmt\", args...)

   Print arg(s) using C \"printf()\" style format specification
   string. Optionally, an IOStream may be passed as the first argument
   to redirect output.

"),

("Text I/O","Base","@sprintf","@sprintf(\"%Fmt\", args...)

   Return \"@printf\" formatted output as string.

"),

("Text I/O","Base","showall","showall(x)

   Show x, printing all elements of arrays

"),

("Text I/O","Base","dump","dump(x)

   Write a thorough text representation of a value to the current
   output stream.

"),

("Text I/O","Base","readall","readall(stream)

   Read the entire contents of an I/O stream as a string.

"),

("Text I/O","Base","readline","readline(stream)

   Read a single line of text, including a trailing newline character
   (if one is reached before the end of the input).

"),

("Text I/O","Base","readuntil","readuntil(stream, delim)

   Read a string, up to and including the given delimiter byte.

"),

("Text I/O","Base","readlines","readlines(stream)

   Read all lines as an array.

"),

("Text I/O","Base","eachline","eachline(stream)

   Create an iterable object that will yield each line from a stream.

"),

("Text I/O","Base","readdlm","readdlm(filename, delim::Char)

   Read a matrix from a text file where each line gives one row, with
   elements separated by the given delimeter. If all data is numeric,
   the result will be a numeric array. If some elements cannot be
   parsed as numbers, a cell array of numbers and strings is returned.

"),

("Text I/O","Base","readdlm","readdlm(filename, delim::Char, T::Type)

   Read a matrix from a text file with a given element type. If \"T\"
   is a numeric type, the result is an array of that type, with any
   non-numeric elements as \"NaN\" for floating-point types, or zero.
   Other useful values of \"T\" include \"ASCIIString\", \"String\",
   and \"Any\".

"),

("Text I/O","Base","writedlm","writedlm(filename, array, delim::Char)

   Write an array to a text file using the given delimeter (defaults
   to comma).

"),

("Text I/O","Base","readcsv","readcsv(filename[, T::Type])

   Equivalent to \"readdlm\" with \"delim\" set to comma.

"),

("Text I/O","Base","writecsv","writecsv(filename, array)

   Equivalent to \"writedlm\" with \"delim\" set to comma.

"),

("Memory-mapped I/O","Base","mmap_array","mmap_array(type, dims, stream[, offset])

   Create an array whose values are linked to a file, using memory-
   mapping. This provides a convenient way of working with data too
   large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted (no
   format conversions are possible), and dims is a tuple containing
   the size of the array.

   The file is specified via the stream.  When you initialize the
   stream, use \"r\" for a \"read-only\" array, and \"w+\" to create a
   new array used to write values to disk. Optionally, you can specify
   an offset (in bytes) if, for example, you want to skip over a
   header in the file.

   **Example**:  A = mmap_array(Int64, (25,30000), s)

   This would create a 25-by-30000 array of Int64s, linked to the file
   associated with stream s.

"),

("Memory-mapped I/O","Base","msync","msync(array)

   Forces synchronization between the in-memory version of a memory-
   mapped array and the on-disk version. You may not need to call this
   function, because synchronization is performed at intervals
   automatically by the operating system. Hower, you can call this
   directly if, for example, you are concerned about losing the result
   of a long-running calculation.

"),

("Memory-mapped I/O","Base","mmap","mmap(len, prot, flags, fd, offset)

   Low-level interface to the mmap system call. See the man page.

"),

("Memory-mapped I/O","Base","munmap","munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With
   mmap_array you do not need to call this directly; the memory is
   unmapped for you when the array goes out of scope.

"),

("Mathematical Functions","Base","-","-(x)

   Unary minus operator.

"),

("Mathematical Functions","Base","+","+(x, y)

   Binary addition operator.

"),

("Mathematical Functions","Base","-","-(x, y)

   Binary subtraction operator.

"),

("Mathematical Functions","Base","*","*(x, y)

   Binary multiplication operator.

"),

("Mathematical Functions","Base","/","/(x, y)

   Binary left-division operator.

"),

("Mathematical Functions","Base","\\","\\(x, y)

   Binary right-division operator.

"),

("Mathematical Functions","Base","^","^(x, y)

   Binary exponentiation operator.

"),

("Mathematical Functions","Base",".+",".+(x, y)

   Element-wise binary addition operator.

"),

("Mathematical Functions","Base",".-",".-(x, y)

   Element-wise binary subtraction operator.

"),

("Mathematical Functions","Base",".*",".*(x, y)

   Element-wise binary multiplication operator.

"),

("Mathematical Functions","Base","./","./(x, y)

   Element-wise binary left division operator.

"),

("Mathematical Functions","Base",".\\",".\\(x, y)

   Element-wise binary right division operator.

"),

("Mathematical Functions","Base",".^",".^(x, y)

   Element-wise binary exponentiation operator.

"),

("Mathematical Functions","Base","div","div(a, b)

   Compute a/b, truncating to an integer

"),

("Mathematical Functions","Base","fld","fld(a, b)

   Largest integer less than or equal to a/b

"),

("Mathematical Functions","Base","mod","mod(x, m)

   Modulus after division, returning in the range [0,m)

"),

("Mathematical Functions","Base","rem","rem(x, m)

   Remainder after division

"),

("Mathematical Functions","Base","%","%(x, m)

   Remainder after division. The operator form of \"rem\".

"),

("Mathematical Functions","Base","mod1","mod1(x, m)

   Modulus after division, returning in the range (0,m]

"),

("Mathematical Functions","Base","//","//(num, den)

   Rational division

"),

("Mathematical Functions","Base","num","num(x)

   Numerator of the rational representation of \"x\"

"),

("Mathematical Functions","Base","den","den(x)

   Denominator of the rational representation of \"x\"

"),

("Mathematical Functions","Base","<<","<<(x, n)

   Left shift operator.

"),

("Mathematical Functions","Base",">>",">>(x, n)

   Right shift operator.

"),

("Mathematical Functions","Base","==","==(x, y)

   Equality comparison operator.

"),

("Mathematical Functions","Base","!=","!=(x, y)

   Not-equals comparison operator.

"),

("Mathematical Functions","Base","<","<(x, y)

   Less-than comparison operator.

"),

("Mathematical Functions","Base","<=","<=(x, y)

   Less-than-or-equals comparison operator.

"),

("Mathematical Functions","Base",">",">(x, y)

   Greater-than comparison operator.

"),

("Mathematical Functions","Base",">=",">=(x, y)

   Greater-than-or-equals comparison operator.

"),

("Mathematical Functions","Base",".==",".==(x, y)

   Element-wise equality comparison operator.

"),

("Mathematical Functions","Base",".!=",".!=(x, y)

   Element-wise not-equals comparison operator.

"),

("Mathematical Functions","Base",".<",".<(x, y)

   Element-wise less-than comparison operator.

"),

("Mathematical Functions","Base",".<=",".<=(x, y)

   Element-wise less-than-or-equals comparison operator.

"),

("Mathematical Functions","Base",".>",".>(x, y)

   Element-wise greater-than comparison operator.

"),

("Mathematical Functions","Base",".>=",".>=(x, y)

   Element-wise greater-than-or-equals comparison operator.

"),

("Mathematical Functions","Base","cmp","cmp(x, y)

   Return -1, 0, or 1 depending on whether \"x<y\", \"x==y\", or
   \"x>y\", respectively

"),

("Mathematical Functions","Base","!","!(x)

   Boolean not

"),

("Mathematical Functions","Base","~","~(x)

   Bitwise not

"),

("Mathematical Functions","Base","&","&(x, y)

   Bitwise and

"),

("Mathematical Functions","Base","|","|(x, y)

   Bitwise or

"),

("Mathematical Functions","Base","\$","\$(x, y)

   Bitwise exclusive or

"),

("Mathematical Functions","Base","sin","sin(x)

   Compute sine of \"x\", where \"x\" is in radians

"),

("Mathematical Functions","Base","cos","cos(x)

   Compute cosine of \"x\", where \"x\" is in radians

"),

("Mathematical Functions","Base","tan","tan(x)

   Compute tangent of \"x\", where \"x\" is in radians

"),

("Mathematical Functions","Base","sind","sind(x)

   Compute sine of \"x\", where \"x\" is in degrees

"),

("Mathematical Functions","Base","cosd","cosd(x)

   Compute cosine of \"x\", where \"x\" is in degrees

"),

("Mathematical Functions","Base","tand","tand(x)

   Compute tangent of \"x\", where \"x\" is in degrees

"),

("Mathematical Functions","Base","sinh","sinh(x)

   Compute hyperbolic sine of \"x\"

"),

("Mathematical Functions","Base","cosh","cosh(x)

   Compute hyperbolic cosine of \"x\"

"),

("Mathematical Functions","Base","tanh","tanh(x)

   Compute hyperbolic tangent of \"x\"

"),

("Mathematical Functions","Base","asin","asin(x)

   Compute the inverse sine of \"x\", where the output is in radians

"),

("Mathematical Functions","Base","acos","acos(x)

   Compute the inverse cosine of \"x\", where the output is in radians

"),

("Mathematical Functions","Base","atan","atan(x)

   Compute the inverse tangent of \"x\", where the output is in
   radians

"),

("Mathematical Functions","Base","atan2","atan2(y, x)

   Compute the inverse tangent of \"y/x\", using the signs of both
   \"x\" and \"y\" to determine the quadrant of the return value.

"),

("Mathematical Functions","Base","asind","asind(x)

   Compute the inverse sine of \"x\", where the output is in degrees

"),

("Mathematical Functions","Base","acosd","acosd(x)

   Compute the inverse cosine of \"x\", where the output is in degrees

"),

("Mathematical Functions","Base","atand","atand(x)

   Compute the inverse tangent of \"x\", where the output is in
   degrees

"),

("Mathematical Functions","Base","sec","sec(x)

   Compute the secant of \"x\", where \"x\" is in radians

"),

("Mathematical Functions","Base","csc","csc(x)

   Compute the cosecant of \"x\", where \"x\" is in radians

"),

("Mathematical Functions","Base","cot","cot(x)

   Compute the cotangent of \"x\", where \"x\" is in radians

"),

("Mathematical Functions","Base","secd","secd(x)

   Compute the secant of \"x\", where \"x\" is in degrees

"),

("Mathematical Functions","Base","cscd","cscd(x)

   Compute the cosecant of \"x\", where \"x\" is in degrees

"),

("Mathematical Functions","Base","cotd","cotd(x)

   Compute the cotangent of \"x\", where \"x\" is in degrees

"),

("Mathematical Functions","Base","asec","asec(x)

   Compute the inverse secant of \"x\", where the output is in radians

"),

("Mathematical Functions","Base","acsc","acsc(x)

   Compute the inverse cosecant of \"x\", where the output is in
   radians

"),

("Mathematical Functions","Base","acot","acot(x)

   Compute the inverse cotangent of \"x\", where the output is in
   radians

"),

("Mathematical Functions","Base","asecd","asecd(x)

   Compute the inverse secant of \"x\", where the output is in degrees

"),

("Mathematical Functions","Base","acscd","acscd(x)

   Compute the inverse cosecant of \"x\", where the output is in
   degrees

"),

("Mathematical Functions","Base","acotd","acotd(x)

   Compute the inverse cotangent of \"x\", where the output is in
   degrees

"),

("Mathematical Functions","Base","sech","sech(x)

   Compute the hyperbolic secant of \"x\"

"),

("Mathematical Functions","Base","csch","csch(x)

   Compute the hyperbolic cosecant of \"x\"

"),

("Mathematical Functions","Base","coth","coth(x)

   Compute the hyperbolic cotangent of \"x\"

"),

("Mathematical Functions","Base","asinh","asinh(x)

   Compute the inverse hyperbolic sine of \"x\"

"),

("Mathematical Functions","Base","acosh","acosh(x)

   Compute the inverse hyperbolic cosine of \"x\"

"),

("Mathematical Functions","Base","atanh","atanh(x)

   Compute the inverse hyperbolic cotangent of \"x\"

"),

("Mathematical Functions","Base","asech","asech(x)

   Compute the inverse hyperbolic secant of \"x\"

"),

("Mathematical Functions","Base","acsch","acsch(x)

   Compute the inverse hyperbolic cosecant of \"x\"

"),

("Mathematical Functions","Base","acoth","acoth(x)

   Compute the inverse hyperbolic cotangent of \"x\"

"),

("Mathematical Functions","Base","sinc","sinc(x)

   Compute \\sin(\\pi x) / x

"),

("Mathematical Functions","Base","cosc","cosc(x)

   Compute \\cos(\\pi x) / x

"),

("Mathematical Functions","Base","degrees2radians","degrees2radians(x)

   Convert \"x\" from degrees to radians

"),

("Mathematical Functions","Base","radians2degrees","radians2degrees(x)

   Convert \"x\" from radians to degrees

"),

("Mathematical Functions","Base","hypot","hypot(x, y)

   Compute the \\sqrt{x^2+y^2} without undue overflow or underflow

"),

("Mathematical Functions","Base","log","log(x)

   Compute the natural logarithm of \"x\"

"),

("Mathematical Functions","Base","log2","log2(x)

   Compute the natural logarithm of \"x\" to base 2

"),

("Mathematical Functions","Base","log10","log10(x)

   Compute the natural logarithm of \"x\" to base 10

"),

("Mathematical Functions","Base","log1p","log1p(x)

   Accurate natural logarithm of \"1+x\"

"),

("Mathematical Functions","Base","frexp","frexp(val, exp)

   Return a number \"x\" such that it has a magnitude in the interval
   \"[1/2, 1)\" or 0, and val = x \\times 2^{exp}.

"),

("Mathematical Functions","Base","exp","exp(x)

   Compute e^x

"),

("Mathematical Functions","Base","exp2","exp2(x)

   Compute 2^x

"),

("Mathematical Functions","Base","ldexp","ldexp(x, n)

   Compute x \\times 2^n

"),

("Mathematical Functions","Base","modf","modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts
   of a number. Both parts have the same sign as the argument.

"),

("Mathematical Functions","Base","expm1","expm1(x)

   Accurately compute e^x-1

"),

("Mathematical Functions","Base","square","square(x)

   Compute x^2

"),

("Mathematical Functions","Base","round","round(x[, digits[, base]]) -> FloatingPoint

   \"round(x)\" returns the nearest integer to \"x\". \"round(x,
   digits)\" rounds to the specified number of digits after the
   decimal place, or before if negative, e.g., \"round(pi,2)\" is
   \"3.14\". \"round(x, digits, base)\" rounds using a different base,
   defaulting to 10, e.g., \"round(pi, 3, 2)\" is \"3.125\".

"),

("Mathematical Functions","Base","ceil","ceil(x[, digits[, base]]) -> FloatingPoint

   Returns the nearest integer not less than \"x\". \"digits\" and
   \"base\" work as above.

"),

("Mathematical Functions","Base","floor","floor(x[, digits[, base]]) -> FloatingPoint

   Returns the nearest integer not greater than \"x\". \"digits\" and
   \"base\" work as above.

"),

("Mathematical Functions","Base","trunc","trunc(x[, digits[, base]]) -> FloatingPoint

   Returns the nearest integer not greater in magnitude than \"x\".
   \"digits\" and \"base\" work as above.

"),

("Mathematical Functions","Base","iround","iround(x) -> Integer

   Returns the nearest integer to \"x\".

"),

("Mathematical Functions","Base","iceil","iceil(x) -> Integer

   Returns the nearest integer not less than \"x\".

"),

("Mathematical Functions","Base","ifloor","ifloor(x) -> Integer

   Returns the nearest integer not greater than \"x\".

"),

("Mathematical Functions","Base","itrunc","itrunc(x) -> Integer

   Returns the nearest integer not greater in magnitude than \"x\".

"),

("Mathematical Functions","Base","signif","signif(x, digits[, base]) -> FloatingPoint

   Rounds (in the sense of \"round\") \"x\" so that there are
   \"digits\" significant digits, under a base \"base\"
   representation, default 10. E.g., \"signif(123.456, 2)\" is
   \"120.0\", and \"signif(357.913, 4, 2)\" is \"352.0\".

"),

("Mathematical Functions","Base","min","min(x, y)

   Return the minimum of \"x\" and \"y\"

"),

("Mathematical Functions","Base","max","max(x, y)

   Return the maximum of \"x\" and \"y\"

"),

("Mathematical Functions","Base","clamp","clamp(x, lo, hi)

   Return x if \"lo <= x <= y\". If \"x < lo\", return \"lo\". If \"x
   > hi\", return \"hi\".

"),

("Mathematical Functions","Base","abs","abs(x)

   Absolute value of \"x\"

"),

("Mathematical Functions","Base","abs2","abs2(x)

   Squared absolute value of \"x\"

"),

("Mathematical Functions","Base","copysign","copysign(x, y)

   Return \"x\" such that it has the same sign as \"y\"

"),

("Mathematical Functions","Base","sign","sign(x)

   Return \"+1\" if \"x\" is positive, \"0\" if \"x == 0\", and \"-1\"
   if \"x\" is negative.

"),

("Mathematical Functions","Base","signbit","signbit(x)

   Returns \"1\" if the value of the sign of \"x\" is negative,
   otherwise \"0\".

"),

("Mathematical Functions","Base","flipsign","flipsign(x, y)

   Return \"x\" with its sign flipped if \"y\" is negative. For
   example \"abs(x) = flipsign(x,x)\".

"),

("Mathematical Functions","Base","sqrt","sqrt(x)

   Return \\sqrt{x}

"),

("Mathematical Functions","Base","cbrt","cbrt(x)

   Return x^{1/3}

"),

("Mathematical Functions","Base","erf","erf(x)

   Compute the error function of \"x\", defined by
   \\frac{2}{\\sqrt{\\pi}} \\int_0^x e^{-t^2} dt for arbitrary complex
   \"x\".

"),

("Mathematical Functions","Base","erfc","erfc(x)

   Compute the complementary error function of \"x\", defined by 1 -
   \\operatorname{erf}(x).

"),

("Mathematical Functions","Base","erfcx","erfcx(x)

   Compute the scaled complementary error function of \"x\", defined
   by e^{x^2} \\operatorname{erfc}(x).  Note also that
   \\operatorname{erfcx}(-ix) computes the Faddeeva function w(x).

"),

("Mathematical Functions","Base","erfi","erfi(x)

   Compute the imaginary error function of \"x\", defined by -i
   \\operatorname{erf}(ix).

"),

("Mathematical Functions","Base","dawson","dawson(x)

   Compute the Dawson function (scaled imaginary error function) of
   \"x\", defined by \\frac{\\sqrt{\\pi}}{2} e^{-x^2}
   \\operatorname{erfi}(x).

"),

("Mathematical Functions","Base","real","real(z)

   Return the real part of the complex number \"z\"

"),

("Mathematical Functions","Base","imag","imag(z)

   Return the imaginary part of the complex number \"z\"

"),

("Mathematical Functions","Base","reim","reim(z)

   Return both the real and imaginary parts of the complex number
   \"z\"

"),

("Mathematical Functions","Base","conj","conj(z)

   Compute the complex conjugate of a complex number \"z\"

"),

("Mathematical Functions","Base","angle","angle(z)

   Compute the phase angle of a complex number \"z\"

"),

("Mathematical Functions","Base","cis","cis(z)

   Return \"cos(z) + i*sin(z)\" if z is real. Return \"(cos(real(z)) +
   i*sin(real(z)))/exp(imag(z))\" if \"z\" is complex

"),

("Mathematical Functions","Base","binomial","binomial(n, k)

   Number of ways to choose \"k\" out of \"n\" items

"),

("Mathematical Functions","Base","factorial","factorial(n)

   Factorial of n

"),

("Mathematical Functions","Base","factorial","factorial(n, k)

   Compute \"factorial(n)/factorial(k)\"

"),

("Mathematical Functions","Base","factor","factor(n)

   Compute the prime factorization of an integer \"n\". Returns a
   dictionary. The keys of the dictionary correspond to the factors,
   and hence are of the same type as \"n\". The value associated with
   each key indicates the number of times the factor appears in the
   factorization.

   **Example**: 100=2*2*5*5; then, \"factor(100) -> [5=>2,2=>2]\"

"),

("Mathematical Functions","Base","gcd","gcd(x, y)

   Greatest common divisor

"),

("Mathematical Functions","Base","lcm","lcm(x, y)

   Least common multiple

"),

("Mathematical Functions","Base","gcdx","gcdx(x, y)

   Greatest common divisor, also returning integer coefficients \"u\"
   and \"v\" that solve \"ux+vy == gcd(x,y)\"

"),

("Mathematical Functions","Base","ispow2","ispow2(n)

   Test whether \"n\" is a power of two

"),

("Mathematical Functions","Base","nextpow2","nextpow2(n)

   Next power of two not less than \"n\"

"),

("Mathematical Functions","Base","prevpow2","prevpow2(n)

   Previous power of two not greater than \"n\"

"),

("Mathematical Functions","Base","nextpow","nextpow(a, n)

   Next power of \"a\" not less than \"n\"

"),

("Mathematical Functions","Base","prevpow","prevpow(a, n)

   Previous power of \"a\" not greater than \"n\"

"),

("Mathematical Functions","Base","nextprod","nextprod([a, b, c], n)

   Next integer not less than \"n\" that can be written \"a^i1 * b^i2
   * c^i3\" for integers \"i1\", \"i2\", \"i3\".

"),

("Mathematical Functions","Base","prevprod","prevprod([a, b, c], n)

   Previous integer not greater than \"n\" that can be written \"a^i1
   * b^i2 * c^i3\" for integers \"i1\", \"i2\", \"i3\".

"),

("Mathematical Functions","Base","invmod","invmod(x, m)

   Inverse of \"x\", modulo \"m\"

"),

("Mathematical Functions","Base","powermod","powermod(x, p, m)

   Compute \"mod(x^p, m)\"

"),

("Mathematical Functions","Base","gamma","gamma(x)

   Compute the gamma function of \"x\"

"),

("Mathematical Functions","Base","lgamma","lgamma(x)

   Compute the logarithm of \"gamma(x)\"

"),

("Mathematical Functions","Base","lfact","lfact(x)

   Compute the logarithmic factorial of \"x\"

"),

("Mathematical Functions","Base","digamma","digamma(x)

   Compute the digamma function of \"x\" (the logarithmic derivative
   of \"gamma(x)\")

"),

("Mathematical Functions","Base","airy","airy(k, x)

   kth derivative of the Airy function \\operatorname{Ai}(x).

"),

("Mathematical Functions","Base","airyai","airyai(x)

   Airy function \\operatorname{Ai}(x).

"),

("Mathematical Functions","Base","airyprime","airyprime(x)

   Airy function derivative \\operatorname{Ai}'(x).

"),

("Mathematical Functions","Base","airyaiprime","airyaiprime(x)

   Airy function derivative \\operatorname{Ai}'(x).

"),

("Mathematical Functions","Base","airybi","airybi(x)

   Airy function \\operatorname{Bi}(x).

"),

("Mathematical Functions","Base","airybiprime","airybiprime(x)

   Airy function derivative \\operatorname{Bi}'(x).

"),

("Mathematical Functions","Base","besselj0","besselj0(x)

   Bessel function of the first kind of order 0, J_0(x).

"),

("Mathematical Functions","Base","besselj1","besselj1(x)

   Bessel function of the first kind of order 1, J_1(x).

"),

("Mathematical Functions","Base","besselj","besselj(nu, x)

   Bessel function of the first kind of order \"nu\", J_\\nu(x).

"),

("Mathematical Functions","Base","bessely0","bessely0(x)

   Bessel function of the second kind of order 0, Y_0(x).

"),

("Mathematical Functions","Base","bessely1","bessely1(x)

   Bessel function of the second kind of order 1, Y_1(x).

"),

("Mathematical Functions","Base","bessely","bessely(nu, x)

   Bessel function of the second kind of order \"nu\", Y_\\nu(x).

"),

("Mathematical Functions","Base","hankelh1","hankelh1(nu, x)

   Bessel function of the third kind of order \"nu\", H^{(1)}_\\nu(x).

"),

("Mathematical Functions","Base","hankelh2","hankelh2(nu, x)

   Bessel function of the third kind of order \"nu\", H^{(2)}_\\nu(x).

"),

("Mathematical Functions","Base","besseli","besseli(nu, x)

   Modified Bessel function of the first kind of order \"nu\",
   I_\\nu(x).

"),

("Mathematical Functions","Base","besselk","besselk(nu, x)

   Modified Bessel function of the second kind of order \"nu\",
   K_\\nu(x).

"),

("Mathematical Functions","Base","beta","beta(x, y)

   Euler integral of the first kind \\operatorname{B}(x,y) =
   \\Gamma(x)\\Gamma(y)/\\Gamma(x+y).

"),

("Mathematical Functions","Base","lbeta","lbeta(x, y)

   Natural logarithm of the beta function
   \\log(\\operatorname{B}(x,y)).

"),

("Mathematical Functions","Base","eta","eta(x)

   Dirichlet eta function \\eta(s) =
   \\sum^\\infty_{n=1}(-)^{n-1}/n^{s}.

"),

("Mathematical Functions","Base","zeta","zeta(x)

   Riemann zeta function \\zeta(s).

"),

("Mathematical Functions","Base","bitmix","bitmix(x, y)

   Hash two integers into a single integer. Useful for constructing
   hash functions.

"),

("Mathematical Functions","Base","ndigits","ndigits(n, b)

   Compute the number of digits in number \"n\" written in base \"b\".

"),

("Data Formats","Base","bin","bin(n[, pad])

   Convert an integer to a binary string, optionally specifying a
   number of digits to pad to.

"),

("Data Formats","Base","hex","hex(n[, pad])

   Convert an integer to a hexadecimal string, optionally specifying a
   number of digits to pad to.

"),

("Data Formats","Base","dec","dec(n[, pad])

   Convert an integer to a decimal string, optionally specifying a
   number of digits to pad to.

"),

("Data Formats","Base","oct","oct(n[, pad])

   Convert an integer to an octal string, optionally specifying a
   number of digits to pad to.

"),

("Data Formats","Base","base","base(base, n[, pad])

   Convert an integer to a string in the given base, optionally
   specifying a number of digits to pad to. The base can be specified
   as either an integer, or as a \"Uint8\" array of character values
   to use as digit symbols.

"),

("Data Formats","Base","bits","bits(n)

   A string giving the literal bit representation of a number.

"),

("Data Formats","Base","parse_int","parse_int(type, str[, base])

   Parse a string as an integer in the given base (default 10),
   yielding a number of the specified type.

"),

("Data Formats","Base","parse_bin","parse_bin(type, str)

   Parse a string as an integer in base 2, yielding a number of the
   specified type.

"),

("Data Formats","Base","parse_oct","parse_oct(type, str)

   Parse a string as an integer in base 8, yielding a number of the
   specified type.

"),

("Data Formats","Base","parse_hex","parse_hex(type, str)

   Parse a string as an integer in base 16, yielding a number of the
   specified type.

"),

("Data Formats","Base","parse_float","parse_float(type, str)

   Parse a string as a decimal floating point number, yielding a
   number of the specified type.

"),

("Data Formats","Base","bool","bool(x)

   Convert a number or numeric array to boolean

"),

("Data Formats","Base","isbool","isbool(x)

   Test whether number or array is boolean

"),

("Data Formats","Base","int","int(x)

   Convert a number or array to the default integer type on your
   platform. Alternatively, \"x\" can be a string, which is parsed as
   an integer.

"),

("Data Formats","Base","uint","uint(x)

   Convert a number or array to the default unsigned integer type on
   your platform. Alternatively, \"x\" can be a string, which is
   parsed as an unsigned integer.

"),

("Data Formats","Base","integer","integer(x)

   Convert a number or array to integer type. If \"x\" is already of
   integer type it is unchanged, otherwise it converts it to the
   default integer type on your platform.

"),

("Data Formats","Base","isinteger","isinteger(x)

   Test whether a number or array is of integer type

"),

("Data Formats","Base","signed","signed(x)

   Convert a number to a signed integer

"),

("Data Formats","Base","unsigned","unsigned(x)

   Convert a number to an unsigned integer

"),

("Data Formats","Base","int8","int8(x)

   Convert a number or array to \"Int8\" data type

"),

("Data Formats","Base","int16","int16(x)

   Convert a number or array to \"Int16\" data type

"),

("Data Formats","Base","int32","int32(x)

   Convert a number or array to \"Int32\" data type

"),

("Data Formats","Base","int64","int64(x)

   Convert a number or array to \"Int64\" data type

"),

("Data Formats","Base","int128","int128(x)

   Convert a number or array to \"Int128\" data type

"),

("Data Formats","Base","uint8","uint8(x)

   Convert a number or array to \"Uint8\" data type

"),

("Data Formats","Base","uint16","uint16(x)

   Convert a number or array to \"Uint16\" data type

"),

("Data Formats","Base","uint32","uint32(x)

   Convert a number or array to \"Uint32\" data type

"),

("Data Formats","Base","uint64","uint64(x)

   Convert a number or array to \"Uint64\" data type

"),

("Data Formats","Base","uint128","uint128(x)

   Convert a number or array to \"Uint128\" data type

"),

("Data Formats","Base","float32","float32(x)

   Convert a number or array to \"Float32\" data type

"),

("Data Formats","Base","float64","float64(x)

   Convert a number or array to \"Float64\" data type

"),

("Data Formats","Base","float","float(x)

   Convert a number, array, or string to a \"FloatingPoint\" data
   type. For numeric data, the smallest suitable \"FloatingPoint\"
   type is used. For strings, it converts to \"Float64\".

"),

("Data Formats","Base","significand","significand(x)

   Extract the significand(s) (a.k.a. mantissa), in binary
   representation, of a floating-point number or array.

   For example, \"significand(15.2)/15.2 == 0.125\", and
   \"significand(15.2)*8 == 15.2\"

"),

("Data Formats","Base","exponent","exponent(x) -> Int

   Get the exponent of a normalized floating-point number.

"),

("Data Formats","Base","float64_valued","float64_valued(x::Rational)

   True if \"x\" can be losslessly represented as a \"Float64\" data
   type

"),

("Data Formats","Base","complex64","complex64(r, i)

   Convert to \"r+i*im\" represented as a \"Complex64\" data type

"),

("Data Formats","Base","complex128","complex128(r, i)

   Convert to \"r+i*im\" represented as a \"Complex128\" data type

"),

("Data Formats","Base","char","char(x)

   Convert a number or array to \"Char\" data type

"),

("Data Formats","Base","complex","complex(r, i)

   Convert real numbers or arrays to complex

"),

("Data Formats","Base","iscomplex","iscomplex(x) -> Bool

   Test whether a number or array is of a complex type

"),

("Data Formats","Base","isreal","isreal(x) -> Bool

   Test whether a number or array is of a real type

"),

("Data Formats","Base","bswap","bswap(n)

   Byte-swap an integer

"),

("Data Formats","Base","num2hex","num2hex(f)

   Get a hexadecimal string of the binary representation of a floating
   point number

"),

("Data Formats","Base","hex2num","hex2num(str)

   Convert a hexadecimal string to the floating point number it
   represents

"),

("Numbers","Base","one","one(x)

   Get the multiplicative identity element for the type of x (x can
   also specify the type itself). For matrices, returns an identity
   matrix of the appropriate size and type.

"),

("Numbers","Base","zero","zero(x)

   Get the additive identity element for the type of x (x can also
   specify the type itself).

"),

("Numbers","Base","pi","pi

   The constant pi

"),

("Numbers","Base","im","im

   The imaginary unit

"),

("Numbers","Base","e","e

   The constant e

"),

("Numbers","Base","Inf","Inf

   Positive infinity of type Float64

"),

("Numbers","Base","Inf32","Inf32

   Positive infinity of type Float32

"),

("Numbers","Base","NaN","NaN

   A not-a-number value of type Float64

"),

("Numbers","Base","NaN32","NaN32

   A not-a-number value of type Float32

"),

("Numbers","Base","isdenormal","isdenormal(f) -> Bool

   Test whether a floating point number is denormal

"),

("Numbers","Base","isfinite","isfinite(f) -> Bool

   Test whether a number is finite

"),

("Numbers","Base","isinf","isinf(f)

   Test whether a number is infinite

"),

("Numbers","Base","isnan","isnan(f)

   Test whether a floating point number is not a number (NaN)

"),

("Numbers","Base","inf","inf(f)

   Returns infinity in the same floating point type as \"f\" (or \"f\"
   can by the type itself)

"),

("Numbers","Base","nan","nan(f)

   Returns NaN in the same floating point type as \"f\" (or \"f\" can
   by the type itself)

"),

("Numbers","Base","nextfloat","nextfloat(f)

   Get the next floating point number in lexicographic order

"),

("Numbers","Base","prevfloat","prevfloat(f) -> Float

   Get the previous floating point number in lexicographic order

"),

("Numbers","Base","integer_valued","integer_valued(x)

   Test whether \"x\" is numerically equal to some integer

"),

("Numbers","Base","real_valued","real_valued(x)

   Test whether \"x\" is numerically equal to some real number

"),

("Numbers","Base","BigInt","BigInt(x)

   Create an arbitrary precision integer. \"x\" may be an \"Int\" (or
   anything that can be converted to an \"Int\") or a \"String\". The
   usual mathematical operators are defined for this type, and results
   are promoted to a \"BigInt\".

"),

("Numbers","Base","BigFloat","BigFloat(x)

   Create an arbitrary precision floating point number. \"x\" may be
   an \"Integer\", a \"Float64\", a \"String\" or a \"BigInt\". The
   usual mathematical operators are defined for this type, and results
   are promoted to a \"BigFloat\".

"),

("Numbers","Base","count_ones","count_ones(x::Integer) -> Integer

   Number of ones in the binary representation of \"x\".

   **Example**: \"count_ones(7) -> 3\"

"),

("Numbers","Base","count_zeros","count_zeros(x::Integer) -> Integer

   Number of zeros in the binary representation of \"x\".

   **Example**: \"count_zeros(int32(2 ^ 16 - 1)) -> 16\"

"),

("Numbers","Base","leading_zeros","leading_zeros(x::Integer) -> Integer

   Number of zeros leading the binary representation of \"x\".

   **Example**: \"leading_zeros(int32(1)) -> 31\"

"),

("Numbers","Base","leading_ones","leading_ones(x::Integer) -> Integer

   Number of ones leading the binary representation of \"x\".

   **Example**: \"leading_ones(int32(2 ^ 32 - 2)) -> 31\"

"),

("Numbers","Base","trailing_zeros","trailing_zeros(x::Integer) -> Integer

   Number of zeros trailing the binary representation of \"x\".

   **Example**: \"trailing_zeros(2) -> 1\"

"),

("Numbers","Base","trailing_ones","trailing_ones(x::Integer) -> Integer

   Number of ones trailing the binary representation of \"x\".

   **Example**: \"trailing_ones(3) -> 2\"

"),

("Numbers","Base","isprime","isprime(x::Integer) -> Bool

   Returns \"true\" if \"x\" is prime, and \"false\" otherwise.

   **Example**: \"isprime(3) -> true\"

"),

("Numbers","Base","isodd","isodd(x::Integer) -> Bool

   Returns \"true\" if \"x\" is odd (that is, not divisible by 2), and
   \"false\" otherwise.

   **Example**: \"isodd(9) -> false\"

"),

("Numbers","Base","iseven","iseven(x::Integer) -> Bool

   Returns \"true\" is \"x\" is even (that is, divisible by 2), and
   \"false\" otherwise.

   **Example**: \"iseven(1) -> false\"

"),

("Random Numbers","Base","srand","srand([rng], seed)

   Seed the RNG with a \"seed\", which may be an unsigned integer or a
   vector of unsigned integers. \"seed\" can even be a filename, in
   which case the seed is read from a file. If the argument \"rng\" is
   not provided, the default global RNG is seeded.

"),

("Random Numbers","Base","MersenneTwister","MersenneTwister([seed])

   Create a \"MersenneTwister\" RNG object. Different RNG objects can
   have their own seeds, which may be useful for generating different
   streams of random numbers.

"),

("Random Numbers","Base","rand","rand()

   Generate a \"Float64\" random number uniformly in [0,1)

"),

("Random Numbers","Base","rand!","rand!([rng], A)

   Populate the array A with random number generated from the
   specified RNG.

"),

("Random Numbers","Base","rand","rand(rng::AbstractRNG[, dims...])

   Generate a random \"Float64\" number or array of the size specified
   by dims, using the specified RNG object. Currently,
   \"MersenneTwister\" is the only available Random Number Generator
   (RNG), which may be seeded using srand.

"),

("Random Numbers","Base","rand","rand(dims or [dims...])

   Generate a random \"Float64\" array of the size specified by dims

"),

("Random Numbers","Base","rand","rand(Int32|Uint32|Int64|Uint64|Int128|Uint128[, dims...])

   Generate a random integer of the given type. Optionally, generate
   an array of random integers of the given type by specifying dims.

"),

("Random Numbers","Base","rand","rand(r[, dims...])

   Generate a random integer from the inclusive interval specified by
   \"Range1 r\" (for example, \"1:n\"). Optionally, generate a random
   integer array.

"),

("Random Numbers","Base","randbool","randbool([dims...])

   Generate a random boolean value. Optionally, generate an array of
   random boolean values.

"),

("Random Numbers","Base","randbool!","randbool!(A)

   Fill an array with random boolean values. A may be an \"Array\" or
   a \"BitArray\".

"),

("Random Numbers","Base","randn","randn(dims or [dims...])

   Generate a normally-distributed random number with mean 0 and
   standard deviation 1. Optionally generate an array of normally-
   distributed random numbers.

"),

("Arrays","Base","ndims","ndims(A) -> Integer

   Returns the number of dimensions of A

"),

("Arrays","Base","size","size(A)

   Returns a tuple containing the dimensions of A

"),

("Arrays","Base","eltype","eltype(A)

   Returns the type of the elements contained in A

"),

("Arrays","Base","length","length(A) -> Integer

   Returns the number of elements in A (note that this differs from
   MATLAB where \"length(A)\" is the largest dimension of \"A\")

"),

("Arrays","Base","nnz","nnz(A)

   Counts the number of nonzero values in array A (dense or sparse)

"),

("Arrays","Base","scale!","scale!(A, k)

   Scale the contents of an array A with k (in-place)

"),

("Arrays","Base","conj!","conj!(A)

   Convert an array to its complex conjugate in-place

"),

("Arrays","Base","stride","stride(A, k)

   Returns the distance in memory (in number of elements) between
   adjacent elements in dimension k

"),

("Arrays","Base","strides","strides(A)

   Returns a tuple of the memory strides in each dimension

"),

("Arrays","Base","Array","Array(type, dims)

   Construct an uninitialized dense array. \"dims\" may be a tuple or
   a series of integer arguments.

"),

("Arrays","Base","getindex","getindex(type[, elements...])

   Construct a 1-d array of the specified type. This is usually called
   with the syntax \"Type[]\". Element values can be specified using
   \"Type[a,b,c,...]\".

"),

("Arrays","Base","cell","cell(dims)

   Construct an uninitialized cell array (heterogeneous array).
   \"dims\" can be either a tuple or a series of integer arguments.

"),

("Arrays","Base","zeros","zeros(type, dims)

   Create an array of all zeros of specified type

"),

("Arrays","Base","ones","ones(type, dims)

   Create an array of all ones of specified type

"),

("Arrays","Base","trues","trues(dims)

   Create a Bool array with all values set to true

"),

("Arrays","Base","falses","falses(dims)

   Create a Bool array with all values set to false

"),

("Arrays","Base","fill","fill(v, dims)

   Create an array filled with \"v\"

"),

("Arrays","Base","fill!","fill!(A, x)

   Fill array \"A\" with value \"x\"

"),

("Arrays","Base","reshape","reshape(A, dims)

   Create an array with the same data as the given array, but with
   different dimensions. An implementation for a particular type of
   array may choose whether the data is copied or shared.

"),

("Arrays","Base","similar","similar(array, element_type, dims)

   Create an uninitialized array of the same type as the given array,
   but with the specified element type and dimensions. The second and
   third arguments are both optional. The \"dims\" argument may be a
   tuple or a series of integer arguments.

"),

("Arrays","Base","reinterpret","reinterpret(type, A)

   Construct an array with the same binary data as the given array,
   but with the specified element type

"),

("Arrays","Base","eye","eye(n)

   n-by-n identity matrix

"),

("Arrays","Base","eye","eye(m, n)

   m-by-n identity matrix

"),

("Arrays","Base","linspace","linspace(start, stop, n)

   Construct a vector of \"n\" linearly-spaced elements from \"start\"
   to \"stop\".

"),

("Arrays","Base","logspace","logspace(start, stop, n)

   Construct a vector of \"n\" logarithmically-spaced numbers from
   \"10^start\" to \"10^stop\".

"),

("Arrays","Base","bsxfun","bsxfun(fn, A, B[, C...])

   Apply binary function \"fn\" to two or more arrays, with singleton
   dimensions expanded.

"),

("Arrays","Base","getindex","getindex(A, ind)

   Returns a subset of array \"A\" as specified by \"ind\", which may
   be an \"Int\", a \"Range\", or a \"Vector\".

"),

("Arrays","Base","sub","sub(A, ind)

   Returns a SubArray, which stores the input \"A\" and \"ind\" rather
   than computing the result immediately. Calling \"getindex\" on a
   SubArray computes the indices on the fly.

"),

("Arrays","Base","slicedim","slicedim(A, d, i)

   Return all the data of \"A\" where the index for dimension \"d\"
   equals \"i\". Equivalent to \"A[:,:,...,i,:,:,...]\" where \"i\" is
   in position \"d\".

"),

("Arrays","Base","setindex!","setindex!(A, X, ind)

   Store values from array \"X\" within some subset of \"A\" as
   specified by \"ind\".

"),

("Arrays","Base","cat","cat(dim, A...)

   Concatenate the input arrays along the specified dimension

"),

("Arrays","Base","vcat","vcat(A...)

   Concatenate along dimension 1

"),

("Arrays","Base","hcat","hcat(A...)

   Concatenate along dimension 2

"),

("Arrays","Base","hvcat","hvcat(rows::(Int...), values...)

   Horizontal and vertical concatenation in one call. This function is
   called for block matrix syntax. The first argument specifies the
   number of arguments to concatenate in each block row. For example,
   \"[a b;c d e]\" calls \"hvcat((2,3),a,b,c,d,e)\".

"),

("Arrays","Base","flipdim","flipdim(A, d)

   Reverse \"A\" in dimension \"d\".

"),

("Arrays","Base","flipud","flipud(A)

   Equivalent to \"flipdim(A,1)\".

"),

("Arrays","Base","fliplr","fliplr(A)

   Equivalent to \"flipdim(A,2)\".

"),

("Arrays","Base","circshift","circshift(A, shifts)

   Circularly shift the data in an array. The second argument is a
   vector giving the amount to shift in each dimension.

"),

("Arrays","Base","find","find(A)

   Return a vector of the linear indexes of the non-zeros in \"A\".

"),

("Arrays","Base","findn","findn(A)

   Return a vector of indexes for each dimension giving the locations
   of the non-zeros in \"A\".

"),

("Arrays","Base","nonzeros","nonzeros(A)

   Return a vector of the non-zero values in array \"A\".

"),

("Arrays","Base","findfirst","findfirst(A)

   Return the index of the first non-zero value in \"A\".

"),

("Arrays","Base","findfirst","findfirst(A, v)

   Return the index of the first element equal to \"v\" in \"A\".

"),

("Arrays","Base","findfirst","findfirst(predicate, A)

   Return the index of the first element that satisfies the given
   predicate in \"A\".

"),

("Arrays","Base","permutedims","permutedims(A, perm)

   Permute the dimensions of array \"A\". \"perm\" is a vector
   specifying a permutation of length \"ndims(A)\". This is a
   generalization of transpose for multi-dimensional arrays. Transpose
   is equivalent to \"permute(A,[2,1])\".

"),

("Arrays","Base","ipermutedims","ipermutedims(A, perm)

   Like \"permutedims()\", except the inverse of the given permutation
   is applied.

"),

("Arrays","Base","squeeze","squeeze(A, dims)

   Remove the dimensions specified by \"dims\" from array \"A\"

"),

("Arrays","Base","vec","vec(Array) -> Vector

   Vectorize an array using column-major convention.

"),

("Arrays","Base","cumprod","cumprod(A[, dim])

   Cumulative product along a dimension.

"),

("Arrays","Base","cumsum","cumsum(A[, dim])

   Cumulative sum along a dimension.

"),

("Arrays","Base","cumsum_kbn","cumsum_kbn(A[, dim])

   Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier
   compensated summation algorithm for additional accuracy.

"),

("Arrays","Base","cummin","cummin(A[, dim])

   Cumulative minimum along a dimension.

"),

("Arrays","Base","cummax","cummax(A[, dim])

   Cumulative maximum along a dimension.

"),

("Arrays","Base","diff","diff(A[, dim])

   Finite difference operator of matrix or vector.

"),

("Arrays","Base","rot180","rot180(A)

   Rotate matrix \"A\" 180 degrees.

"),

("Arrays","Base","rotl90","rotl90(A)

   Rotate matrix \"A\" left 90 degrees.

"),

("Arrays","Base","rotr90","rotr90(A)

   Rotate matrix \"A\" right 90 degrees.

"),

("Arrays","Base","reducedim","reducedim(f, A, dims, initial)

   Reduce 2-argument function \"f\" along dimensions of \"A\".
   \"dims\" is a vector specifying the dimensions to reduce, and
   \"initial\" is the initial value to use in the reductions.

"),

("Arrays","Base","sum_kbn","sum_kbn(A)

   Returns the sum of all array elements, using the Kahan-Babuska-
   Neumaier compensated summation algorithm for additional accuracy.

"),

("Combinatorics","Base","nthperm","nthperm(v, k)

   Compute the kth lexicographic permutation of a vector.

"),

("Combinatorics","Base","nthperm!","nthperm!(v, k)

   In-place version of \"nthperm()\".

"),

("Combinatorics","Base","randperm","randperm(n)

   Construct a random permutation of the given length.

"),

("Combinatorics","Base","invperm","invperm(v)

   Return the inverse permutation of v.

"),

("Combinatorics","Base","isperm","isperm(v) -> Bool

   Returns true if v is a valid permutation.

"),

("Combinatorics","Base","permute!","permute!(v, p)

   Permute vector \"v\" in-place, according to permutation \"p\".  No
   checking is done to verify that \"p\" is a permutation.

   To return a new permutation, use \"v[p]\".  Note that this is
   generally faster than \"permute!(v,p)\" for large vectors.

"),

("Combinatorics","Base","ipermute!","ipermute!(v, p)

   Like permute!, but the inverse of the given permutation is applied.

"),

("Combinatorics","Base","randcycle","randcycle(n)

   Construct a random cyclic permutation of the given length.

"),

("Combinatorics","Base","shuffle","shuffle(v)

   Randomly rearrange the elements of a vector.

"),

("Combinatorics","Base","shuffle!","shuffle!(v)

   In-place version of \"shuffle()\".

"),

("Combinatorics","Base","reverse","reverse(v)

   Reverse vector \"v\".

"),

("Combinatorics","Base","reverse!","reverse!(v) -> v

   In-place version of \"reverse()\".

"),

("Combinatorics","Base","combinations","combinations(array, n)

   Generate all combinations of \"n\" elements from a given array.
   Because the number of combinations can be very large, this function
   runs inside a Task to produce values on demand. Write \"c = @task
   combinations(a,n)\", then iterate \"c\" or call \"consume\" on it.

"),

("Combinatorics","Base","integer_partitions","integer_partitions(n, m)

   Generate all arrays of \"m\" integers that sum to \"n\". Because
   the number of partitions can be very large, this function runs
   inside a Task to produce values on demand. Write \"c = @task
   integer_partitions(n,m)\", then iterate \"c\" or call \"consume\"
   on it.

"),

("Combinatorics","Base","partitions","partitions(array)

   Generate all set partitions of the elements of an array,
   represented as arrays of arrays. Because the number of partitions
   can be very large, this function runs inside a Task to produce
   values on demand. Write \"c = @task partitions(a)\", then iterate
   \"c\" or call \"consume\" on it.

"),

("Statistics","Base","mean","mean(v[, region])

   Compute the mean of whole array \"v\", or optionally along the
   dimensions in \"region\".

"),

("Statistics","Base","std","std(v[, region])

   Compute the sample standard deviation of a vector or array``v``,
   optionally along dimensions in \"region\". The algorithm returns an
   estimator of the generative distribution's standard deviation under
   the assumption that each entry of \"v\" is an IID draw from that
   generative distribution. This computation is equivalent to
   calculating \"sqrt(sum((v - mean(v)).^2) / (length(v) - 1))\".

"),

("Statistics","Base","stdm","stdm(v, m)

   Compute the sample standard deviation of a vector \"v\" with known
   mean \"m\".

"),

("Statistics","Base","var","var(v[, region])

   Compute the sample variance of a vector or array``v``, optionally
   along dimensions in \"region\". The algorithm will return an
   estimator of the generative distribution's variance under the
   assumption that each entry of \"v\" is an IID draw from that
   generative distribution. This computation is equivalent to
   calculating \"sum((v - mean(v)).^2) / (length(v) - 1)\".

"),

("Statistics","Base","varm","varm(v, m)

   Compute the sample variance of a vector \"v\" with known mean
   \"m\".

"),

("Statistics","Base","median","median(v)

   Compute the median of a vector \"v\".

"),

("Statistics","Base","hist","hist(v[, n])

   Compute the histogram of \"v\", optionally using \"n\" bins.

"),

("Statistics","Base","hist","hist(v, e)

   Compute the histogram of \"v\" using a vector \"e\" as the edges
   for the bins.

"),

("Statistics","Base","quantile","quantile(v, p)

   Compute the quantiles of a vector \"v\" at a specified set of
   probability values \"p\".

"),

("Statistics","Base","quantile","quantile(v)

   Compute the quantiles of a vector \"v\" at the probability values
   \"[.0, .2, .4, .6, .8, 1.0]\".

"),

("Statistics","Base","cov","cov(v1[, v2])

   Compute the Pearson covariance between two vectors \"v1\" and
   \"v2\". If called with a single element \"v\", then computes
   covariance of columns of \"v\".

"),

("Statistics","Base","cor","cor(v1[, v2])

   Compute the Pearson correlation between two vectors \"v1\" and
   \"v2\". If called with a single element \"v\", then computes
   correlation of columns of \"v\".

"),

("Signal Processing","Base","fft","fft(A[, dims])

   Performs a multidimensional FFT of the array \"A\".  The optional
   \"dims\" argument specifies an iterable subset of dimensions (e.g.
   an integer, range, tuple, or array) to transform along.  Most
   efficient if the size of \"A\" along the transformed dimensions is
   a product of small primes; see \"nextprod()\".  See also
   \"plan_fft()\" for even greater efficiency.

   A one-dimensional FFT computes the one-dimensional discrete Fourier
   transform (DFT) as defined by \\operatorname{DFT}[k] =
   \\sum_{n=1}^{\\operatorname{length}(A)} \\exp\\left(-i\\frac{2\\pi
   (n-1)(k-1)}{\\operatorname{length}(A)} \\right) A[n].  A
   multidimensional FFT simply performs this operation along each
   transformed dimension of \"A\".

"),

("Signal Processing","Base","fft!","fft!(A[, dims])

   Same as \"fft()\", but operates in-place on \"A\", which must be an
   array of complex floating-point numbers.

"),

("Signal Processing","Base","ifft","ifft(A[, dims])

   Multidimensional inverse FFT.

   A one-dimensional backward FFT computes \\operatorname{BDFT}[k] =
   \\sum_{n=1}^{\\operatorname{length}(A)} \\exp\\left(+i\\frac{2\\pi
   (n-1)(k-1)}{\\operatorname{length}(A)} \\right) A[n].  A
   multidimensional backward FFT simply performs this operation along
   each transformed dimension of \"A\".  The inverse FFT computes the
   same thing divided by the product of the transformed dimensions.

"),

("Signal Processing","Base","ifft!","ifft!(A[, dims])

   Same as \"ifft()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","bfft","bfft(A[, dims])

   Similar to \"ifft()\", but computes an unnormalized inverse
   (backward) transform, which must be divided by the product of the
   sizes of the transformed dimensions in order to obtain the inverse.
   (This is slightly more efficient than \"ifft()\" because it omits a
   scaling step, which in some applications can be combined with other
   computational steps elsewhere.)

"),

("Signal Processing","Base","bfft!","bfft!(A[, dims])

   Same as \"bfft()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","plan_fft","plan_fft(A[, dims[, flags[, timelimit]]])

   Pre-plan an optimized FFT along given dimensions (\"dims\") of
   arrays matching the shape and type of \"A\".  (The first two
   arguments have the same meaning as for \"fft()\".)  Returns a
   function \"plan(A)\" that computes \"fft(A, dims)\" quickly.

   The \"flags\" argument is a bitwise-or of FFTW planner flags,
   defaulting to \"FFTW.ESTIMATE\".  e.g. passing \"FFTW.MEASURE\" or
   \"FFTW.PATIENT\" will instead spend several seconds (or more)
   benchmarking different possible FFT algorithms and picking the
   fastest one; see the FFTW manual for more information on planner
   flags.  The optional \"timelimit\" argument specifies a rough upper
   bound on the allowed planning time, in seconds. Passing
   \"FFTW.MEASURE\" or \"FFTW.PATIENT\" may cause the input array
   \"A\" to be overwritten with zeros during plan creation.

   \"plan_fft!()\" is the same as \"plan_fft()\" but creates a plan
   that operates in-place on its argument (which must be an array of
   complex floating-point numbers).  \"plan_ifft()\" and so on are
   similar but produce plans that perform the equivalent of the
   inverse transforms \"ifft()\" and so on.

"),

("Signal Processing","Base","plan_ifft","plan_ifft(A[, dims[, flags[, timelimit]]])

   Same as \"plan_fft()\", but produces a plan that performs inverse
   transforms \"ifft()\".

"),

("Signal Processing","Base","plan_bfft","plan_bfft(A[, dims[, flags[, timelimit]]])

   Same as \"plan_fft()\", but produces a plan that performs an
   unnormalized backwards transform \"bfft()\".

"),

("Signal Processing","Base","plan_fft!","plan_fft!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_fft()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","plan_ifft!","plan_ifft!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_ifft()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","plan_bfft!","plan_bfft!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_bfft()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","rfft","rfft(A[, dims])

   Multidimensional FFT of a real array A, exploiting the fact that
   the transform has conjugate symmetry in order to save roughly half
   the computational time and storage costs compared with \"fft()\".
   If \"A\" has size \"(n_1, ..., n_d)\", the result has size
   \"(floor(n_1/2)+1, ..., n_d)\".

   The optional \"dims\" argument specifies an iterable subset of one
   or more dimensions of \"A\" to transform, similar to \"fft()\".
   Instead of (roughly) halving the first dimension of \"A\" in the
   result, the \"dims[1]\" dimension is (roughly) halved in the same
   way.

"),

("Signal Processing","Base","irfft","irfft(A, d[, dims])

   Inverse of \"rfft()\": for a complex array \"A\", gives the
   corresponding real array whose FFT yields \"A\" in the first half.
   As for \"rfft()\", \"dims\" is an optional subset of dimensions to
   transform, defaulting to \"1:ndims(A)\".

   \"d\" is the length of the transformed real array along the
   \"dims[1]\" dimension, which must satisfy \"d ==
   floor(size(A,dims[1])/2)+1\". (This parameter cannot be inferred
   from \"size(A)\" due to the possibility of rounding by the
   \"floor\" function here.)

"),

("Signal Processing","Base","brfft","brfft(A, d[, dims])

   Similar to \"irfft()\" but computes an unnormalized inverse
   transform (similar to \"bfft()\"), which must be divided by the
   product of the sizes of the transformed dimensions (of the real
   output array) in order to obtain the inverse transform.

"),

("Signal Processing","Base","plan_rfft","plan_rfft(A[, dims[, flags[, timelimit]]])

   Pre-plan an optimized real-input FFT, similar to \"plan_fft()\"
   except for \"rfft()\" instead of \"fft()\".  The first two
   arguments, and the size of the transformed result, are the same as
   for \"rfft()\".

"),

("Signal Processing","Base","plan_irfft","plan_irfft(A, d[, dims[, flags[, timelimit]]])

   Pre-plan an optimized inverse real-input FFT, similar to
   \"plan_rfft()\" except for \"irfft()\" and \"brfft()\",
   respectively.  The first three arguments have the same meaning as
   for \"irfft()\".

"),

("Signal Processing","Base","dct","dct(A[, dims])

   Performs a multidimensional type-II discrete cosine transform (DCT)
   of the array \"A\", using the unitary normalization of the DCT. The
   optional \"dims\" argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of \"A\" along the transformed
   dimensions is a product of small primes; see \"nextprod()\".  See
   also \"plan_dct()\" for even greater efficiency.

"),

("Signal Processing","Base","dct!","dct!(A[, dims])

   Same as \"dct!()\", except that it operates in-place on \"A\",
   which must be an array of real or complex floating-point values.

"),

("Signal Processing","Base","idct","idct(A[, dims])

   Computes the multidimensional inverse discrete cosine transform
   (DCT) of the array \"A\" (technically, a type-III DCT with the
   unitary normalization). The optional \"dims\" argument specifies an
   iterable subset of dimensions (e.g. an integer, range, tuple, or
   array) to transform along.  Most efficient if the size of \"A\"
   along the transformed dimensions is a product of small primes; see
   \"nextprod()\".  See also \"plan_idct()\" for even greater
   efficiency.

"),

("Signal Processing","Base","idct!","idct!(A[, dims])

   Same as \"idct!()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","plan_dct","plan_dct(A[, dims[, flags[, timelimit]]])

   Pre-plan an optimized discrete cosine transform (DCT), similar to
   \"plan_fft()\" except producing a function that computes \"dct()\".
   The first two arguments have the same meaning as for \"dct()\".

"),

("Signal Processing","Base","plan_dct!","plan_dct!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_dct()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","plan_idct","plan_idct(A[, dims[, flags[, timelimit]]])

   Pre-plan an optimized inverse discrete cosine transform (DCT),
   similar to \"plan_fft()\" except producing a function that computes
   \"idct()\". The first two arguments have the same meaning as for
   \"idct()\".

"),

("Signal Processing","Base","plan_idct!","plan_idct!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_idct()\", but operates in-place on \"A\".

"),

("Signal Processing","Base","FFTW","FFTW.r2r(A, kind[, dims])

   Performs a multidimensional real-input/real-output (r2r) transform
   of type \"kind\" of the array \"A\", as defined in the FFTW manual.
   \"kind\" specifies either a discrete cosine transform of various
   types (\"FFTW.REDFT00\", \"FFTW.REDFT01\", \"FFTW.REDFT10\", or
   \"FFTW.REDFT11\"), a discrete sine transform of various types
   (\"FFTW.RODFT00\", \"FFTW.RODFT01\", \"FFTW.RODFT10\", or
   \"FFTW.RODFT11\"), a real-input DFT with halfcomplex-format output
   (\"FFTW.R2HC\" and its inverse \"FFTW.HC2R\"), or a discrete
   Hartley transform (\"FFTW.DHT\").  The \"kind\" argument may be an
   array or tuple in order to specify different transform types along
   the different dimensions of \"A\"; \"kind[end]\" is used for any
   unspecified dimensions.  See the FFTW manual for precise
   definitions of these transform types, at
   *<http://www.fftw.org/doc>*.

   The optional \"dims\" argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along. \"kind[i]\" is then the transform type for \"dims[i]\", with
   \"kind[end]\" being used for \"i > length(kind)\".

   See also \"FFTW.plan_r2r()\" to pre-plan optimized r2r transforms.

"),

("Signal Processing","Base","FFTW","FFTW.r2r!(A, kind[, dims])

   \"FFTW.r2r!()\" is the same as \"FFTW.r2r()\", but operates in-
   place on \"A\", which must be an array of real or complex floating-
   point numbers.

"),

("Signal Processing","Base","FFTW","FFTW.plan_r2r(A, kind[, dims[, flags[, timelimit]]])

   Pre-plan an optimized r2r transform, similar to \"plan_fft()\"
   except that the transforms (and the first three arguments)
   correspond to \"FFTW.r2r()\" and \"FFTW.r2r!()\", respectively.

"),

("Signal Processing","Base","FFTW","FFTW.plan_r2r!(A, kind[, dims[, flags[, timelimit]]])

   Similar to \"plan_fft()\", but corresponds to \"FFTW.r2r!()\".

"),

("Signal Processing","Base","fftshift","fftshift(x)

   Swap the first and second halves of each dimension of \"x\".

"),

("Signal Processing","Base","fftshift","fftshift(x, dim)

   Swap the first and second halves of the given dimension of array
   \"x\".

"),

("Signal Processing","Base","ifftshift","ifftshift(x[, dim])

   Undoes the effect of \"fftshift\".

"),

("Signal Processing","Base","filt","filt(b, a, x)

   Apply filter described by vectors \"a\" and \"b\" to vector \"x\".

"),

("Signal Processing","Base","deconv","deconv(b, a)

   Construct vector \"c\" such that \"b = conv(a,c) + r\". Equivalent
   to polynomial division.

"),

("Signal Processing","Base","conv","conv(u, v)

   Convolution of two vectors. Uses FFT algorithm.

"),

("Signal Processing","Base","xcorr","xcorr(u, v)

   Compute the cross-correlation of two vectors.

"),

("Parallel Computing","Base","addprocs_local","addprocs_local(n)

   Add processes on the local machine. Can be used to take advantage
   of multiple cores.

"),

("Parallel Computing","Base","addprocs_ssh","addprocs_ssh({\"host1\", \"host2\", ...})

   Add processes on remote machines via SSH. Requires julia to be
   installed in the same location on each node, or to be available via
   a shared file system.

"),

("Parallel Computing","Base","addprocs_sge","addprocs_sge(n)

   Add processes via the Sun/Oracle Grid Engine batch queue, using
   \"qsub\".

"),

("Parallel Computing","Base","nprocs","nprocs()

   Get the number of available processors.

"),

("Parallel Computing","Base","myid","myid()

   Get the id of the current processor.

"),

("Parallel Computing","Base","pmap","pmap(f, c)

   Transform collection \"c\" by applying \"f\" to each element in
   parallel.

"),

("Parallel Computing","Base","remote_call","remote_call(id, func, args...)

   Call a function asynchronously on the given arguments on the
   specified processor. Returns a \"RemoteRef\".

"),

("Parallel Computing","Base","wait","wait(RemoteRef)

   Wait for a value to become available for the specified remote
   reference.

"),

("Parallel Computing","Base","fetch","fetch(RemoteRef)

   Wait for and get the value of a remote reference.

"),

("Parallel Computing","Base","remote_call_wait","remote_call_wait(id, func, args...)

   Perform \"wait(remote_call(...))\" in one message.

"),

("Parallel Computing","Base","remote_call_fetch","remote_call_fetch(id, func, args...)

   Perform \"fetch(remote_call(...))\" in one message.

"),

("Parallel Computing","Base","put","put(RemoteRef, value)

   Store a value to a remote reference. Implements \"shared queue of
   length 1\" semantics: if a value is already present, blocks until
   the value is removed with \"take\".

"),

("Parallel Computing","Base","take","take(RemoteRef)

   Fetch the value of a remote reference, removing it so that the
   reference is empty again.

"),

("Parallel Computing","Base","RemoteRef","RemoteRef()

   Make an uninitialized remote reference on the local machine.

"),

("Parallel Computing","Base","RemoteRef","RemoteRef(n)

   Make an uninitialized remote reference on processor \"n\".

"),

("Distributed Arrays","Base","DArray","DArray(init, dims[, procs, dist])

   Construct a distributed array. \"init\" is a function accepting a
   tuple of index ranges. This function should return a chunk of the
   distributed array for the specified indexes. \"dims\" is the
   overall size of the distributed array. \"procs\" optionally
   specifies a vector of processor IDs to use. \"dist\" is an integer
   vector specifying how many chunks the distributed array should be
   divided into in each dimension.

"),

("Distributed Arrays","Base","dzeros","dzeros(dims, ...)

   Construct a distributed array of zeros. Trailing arguments are the
   same as those accepted by \"darray\".

"),

("Distributed Arrays","Base","dones","dones(dims, ...)

   Construct a distributed array of ones. Trailing arguments are the
   same as those accepted by \"darray\".

"),

("Distributed Arrays","Base","dfill","dfill(x, dims, ...)

   Construct a distributed array filled with value \"x\". Trailing
   arguments are the same as those accepted by \"darray\".

"),

("Distributed Arrays","Base","drand","drand(dims, ...)

   Construct a distributed uniform random array. Trailing arguments
   are the same as those accepted by \"darray\".

"),

("Distributed Arrays","Base","drandn","drandn(dims, ...)

   Construct a distributed normal random array. Trailing arguments are
   the same as those accepted by \"darray\".

"),

("Distributed Arrays","Base","distribute","distribute(a)

   Convert a local array to distributed

"),

("Distributed Arrays","Base","localize","localize(d)

   Get the local piece of a distributed array

"),

("Distributed Arrays","Base","myindexes","myindexes(d)

   A tuple describing the indexes owned by the local processor

"),

("Distributed Arrays","Base","procs","procs(d)

   Get the vector of processors storing pieces of \"d\"

"),

("System","Base","run","run(command)

   Run a command object, constructed with backticks. Throws an error
   if anything goes wrong, including the process exiting with a non-
   zero status.

"),

("System","Base","spawn","spawn(command)

   Run a command object asynchronously, returning the resulting
   \"Process\" object.

"),

("System","Base","success","success(command)

   Run a command object, constructed with backticks, and tell whether
   it was successful (exited with a code of 0).

"),

("System","Base","readsfrom","readsfrom(command)

   Starts running a command asynchronously, and returns a tuple
   (stream,process). The first value is a stream reading from the
   process' standard output.

"),

("System","Base","writesto","writesto(command)

   Starts running a command asynchronously, and returns a tuple
   (stream,process). The first value is a stream writing to the
   process' standard input.

"),

("System","Base","readandwrite","readandwrite(command)

   Starts running a command asynchronously, and returns a tuple
   (stdout,stdin,process) of the output stream and input stream of the
   process, and the process object itself.

"),

("System","Base",">",">()

   Redirect standard output of a process.

   **Example**: \"run(`ls` > \"out.log\")\"

"),

("System","Base","<","<()

   Redirect standard input of a process.

"),

("System","Base",">>",">>()

   Redirect standard output of a process, appending to the destination
   file.

"),

("System","Base",".>",".>()

   Redirect the standard error stream of a process.

"),

("System","Base","gethostname","gethostname() -> String

   Get the local machine's host name.

"),

("System","Base","getipaddr","getipaddr() -> String

   Get the IP address of the local machine, as a string of the form
   \"x.x.x.x\".

"),

("System","Base","pwd","pwd() -> String

   Get the current working directory.

"),

("System","Base","cd","cd(dir::String)

   Set the current working directory. Returns the new current
   directory.

"),

("System","Base","cd","cd(f[, \"dir\"])

   Temporarily changes the current working directory (HOME if not
   specified) and applies function f before returning.

"),

("System","Base","mkdir","mkdir(path[, mode])

   Make a new directory with name \"path\" and permissions \"mode\".
   \"mode\" defaults to 0o777, modified by the current file creation
   mask.

"),

("System","Base","rmdir","rmdir(path)

   Remove the directory named \"path\".

"),

("System","Base","getpid","getpid() -> Int32

   Get julia's process ID.

"),

("System","Base","time","time()

   Get the system time in seconds since the epoch, with fairly high
   (typically, microsecond) resolution.

"),

("System","Base","time_ns","time_ns()

   Get the time in nanoseconds. The time corresponding to 0 is
   undefined, and wraps every 5.8 years.

"),

("System","Base","tic","tic()

   Set a timer to be read by the next call to \"toc()\" or \"toq()\".
   The macro call \"@time expr\" can also be used to time evaluation.

"),

("System","Base","toc","toc()

   Print and return the time elapsed since the last \"tic()\".

"),

("System","Base","toq","toq()

   Return, but do not print, the time elapsed since the last
   \"tic()\".

"),

("System","Base","EnvHash","EnvHash() -> EnvHash

   A singleton of this type provides a hash table interface to
   environment variables.

"),

("System","Base","ENV","ENV

   Reference to the singleton \"EnvHash\", providing a dictionary
   interface to system environment variables.

"),

("C Interface","Base","ccall","ccall((symbol, library) or fptr, RetType, (ArgType1, ...), ArgVar1, ...)

   Call function in C-exported shared library, specified by (function
   name, library) tuple (String or :Symbol). Alternatively, ccall may
   be used to call a function pointer returned by dlsym, but note that
   this usage is generally discouraged to facilitate future static
   compilation.

"),

("C Interface","Base","cfunction","cfunction(fun::Function, RetType::Type, (ArgTypes...))

   Generate C-callable function pointer from Julia function.

"),

("C Interface","Base","dlopen","dlopen(libfile::String[, flags::Integer])

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

"),

("C Interface","Base","dlsym","dlsym(handle, sym)

   Look up a symbol from a shared library handle, return callable
   function pointer on success.

"),

("C Interface","Base","dlsym_e","dlsym_e(handle, sym)

   Look up a symbol from a shared library handle, silently return NULL
   pointer on lookup failure.

"),

("C Interface","Base","dlclose","dlclose(handle)

   Close shared library referenced by handle.

"),

("C Interface","Base","c_free","c_free(addr::Ptr)

   Call free() from C standard library.

"),

("C Interface","Base","unsafe_ref","unsafe_ref(p::Ptr{T}, i::Integer)

   Dereference the pointer \"p[i]\" or \"*p\", returning a copy of
   type T.

"),

("C Interface","Base","unsafe_assign","unsafe_assign(p::Ptr{T}, x, i::Integer)

   Assign to the pointer \"p[i] = x\" or \"*p = x\", making a copy of
   object x into the memory at p.

"),

("C Interface","Base","pointer","pointer(a[, index])

   Get the native address of an array element. Be careful to ensure
   that a julia reference to \"a\" exists as long as this pointer will
   be used.

"),

("C Interface","Base","pointer","pointer(type, int)

   Convert an integer to a pointer of the specified element type.

"),

("C Interface","Base","pointer_to_array","pointer_to_array(p, dims[, own])

   Wrap a native pointer as a Julia Array object. The pointer element
   type determines the array element type. \"own\" optionally
   specifies whether Julia should take ownership of the memory,
   calling \"free\" on the pointer when the array is no longer
   referenced.

"),

("Errors","Base","error","error(message::String)

   Raise an error with the given message

"),

("Errors","Base","throw","throw(e)

   Throw an object as an exception

"),

("Errors","Base","errno","errno()

   Get the value of the C library's \"errno\"

"),

("Errors","Base","strerror","strerror(n)

   Convert a system call error code to a descriptive string

"),

("Errors","Base","assert","assert(cond)

   Raise an error if \"cond\" is false. Also available as the macro
   \"@assert expr\".

"),

("Tasks","Base","Task","Task(func)

   Create a \"Task\" (i.e. thread, or coroutine) to execute the given
   function. The task exits when this function returns.

"),

("Tasks","Base","yieldto","yieldto(task, args...)

   Switch to the given task. The first time a task is switched to, the
   task's function is called with \"args\". On subsequent switches,
   \"args\" are returned from the task's last call to \"yieldto\".

"),

("Tasks","Base","current_task","current_task()

   Get the currently running Task.

"),

("Tasks","Base","istaskdone","istaskdone(task)

   Tell whether a task has exited.

"),

("Tasks","Base","consume","consume(task)

   Receive the next value passed to \"produce\" by the specified task.

"),

("Tasks","Base","produce","produce(value)

   Send the given value to the last \"consume\" call, switching to the
   consumer task.

"),

("Tasks","Base","make_scheduled","make_scheduled(task)

   Register a task with the main event loop, so it will automatically
   run when possible.

"),

("Tasks","Base","yield","yield()

   For scheduled tasks, switch back to the scheduler to allow another
   scheduled task to run.

"),

("Tasks","Base","tls","tls(symbol)

   Look up the value of a symbol in the current task's task-local
   storage.

"),

("Tasks","Base","tls","tls(symbol, value)

   Assign a value to a symbol in the current task's task-local
   storage.

"),

("Constants","Base","OS_NAME","OS_NAME

   A symbol representing the name of the operating system. Possible
   values are \":Linux\", \":Darwin\" (OS X), or \":Windows\".

"),

("Constants","Base","ARGS","ARGS

   An array of the command line arguments passed to Julia, as strings.

"),

("Constants","Base","C_NULL","C_NULL

   The C null pointer constant, sometimes used when calling external
   code.

"),

("Constants","Base","CPU_CORES","CPU_CORES

   The number of CPU cores in the system.

"),

("Constants","Base","WORD_SIZE","WORD_SIZE

   Standard word size on the current machine, in bits.

"),

("Constants","Base","VERSION","VERSION

   An object describing which version of Julia is in use.

"),

("Constants","Base","LOAD_PATH","LOAD_PATH

   An array of paths (as strings) where the \"require\" function looks
   for code.

"),

("Filesystem","Base","isblockdev","isblockdev(path) -> Bool

   Returns \"true\" if \"path\" is a block device, \"false\"
   otherwise.

"),

("Filesystem","Base","ischardev","ischardev(path) -> Bool

   Returns \"true\" if \"path\" is a character device, \"false\"
   otherwise.

"),

("Filesystem","Base","isdir","isdir(path) -> Bool

   Returns \"true\" if \"path\" is a directory, \"false\" otherwise.

"),

("Filesystem","Base","isexecutable","isexecutable(path) -> Bool

   Returns \"true\" if the current user has permission to execute
   \"path\", \"false\" otherwise.

"),

("Filesystem","Base","isfifo","isfifo(path) -> Bool

   Returns \"true\" if \"path\" is a FIFO, \"false\" otherwise.

"),

("Filesystem","Base","isfile","isfile(path) -> Bool

   Returns \"true\" if \"path\" is a regular file, \"false\"
   otherwise.

"),

("Filesystem","Base","islink","islink(path) -> Bool

   Returns \"true\" if \"path\" is a symbolic link, \"false\"
   otherwise.

"),

("Filesystem","Base","ispath","ispath(path) -> Bool

   Returns \"true\" if \"path\" is a valid filesystem path, \"false\"
   otherwise.

"),

("Filesystem","Base","isreadable","isreadable(path) -> Bool

   Returns \"true\" if the current user has permission to read
   \"path\", \"false\" otherwise.

"),

("Filesystem","Base","issetgid","issetgid(path) -> Bool

   Returns \"true\" if \"path\" has the setgid flag set, \"false\"
   otherwise.

"),

("Filesystem","Base","issetuid","issetuid(path) -> Bool

   Returns \"true\" if \"path\" has the setuid flag set, \"false\"
   otherwise.

"),

("Filesystem","Base","issocket","issocket(path) -> Bool

   Returns \"true\" if \"path\" is a socket, \"false\" otherwise.

"),

("Filesystem","Base","issticky","issticky(path) -> Bool

   Returns \"true\" if \"path\" has the sticky bit set, \"false\"
   otherwise.

"),

("Filesystem","Base","iswriteable","iswriteable(path) -> Bool

   Returns \"true\" if the current user has permission to write to
   \"path\", \"false\" otherwise.

"),


("Linear Algebra","","*","*(A, B)

   Matrix multiplication

"),

("Linear Algebra","","\\","\\(A, B)

   Matrix division using a polyalgorithm. For input matrices \"A\" and
   \"B\", the result \"X\" is such that \"A*X == B\" when \"A\" is
   square.  The solver that is used depends upon the structure of
   \"A\".  A direct solver is used for upper- or lower triangular
   \"A\".  For Hermitian \"A\" (equivalent to symmetric \"A\" for non-
   complex \"A\") the BunchKaufman factorization is used.  Otherwise
   an LU factorization is used. For rectangular \"A\" the result is
   the minimum-norm least squares solution computed by reducing \"A\"
   to bidiagonal form and solving the bidiagonal least squares
   problem.  For sparse, square \"A\" the LU factorization (from
   UMFPACK) is used.

"),

("Linear Algebra","","dot","dot(x, y)

   Compute the dot product

"),

("Linear Algebra","","cross","cross(x, y)

   Compute the cross product of two 3-vectors

"),

("Linear Algebra","","norm","norm(a)

   Compute the norm of a \"Vector\" or a \"Matrix\"

"),

("Linear Algebra","","lu","lu(A) -> L, U, P

   Compute the LU factorization of \"A\", such that \"P*A = L*U\".

"),

("Linear Algebra","","lufact","lufact(A) -> LUDense

   Compute the LU factorization of \"A\", returning an \"LUDense\"
   object for dense \"A\" or an \"UmfpackLU\" object for sparse \"A\".
   The individual components of the factorization \"F\" can be accesed
   by indexing: \"F[:L]\", \"F[:U]\", and \"F[:P]\" (permutation
   matrix) or \"F[:p]\" (permutation vector). An \"UmfpackLU\" object
   has additional components \"F[:q]\" (the left permutation vector)
   and \"Rs\" the vector of scaling factors. The following functions
   are available for both \"LUDense\" and \"UmfpackLU\" objects:
   \"size\", \"\\\" and \"det\".  For \"LUDense\" there is also an
   \"inv\" method.  The sparse LU factorization is such that \"L*U\"
   is equal to``diagmm(Rs,A)[p,q]``.

"),

("Linear Algebra","","lufact!","lufact!(A) -> LUDense

   \"lufact!\" is the same as \"lufact\" but saves space by
   overwriting the input A, instead of creating a copy.  For sparse
   \"A\" the \"nzval\" field is not overwritten but the index fields,
   \"colptr\" and \"rowval\" are decremented in place, converting from
   1-based indices to 0-based indices.

"),

("Linear Algebra","","chol","chol(A[, LU]) -> F

   Compute Cholesky factorization of a symmetric positive-definite
   matrix \"A\" and return the matrix \"F\". If \"LU\" is \"L\"
   (Lower), \"A = L*L'\". If \"LU\" is \"U\" (Upper), \"A = R'*R\".

"),

("Linear Algebra","","cholfact","cholfact(A[, LU]) -> CholeskyDense

   Compute the Cholesky factorization of a dense symmetric positive-
   definite matrix \"A\" and return a \"CholeskyDense\" object. \"LU\"
   may be 'L' for using the lower part or 'U' for the upper part. The
   default is to use 'U'. The triangular matrix can be obtained from
   the factorization \"F\" with: \"F[:L]\" and \"F[:U]\". The
   following functions are available for \"CholeskyDense\" objects:
   \"size\", \"\\\", \"inv\", \"det\". A \"LAPACK.PosDefException\"
   error is thrown in case the matrix is not positive definite.

"),

("Linear Algebra","","cholfact","cholfact(A[, ll]) -> CholmodFactor

   Compute the sparse Cholesky factorization of a sparse matrix \"A\".
   If \"A\" is Hermitian its Cholesky factor is determined.  If \"A\"
   is not Hermitian the Cholesky factor of \"A*A'\" is determined. A
   fill-reducing permutation is used.  Methods for \"size\",
   \"solve\", \"\\\", \"findn_nzs\", \"diag\", \"det\" and \"logdet\".
   One of the solve methods includes an integer argument that can be
   used to solve systems involving parts of the factorization only.
   The optional boolean argument, \"ll\" determines whether the
   factorization returned is of the \"A[p,p] = L*L'\" form, where
   \"L\" is lower triangular or \"A[p,p] = diagmm(L,D)*L'\" form where
   \"L\" is unit lower triangular and \"D\" is a non-negative vector.
   The default is LDL.

"),

("Linear Algebra","","cholpfact","cholpfact(A[, LU]) -> CholeskyPivotedDense

   Compute the pivoted Cholesky factorization of a symmetric positive
   semi-definite matrix \"A\" and return a \"CholeskyDensePivoted\"
   object. \"LU\" may be 'L' for using the lower part or 'U' for the
   upper part. The default is to use 'U'. The triangular factors
   containted in the factorization \"F\" can be obtained with
   \"F[:L]\" and \"F[:U]\", whereas the permutation can be obtained
   with \"F[:P]\" or \"F[:p]\". The following functions are available
   for \"CholeskyDensePivoted\" objects: \"size\", \"\\\", \"inv\",
   \"det\". A \"LAPACK.RankDeficientException\" error is thrown in
   case the matrix is rank deficient.

"),

("Linear Algebra","","cholpfact!","cholpfact!(A[, LU]) -> CholeskyPivotedDense

   \"cholpfact!\" is the same as \"cholpfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","","qr","qr(A) -> Q, R

   Compute the QR factorization of \"A\" such that \"A = Q*R\". Also
   see \"qrfact\".

"),

("Linear Algebra","","qrfact","qrfact(A)

   Compute the QR factorization of \"A\" and return a \"QRDense\"
   object. The coomponents of the factorization \"F\" can be accessed
   as follows: the orthogonal matrix \"Q\" can be extracted with
   \"F[:Q]\" and the triangular matrix \"R\" with \"F[:R]\". The
   following functions are available for \"QRDense\" objects:
   \"size\", \"\\\". When \"Q\" is extracted, the resulting type is
   the \"QRDenseQ\" object, and has the \"*\" operator overloaded to
   support efficient multiplication by \"Q\" and \"Q'\".

"),

("Linear Algebra","","qrfact!","qrfact!(A)

   \"qrfact!\" is the same as \"qrfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","","qrp","qrp(A) -> Q, R, P

   Compute the QR factorization of \"A\" with pivoting, such that
   \"A*P = Q*R\", Also see \"qrpfact\".

"),

("Linear Algebra","","qrpfact","qrpfact(A) -> QRPivotedDense

   Compute the QR factorization of \"A\" with pivoting and return a
   \"QRDensePivoted\" object. The components of the factorization
   \"F\" can be accessed as follows: the orthogonal matrix \"Q\" can
   be extracted with \"F[:Q]\", the triangular matrix \"R\" with
   \"F[:R]\", and the permutation with \"F[:P]\" or \"F[:p]\". The
   following functions are available for \"QRDensePivoted\" objects:
   \"size\", \"\\\". When \"Q\" is extracted, the resulting type is
   the \"QRDenseQ\" object, and has the \"*\" operator overloaded to
   support efficient multiplication by \"Q\" and \"Q'\". A
   \"QRDenseQ\" matrix can be converted into a regular matrix with
   \"full\".

"),

("Linear Algebra","","qrpfact!","qrpfact!(A) -> QRPivotedDense

   \"qrpfact!\" is the same as \"qrpfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","","sqrtm","sqrtm(A)

   Compute the matrix square root of \"A\". If \"B = sqrtm(A)\", then
   \"B*B == A\" within roundoff error.

"),

("Linear Algebra","","eig","eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

"),

("Linear Algebra","","eigvals","eigvals(A)

   Returns the eigenvalues of \"A\".

"),

("Linear Algebra","","eigfact","eigfact(A)

   Compute the eigenvalue decomposition of \"A\" and return an
   \"EigenDense\" object. If \"F\" is the factorization object, the
   eigenvalues can be accessed with \"F[:values]\" and the
   eigenvectors with \"F[:vectors]\". The following functions are
   available for \"EigenDense\" objects: \"inv\", \"det\".

"),

("Linear Algebra","","eigfact!","eigfact!(A)

   \"eigfact!\" is the same as \"eigfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","","hessfact","hessfact(A)

   Compute the Hessenberg decomposition of \"A\" and return a
   \"HessenbergDense\" object. If \"F\" is the factorization object,
   the unitary matrix can be accessed with \"F[:Q]\" and the
   Hessenberg matrix with \"F[:H]\". When \"Q\" is extracted, the
   resulting type is the \"HessenbergDenseQ\" object, and may be
   converted to a regular matrix with \"full\".

"),

("Linear Algebra","","hessfact!","hessfact!(A)

   \"hessfact!\" is the same as \"hessfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","","svdfact","svdfact(A[, thin]) -> SVDDense

   Compute the Singular Value Decomposition (SVD) of \"A\" and return
   an \"SVDDense\" object. \"U\", \"S\", \"V\" and \"Vt\" can be
   obtained from the factorization \"F\" with \"F[:U]\", \"F[:S]\",
   \"F[:V]\" and \"F[:Vt]\", such that \"A = U*diagm(S)*Vt\". If
   \"thin\" is \"true\", an economy mode decomposition is returned.
   The algorithm produces \"Vt\" and hence \"Vt\" is more efficient to
   extract than \"V\".

"),

("Linear Algebra","","svdfact!","svdfact!(A[, thin]) -> SVDDense

   \"svdfact!\" is the same as \"svdfact\" but saves space by
   overwriting the input A, instead of creating a copy. If \"thin\" is
   \"true\", an economy mode decomposition is returned.

"),

("Linear Algebra","","svd","svd(A[, thin]) -> U, S, V

   Compute the SVD of A, returning \"U\", vector \"S\", and \"V\" such
   that \"A == U*diagm(S)*V'\". If \"thin\" is \"true\", an economy
   mode decomposition is returned.

"),

("Linear Algebra","","svdvals","svdvals(A)

   Returns the singular values of \"A\".

"),

("Linear Algebra","","svdvals!","svdvals!(A)

   Returns the singular values of \"A\", while saving space by
   overwriting the input.

"),

("Linear Algebra","","svdfact","svdfact(A, B) -> GSVDDense

   Compute the generalized SVD of \"A\" and \"B\", returning a
   \"GSVDDense\" Factorization object, such that \"A = U*D1*R0*Q'\"
   and \"B = V*D2*R0*Q'\".

"),

("Linear Algebra","","svd","svd(A, B) -> U, V, Q, D1, D2, R0

   Compute the generalized SVD of \"A\" and \"B\", returning \"U\",
   \"V\", \"Q\", \"D1\", \"D2\", and \"R0\" such that \"A =
   U*D1*R0*Q'\" and \"B = V*D2*R0*Q'\".

"),

("Linear Algebra","","svdvals","svdvals(A, B)

   Return only the singular values from the generalized singular value
   decomposition of \"A\" and \"B\".

"),

("Linear Algebra","","triu","triu(M)

   Upper triangle of a matrix

"),

("Linear Algebra","","tril","tril(M)

   Lower triangle of a matrix

"),

("Linear Algebra","","diag","diag(M[, k])

   The \"k\"-th diagonal of a matrix, as a vector

"),

("Linear Algebra","","diagm","diagm(v[, k])

   Construct a diagonal matrix and place \"v\" on the \"k\"-th
   diagonal

"),

("Linear Algebra","","diagmm","diagmm(matrix, vector)

   Multiply matrices, interpreting the vector argument as a diagonal
   matrix. The arguments may occur in the other order to multiply with
   the diagonal matrix on the left.

"),

("Linear Algebra","","Tridiagonal","Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal,
   and upper diagonal

"),

("Linear Algebra","","Woodbury","Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury
   matrix identity

"),

("Linear Algebra","","rank","rank(M)

   Compute the rank of a matrix

"),

("Linear Algebra","","norm","norm(A[, p])

   Compute the \"p\"-norm of a vector or a matrix. \"p\" is \"2\" by
   default, if not provided. If \"A\" is a vector, \"norm(A, p)\"
   computes the \"p\"-norm. \"norm(A, Inf)\" returns the largest value
   in \"abs(A)\", whereas \"norm(A, -Inf)\" returns the smallest. If
   \"A\" is a matrix, valid values for \"p\" are \"1\", \"2\", or
   \"Inf\". In order to compute the Frobenius norm, use \"normfro\".

"),

("Linear Algebra","","normfro","normfro(A)

   Compute the Frobenius norm of a matrix \"A\".

"),

("Linear Algebra","","cond","cond(M[, p])

   Matrix condition number, computed using the p-norm. \"p\" is 2 by
   default, if not provided. Valid values for \"p\" are \"1\", \"2\",
   or \"Inf\".

"),

("Linear Algebra","","trace","trace(M)

   Matrix trace

"),

("Linear Algebra","","det","det(M)

   Matrix determinant

"),

("Linear Algebra","","inv","inv(M)

   Matrix inverse

"),

("Linear Algebra","","pinv","pinv(M)

   Moore-Penrose inverse

"),

("Linear Algebra","","null","null(M)

   Basis for null space of M.

"),

("Linear Algebra","","repmat","repmat(A, n, m)

   Construct a matrix by repeating the given matrix \"n\" times in
   dimension 1 and \"m\" times in dimension 2.

"),

("Linear Algebra","","kron","kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

"),

("Linear Algebra","","linreg","linreg(x, y)

   Determine parameters \"[a, b]\" that minimize the squared error
   between \"y\" and \"a+b*x\".

"),

("Linear Algebra","","linreg","linreg(x, y, w)

   Weighted least-squares linear regression.

"),

("Linear Algebra","","expm","expm(A)

   Matrix exponential.

"),

("Linear Algebra","","issym","issym(A)

   Test whether a matrix is symmetric.

"),

("Linear Algebra","","isposdef","isposdef(A)

   Test whether a matrix is positive-definite.

"),

("Linear Algebra","","istril","istril(A)

   Test whether a matrix is lower-triangular.

"),

("Linear Algebra","","istriu","istriu(A)

   Test whether a matrix is upper-triangular.

"),

("Linear Algebra","","ishermitian","ishermitian(A)

   Test whether a matrix is hermitian.

"),

("Linear Algebra","","transpose","transpose(A)

   The transpose operator (.').

"),

("Linear Algebra","","ctranspose","ctranspose(A)

   The conjugate transpose operator (').

"),

("BLAS Functions","","copy!","copy!(n, X, incx, Y, incy)

   Copy \"n\" elements of array \"X\" with stride \"incx\" to array
   \"Y\" with stride \"incy\".  Returns \"Y\".

"),

("BLAS Functions","","dot","dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of \"n\" elements of array
   \"X\" with stride \"incx\" and \"n\" elements of array \"Y\" with
   stride \"incy\".  There are no \"dot\" methods for \"Complex\"
   arrays.

"),

("BLAS Functions","","nrm2","nrm2(n, X, incx)

   2-norm of a vector consisting of \"n\" elements of array \"X\" with
   stride \"incx\".

"),

("BLAS Functions","","axpy!","axpy!(n, a, X, incx, Y, incy)

   Overwrite \"Y\" with \"a*X + Y\".  Returns \"Y\".

"),

("BLAS Functions","","syrk!","syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix \"C\" as \"alpha*A*A.' +
   beta*C\" or \"alpha*A.'*A + beta*C\" according to whether \"trans\"
   is 'N' or 'T'.  When \"uplo\" is 'U' the upper triangle of \"C\" is
   updated ('L' for lower triangle).  Returns \"C\".

"),

("BLAS Functions","","syrk","syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according
   to \"uplo\" ('U' or 'L'), of \"alpha*A*A.'\" or \"alpha*A.'*A\",
   according to \"trans\" ('N' or 'T').

"),

("BLAS Functions","","herk!","herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian
   matrix \"C\" as \"alpha*A*A' + beta*C\" or \"alpha*A'*A + beta*C\"
   according to whether \"trans\" is 'N' or 'T'.  When \"uplo\" is 'U'
   the upper triangle of \"C\" is updated ('L' for lower triangle).
   Returns \"C\".

"),

("BLAS Functions","","herk","herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle
   or the lower triangle, according to \"uplo\" ('U' or 'L'), of
   \"alpha*A*A'\" or \"alpha*A'*A\", according to \"trans\" ('N' or
   'T').

"),

("BLAS Functions","","gbmv!","gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" or \"alpha*A'*x +
   beta*y\" according to \"trans\" ('N' or 'T').  The matrix \"A\" is
   a general band matrix of dimension \"m\" by \"size(A,2)\" with
   \"kl\" sub-diagonals and \"ku\" super-diagonals. Returns the
   updated \"y\".

"),

("BLAS Functions","","gbmv","gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns \"alpha*A*x\" or \"alpha*A'*x\" according to \"trans\" ('N'
   or 'T'). The matrix \"A\" is a general band matrix of dimension
   \"m\" by \"size(A,2)\" with \"kl\" sub-diagonals and \"ku\" super-
   diagonals.

"),

("BLAS Functions","","sbmv!","sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" where \"A\" is a a
   symmetric band matrix of order \"size(A,2)\" with \"k\" super-
   diagonals stored in the argument \"A\".  The storage layout for
   \"A\" is described the reference BLAS module, level-2 BLAS at
   *<http://www.netlib.org/lapack/explore-html/>*.

   Returns the updated \"y\".

"),

("BLAS Functions","","sbmv","sbmv(uplo, k, alpha, A, x)

   Returns \"alpha*A*x\" where \"A\" is a symmetric band matrix of
   order \"size(A,2)\" with \"k\" super-diagonals stored in the
   argument \"A\".

"),

("BLAS Functions","","gemm!","gemm!(tA, tB, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or the other three variants
   according to \"tA\" (transpose \"A\") and \"tB\".  Returns the
   updated \"C\".

"),

("BLAS Functions","","gemm","gemm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("Punctuation","","punctuation","punctuation

   +-----------+---------------------------------------------------------------------------------------------+
   | symbol    | meaning                                                                                     |
   +===========+=============================================================================================+
   | \\\"@m\\\"    | invoke macro m; followed by space-separated expressions                                     |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"!\\\"     | prefix \\\"not\\\" operator                                                                     |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"!\\\"     | at the end of a function name, indicates that a function modifies its argument(s)           |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"#\\\"     | begin single line comment                                                                   |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"\\\$\\\"    | xor operator, string and expression interpolation                                           |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"%\\\"     | remainder operator                                                                          |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"^\\\"     | exponent operator                                                                           |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"&\\\"     | bitwise and                                                                                 |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"*\\\"     | multiply, or matrix multiply                                                                |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"()\\\"    | the empty tuple                                                                             |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"~\\\"     | bitwise not operator                                                                        |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"\\\\\\\"    | backslash operator                                                                          |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"a[]\\\"   | array indexing                                                                              |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"[,]\\\"   | vertical concatenation                                                                      |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"[;]\\\"   | also vertical concatenation                                                                 |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"[  ]\\\"  | with space-separated expressions, horizontal concatenation                                  |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"T{ }\\\"  | parametric type instantiation                                                               |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"{  }\\\"  | construct a cell array                                                                      |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\";\\\"     | statement separator                                                                         |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\",\\\"     | separate function arguments or tuple components                                             |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"?\\\"     | 3-argument conditional operator                                                             |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"\\\"\\\"\\\"  | delimit string literals                                                                     |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"''\\\"    | delimit character literals                                                                  |
   +-----------+---------------------------------------------------------------------------------------------+
   | >>``<<    | delimit external process (command) specifications                                           |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"...\\\"   | splice arguments into a function call, or declare a varargs function                        |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\".\\\"     | access named fields in objects or names inside modules, also prefixes elementwise operators |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"a:b\\\"   | range                                                                                       |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"a:s:b\\\" | range                                                                                       |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\":\\\"     | index an entire dimension                                                                   |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\"::\\\"    | type annotation                                                                             |
   +-----------+---------------------------------------------------------------------------------------------+
   | \\\":( )\\\"  | quoted expression                                                                           |
   +-----------+---------------------------------------------------------------------------------------------+

"),

("Base.Sort","Base.Sort","sort","sort(v[, alg[, ord]])

   Sort a vector in ascending order.  Specify \"alg\" to choose a
   particular sorting algorithm (\"Sort.InsertionSort\",
   \"Sort.QuickSort\", \"Sort.MergeSort\", or \"Sort.TimSort\"), and
   \"ord\" to sort with a custom ordering (e.g., Sort.Reverse or a
   comparison function).

"),

("Base.Sort","Base.Sort","sort!","sort!(...)

   In-place sort.

"),

("Base.Sort","Base.Sort","sortby","sortby(v, by[, alg])

   Sort a vector according to \"by(v)\".  Specify \"alg\" to choose a
   particular sorting algorithm (\"Sort.InsertionSort\",
   \"Sort.QuickSort\", \"Sort.MergeSort\", or \"Sort.TimSort\").

"),

("Base.Sort","Base.Sort","sortby!","sortby!(...)

   In-place \"sortby\".

"),

("Base.Sort","Base.Sort","sortperm","sortperm(v[, alg[, ord]])

   Return a permutation vector, which when applied to the input vector
   \"v\" will sort it.  Specify \"alg\" to choose a particular sorting
   algorithm (\"Sort.InsertionSort\", \"Sort.QuickSort\",
   \"Sort.MergeSort\", or \"Sort.TimSort\"), and \"ord\" to sort with
   a custom ordering (e.g., Sort.Reverse or a comparison function).

"),

("Base.Sort","Base.Sort","issorted","issorted(v[, ord])

   Test whether a vector is in ascending sorted order.  If specified,
   \"ord\" gives the ordering to test.

"),

("Base.Sort","Base.Sort","searchsorted","searchsorted(a, x[, ord])

   Returns the index of the first value of \"a\" equal to or
   succeeding \"x\", according to ordering \"ord\" (default:
   \"Sort.Forward\").

   Alias for \"searchsortedfirst()\"

"),

("Base.Sort","Base.Sort","searchsortedfirst","searchsortedfirst(a, x[, ord])

   Returns the index of the first value of \"a\" equal to or
   succeeding \"x\", according to ordering \"ord\" (default:
   \"Sort.Forward\").

"),

("Base.Sort","Base.Sort","searchsortedlast","searchsortedlast(a, x[, ord])

   Returns the index of the last value of \"a\" preceding or equal to
   \"x\", according to ordering \"ord\" (default: \"Sort.Forward\").

"),

("Base.Sort","Base.Sort","select","select(v, k[, ord])

   Find the element in position \"k\" in the sorted vector \"v\"
   without sorting, according to ordering \"ord\" (default:
   \"Sort.Forward\").

"),

("Base.Sort","Base.Sort","select!","select!(v, k[, ord])

   Version of \"select\" which permutes the input vector in place.

"),

("Sparse Matrices","","sparse","sparse(I, J, V[, m, n, combine])

   Create a sparse matrix \"S\" of dimensions \"m x n\" such that
   \"S[I[k], J[k]] = V[k]\". The \"combine\" function is used to
   combine duplicates. If \"m\" and \"n\" are not specified, they are
   set to \"max(I)\" and \"max(J)\" respectively. If the \"combine\"
   function is not supplied, duplicates are added by default.

"),

("Sparse Matrices","","sparsevec","sparsevec(I, V[, m, combine])

   Create a sparse matrix \"S\" of size \"m x 1\" such that \"S[I[k]]
   = V[k]\". Duplicates are combined using the \"combine\" function,
   which defaults to *+* if it is not provided. In julia, sparse
   vectors are really just sparse matrices with one column. Given
   Julia's Compressed Sparse Columns (CSC) storage format, a sparse
   column matrix with one column is sparse, whereas a sparse row
   matrix with one row ends up being dense.

"),

("Sparse Matrices","","sparsevec","sparsevec(D::Dict[, m])

   Create a sparse matrix of size \"m x 1\" where the row values are
   keys from the dictionary, and the nonzero values are the values
   from the dictionary.

"),

("Sparse Matrices","","issparse","issparse(S)

   Returns \"true\" if \"S\" is sparse, and \"false\" otherwise.

"),

("Sparse Matrices","","sparse","sparse(A)

   Convert a dense matrix \"A\" into a sparse matrix.

"),

("Sparse Matrices","","sparsevec","sparsevec(A)

   Convert a dense vector \"A\" into a sparse matrix of size \"m x
   1\". In julia, sparse vectors are really just sparse matrices with
   one column.

"),

("Sparse Matrices","","dense","dense(S)

   Convert a sparse matrix \"S\" into a dense matrix.

"),

("Sparse Matrices","","full","full(S)

   Convert a sparse matrix \"S\" into a dense matrix.

"),

("Sparse Matrices","","spzeros","spzeros(m, n)

   Create an empty sparse matrix of size \"m x n\".

"),

("Sparse Matrices","","speye","speye(type, m[, n])

   Create a sparse identity matrix of specified type of size \"m x
   m\". In case \"n\" is supplied, create a sparse identity matrix of
   size \"m x n\".

"),

("Sparse Matrices","","spones","spones(S)

   Create a sparse matrix with the same structure as that of \"S\",
   but with every nonzero element having the value \"1.0\".

"),

("Sparse Matrices","","sprand","sprand(m, n, density[, rng])

   Create a random sparse matrix with the specified density. Nonzeros
   are sampled from the distribution specified by \"rng\". The uniform
   distribution is used in case \"rng\" is not specified.

"),

("Sparse Matrices","","sprandn","sprandn(m, n, density)

   Create a random sparse matrix of specified density with nonzeros
   sampled from the normal distribution.

"),

("Sparse Matrices","","sprandbool","sprandbool(m, n, density)

   Create a random sparse boolean matrix with the specified density.

"),


}
