# automatically generated -- do not edit

{

("Getting Around","Base","exit","exit([code])

   Quit (or control-D at the prompt). The default exit code is zero,
   indicating that the processes completed successfully.

"),

("Getting Around","Base","quit","quit()

   Calls \"exit(0)\".

"),

("Getting Around","Base","atexit","atexit(f)

   Register a zero-argument function to be called at exit.

"),

("Getting Around","Base","isinteractive","isinteractive()

   Determine whether Julia is running an interactive session.

"),

("Getting Around","Base","whos","whos([Module,] [pattern::Regex])

   Print information about global variables in a module, optionally
   restricted to those matching \"pattern\".

"),

("Getting Around","Base","edit","edit(file::String[, line])

   Edit a file optionally providing a line number to edit at. Returns
   to the julia prompt when you quit the editor.

"),

("Getting Around","Base","edit","edit(function[, types])

   Edit the definition of a function, optionally specifying a tuple of
   types to indicate which method to edit.

"),

("Getting Around","Base","less","less(file::String[, line])

   Show a file using the default pager, optionally providing a
   starting line number. Returns to the julia prompt when you quit the
   pager.

"),

("Getting Around","Base","less","less(function[, types])

   Show the definition of a function using the default pager,
   optionally specifying a tuple of types to indicate which method to
   see.

"),

("Getting Around","Base","clipboard","clipboard(x)

   Send a printed form of \"x\" to the operating system clipboard
   (\"copy\").

"),

("Getting Around","Base","clipboard","clipboard() -> String

   Return the contents of the operating system clipboard (\"paste\").

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

("Getting Around","Base","help","help(name)

   Get help for a function. \"name\" can be an object or a string.

"),

("Getting Around","Base","apropos","apropos(string)

   Search documentation for functions related to \"string\".

"),

("Getting Around","Base","which","which(f, args...)

   Show which method of \"f\" will be called for the given arguments.

"),

("Getting Around","Base","@which","@which()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"which\" function on the resulting expression

"),

("Getting Around","Base","methods","methods(f)

   Show all methods of \"f\" with their argument types.

"),

("Getting Around","Base","methodswith","methodswith(typ[, showparents])

   Show all methods with an argument of type \"typ\". If optional
   \"showparents\" is \"true\", also show arguments with a parent type
   of \"typ\", excluding type \"Any\".

"),

("Getting Around","Base","@show","@show()

   Show an expression and result, returning the result

"),

("Getting Around","Base","versioninfo","versioninfo([verbose::Bool])

   Print information about the version of Julia in use. If the
   \"verbose\" argument is true, detailed system information is shown
   as well.

"),

("All Objects","Base","is","is(x, y)

   Determine whether \"x\" and \"y\" are identical, in the sense that
   no program could distinguish them. Compares mutable objects by
   address in memory, and compares immutable objects (such as numbers)
   by contents at the bit level. This function is sometimes called
   \"egal\". The \"===\" operator is an alias for this function.

"),

("All Objects","Base","isa","isa(x, type)

   Determine whether \"x\" is of the given type.

"),

("All Objects","Base","isequal","isequal(x, y)

   True if and only if \"x\" and \"y\" have the same contents. Loosely
   speaking, this means \"x\" and \"y\" would look the same when
   printed. This is the default comparison function used by hash
   tables (\"Dict\"). New types with a notion of equality should
   implement this function, except for numbers, which should implement
   \"==\" instead. However, numeric types with special values might
   need to implement \"isequal\" as well. For example, floating point
   \"NaN\" values are not \"==\", but are all equivalent in the sense
   of \"isequal\". Numbers of different types are considered unequal.
   Mutable containers should generally implement \"isequal\" by
   calling \"isequal\" recursively on all contents.

"),

("All Objects","Base","isless","isless(x, y)

   Test whether \"x\" is less than \"y\". Provides a total order
   consistent with \"isequal\". Values that are normally unordered,
   such as \"NaN\", are ordered in an arbitrary but consistent
   fashion. This is the default comparison used by \"sort\". Non-
   numeric types that can be ordered should implement this function.
   Numeric types only need to implement it if they have special values
   such as \"NaN\".

"),

("All Objects","Base","ifelse","ifelse(condition::Bool, x, y)

   Return \"x\" if \"condition\" is true, otherwise return \"y\". This
   differs from \"?\" or \"if\" in that it is an ordinary function, so
   all the arguments are evaluated first.

"),

("All Objects","Base","lexcmp","lexcmp(x, y)

   Compare \"x\" and \"y\" lexicographically and return -1, 0, or 1
   depending on whether \"x\" is less than, equal to, or greater than
   \"y\", respectively. This function should be defined for
   lexicographically comparable types, and \"lexless\" will call
   \"lexcmp\" by default.

"),

("All Objects","Base","lexless","lexless(x, y)

   Determine whether \"x\" is lexicographically less than \"y\".

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

("All Objects","Base","isdefined","isdefined(object, index | symbol)

   Tests whether an assignable location is defined. The arguments can
   be an array and index, a composite object and field name (as a
   symbol), or a module and a symbol.

"),

("All Objects","Base","convert","convert(type, x)

   Try to convert \"x\" to the given type. Conversions from floating
   point to integer, rational to integer, and complex to real will
   raise an \"InexactError\" if \"x\" cannot be represented exactly in
   the new type.

"),

("All Objects","Base","promote","promote(xs...)

   Convert all arguments to their common promotion type (if any), and
   return them all (as a tuple).

"),

("All Objects","Base","oftype","oftype(x, y)

   Convert \"y\" to the type of \"x\".

"),

("All Objects","Base","identity","identity(x)

   The identity function. Returns its argument.

"),

("Types","Base","super","super(T::DataType)

   Return the supertype of DataType T

"),

("Types","Base","issubtype","issubtype(type1, type2)

   True if and only if all values of \"type1\" are also of \"type2\".
   Can also be written using the \"<:\" infix operator as \"type1 <:
   type2\".

"),

("Types","Base","<:","<:(T1, T2)

   Subtype operator, equivalent to \"issubtype(T1,T2)\".

"),

("Types","Base","subtypes","subtypes(T::DataType)

   Return a list of immediate subtypes of DataType T.  Note that all
   currently loaded subtypes are included, including those not visible
   in the current module.

"),

("Types","Base","subtypetree","subtypetree(T::DataType)

   Return a nested list of all subtypes of DataType T.  Note that all
   currently loaded subtypes are included, including those not visible
   in the current module.

"),

("Types","Base","typemin","typemin(type)

   The lowest value representable by the given (real) numeric type.

"),

("Types","Base","typemax","typemax(type)

   The highest value representable by the given (real) numeric type.

"),

("Types","Base","realmin","realmin(type)

   The smallest in absolute value non-subnormal value representable by
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

("Types","Base","promote_rule","promote_rule(type1, type2)

   Specifies what type should be used by \"promote\" when given values
   of types \"type1\" and \"type2\". This function should not be
   called directly, but should have definitions added to it for new
   types as appropriate.

"),

("Types","Base","getfield","getfield(value, name::Symbol)

   Extract a named field from a value of composite type. The syntax
   \"a.b\" calls \"getfield(a, :b)\", and the syntax \"a.(b)\" calls
   \"getfield(a, b)\".

"),

("Types","Base","setfield","setfield(value, name::Symbol, x)

   Assign \"x\" to a named field in \"value\" of composite type. The
   syntax \"a.b = c\" calls \"setfield(a, :b, c)\", and the syntax
   \"a.(b) = c\" calls \"setfield(a, b, c)\".

"),

("Types","Base","fieldoffsets","fieldoffsets(type)

   The byte offset of each field of a type relative to the data start.
   For example, we could use it in the following manner to summarize
   information about a struct type:

      structinfo(T) = [zip(fieldoffsets(T),names(T),T.types)...]
      structinfo(Stat)

"),

("Types","Base","fieldtype","fieldtype(value, name::Symbol)

   Determine the declared type of a named field in a value of
   composite type.

"),

("Types","Base","isimmutable","isimmutable(v)

   True if value \"v\" is immutable.  See *Immutable Composite Types*
   for a discussion of immutability.

"),

("Types","Base","isbits","isbits(T)

   True if \"T\" is a \"plain data\" type, meaning it is immutable and
   contains no references to other values. Typical examples are
   numeric types such as \"Uint8\", \"Float64\", and
   \"Complex{Float64}\".

"),

("Types","Base","isleaftype","isleaftype(T)

   Determine whether \"T\" is a concrete type that can have instances,
   meaning its only subtypes are itself and \"None\" (but \"T\" itself
   is not \"None\").

"),

("Types","Base","typejoin","typejoin(T, S)

   Compute a type that contains both \"T\" and \"S\".

"),

("Types","Base","typeintersect","typeintersect(T, S)

   Compute a type that contains the intersection of \"T\" and \"S\".
   Usually this will be the smallest such type or one close to it.

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

("Generic Functions","Base","|>","|>(x, f)

   Applies a function to the preceding argument which allows for easy
   function chaining.

   **Example**: \"[1:5] |> x->x.^2 |> sum |> inv\"

"),

("Syntax","Base","eval","eval(expr::Expr)

   Evaluate an expression and return the value.

"),

("Syntax","Base","@eval","@eval()

   Evaluate an expression and return the value.

"),

("Syntax","Base","evalfile","evalfile(path::String)

   Evaluate all expressions in the given file, and return the value of
   the last one. No other processing (path searching, fetching from
   node 1, etc.) is performed.

"),

("Syntax","Base","esc","esc(e::ANY)

   Only valid in the context of an Expr returned from a macro.
   Prevents the macro hygine pass from turning embedded variables into
   gensym variables. See the *Macros* section of the Metaprogramming
   chapter of the manual for more details and examples.

"),

("Syntax","Base","gensym","gensym([tag])

   Generates a symbol which will not conflict with other variable
   names.

"),

("Syntax","Base","@gensym","@gensym()

   Generates a gensym symbol for a variable. For example, *@gensym x
   y* is transformed into *x = gensym(\"x\"); y = gensym(\"y\")*.

"),

("Syntax","Base","parse","parse(str, [start]; greedy=true, raise=false)

   Parse the expression string and return an expression (which could
   later be passed to eval for execution). Start is the index of the
   first character to start parsing (default is 1). If greedy is true
   (default), parse will try to consume as much input as it can;
   otherwise, it will stop as soon as it has parsed a valid token. If
   raise is true (default), parse errors will raise an error;
   otherwise, parse will return the error as an expression object.

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

("Iterable Collections","Base","in","in(item, collection) -> Bool

   Determine whether an item is in the given collection, in the sense
   that it is \"isequal\" to one of the values generated by iterating
   over the collection.

"),

("Iterable Collections","Base","eltype","eltype(collection)

   Determine the type of the elements generated by iterating
   \"collection\". For associative collections, this will be a
   \"(key,value)\" tuple type.

"),

("Iterable Collections","Base","indexin","indexin(a, b)

   Returns a vector containing the highest index in \"b\" for each
   value in \"a\" that is a member of \"b\" . The output vector
   contains 0 wherever \"a\" is not a member of \"b\".

"),

("Iterable Collections","Base","findin","findin(a, b)

   Returns the indices of elements in collection \"a\" that appear in
   collection \"b\"

"),

("Iterable Collections","Base","unique","unique(itr)

   Returns an array containing only the unique elements of the
   iterable \"itr\", in the order that the first of each set of
   equivalent elements originally appears.

"),

("Iterable Collections","Base","reduce","reduce(op, v0, itr)

   Reduce the given collection with the given operator, i.e.
   accumulate \"v = op(v,elt)\" for each element, where \"v\" starts
   as \"v0\". Reductions for certain commonly-used operators are
   available in a more convenient 1-argument form: \"maximum(itr)\",
   \"minimum(itr)\", \"sum(itr)\", \"prod(itr)\", \"any(itr)\",
   \"all(itr)\".

   The associativity of the reduction is implementation-dependent; if
   you need a particular associativity, e.g. left-to-right, you should
   write your own loop.

"),

("Iterable Collections","Base","maximum","maximum(itr)

   Returns the largest element in a collection

"),

("Iterable Collections","Base","maximum","maximum(A, dims)

   Compute the maximum value of an array over the given dimensions

"),

("Iterable Collections","Base","minimum","minimum(itr)

   Returns the smallest element in a collection

"),

("Iterable Collections","Base","minimum","minimum(A, dims)

   Compute the minimum value of an array over the given dimensions

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

("Iterable Collections","Base","sum","sum(A, dims)

   Sum elements of an array over the given dimensions.

"),

("Iterable Collections","Base","sum","sum(f, itr)

   Sum the results of calling function \"f\" on each element of
   \"itr\".

"),

("Iterable Collections","Base","prod","prod(itr)

   Returns the product of all elements of a collection

"),

("Iterable Collections","Base","prod","prod(A, dims)

   Multiply elements of an array over the given dimensions.

"),

("Iterable Collections","Base","any","any(itr) -> Bool

   Test whether any elements of a boolean collection are true

"),

("Iterable Collections","Base","any","any(A, dims)

   Test whether any values along the given dimensions of an array are
   true.

"),

("Iterable Collections","Base","all","all(itr) -> Bool

   Test whether all elements of a boolean collection are true

"),

("Iterable Collections","Base","all","all(A, dims)

   Test whether all values along the given dimensions of an array are
   true.

"),

("Iterable Collections","Base","count","count(p, itr) -> Integer

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

   The associativity of the reduction is implementation-dependent; if
   you need a particular associativity, e.g. left-to-right, you should
   write your own loop.

"),

("Iterable Collections","Base","first","first(coll)

   Get the first element of an iterable collection.

"),

("Iterable Collections","Base","last","last(coll)

   Get the last element of an ordered collection, if it can be
   computed in O(1) time. This is accomplished by calling \"endof\" to
   get the last index.

"),

("Iterable Collections","Base","step","step(r)

   Get the step size of a \"Range\" object.

"),

("Iterable Collections","Base","collect","collect(collection)

   Return an array of all items in a collection. For associative
   collections, returns (key, value) tuples.

"),

("Iterable Collections","Base","collect","collect(element_type, collection)

   Return an array of type \"Array{element_type,1}\" of all items in a
   collection.

"),

("Iterable Collections","Base","issubset","issubset(a, b)

   Determine whether every element of \"a\" is also in \"b\", using
   the \"in\" function.

"),

("Iterable Collections","Base","filter","filter(function, collection)

   Return a copy of \"collection\", removing elements for which
   \"function\" is false. For associative collections, the function is
   passed two arguments (key and value).

"),

("Iterable Collections","Base","filter!","filter!(function, collection)

   Update \"collection\", removing elements for which \"function\" is
   false. For associative collections, the function is passed two
   arguments (key and value).

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

("Associative Collections","Base","Dict","Dict()

   \"Dict{K,V}()\" constructs a hashtable with keys of type K and
   values of type V. The literal syntax is \"{\"A\"=>1, \"B\"=>2}\"
   for a \"Dict{Any,Any}\", or \"[\"A\"=>1, \"B\"=>2]\" for a \"Dict\"
   of inferred type.

"),

("Associative Collections","Base","haskey","haskey(collection, key)

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

   Delete the mapping for the given key in a collection, and return
   the colection.

"),

("Associative Collections","Base","pop!","pop!(collection, key[, default])

   Delete and return the mapping for \"key\" if it exists in
   \"collection\", otherwise return \"default\", or throw an error if
   default is not specified.

"),

("Associative Collections","Base","keys","keys(collection)

   Return an iterator over all keys in a collection.
   \"collect(keys(d))\" returns an array of keys.

"),

("Associative Collections","Base","values","values(collection)

   Return an iterator over all values in a collection.
   \"collect(values(d))\" returns an array of values.

"),

("Associative Collections","Base","merge","merge(collection, others...)

   Construct a merged collection from the given collections.

"),

("Associative Collections","Base","merge!","merge!(collection, others...)

   Update collection with pairs from the other collections

"),

("Associative Collections","Base","sizehint","sizehint(s, n)

   Suggest that collection \"s\" reserve capacity for at least \"n\"
   elements. This can improve performance.

"),

("Set-Like Collections","Base","add!","add!(collection, key)

   Add an element to a set-like collection.

"),

("Set-Like Collections","Base","Set","Set(x...)

   Construct a \"Set\" with the given elements. Should be used instead
   of \"IntSet\" for sparse integer sets, or for sets of arbitrary
   objects.

"),

("Set-Like Collections","Base","IntSet","IntSet(i...)

   Construct a sorted set of the given integers. Implemented as a bit
   string, and therefore designed for dense integer sets. If the set
   will be sparse (for example holding a single very large integer),
   use \"Set\" instead.

"),

("Set-Like Collections","Base","union","union(s1, s2...)

   Construct the union of two or more sets. Maintains order with
   arrays.

"),

("Set-Like Collections","Base","union!","union!(s, iterable)

   Union each element of \"iterable\" into set \"s\" in-place.

"),

("Set-Like Collections","Base","intersect","intersect(s1, s2...)

   Construct the intersection of two or more sets. Maintains order and
   multiplicity of the first argument for arrays and ranges.

"),

("Set-Like Collections","Base","setdiff","setdiff(s1, s2)

   Construct the set of elements in \"s1\" but not \"s2\". Maintains
   order with arrays.

"),

("Set-Like Collections","Base","setdiff!","setdiff!(s, iterable)

   Remove each element of \"iterable\" from set \"s\" in-place.

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

("Set-Like Collections","Base","intersect!","intersect!(s1, s2)

   Intersects IntSets s1 and s2 and overwrites the set s1 with the
   result. If needed, s1 will be expanded to the size of s2.

"),

("Set-Like Collections","Base","issubset","issubset(A, S) -> Bool

   True if \"A ⊆ S\" (A is a subset of or equal to S)

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

("Dequeues","Base","splice!","splice!(collection, index[, replacement]) -> item

   Remove the item at the given index, and return the removed item.
   Subsequent items are shifted down to fill the resulting gap. If
   specified, replacement values from an ordered collection will be
   spliced in place of the removed item.

"),

("Dequeues","Base","splice!","splice!(collection, range[, replacement]) -> items

   Remove items in the specified index range, and return a collection
   containing the removed items. Subsequent items are shifted down to
   fill the resulting gap. If specified, replacement values from an
   ordered collection will be spliced in place of the removed items.

"),

("Dequeues","Base","resize!","resize!(collection, n) -> collection

   Resize collection to contain \"n\" elements.

"),

("Dequeues","Base","append!","append!(collection, items) -> collection.

   Add the elements of \"items\" to the end of a collection.
   \"append!([1],[2,3]) => [1,2,3]\"

"),

("Dequeues","Base","prepend!","prepend!(collection, items) -> collection

   Insert the elements of \"items\" to the beginning of a collection.
   \"prepend!([3],[1,2]) => [1,2,3]\"

"),

("Strings","Base","length","length(s)

   The number of characters in string \"s\".

"),

("Strings","Base","sizeof","sizeof(s::String)

   The number of bytes in string \"s\".

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

("Strings","Base","ismatch","ismatch(r::Regex, s::String) -> Bool

   Test whether a string contains a match of the given regular
   expression.

"),

("Strings","Base","match","match(r::Regex, s::String[, idx::Integer[, addopts]])

   Search for the first match of the regular expression \"r\" in \"s\"
   and return a RegexMatch object containing the match, or nothing if
   the match failed. The matching substring can be retrieved by
   accessing \"m.match\" and the captured sequences can be retrieved
   by accessing \"m.captures\"

"),

("Strings","Base","eachmatch","eachmatch(r::Regex, s::String[, overlap::Bool=false])

   Search for all matches of a the regular expression \"r\" in \"s\"
   and return a iterator over the matches. If overlap is true, the
   matching sequences are allowed to overlap indices in the original
   string, otherwise they must be from distinct character ranges.

"),

("Strings","Base","matchall","matchall(r::Regex, s::String[, overlap::Bool=false]) -> Vector{String}

   Return a vector of the matching substrings from eachmatch.

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

   Search for the first occurance of the given characters within the
   given string. The second argument may be a single character, a
   vector or a set of characters, a string, or a regular expression
   (though regular expressions are only allowed on contiguous strings,
   such as ASCII or UTF-8 strings). The third argument optionally
   specifies a starting index. The return value is a range of indexes
   where the matching sequence is found, such that \"s[search(s,x)] ==
   x\":

   *search(string, \"substring\")* = *start:end* such that
   \"string[start:end] == \"substring\"\", or *0:-1* if unmatched.

   *search(string, 'c')*         = *index* such that \"string[index]
   == 'c'\", or *0* if unmatched.

"),

("Strings","Base","rsearch","rsearch(string, chars[, start])

   Similar to \"search\", but returning the last occurance of the
   given characters within the given string, searching in reverse from
   \"start\".

"),

("Strings","Base","searchindex","searchindex(string, substring[, start])

   Similar to \"search\", but return only the start index at which the
   substring is found, or 0 if it is not.

"),

("Strings","Base","rsearchindex","rsearchindex(string, substring[, start])

   Similar to \"rsearch\", but return only the start index at which
   the substring is found, or 0 if it is not.

"),

("Strings","Base","contains","contains(haystack, needle)

   Determine whether the second argument is a substring of the first.

"),

("Strings","Base","replace","replace(string, pat, r[, n])

   Search for the given pattern \"pat\", and replace each occurrence
   with \"r\". If \"n\" is provided, replace at most \"n\"
   occurrences.  As with search, the second argument may be a single
   character, a vector or a set of characters, a string, or a regular
   expression. If \"r\" is a function, each occurrence is replaced
   with \"r(s)\" where \"s\" is the matched substring.

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

("Strings","Base","rsplit","rsplit(string, [chars, [limit,] [include_empty]])

   Similar to \"split\", but starting from the end of the string.

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

("Strings","Base","beginswith","beginswith(string, prefix)

   Returns \"true\" if \"string\" starts with \"prefix\".

"),

("Strings","Base","endswith","endswith(string, suffix)

   Returns \"true\" if \"string\" ends with \"suffix\".

"),

("Strings","Base","uppercase","uppercase(string)

   Returns \"string\" with all characters converted to uppercase.

"),

("Strings","Base","lowercase","lowercase(string)

   Returns \"string\" with all characters converted to lowercase.

"),

("Strings","Base","ucfirst","ucfirst(string)

   Returns \"string\" with the first character converted to uppercase.

"),

("Strings","Base","lcfirst","lcfirst(string)

   Returns \"string\" with the first character converted to lowercase.

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

("Strings","Base","isalnum","isalnum(c::Union(Char, String))

   Tests whether a character is alphanumeric, or whether this is true
   for all elements of a string.

"),

("Strings","Base","isalpha","isalpha(c::Union(Char, String))

   Tests whether a character is alphabetic, or whether this is true
   for all elements of a string.

"),

("Strings","Base","isascii","isascii(c::Union(Char, String))

   Tests whether a character belongs to the ASCII character set, or
   whether this is true for all elements of a string.

"),

("Strings","Base","isblank","isblank(c::Union(Char, String))

   Tests whether a character is a tab or space, or whether this is
   true for all elements of a string.

"),

("Strings","Base","iscntrl","iscntrl(c::Union(Char, String))

   Tests whether a character is a control character, or whether this
   is true for all elements of a string.

"),

("Strings","Base","isdigit","isdigit(c::Union(Char, String))

   Tests whether a character is a numeric digit (0-9), or whether this
   is true for all elements of a string.

"),

("Strings","Base","isgraph","isgraph(c::Union(Char, String))

   Tests whether a character is printable, and not a space, or whether
   this is true for all elements of a string.

"),

("Strings","Base","islower","islower(c::Union(Char, String))

   Tests whether a character is a lowercase letter, or whether this is
   true for all elements of a string.

"),

("Strings","Base","isprint","isprint(c::Union(Char, String))

   Tests whether a character is printable, including space, or whether
   this is true for all elements of a string.

"),

("Strings","Base","ispunct","ispunct(c::Union(Char, String))

   Tests whether a character is printable, and not a space or
   alphanumeric, or whether this is true for all elements of a string.

"),

("Strings","Base","isspace","isspace(c::Union(Char, String))

   Tests whether a character is any whitespace character, or whether
   this is true for all elements of a string.

"),

("Strings","Base","isupper","isupper(c::Union(Char, String))

   Tests whether a character is an uppercase letter, or whether this
   is true for all elements of a string.

"),

("Strings","Base","isxdigit","isxdigit(c::Union(Char, String))

   Tests whether a character is a valid hexadecimal digit, or whether
   this is true for all elements of a string.

"),

("Strings","Base","symbol","symbol(str)

   Convert a string to a \"Symbol\".

"),

("Strings","Base","escape_string","escape_string(str::String) -> String

   General escaping of traditional C and Unicode escape sequences. See
   \"print_escaped()\" for more general escaping.

"),

("Strings","Base","unescape_string","unescape_string(s::String) -> String

   General unescaping of traditional C and Unicode escape sequences.
   Reverse of \"escape_string()\". See also \"print_unescaped()\".

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

("I/O","Base","IOBuffer","IOBuffer() -> IOBuffer

   Create an in-memory I/O stream.

"),

("I/O","Base","IOBuffer","IOBuffer(size::Int)

   Create a fixed size IOBuffer. The buffer will not grow dynamically.

"),

("I/O","Base","IOBuffer","IOBuffer(string)

   Create a read-only IOBuffer on the data underlying the given string

"),

("I/O","Base","IOBuffer","IOBuffer([data][, readable, writable[, maxsize]])

   Create an IOBuffer, which may optionally operate on a pre-existing
   array. If the readable/writable arguments are given, they restrict
   whether or not the buffer may be read from or written to
   respectively. By default the buffer is readable but not writable.
   The last argument optionally specifies a size beyond which the
   buffer may not be grown.

"),

("I/O","Base","takebuf_array","takebuf_array(b::IOBuffer)

   Obtain the contents of an \"IOBuffer\" as an array, without
   copying.

"),

("I/O","Base","takebuf_string","takebuf_string(b::IOBuffer)

   Obtain the contents of an \"IOBuffer\" as a string, without
   copying.

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

("I/O","Base","flush_cstdio","flush_cstdio()

   Flushes the C \"stdout\" and \"stderr\" streams (which may have
   been written to by external C code).

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

("I/O","Base","readbytes!","readbytes!(stream, b::Vector{Uint8}, nb=length(b))

   Read at most \"nb\" bytes from the stream into \"b\", returning the
   number of bytes read (increasing the size of \"b\" as needed).

"),

("I/O","Base","readbytes","readbytes(stream, nb=typemax(Int))

   Read at most \"nb\" bytes from the stream, returning a
   \"Vector{Uint8}\" of the bytes read.

"),

("I/O","Base","position","position(s)

   Get the current position of a stream.

"),

("I/O","Base","seek","seek(s, pos)

   Seek a stream to the given position.

"),

("I/O","Base","seekstart","seekstart(s)

   Seek a stream to its beginning.

"),

("I/O","Base","seekend","seekend(s)

   Seek a stream to its end.

"),

("I/O","Base","skip","skip(s, offset)

   Seek a stream relative to the current position.

"),

("I/O","Base","eof","eof(stream)

   Tests whether an I/O stream is at end-of-file. If the stream is not
   yet exhausted, this function will block to wait for more data if
   necessary, and then return \"false\". Therefore it is always safe
   to read one byte after seeing \"eof\" return \"false\". \"eof\"
   will return \"false\" as long as buffered data is still available,
   even if the remote end of a connection is closed.

"),

("I/O","Base","isreadonly","isreadonly(stream)

   Determine whether a stream is read-only.

"),

("I/O","Base","isopen","isopen(stream)

   Determine whether a stream is open (i.e. has not been closed yet).
   If the connection has been closed remotely (in case of e.g. a
   socket), \"isopen\" will return \"false\" even though buffered data
   may still be available. Use \"eof\" to check if necessary.

"),

("I/O","Base","ntoh","ntoh(x)

   Converts the endianness of a value from Network byte order (big-
   endian) to that used by the Host.

"),

("I/O","Base","hton","hton(x)

   Converts the endianness of a value from that used by the Host to
   Network byte order (big-endian).

"),

("I/O","Base","ltoh","ltoh(x)

   Converts the endianness of a value from Little-endian to that used
   by the Host.

"),

("I/O","Base","htol","htol(x)

   Converts the endianness of a value from that used by the Host to
   Little-endian.

"),

("I/O","Base","ENDIAN_BOM","ENDIAN_BOM

   The 32-bit byte-order-mark indicates the native byte order of the
   host machine. Little-endian machines will contain the value
   0x04030201. Big-endian machines will contain the value 0x01020304.

"),

("I/O","Base","serialize","serialize(stream, value)

   Write an arbitrary value to a stream in an opaque format, such that
   it can be read back by \"deserialize\". The read-back value will be
   as identical as possible to the original. In general, this process
   will not work if the reading and writing are done by different
   versions of Julia, or an instance of Julia with a different system
   image.

"),

("I/O","Base","deserialize","deserialize(stream)

   Read a value written by \"serialize\".

"),

("I/O","Base","print_escaped","print_escaped(io, str::String, esc::String)

   General escaping of traditional C and Unicode escape sequences,
   plus any characters in esc are also escaped (with a backslash).

"),

("I/O","Base","print_unescaped","print_unescaped(io, s::String)

   General unescaping of traditional C and Unicode escape sequences.
   Reverse of \"print_escaped()\".

"),

("I/O","Base","print_joined","print_joined(io, items, delim[, last])

   Print elements of \"items\" to \"io\" with \"delim\" between them.
   If \"last\" is specified, it is used as the final delimiter instead
   of \"delim\".

"),

("I/O","Base","print_shortest","print_shortest(io, x)

   Print the shortest possible representation of number \"x\" as a
   floating point number, ensuring that it would parse to the exact
   same number.

"),

("I/O","Base","fd","fd(stream)

   Returns the file descriptor backing the stream or file. Note that
   this function only applies to synchronous *File*'s and *IOStream*'s
   not to any of the asynchronous streams.

"),

("I/O","Base","redirect_stdout","redirect_stdout()

   Create a pipe to which all C and Julia level STDOUT output will be
   redirected. Returns a tuple (rd,wr) representing the pipe ends.
   Data written to STDOUT may now be read from the rd end of the pipe.
   The wr end is given for convenience in case the old STDOUT object
   was cached by the user and needs to be replaced elsewhere.

"),

("I/O","Base","redirect_stdout","redirect_stdout(stream)

   Replace STDOUT by stream for all C and julia level output to
   STDOUT. Note that *stream* must be a TTY, a Pipe or a TcpSocket.

"),

("I/O","Base","redirect_stderr","redirect_stderr([stream])

   Like redirect_stdout, but for STDERR

"),

("I/O","Base","redirect_stdin","redirect_stdin([stream])

   Like redirect_stdout, but for STDIN. Note that the order of the
   return tuple is still (rd,wr), i.e. data to be read from STDIN, may
   be written to wr.

"),

("I/O","Base","readchomp","readchomp(x)

   Read the entirety of x as a string but remove trailing newlines.
   Equivalent to chomp(readall(x)).

"),

("I/O","Base","readdir","readdir([dir]) -> Vector{ByteString}

   Returns the files and directories in the directory *dir* (or the
   current working directory if not given).

"),

("I/O","Base","truncate","truncate(file, n)

   Resize the file or buffer given by the first argument to exactly
   *n* bytes, filling previously unallocated space with '0' if the
   file or buffer is grown

"),

("I/O","Base","skipchars","skipchars(stream, predicate; linecomment::Char)

   Advance the stream until before the first character for which
   \"predicate\" returns false. For example \"skipchars(stream,
   isspace)\" will skip all whitespace. If keyword argument
   \"linecomment\" is specified, characters from that character
   through the end of a line will also be skipped.

"),

("I/O","Base","countlines","countlines(io[, eol::Char])

   Read io until the end of the stream/file and count the number of
   non-empty lines. To specify a file pass the filename as the first
   argument. EOL markers other than 'n' are supported by passing them
   as the second argument.

"),

("I/O","Base","PipeBuffer","PipeBuffer()

   An IOBuffer that allows reading and performs writes by appending.
   Seeking and truncating are not supported. See IOBuffer for the
   available constructors.

"),

("I/O","Base","PipeBuffer","PipeBuffer(data::Vector{Uint8}[, maxsize])

   Create a PipeBuffer to operate on a data vector, optionally
   specifying a size beyond which the underlying Array may not be
   grown.

"),

("I/O","Base","readavailable","readavailable(stream)

   Read all available data on the stream, blocking the task only if no
   data is available.

"),

("I/O","Base","stat","stat(file)

   Returns a structure whose fields contain information about the
   file. The fields of the structure are:

   +-----------+------------------------------------------------------------------------+
   | size      | The size (in bytes) of the file                                        |
   +-----------+------------------------------------------------------------------------+
   | device    | ID of the device that contains the file                                |
   +-----------+------------------------------------------------------------------------+
   | inode     | The inode number of the file                                           |
   +-----------+------------------------------------------------------------------------+
   | mode      | The protection mode of the file                                        |
   +-----------+------------------------------------------------------------------------+
   | nlink     | The number of hard links to the file                                   |
   +-----------+------------------------------------------------------------------------+
   | uid       | The user id of the owner of the file                                   |
   +-----------+------------------------------------------------------------------------+
   | gid       | The group id of the file owner                                         |
   +-----------+------------------------------------------------------------------------+
   | rdev      | If this file refers to a device, the ID of the device it refers to     |
   +-----------+------------------------------------------------------------------------+
   | blksize   | The file-system preffered block size for the file                      |
   +-----------+------------------------------------------------------------------------+
   | blocks    | The number of such blocks allocated                                    |
   +-----------+------------------------------------------------------------------------+
   | mtime     | Unix timestamp of when the file was last modified                      |
   +-----------+------------------------------------------------------------------------+
   | ctime     | Unix timestamp of when the file was created                            |
   +-----------+------------------------------------------------------------------------+

"),

("I/O","Base","lstat","lstat(file)

   Like stat, but for symbolic links gets the info for the link itself
   rather than the file it refers to. This function must be called on
   a file path rather than a file object or a file descriptor.

"),

("I/O","Base","ctime","ctime(file)

   Equivalent to stat(file).ctime

"),

("I/O","Base","mtime","mtime(file)

   Equivalent to stat(file).mtime

"),

("I/O","Base","filemode","filemode(file)

   Equivalent to stat(file).mode

"),

("I/O","Base","filesize","filesize(path...)

   Equivalent to stat(file).size

"),

("I/O","Base","uperm","uperm(file)

   Gets the permissions of the owner of the file as a bitfield of

   +------+-----------------------+
   | 01   | Execute Permission    |
   +------+-----------------------+
   | 02   | Write Permission      |
   +------+-----------------------+
   | 04   | Read Permission       |
   +------+-----------------------+

   For allowed arguments, see the stat method.

"),

("I/O","Base","gperm","gperm(file)

   Like uperm but gets the permissions of the group owning the file

"),

("I/O","Base","operm","operm(file)

   Like uperm but gets the permissions for people who neither own the
   file nor are a member of the group owning the file

"),

("I/O","Base","cp","cp(src::String, dst::String)

   Copy a file from *src* to *dest*.

"),

("I/O","Base","download","download(url[, localfile])

   Download a file from the given url, optionally renaming it to the
   given local file name. Note that this function relies on the
   availability of external tools such as \"curl\", \"wget\" or
   \"fetch\" to download the file and is provided for convenience. For
   production use or situations in which more options are need, please
   use a package that provides the desired functionality instead.

"),

("I/O","Base","mv","mv(src::String, dst::String)

   Move a file from *src* to *dst*.

"),

("I/O","Base","rm","rm(path::String)

   Delete the file at the given path. Note that this does not work on
   directories.

"),

("I/O","Base","touch","touch(path::String)

   Update the last-modified timestamp on a file to the current time.

"),

("Network I/O","Base","connect","connect([host], port) -> TcpSocket

   Connect to the host \"host\" on port \"port\"

"),

("Network I/O","Base","connect","connect(path) -> Pipe

   Connect to the Named Pipe/Domain Socket at \"path\"

"),

("Network I/O","Base","listen","listen([addr], port) -> TcpServer

   Listen on port on the address specified by \"addr\". By default
   this listens on localhost only. To listen on all interfaces pass,
   \"IPv4(0)\" or \"IPv6(0)\" as appropriate.

"),

("Network I/O","Base","listen","listen(path) -> PipeServer

   Listens on/Creates a Named Pipe/Domain Socket

"),

("Network I/O","Base","getaddrinfo","getaddrinfo(host)

   Gets the IP address of the \"host\" (may have to do a DNS lookup)

"),

("Network I/O","Base","parseip","parseip(addr)

   Parse a string specifying an IPv4 or IPv6 ip address.

"),

("Network I/O","Base","IPv4","IPv4(host::Integer) -> IPv4

   Returns IPv4 object from ip address formatted as Integer

"),

("Network I/O","Base","IPv6","IPv6(host::Integer) -> IPv6

   Returns IPv6 object from ip address formatted as Integer

"),

("Network I/O","Base","nb_available","nb_available(stream)

   Returns the number of bytes available for reading before a read
   from this stream or buffer will block.

"),

("Network I/O","Base","accept","accept(server[, client])

   Accepts a connection on the given server and returns a connection
   to the client. An uninitialized client stream may be provided, in
   which case it will be used instead of creating a new stream.

"),

("Network I/O","Base","listenany","listenany(port_hint) -> (Uint16, TcpServer)

   Create a TcpServer on any port, using hint as a starting point.
   Returns a tuple of the actual port that the server was created on
   and the server itself.

"),

("Network I/O","Base","watch_file","watch_file(cb=false, s; poll=false)

   Watch file or directory \"s\" and run callback \"cb\" when \"s\" is
   modified. The \"poll\" parameter specifies whether to use file
   system event monitoring or polling. The callback function \"cb\"
   should accept 3 arguments: \"(filename, events, status)\" where
   \"filename\" is the name of file that was modified, \"events\" is
   an object with boolean fields \"changed\" and \"renamed\" when
   using file system event monitoring, or \"readable\" and
   \"writable\" when using polling, and \"status\" is always 0. Pass
   \"false\" for \"cb\" to not use a callback function.

"),

("Network I/O","Base","poll_fd","poll_fd(fd, seconds::Real; readable=false, writable=false)

   Poll a file descriptor fd for changes in the read or write
   availability and with a timeout given by the second argument. If
   the timeout is not needed, use \"wait(fd)\" instead. The keyword
   arguments determine which of read and/or write status should be
   monitored and at least one of them needs to be set to true. The
   returned value is an object with boolean fields \"readable\",
   \"writable\", and \"timedout\", giving the result of the polling.

"),

("Network I/O","Base","poll_file","poll_file(s, interval_seconds::Real, seconds::Real)

   Monitor a file for changes by polling every *interval_seconds*
   seconds for *seconds* seconds. A return value of true indicates the
   file changed, a return value of false indicates a timeout.

"),

("Text I/O","Base","show","show(x)

   Write an informative text representation of a value to the current
   output stream. New types should overload \"show(io, x)\" where the
   first argument is a stream. The representation used by \"show\"
   generally includes Julia-specific formatting and type information.

"),

("Text I/O","Base","showcompact","showcompact(x)

   Show a more compact representation of a value. This is used for
   printing array elements. If a new type has a different compact
   representation, it should overload \"showcompact(io, x)\" where the
   first argument is a stream.

"),

("Text I/O","Base","showall","showall(x)

   Similar to \"show\", except shows all elements of arrays.

"),

("Text I/O","Base","summary","summary(x)

   Return a string giving a brief description of a value. By default
   returns \"string(typeof(x))\". For arrays, returns strings like
   \"2x2 Float64 Array\".

"),

("Text I/O","Base","print","print(x)

   Write (to the default output stream) a canonical (un-decorated)
   text representation of a value if there is one, otherwise call
   \"show\". The representation used by \"print\" includes minimal
   formatting and tries to avoid Julia-specific details.

"),

("Text I/O","Base","println","println(x)

   Print (using \"print()\") \"x\" followed by a newline.

"),

("Text I/O","Base","print_with_color","print_with_color(color::Symbol[, io], strings...)

   Print strings in a color specified as a symbol, for example
   \":red\" or \":blue\".

"),

("Text I/O","Base","info","info(msg)

   Display an informational message.

"),

("Text I/O","Base","warn","warn(msg)

   Display a warning.

"),

("Text I/O","Base","@printf","@printf([io::IOStream], \"%Fmt\", args...)

   Print arg(s) using C \"printf()\" style format specification
   string. Optionally, an IOStream may be passed as the first argument
   to redirect output.

"),

("Text I/O","Base","@sprintf","@sprintf(\"%Fmt\", args...)

   Return \"@printf\" formatted output as string.

"),

("Text I/O","Base","sprint","sprint(f::Function, args...)

   Call the given function with an I/O stream and the supplied extra
   arguments. Everything written to this I/O stream is returned as a
   string.

"),

("Text I/O","Base","showerror","showerror(io, e)

   Show a descriptive representation of an exception object.

"),

("Text I/O","Base","dump","dump(x)

   Show all user-visible structure of a value.

"),

("Text I/O","Base","xdump","xdump(x)

   Show all structure of a value, including all fields of objects.

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

("Text I/O","Base","readdlm","readdlm(source, delim::Char; has_header=false, use_mmap=false, ignore_invalid_chars=false)

   Read a matrix from the source where each line gives one row, with
   elements separated by the given delimeter. The source can be a text
   file, stream or byte array. Memory mapped filed can be used by
   passing the byte array representation of the mapped segment as
   source.

   If \"has_header\" is \"true\" the first row of data would be read
   as headers and the tuple \"(data_cells, header_cells)\" is returned
   instead of only \"data_cells\".

   If \"use_mmap\" is \"true\" the file specified by \"source\" is
   memory mapped for potential speedups.

   If \"ignore_invalid_chars\" is \"true\" bytes in \"source\" with
   invalid character encoding will be ignored. Otherwise an error is
   thrown indicating the offending character position.

   If all data is numeric, the result will be a numeric array. If some
   elements cannot be parsed as numbers, a cell array of numbers and
   strings is returned.

"),

("Text I/O","Base","readdlm","readdlm(source, delim::Char, T::Type; options...)

   Read a matrix from the source with a given element type. If \"T\"
   is a numeric type, the result is an array of that type, with any
   non-numeric elements as \"NaN\" for floating-point types, or zero.
   Other useful values of \"T\" include \"ASCIIString\", \"String\",
   and \"Any\".

"),

("Text I/O","Base","writedlm","writedlm(filename, array, delim::Char)

   Write an array to a text file using the given delimeter (defaults
   to comma).

"),

("Text I/O","Base","readcsv","readcsv(source, [T::Type]; options...)

   Equivalent to \"readdlm\" with \"delim\" set to comma.

"),

("Text I/O","Base","writecsv","writecsv(filename, array)

   Equivalent to \"writedlm\" with \"delim\" set to comma.

"),

("Text I/O","Base","Base64Pipe","Base64Pipe(ostream)

   Returns a new write-only I/O stream, which converts any bytes
   written to it into base64-encoded ASCII bytes written to
   \"ostream\".  Calling \"close\" on the \"Base64Pipe\" stream is
   necessary to complete the encoding (but does not close
   \"ostream\").

"),

("Text I/O","Base","base64","base64(writefunc, args...)
base64(args...)

   Given a \"write\"-like function \"writefunc\", which takes an I/O
   stream as its first argument, \"base64(writefunc, args...)\" calls
   \"writefunc\" to write \"args...\" to a base64-encoded string, and
   returns the string.  \"base64(args...)\" is equivalent to
   \"base64(write, args...)\": it converts its arguments into bytes
   using the standard \"write\" functions and returns the
   base64-encoded string.

"),

("Multimedia I/O","Base","display","display(x)
display(d::Display, x)
display(mime, x)
display(d::Display, mime, x)

   Display \"x\" using the topmost applicable display in the display
   stack, typically using the richest supported multimedia output for
   \"x\", with plain-text \"STDOUT\" output as a fallback.  The
   \"display(d, x)\" variant attempts to display \"x\" on the given
   display \"d\" only, throwing a \"MethodError\" if \"d\" cannot
   display objects of this type.

   There are also two variants with a \"mime\" argument (a MIME type
   string, such as \"\"image/png\"\"), which attempt to display \"x\"
   using the requesed MIME type *only*, throwing a \"MethodError\" if
   this type is not supported by either the display(s) or by \"x\".
   With these variants, one can also supply the \"raw\" data in the
   requested MIME type by passing \"x::String\" (for MIME types with
   text-based storage, such as text/html or application/postscript) or
   \"x::Vector{Uint8}\" (for binary MIME types).

"),

("Multimedia I/O","Base","redisplay","redisplay(x)
redisplay(d::Display, x)
redisplay(mime, x)
redisplay(d::Display, mime, x)

   By default, the \"redisplay\" functions simply call \"display\".
   However, some display backends may override \"redisplay\" to modify
   an existing display of \"x\" (if any).   Using \"redisplay\" is
   also a hint to the backend that \"x\" may be redisplayed several
   times, and the backend may choose to defer the display until (for
   example) the next interactive prompt.

"),

("Multimedia I/O","Base","displayable","displayable(mime)
displayable(d::Display, mime)

   Returns a boolean value indicating whether the given \"mime\" type
   (string) is displayable by any of the displays in the current
   display stack, or specifically by the display \"d\" in the second
   variant.

"),

("Multimedia I/O","Base","writemime","writemime(stream, mime, x)

   The \"display\" functions ultimately call \"writemime\" in order to
   write an object \"x\" as a given \"mime\" type to a given I/O
   \"stream\" (usually a memory buffer), if possible.  In order to
   provide a rich multimedia representation of a user-defined type
   \"T\", it is only necessary to define a new \"writemime\" method
   for \"T\", via: \"writemime(stream, ::MIME\"mime\", x::T) = ...\",
   where \"mime\" is a MIME-type string and the function body calls
   \"write\" (or similar) to write that representation of \"x\" to
   \"stream\". (Note that the \"MIME\"\"\" notation only supports
   literal strings; to construct \"MIME\" types in a more flexible
   manner use \"MIME{symbol(\"\")}\".)

   For example, if you define a \"MyImage\" type and know how to write
   it to a PNG file, you could define a function \"writemime(stream,
   ::MIME\"image/png\", x::MyImage) = ...`\" to allow your images to
   be displayed on any PNG-capable \"Display\" (such as IJulia). As
   usual, be sure to \"import Base.writemime\" in order to add new
   methods to the built-in Julia function \"writemime\".

   Technically, the \"MIME\"mime\"\" macro defines a singleton type
   for the given \"mime\" string, which allows us to exploit Julia's
   dispatch mechanisms in determining how to display objects of any
   given type.

"),

("Multimedia I/O","Base","mimewritable","mimewritable(mime, x)

   Returns a boolean value indicating whether or not the object \"x\"
   can be written as the given \"mime\" type.  (By default, this is
   determined automatically by the existence of the corresponding
   \"writemime\" function for \"typeof(x)\".)

"),

("Multimedia I/O","Base","reprmime","reprmime(mime, x)

   Returns a \"String\" or \"Vector{Uint8}\" containing the
   representation of \"x\" in the requested \"mime\" type, as written
   by \"writemime\" (throwing a \"MethodError\" if no appropriate
   \"writemime\" is available).  A \"String\" is returned for MIME
   types with textual representations (such as \"\"text/html\"\" or
   \"\"application/postscript\"\"), whereas binary data is returned as
   \"Vector{Uint8}\".  (The function \"istext(mime)\" returns whether
   or not Julia treats a given \"mime\" type as text.)

   As a special case, if \"x\" is a \"String\" (for textual MIME
   types) or a \"Vector{Uint8}\" (for binary MIME types), the
   \"reprmime\" function assumes that \"x\" is already in the
   requested \"mime\" format and simply returns \"x\".

"),

("Multimedia I/O","Base","stringmime","stringmime(mime, x)

   Returns a \"String\" containing the representation of \"x\" in the
   requested \"mime\" type.  This is similar to \"reprmime\" except
   that binary data is base64-encoded as an ASCII string.

"),

("Multimedia I/O","Base","pushdisplay","pushdisplay(d::Display)

   Pushes a new display \"d\" on top of the global display-backend
   stack.  Calling \"display(x)\" or \"display(mime, x)\" will display
   \"x\" on the topmost compatible backend in the stack (i.e., the
   topmost backend that does not throw a \"MethodError\").

"),

("Multimedia I/O","Base","popdisplay","popdisplay()
popdisplay(d::Display)

   Pop the topmost backend off of the display-backend stack, or the
   topmost copy of \"d\" in the second variant.

"),

("Multimedia I/O","Base","TextDisplay","TextDisplay(stream)

   Returns a \"TextDisplay <: Display\", which can display any object
   as the text/plain MIME type (only), writing the text representation
   to the given I/O stream.  (The text representation is the same as
   the way an object is printed in the Julia REPL.)

"),

("Multimedia I/O","Base","istext","istext(m::MIME)

   Determine whether a MIME type is text data.

"),

("Memory-mapped I/O","Base","mmap_array","mmap_array(type, dims, stream[, offset])

   Create an \"Array\" whose values are linked to a file, using
   memory-mapping. This provides a convenient way of working with data
   too large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted.
   Note that the file must be stored in binary format, and no format
   conversions are possible (this is a limitation of operating
   systems, not Julia).

   dims is a tuple specifying the size of the array.

   The file is passed via the stream argument.  When you initialize
   the stream, use \"\"r\"\" for a \"read-only\" array, and \"\"w+\"\"
   to create a new array used to write values to disk.

   Optionally, you can specify an offset (in bytes) if, for example,
   you want to skip over a header in the file. The default value for
   the offset is the current stream position.

   **Example**:

      # Create a file for mmapping
      # (you could alternatively use mmap_array to do this step, too)
      A = rand(1:20, 5, 30)
      s = open(\"/tmp/mmap.bin\", \"w+\")
      # We'll write the dimensions of the array as the first two Ints in the file
      write(s, size(A,1))
      write(s, size(A,2))
      # Now write the data
      write(s, A)
      close(s)

      # Test by reading it back in
      s = open(\"/tmp/mmap.bin\")   # default is read-only
      m = read(s, Int)
      n = read(s, Int)
      A2 = mmap_array(Int, (m,n), s)

   This would create a m-by-n \"Matrix{Int}\", linked to the file
   associated with stream \"s\".

   A more portable file would need to encode the word size---32 bit or
   64 bit---and endianness information in the header. In practice,
   consider encoding binary data using standard formats like HDF5
   (which can be used with memory-mapping).

"),

("Memory-mapped I/O","Base","mmap_bitarray","mmap_bitarray([type], dims, stream[, offset])

   Create a \"BitArray\" whose values are linked to a file, using
   memory-mapping; it has the same purpose, works in the same way, and
   has the same arguments, as \"mmap_array()\", but the byte
   representation is different. The \"type\" parameter is optional,
   and must be \"Bool\" if given.

   **Example**:  \"B = mmap_bitarray((25,30000), s)\"

   This would create a 25-by-30000 \"BitArray\", linked to the file
   associated with stream \"s\".

"),

("Memory-mapped I/O","Base","msync","msync(array)

   Forces synchronization between the in-memory version of a memory-
   mapped \"Array\" or \"BitArray\" and the on-disk version.

"),

("Memory-mapped I/O","Base","msync","msync(ptr, len[, flags])

   Forces synchronization of the mmap'd memory region from ptr to
   ptr+len. Flags defaults to MS_SYNC, but can be a combination of
   MS_ASYNC, MS_SYNC, or MS_INVALIDATE. See your platform man page for
   specifics. The flags argument is not valid on Windows.

   You may not need to call \"msync\", because synchronization is
   performed at intervals automatically by the operating system.
   However, you can call this directly if, for example, you are
   concerned about losing the result of a long-running calculation.

"),

("Memory-mapped I/O","Base","MS_ASYNC","MS_ASYNC

   Enum constant for msync. See your platform man page for details.
   (not available on Windows).

"),

("Memory-mapped I/O","Base","MS_SYNC","MS_SYNC

   Enum constant for msync. See your platform man page for details.
   (not available on Windows).

"),

("Memory-mapped I/O","Base","MS_INVALIDATE","MS_INVALIDATE

   Enum constant for msync. See your platform man page for details.
   (not available on Windows).

"),

("Memory-mapped I/O","Base","mmap","mmap(len, prot, flags, fd, offset)

   Low-level interface to the mmap system call. See the man page.

"),

("Memory-mapped I/O","Base","munmap","munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With
   mmap_array you do not need to call this directly; the memory is
   unmapped for you when the array goes out of scope.

"),

("Mathematical Operators","Base","-","-(x)

   Unary minus operator.

"),

("Mathematical Operators","Base","+","+(x, y)

   Binary addition operator.

"),

("Mathematical Operators","Base","-","-(x, y)

   Binary subtraction operator.

"),

("Mathematical Operators","Base","*","*(x, y)

   Binary multiplication operator.

"),

("Mathematical Operators","Base","/","/(x, y)

   Binary left-division operator.

"),

("Mathematical Operators","Base","\\","\\(x, y)

   Binary right-division operator.

"),

("Mathematical Operators","Base","^","^(x, y)

   Binary exponentiation operator.

"),

("Mathematical Operators","Base",".+",".+(x, y)

   Element-wise binary addition operator.

"),

("Mathematical Operators","Base",".-",".-(x, y)

   Element-wise binary subtraction operator.

"),

("Mathematical Operators","Base",".*",".*(x, y)

   Element-wise binary multiplication operator.

"),

("Mathematical Operators","Base","./","./(x, y)

   Element-wise binary left division operator.

"),

("Mathematical Operators","Base",".\\",".\\(x, y)

   Element-wise binary right division operator.

"),

("Mathematical Operators","Base",".^",".^(x, y)

   Element-wise binary exponentiation operator.

"),

("Mathematical Operators","Base","div","div(a, b)

   Compute a/b, truncating to an integer

"),

("Mathematical Operators","Base","fld","fld(a, b)

   Largest integer less than or equal to a/b

"),

("Mathematical Operators","Base","mod","mod(x, m)

   Modulus after division, returning in the range [0,m)

"),

("Mathematical Operators","Base","rem","rem(x, m)

   Remainder after division

"),

("Mathematical Operators","Base","divrem","divrem(x, y)

   Compute \"x/y\" and \"x%y\" at the same time

"),

("Mathematical Operators","Base","%","%(x, m)

   Remainder after division. The operator form of \"rem\".

"),

("Mathematical Operators","Base","mod1","mod1(x, m)

   Modulus after division, returning in the range (0,m]

"),

("Mathematical Operators","Base","rem1","rem1(x, m)

   Remainder after division, returning in the range (0,m]

"),

("Mathematical Operators","Base","//","//(num, den)

   Rational division

"),

("Mathematical Operators","Base","rationalize","rationalize([Type], x)

   Approximate the number x as a rational fraction

"),

("Mathematical Operators","Base","num","num(x)

   Numerator of the rational representation of \"x\"

"),

("Mathematical Operators","Base","den","den(x)

   Denominator of the rational representation of \"x\"

"),

("Mathematical Operators","Base","<<","<<(x, n)

   Left shift operator.

"),

("Mathematical Operators","Base",">>",">>(x, n)

   Right shift operator.

"),

("Mathematical Operators","Base",">>>",">>>(x, n)

   Unsigned right shift operator.

"),

("Mathematical Operators","Base",":",":(start[, step], stop)

   Range operator. \"a:b\" constructs a range from \"a\" to \"b\" with
   a step size of 1, and \"a:s:b\" is similar but uses a step size of
   \"s\". These syntaxes call the function \"colon\". The colon is
   also used in indexing to select whole dimensions.

"),

("Mathematical Operators","Base","colon","colon(start[, step], stop)

   Called by \":\" syntax for constructing ranges.

"),

("Mathematical Operators","Base","==","==(x, y)

   Numeric equality operator. Compares numbers and number-like values
   (e.g. arrays) by numeric value. True for numbers of different types
   that represent the same value (e.g. \"2\" and \"2.0\"). Follows
   IEEE semantics for floating-point numbers. New numeric types should
   implement this function for two arguments of the new type.

"),

("Mathematical Operators","Base","!=","!=(x, y)

   Not-equals comparison operator. Always gives the opposite answer as
   \"==\". New types should generally not implement this, and rely on
   the fallback definition \"!=(x,y) = !(x==y)\" instead.

"),

("Mathematical Operators","Base","===","===(x, y)

   See the \"is()\" operator

"),

("Mathematical Operators","Base","!==","!==(x, y)

   Equivalent to \"!is(x, y)\"

"),

("Mathematical Operators","Base","<","<(x, y)

   Less-than comparison operator. New numeric types should implement
   this function for two arguments of the new type.

"),

("Mathematical Operators","Base","<=","<=(x, y)

   Less-than-or-equals comparison operator.

"),

("Mathematical Operators","Base",">",">(x, y)

   Greater-than comparison operator. Generally, new types should
   implement \"<\" instead of this function, and rely on the fallback
   definition \">(x,y) = y<x\".

"),

("Mathematical Operators","Base",">=",">=(x, y)

   Greater-than-or-equals comparison operator.

"),

("Mathematical Operators","Base",".==",".==(x, y)

   Element-wise equality comparison operator.

"),

("Mathematical Operators","Base",".!=",".!=(x, y)

   Element-wise not-equals comparison operator.

"),

("Mathematical Operators","Base",".<",".<(x, y)

   Element-wise less-than comparison operator.

"),

("Mathematical Operators","Base",".<=",".<=(x, y)

   Element-wise less-than-or-equals comparison operator.

"),

("Mathematical Operators","Base",".>",".>(x, y)

   Element-wise greater-than comparison operator.

"),

("Mathematical Operators","Base",".>=",".>=(x, y)

   Element-wise greater-than-or-equals comparison operator.

"),

("Mathematical Operators","Base","cmp","cmp(x, y)

   Return -1, 0, or 1 depending on whether \"x<y\", \"x==y\", or
   \"x>y\", respectively.

"),

("Mathematical Operators","Base","~","~(x)

   Bitwise not

"),

("Mathematical Operators","Base","&","&(x, y)

   Bitwise and

"),

("Mathematical Operators","Base","|","|(x, y)

   Bitwise or

"),

("Mathematical Operators","Base","\$","\$(x, y)

   Bitwise exclusive or

"),

("Mathematical Operators","Base","!","!(x)

   Boolean not

"),

("Mathematical Operators","Base","&&","&&(x, y)

   Boolean and

"),

("Mathematical Operators","Base","||","||(x, y)

   Boolean or

"),

("Mathematical Operators","Base","A_ldiv_Bc","A_ldiv_Bc(a, b)

   Matrix operator A \\ B^H

"),

("Mathematical Operators","Base","A_ldiv_Bt","A_ldiv_Bt(a, b)

   Matrix operator A \\ B^T

"),

("Mathematical Operators","Base","A_mul_B","A_mul_B(...)

   Matrix operator A B

"),

("Mathematical Operators","Base","A_mul_Bc","A_mul_Bc(...)

   Matrix operator A B^H

"),

("Mathematical Operators","Base","A_mul_Bt","A_mul_Bt(...)

   Matrix operator A B^T

"),

("Mathematical Operators","Base","A_rdiv_Bc","A_rdiv_Bc(...)

   Matrix operator A / B^H

"),

("Mathematical Operators","Base","A_rdiv_Bt","A_rdiv_Bt(a, b)

   Matrix operator A / B^T

"),

("Mathematical Operators","Base","Ac_ldiv_B","Ac_ldiv_B(...)

   Matrix operator A^H \\ B

"),

("Mathematical Operators","Base","Ac_ldiv_Bc","Ac_ldiv_Bc(...)

   Matrix operator A^H \\ B^H

"),

("Mathematical Operators","Base","Ac_mul_B","Ac_mul_B(...)

   Matrix operator A^H B

"),

("Mathematical Operators","Base","Ac_mul_Bc","Ac_mul_Bc(...)

   Matrix operator A^H B^H

"),

("Mathematical Operators","Base","Ac_rdiv_B","Ac_rdiv_B(a, b)

   Matrix operator A^H / B

"),

("Mathematical Operators","Base","Ac_rdiv_Bc","Ac_rdiv_Bc(a, b)

   Matrix operator A^H / B^H

"),

("Mathematical Operators","Base","At_ldiv_B","At_ldiv_B(...)

   Matrix operator A^T \\ B

"),

("Mathematical Operators","Base","At_ldiv_Bt","At_ldiv_Bt(...)

   Matrix operator A^T \\ B^T

"),

("Mathematical Operators","Base","At_mul_B","At_mul_B(...)

   Matrix operator A^T B

"),

("Mathematical Operators","Base","At_mul_Bt","At_mul_Bt(...)

   Matrix operator A^T B^T

"),

("Mathematical Operators","Base","At_rdiv_B","At_rdiv_B(a, b)

   Matrix operator A^T / B

"),

("Mathematical Operators","Base","At_rdiv_Bt","At_rdiv_Bt(a, b)

   Matrix operator A^T / B^T

"),

("Mathematical Functions","Base","isapprox","isapprox(x::Number, y::Number; rtol::Real=cbrt(maxeps), atol::Real=sqrt(maxeps))

   Inexact equality comparison - behaves slightly different depending
   on types of input args:

   * For \"FloatingPoint\" numbers, \"isapprox\" returns \"true\" if
     \"abs(x-y) <= atol + rtol*max(abs(x), abs(y))\".

   * For \"Integer\" and \"Rational\" numbers, \"isapprox\" returns
     \"true\" if \"abs(x-y) <= atol\". The *rtol* argument is ignored.
     If one of \"x\" and \"y\" is \"FloatingPoint\", the other is
     promoted, and the method above is called instead.

   * For \"Complex\" numbers, the distance in the complex plane is
     compared, using the same criterion as above.

   For default tolerance arguments, \"maxeps = max(eps(abs(x)),
   eps(abs(y)))\".

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

("Mathematical Functions","Base","sinpi","sinpi(x)

   Compute \\sin(\\pi x) more accurately than \"sin(pi*x)\",
   especially for large \"x\".

"),

("Mathematical Functions","Base","cospi","cospi(x)

   Compute \\cos(\\pi x) more accurately than \"cos(pi*x)\",
   especially for large \"x\".

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

   Compute the inverse hyperbolic tangent of \"x\"

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

   Compute \\sin(\\pi x) / (\\pi x) if x \\neq 0, and 1 if x = 0.

"),

("Mathematical Functions","Base","cosc","cosc(x)

   Compute \\cos(\\pi x) / x - \\sin(\\pi x) / (\\pi x^2) if x \\neq
   0, and 0 if x = 0. This is the derivative of \"sinc(x)\".

"),

("Mathematical Functions","Base","degrees2radians","degrees2radians(x)

   Convert \"x\" from degrees to radians

"),

("Mathematical Functions","Base","radians2degrees","radians2degrees(x)

   Convert \"x\" from radians to degrees

"),

("Mathematical Functions","Base","hypot","hypot(x, y)

   Compute the \\sqrt{x^2+y^2} avoiding overflow and underflow

"),

("Mathematical Functions","Base","log","log(x)

   Compute the natural logarithm of \"x\". Throws \"DomainError\" for
   negative \"Real\" arguments. Use complex negative arguments
   instead.

"),

("Mathematical Functions","Base","log2","log2(x)

   Compute the logarithm of \"x\" to base 2. Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Mathematical Functions","Base","log10","log10(x)

   Compute the logarithm of \"x\" to base 10. Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Mathematical Functions","Base","log1p","log1p(x)

   Accurate natural logarithm of \"1+x\".  Throws \"DomainError\" for
   \"Real\" arguments less than -1.

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

("Mathematical Functions","Base","exp10","exp10(x)

   Compute 10^x

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

("Mathematical Functions","Base","round","round(x[, digits[, base]])

   \"round(x)\" returns the nearest integral value of the same type as
   \"x\" to \"x\". \"round(x, digits)\" rounds to the specified number
   of digits after the decimal place, or before if negative, e.g.,
   \"round(pi,2)\" is \"3.14\". \"round(x, digits, base)\" rounds
   using a different base, defaulting to 10, e.g., \"round(pi, 3, 2)\"
   is \"3.125\".

"),

("Mathematical Functions","Base","ceil","ceil(x[, digits[, base]])

   Returns the nearest integral value of the same type as \"x\" not
   less than \"x\". \"digits\" and \"base\" work as above.

"),

("Mathematical Functions","Base","floor","floor(x[, digits[, base]])

   Returns the nearest integral value of the same type as \"x\" not
   greater than \"x\". \"digits\" and \"base\" work as above.

"),

("Mathematical Functions","Base","trunc","trunc(x[, digits[, base]])

   Returns the nearest integral value of the same type as \"x\" not
   greater in magnitude than \"x\". \"digits\" and \"base\" work as
   above.

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

("Mathematical Functions","Base","signif","signif(x, digits[, base])

   Rounds (in the sense of \"round\") \"x\" so that there are
   \"digits\" significant digits, under a base \"base\"
   representation, default 10. E.g., \"signif(123.456, 2)\" is
   \"120.0\", and \"signif(357.913, 4, 2)\" is \"352.0\".

"),

("Mathematical Functions","Base","min","min(x, y, ...)

   Return the minimum of the arguments. Operates elementwise over
   arrays.

"),

("Mathematical Functions","Base","max","max(x, y, ...)

   Return the maximum of the arguments. Operates elementwise over
   arrays.

"),

("Mathematical Functions","Base","clamp","clamp(x, lo, hi)

   Return x if \"lo <= x <= hi\". If \"x < lo\", return \"lo\". If \"x
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

   Return \\sqrt{x}. Throws \"DomainError\" for negative \"Real\"
   arguments. Use complex negative arguments instead.

"),

("Mathematical Functions","Base","isqrt","isqrt(x)

   Integer square root.

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

("Mathematical Functions","Base","erfinv","erfinv(x)

   Compute the inverse error function of a real \"x\", defined by
   \\operatorname{erf}(\\operatorname{erfinv}(x)) = x.

"),

("Mathematical Functions","Base","erfcinv","erfcinv(x)

   Compute the inverse error complementary function of a real \"x\",
   defined by \\operatorname{erfc}(\\operatorname{erfcinv}(x)) = x.

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

   Greatest common (positive) divisor (or zero if x and y are both zero).

"),

("Mathematical Functions","Base","lcm","lcm(x, y)

   Least common (non-negative) multiple.

"),

("Mathematical Functions","Base","gcdx","gcdx(x, y)

   Greatest common (positive) divisor, also returning integer coefficients \"u\"
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

("Mathematical Functions","Base","nextprod","nextprod([k_1, k_2, ...], n)

   Next integer not less than \"n\" that can be written as \\prod
   k_i^{p_i} for integers p_1, p_2, etc.

"),

("Mathematical Functions","Base","prevprod","prevprod([k_1, k_2, ...], n)

   Previous integer not greater than \"n\" that can be written as
   \\prod k_i^{p_i} for integers p_1, p_2, etc.

"),

("Mathematical Functions","Base","invmod","invmod(x, m)

   Take the inverse of \"x\" modulo \"m\": *y* such that xy = 1 \\pmod
   m

"),

("Mathematical Functions","Base","powermod","powermod(x, p, m)

   Compute x^p \\pmod m

"),

("Mathematical Functions","Base","gamma","gamma(x)

   Compute the gamma function of \"x\"

"),

("Mathematical Functions","Base","lgamma","lgamma(x)

   Compute the logarithm of absolute value of \"gamma(x)\"

"),

("Mathematical Functions","Base","lfact","lfact(x)

   Compute the logarithmic factorial of \"x\"

"),

("Mathematical Functions","Base","digamma","digamma(x)

   Compute the digamma function of \"x\" (the logarithmic derivative
   of \"gamma(x)\")

"),

("Mathematical Functions","Base","invdigamma","invdigamma(x)

   Compute the inverse digamma function of \"x\".

"),

("Mathematical Functions","Base","trigamma","trigamma(x)

   Compute the trigamma function of \"x\" (the logarithmic second
   derivative of \"gamma(x)\")

"),

("Mathematical Functions","Base","polygamma","polygamma(m, x)

   Compute the polygamma function of order \"m\" of argument \"x\"
   (the \"(m+1)th\" derivative of the logarithm of \"gamma(x)\")

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

("Mathematical Functions","Base","besselh","besselh(nu, k, x)

   Bessel function of the third kind of order \"nu\" (Hankel
   function). \"k\" is either 1 or 2, selecting \"hankelh1\" or
   \"hankelh2\", respectively.

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

   Natural logarithm of the absolute value of the beta function
   \\log(|\\operatorname{B}(x,y)|).

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

("Data Formats","Base","digits","digits(n[, base][, pad])

   Returns an array of the digits of \"n\" in the given base,
   optionally padded with zeros to a specified size. More significant
   digits are at higher indexes, such that \"n ==
   sum([digits[k]*base^(k-1) for k=1:length(digits)])\".

"),

("Data Formats","Base","bits","bits(n)

   A string giving the literal bit representation of a number.

"),

("Data Formats","Base","parseint","parseint([type], str[, base])

   Parse a string as an integer in the given base (default 10),
   yielding a number of the specified type (default \"Int\").

"),

("Data Formats","Base","parsefloat","parsefloat([type], str)

   Parse a string as a decimal floating point number, yielding a
   number of the specified type.

"),

("Data Formats","Base","big","big(x)

   Convert a number to a maximum precision representation (typically
   \"BigInt\" or \"BigFloat\"). See \"BigFloat\" for information about
   some pitfalls with floating-point numbers.

"),

("Data Formats","Base","bool","bool(x)

   Convert a number or numeric array to boolean

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

("Data Formats","Base","float16","float16(x)

   Convert a number or array to \"Float16\" data type

"),

("Data Formats","Base","float32","float32(x)

   Convert a number or array to \"Float32\" data type

"),

("Data Formats","Base","float64","float64(x)

   Convert a number or array to \"Float64\" data type

"),

("Data Formats","Base","float32_isvalid","float32_isvalid(x, out::Vector{Float32}) -> Bool

   Convert a number or array to \"Float32\" data type, returning true
   if successful. The result of the conversion is stored in
   \"out[1]\".

"),

("Data Formats","Base","float64_isvalid","float64_isvalid(x, out::Vector{Float64}) -> Bool

   Convert a number or array to \"Float64\" data type, returning true
   if successful. The result of the conversion is stored in
   \"out[1]\".

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

("Data Formats","Base","hex2bytes","hex2bytes(s::ASCIIString)

   Convert an arbitrarily long hexadecimal string to its binary
   representation. Returns an Array{Uint8, 1}, i.e. an array of bytes.

"),

("Data Formats","Base","bytes2hex","bytes2hex(bin_arr::Array{Uint8, 1})

   Convert an array of bytes to its hexadecimal representation. All
   characters are in lower-case. Returns an ASCIIString.

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

("Numbers","Base","catalan","catalan

   Catalan's constant

"),

("Numbers","Base","Inf","Inf

   Positive infinity of type Float64

"),

("Numbers","Base","Inf32","Inf32

   Positive infinity of type Float32

"),

("Numbers","Base","Inf16","Inf16

   Positive infinity of type Float16

"),

("Numbers","Base","NaN","NaN

   A not-a-number value of type Float64

"),

("Numbers","Base","NaN32","NaN32

   A not-a-number value of type Float32

"),

("Numbers","Base","NaN16","NaN16

   A not-a-number value of type Float16

"),

("Numbers","Base","issubnormal","issubnormal(f) -> Bool

   Test whether a floating point number is subnormal

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

("Numbers","Base","isinteger","isinteger(x)

   Test whether \"x\" or all its elements are numerically equal to
   some integer

"),

("Numbers","Base","isreal","isreal(x)

   Test whether \"x\" or all its elements are numerically equal to
   some real number

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
   are promoted to a \"BigFloat\". Note that because floating-point
   numbers are not exactly-representable in decimal notation,
   \"BigFloat(2.1)\" may not yield what you expect. You may prefer to
   initialize constants using strings, e.g., \"BigFloat(\"2.1\")\".

"),

("Numbers","Base","get_rounding","get_rounding()

   Get the current floating point rounding mode. Valid modes are
   \"RoundNearest\", \"RoundToZero\", \"RoundUp\" and \"RoundDown\".

"),

("Numbers","Base","set_rounding","set_rounding(mode)

   Set the floating point rounding mode. See \"get_rounding\" for
   available modes

"),

("Numbers","Base","with_rounding","with_rounding(f::Function, mode)

   Change the floating point rounding mode for the duration of \"f\".
   It is logically equivalent to:

      old = get_rounding()
      set_rounding(mode)
      f()
      set_rounding(old)

   See \"get_rounding\" for available rounding modes.

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

("Numbers","Base","primes","primes(n)

   Returns a collection of the prime numbers <= \"n\".

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

("BigFloats","Base","precision","precision(num::FloatingPoint)

   Get the precision of a floating point number, as defined by the
   effective number of bits in the mantissa.

"),

("BigFloats","Base","get_bigfloat_precision","get_bigfloat_precision()

   Get the precision (in bits) currently used for BigFloat arithmetic.

"),

("BigFloats","Base","set_bigfloat_precision","set_bigfloat_precision(x::Int64)

   Set the precision (in bits) to be used to BigFloat arithmetic.

"),

("BigFloats","Base","with_bigfloat_precision","with_bigfloat_precision(f::Function, precision::Integer)

   Change the BigFloat arithmetic precision (in bits) for the duration
   of \"f\". It is logically equivalent to:

      old = get_bigfloat_precision()
      set_bigfloat_precision(precision)
      f()
      set_bigfloat_precision(old)

"),

("BigFloats","Base","get_bigfloat_rounding","get_bigfloat_rounding()

   Get the current BigFloat rounding mode. Valid modes are
   \"RoundNearest\", \"RoundToZero\", \"RoundUp\", \"RoundDown\",
   \"RoundFromZero\"

"),

("BigFloats","Base","set_bigfloat_rounding","set_bigfloat_rounding(mode)

   Set the BigFloat rounding mode. See get_bigfloat_rounding for
   available modes

"),

("BigFloats","Base","with_bigfloat_rounding","with_bigfloat_rounding(f::Function, mode)

   Change the BigFloat rounding mode for the duration of \"f\". See
   \"get_bigfloat_rounding\" for available rounding modes; see also
   \"with_bigfloat_precision\".

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

("Random Numbers","Base","randn!","randn!(A::Array{Float64, N})

   Fill the array A with normally-distributed (mean 0, standard
   deviation 1) random numbers. Also see the rand function.

"),

("Random Numbers","Base","randsym","randsym(n)

   Generate a \"nxn\" symmetric array of normally-distributed random
   numbers with mean 0 and standard deviation 1.

"),

("Arrays","Base","ndims","ndims(A) -> Integer

   Returns the number of dimensions of A

"),

("Arrays","Base","size","size(A)

   Returns a tuple containing the dimensions of A

"),

("Arrays","Base","iseltype","iseltype(A, T)

   Tests whether A or its elements are of type T

"),

("Arrays","Base","length","length(A) -> Integer

   Returns the number of elements in A

"),

("Arrays","Base","nnz","nnz(A)

   Counts the number of nonzero values in array A (dense or sparse)

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

("Arrays","Base","ind2sub","ind2sub(dims, index) -> subscripts

   Returns a tuple of subscripts into an array with dimensions
   \"dims\", corresponding to the linear index \"index\"

   **Example** \"i, j, ... = ind2sub(size(A), indmax(A))\" provides
   the indices of the maximum element

"),

("Arrays","Base","sub2ind","sub2ind(dims, i, j, k...) -> index

   The inverse of \"ind2sub\", returns the linear index corresponding
   to the provided subscripts

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

("Arrays","Base","infs","infs(type, dims)

   Create an array where every element is infinite and of the
   specified type

"),

("Arrays","Base","nans","nans(type, dims)

   Create an array where every element is NaN of the specified type

"),

("Arrays","Base","trues","trues(dims)

   Create a \"BitArray\" with all values set to true

"),

("Arrays","Base","falses","falses(dims)

   Create a \"BitArray\" with all values set to false

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

   Change the type-interpretation of a block of memory. For example,
   \"reinterpret(Float32, uint32(7))\" interprets the 4 bytes
   corresponding to \"uint32(7)\" as a \"Float32\". For arrays, this
   constructs an array with the same binary data as the given array,
   but with the specified element type.

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

("Arrays","Base","broadcast","broadcast(f, As...)

   Broadcasts the arrays \"As\" to a common size by expanding
   singleton dimensions, and returns an array of the results
   \"f(as...)\" for each position.

"),

("Arrays","Base","broadcast!","broadcast!(f, dest, As...)

   Like \"broadcast\", but store the result of \"broadcast(f, As...)\"
   in the \"dest\" array. Note that \"dest\" is only used to store the
   result, and does not supply arguments to \"f\" unless it is also
   listed in the \"As\", as in \"broadcast!(f, A, A, B)\" to perform
   \"A[:] = broadcast(f, A, B)\".

"),

("Arrays","Base","broadcast_function","broadcast_function(f)

   Returns a function \"broadcast_f\" such that
   \"broadcast_function(f)(As...) === broadcast(f, As...)\". Most
   useful in the form \"const broadcast_f = broadcast_function(f)\".

"),

("Arrays","Base","broadcast!_function","broadcast!_function(f)

   Like \"broadcast_function\", but for \"broadcast!\".

"),

("Arrays","Base","getindex","getindex(A, inds...)

   Returns a subset of array \"A\" as specified by \"inds\", where
   each \"ind\" may be an \"Int\", a \"Range\", or a \"Vector\".

"),

("Arrays","Base","sub","sub(A, inds...)

   Returns a SubArray, which stores the input \"A\" and \"inds\"
   rather than computing the result immediately. Calling \"getindex\"
   on a SubArray computes the indices on the fly.

"),

("Arrays","Base","parent","parent(A)

   Returns the \"parent array\" of an array view type (e.g.,
   SubArray), or the array itself if it is not a view

"),

("Arrays","Base","parentindexes","parentindexes(A)

   From an array view \"A\", returns the corresponding indexes in the
   parent

"),

("Arrays","Base","slicedim","slicedim(A, d, i)

   Return all the data of \"A\" where the index for dimension \"d\"
   equals \"i\". Equivalent to \"A[:,:,...,i,:,:,...]\" where \"i\" is
   in position \"d\".

"),

("Arrays","Base","slice","slice(A, inds...)

   Create a view of the given indexes of array \"A\", dropping
   dimensions indexed with scalars.

"),

("Arrays","Base","setindex!","setindex!(A, X, inds...)

   Store values from array \"X\" within some subset of \"A\" as
   specified by \"inds\".

"),

("Arrays","Base","broadcast_getindex","broadcast_getindex(A, inds...)

   Broadcasts the \"inds\" arrays to a common size like \"broadcast\",
   and returns an array of the results \"A[ks...]\", where \"ks\" goes
   over the positions in the broadcast.

"),

("Arrays","Base","broadcast_setindex!","broadcast_setindex!(A, X, inds...)

   Broadcasts the \"X\" and \"inds\" arrays to a common size and
   stores the value from each position in \"X\" at the indices given
   by the same positions in \"inds\".

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

   If the first argument is a single integer \"n\", then all block
   rows are assumed to have \"n\" block columns.

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

("Arrays","Base","find","find(f, A)

   Return a vector of the linear indexes of  \"A\" where \"f\" returns
   true.

"),

("Arrays","Base","findn","findn(A)

   Return a vector of indexes for each dimension giving the locations
   of the non-zeros in \"A\".

"),

("Arrays","Base","findnz","findnz(A)

   Return a tuple \"(I, J, V)\" where \"I\" and \"J\" are the row and
   column indexes of the non-zero values in matrix \"A\", and \"V\" is
   a vector of the non-zero values.

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

("Arrays","Base","findnext","findnext(A, i)

   Find the next index >= \"i\" of a non-zero element of \"A\", or
   \"0\" if not found.

"),

("Arrays","Base","findnext","findnext(predicate, A, i)

   Find the next index >= \"i\" of an element of \"A\" satisfying the
   given predicate, or \"0\" if not found.

"),

("Arrays","Base","findnext","findnext(A, v, i)

   Find the next index >= \"i\" of an element of \"A\" equal to \"v\"
   (using \"==\"), or \"0\" if not found.

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

("Arrays","Base","promote_shape","promote_shape(s1, s2)

   Check two array shapes for compatibility, allowing trailing
   singleton dimensions, and return whichever shape has more
   dimensions.

"),

("Arrays","Base","checkbounds","checkbounds(array, indexes...)

   Throw an error if the specified indexes are not in bounds for the
   given array.

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

("Arrays","Base","gradient","gradient(F[, h])

   Compute differences along vector \"F\", using \"h\" as the spacing
   between points. The default spacing is one.

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

   The associativity of the reduction is implementation-dependent; if
   you need a particular associativity, e.g. left-to-right, you should
   write your own loop.

"),

("Arrays","Base","mapslices","mapslices(f, A, dims)

   Transform the given dimensions of array \"A\" using function \"f\".
   \"f\" is called on each slice of \"A\" of the form
   \"A[...,:,...,:,...]\". \"dims\" is an integer vector specifying
   where the colons go in this expression. The results are
   concatenated along the remaining dimensions. For example, if
   \"dims\" is \"[1,2]\" and A is 4-dimensional, \"f\" is called on
   \"A[:,:,i,j]\" for all \"i\" and \"j\".

"),

("Arrays","Base","sum_kbn","sum_kbn(A)

   Returns the sum of all array elements, using the Kahan-Babuska-
   Neumaier compensated summation algorithm for additional accuracy.

"),

("Arrays","Base","cartesianmap","cartesianmap(f, dims)

   Given a \"dims\" tuple of integers \"(m, n, ...)\", call \"f\" on
   all combinations of integers in the ranges \"1:m\", \"1:n\", etc.
   Example:

      julia> cartesianmap(println, (2,2))
      11
      21
      12
      22

"),

("Arrays","Base","bitpack","bitpack(A::AbstractArray{T, N}) -> BitArray

   Converts a numeric array to a packed boolean array

"),

("Arrays","Base","bitunpack","bitunpack(B::BitArray{N}) -> Array{Bool,N}

   Converts a packed boolean array to an array of booleans

"),

("Arrays","Base","flipbits!","flipbits!(B::BitArray{N}) -> BitArray{N}

   Performs a bitwise not operation on B. See *~ operator*.

"),

("Arrays","Base","rol","rol(B::BitArray{1}, i::Integer) -> BitArray{1}

   Left rotation operator.

"),

("Arrays","Base","ror","ror(B::BitArray{1}, i::Integer) -> BitArray{1}

   Right rotation operator.

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

   Return a randomly permuted copy of \"v\".

"),

("Combinatorics","Base","shuffle!","shuffle!(v)

   In-place version of \"shuffle()\".

"),

("Combinatorics","Base","reverse","reverse(v[, start=1[, stop=length(v)]])

   Return a copy of \"v\" reversed from start to stop.

"),

("Combinatorics","Base","reverse!","reverse!(v[, start=1[, stop=length(v)]]) -> v

   In-place version of \"reverse()\".

"),

("Combinatorics","Base","combinations","combinations(itr, n)

   Generate all combinations of \"n\" elements from a given iterable
   object.  Because the number of combinations can be very large, this
   function returns an iterator object. Use
   \"collect(combinations(a,n))\" to get an array of all combinations.

"),

("Combinatorics","Base","permutations","permutations(itr)

   Generate all permutations of a given iterable object.  Because the
   number of permutations can be very large, this function returns an
   iterator object. Use \"collect(permutations(a,n))\" to get an array
   of all permutations.

"),

("Combinatorics","Base","partitions","partitions(n)

   Generate all integer arrays that sum to \"n\". Because the number
   of partitions can be very large, this function returns an iterator
   object. Use \"collect(partitions(n))\" to get an array of all
   partitions. The number of partitions to generete can be efficiently
   computed using \"length(partitions(n))\".

"),

("Combinatorics","Base","partitions","partitions(n, m)

   Generate all arrays of \"m\" integers that sum to \"n\". Because
   the number of partitions can be very large, this function returns
   an iterator object. Use \"collect(partitions(n,m))\" to get an
   array of all partitions. The number of partitions to generete can
   be efficiently computed using \"length(partitions(n,m))\".

"),

("Combinatorics","Base","partitions","partitions(array)

   Generate all set partitions of the elements of an array,
   represented as arrays of arrays. Because the number of partitions
   can be very large, this function returns an iterator object. Use
   \"collect(partitions(array))\" to get an array of all partitions.
   The number of partitions to generete can be efficiently computed
   using \"length(partitions(array))\".

"),

("Statistics","Base","mean","mean(v[, region])

   Compute the mean of whole array \"v\", or optionally along the
   dimensions in \"region\". Note: Julia does not ignore \"NaN\"
   values in the computation. For applications requiring the handling
   of missing data, the \"DataArray\" package is recommended.

"),

("Statistics","Base","std","std(v[, region])

   Compute the sample standard deviation of a vector or array \"v\",
   optionally along dimensions in \"region\". The algorithm returns an
   estimator of the generative distribution's standard deviation under
   the assumption that each entry of \"v\" is an IID drawn from that
   generative distribution. This computation is equivalent to
   calculating \"sqrt(sum((v - mean(v)).^2) / (length(v) - 1))\".
   Note: Julia does not ignore \"NaN\" values in the computation. For
   applications requiring the handling of missing data, the
   \"DataArray\" package is recommended.

"),

("Statistics","Base","stdm","stdm(v, m)

   Compute the sample standard deviation of a vector \"v\" with known
   mean \"m\". Note: Julia does not ignore \"NaN\" values in the
   computation.

"),

("Statistics","Base","var","var(v[, region])

   Compute the sample variance of a vector or array \"v\", optionally
   along dimensions in \"region\". The algorithm will return an
   estimator of the generative distribution's variance under the
   assumption that each entry of \"v\" is an IID drawn from that
   generative distribution. This computation is equivalent to
   calculating \"sum((v - mean(v)).^2) / (length(v) - 1)\". Note:
   Julia does not ignore \"NaN\" values in the computation. For
   applications requiring the handling of missing data, the
   \"DataArray\" package is recommended.

"),

("Statistics","Base","varm","varm(v, m)

   Compute the sample variance of a vector \"v\" with known mean
   \"m\". Note: Julia does not ignore \"NaN\" values in the
   computation.

"),

("Statistics","Base","median","median(v; checknan::Bool=true)

   Compute the median of a vector \"v\". If keyword argument
   \"checknan\" is true (the default), an error is raised for data
   containing NaN values. Note: Julia does not ignore \"NaN\" values
   in the computation. For applications requiring the handling of
   missing data, the \"DataArray\" package is recommended.

"),

("Statistics","Base","median!","median!(v; checknan::Bool=true)

   Like \"median\", but may overwrite the input vector.

"),

("Statistics","Base","hist","hist(v[, n]) -> e, counts

   Compute the histogram of \"v\", optionally using approximately
   \"n\" bins. The return values are a range \"e\", which correspond
   to the edges of the bins, and \"counts\" containing the number of
   elements of \"v\" in each bin. Note: Julia does not ignore \"NaN\"
   values in the computation.

"),

("Statistics","Base","hist","hist(v, e) -> e, counts

   Compute the histogram of \"v\" using a vector/range \"e\" as the
   edges for the bins. The result will be a vector of length
   \"length(e) - 1\", such that the element at location \"i\"
   satisfies \"sum(e[i] .< v .<= e[i+1])\". Note: Julia does not
   ignore \"NaN\" values in the computation.

"),

("Statistics","Base","hist2d","hist2d(M, e1, e2) -> (edge1, edge2, counts)

   Compute a \"2d histogram\" of a set of N points specified by N-by-2
   matrix \"M\". Arguments \"e1\" and \"e2\" are bins for each
   dimension, specified either as integer bin counts or vectors of bin
   edges. The result is a tuple of \"edge1\" (the bin edges used in
   the first dimension), \"edge2\" (the bin edges used in the second
   dimension), and \"counts\", a histogram matrix of size
   \"(length(edge1)-1, length(edge2)-1)\". Note: Julia does not ignore
   \"NaN\" values in the computation.

"),

("Statistics","Base","histrange","histrange(v, n)

   Compute *nice* bin ranges for the edges of a histogram of \"v\",
   using approximately \"n\" bins. The resulting step sizes will be 1,
   2 or 5 multiplied by a power of 10. Note: Julia does not ignore
   \"NaN\" values in the computation.

"),

("Statistics","Base","midpoints","midpoints(e)

   Compute the midpoints of the bins with edges \"e\". The result is a
   vector/range of length \"length(e) - 1\". Note: Julia does not
   ignore \"NaN\" values in the computation.

"),

("Statistics","Base","quantile","quantile(v, p)

   Compute the quantiles of a vector \"v\" at a specified set of
   probability values \"p\". Note: Julia does not ignore \"NaN\"
   values in the computation.

"),

("Statistics","Base","quantile","quantile(v, p)

   Compute the quantile of a vector \"v\" at the probability \"p\".
   Note: Julia does not ignore \"NaN\" values in the computation.

"),

("Statistics","Base","quantile!","quantile!(v, p)

   Like \"quantile\", but overwrites the input vector.

"),

("Statistics","Base","cov","cov(v1[, v2])

   Compute the Pearson covariance between two vectors \"v1\" and
   \"v2\". If called with a single element \"v\", then computes
   covariance of columns of \"v\". Note: Julia does not ignore \"NaN\"
   values in the computation.

"),

("Statistics","Base","cor","cor(v1[, v2])

   Compute the Pearson correlation between two vectors \"v1\" and
   \"v2\". If called with a single element \"v\", then computes
   correlation of columns of \"v\". Note: Julia does not ignore
   \"NaN\" values in the computation.

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

("Signal Processing","Base","plan_brfft","plan_brfft(A, d[, dims[, flags[, timelimit]]])

   Pre-plan an optimized real-input unnormalized transform, similar to
   \"plan_rfft()\" except for \"brfft()\" instead of \"rfft()\". The
   first two arguments and the size of the transformed result, are the
   same as for \"brfft()\".

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

("Signal Processing","Base","conv2","conv2(u, v, A)

   2-D convolution of the matrix \"A\" with the 2-D separable kernel
   generated by the vectors \"u\" and \"v\".  Uses 2-D FFT algorithm

"),

("Signal Processing","Base","conv2","conv2(B, A)

   2-D convolution of the matrix \"B\" with the matrix \"A\".  Uses
   2-D FFT algorithm

"),

("Signal Processing","Base","xcorr","xcorr(u, v)

   Compute the cross-correlation of two vectors.

"),

("Signal Processing","Base.FFTW","r2r","r2r(A, kind[, dims])

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
   definitions of these transform types, at http://www.fftw.org/doc.

   The optional \"dims\" argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along. \"kind[i]\" is then the transform type for \"dims[i]\", with
   \"kind[end]\" being used for \"i > length(kind)\".

   See also \"plan_r2r()\" to pre-plan optimized r2r transforms.

"),

("Signal Processing","Base.FFTW","r2r!","r2r!(A, kind[, dims])

   Same as \"r2r()\", but operates in-place on \"A\", which must be an
   array of real or complex floating-point numbers.

"),

("Signal Processing","Base.FFTW","plan_r2r","plan_r2r(A, kind[, dims[, flags[, timelimit]]])

   Pre-plan an optimized r2r transform, similar to \"Base.plan_fft()\"
   except that the transforms (and the first three arguments)
   correspond to \"r2r()\" and \"r2r!()\", respectively.

"),

("Signal Processing","Base.FFTW","plan_r2r!","plan_r2r!(A, kind[, dims[, flags[, timelimit]]])

   Similar to \"Base.plan_fft()\", but corresponds to \"r2r!()\".

"),

("Numerical Integration","Base","quadgk","quadgk(f, a, b, c...; reltol=sqrt(eps), abstol=0, maxevals=10^7, order=7)

   Numerically integrate the function \"f(x)\" from \"a\" to \"b\",
   and optionally over additional intervals \"b\" to \"c\" and so on.
   Keyword options include a relative error tolerance \"reltol\"
   (defaults to \"sqrt(eps)\" in the precision of the endpoints), an
   absolute error tolerance \"abstol\" (defaults to 0), a maximum
   number of function evaluations \"maxevals\" (defaults to \"10^7\"),
   and the \"order\" of the integration rule (defaults to 7).

   Returns a pair \"(I,E)\" of the estimated integral \"I\" and an
   estimated upper bound on the absolute error \"E\".  If \"maxevals\"
   is not exceeded then either \"E <= abstol\" or \"E <=
   reltol*norm(I)\" will hold.  (Note that it is useful to specify a
   positive \"abstol\" in cases where \"norm(I)\" may be zero.)

   The endpoints \"a\" etcetera can also be complex (in which case the
   integral is performed over straight-line segments in the complex
   plane).  If the endpoints are \"BigFloat\", then the integration
   will be performed in \"BigFloat\" precision as well (note: it is
   advisable to increase the integration \"order\" in rough proportion
   to the precision, for smooth integrands).  More generally, the
   precision is set by the precision of the integration endpoints
   (promoted to floating-point types).

   The integrand \"f(x)\" can return any numeric scalar, vector, or
   matrix type, or in fact any type supporting \"+\", \"-\",
   multiplication by real values, and a \"norm\" (i.e., any normed
   vector space).

   The algorithm is an adaptive Gauss-Kronrod integration technique:
   the integral in each interval is estimated using a Kronrod rule
   (\"2*order+1\" points) and the error is estimated using an embedded
   Gauss rule (\"order\" points).   The interval with the largest
   error is then subdivided into two intervals and the process is
   repeated until the desired error tolerance is achieved.

   These quadrature rules work best for smooth functions within each
   interval, so if your function has a known discontinuity or other
   singularity, it is best to subdivide your interval to put the
   singularity at an endpoint.  For example, if \"f\" has a
   discontinuity at \"x=0.7\" and you want to integrate from 0 to 1,
   you should use \"quadgk(f, 0,0.7,1)\" to subdivide the interval at
   the point of discontinuity.  The integrand is never evaluated
   exactly at the endpoints of the intervals, so it is possible to
   integrate functions that diverge at the endpoints as long as the
   singularity is integrable (for example, a \"log(x)\" or
   \"1/sqrt(x)\" singularity).

   For real-valued endpoints, the starting and/or ending points may be
   infinite.  (A coordinate transformation is performed internally to
   map the infinite interval to a finite one.)

"),

("Parallel Computing","Base","addprocs","addprocs(n; cman::ClusterManager=LocalManager()) -> List of process identifiers

   \"addprocs(4)\" will add 4 processes on the local machine. This can
   be used to take advantage of multiple cores.

   Keyword argument \"cman\" can be used to provide a custom cluster
   manager to start workers. For example Beowulf clusters are
   supported via a custom cluster manager implemented in  package
   \"ClusterManagers\".

   See the documentation for package \"ClusterManagers\" for more
   information on how to write a custom cluster manager.

"),

("Parallel Computing","Base","addprocs","addprocs(machines; tunnel=false, dir=JULIA_HOME, sshflags::Cmd=``) -> List of process identifiers

   Add processes on remote machines via SSH. Requires julia to be
   installed in the same location on each node, or to be available via
   a shared file system.

   \"machines\" is a vector of host definitions of the form
   \"[user@]host[:port]\". A worker is started for each such
   definition.

   Keyword arguments:

   \"tunnel\" : if \"true\" then SSH tunneling will be used to connect
   to the worker.

   \"dir\" :  specifies the location of the julia binaries on the
   worker nodes.

   \"sshflags\" : specifies additional ssh options, e.g.
   \"sshflags=`-i /home/foo/bar.pem`\" .

"),

("Parallel Computing","Base","nprocs","nprocs()

   Get the number of available processors.

"),

("Parallel Computing","Base","nworkers","nworkers()

   Get the number of available worker processors. This is one less
   than nprocs(). Equal to nprocs() if nprocs() == 1.

"),

("Parallel Computing","Base","procs","procs()

   Returns a list of all process identifiers.

"),

("Parallel Computing","Base","workers","workers()

   Returns a list of all worker process identifiers.

"),

("Parallel Computing","Base","rmprocs","rmprocs(pids...)

   Removes the specified workers.

"),

("Parallel Computing","Base","interrupt","interrupt([pids...])

   Interrupt the current executing task on the specified workers. This
   is equivalent to pressing Ctrl-C on the local machine. If no
   arguments are given, all workers are interrupted.

"),

("Parallel Computing","Base","myid","myid()

   Get the id of the current processor.

"),

("Parallel Computing","Base","pmap","pmap(f, lsts...; err_retry=true, err_stop=false)

   Transform collections \"lsts\" by applying \"f\" to each element in
   parallel. If \"nprocs() > 1\", the calling process will be
   dedicated to assigning tasks. All other available processes will be
   used as parallel workers.

   If \"err_retry\" is true, it retries a failed application of \"f\"
   on a different worker. If \"err_stop\" is true, it takes precedence
   over the value of \"err_retry\" and \"pmap\" stops execution on the
   first error.

"),

("Parallel Computing","Base","remotecall","remotecall(id, func, args...)

   Call a function asynchronously on the given arguments on the
   specified processor. Returns a \"RemoteRef\".

"),

("Parallel Computing","Base","wait","wait(x)

   Block the current task until some event occurs, depending on the
   type of the argument:

   * \"RemoteRef\": Wait for a value to become available for the
     specified remote reference.

   * \"Condition\": Wait for \"notify\" on a condition.

   * \"Process\": Wait for a process or process chain to exit. The
     \"exitcode\" field of a process can be used to determine success
     or failure.

   * \"Task\": Wait for a \"Task\" to finish, returning its result
     value.

   * \"RawFD\": Wait for changes on a file descriptor (see *poll_fd*
     for keyword arguments and return code)

"),

("Parallel Computing","Base","fetch","fetch(RemoteRef)

   Wait for and get the value of a remote reference.

"),

("Parallel Computing","Base","remotecall_wait","remotecall_wait(id, func, args...)

   Perform \"wait(remotecall(...))\" in one message.

"),

("Parallel Computing","Base","remotecall_fetch","remotecall_fetch(id, func, args...)

   Perform \"fetch(remotecall(...))\" in one message.

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

("Parallel Computing","Base","isready","isready(RemoteRef)

   Determine whether a \"RemoteRef\" has a value stored to it. Note
   that this function can easily cause race conditions, since by the
   time you receive its result it may no longer be true. It is
   recommended that this function only be used on a \"RemoteRef\" that
   is assigned once.

"),

("Parallel Computing","Base","RemoteRef","RemoteRef()

   Make an uninitialized remote reference on the local machine.

"),

("Parallel Computing","Base","RemoteRef","RemoteRef(n)

   Make an uninitialized remote reference on processor \"n\".

"),

("Parallel Computing","Base","timedwait","timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

   Waits till \"testcb\" returns \"true\" or for \"secs`\" seconds,
   whichever is earlier. \"testcb\" is polled every \"pollint\"
   seconds.

"),

("Parallel Computing","Base","@spawn","@spawn()

   Execute an expression on an automatically-chosen processor,
   returning a \"RemoteRef\" to the result.

"),

("Parallel Computing","Base","@spawnat","@spawnat()

   Accepts two arguments, \"p\" and an expression, and runs the
   expression asynchronously on processor \"p\", returning a
   \"RemoteRef\" to the result.

"),

("Parallel Computing","Base","@fetch","@fetch()

   Equivalent to \"fetch(@spawn expr)\".

"),

("Parallel Computing","Base","@fetchfrom","@fetchfrom()

   Equivalent to \"fetch(@spawnat p expr)\".

"),

("Parallel Computing","Base","@async","@async()

   Schedule an expression to run on the local machine, also adding it
   to the set of items that the nearest enclosing \"@sync\" waits for.

"),

("Parallel Computing","Base","@sync","@sync()

   Wait until all dynamically-enclosed uses of \"@async\", \"@spawn\",
   and \"@spawnat\" complete.

"),

("Distributed Arrays","Base","DArray","DArray(init, dims[, procs, dist])

   Construct a distributed array. \"init\" is a function that accepts
   a tuple of index ranges. This function should allocate a local
   chunk of the distributed array and initialize it for the specified
   indices. \"dims\" is the overall size of the distributed array.
   \"procs\" optionally specifies a vector of processor IDs to use. If
   unspecified, the array is distributed over all worker processes
   only. Typically, when runnning in distributed mode, i.e.,
   \"nprocs() > 1\", this would mean that no chunk of the distributed
   array exists on the process hosting the interactive julia prompt.
   \"dist\" is an integer vector specifying how many chunks the
   distributed array should be divided into in each dimension.

   For example, the \"dfill\" function that creates a distributed
   array and fills it with a value \"v\" is implemented as:

   \"dfill(v, args...) = DArray(I->fill(v, map(length,I)), args...)\"

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

("Distributed Arrays","Base","localpart","localpart(d)

   Get the local piece of a distributed array. Returns an empty array
   if no local part exists on the calling process.

"),

("Distributed Arrays","Base","myindexes","myindexes(d)

   A tuple describing the indexes owned by the local processor.
   Returns a tuple with empty ranges if no local part exists on the
   calling process.

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

("System","Base","DevNull","DevNull

   Used in a stream redirect to discard all data written to it.
   Essentially equivalent to /dev/null on Unix or NUL on Windows.
   Usage: \"run(`cat test.txt` |> DevNull)\"

"),

("System","Base","success","success(command)

   Run a command object, constructed with backticks, and tell whether
   it was successful (exited with a code of 0). An exception is raised
   if the process cannot be started.

"),

("System","Base","process_running","process_running(p::Process)

   Determine whether a process is currently running.

"),

("System","Base","process_exited","process_exited(p::Process)

   Determine whether a process has exited.

"),

("System","Base","kill","kill(p::Process, signum=SIGTERM)

   Send a signal to a process. The default is to terminate the
   process.

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

("System","Base","ignorestatus","ignorestatus(command)

   Mark a command object so that running it will not throw an error if
   the result code is non-zero.

"),

("System","Base","detach","detach(command)

   Mark a command object so that it will be run in a new process
   group, allowing it to outlive the julia process, and not have
   Ctrl-C interrupts passed to it.

"),

("System","Base","setenv","setenv(command, env)

   Set environment variables to use when running the given command.
   \"env\" is either a dictionary mapping strings to strings, or an
   array of strings of the form \"\"var=val\"\".

"),

("System","Base","|>","|>(command, command)
|>(command, filename)
|>(filename, command)

   Redirect operator. Used for piping the output of a process into
   another (first form) or to redirect the standard output/input of a
   command to/from a file (second and third forms).

   **Examples**:
      * \"run(`ls` |> `grep xyz`)\"

      * \"run(`ls` |> \"out.txt\")\"

      * \"run(\"out.txt\" |> `grep xyz`)\"

"),

("System","Base",">>",">>(command, filename)

   Redirect standard output of a process, appending to the destination
   file.

"),

("System","Base",".>",".>(command, filename)

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

("System","Base","cd","cd(f[, dir])

   Temporarily changes the current working directory (HOME if not
   specified) and applies function f before returning.

"),

("System","Base","mkdir","mkdir(path[, mode])

   Make a new directory with name \"path\" and permissions \"mode\".
   \"mode\" defaults to 0o777, modified by the current file creation
   mask.

"),

("System","Base","mkpath","mkpath(path[, mode])

   Create all directories in the given \"path\", with permissions
   \"mode\". \"mode\" defaults to 0o777, modified by the current file
   creation mask.

"),

("System","Base","rmdir","rmdir(path)

   Remove the directory named \"path\".

"),

("System","Base","getpid","getpid() -> Int32

   Get julia's process ID.

"),

("System","Base","time","time([t::TmStruct])

   Get the system time in seconds since the epoch, with fairly high
   (typically, microsecond) resolution. When passed a \"TmStruct\",
   converts it to a number of seconds since the epoch.

"),

("System","Base","time_ns","time_ns()

   Get the time in nanoseconds. The time corresponding to 0 is
   undefined, and wraps every 5.8 years.

"),

("System","Base","strftime","strftime([format], time)

   Convert time, given as a number of seconds since the epoch or a
   \"TmStruct\", to a formatted string using the given format.
   Supported formats are the same as those in the standard C library.

"),

("System","Base","strptime","strptime([format], timestr)

   Parse a formatted time string into a \"TmStruct\" giving the
   seconds, minute, hour, date, etc. Supported formats are the same as
   those in the standard C library. On some platforms, timezones will
   not be parsed correctly. If the result of this function will be
   passed to \"time\" to convert it to seconds since the epoch, the
   \"isdst\" field should be filled in manually. Setting it to \"-1\"
   will tell the C library to use the current system settings to
   determine the timezone.

"),

("System","Base","TmStruct","TmStruct([seconds])

   Convert a number of seconds since the epoch to broken-down format,
   with fields \"sec\", \"min\", \"hour\", \"mday\", \"month\",
   \"year\", \"wday\", \"yday\", and \"isdst\".

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

("System","Base","@time","@time()

   A macro to execute and expression, printing time it took to execute
   and the total number of bytes its execution caused to be allocated,
   before returning the value of the expression.

"),

("System","Base","@elapsed","@elapsed()

   A macro to evaluate an expression, discarding the resulting value,
   instead returning the number of seconds it took to execute as a
   floating-point number.

"),

("System","Base","@allocated","@allocated()

   A macro to evaluate an expression, discarding the resulting value,
   instead returning the total number of bytes allocated during
   evaluation of the expression.

"),

("System","Base","EnvHash","EnvHash() -> EnvHash

   A singleton of this type provides a hash table interface to
   environment variables.

"),

("System","Base","ENV","ENV

   Reference to the singleton \"EnvHash\", providing a dictionary
   interface to system environment variables.

"),

("System","Base","@unix","@unix()

   Given \"@unix? a : b\", do \"a\" on Unix systems (including Linux
   and OS X) and \"b\" elsewhere. See documentation for Handling
   Platform Variations in the Calling C and Fortran Code section of
   the manual.

"),

("System","Base","@osx","@osx()

   Given \"@osx? a : b\", do \"a\" on OS X and \"b\" elsewhere. See
   documentation for Handling Platform Variations in the Calling C and
   Fortran Code section of the manual.

"),

("System","Base","@linux","@linux()

   Given \"@linux? a : b\", do \"a\" on Linux and \"b\" elsewhere. See
   documentation for Handling Platform Variations in the Calling C and
   Fortran Code section of the manual.

"),

("System","Base","@windows","@windows()

   Given \"@windows? a : b\", do \"a\" on Windows and \"b\" elsewhere.
   See documentation for Handling Platform Variations in the Calling C
   and Fortran Code section of the manual.

"),

("C Interface","Base","ccall","ccall((symbol, library) or fptr, RetType, (ArgType1, ...), ArgVar1, ...)

   Call function in C-exported shared library, specified by
   \"(function name, library)\" tuple, where each component is a
   String or :Symbol. Alternatively, ccall may be used to call a
   function pointer returned by dlsym, but note that this usage is
   generally discouraged to facilitate future static compilation. Note
   that the argument type tuple must be a literal tuple, and not a
   tuple-valued variable or expression.

"),

("C Interface","Base","cglobal","cglobal((symbol, library) or ptr[, Type=Void])

   Obtain a pointer to a global variable in a C-exported shared
   library, specified exactly as in \"ccall\".  Returns a
   \"Ptr{Type}\", defaulting to \"Ptr{Void}\" if no Type argument is
   supplied.  The values can be read or written by \"unsafe_load\" or
   \"unsafe_store!\", respectively.

"),

("C Interface","Base","cfunction","cfunction(fun::Function, RetType::Type, (ArgTypes...))

   Generate C-callable function pointer from Julia function. Type
   annotation of the return value in the callback function is a must
   for situations where Julia cannot infer the return type
   automatically.

   For example:

      function foo()
        # body

        retval::Float64
      end

      bar = cfunction(foo, Float64, ())

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

("C Interface","Base","dlopen_e","dlopen_e(libfile::String[, flags::Integer])

   Similar to \"dlopen\", except returns a NULL pointer instead of
   raising errors.

"),

("C Interface","Base","RTLD_DEEPBIND","RTLD_DEEPBIND

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

"),

("C Interface","Base","RTLD_FIRST","RTLD_FIRST

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

"),

("C Interface","Base","RTLD_GLOBAL","RTLD_GLOBAL

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

"),

("C Interface","Base","RTLD_LAZY","RTLD_LAZY

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

"),

("C Interface","Base","RTLD_LOCAL","RTLD_LOCAL

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

"),

("C Interface","Base","RTLD_NODELETE","RTLD_NODELETE

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

"),

("C Interface","Base","RTLD_NOLOAD","RTLD_NOLOAD

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

"),

("C Interface","Base","RTLD_NOW","RTLD_NOW

   Enum constant for dlopen. See your platform man page for details,
   if applicable.

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

("C Interface","Base","c_malloc","c_malloc(size::Integer)

   Call \"malloc\" from the C standard library.

"),

("C Interface","Base","c_free","c_free(addr::Ptr)

   Call \"free\" from the C standard library.

"),

("C Interface","Base","unsafe_load","unsafe_load(p::Ptr{T}, i::Integer)

   Dereference the pointer \"p[i]\" or \"*p\", returning a copy of
   type T.

"),

("C Interface","Base","unsafe_store!","unsafe_store!(p::Ptr{T}, x, i::Integer)

   Assign to the pointer \"p[i] = x\" or \"*p = x\", making a copy of
   object x into the memory at p.

"),

("C Interface","Base","unsafe_copy!","unsafe_copy!(dest::Ptr{T}, src::Ptr{T}, N)

   Copy \"N\" elements from a source pointer to a destination, with no
   checking. The size of an element is determined by the type of the
   pointers.

"),

("C Interface","Base","unsafe_copy!","unsafe_copy!(dest::Array, do, src::Array, so, N)

   Copy \"N\" elements from a source array to a destination, starting
   at offset \"so\" in the source and \"do\" in the destination.

"),

("C Interface","Base","copy!","copy!(dest, src)

   Copy all elements from collection \"src\" to array \"dest\".

"),

("C Interface","Base","copy!","copy!(dest, do, src, so, N)

   Copy \"N\" elements from collection \"src\" starting at offset
   \"so\", to array \"dest\" starting at offset \"do\".

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

("C Interface","Base","pointer_from_objref","pointer_from_objref(obj)

   Get the memory address of a Julia object as a \"Ptr\". The
   existence of the resulting \"Ptr\" will not protect the object from
   garbage collection, so you must ensure that the object remains
   referenced for the whole time that the \"Ptr\" will be used.

"),

("C Interface","Base","unsafe_pointer_to_objref","unsafe_pointer_to_objref(p::Ptr)

   Convert a \"Ptr\" to an object reference. Assumes the pointer
   refers to a valid heap-allocated Julia object. If this is not the
   case, undefined behavior results, hence this function is considered
   \"unsafe\" and should be used with care.

"),

("C Interface","Base","disable_sigint","disable_sigint(f::Function)

   Disable Ctrl-C handler during execution of a function, for calling
   external code that is not interrupt safe. Intended to be called
   using \"do\" block syntax as follows:

      disable_sigint() do
          # interrupt-unsafe code
          ...
      end

"),

("C Interface","Base","reenable_sigint","reenable_sigint(f::Function)

   Re-enable Ctrl-C handler during execution of a function.
   Temporarily reverses the effect of \"disable_sigint\".

"),

("C Interface","Base","find_library","find_library(names, locations)

   Searches for the first library in \"names\" in the paths in the
   \"locations\" list, \"DL_LOAD_PATH\", or system library paths (in
   that order) which can successfully be dlopen'd. On success, the
   return value will be one of the names (potentially prefixed by one
   of the paths in locations). This string can be assigned to a
   \"global const\" and used as the library name in future
   \"ccall\"'s. On failure, it returns the empty string.

"),

("C Interface","Base","DL_LOAD_PATH","DL_LOAD_PATH

   When calling \"dlopen\", the paths in this list will be searched
   first, in order, before searching the system locations for a valid
   library handle.

"),

("C Interface","Base","Cchar","Cchar

   Equivalent to the native \"char\" c-type

"),

("C Interface","Base","Cuchar","Cuchar

   Equivalent to the native \"unsigned char\" c-type (Uint8)

"),

("C Interface","Base","Cshort","Cshort

   Equivalent to the native \"signed short\" c-type (Int16)

"),

("C Interface","Base","Cushort","Cushort

   Equivalent to the native \"unsigned short\" c-type (Uint16)

"),

("C Interface","Base","Cint","Cint

   Equivalent to the native \"signed int\" c-type (Int32)

"),

("C Interface","Base","Cuint","Cuint

   Equivalent to the native \"unsigned int\" c-type (Uint32)

"),

("C Interface","Base","Clong","Clong

   Equivalent to the native \"signed long\" c-type

"),

("C Interface","Base","Culong","Culong

   Equivalent to the native \"unsigned long\" c-type

"),

("C Interface","Base","Clonglong","Clonglong

   Equivalent to the native \"signed long long\" c-type (Int64)

"),

("C Interface","Base","Culonglong","Culonglong

   Equivalent to the native \"unsigned long long\" c-type (Uint64)

"),

("C Interface","Base","Csize_t","Csize_t

   Equivalent to the native \"size_t\" c-type (Uint)

"),

("C Interface","Base","Cssize_t","Cssize_t

   Equivalent to the native \"ssize_t\" c-type

"),

("C Interface","Base","Cptrdiff_t","Cptrdiff_t

   Equivalent to the native \"ptrdiff_t\" c-type (Int)

"),

("C Interface","Base","Coff_t","Coff_t

   Equivalent to the native \"off_t\" c-type

"),

("C Interface","Base","Cwchar_t","Cwchar_t

   Equivalent to the native \"wchar_t\" c-type (Int32)

"),

("C Interface","Base","Cfloat","Cfloat

   Equivalent to the native \"float\" c-type (Float32)

"),

("C Interface","Base","Cdouble","Cdouble

   Equivalent to the native \"double\" c-type (Float64)

"),

("Errors","Base","error","error(message::String)

   Raise an error with the given message

"),

("Errors","Base","throw","throw(e)

   Throw an object as an exception

"),

("Errors","Base","rethrow","rethrow([e])

   Throw an object without changing the current exception backtrace.
   The default argument is the current exception (if called within a
   \"catch\" block).

"),

("Errors","Base","backtrace","backtrace()

   Get a backtrace object for the current program point.

"),

("Errors","Base","catch_backtrace","catch_backtrace()

   Get the backtrace of the current exception, for use within
   \"catch\" blocks.

"),

("Errors","Base","errno","errno()

   Get the value of the C library's \"errno\"

"),

("Errors","Base","systemerror","systemerror(sysfunc, iftrue)

   Raises a \"SystemError\" for \"errno\" with the descriptive string
   \"sysfunc\" if \"bool\" is true

"),

("Errors","Base","strerror","strerror(n)

   Convert a system call error code to a descriptive string

"),

("Errors","Base","assert","assert(cond[, text])

   Raise an error if \"cond\" is false. Also available as the macro
   \"@assert expr\".

"),

("Errors","Base","@assert","@assert()

   Raise an error if \"cond\" is false. Preferred syntax for writings
   assertions.

"),

("Errors","Base","ArgumentError","ArgumentError

   The parameters given to a function call are not valid.

"),

("Errors","Base","BoundsError","BoundsError

   An indexing operation into an array tried to access an out-of-
   bounds element.

"),

("Errors","Base","EOFError","EOFError

   No more data was available to read from a file or stream.

"),

("Errors","Base","ErrorException","ErrorException

   Generic error type. The error message, in the *.msg* field, may
   provide more specific details.

"),

("Errors","Base","KeyError","KeyError

   An indexing operation into an \"Associative\" (\"Dict\") or \"Set\"
   like object tried to access or delete a non-existent element.

"),

("Errors","Base","LoadError","LoadError

   An error occurred while *including*, *requiring*, or *using* a
   file. The error specifics should be available in the *.error*
   field.

"),

("Errors","Base","MethodError","MethodError

   A method with the required type signature does not exist in the
   given generic function.

"),

("Errors","Base","ParseError","ParseError

   The expression passed to the *parse* function could not be
   interpreted as a valid Julia expression.

"),

("Errors","Base","ProcessExitedException","ProcessExitedException

   After a client Julia process has exited, further attempts to
   reference the dead child will throw this exception.

"),

("Errors","Base","SystemError","SystemError

   A system call failed with an error code (in the \"errno\" global
   variable).

"),

("Errors","Base","TypeError","TypeError

   A type assertion failure, or calling an intrinsic function with an
   incorrect argument type.

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

("Tasks","Base","yield","yield()

   For scheduled tasks, switch back to the scheduler to allow another
   scheduled task to run. A task that calls this function is still
   runnable, and will be restarted immediately if there are no other
   runnable tasks.

"),

("Tasks","Base","task_local_storage","task_local_storage(symbol)

   Look up the value of a symbol in the current task's task-local
   storage.

"),

("Tasks","Base","task_local_storage","task_local_storage(symbol, value)

   Assign a value to a symbol in the current task's task-local
   storage.

"),

("Tasks","Base","task_local_storage","task_local_storage(body, symbol, value)

   Call the function \"body\" with a modified task-local storage, in
   which \"value\" is assigned to \"symbol\"; the previous value of
   \"symbol\", or lack thereof, is restored afterwards. Useful for
   emulating dynamic scoping.

"),

("Tasks","Base","Condition","Condition()

   Create an edge-triggered event source that tasks can wait for.
   Tasks that call \"wait\" on a \"Condition\" are suspended and
   queued. Tasks are woken up when \"notify\" is later called on the
   \"Condition\". Edge triggering means that only tasks waiting at the
   time \"notify\" is called can be woken up. For level-triggered
   notifications, you must keep extra state to keep track of whether a
   notification has happened. The \"RemoteRef\" type does this, and so
   can be used for level-triggered events.

"),

("Tasks","Base","notify","notify(condition, val=nothing; all=true, error=false)

   Wake up tasks waiting for a condition, passing them \"val\". If
   \"all\" is true (the default), all waiting tasks are woken,
   otherwise only one is. If \"error\" is true, the passed value is
   raised as an exception in the woken tasks.

"),

("Tasks","Base","schedule","schedule(t::Task)

   Add a task to the scheduler's queue. This causes the task to run
   constantly when the system is otherwise idle, unless the task
   performs a blocking operation such as \"wait\".

"),

("Tasks","Base","@schedule","@schedule()

   Wrap an expression in a Task and add it to the scheduler's queue.

"),

("Tasks","Base","@task","@task()

   Wrap an expression in a Task executing it, and return the Task.
   This only creates a task, and does not run it.

"),

("Tasks","Base","sleep","sleep(seconds)

   Block the current task for a specified number of seconds.

"),

("Events","Base","Timer","Timer(f::Function)

   Create a timer to call the given callback function. The callback is
   passed two arguments: the timer object itself, and a status code,
   which will be 0 unless an error occurs. The timer can be started
   and stopped with \"start_timer\" and \"stop_timer\".

"),

("Events","Base","start_timer","start_timer(t::Timer, delay, repeat)

   Start invoking the callback for a \"Timer\" after the specified
   initial delay, and then repeating with the given interval. Times
   are in seconds. If \"repeat\" is \"0\", the timer is only triggered
   once.

"),

("Events","Base","stop_timer","stop_timer(t::Timer)

   Stop invoking the callback for a timer.

"),

("Reflection","Base","module_name","module_name(m::Module) -> Symbol

   Get the name of a module as a symbol.

"),

("Reflection","Base","module_parent","module_parent(m::Module) -> Module

   Get a module's enclosing module. \"Main\" is its own parent.

"),

("Reflection","Base","current_module","current_module() -> Module

   Get the *dynamically* current module, which is the module code is
   currently being read from. In general, this is not the same as the
   module containing the call to this function.

"),

("Reflection","Base","fullname","fullname(m::Module)

   Get the fully-qualified name of a module as a tuple of symbols. For
   example, \"fullname(Base.Pkg)\" gives \"(:Base,:Pkg)\", and
   \"fullname(Main)\" gives \"()\".

"),

("Reflection","Base","names","names(x)

   Get an array of the names exported by a module, or the fields of a
   data type.

"),

("Reflection","Base","isconst","isconst([m::Module], s::Symbol) -> Bool

   Determine whether a global is declared \"const\" in a given module.

"),

("Reflection","Base","isgeneric","isgeneric(f::Function) -> Bool

   Determine whether a function is generic.

"),

("Reflection","Base","function_name","function_name(f::Function) -> Symbol

   Get the name of a generic function as a symbol, or \":anonymous\".

"),

("Reflection","Base","function_module","function_module(f::Function, types) -> Module

   Determine the module containing a given definition of a generic
   function.

"),

("Reflection","Base","functionloc","functionloc(f::Function, types)

   Returns a tuple \"(filename,line)\" giving the location of a method
   definition.

"),

("Reflection","Base","functionlocs","functionlocs(f::Function, types)

   Returns an array of the results of \"functionloc\" for all matching
   definitions.

"),

("Internals","Base","gc","gc()

   Perform garbage collection. This should not generally be used.

"),

("Internals","Base","gc_disable","gc_disable()

   Disable garbage collection. This should be used only with extreme
   caution, as it can cause memory use to grow without bound.

"),

("Internals","Base","gc_enable","gc_enable()

   Re-enable garbage collection after calling \"gc_disable\".

"),

("Internals","Base","macroexpand","macroexpand(x)

   Takes the expression x and returns an equivalent expression with
   all macros removed (expanded).

"),

("Internals","Base","expand","expand(x)

   Takes the expression x and returns an equivalent expression in
   lowered form

"),

("Internals","Base","code_lowered","code_lowered(f, types)

   Returns an array of lowered ASTs for the methods matching the given
   generic function and type signature.

"),

("Internals","Base","code_typed","code_typed(f, types)

   Returns an array of lowered and type-inferred ASTs for the methods
   matching the given generic function and type signature.

"),

("Internals","Base","code_llvm","code_llvm(f, types)

   Prints the LLVM bitcodes generated for running the method matching
   the given generic function and type signature to STDOUT.

"),

("Internals","Base","code_native","code_native(f, types)

   Prints the native assembly instructions generated for running the
   method matching the given generic function and type signature to
   STDOUT.

"),

("Internals","Base","precompile","precompile(f, args::(Any..., ))

   Compile the given function *f* for the argument tuple (of types)
   *args*, but do not execute it.

"),

("Collections and Data Structures","Base.Collections","PriorityQueue{K,V}","PriorityQueue{K,V}([ord])

   Construct a new PriorityQueue, with keys of type K and
   values/priorites of type V. If an order is not given, the priority
   queue is min-ordered using the default comparison for V.

"),

("Collections and Data Structures","Base.Collections","enqueue!","enqueue!(pq, k, v)

   Insert the a key \"k\" into a priority queue \"pq\" with priority
   \"v\".

"),

("Collections and Data Structures","Base.Collections","dequeue!","dequeue!(pq)

   Remove and return the lowest priority key from a priority queue.

"),

("Collections and Data Structures","Base.Collections","peek","peek(pq)

   Return the lowest priority key from a priority queue without
   removing that key from the queue.

"),

("Collections and Data Structures","Base.Collections","heapify","heapify(v[, ord])

   Return a new vector in binary heap order, optionally using the
   given ordering.

"),

("Collections and Data Structures","Base.Collections","heapify!","heapify!(v[, ord])

   In-place heapify.

"),

("Collections and Data Structures","Base.Collections","isheap","isheap(v[, ord])

   Return true iff an array is heap-ordered according to the given
   order.

"),

("Collections and Data Structures","Base.Collections","heappush!","heappush!(v[, ord])

   Given a binary heap-ordered array, push a new element, preserving
   the heap property. For efficiency, this function does not check
   that the array is indeed heap-ordered.

"),

("Collections and Data Structures","Base.Collections","heappop!","heappop!(v[, ord])

   Given a binary heap-ordered array, remove and return the lowest
   ordered element. For efficiency, this function does not check that
   the array is indeed heap-ordered.

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

("Filesystem","Base","iswritable","iswritable(path) -> Bool

   Returns \"true\" if the current user has permission to write to
   \"path\", \"false\" otherwise.

"),

("Filesystem","Base","homedir","homedir() -> String

   Return the current user's home directory.

"),

("Filesystem","Base","dirname","dirname(path::String) -> String

   Get the directory part of a path.

"),

("Filesystem","Base","basename","basename(path::String) -> String

   Get the file name part of a path.

"),

("Filesystem","Base","isabspath","isabspath(path::String) -> Bool

   Determines whether a path is absolute (begins at the root
   directory).

"),

("Filesystem","Base","isdirpath","isdirpath(path::String) -> Bool

   Determines whether a path refers to a directory (for example, ends
   with a path separator).

"),

("Filesystem","Base","joinpath","joinpath(parts...) -> String

   Join path components into a full path. If some argument is an
   absolute path, then prior components are dropped.

"),

("Filesystem","Base","abspath","abspath(path::String) -> String

   Convert a path to an absolute path by adding the current directory
   if necessary.

"),

("Filesystem","Base","normpath","normpath(path::String) -> String

   Normalize a path, removing \".\" and \"..\" entries.

"),

("Filesystem","Base","realpath","realpath(path::String) -> String

   Canonicalize a path by expanding symbolic links and removing \".\"
   and \"..\" entries.

"),

("Filesystem","Base","expanduser","expanduser(path::String) -> String

   On Unix systems, replace a tilde character at the start of a path
   with the current user's home directory.

"),

("Filesystem","Base","splitdir","splitdir(path::String) -> (String, String)

   Split a path into a tuple of the directory name and file name.

"),

("Filesystem","Base","splitdrive","splitdrive(path::String) -> (String, String)

   On Windows, split a path into the drive letter part and the path
   part. On Unix systems, the first component is always the empty
   string.

"),

("Filesystem","Base","splitext","splitext(path::String) -> (String, String)

   If the last component of a path contains a dot, split the path into
   everything before the dot and everything including and after the
   dot. Otherwise, return a tuple of the argument unmodified and the
   empty string.

"),

("Filesystem","Base","tempname","tempname()

   Generate a unique temporary filename.

"),

("Filesystem","Base","tempdir","tempdir()

   Obtain the path of a temporary directory.

"),

("Filesystem","Base","mktemp","mktemp()

   Returns \"(path, io)\", where \"path\" is the path of a new
   temporary file and \"io\" is an open file object for this path.

"),

("Filesystem","Base","mktempdir","mktempdir()

   Create a temporary directory and return its path.

"),

("Graphics","Base","Vec2","Vec2(x, y)

   Creates a point in two dimensions

"),

("Graphics","Base","BoundingBox","BoundingBox(xmin, xmax, ymin, ymax)

   Creates a box in two dimensions with the given edges

"),

("Graphics","Base","BoundingBox","BoundingBox(objs...)

   Creates a box in two dimensions that encloses all objects

"),

("Graphics","Base","width","width(obj)

   Computes the width of an object

"),

("Graphics","Base","height","height(obj)

   Computes the height of an object

"),

("Graphics","Base","xmin","xmin(obj)

   Computes the minimum x-coordinate contained in an object

"),

("Graphics","Base","xmax","xmax(obj)

   Computes the maximum x-coordinate contained in an object

"),

("Graphics","Base","ymin","ymin(obj)

   Computes the minimum y-coordinate contained in an object

"),

("Graphics","Base","ymax","ymax(obj)

   Computes the maximum y-coordinate contained in an object

"),

("Graphics","Base","diagonal","diagonal(obj)

   Return the length of the diagonal of an object

"),

("Graphics","Base","aspect_ratio","aspect_ratio(obj)

   Compute the height/width of an object

"),

("Graphics","Base","center","center(obj)

   Return the point in the center of an object

"),

("Graphics","Base","xrange","xrange(obj)

   Returns a tuple \"(xmin(obj), xmax(obj))\"

"),

("Graphics","Base","yrange","yrange(obj)

   Returns a tuple \"(ymin(obj), ymax(obj))\"

"),

("Graphics","Base","rotate","rotate(obj, angle, origin) -> newobj

   Rotates an object around origin by the specified angle (radians),
   returning a new object of the same type.  Because of type-
   constancy, this new object may not always be a strict geometric
   rotation of the input; for example, if \"obj\" is a \"BoundingBox\"
   the return is the smallest \"BoundingBox\" that encloses the
   rotated input.

"),

("Graphics","Base","shift","shift(obj, dx, dy)

   Returns an object shifted horizontally and vertically by the
   indicated amounts

"),

("Graphics","Base","*","*(obj, s::Real)

   Scale the width and height of a graphics object, keeping the center
   fixed

"),

("Graphics","Base","+","+(bb1::BoundingBox, bb2::BoundingBox) -> BoundingBox

   Returns the smallest box containing both boxes

"),

("Graphics","Base","&","&(bb1::BoundingBox, bb2::BoundingBox) -> BoundingBox

   Returns the intersection, the largest box contained in both boxes

"),

("Graphics","Base","deform","deform(bb::BoundingBox, dxmin, dxmax, dymin, dymax)

   Returns a bounding box with all edges shifted by the indicated
   amounts

"),

("Graphics","Base","isinside","isinside(bb::BoundingBox, x, y)

   True if the given point is inside the box

"),

("Graphics","Base","isinside","isinside(bb::BoundingBox, point)

   True if the given point is inside the box

"),


("Linear Algebra","Base","*","*(A, B)

   Matrix multiplication

"),

("Linear Algebra","Base","\\","\\(A, B)

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

("Linear Algebra","Base","dot","dot(x, y)

   Compute the dot product

"),

("Linear Algebra","Base","cross","cross(x, y)

   Compute the cross product of two 3-vectors

"),

("Linear Algebra","Base","norm","norm(a)

   Compute the norm of a \"Vector\" or a \"Matrix\"

"),

("Linear Algebra","Base","rref","rref(A)

   Compute the reduced row echelon form of the matrix A.

"),

("Linear Algebra","Base","factorize","factorize(A)

   Compute a convenient factorization (including LU, Cholesky, Bunch
   Kaufman, Triangular) of A, based upon the type of the input matrix.
   The return value can then be reused for efficient solving of
   multiple systems. For example: \"A=factorize(A); x=A\\\\b;
   y=A\\\\C\".

"),

("Linear Algebra","Base","factorize!","factorize!(A)

   \"factorize!\" is the same as \"factorize()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","lu","lu(A) -> L, U, p

   Compute the LU factorization of \"A\", such that \"A[p,:] = L*U\".

"),

("Linear Algebra","Base","lufact","lufact(A) -> LU

   Compute the LU factorization of \"A\", returning an \"LU\" object
   for dense \"A\" or an \"UmfpackLU\" object for sparse \"A\". The
   individual components of the factorization \"F\" can be accesed by
   indexing: \"F[:L]\", \"F[:U]\", and \"F[:P]\" (permutation matrix)
   or \"F[:p]\" (permutation vector). An \"UmfpackLU\" object has
   additional components \"F[:q]\" (the left permutation vector) and
   \"Rs\" the vector of scaling factors. The following functions are
   available for both \"LU\" and \"UmfpackLU\" objects: \"size\",
   \"\\\" and \"det\".  For \"LU\" there is also an \"inv\" method.
   The sparse LU factorization is such that \"L*U\" is equal
   to``scale(Rs,A)[p,q]``.

"),

("Linear Algebra","Base","lufact!","lufact!(A) -> LU

   \"lufact!\" is the same as \"lufact()\", but saves space by
   overwriting the input A, instead of creating a copy.  For sparse
   \"A\" the \"nzval\" field is not overwritten but the index fields,
   \"colptr\" and \"rowval\" are decremented in place, converting from
   1-based indices to 0-based indices.

"),

("Linear Algebra","Base","chol","chol(A[, LU]) -> F

   Compute Cholesky factorization of a symmetric positive-definite
   matrix \"A\" and return the matrix \"F\". If \"LU\" is \"L\"
   (Lower), \"A = L*L'\". If \"LU\" is \"U\" (Upper), \"A = R'*R\".

"),

("Linear Algebra","Base","cholfact","cholfact(A[, LU]) -> Cholesky

   Compute the Cholesky factorization of a dense symmetric positive-
   definite matrix \"A\" and return a \"Cholesky\" object. \"LU\" may
   be 'L' for using the lower part or 'U' for the upper part. The
   default is to use 'U'. The triangular matrix can be obtained from
   the factorization \"F\" with: \"F[:L]\" and \"F[:U]\". The
   following functions are available for \"Cholesky\" objects:
   \"size\", \"\\\", \"inv\", \"det\". A \"LAPACK.PosDefException\"
   error is thrown in case the matrix is not positive definite.

"),

("Linear Algebra","Base","cholfact","cholfact(A[, ll]) -> CholmodFactor

   Compute the sparse Cholesky factorization of a sparse matrix \"A\".
   If \"A\" is Hermitian its Cholesky factor is determined.  If \"A\"
   is not Hermitian the Cholesky factor of \"A*A'\" is determined. A
   fill-reducing permutation is used.  Methods for \"size\",
   \"solve\", \"\\\", \"findn_nzs\", \"diag\", \"det\" and \"logdet\".
   One of the solve methods includes an integer argument that can be
   used to solve systems involving parts of the factorization only.
   The optional boolean argument, \"ll\" determines whether the
   factorization returned is of the \"A[p,p] = L*L'\" form, where
   \"L\" is lower triangular or \"A[p,p] = scale(L,D)*L'\" form where
   \"L\" is unit lower triangular and \"D\" is a non-negative vector.
   The default is LDL.

"),

("Linear Algebra","Base","cholfact!","cholfact!(A[, LU]) -> Cholesky

   \"cholfact!\" is the same as \"cholfact()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","cholpfact","cholpfact(A[, LU]) -> CholeskyPivoted

   Compute the pivoted Cholesky factorization of a symmetric positive
   semi-definite matrix \"A\" and return a \"CholeskyPivoted\" object.
   \"LU\" may be 'L' for using the lower part or 'U' for the upper
   part. The default is to use 'U'. The triangular factors contained
   in the factorization \"F\" can be obtained with \"F[:L]\" and
   \"F[:U]\", whereas the permutation can be obtained with \"F[:P]\"
   or \"F[:p]\". The following functions are available for
   \"CholeskyPivoted\" objects: \"size\", \"\\\", \"inv\", \"det\". A
   \"LAPACK.RankDeficientException\" error is thrown in case the
   matrix is rank deficient.

"),

("Linear Algebra","Base","cholpfact!","cholpfact!(A[, LU]) -> CholeskyPivoted

   \"cholpfact!\" is the same as \"cholpfact\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","qr","qr(A[, thin]) -> Q, R

   Compute the QR factorization of \"A\" such that \"A = Q*R\". Also
   see \"qrfact\". The default is to compute a thin factorization.
   Note that *R* is not extended with zeros when the full *Q* is
   requested.

"),

("Linear Algebra","Base","qrfact","qrfact(A)

   Computes the QR factorization of \"A\" and returns a \"QR\" type,
   which is a \"Factorization\" \"F\" consisting of an orthogonal
   matrix \"F[:Q]\" and a triangular matrix \"F[:R]\". The following
   functions are available for \"QR\" objects: \"size\", \"\\\". The
   orthogonal matrix \"Q=F[:Q]\" is a \"QRPackedQ\" type which has the
   \"*\" operator overloaded to support efficient multiplication by
   \"Q\" and \"Q'\". Multiplication with respect to either thin or
   full \"Q\" is allowed, i.e. both \"F[:Q]*F[:R]\" and \"F[:Q]*A\"
   are supported. A \"QRPackedQ\" matrix can be converted into a
   regular matrix with \"full\".

"),

("Linear Algebra","Base","qrfact!","qrfact!(A)

   \"qrfact!\" is the same as \"qrfact()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","qrp","qrp(A[, thin]) -> Q, R, p

   Computes the QR factorization of \"A\" with pivoting, such that
   \"A[:,p] = Q*R\", Also see \"qrpfact\". The default is to compute a
   thin factorization.

"),

("Linear Algebra","Base","qrpfact","qrpfact(A) -> QRPivoted

   Computes the QR factorization of \"A\" with pivoting and returns a
   \"QRPivoted\" object, which is a \"Factorization\" \"F\" consisting
   of an orthogonal matrix \"F[:Q]\", a triangular matrix \"F[:R]\",
   and a permutation \"F[:p]\" (or  its matrix representation
   \"F[:P]\"). The following functions are available for \"QRPivoted\"
   objects: \"size\", \"\\\". The orthogonal matrix \"Q=F[:Q]\" is a
   \"QRPivotedQ\" type which has the \"*\" operator overloaded to
   support efficient multiplication by \"Q\" and \"Q'\".
   Multiplication with respect to either the thin or full \"Q\" is
   allowed, i.e. both \"F[:Q]*F[:R]\" and \"F[:Q]*A\" are supperted. A
   \"QRPivotedQ\" matrix can be converted into a regular matrix with
   \"full\".

"),

("Linear Algebra","Base","qrpfact!","qrpfact!(A) -> QRPivoted

   \"qrpfact!\" is the same as \"qrpfact()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","bkfact","bkfact(A) -> BunchKaufman

   Compute the Bunch Kaufman factorization of a real symmetric or
   complex Hermitian matrix \"A\" and return a \"BunchKaufman\"
   object. The following functions are available for \"BunchKaufman\"
   objects: \"size\", \"\\\", \"inv\", \"issym\", \"ishermitian\".

"),

("Linear Algebra","Base","bkfact!","bkfact!(A) -> BunchKaufman

   \"bkfact!\" is the same as \"bkfact()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","sqrtm","sqrtm(A)

   Compute the matrix square root of \"A\". If \"B = sqrtm(A)\", then
   \"B*B == A\" within roundoff error.

"),

("Linear Algebra","Base","eig","eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

"),

("Linear Algebra","Base","eig","eig(A, B) -> D, V

   Compute generalized eigenvalues and vectors of A and B

"),

("Linear Algebra","Base","eigvals","eigvals(A)

   Returns the eigenvalues of \"A\".

"),

("Linear Algebra","Base","eigmax","eigmax(A)

   Returns the largest eigenvalue of \"A\".

"),

("Linear Algebra","Base","eigmin","eigmin(A)

   Returns the smallest eigenvalue of \"A\".

"),

("Linear Algebra","Base","eigvecs","eigvecs(A[, eigvals])

   Returns the eigenvectors of \"A\".

   For SymTridiagonal matrices, if the optional vector of eigenvalues
   \"eigvals\" is specified, returns the specific corresponding
   eigenvectors.

"),

("Linear Algebra","Base","eigfact","eigfact(A)

   Compute the eigenvalue decomposition of \"A\" and return an
   \"Eigen\" object. If \"F\" is the factorization object, the
   eigenvalues can be accessed with \"F[:values]\" and the
   eigenvectors with \"F[:vectors]\". The following functions are
   available for \"Eigen\" objects: \"inv\", \"det\".

"),

("Linear Algebra","Base","eigfact","eigfact(A, B)

   Compute the generalized eigenvalue decomposition of \"A\" and \"B\"
   and return an \"GeneralizedEigen\" object. If \"F\" is the
   factorization object, the eigenvalues can be accessed with
   \"F[:values]\" and the eigenvectors with \"F[:vectors]\".

"),

("Linear Algebra","Base","eigfact!","eigfact!(A[, B])

   \"eigfact!\" is the same as \"eigfact()\", but saves space by
   overwriting the input A (and B), instead of creating a copy.

"),

("Linear Algebra","Base","hessfact","hessfact(A)

   Compute the Hessenberg decomposition of \"A\" and return a
   \"Hessenberg\" object. If \"F\" is the factorization object, the
   unitary matrix can be accessed with \"F[:Q]\" and the Hessenberg
   matrix with \"F[:H]\". When \"Q\" is extracted, the resulting type
   is the \"HessenbergQ\" object, and may be converted to a regular
   matrix with \"full\".

"),

("Linear Algebra","Base","hessfact!","hessfact!(A)

   \"hessfact!\" is the same as \"hessfact()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","schurfact","schurfact(A) -> Schur

   Computes the Schur factorization of the matrix \"A\". The (quasi)
   triangular Schur factor can be obtained from the \"Schur\" object
   \"F\" with either \"F[:Schur]\" or \"F[:T]\" and the
   unitary/orthogonal Schur vectors can be obtained with
   \"F[:vectors]\" or \"F[:Z]\" such that
   \"A=F[:vectors]*F[:Schur]*F[:vectors]'\". The eigenvalues of \"A\"
   can be obtained with \"F[:values]\".

"),

("Linear Algebra","Base","schurfact!","schurfact!(A)

   Computer the Schur factorization of A, overwriting A in the
   process. See \"schurfact()\"

"),

("Linear Algebra","Base","schur","schur(A) -> Schur[:T], Schur[:Z], Schur[:values]

   See schurfact

"),

("Linear Algebra","Base","schurfact","schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the
   matrices \"A\" and \"B\". The (quasi) triangular Schur factors can
   be obtained from the \"Schur\" object \"F\" with \"F[:S]\" and
   \"F[:T]\", the left unitary/orthogonal Schur vectors can be
   obtained with \"F[:left]\" or \"F[:Q]\" and the right
   unitary/orthogonal Schur vectors can be obtained with \"F[:right]\"
   or \"F[:Z]\" such that \"A=F[:left]*F[:S]*F[:right]'\" and
   \"B=F[:left]*F[:T]*F[:right]'\". The generalized eigenvalues of
   \"A\" and \"B\" can be obtained with \"F[:alpha]./F[:beta]\".

"),

("Linear Algebra","Base","schur","schur(A, B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See schurfact

"),

("Linear Algebra","Base","svdfact","svdfact(A[, thin]) -> SVD

   Compute the Singular Value Decomposition (SVD) of \"A\" and return
   an \"SVD\" object. \"U\", \"S\", \"V\" and \"Vt\" can be obtained
   from the factorization \"F\" with \"F[:U]\", \"F[:S]\", \"F[:V]\"
   and \"F[:Vt]\", such that \"A = U*diagm(S)*Vt\". If \"thin\" is
   \"true\", an economy mode decomposition is returned. The algorithm
   produces \"Vt\" and hence \"Vt\" is more efficient to extract than
   \"V\". The default is to produce a thin decomposition.

"),

("Linear Algebra","Base","svdfact!","svdfact!(A[, thin]) -> SVD

   \"svdfact!\" is the same as \"svdfact()\", but saves space by
   overwriting the input A, instead of creating a copy. If \"thin\" is
   \"true\", an economy mode decomposition is returned. The default is
   to produce a thin decomposition.

"),

("Linear Algebra","Base","svd","svd(A[, thin]) -> U, S, V

   Compute the SVD of A, returning \"U\", vector \"S\", and \"V\" such
   that \"A == U*diagm(S)*V'\". If \"thin\" is \"true\", an economy
   mode decomposition is returned.

"),

("Linear Algebra","Base","svdvals","svdvals(A)

   Returns the singular values of \"A\".

"),

("Linear Algebra","Base","svdvals!","svdvals!(A)

   Returns the singular values of \"A\", while saving space by
   overwriting the input.

"),

("Linear Algebra","Base","svdfact","svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of \"A\" and \"B\", returning a
   \"GeneralizedSVD\" Factorization object, such that \"A =
   U*D1*R0*Q'\" and \"B = V*D2*R0*Q'\".

"),

("Linear Algebra","Base","svd","svd(A, B) -> U, V, Q, D1, D2, R0

   Compute the generalized SVD of \"A\" and \"B\", returning \"U\",
   \"V\", \"Q\", \"D1\", \"D2\", and \"R0\" such that \"A =
   U*D1*R0*Q'\" and \"B = V*D2*R0*Q'\".

"),

("Linear Algebra","Base","svdvals","svdvals(A, B)

   Return only the singular values from the generalized singular value
   decomposition of \"A\" and \"B\".

"),

("Linear Algebra","Base","triu","triu(M)

   Upper triangle of a matrix.

"),

("Linear Algebra","Base","triu!","triu!(M)

   Upper triangle of a matrix, overwriting M in the process.

"),

("Linear Algebra","Base","tril","tril(M)

   Lower triangle of a matrix.

"),

("Linear Algebra","Base","tril!","tril!(M)

   Lower triangle of a matrix, overwriting M in the process.

"),

("Linear Algebra","Base","diagind","diagind(M[, k])

   A \"Range\" giving the indices of the \"k\"-th diagonal of the
   matrix \"M\".

"),

("Linear Algebra","Base","diag","diag(M[, k])

   The \"k\"-th diagonal of a matrix, as a vector.

"),

("Linear Algebra","Base","diagm","diagm(v[, k])

   Construct a diagonal matrix and place \"v\" on the \"k\"-th
   diagonal.

"),

("Linear Algebra","Base","scale","scale(A, B)

   \"scale(A::Array, B::Number)\" scales all values in \"A\" with
   \"B\". Note: In cases where the array is big enough, \"scale\" can
   be much faster than \"A .* B\", due to the use of BLAS.

   \"scale(A::Matrix, B::Vector)\" is the same as multiplying with a
   diagonal matrix on the right, and scales the columns of \"A\" with
   the values in \"B\".

   \"scale(A::Vector, B::Matrix)\" is the same as multiplying with a
   diagonal matrix on the left, and scales the rows of \"B\" with the
   values in \"A\".

"),

("Linear Algebra","Base","scale!","scale!(A, B)

   \"scale!(A,B)\" overwrites the input array with the scaled result.

"),

("Linear Algebra","Base","symmetrize!","symmetrize!(A[, UL::Char])

   \"symmetrize!(A)\" converts from the BLAS/LAPACK symmetric storage
   format, in which only the \"UL\" ('U'pper or 'L'ower, default 'U')
   triangle is used, to a full symmetric matrix.

"),

("Linear Algebra","Base","Tridiagonal","Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal,
   and upper diagonal, respectively.  The result is of type
   \"Tridiagonal\" and provides efficient specialized linear solvers,
   but may be converted into a regular matrix with \"full\".

"),

("Linear Algebra","Base","Bidiagonal","Bidiagonal(dv, ev, isupper)

   Constructs an upper (isupper=true) or lower (isupper=false)
   bidiagonal matrix using the given diagonal (dv) and off-diagonal
   (ev) vectors.  The result is of type \"Bidiagonal\" and provides
   efficient specialized linear solvers, but may be converted into a
   regular matrix with \"full\".

"),

("Linear Algebra","Base","SymTridiagonal","SymTridiagonal(d, du)

   Construct a real-symmetric tridiagonal matrix from the diagonal and
   upper diagonal, respectively. The result is of type
   \"SymTridiagonal\" and provides efficient specialized eigensolvers,
   but may be converted into a regular matrix with \"full\".

"),

("Linear Algebra","Base","Woodbury","Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury
   matrix identity

"),

("Linear Algebra","Base","rank","rank(M)

   Compute the rank of a matrix

"),

("Linear Algebra","Base","norm","norm(A[, p])

   Compute the \"p\"-norm of a vector or a matrix. \"p\" is \"2\" by
   default, if not provided. If \"A\" is a vector, \"norm(A, p)\"
   computes the \"p\"-norm. \"norm(A, Inf)\" returns the largest value
   in \"abs(A)\", whereas \"norm(A, -Inf)\" returns the smallest. If
   \"A\" is a matrix, valid values for \"p\" are \"1\", \"2\", or
   \"Inf\". In order to compute the Frobenius norm, use \"normfro\".

"),

("Linear Algebra","Base","normfro","normfro(A)

   Compute the Frobenius norm of a matrix \"A\".

"),

("Linear Algebra","Base","cond","cond(M[, p])

   Matrix condition number, computed using the p-norm. \"p\" is 2 by
   default, if not provided. Valid values for \"p\" are \"1\", \"2\",
   or \"Inf\".

"),

("Linear Algebra","Base","trace","trace(M)

   Matrix trace

"),

("Linear Algebra","Base","det","det(M)

   Matrix determinant

"),

("Linear Algebra","Base","logdet","logdet(M)

   Log of Matrix determinant. Equivalent to \"log(det(M))\", but may
   provide increased accuracy and/or speed.

"),

("Linear Algebra","Base","inv","inv(M)

   Matrix inverse

"),

("Linear Algebra","Base","pinv","pinv(M)

   Moore-Penrose inverse

"),

("Linear Algebra","Base","null","null(M)

   Basis for null space of M.

"),

("Linear Algebra","Base","repmat","repmat(A, n, m)

   Construct a matrix by repeating the given matrix \"n\" times in
   dimension 1 and \"m\" times in dimension 2.

"),

("Linear Algebra","Base","repeat","repeat(A, inner = Int[], outer = Int[])

   Construct an array by repeating the entries of \"A\". The i-th
   element of \"inner\" specifies the number of times that the
   individual entries of the i-th dimension of \"A\" should be
   repeated. The i-th element of \"outer\" specifies the number of
   times that a slice along the i-th dimension of \"A\" should be
   repeated.

"),

("Linear Algebra","Base","kron","kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

"),

("Linear Algebra","Base","linreg","linreg(x, y)

   Determine parameters \"[a, b]\" that minimize the squared error
   between \"y\" and \"a+b*x\".

"),

("Linear Algebra","Base","linreg","linreg(x, y, w)

   Weighted least-squares linear regression.

"),

("Linear Algebra","Base","expm","expm(A)

   Matrix exponential.

"),

("Linear Algebra","Base","issym","issym(A)

   Test whether a matrix is symmetric.

"),

("Linear Algebra","Base","isposdef","isposdef(A)

   Test whether a matrix is positive-definite.

"),

("Linear Algebra","Base","isposdef!","isposdef!(A)

   Test whether a matrix is positive-definite, overwriting A in the
   processes.

"),

("Linear Algebra","Base","istril","istril(A)

   Test whether a matrix is lower-triangular.

"),

("Linear Algebra","Base","istriu","istriu(A)

   Test whether a matrix is upper-triangular.

"),

("Linear Algebra","Base","ishermitian","ishermitian(A)

   Test whether a matrix is hermitian.

"),

("Linear Algebra","Base","transpose","transpose(A)

   The transpose operator (\".'\").

"),

("Linear Algebra","Base","ctranspose","ctranspose(A)

   The conjugate transpose operator (\"'\").

"),

("Linear Algebra","Base","eigs","eigs(A; nev=6, which=\"LM\", tol=0.0, maxiter=1000, sigma=0, ritzvec=true, op_part=:real, v0=zeros((0, ))) -> (d[, v], nconv, niter, nmult, resid)

   \"eigs\" computes eigenvalues \"d\" of A using Arnoldi
   factorization. The following keyword arguments are supported:
      * \"nev\": Number of eigenvalues

      * \"which\": type of eigenvalues (\"LM\", \"SM\")

      * \"tol\": tolerance (tol \\le 0.0 defaults to
        \"DLAMCH('EPS')\")

      * \"maxiter\": Maximum number of iterations

      * \"sigma\": find eigenvalues close to \"sigma\" using shift and
        invert

      * \"ritzvec\": Returns the Ritz vectors \"v\" (eigenvectors) if
        \"true\"

      * \"op_part\": which part of linear operator to use for real A
        (:real, :imag)

      * \"v0\": starting vector from which to start the Arnoldi
        iteration

   \"eigs\" returns the \"nev\" requested eigenvalues in \"d\", the
   corresponding Ritz vectors \"v\" (only if \"ritzvec=true\"), the
   number of converged eigenvalues \"nconv\", the number of iterations
   \"niter\" and the number of matrix vector multiplications
   \"nmult\", as well as the final residual vector \"resid\".

"),

("Linear Algebra","Base","svds","svds(A; nev=6, which=\"LA\", tol=0.0, maxiter=1000, ritzvec=true)

   \"svds\" computes the singular values of A using Arnoldi
   factorization. The following keyword arguments are supported:
      * \"nsv\": Number of singular values

      * \"which\": type of singular values (\"LA\")

      * \"tol\": tolerance (tol \\le 0.0 defaults to
        \"DLAMCH('EPS')\")

      * \"maxiter\": Maximum number of iterations

      * \"ritzvec\": Returns the singular vectors if \"true\"

"),

("Linear Algebra","Base","peakflops","peakflops(n; parallel=false)

   \"peakflops\" computes the peak flop rate of the computer by using
   BLAS dgemm. By default, if no arguments are specified, it
   multiplies a matrix of size \"n x n\", where \"n = 2000\". If the
   underlying BLAS is using multiple threads, higher flop rates are
   realized. The number of BLAS threads can be set with
   \"blas_set_num_threads(n)\".

   If the keyword argument \"parallel\" is set to \"true\",
   \"peakflops\" is run in parallel on all the worker processors. The
   flop rate of the entire parallel computer is returned. When running
   in parallel, only 1 BLAS thread is used. The argument \"n\" still
   refers to the size of the problem that is solved on each processor.

"),

("BLAS Functions","Base","dot","dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of \"n\" elements of array
   \"X\" with stride \"incx\" and \"n\" elements of array \"Y\" with
   stride \"incy\".  There are no \"dot\" methods for \"Complex\"
   arrays.

"),

("BLAS Functions","Base.LinAlg.BLAS","blascopy!","blascopy!(n, X, incx, Y, incy)

   Copy \"n\" elements of array \"X\" with stride \"incx\" to array
   \"Y\" with stride \"incy\".  Returns \"Y\".

"),

("BLAS Functions","Base.LinAlg.BLAS","nrm2","nrm2(n, X, incx)

   2-norm of a vector consisting of \"n\" elements of array \"X\" with
   stride \"incx\".

"),

("BLAS Functions","Base.LinAlg.BLAS","asum","asum(n, X, incx)

   sum of the absolute values of the first \"n\" elements of array
   \"X\" with stride \"incx\".

"),

("BLAS Functions","Base.LinAlg.BLAS","axpy!","axpy!(n, a, X, incx, Y, incy)

   Overwrite \"Y\" with \"a*X + Y\".  Returns \"Y\".

"),

("BLAS Functions","Base.LinAlg.BLAS","scal!","scal!(n, a, X, incx)

   Overwrite \"X\" with \"a*X\".  Returns \"X\".

"),

("BLAS Functions","Base.LinAlg.BLAS","scal","scal(n, a, X, incx)

   Returns \"a*X\".

"),

("BLAS Functions","Base.LinAlg.BLAS","syrk!","syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix \"C\" as \"alpha*A*A.' +
   beta*C\" or \"alpha*A.'*A + beta*C\" according to whether \"trans\"
   is 'N' or 'T'.  When \"uplo\" is 'U' the upper triangle of \"C\" is
   updated ('L' for lower triangle).  Returns \"C\".

"),

("BLAS Functions","Base.LinAlg.BLAS","syrk","syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according
   to \"uplo\" ('U' or 'L'), of \"alpha*A*A.'\" or \"alpha*A.'*A\",
   according to \"trans\" ('N' or 'T').

"),

("BLAS Functions","Base.LinAlg.BLAS","herk!","herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian
   matrix \"C\" as \"alpha*A*A' + beta*C\" or \"alpha*A'*A + beta*C\"
   according to whether \"trans\" is 'N' or 'T'.  When \"uplo\" is 'U'
   the upper triangle of \"C\" is updated ('L' for lower triangle).
   Returns \"C\".

"),

("BLAS Functions","Base.LinAlg.BLAS","herk","herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle
   or the lower triangle, according to \"uplo\" ('U' or 'L'), of
   \"alpha*A*A'\" or \"alpha*A'*A\", according to \"trans\" ('N' or
   'T').

"),

("BLAS Functions","Base.LinAlg.BLAS","gbmv!","gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" or \"alpha*A'*x +
   beta*y\" according to \"trans\" ('N' or 'T').  The matrix \"A\" is
   a general band matrix of dimension \"m\" by \"size(A,2)\" with
   \"kl\" sub-diagonals and \"ku\" super-diagonals. Returns the
   updated \"y\".

"),

("BLAS Functions","Base.LinAlg.BLAS","gbmv","gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns \"alpha*A*x\" or \"alpha*A'*x\" according to \"trans\" ('N'
   or 'T'). The matrix \"A\" is a general band matrix of dimension
   \"m\" by \"size(A,2)\" with \"kl\" sub-diagonals and \"ku\" super-
   diagonals.

"),

("BLAS Functions","Base.LinAlg.BLAS","sbmv!","sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" where \"A\" is a a
   symmetric band matrix of order \"size(A,2)\" with \"k\" super-
   diagonals stored in the argument \"A\".  The storage layout for
   \"A\" is described the reference BLAS module, level-2 BLAS at
   http://www.netlib.org/lapack/explore-html/.

   Returns the updated \"y\".

"),

("BLAS Functions","Base.LinAlg.BLAS","sbmv","sbmv(uplo, k, alpha, A, x)

   Returns \"alpha*A*x\" where \"A\" is a symmetric band matrix of
   order \"size(A,2)\" with \"k\" super-diagonals stored in the
   argument \"A\".

"),

("BLAS Functions","Base.LinAlg.BLAS","sbmv","sbmv(uplo, k, A, x)

   Returns \"A*x\" where \"A\" is a symmetric band matrix of order
   \"size(A,2)\" with \"k\" super-diagonals stored in the argument
   \"A\".

"),

("BLAS Functions","Base.LinAlg.BLAS","gemm!","gemm!(tA, tB, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or the other three variants
   according to \"tA\" (transpose \"A\") and \"tB\".  Returns the
   updated \"C\".

"),

("BLAS Functions","Base.LinAlg.BLAS","gemm","gemm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("BLAS Functions","Base.LinAlg.BLAS","gemm","gemm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("BLAS Functions","Base.LinAlg.BLAS","gemv!","gemv!(tA, alpha, A, x, beta, y)

   Update the vector \"y\" as \"alpha*A*x + beta*x\" or \"alpha*A'x +
   beta*x\" according to \"tA\" (transpose \"A\"). Returns the updated
   \"y\".

"),

("BLAS Functions","Base.LinAlg.BLAS","gemv","gemv(tA, alpha, A, x)

   Returns \"alpha*A*x\" or \"alpha*A'x\" according to \"tA\"
   (transpose \"A\").

"),

("BLAS Functions","Base.LinAlg.BLAS","gemv","gemv(tA, alpha, A, x)

   Returns \"A*x\" or \"A'x\" according to \"tA\" (transpose \"A\").

"),

("BLAS Functions","Base.LinAlg.BLAS","symm!","symm!(side, ul, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or \"alpha*B*A + beta*C\"
   according to \"side\". \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.  Returns the updated \"C\".

"),

("BLAS Functions","Base.LinAlg.BLAS","symm","symm(side, ul, alpha, A, B)

   Returns \"alpha*A*B\" or \"alpha*B*A\" according to \"side\". \"A\"
   is assumed to be symmetric.  Only the \"ul\" triangle of \"A\" is
   used.

"),

("BLAS Functions","Base.LinAlg.BLAS","symm","symm(side, ul, A, B)

   Returns \"A*B\" or \"B*A\" according to \"side\".  \"A\" is assumed
   to be symmetric.  Only the \"ul\" triangle of \"A\" is used.

"),

("BLAS Functions","Base.LinAlg.BLAS","symm","symm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("BLAS Functions","Base.LinAlg.BLAS","symv!","symv!(ul, alpha, A, x, beta, y)

   Update the vector \"y\" as \"alpha*A*y + beta*y\". \"A\" is assumed
   to be symmetric.  Only the \"ul\" triangle of \"A\" is used.
   Returns the updated \"y\".

"),

("BLAS Functions","Base.LinAlg.BLAS","symv","symv(ul, alpha, A, x)

   Returns \"alpha*A*x\". \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.

"),

("BLAS Functions","Base.LinAlg.BLAS","symv","symv(ul, A, x)

   Returns \"A*x\".  \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.

"),

("BLAS Functions","Base.LinAlg.BLAS","trmm!","trmm!(side, ul, tA, dA, alpha, A, B)

   Update \"B\" as \"alpha*A*B\" or one of the other three variants
   determined by \"side\" (A on left or right) and \"tA\" (transpose
   A). Only the \"ul\" triangle of \"A\" is used.  \"dA\" indicates if
   \"A\" is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated \"B\".

"),

("BLAS Functions","Base.LinAlg.BLAS","trmm","trmm(side, ul, tA, dA, alpha, A, B)

   Returns \"alpha*A*B\" or one of the other three variants determined
   by \"side\" (A on left or right) and \"tA\" (transpose A). Only the
   \"ul\" triangle of \"A\" is used.  \"dA\" indicates if \"A\" is
   unit-triangular (the diagonal is assumed to be all ones).

"),

("BLAS Functions","Base.LinAlg.BLAS","trsm!","trsm!(side, ul, tA, dA, alpha, A, B)

   Overwrite \"B\" with the solution to \"A*X = alpha*B\" or one of
   the other three variants determined by \"side\" (A on left or right
   of \"X\") and \"tA\" (transpose A). Only the \"ul\" triangle of
   \"A\" is used.  \"dA\" indicates if \"A\" is unit-triangular (the
   diagonal is assumed to be all ones).  Returns the updated \"B\".

"),

("BLAS Functions","Base.LinAlg.BLAS","trsm","trsm(side, ul, tA, dA, alpha, A, B)

   Returns the solution to \"A*X = alpha*B\" or one of the other three
   variants determined by \"side\" (A on left or right of \"X\") and
   \"tA\" (transpose A). Only the \"ul\" triangle of \"A\" is used.
   \"dA\" indicates if \"A\" is unit-triangular (the diagonal is
   assumed to be all ones).

"),

("BLAS Functions","Base.LinAlg.BLAS","trmv!","trmv!(side, ul, tA, dA, alpha, A, b)

   Update \"b\" as \"alpha*A*b\" or one of the other three variants
   determined by \"side\" (A on left or right) and \"tA\" (transpose
   A). Only the \"ul\" triangle of \"A\" is used.  \"dA\" indicates if
   \"A\" is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated \"b\".

"),

("BLAS Functions","Base.LinAlg.BLAS","trmv","trmv(side, ul, tA, dA, alpha, A, b)

   Returns \"alpha*A*b\" or one of the other three variants determined
   by \"side\" (A on left or right) and \"tA\" (transpose A). Only the
   \"ul\" triangle of \"A\" is used.  \"dA\" indicates if \"A\" is
   unit-triangular (the diagonal is assumed to be all ones).

"),

("BLAS Functions","Base.LinAlg.BLAS","trsv!","trsv!(side, ul, tA, dA, alpha, A, b)

   Overwrite \"b\" with the solution to \"A*X = alpha*b\" or one of
   the other three variants determined by \"side\" (A on left or right
   of \"X\") and \"tA\" (transpose A). Only the \"ul\" triangle of
   \"A\" is used.  \"dA\" indicates if \"A\" is unit-triangular (the
   diagonal is assumed to be all ones).  Returns the updated \"b\".

"),

("BLAS Functions","Base.LinAlg.BLAS","trsv","trsv(side, ul, tA, dA, alpha, A, b)

   Returns the solution to \"A*X = alpha*b\" or one of the other three
   variants determined by \"side\" (A on left or right of \"X\") and
   \"tA\" (transpose A). Only the \"ul\" triangle of \"A\" is used.
   \"dA\" indicates if \"A\" is unit-triangular (the diagonal is
   assumed to be all ones).

"),

("BLAS Functions","Base.LinAlg.BLAS","blas_set_num_threads","blas_set_num_threads(n)

   Set the number of threads the BLAS library should use.

"),

("Package Manager Functions","Base.Pkg","dir","dir() -> String

   Returns the absolute path of the package directory. This defaults
   to \"joinpath(homedir(),\".julia\")\" on all platforms (i.e.
   \"~/.julia\" in UNIX shell syntax). If the \"JULIA_PKGDIR\"
   environment variable is set, that path is used instead. If
   \"JULIA_PKGDIR\" is a relative path, it is interpreted relative to
   whatever the current working directory is.

"),

("Package Manager Functions","Base.Pkg","dir","dir(names...) -> String

   Equivalent to \"normpath(Pkg.dir(),names...)\" – i.e. it appends
   path components to the package directory and normalizes the
   resulting path. In particular, \"Pkg.dir(pkg)\" returns the path to
   the package \"pkg\".

"),

("Package Manager Functions","Base.Pkg","init","init()

   Initialize \"Pkg.dir()\" as a package directory. This will be done
   automatically when the \"JULIA_PKGDIR\" is not set and
   \"Pkg.dir()\" uses its default value.

"),

("Package Manager Functions","Base.Pkg","resolve","resolve()

   Determines an optimal, consistent set of package versions to
   install or upgrade to. The optimal set of package versions is based
   on the contents of \"Pkg.dir(\"REQUIRE\")\" and the state of
   installed packages in \"Pkg.dir()\", Packages that are no longer
   required are moved into \"Pkg.dir(\".trash\")\".

"),

("Package Manager Functions","Base.Pkg","edit","edit()

   Opens \"Pkg.dir(\"REQUIRE\")\" in the editor specified by the
   \"VISUAL\" or \"EDITOR\" environment variables; when the editor
   command returns, it runs \"Pkg.resolve()\" to determine and install
   a new optimal set of installed package versions.

"),

("Package Manager Functions","Base.Pkg","add","add(pkg, vers...)

   Add a requirement entry for \"pkg\" to \"Pkg.dir(\"REQUIRE\")\" and
   call \"Pkg.resolve()\". If \"vers\" are given, they must be
   \"VersionNumber\" objects and they specify acceptable version
   intervals for \"pkg\".

"),

("Package Manager Functions","Base.Pkg","rm","rm(pkg)

   Remove all requirement entries for \"pkg\" from
   \"Pkg.dir(\"REQUIRE\")\" and call \"Pkg.resolve()\".

"),

("Package Manager Functions","Base.Pkg","clone","clone(url[, pkg])

   Clone a package directly from the git URL \"url\". The package does
   not need to be a registered in \"Pkg.dir(\"METADATA\")\". The
   package repo is cloned by the name \"pkg\" if provided; if not
   provided, \"pkg\" is determined automatically from \"url\".

"),

("Package Manager Functions","Base.Pkg","clone","clone(pkg)

   If \"pkg\" has a URL registered in \"Pkg.dir(\"METADATA\")\", clone
   it from that URL on the default branch. The package does not need
   to have any registered versions.

"),

("Package Manager Functions","Base.Pkg","available","available() -> Vector{ASCIIString}

   Returns the names of available packages.

"),

("Package Manager Functions","Base.Pkg","available","available(pkg) -> Vector{VersionNumber}

   Returns the version numbers available for package \"pkg\".

"),

("Package Manager Functions","Base.Pkg","installed","installed() -> Dict{ASCIIString,VersionNumber}

   Returns a dictionary mapping installed package names to the
   installed version number of each package.

"),

("Package Manager Functions","Base.Pkg","installed","installed(pkg) -> Nothing | VersionNumber

   If \"pkg\" is installed, return the installed version number,
   otherwise return \"nothing\".

"),

("Package Manager Functions","Base.Pkg","status","status()

   Prints out a summary of what packages are installed and what
   version and state they're in.

"),

("Package Manager Functions","Base.Pkg","update","update()

   Update package the metadata repo – kept in
   \"Pkg.dir(\"METADATA\")\" – then update any fixed packages that can
   safely be pulled from their origin; then call \"Pkg.resolve()\" to
   determine a new optimal set of packages versions.

"),

("Package Manager Functions","Base.Pkg","checkout","checkout(pkg[, branch=\"master\"])

   Checkout the \"Pkg.dir(pkg)\" repo to the branch \"branch\".
   Defaults to checking out the \"master\" branch.

"),

("Package Manager Functions","Base.Pkg","pin","pin(pkg)

   Pin \"pkg\" at the current version.

"),

("Package Manager Functions","Base.Pkg","pin","pin(pkg, version)

   Pin \"pkg\" at registered version \"version\".

"),

("Package Manager Functions","Base.Pkg","free","free(pkg)

   Free the package \"pkg\" to be managed by the package manager
   again. It calls \"Pkg.resolve()\" to determine optimal package
   versions after. This is an inverse for both \"Pkg.checkout\" and
   \"Pkg.pin\".

"),

("Package Manager Functions","Base.Pkg","build","build()

   Run the build scripts for all installed packages in depth-first
   recursive order.

"),

("Package Manager Functions","Base.Pkg","build","build(pkgs...)

   Run the build scripts for each package in \"pkgs\" and all of their
   dependencies in depth-first recursive order. This is called
   automatically by \"Pkg.resolve()\" on all installed or updated
   packages.

"),

("Package Manager Functions","Base.Pkg","generate","generate(pkg, license)

   Generate a new package named \"pkg\" with one of these license
   keys: \"\"MIT\"\" or \"\"BSD\"\". If you want to make a package
   with a different license, you can edit it afterwards. Generate
   creates a git repo at \"Pkg.dir(pkg)\" for the package and inside
   it \"LICENSE.md\", \"README.md\", the julia entrypoint
   \"\$pkg/src/\$pkg.jl\", and a travis test file, \".travis.yml\".

"),

("Package Manager Functions","Base.Pkg","register","register(pkg[, url])

   Register \"pkg\" at the git URL \"url\", defaulting to the
   configured origin URL of the git repo \"Pkg.dir(pkg)\".

"),

("Package Manager Functions","Base.Pkg","tag","tag(pkg[, ver[, commit]])

   Tag \"commit\" as version \"ver\" of package \"pkg\" and create a
   version entry in \"METADATA\". If not provided, \"commit\" defaults
   to the current commit of the \"pkg\" repo. If \"ver\" is one of the
   symbols \":patch\", \":minor\", \":major\" the next patch, minor or
   major version is used. If \"ver\" is not provided, it defaults to
   \":patch\".

"),

("Package Manager Functions","Base.Pkg","publish","publish()

   For each new package version tagged in \"METADATA\" not already
   published, make sure that the tagged package commits have been
   pushed to the repo at the registered URL for the package and if
   they all have, push \"METADATA\".

"),

("Profiling","Base","@profile","@profile()

   \"@profile <expression>\" runs your expression while taking
   periodic backtraces.  These are appended to an internal buffer of
   backtraces.

"),

("Profiling","Base.Profile","clear","clear()

   Clear any existing backtraces from the internal buffer.

"),

("Profiling","Base.Profile","print","print([io::IO = STDOUT], [data::Vector]; format = :tree, C = false, combine = true, cols = tty_cols())

   Prints profiling results to \"io\" (by default, \"STDOUT\"). If you
   do not supply a \"data\" vector, the internal buffer of accumulated
   backtraces will be used.  \"format\" can be \":tree\" or \":flat\".
   If \"C==true\", backtraces from C and Fortran code are shown.
   \"combine==true\" merges instruction pointers that correspond to
   the same line of code.  \"cols\" controls the width of the display.

"),

("Profiling","Base.Profile","print","print([io::IO = STDOUT], data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   Prints profiling results to \"io\". This variant is used to examine
   results exported by a previous call to \"Profile.retrieve()\".
   Supply the vector \"data\" of backtraces and a dictionary
   \"lidict\" of line information.

"),

("Profiling","Base.Profile","init","init(n::Integer, delay::Float64)

   Configure the \"delay\" between backtraces (measured in seconds),
   and the number \"n\" of instruction pointers that may be stored.
   Each instruction pointer corresponds to a single line of code;
   backtraces generally consist of a long list of instruction
   pointers. Default settings are \"n=10^6\" and \"delay=0.001\".

"),

("Profiling","Base.Profile","fetch","fetch() -> data

   Returns a reference to the internal buffer of backtraces. Note that
   subsequent operations, like \"Profile.clear()\", can affect
   \"data\" unless you first make a copy. Note that the values in
   \"data\" have meaning only on this machine in the current session,
   because it depends on the exact memory addresses used in JIT-
   compiling. This function is primarily for internal use;
   \"Profile.retrieve()\" may be a better choice for most users.

"),

("Profiling","Base.Profile","retrieve","retrieve(;C = false) -> data, lidict

   \"Exports\" profiling results in a portable format, returning the
   set of all backtraces (\"data\") and a dictionary that maps the
   (session-specific) instruction pointers in \"data\" to \"LineInfo\"
   values that store the file name, function name, and line number.
   This function allows you to save profiling results for future
   analysis.

"),


("Sorting and Related Functions","Base","sort!","sort!(v, [dim,] [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the vector \"v\" in place. \"QuickSort\" is used by default
   for numeric arrays while \"MergeSort\" is used for other arrays.
   You can specify an algorithm to use via the \"alg\" keyword (see
   Sorting Algorithms for available algorithms). The \"by\" keyword
   lets you provide a function that will be applied to each element
   before comparison; the \"lt\" keyword allows providing a custom
   \"less than\" function; use \"rev=true\" to reverse the sorting
   order. These options are independent and can be used together in
   all possible combinations: if both \"by\" and \"lt\" are specified,
   the \"lt\" function is applied to the result of the \"by\"
   function; \"rev=true\" reverses whatever ordering specified via the
   \"by\" and \"lt\" keywords.

"),

("Sorting and Related Functions","Base","sort","sort(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of \"sort!\" that returns a sorted copy of \"v\" leaving
   \"v\" itself unmodified.

"),

("Sorting and Related Functions","Base","sort","sort(A, dim, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort a multidimensional array \"A\" along the given dimension.

"),

("Sorting and Related Functions","Base","sortperm","sortperm(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Return a permutation vector of indices of \"v\" that puts it in
   sorted order. Specify \"alg\" to choose a particular sorting
   algorithm (see Sorting Algorithms). \"MergeSort\" is used by
   default, and since it is stable, the resulting permutation will be
   the lexicographically first one that puts the input array into
   sorted order – i.e. indices of equal elements appear in ascending
   order. If you choose a non-stable sorting algorithm such as
   \"QuickSort\", a different permutation that puts the array into
   order may be returned. The order is specified using the same
   keywords as \"sort!\".

"),

("Sorting and Related Functions","Base","sortrows","sortrows(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the rows of matrix \"A\" lexicographically.

"),

("Sorting and Related Functions","Base","sortcols","sortcols(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the columns of matrix \"A\" lexicographically.

"),

("Sorting and Related Functions","Base","issorted","issorted(v, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Test whether a vector is in sorted order. The \"by\", \"lt\" and
   \"rev\" keywords modify what order is considered to be sorted just
   as they do for \"sort\".

"),

("Sorting and Related Functions","Base","searchsorted","searchsorted(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the range of indices of \"a\" which compare as equal to
   \"x\" according to the order specified by the \"by\", \"lt\" and
   \"rev\" keywords, assuming that \"a\" is already sorted in that
   order. Returns an empty range located at the insertion point if
   \"a\" does not contain values equal to \"x\".

"),

("Sorting and Related Functions","Base","searchsortedfirst","searchsortedfirst(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the first value in \"a\" greater than or equal
   to \"x\", according to the specified order. Returns \"length(a)+1\"
   if \"x\" is greater than all values in \"a\".

"),

("Sorting and Related Functions","Base","searchsortedlast","searchsortedlast(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the last value in \"a\" less than or equal to
   \"x\", according to the specified order. Returns \"0\" if \"x\" is
   less than all values in \"a\".

"),

("Sorting and Related Functions","Base","select!","select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Partially sort the vector \"v\" in place, according to the order
   specified by \"by\", \"lt\" and \"rev\" so that the value at index
   \"k\" (or range of adjacent values if \"k\" is a range) occurs at
   the position where it would appear if the array were fully sorted.
   If \"k\" is a single index, that values is returned; if \"k\" is a
   range, an array of values at those indices is returned. Note that
   \"select!\" does not fully sort the input array, but does leave the
   returned elements where they would be if the array were fully
   sorted.

"),

("Sorting and Related Functions","Base","select","select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of \"select!\" which copies \"v\" before partially sorting
   it, thereby returning the same thing as \"select!\" but leaving
   \"v\" unmodified.

"),

("Sparse Matrices","Base","sparse","sparse(I, J, V[, m, n, combine])

   Create a sparse matrix \"S\" of dimensions \"m x n\" such that
   \"S[I[k], J[k]] = V[k]\". The \"combine\" function is used to
   combine duplicates. If \"m\" and \"n\" are not specified, they are
   set to \"max(I)\" and \"max(J)\" respectively. If the \"combine\"
   function is not supplied, duplicates are added by default.

"),

("Sparse Matrices","Base","sparsevec","sparsevec(I, V[, m, combine])

   Create a sparse matrix \"S\" of size \"m x 1\" such that \"S[I[k]]
   = V[k]\". Duplicates are combined using the \"combine\" function,
   which defaults to \"+\" if it is not provided. In julia, sparse
   vectors are really just sparse matrices with one column. Given
   Julia's Compressed Sparse Columns (CSC) storage format, a sparse
   column matrix with one column is sparse, whereas a sparse row
   matrix with one row ends up being dense.

"),

("Sparse Matrices","Base","sparsevec","sparsevec(D::Dict[, m])

   Create a sparse matrix of size \"m x 1\" where the row values are
   keys from the dictionary, and the nonzero values are the values
   from the dictionary.

"),

("Sparse Matrices","Base","issparse","issparse(S)

   Returns \"true\" if \"S\" is sparse, and \"false\" otherwise.

"),

("Sparse Matrices","Base","sparse","sparse(A)

   Convert a dense matrix \"A\" into a sparse matrix.

"),

("Sparse Matrices","Base","sparsevec","sparsevec(A)

   Convert a dense vector \"A\" into a sparse matrix of size \"m x
   1\". In julia, sparse vectors are really just sparse matrices with
   one column.

"),

("Sparse Matrices","Base","dense","dense(S)

   Convert a sparse matrix \"S\" into a dense matrix.

"),

("Sparse Matrices","Base","full","full(S)

   Convert a sparse matrix \"S\" into a dense matrix.

"),

("Sparse Matrices","Base","spzeros","spzeros(m, n)

   Create an empty sparse matrix of size \"m x n\".

"),

("Sparse Matrices","Base","spones","spones(S)

   Create a sparse matrix with the same structure as that of \"S\",
   but with every nonzero element having the value \"1.0\".

"),

("Sparse Matrices","Base","speye","speye(type, m[, n])

   Create a sparse identity matrix of specified type of size \"m x
   m\". In case \"n\" is supplied, create a sparse identity matrix of
   size \"m x n\".

"),

("Sparse Matrices","Base","spdiagm","spdiagm(B, d[, m, n])

   Construct a sparse diagonal matrix. \"B\" is a tuple of vectors
   containing the diagonals and \"d\" is a tuple containing the
   positions of the diagonals. In the case the input contains only one
   diagonaly, \"B\" can be a vector (instead of a tuple) and \"d\" can
   be the diagonal position (instead of a tuple), defaulting to 0
   (diagonal). Optionally, \"m\" and \"n\" specify the size of the
   resulting sparse matrix.

"),

("Sparse Matrices","Base","sprand","sprand(m, n, density[, rng])

   Create a random sparse matrix with the specified density. Nonzeros
   are sampled from the distribution specified by \"rng\". The uniform
   distribution is used in case \"rng\" is not specified.

"),

("Sparse Matrices","Base","sprandn","sprandn(m, n, density)

   Create a random sparse matrix of specified density with nonzeros
   sampled from the normal distribution.

"),

("Sparse Matrices","Base","sprandbool","sprandbool(m, n, density)

   Create a random sparse boolean matrix with the specified density.

"),

("Sparse Matrices","Base","etree","etree(A[, post])

   Compute the elimination tree of a symmetric sparse matrix \"A\"
   from \"triu(A)\" and, optionally, its post-ordering permutation.

"),

("Sparse Matrices","Base","symperm","symperm(A, p)

   Return the symmetric permutation of A, which is \"A[p,p]\". A
   should be symmetric and sparse, where only the upper triangular
   part of the matrix is stored. This algorithm ignores the lower
   triangular part of the matrix. Only the upper triangular part of
   the result is returned as well.

"),

("Unit and Functional Testing","Base.Test","@test","@test(ex)

   Test the expression \"ex\" and calls the current handler to handle
   the result.

"),

("Unit and Functional Testing","Base.Test","@test_throws","@test_throws(ex)

   Test the expression \"ex\" and calls the current handler to handle
   the result in the following manner:

   * If the test doesn't throw an error, the \"Failure\" case is
     called.

   * If the test throws an error, the \"Success\" case is called.

"),

("Unit and Functional Testing","Base.Test","@test_approx_eq","@test_approx_eq(a, b)

   Test two floating point numbers \"a\" and \"b\" for equality taking
   in account small numerical errors.

"),

("Unit and Functional Testing","Base.Test","@test_approx_eq_eps","@test_approx_eq_eps(a, b, tol)

   Test two floating point numbers \"a\" and \"b\" for equality taking
   in account a margin of tolerance given by \"tol\".

"),

("Unit and Functional Testing","Base.Test","with_handler","with_handler(f, handler)

   Run the function \"f\" using the \"handler\" as the handler.

"),


}
