# automatically generated from files in doc/stdlib/ -- do not edit here

Any[

("Base","exit","exit([code])

   Quit (or control-D at the prompt). The default exit code is zero,
   indicating that the processes completed successfully.

"),

("Base","quit","quit()

   Quit the program indicating that the processes completed
   successfully. This function calls \"exit(0)\" (see \"exit()\").

"),

("Base","atexit","atexit(f)

   Register a zero-argument function to be called at exit.

"),

("Base","isinteractive","isinteractive() -> Bool

   Determine whether Julia is running an interactive session.

"),

("Base","whos","whos([Module,] [pattern::Regex])

   Print information about exported global variables in a module,
   optionally restricted to those matching \"pattern\".

"),

("Base","edit","edit(file::AbstractString[, line])

   Edit a file optionally providing a line number to edit at. Returns
   to the julia prompt when you quit the editor.

"),

("Base","edit","edit(function[, types])

   Edit the definition of a function, optionally specifying a tuple of
   types to indicate which method to edit.

"),

("Base","@edit","@edit()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"edit\" function on the resulting expression

"),

("Base","less","less(file::AbstractString[, line])

   Show a file using the default pager, optionally providing a
   starting line number. Returns to the julia prompt when you quit the
   pager.

"),

("Base","less","less(function[, types])

   Show the definition of a function using the default pager,
   optionally specifying a tuple of types to indicate which method to
   see.

"),

("Base","@less","@less()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"less\" function on the resulting expression

"),

("Base","clipboard","clipboard(x)

   Send a printed form of \"x\" to the operating system clipboard
   (\"copy\").

"),

("Base","clipboard","clipboard() -> AbstractString

   Return a string with the contents of the operating system clipboard
   (\"paste\").

"),

("Base","require","require(file::AbstractString...)

   Load source files once, in the context of the \"Main\" module, on
   every active node, searching standard locations for files.
   \"require\" is considered a top-level operation, so it sets the
   current \"include\" path but does not use it to search for files
   (see help for \"include\"). This function is typically used to load
   library code, and is implicitly called by \"using\" to load
   packages.

   When searching for files, \"require\" first looks in the current
   working directory, then looks for package code under \"Pkg.dir()\",
   then tries paths in the global array \"LOAD_PATH\".

"),

("Base","reload","reload(file::AbstractString)

   Like \"require\", except forces loading of files regardless of
   whether they have been loaded before. Typically used when
   interactively developing libraries.

"),

("Base","include","include(path::AbstractString)

   Evaluate the contents of a source file in the current context.
   During including, a task-local include path is set to the directory
   containing the file. Nested calls to \"include\" will search
   relative to that path. All paths refer to files on node 1 when
   running in parallel, and files will be fetched from node 1. This
   function is typically used to load source interactively, or to
   combine files in packages that are broken into multiple source
   files.

"),

("Base","include_string","include_string(code::AbstractString)

   Like \"include\", except reads code from the given string rather
   than from a file. Since there is no file path involved, no path
   processing or fetching from node 1 is done.

"),

("Base","help","help(name)

   Get help for a function. \"name\" can be an object or a string.

"),

("Base","apropos","apropos(string)

   Search documentation for functions related to \"string\".

"),

("Base","which","which(f, types)

   Return the method of \"f\" (a \"Method\" object) that will be
   called for arguments with the given types.

"),

("Base","@which","@which()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"which\" function on the resulting expression

"),

("Base","methods","methods(f[, types])

   Show all methods of \"f\" with their argument types.

   If \"types\" is specified, an array of methods whose types match is
   returned.

"),

("Base","methodswith","methodswith(typ[, module or function][, showparents])

   Return an array of methods with an argument of type \"typ\". If
   optional \"showparents\" is \"true\", also return arguments with a
   parent type of \"typ\", excluding type \"Any\".

   The optional second argument restricts the search to a particular
   module or function.

"),

("Base","@show","@show()

   Show an expression and result, returning the result

"),

("Base","versioninfo","versioninfo([verbose::Bool])

   Print information about the version of Julia in use. If the
   \"verbose\" argument is true, detailed system information is shown
   as well.

"),

("Base","workspace","workspace()

   Replace the top-level module (\"Main\") with a new one, providing a
   clean workspace. The previous \"Main\" module is made available as
   \"LastMain\". A previously-loaded package can be accessed using a
   statement such as \"using LastMain.Package\".

   This function should only be used interactively.

"),

("Base","is","is(x, y) -> Bool
===(x, y) -> Bool
≡(x, y) -> Bool

   Determine whether \"x\" and \"y\" are identical, in the sense that
   no program could distinguish them. Compares mutable objects by
   address in memory, and compares immutable objects (such as numbers)
   by contents at the bit level. This function is sometimes called
   \"egal\".

"),

("Base","isa","isa(x, type) -> Bool

   Determine whether \"x\" is of the given \"type\".

"),

("Base","isequal","isequal(x, y)

   Similar to \"==\", except treats all floating-point \"NaN\" values
   as equal to each other, and treats \"-0.0\" as unequal to \"0.0\".
   The default implementation of \"isequal\" calls \"==\", so if you
   have a type that doesn't have these floating-point subtleties then
   you probably only need to define \"==\".

   \"isequal\" is the comparison function used by hash tables
   (\"Dict\"). \"isequal(x,y)\" must imply that \"hash(x) ==
   hash(y)\".

   This typically means that if you define your own \"==\" function
   then you must define a corresponding \"hash\" (and vice versa).
   Collections typically implement \"isequal\" by calling \"isequal\"
   recursively on all contents.

   Scalar types generally do not need to implement \"isequal\"
   separate from \"==\", unless they represent floating-point numbers
   amenable to a more efficient implementation than that provided as a
   generic fallback (based on \"isnan\", \"signbit\", and \"==\").

"),

("Base","isless","isless(x, y)

   Test whether \"x\" is less than \"y\", according to a canonical
   total order. Values that are normally unordered, such as \"NaN\",
   are ordered in an arbitrary but consistent fashion. This is the
   default comparison used by \"sort\". Non-numeric types with a
   canonical total order should implement this function. Numeric types
   only need to implement it if they have special values such as
   \"NaN\".

"),

("Base","ifelse","ifelse(condition::Bool, x, y)

   Return \"x\" if \"condition\" is true, otherwise return \"y\". This
   differs from \"?\" or \"if\" in that it is an ordinary function, so
   all the arguments are evaluated first. In some cases, using
   \"ifelse\" instead of an \"if\" statement can eliminate the branch
   in generated code and provide higher performance in tight loops.

"),

("Base","lexcmp","lexcmp(x, y)

   Compare \"x\" and \"y\" lexicographically and return -1, 0, or 1
   depending on whether \"x\" is less than, equal to, or greater than
   \"y\", respectively. This function should be defined for
   lexicographically comparable types, and \"lexless\" will call
   \"lexcmp\" by default.

"),

("Base","lexless","lexless(x, y)

   Determine whether \"x\" is lexicographically less than \"y\".

"),

("Base","typeof","typeof(x)

   Get the concrete type of \"x\".

"),

("Base","tuple","tuple(xs...)

   Construct a tuple of the given objects.

"),

("Base","ntuple","ntuple(n, f::Function)

   Create a tuple of length \"n\", computing each element as \"f(i)\",
   where \"i\" is the index of the element.

"),

("Base","object_id","object_id(x)

   Get a unique integer id for \"x\". \"object_id(x)==object_id(y)\"
   if and only if \"is(x,y)\".

"),

("Base","hash","hash(x[, h])

   Compute an integer hash code such that \"isequal(x,y)\" implies
   \"hash(x)==hash(y)\". The optional second argument \"h\" is a hash
   code to be mixed with the result.

   New types should implement the 2-argument form, typically  by
   calling the 2-argument \"hash\" method recursively in order to mix
   hashes of the contents with each other (and with \"h\").
   Typically, any type that implements \"hash\" should also implement
   its own \"==\" (hence \"isequal\") to guarantee the property
   mentioned above.

"),

("Base","finalizer","finalizer(x, function)

   Register a function \"f(x)\" to be called when there are no
   program-accessible references to \"x\". The behavior of this
   function is unpredictable if \"x\" is of a bits type.

"),

("Base","finalize","finalize(x)

   Immediately run finalizers registered for object \"x\".

"),

("Base","copy","copy(x)

   Create a shallow copy of \"x\": the outer structure is copied, but
   not all internal values. For example, copying an array produces a
   new array with identically-same elements as the original.

"),

("Base","deepcopy","deepcopy(x)

   Create a deep copy of \"x\": everything is copied recursively,
   resulting in a fully independent object. For example, deep-copying
   an array produces a new array whose elements are deep copies of the
   original elements. Calling *deepcopy* on an object should generally
   have the same effect as serializing and then deserializing it.

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

("Base","isdefined","isdefined([object], index | symbol)

   Tests whether an assignable location is defined. The arguments can
   be an array and index, a composite object and field name (as a
   symbol), or a module and a symbol. With a single symbol argument,
   tests whether a global variable with that name is defined in
   \"current_module()\".

"),

("Base","convert","convert(T, x)

   Convert \"x\" to a value of type \"T\".

   If \"T\" is an \"Integer\" type, an \"InexactError\" will be raised
   if \"x\" is not representable by \"T\", for example if \"x\" is not
   integer-valued, or is outside the range supported by \"T\".

      julia> convert(Int, 3.0)
      3

      julia> convert(Int, 3.5)
      ERROR: InexactError()
       in convert at int.jl:185

   If \"T\" is a \"FloatingPoint\" or \"Rational\" type, then it will
   return the closest value to \"x\" representable by \"T\".

      julia> x = 1/3
      0.3333333333333333

      julia> convert(Float32, x)
      0.33333334f0

      julia> convert(Rational{Int32}, x)
      1//3

      julia> convert(Rational{Int64}, x)
      6004799503160661//18014398509481984

"),

("Base","promote","promote(xs...)

   Convert all arguments to their common promotion type (if any), and
   return them all (as a tuple).

"),

("Base","oftype","oftype(x, y)

   Convert \"y\" to the type of \"x\" (\"convert(typeof(x), y)\").

"),

("Base","widen","widen(type | x)

   If the argument is a type, return a \"larger\" type (for numeric
   types, this will be a type with at least as much range and
   precision as the argument, and usually more). Otherwise the
   argument \"x\" is converted to \"widen(typeof(x))\".

      julia> widen(Int32)
      Int64

      julia> widen(1.5f0)
      1.5

"),

("Base","identity","identity(x)

   The identity function. Returns its argument.

"),

("Base","super","super(T::DataType)

   Return the supertype of DataType T

"),

("Base","issubtype","issubtype(type1, type2)

   True if and only if all values of \"type1\" are also of \"type2\".
   Can also be written using the \"<:\" infix operator as \"type1 <:
   type2\".

"),

("Base","<:","<:(T1, T2)

   Subtype operator, equivalent to \"issubtype(T1,T2)\".

"),

("Base","subtypes","subtypes(T::DataType)

   Return a list of immediate subtypes of DataType T.  Note that all
   currently loaded subtypes are included, including those not visible
   in the current module.

"),

("Base","subtypetree","subtypetree(T::DataType)

   Return a nested list of all subtypes of DataType T.  Note that all
   currently loaded subtypes are included, including those not visible
   in the current module.

"),

("Base","typemin","typemin(type)

   The lowest value representable by the given (real) numeric type.

"),

("Base","typemax","typemax(type)

   The highest value representable by the given (real) numeric type.

"),

("Base","realmin","realmin(type)

   The smallest in absolute value non-subnormal value representable by
   the given floating-point type

"),

("Base","realmax","realmax(type)

   The highest finite value representable by the given floating-point
   type

"),

("Base","maxintfloat","maxintfloat(type)

   The largest integer losslessly representable by the given floating-
   point type

"),

("Base","sizeof","sizeof(type)

   Size, in bytes, of the canonical binary representation of the given
   type, if any.

"),

("Base","eps","eps([type])

   The distance between 1.0 and the next larger representable
   floating-point value of \"type\". Only floating-point types are
   sensible arguments. If \"type\" is omitted, then \"eps(Float64)\"
   is returned.

"),

("Base","eps","eps(x)

   The distance between \"x\" and the next larger representable
   floating-point value of the same type as \"x\".

"),

("Base","promote_type","promote_type(type1, type2)

   Determine a type big enough to hold values of each argument type
   without loss, whenever possible. In some cases, where no type
   exists to which both types can be promoted losslessly, some loss is
   tolerated; for example, \"promote_type(Int64,Float64)\" returns
   \"Float64\" even though strictly, not all \"Int64\" values can be
   represented exactly as \"Float64\" values.

"),

("Base","promote_rule","promote_rule(type1, type2)

   Specifies what type should be used by \"promote\" when given values
   of types \"type1\" and \"type2\". This function should not be
   called directly, but should have definitions added to it for new
   types as appropriate.

"),

("Base","getfield","getfield(value, name::Symbol)

   Extract a named field from a value of composite type. The syntax
   \"a.b\" calls \"getfield(a, :b)\", and the syntax \"a.(b)\" calls
   \"getfield(a, b)\".

"),

("Base","setfield!","setfield!(value, name::Symbol, x)

   Assign \"x\" to a named field in \"value\" of composite type. The
   syntax \"a.b = c\" calls \"setfield!(a, :b, c)\", and the syntax
   \"a.(b) = c\" calls \"setfield!(a, b, c)\".

"),

("Base","fieldoffsets","fieldoffsets(type)

   The byte offset of each field of a type relative to the data start.
   For example, we could use it in the following manner to summarize
   information about a struct type:

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

"),

("Base","fieldtype","fieldtype(type, name::Symbol | index::Int)

   Determine the declared type of a field (specified by name or index)
   in a composite type.

"),

("Base","isimmutable","isimmutable(v)

   True if value \"v\" is immutable.  See *Immutable Composite Types*
   for a discussion of immutability. Note that this function works on
   values, so if you give it a type, it will tell you that a value of
   \"DataType\" is mutable.

"),

("Base","isbits","isbits(T)

   True if \"T\" is a \"plain data\" type, meaning it is immutable and
   contains no references to other values. Typical examples are
   numeric types such as \"UInt8\", \"Float64\", and
   \"Complex{Float64}\".

      julia> isbits(Complex{Float64})
      true

      julia> isbits(Complex)
      false

"),

("Base","isleaftype","isleaftype(T)

   Determine whether \"T\" is a concrete type that can have instances,
   meaning its only subtypes are itself and \"None\" (but \"T\" itself
   is not \"None\").

"),

("Base","typejoin","typejoin(T, S)

   Compute a type that contains both \"T\" and \"S\".

"),

("Base","typeintersect","typeintersect(T, S)

   Compute a type that contains the intersection of \"T\" and \"S\".
   Usually this will be the smallest such type or one close to it.

"),

("Base","apply","apply(f, x...)

   Accepts a function and several arguments, each of which must be
   iterable. The elements generated by all the arguments are appended
   into a single list, which is then passed to \"f\" as its argument
   list.

      julia> function f(x, y) # Define a function f
                 x + y
             end;

      julia> apply(f, [1 2]) # Apply f with 1 and 2 as arguments
      3

   \"apply\" is called to implement the \"...\" argument splicing
   syntax, and is usually not called directly: \"apply(f,x) ===
   f(x...)\"

"),

("Base","method_exists","method_exists(f, tuple) -> Bool

   Determine whether the given generic function has a method matching
   the given tuple of argument types.

      julia> method_exists(length, (Array,))
      true

"),

("Base","applicable","applicable(f, args...) -> Bool

   Determine whether the given generic function has a method
   applicable to the given arguments.

      julia> function f(x, y)
                 x + y
             end;

      julia> applicable(f, 1)
      false

      julia> applicable(f, 1, 2)
      true

"),

("Base","invoke","invoke(f, (types...), args...)

   Invoke a method for the given generic function matching the
   specified types (as a tuple), on the specified arguments. The
   arguments must be compatible with the specified types. This allows
   invoking a method other than the most specific matching method,
   which is useful when the behavior of a more general definition is
   explicitly needed (often as part of the implementation of a more
   specific method of the same function).

"),

("Base","|>","|>(x, f)

   Applies a function to the preceding argument. This allows for easy
   function chaining.

      julia> [1:5] |> x->x.^2 |> sum |> inv
      0.01818181818181818

"),

("Base","eval","eval([m::Module], expr::Expr)

   Evaluate an expression in the given module and return the result.
   Every module (except those defined with \"baremodule\") has its own
   1-argument definition of \"eval\", which evaluates expressions in
   that module.

"),

("Base","@eval","@eval()

   Evaluate an expression and return the value.

"),

("Base","evalfile","evalfile(path::AbstractString)

   Evaluate all expressions in the given file, and return the value of
   the last one. No other processing (path searching, fetching from
   node 1, etc.) is performed.

"),

("Base","esc","esc(e::ANY)

   Only valid in the context of an Expr returned from a macro.
   Prevents the macro hygiene pass from turning embedded variables
   into gensym variables. See the *Macros* section of the
   Metaprogramming chapter of the manual for more details and
   examples.

"),

("Base","gensym","gensym([tag])

   Generates a symbol which will not conflict with other variable
   names.

"),

("Base","@gensym","@gensym()

   Generates a gensym symbol for a variable. For example, \"@gensym x
   y\" is transformed into \"x = gensym(\"x\"); y = gensym(\"y\")\".

"),

("Base","parse","parse(str, start; greedy=true, raise=true)

   Parse the expression string and return an expression (which could
   later be passed to eval for execution). Start is the index of the
   first character to start parsing. If \"greedy\" is true (default),
   \"parse\" will try to consume as much input as it can; otherwise,
   it will stop as soon as it has parsed a valid expression. If
   \"raise\" is true (default), syntax errors will raise an error;
   otherwise, \"parse\" will return an expression that will raise an
   error upon evaluation.

"),

("Base","parse","parse(str; raise=true)

   Parse the whole string greedily, returning a single expression.  An
   error is thrown if there are additional characters after the first
   expression. If \"raise\" is true (default), syntax errors will
   raise an error; otherwise, \"parse\" will return an expression that
   will raise an error upon evaluation.

"),

("Base","start","start(iter) -> state

   Get initial iteration state for an iterable object

"),

("Base","done","done(iter, state) -> Bool

   Test whether we are done iterating

"),

("Base","next","next(iter, state) -> item, state

   For a given iterable object and iteration state, return the current
   item and the next iteration state

"),

("Base","zip","zip(iters...)

   For a set of iterable objects, returns an iterable of tuples, where
   the \"i\"th tuple contains the \"i\"th component of each input
   iterable.

   Note that \"zip\" is its own inverse: \"[zip(zip(a...)...)...] ==
   [a...]\".

"),

("Base","enumerate","enumerate(iter)

   Return an iterator that yields \"(i, x)\" where \"i\" is an index
   starting at 1, and \"x\" is the \"i\"th value from the given
   iterator. It's useful when you need not only the values \"x\" over
   which you are iterating, but also the index \"i\" of the
   iterations.

      julia> a = [\"a\", \"b\", \"c\"];

      julia> for (index, value) in enumerate(a)
                 println(\"\$index \$value\")
             end
      1 a
      2 b
      3 c

"),

("Base","isempty","isempty(collection) -> Bool

   Determine whether a collection is empty (has no elements).

      julia> isempty([])
      true

      julia> isempty([1 2 3])
      false

"),

("Base","empty!","empty!(collection) -> collection

   Remove all elements from a \"collection\".

"),

("Base","length","length(collection) -> Integer

   For ordered, indexable collections, the maximum index \"i\" for
   which \"getindex(collection, i)\" is valid. For unordered
   collections, the number of elements.

"),

("Base","endof","endof(collection) -> Integer

   Returns the last index of the collection.

      julia> endof([1,2,4])
      3

"),

("Base","in","in(item, collection) -> Bool
∈(item, collection) -> Bool
∋(collection, item) -> Bool
∉(item, collection) -> Bool
∌(collection, item) -> Bool

   Determine whether an item is in the given collection, in the sense
   that it is \"==\" to one of the values generated by iterating over
   the collection. Some collections need a slightly different
   definition; for example Sets check whether the item is \"isequal\"
   to one of the elements. Dicts look for \"(key,value)\" pairs, and
   the key is compared using \"isequal\". To test for the presence of
   a key in a dictionary, use \"haskey\" or \"k in keys(dict)\".

"),

("Base","eltype","eltype(collection)

   Determine the type of the elements generated by iterating
   \"collection\". For associative collections, this will be a
   \"(key,value)\" tuple type.

"),

("Base","indexin","indexin(a, b)

   Returns a vector containing the highest index in \"b\" for each
   value in \"a\" that is a member of \"b\" . The output vector
   contains 0 wherever \"a\" is not a member of \"b\".

"),

("Base","findin","findin(a, b)

   Returns the indices of elements in collection \"a\" that appear in
   collection \"b\"

"),

("Base","unique","unique(itr[, dim])

   Returns an array containing only the unique elements of the
   iterable \"itr\", in the order that the first of each set of
   equivalent elements originally appears. If \"dim\" is specified,
   returns unique regions of the array \"itr\" along \"dim\".

"),

("Base","reduce","reduce(op, v0, itr)

   Reduce the given collection \"ìtr\" with the given binary operator
   \"op\". \"v0\" must be a neutral element for \"op\" that will be
   returned for empty collections. It is unspecified whether \"v0\" is
   used for non-empty collections.

   Reductions for certain commonly-used operators have special
   implementations which should be used instead: \"maximum(itr)\",
   \"minimum(itr)\", \"sum(itr)\", \"prod(itr)\", \"any(itr)\",
   \"all(itr)\".

   The associativity of the reduction is implementation-dependent.
   This means that you can't use non-associative operations like \"-\"
   because it is undefined whether \"reduce(-,[1,2,3])\" should be
   evaluated as \"(1-2)-3\" or \"1-(2-3)\". Use \"foldl\" or \"foldr\"
   instead for guaranteed left or right associativity.

   Some operations accumulate error, and parallelism will also be
   easier if the reduction can be executed in groups. Future versions
   of Julia might change the algorithm. Note that the elements are not
   reordered if you use an ordered collection.

"),

("Base","reduce","reduce(op, itr)

   Like \"reduce(op, v0, itr)\". This cannot be used with empty
   collections, except for some special cases (e.g. when \"op\" is one
   of \"+\", \"*\", \"max\", \"min\", \"&\", \"|\") when Julia can
   determine the neutral element of \"op\".

"),

("Base","foldl","foldl(op, v0, itr)

   Like \"reduce\", but with guaranteed left associativity. \"v0\"
   will be used exactly once.

"),

("Base","foldl","foldl(op, itr)

   Like \"foldl(op, v0, itr)\", but using the first element of \"itr\"
   as \"v0\". In general, this cannot be used with empty collections
   (see \"reduce(op, itr)\").

"),

("Base","foldr","foldr(op, v0, itr)

   Like \"reduce\", but with guaranteed right associativity. \"v0\"
   will be used exactly once.

"),

("Base","foldr","foldr(op, itr)

   Like \"foldr(op, v0, itr)\", but using the last element of \"itr\"
   as \"v0\". In general, this cannot be used with empty collections
   (see \"reduce(op, itr)\").

"),

("Base","maximum","maximum(itr)

   Returns the largest element in a collection.

"),

("Base","maximum","maximum(A, dims)

   Compute the maximum value of an array over the given dimensions.

"),

("Base","maximum!","maximum!(r, A)

   Compute the maximum value of \"A\" over the singleton dimensions of
   \"r\", and write results to \"r\".

"),

("Base","minimum","minimum(itr)

   Returns the smallest element in a collection.

"),

("Base","minimum","minimum(A, dims)

   Compute the minimum value of an array over the given dimensions.

"),

("Base","minimum!","minimum!(r, A)

   Compute the minimum value of \"A\" over the singleton dimensions of
   \"r\", and write results to \"r\".

"),

("Base","extrema","extrema(itr)

   Compute both the minimum and maximum element in a single pass, and
   return them as a 2-tuple.

"),

("Base","indmax","indmax(itr) -> Integer

   Returns the index of the maximum element in a collection.

"),

("Base","indmin","indmin(itr) -> Integer

   Returns the index of the minimum element in a collection.

"),

("Base","findmax","findmax(itr) -> (x, index)

   Returns the maximum element and its index.

"),

("Base","findmax","findmax(A, dims) -> (maxval, index)

   For an array input, returns the value and index of the maximum over
   the given dimensions.

"),

("Base","findmin","findmin(itr) -> (x, index)

   Returns the minimum element and its index.

"),

("Base","findmin","findmin(A, dims) -> (minval, index)

   For an array input, returns the value and index of the minimum over
   the given dimensions.

"),

("Base","maxabs","maxabs(itr)

   Compute the maximum absolute value of a collection of values.

"),

("Base","maxabs","maxabs(A, dims)

   Compute the maximum absolute values over given dimensions.

"),

("Base","maxabs!","maxabs!(r, A)

   Compute the maximum absolute values over the singleton dimensions
   of \"r\", and write values to \"r\".

"),

("Base","minabs","minabs(itr)

   Compute the minimum absolute value of a collection of values.

"),

("Base","minabs","minabs(A, dims)

   Compute the minimum absolute values over given dimensions.

"),

("Base","minabs!","minabs!(r, A)

   Compute the minimum absolute values over the singleton dimensions
   of \"r\", and write values to \"r\".

"),

("Base","sum","sum(itr)

   Returns the sum of all elements in a collection.

"),

("Base","sum","sum(A, dims)

   Sum elements of an array over the given dimensions.

"),

("Base","sum!","sum!(r, A)

   Sum elements of \"A\" over the singleton dimensions of \"r\", and
   write results to \"r\".

"),

("Base","sum","sum(f, itr)

   Sum the results of calling function \"f\" on each element of
   \"itr\".

"),

("Base","sumabs","sumabs(itr)

   Sum absolute values of all elements in a collection. This is
   equivalent to *sum(abs(itr))* but faster.

"),

("Base","sumabs","sumabs(A, dims)

   Sum absolute values of elements of an array over the given
   dimensions.

"),

("Base","sumabs!","sumabs!(r, A)

   Sum absolute values of elements of \"A\" over the singleton
   dimensions of \"r\", and write results to \"r\".

"),

("Base","sumabs2","sumabs2(itr)

   Sum squared absolute values of all elements in a collection. This
   is equivalent to *sum(abs2(itr))* but faster.

"),

("Base","sumabs2","sumabs2(A, dims)

   Sum squared absolute values of elements of an array over the given
   dimensions.

"),

("Base","sumabs2!","sumabs2!(r, A)

   Sum squared absolute values of elements of \"A\" over the singleton
   dimensions of \"r\", and write results to \"r\".

"),

("Base","prod","prod(itr)

   Returns the product of all elements of a collection.

"),

("Base","prod","prod(A, dims)

   Multiply elements of an array over the given dimensions.

"),

("Base","prod!","prod!(r, A)

   Multiply elements of \"A\" over the singleton dimensions of \"r\",
   and write results to \"r\".

"),

("Base","any","any(itr) -> Bool

   Test whether any elements of a boolean collection are true.

"),

("Base","any","any(A, dims)

   Test whether any values along the given dimensions of an array are
   true.

"),

("Base","any!","any!(r, A)

   Test whether any values in \"A\" along the singleton dimensions of
   \"r\" are true, and write results to \"r\".

"),

("Base","all","all(itr) -> Bool

   Test whether all elements of a boolean collection are true.

"),

("Base","all","all(A, dims)

   Test whether all values along the given dimensions of an array are
   true.

"),

("Base","all!","all!(r, A)

   Test whether all values in \"A\" along the singleton dimensions of
   \"r\" are true, and write results to \"r\".

"),

("Base","count","count(p, itr) -> Integer

   Count the number of elements in \"itr\" for which predicate \"p\"
   returns true.

"),

("Base","any","any(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for any elements of
   \"itr\".

"),

("Base","all","all(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for all elements of
   \"itr\".

      julia> all(i->(4<=i<=6), [4,5,6])
      true

"),

("Base","map","map(f, c...) -> collection

   Transform collection \"c\" by applying \"f\" to each element. For
   multiple collection arguments, apply \"f\" elementwise.

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

"),

("Base","map!","map!(function, collection)

   In-place version of \"map()\".

"),

("Base","map!","map!(function, destination, collection...)

   Like \"map()\", but stores the result in \"destination\" rather
   than a new collection. \"destination\" must be at least as large as
   the first collection.

"),

("Base","mapreduce","mapreduce(f, op, v0, itr)

   Apply function \"f\" to each element in \"itr\", and then reduce
   the result using the binary function \"op\". \"v0\" must be a
   neutral element for \"op\" that will be returned for empty
   collections. It is unspecified whether \"v0\" is used for non-empty
   collections.

   \"mapreduce\" is functionally equivalent to calling \"reduce(op,
   v0, map(f, itr))\", but will in general execute faster since no
   intermediate collection needs to be created. See documentation for
   \"reduce\" and \"map\".

      julia> mapreduce(x->x^2, +, [1:3]) # == 1 + 4 + 9
      14

   The associativity of the reduction is implementation-dependent. Use
   \"mapfoldl\" or \"mapfoldr\" instead for guaranteed left or right
   associativity.

"),

("Base","mapreduce","mapreduce(f, op, itr)

   Like \"mapreduce(f, op, v0, itr)\". In general, this cannot be used
   with empty collections (see \"reduce(op, itr)\").

"),

("Base","mapfoldl","mapfoldl(f, op, v0, itr)

   Like \"mapreduce\", but with guaranteed left associativity. \"v0\"
   will be used exactly once.

"),

("Base","mapfoldl","mapfoldl(f, op, itr)

   Like \"mapfoldl(f, op, v0, itr)\", but using the first element of
   \"itr\" as \"v0\". In general, this cannot be used with empty
   collections (see \"reduce(op, itr)\").

"),

("Base","mapfoldr","mapfoldr(f, op, v0, itr)

   Like \"mapreduce\", but with guaranteed right associativity. \"v0\"
   will be used exactly once.

"),

("Base","mapfoldr","mapfoldr(f, op, itr)

   Like \"mapfoldr(f, op, v0, itr)\", but using the first element of
   \"itr\" as \"v0\". In general, this cannot be used with empty
   collections (see \"reduce(op, itr)\").

"),

("Base","first","first(coll)

   Get the first element of an iterable collection. Returns the start
   point of a \"Range\" even if it is empty.

"),

("Base","last","last(coll)

   Get the last element of an ordered collection, if it can be
   computed in O(1) time. This is accomplished by calling \"endof\" to
   get the last index. Returns the end point of a \"Range\" even if it
   is empty.

"),

("Base","step","step(r)

   Get the step size of a \"Range\" object.

"),

("Base","collect","collect(collection)

   Return an array of all items in a collection. For associative
   collections, returns (key, value) tuples.

"),

("Base","collect","collect(element_type, collection)

   Return an array of type \"Array{element_type,1}\" of all items in a
   collection.

"),

("Base","issubset","issubset(a, b)
⊆(A, S) -> Bool
⊈(A, S) -> Bool
⊊(A, S) -> Bool

   Determine whether every element of \"a\" is also in \"b\", using
   the \"in\" function.

"),

("Base","filter","filter(function, collection)

   Return a copy of \"collection\", removing elements for which
   \"function\" is false. For associative collections, the function is
   passed two arguments (key and value).

"),

("Base","filter!","filter!(function, collection)

   Update \"collection\", removing elements for which \"function\" is
   false. For associative collections, the function is passed two
   arguments (key and value).

"),

("Base","getindex","getindex(collection, key...)

   Retrieve the value(s) stored at the given key or index within a
   collection. The syntax \"a[i,j,...]\" is converted by the compiler
   to \"getindex(a, i, j, ...)\".

"),

("Base","setindex!","setindex!(collection, value, key...)

   Store the given value at the given key or index within a
   collection. The syntax \"a[i,j,...] = x\" is converted by the
   compiler to \"setindex!(a, x, i, j, ...)\".

"),

("Base","Dict","Dict([itr])

   \"Dict{K,V}()\" constructs a hash table with keys of type K and
   values of type V. Given a single iterable argument, constructs a
   \"Dict\" whose key-value pairs are taken from 2-tuples
   \"(key,value)\" generated by the argument.

   Alternatively, a sequence of pair arguments may be passed:
   \"Dict{K,V}(\"A\"=>1, \"B\"=>2)\".

"),

("Base","haskey","haskey(collection, key) -> Bool

   Determine whether a collection has a mapping for a given key.

"),

("Base","get","get(collection, key, default)

   Return the value stored for the given key, or the given default
   value if no mapping for the key is present.

"),

("Base","get","get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, return \"f()\".  Use \"get!\" to also store the
   default value in the dictionary.

   This is intended to be called using \"do\" block syntax:

      get(dict, key) do
          # default value calculated here
               time()
      end

"),

("Base","get!","get!(collection, key, default)

   Return the value stored for the given key, or if no mapping for the
   key is present, store \"key => default\", and return \"default\".

"),

("Base","get!","get!(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, store \"key => f()\", and return \"f()\".

   This is intended to be called using \"do\" block syntax:

      get!(dict, key) do
          # default value calculated here
               time()
      end

"),

("Base","getkey","getkey(collection, key, default)

   Return the key matching argument \"key\" if one exists in
   \"collection\", otherwise return \"default\".

"),

("Base","delete!","delete!(collection, key)

   Delete the mapping for the given key in a collection, and return
   the collection.

"),

("Base","pop!","pop!(collection, key[, default])

   Delete and return the mapping for \"key\" if it exists in
   \"collection\", otherwise return \"default\", or throw an error if
   default is not specified.

"),

("Base","keys","keys(collection)

   Return an iterator over all keys in a collection.
   \"collect(keys(d))\" returns an array of keys.

"),

("Base","values","values(collection)

   Return an iterator over all values in a collection.
   \"collect(values(d))\" returns an array of values.

"),

("Base","merge","merge(collection, others...)

   Construct a merged collection from the given collections. If
   necessary, the types of the resulting collection will be promoted
   to accommodate the types of the merged collections:

      julia> a = Dict(\"foo\" => 0.0, \"bar\" => 42.0)
      Dict{ASCIIString,Float64} with 2 entries:
        \"bar\" => 42.0
        \"foo\" => 0.0

      julia> b = Dict(utf8(\"baz\") => 17, utf8(\"qux\") => 4711)
      Dict{UTF8String,Int64} with 2 entries:
        \"baz\" => 17
        \"qux\" => 4711

      julia> merge(a, b)
      Dict{UTF8String,Float64} with 4 entries:
        \"qux\" => 4711.0
        \"bar\" => 42.0
        \"baz\" => 17.0
        \"foo\" => 0.0

"),

("Base","merge!","merge!(collection, others...)

   Update collection with pairs from the other collections

"),

("Base","sizehint!","sizehint!(s, n)

   Suggest that collection \"s\" reserve capacity for at least \"n\"
   elements. This can improve performance.

"),

("Base","Set","Set([itr])

   Construct a \"Set\" of the values generated by the given iterable
   object, or an empty set. Should be used instead of \"IntSet\" for
   sparse integer sets, or for sets of arbitrary objects.

"),

("Base","IntSet","IntSet([itr])

   Construct a sorted set of the integers generated by the given
   iterable object, or an empty set. Implemented as a bit string, and
   therefore designed for dense integer sets. Only non-negative
   integers can be stored. If the set will be sparse (for example
   holding a single very large integer), use \"Set\" instead.

"),

("Base","union","union(s1, s2...)
∪(s1, s2)

   Construct the union of two or more sets. Maintains order with
   arrays.

"),

("Base","union!","union!(s, iterable)

   Union each element of \"iterable\" into set \"s\" in-place.

"),

("Base","intersect","intersect(s1, s2...)
∩(s1, s2)

   Construct the intersection of two or more sets. Maintains order and
   multiplicity of the first argument for arrays and ranges.

"),

("Base","setdiff","setdiff(s1, s2)

   Construct the set of elements in \"s1\" but not \"s2\". Maintains
   order with arrays. Note that both arguments must be collections,
   and both will be iterated over. In particular,
   \"setdiff(set,element)\" where \"element\" is a potential member of
   \"set\", will not work in general.

"),

("Base","setdiff!","setdiff!(s, iterable)

   Remove each element of \"iterable\" from set \"s\" in-place.

"),

("Base","symdiff","symdiff(s1, s2...)

   Construct the symmetric difference of elements in the passed in
   sets or arrays. Maintains order with arrays.

"),

("Base","symdiff!","symdiff!(s, n)

   IntSet s is destructively modified to toggle the inclusion of
   integer \"n\".

"),

("Base","symdiff!","symdiff!(s, itr)

   For each element in \"itr\", destructively toggle its inclusion in
   set \"s\".

"),

("Base","symdiff!","symdiff!(s1, s2)

   Construct the symmetric difference of IntSets \"s1\" and \"s2\",
   storing the result in \"s1\".

"),

("Base","complement","complement(s)

   Returns the set-complement of IntSet \"s\".

"),

("Base","complement!","complement!(s)

   Mutates IntSet \"s\" into its set-complement.

"),

("Base","intersect!","intersect!(s1, s2)

   Intersects IntSets \"s1\" and \"s2\" and overwrites the set \"s1\"
   with the result. If needed, s1 will be expanded to the size of
   \"s2\".

"),

("Base","issubset","issubset(A, S) -> Bool
⊆(A, S) -> Bool

   True if A is a subset of or equal to S.

"),

("Base","push!","push!(collection, items...) -> collection

   Insert items at the end of a collection.

"),

("Base","pop!","pop!(collection) -> item

   Remove the last item in a collection and return it.

"),

("Base","unshift!","unshift!(collection, items...) -> collection

   Insert items at the beginning of a collection.

"),

("Base","shift!","shift!(collection) -> item

   Remove the first item in a collection.

"),

("Base","insert!","insert!(collection, index, item)

   Insert an item at the given index.

"),

("Base","deleteat!","deleteat!(collection, index)

   Remove the item at the given index, and return the modified
   collection. Subsequent items are shifted to fill the resulting gap.

"),

("Base","deleteat!","deleteat!(collection, itr)

   Remove the items at the indices given by \"itr\", and return the
   modified collection. Subsequent items are shifted to fill the
   resulting gap.  \"itr\" must be sorted and unique.

"),

("Base","splice!","splice!(collection, index[, replacement]) -> item

   Remove the item at the given index, and return the removed item.
   Subsequent items are shifted down to fill the resulting gap. If
   specified, replacement values from an ordered collection will be
   spliced in place of the removed item.

   To insert \"replacement\" before an index \"n\" without removing
   any items, use \"splice!(collection, n:n-1, replacement)\".

"),

("Base","splice!","splice!(collection, range[, replacement]) -> items

   Remove items in the specified index range, and return a collection
   containing the removed items. Subsequent items are shifted down to
   fill the resulting gap. If specified, replacement values from an
   ordered collection will be spliced in place of the removed items.

   To insert \"replacement\" before an index \"n\" without removing
   any items, use \"splice!(collection, n:n-1, replacement)\".

"),

("Base","resize!","resize!(collection, n) -> collection

   Resize collection to contain \"n\" elements.

"),

("Base","append!","append!(collection, items) -> collection.

   Add the elements of \"items\" to the end of a collection.

      julia> append!([1],[2,3])
      3-element Array{Int64,1}:
       1
       2
       3

"),

("Base","prepend!","prepend!(collection, items) -> collection

   Insert the elements of \"items\" to the beginning of a collection.

      julia> prepend!([3],[1,2])
      3-element Array{Int64,1}:
       1
       2
       3

"),

("Base","get","get(x)

   Attempt to access the value of the \"Nullable\" object, \"x\".
   Returns the value if it is present; otherwise, throws a
   \"NullException\".

"),

("Base","get","get(x, y)

   Attempt to access the value of the \"Nullable{T}\" object, \"x\".
   Returns the value if it is present; otherwise, returns \"convert(T,
   y)\".

"),

("Base","isnull","isnull(x)

   Does the \"Nullable\" object \"x\" have a value or not?

"),

("Base","length","length(s)

   The number of characters in string \"s\".

"),

("Base","sizeof","sizeof(s::AbstractString)

   The number of bytes in string \"s\".

"),

("Base","*","*(s, t)

   Concatenate strings. The \"*\" operator is an alias to this
   function.

      julia> \"Hello \" * \"world\"
      \"Hello world\"

"),

("Base","^","^(s, n)

   Repeat \"n\" times the string \"s\". The \"^\" operator is an alias
   to this function.

      julia> \"Test \"^3
      \"Test Test Test \"

"),

("Base","string","string(xs...)

   Create a string from any values using the \"print\" function.

"),

("Base","repr","repr(x)

   Create a string from any value using the \"showall\" function.

"),

("Base","bytestring","bytestring(::Ptr{UInt8}[, length])

   Create a string from the address of a C (0-terminated) string
   encoded in ASCII or UTF-8. A copy is made; the ptr can be safely
   freed. If \"length\" is specified, the string does not have to be
   0-terminated.

"),

("Base","bytestring","bytestring(s)

   Convert a string to a contiguous byte array representation
   appropriate for passing it to C functions. The string will be
   encoded as either ASCII or UTF-8.

"),

("Base","ascii","ascii(::Array{UInt8, 1})

   Create an ASCII string from a byte array.

"),

("Base","ascii","ascii(s)

   Convert a string to a contiguous ASCII string (all characters must
   be valid ASCII characters).

"),

("Base","utf8","utf8(::Array{UInt8, 1})

   Create a UTF-8 string from a byte array.

"),

("Base","utf8","utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must
   be valid UTF-8 characters).

"),

("Base","normalize_string","normalize_string(s, normalform::Symbol)

   Normalize the string \"s\" according to one of the four \"normal
   forms\" of the Unicode standard: \"normalform\" can be \":NFC\",
   \":NFD\", \":NFKC\", or \":NFKD\".  Normal forms C (canonical
   composition) and D (canonical decomposition) convert different
   visually identical representations of the same abstract string into
   a single canonical form, with form C being more compact.  Normal
   forms KC and KD additionally canonicalize \"compatibility
   equivalents\": they convert characters that are abstractly similar
   but visually distinct into a single canonical choice (e.g. they
   expand ligatures into the individual characters), with form KC
   being more compact.

   Alternatively, finer control and additional transformations may be
   be obtained by calling *normalize_string(s; keywords...)*, where
   any number of the following boolean keywords options (which all
   default to \"false\" except for \"compose\") are specified:

   * \"compose=false\": do not perform canonical composition

   * \"decompose=true\": do canonical decomposition instead of
     canonical composition (\"compose=true\" is ignored if present)

   * \"compat=true\": compatibility equivalents are canonicalized

   * \"casefold=true\": perform Unicode case folding, e.g. for case-
     insensitive string comparison

   * \"newline2lf=true\", \"newline2ls=true\", or
     \"newline2ps=true\": convert various newline sequences (LF, CRLF,
     CR, NEL) into a linefeed (LF), line-separation (LS), or
     paragraph-separation (PS) character, respectively

   * \"stripmark=true\": strip diacritical marks (e.g. accents)

   * \"stripignore=true\": strip Unicode's \"default ignorable\"
     characters (e.g. the soft hyphen or the left-to-right marker)

   * \"stripcc=true\": strip control characters; horizontal tabs and
     form feeds are converted to spaces; newlines are also converted
     to spaces unless a newline-conversion flag was specified

   * \"rejectna=true\": throw an error if unassigned code points are
     found

   * \"stable=true\": enforce Unicode Versioning Stability

   For example, NFKC corresponds to the options \"compose=true,
   compat=true, stable=true\".

"),

("Base","is_valid_ascii","is_valid_ascii(s) -> Bool

   Returns true if the argument (\"ASCIIString\", \"UTF8String\", or
   byte vector) is valid ASCII, false otherwise.

"),

("Base","is_valid_utf8","is_valid_utf8(s) -> Bool

   Returns true if the argument (\"ASCIIString\", \"UTF8String\", or
   byte vector) is valid UTF-8, false otherwise.

"),

("Base","is_valid_char","is_valid_char(c) -> Bool

   Returns true if the given char or integer is a valid Unicode code
   point.

"),

("Base","is_assigned_char","is_assigned_char(c) -> Bool

   Returns true if the given char or integer is an assigned Unicode
   code point.

"),

("Base","ismatch","ismatch(r::Regex, s::AbstractString) -> Bool

   Test whether a string contains a match of the given regular
   expression.

"),

("Base","match","match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

   Search for the first match of the regular expression \"r\" in \"s\"
   and return a RegexMatch object containing the match, or nothing if
   the match failed. The matching substring can be retrieved by
   accessing \"m.match\" and the captured sequences can be retrieved
   by accessing \"m.captures\" The optional \"idx\" argument specifies
   an index at which to start the search.

"),

("Base","eachmatch","eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

   Search for all matches of a the regular expression \"r\" in \"s\"
   and return a iterator over the matches. If overlap is true, the
   matching sequences are allowed to overlap indices in the original
   string, otherwise they must be from distinct character ranges.

"),

("Base","matchall","matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

   Return a vector of the matching substrings from eachmatch.

"),

("Base","lpad","lpad(string, n, p)

   Make a string at least \"n\" characters long by padding on the left
   with copies of \"p\".

"),

("Base","rpad","rpad(string, n, p)

   Make a string at least \"n\" characters long by padding on the
   right with copies of \"p\".

"),

("Base","search","search(string, chars[, start])

   Search for the first occurrence of the given characters within the
   given string. The second argument may be a single character, a
   vector or a set of characters, a string, or a regular expression
   (though regular expressions are only allowed on contiguous strings,
   such as ASCII or UTF-8 strings). The third argument optionally
   specifies a starting index. The return value is a range of indexes
   where the matching sequence is found, such that \"s[search(s,x)] ==
   x\":

   \"search(string, \"substring\")\" = \"start:end\" such that
   \"string[start:end] == \"substring\"\", or \"0:-1\" if unmatched.

   \"search(string, 'c')\"         = \"index\" such that
   \"string[index] == 'c'\", or \"0\" if unmatched.

"),

("Base","rsearch","rsearch(string, chars[, start])

   Similar to \"search\", but returning the last occurrence of the
   given characters within the given string, searching in reverse from
   \"start\".

"),

("Base","searchindex","searchindex(string, substring[, start])

   Similar to \"search\", but return only the start index at which the
   substring is found, or 0 if it is not.

"),

("Base","rsearchindex","rsearchindex(string, substring[, start])

   Similar to \"rsearch\", but return only the start index at which
   the substring is found, or 0 if it is not.

"),

("Base","contains","contains(haystack, needle)

   Determine whether the second argument is a substring of the first.

"),

("Base","replace","replace(string, pat, r[, n])

   Search for the given pattern \"pat\", and replace each occurrence
   with \"r\". If \"n\" is provided, replace at most \"n\"
   occurrences.  As with search, the second argument may be a single
   character, a vector or a set of characters, a string, or a regular
   expression. If \"r\" is a function, each occurrence is replaced
   with \"r(s)\" where \"s\" is the matched substring.

"),

("Base","split","split(string, [chars]; limit=0, keep=true)

   Return an array of substrings by splitting the given string on
   occurrences of the given character delimiters, which may be
   specified in any of the formats allowed by \"search\"'s second
   argument (i.e. a single character, collection of characters,
   string, or regular expression). If \"chars\" is omitted, it
   defaults to the set of all space characters, and \"keep\" is taken
   to be false. The two keyword arguments are optional: they are are a
   maximum size for the result and a flag determining whether empty
   fields should be kept in the result.

"),

("Base","rsplit","rsplit(string, [chars]; limit=0, keep=true)

   Similar to \"split\", but starting from the end of the string.

"),

("Base","strip","strip(string[, chars])

   Return \"string\" with any leading and trailing whitespace removed.
   If \"chars\" (a character, or vector or set of characters) is
   provided, instead remove characters contained in it.

"),

("Base","lstrip","lstrip(string[, chars])

   Return \"string\" with any leading whitespace removed. If \"chars\"
   (a character, or vector or set of characters) is provided, instead
   remove characters contained in it.

"),

("Base","rstrip","rstrip(string[, chars])

   Return \"string\" with any trailing whitespace removed. If
   \"chars\" (a character, or vector or set of characters) is
   provided, instead remove characters contained in it.

"),

("Base","beginswith","beginswith(string, prefix | chars)

   Returns \"true\" if \"string\" starts with \"prefix\". If the
   second argument is a vector or set of characters, tests whether the
   first character of \"string\" belongs to that set.

"),

("Base","endswith","endswith(string, suffix | chars)

   Returns \"true\" if \"string\" ends with \"suffix\". If the second
   argument is a vector or set of characters, tests whether the last
   character of \"string\" belongs to that set.

"),

("Base","uppercase","uppercase(string)

   Returns \"string\" with all characters converted to uppercase.

"),

("Base","lowercase","lowercase(string)

   Returns \"string\" with all characters converted to lowercase.

"),

("Base","ucfirst","ucfirst(string)

   Returns \"string\" with the first character converted to uppercase.

"),

("Base","lcfirst","lcfirst(string)

   Returns \"string\" with the first character converted to lowercase.

"),

("Base","join","join(strings, delim[, last])

   Join an array of \"strings\" into a single string, inserting the
   given delimiter between adjacent strings. If \"last\" is given, it
   will be used instead of \"delim\" between the last two strings. For
   example, \"join([\"apples\", \"bananas\", \"pineapples\"], \", \",
   \" and \") == \"apples, bananas and pineapples\"\".

   \"strings\" can be any iterable over elements \"x\" which are
   convertible to strings via \"print(io::IOBuffer, x)\".

"),

("Base","chop","chop(string)

   Remove the last character from a string

"),

("Base","chomp","chomp(string)

   Remove a trailing newline from a string

"),

("Base","ind2chr","ind2chr(string, i)

   Convert a byte index to a character index

"),

("Base","chr2ind","chr2ind(string, i)

   Convert a character index to a byte index

"),

("Base","isvalid","isvalid(str, i)

   Tells whether index \"i\" is valid for the given string

"),

("Base","nextind","nextind(str, i)

   Get the next valid string index after \"i\". Returns a value
   greater than \"endof(str)\" at or after the end of the string.

"),

("Base","prevind","prevind(str, i)

   Get the previous valid string index before \"i\". Returns a value
   less than \"1\" at the beginning of the string.

"),

("Base","randstring","randstring(len)

   Create a random ASCII string of length \"len\", consisting of
   upper- and lower-case letters and the digits 0-9

"),

("Base","charwidth","charwidth(c)

   Gives the number of columns needed to print a character.

"),

("Base","strwidth","strwidth(s)

   Gives the number of columns needed to print a string.

"),

("Base","isalnum","isalnum(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is alphanumeric, or whether this is true
   for all elements of a string.  A character is classified as
   alphabetic if it belongs to the Unicode general category Letter or
   Number, i.e. a character whose category code begins with 'L' or
   'N'.

"),

("Base","isalpha","isalpha(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is alphabetic, or whether this is true
   for all elements of a string. A character is classified as
   alphabetic if it belongs to the Unicode general category Letter,
   i.e. a character whose category code begins with 'L'.

"),

("Base","isascii","isascii(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character belongs to the ASCII character set, or
   whether this is true for all elements of a string.

"),

("Base","iscntrl","iscntrl(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is a control character, or whether this
   is true for all elements of a string.  Control characters are the
   non-printing characters of the Latin-1 subset of Unicode.

"),

("Base","isdigit","isdigit(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is a numeric digit (0-9), or whether this
   is true for all elements of a string.

"),

("Base","isgraph","isgraph(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is printable, and not a space, or whether
   this is true for all elements of a string.  Any character that
   would cause a printer to use ink should be classified with
   isgraph(c)==true.

"),

("Base","islower","islower(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is a lowercase letter, or whether this is
   true for all elements of a string.  A character is classified as
   lowercase if it belongs to Unicode category Ll, Letter: Lowercase.

"),

("Base","isnumber","isnumber(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is numeric, or whether this is true for
   all elements of a string.   A character is classified as numeric if
   it belongs to the Unicode general category Number, i.e. a character
   whose category code begins with 'N'.

"),

("Base","isprint","isprint(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is printable, including spaces, but not a
   control character. For strings, tests whether this is true for all
   elements of the string.

"),

("Base","ispunct","ispunct(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character belongs to the Unicode general category
   Punctuation, i.e. a character whose category code begins with 'P'.
   For strings, tests whether this is true for all elements of the
   string.

"),

("Base","isspace","isspace(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is any whitespace character.  Includes
   ASCII characters '\\t', '\\n', '\\v', '\\f', '\\r', and ' ',
   Latin-1 character U+0085, and characters in Unicode category Zs.
   For strings, tests whether this    is true for all elements of the
   string.

"),

("Base","isupper","isupper(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is an uppercase letter, or whether this
   is true for all elements of a string.    A character is classified
   as uppercase if it belongs to Unicode category Lu, Letter:
   Uppercase, or Lt, Letter: Titlecase.

"),

("Base","isxdigit","isxdigit(c::Union(Char, AbstractString)) -> Bool

   Tests whether a character is a valid hexadecimal digit, or whether
   this is true for all elements of a string.

"),

("Base","symbol","symbol(str) -> Symbol

   Convert a string to a \"Symbol\".

"),

("Base","escape_string","escape_string(str::AbstractString) -> AbstractString

   General escaping of traditional C and Unicode escape sequences. See
   \"print_escaped()\" for more general escaping.

"),

("Base","unescape_string","unescape_string(s::AbstractString) -> AbstractString

   General unescaping of traditional C and Unicode escape sequences.
   Reverse of \"escape_string()\". See also \"print_unescaped()\".

"),

("Base","utf16","utf16(s)

   Create a UTF-16 string from a byte array, array of \"UInt16\", or
   any other string type.  (Data must be valid UTF-16.  Conversions of
   byte arrays check for a byte-order marker in the first two bytes,
   and do not include it in the resulting string.)

   Note that the resulting \"UTF16String\" data is terminated by the
   NUL codepoint (16-bit zero), which is not treated as a character in
   the string (so that it is mostly invisible in Julia); this allows
   the string to be passed directly to external functions requiring
   NUL-terminated data.  This NUL is appended automatically by the
   *utf16(s)* conversion function.  If you have a \"UInt16\" array
   \"A\" that is already NUL-terminated valid UTF-16 data, then you
   can instead use *UTF16String(A)`* to construct the string without
   making a copy of the data and treating the NUL as a terminator
   rather than as part of the string.

"),

("Base","utf16","utf16(::Union(Ptr{UInt16}, Ptr{Int16})[, length])

   Create a string from the address of a NUL-terminated UTF-16 string.
   A copy is made; the pointer can be safely freed. If \"length\" is
   specified, the string does not have to be NUL-terminated.

"),

("Base","is_valid_utf16","is_valid_utf16(s) -> Bool

   Returns true if the argument (\"UTF16String\" or \"UInt16\" array)
   is valid UTF-16.

"),

("Base","utf32","utf32(s)

   Create a UTF-32 string from a byte array, array of \"UInt32\", or
   any other string type.  (Conversions of byte arrays check for a
   byte-order marker in the first four bytes, and do not include it in
   the resulting string.)

   Note that the resulting \"UTF32String\" data is terminated by the
   NUL codepoint (32-bit zero), which is not treated as a character in
   the string (so that it is mostly invisible in Julia); this allows
   the string to be passed directly to external functions requiring
   NUL-terminated data.  This NUL is appended automatically by the
   *utf32(s)* conversion function.  If you have a \"UInt32\" array
   \"A\" that is already NUL-terminated UTF-32 data, then you can
   instead use *UTF32String(A)`* to construct the string without
   making a copy of the data and treating the NUL as a terminator
   rather than as part of the string.

"),

("Base","utf32","utf32(::Union(Ptr{Char}, Ptr{UInt32}, Ptr{Int32})[, length])

   Create a string from the address of a NUL-terminated UTF-32 string.
   A copy is made; the pointer can be safely freed. If \"length\" is
   specified, the string does not have to be NUL-terminated.

"),

("Base","wstring","wstring(s)

   This is a synonym for either \"utf32(s)\" or \"utf16(s)\",
   depending on whether \"Cwchar_t\" is 32 or 16 bits, respectively.
   The synonym \"WString\" for \"UTF32String\" or \"UTF16String\" is
   also provided.

"),

("Base","STDOUT","STDOUT

   Global variable referring to the standard out stream.

"),

("Base","STDERR","STDERR

   Global variable referring to the standard error stream.

"),

("Base","STDIN","STDIN

   Global variable referring to the standard input stream.

"),

("Base","open","open(file_name[, read, write, create, truncate, append]) -> IOStream

   Open a file in a mode specified by five boolean arguments. The
   default is to open files for reading only. Returns a stream for
   accessing the file.

"),

("Base","open","open(file_name[, mode]) -> IOStream

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

("Base","open","open(f::function, args...)

   Apply the function \"f\" to the result of \"open(args...)\" and
   close the resulting file descriptor upon completion.

   **Example**: \"open(readall, \"file.txt\")\"

"),

("Base","IOBuffer","IOBuffer() -> IOBuffer

   Create an in-memory I/O stream.

"),

("Base","IOBuffer","IOBuffer(size::Int)

   Create a fixed size IOBuffer. The buffer will not grow dynamically.

"),

("Base","IOBuffer","IOBuffer(string)

   Create a read-only IOBuffer on the data underlying the given string

"),

("Base","IOBuffer","IOBuffer([data][, readable, writable[, maxsize]])

   Create an IOBuffer, which may optionally operate on a pre-existing
   array. If the readable/writable arguments are given, they restrict
   whether or not the buffer may be read from or written to
   respectively. By default the buffer is readable but not writable.
   The last argument optionally specifies a size beyond which the
   buffer may not be grown.

"),

("Base","takebuf_array","takebuf_array(b::IOBuffer)

   Obtain the contents of an \"IOBuffer\" as an array, without
   copying. Afterwards, the IOBuffer is reset to its initial state.

"),

("Base","takebuf_string","takebuf_string(b::IOBuffer)

   Obtain the contents of an \"IOBuffer\" as a string, without
   copying. Afterwards, the IOBuffer is reset to its initial state.

"),

("Base","fdio","fdio([name::AbstractString], fd::Integer[, own::Bool]) -> IOStream

   Create an \"IOStream\" object from an integer file descriptor. If
   \"own\" is true, closing this object will close the underlying
   descriptor. By default, an \"IOStream\" is closed when it is
   garbage collected. \"name\" allows you to associate the descriptor
   with a named file.

"),

("Base","flush","flush(stream)

   Commit all currently buffered writes to the given stream.

"),

("Base","flush_cstdio","flush_cstdio()

   Flushes the C \"stdout\" and \"stderr\" streams (which may have
   been written to by external C code).

"),

("Base","close","close(stream)

   Close an I/O stream. Performs a \"flush\" first.

"),

("Base","write","write(stream, x)

   Write the canonical binary representation of a value to the given
   stream.

"),

("Base","read","read(stream, type)

   Read a value of the given type from a stream, in canonical binary
   representation.

"),

("Base","read","read(stream, type, dims)

   Read a series of values of the given type from a stream, in
   canonical binary representation. \"dims\" is either a tuple or a
   series of integer arguments specifying the size of \"Array\" to
   return.

"),

("Base","read!","read!(stream, array::Array)

   Read binary data from a stream, filling in the argument \"array\".

"),

("Base","readbytes!","readbytes!(stream, b::Vector{UInt8}, nb=length(b))

   Read at most \"nb\" bytes from the stream into \"b\", returning the
   number of bytes read (increasing the size of \"b\" as needed).

"),

("Base","readbytes","readbytes(stream, nb=typemax(Int))

   Read at most \"nb\" bytes from the stream, returning a
   \"Vector{UInt8}\" of the bytes read.

"),

("Base","position","position(s)

   Get the current position of a stream.

"),

("Base","seek","seek(s, pos)

   Seek a stream to the given position.

"),

("Base","seekstart","seekstart(s)

   Seek a stream to its beginning.

"),

("Base","seekend","seekend(s)

   Seek a stream to its end.

"),

("Base","skip","skip(s, offset)

   Seek a stream relative to the current position.

"),

("Base","mark","mark(s)

   Add a mark at the current position of stream \"s\".  Returns the
   marked position.

   See also \"unmark()\", \"reset()\", \"ismarked()\"

"),

("Base","unmark","unmark(s)

   Remove a mark from stream \"s\". Returns \"true\" if the stream was
   marked, \"false\" otherwise.

   See also \"mark()\", \"reset()\", \"ismarked()\"

"),

("Base","reset","reset(s)

   Reset a stream \"s\" to a previously marked position, and remove
   the mark. Returns the previously marked position. Throws an error
   if the stream is not marked.

   See also \"mark()\", \"unmark()\", \"ismarked()\"

"),

("Base","ismarked","ismarked(s)

   Returns true if stream \"s\" is marked.

   See also \"mark()\", \"unmark()\", \"reset()\"

"),

("Base","eof","eof(stream) -> Bool

   Tests whether an I/O stream is at end-of-file. If the stream is not
   yet exhausted, this function will block to wait for more data if
   necessary, and then return \"false\". Therefore it is always safe
   to read one byte after seeing \"eof\" return \"false\". \"eof\"
   will return \"false\" as long as buffered data is still available,
   even if the remote end of a connection is closed.

"),

("Base","isreadonly","isreadonly(stream) -> Bool

   Determine whether a stream is read-only.

"),

("Base","isopen","isopen(stream) -> Bool

   Determine whether a stream is open (i.e. has not been closed yet).
   If the connection has been closed remotely (in case of e.g. a
   socket), \"isopen\" will return \"false\" even though buffered data
   may still be available. Use \"eof\" to check if necessary.

"),

("Base","ntoh","ntoh(x)

   Converts the endianness of a value from Network byte order (big-
   endian) to that used by the Host.

"),

("Base","hton","hton(x)

   Converts the endianness of a value from that used by the Host to
   Network byte order (big-endian).

"),

("Base","ltoh","ltoh(x)

   Converts the endianness of a value from Little-endian to that used
   by the Host.

"),

("Base","htol","htol(x)

   Converts the endianness of a value from that used by the Host to
   Little-endian.

"),

("Base","ENDIAN_BOM","ENDIAN_BOM

   The 32-bit byte-order-mark indicates the native byte order of the
   host machine. Little-endian machines will contain the value
   0x04030201. Big-endian machines will contain the value 0x01020304.

"),

("Base","serialize","serialize(stream, value)

   Write an arbitrary value to a stream in an opaque format, such that
   it can be read back by \"deserialize\". The read-back value will be
   as identical as possible to the original. In general, this process
   will not work if the reading and writing are done by different
   versions of Julia, or an instance of Julia with a different system
   image.

"),

("Base","deserialize","deserialize(stream)

   Read a value written by \"serialize\".

"),

("Base","print_escaped","print_escaped(io, str::AbstractString, esc::AbstractString)

   General escaping of traditional C and Unicode escape sequences,
   plus any characters in esc are also escaped (with a backslash).

"),

("Base","print_unescaped","print_unescaped(io, s::AbstractString)

   General unescaping of traditional C and Unicode escape sequences.
   Reverse of \"print_escaped()\".

"),

("Base","print_joined","print_joined(io, items, delim[, last])

   Print elements of \"items\" to \"io\" with \"delim\" between them.
   If \"last\" is specified, it is used as the final delimiter instead
   of \"delim\".

"),

("Base","print_shortest","print_shortest(io, x)

   Print the shortest possible representation of number \"x\" as a
   floating point number, ensuring that it would parse to the exact
   same number.

"),

("Base","fd","fd(stream)

   Returns the file descriptor backing the stream or file. Note that
   this function only applies to synchronous *File*'s and *IOStream*'s
   not to any of the asynchronous streams.

"),

("Base","redirect_stdout","redirect_stdout()

   Create a pipe to which all C and Julia level STDOUT output will be
   redirected. Returns a tuple (rd,wr) representing the pipe ends.
   Data written to STDOUT may now be read from the rd end of the pipe.
   The wr end is given for convenience in case the old STDOUT object
   was cached by the user and needs to be replaced elsewhere.

"),

("Base","redirect_stdout","redirect_stdout(stream)

   Replace STDOUT by stream for all C and julia level output to
   STDOUT. Note that *stream* must be a TTY, a Pipe or a TcpSocket.

"),

("Base","redirect_stderr","redirect_stderr([stream])

   Like redirect_stdout, but for STDERR

"),

("Base","redirect_stdin","redirect_stdin([stream])

   Like redirect_stdout, but for STDIN. Note that the order of the
   return tuple is still (rd,wr), i.e. data to be read from STDIN, may
   be written to wr.

"),

("Base","readchomp","readchomp(x)

   Read the entirety of x as a string but remove trailing newlines.
   Equivalent to chomp(readall(x)).

"),

("Base","readdir","readdir([dir]) -> Vector{ByteString}

   Returns the files and directories in the directory *dir* (or the
   current working directory if not given).

"),

("Base","truncate","truncate(file, n)

   Resize the file or buffer given by the first argument to exactly
   *n* bytes, filling previously unallocated space with '0' if the
   file or buffer is grown

"),

("Base","skipchars","skipchars(stream, predicate; linecomment::Char)

   Advance the stream until before the first character for which
   \"predicate\" returns false. For example \"skipchars(stream,
   isspace)\" will skip all whitespace. If keyword argument
   \"linecomment\" is specified, characters from that character
   through the end of a line will also be skipped.

"),

("Base","countlines","countlines(io[, eol::Char])

   Read io until the end of the stream/file and count the number of
   non-empty lines. To specify a file pass the filename as the first
   argument. EOL markers other than 'n' are supported by passing them
   as the second argument.

"),

("Base","PipeBuffer","PipeBuffer()

   An IOBuffer that allows reading and performs writes by appending.
   Seeking and truncating are not supported. See IOBuffer for the
   available constructors.

"),

("Base","PipeBuffer","PipeBuffer(data::Vector{UInt8}[, maxsize])

   Create a PipeBuffer to operate on a data vector, optionally
   specifying a size beyond which the underlying Array may not be
   grown.

"),

("Base","readavailable","readavailable(stream)

   Read all available data on the stream, blocking the task only if no
   data is available.

"),

("Base","stat","stat(file)

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
   | blksize   | The file-system preferred block size for the file                      |
   +-----------+------------------------------------------------------------------------+
   | blocks    | The number of such blocks allocated                                    |
   +-----------+------------------------------------------------------------------------+
   | mtime     | Unix timestamp of when the file was last modified                      |
   +-----------+------------------------------------------------------------------------+
   | ctime     | Unix timestamp of when the file was created                            |
   +-----------+------------------------------------------------------------------------+

"),

("Base","lstat","lstat(file)

   Like stat, but for symbolic links gets the info for the link itself
   rather than the file it refers to. This function must be called on
   a file path rather than a file object or a file descriptor.

"),

("Base","ctime","ctime(file)

   Equivalent to stat(file).ctime

"),

("Base","mtime","mtime(file)

   Equivalent to stat(file).mtime

"),

("Base","filemode","filemode(file)

   Equivalent to stat(file).mode

"),

("Base","filesize","filesize(path...)

   Equivalent to stat(file).size

"),

("Base","uperm","uperm(file)

   Gets the permissions of the owner of the file as a bitfield of

   +------+-----------------------+
   | 01   | Execute Permission    |
   +------+-----------------------+
   | 02   | Write Permission      |
   +------+-----------------------+
   | 04   | Read Permission       |
   +------+-----------------------+

   For allowed arguments, see \"stat\".

"),

("Base","gperm","gperm(file)

   Like uperm but gets the permissions of the group owning the file

"),

("Base","operm","operm(file)

   Like uperm but gets the permissions for people who neither own the
   file nor are a member of the group owning the file

"),

("Base","cp","cp(src::AbstractString, dst::AbstractString)

   Copy a file from *src* to *dest*.

"),

("Base","download","download(url[, localfile])

   Download a file from the given url, optionally renaming it to the
   given local file name. Note that this function relies on the
   availability of external tools such as \"curl\", \"wget\" or
   \"fetch\" to download the file and is provided for convenience. For
   production use or situations in which more options are need, please
   use a package that provides the desired functionality instead.

"),

("Base","mv","mv(src::AbstractString, dst::AbstractString)

   Move a file from *src* to *dst*.

"),

("Base","rm","rm(path::AbstractString; recursive=false)

   Delete the file, link, or empty directory at the given path. If
   \"recursive=true\" is passed and the path is a directory, then all
   contents are removed recursively.

"),

("Base","touch","touch(path::AbstractString)

   Update the last-modified timestamp on a file to the current time.

"),

("Base","connect","connect([host], port) -> TcpSocket

   Connect to the host \"host\" on port \"port\"

"),

("Base","connect","connect(path) -> Pipe

   Connect to the Named Pipe/Domain Socket at \"path\"

"),

("Base","listen","listen([addr], port) -> TcpServer

   Listen on port on the address specified by \"addr\". By default
   this listens on localhost only. To listen on all interfaces pass,
   \"IPv4(0)\" or \"IPv6(0)\" as appropriate.

"),

("Base","listen","listen(path) -> PipeServer

   Listens on/Creates a Named Pipe/Domain Socket

"),

("Base","getaddrinfo","getaddrinfo(host)

   Gets the IP address of the \"host\" (may have to do a DNS lookup)

"),

("Base","parseip","parseip(addr)

   Parse a string specifying an IPv4 or IPv6 ip address.

"),

("Base","IPv4","IPv4(host::Integer) -> IPv4

   Returns IPv4 object from ip address formatted as Integer

"),

("Base","IPv6","IPv6(host::Integer) -> IPv6

   Returns IPv6 object from ip address formatted as Integer

"),

("Base","nb_available","nb_available(stream)

   Returns the number of bytes available for reading before a read
   from this stream or buffer will block.

"),

("Base","accept","accept(server[, client])

   Accepts a connection on the given server and returns a connection
   to the client. An uninitialized client stream may be provided, in
   which case it will be used instead of creating a new stream.

"),

("Base","listenany","listenany(port_hint) -> (UInt16, TcpServer)

   Create a TcpServer on any port, using hint as a starting point.
   Returns a tuple of the actual port that the server was created on
   and the server itself.

"),

("Base","watch_file","watch_file(cb=false, s; poll=false)

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

("Base","poll_fd","poll_fd(fd, seconds::Real; readable=false, writable=false)

   Poll a file descriptor fd for changes in the read or write
   availability and with a timeout given by the second argument. If
   the timeout is not needed, use \"wait(fd)\" instead. The keyword
   arguments determine which of read and/or write status should be
   monitored and at least one of them needs to be set to true. The
   returned value is an object with boolean fields \"readable\",
   \"writable\", and \"timedout\", giving the result of the polling.

"),

("Base","poll_file","poll_file(s, interval_seconds::Real, seconds::Real)

   Monitor a file for changes by polling every *interval_seconds*
   seconds for *seconds* seconds. A return value of true indicates the
   file changed, a return value of false indicates a timeout.

"),

("Base","show","show(x)

   Write an informative text representation of a value to the current
   output stream. New types should overload \"show(io, x)\" where the
   first argument is a stream. The representation used by \"show\"
   generally includes Julia-specific formatting and type information.

"),

("Base","showcompact","showcompact(x)

   Show a more compact representation of a value. This is used for
   printing array elements. If a new type has a different compact
   representation, it should overload \"showcompact(io, x)\" where the
   first argument is a stream.

"),

("Base","showall","showall(x)

   Similar to \"show\", except shows all elements of arrays.

"),

("Base","summary","summary(x)

   Return a string giving a brief description of a value. By default
   returns \"string(typeof(x))\". For arrays, returns strings like
   \"2x2 Float64 Array\".

"),

("Base","print","print(x)

   Write (to the default output stream) a canonical (un-decorated)
   text representation of a value if there is one, otherwise call
   \"show\". The representation used by \"print\" includes minimal
   formatting and tries to avoid Julia-specific details.

"),

("Base","println","println(x)

   Print (using \"print()\") \"x\" followed by a newline.

"),

("Base","print_with_color","print_with_color(color::Symbol[, io], strings...)

   Print strings in a color specified as a symbol, for example
   \":red\" or \":blue\".

"),

("Base","info","info(msg)

   Display an informational message.

"),

("Base","warn","warn(msg)

   Display a warning.

"),

("Base","@printf","@printf([io::IOStream], \"%Fmt\", args...)

   Print arg(s) using C \"printf()\" style format specification
   string. Optionally, an IOStream may be passed as the first argument
   to redirect output.

"),

("Base","@sprintf","@sprintf(\"%Fmt\", args...)

   Return \"@printf\" formatted output as string.

"),

("Base","sprint","sprint(f::Function, args...)

   Call the given function with an I/O stream and the supplied extra
   arguments. Everything written to this I/O stream is returned as a
   string.

"),

("Base","showerror","showerror(io, e)

   Show a descriptive representation of an exception object.

"),

("Base","dump","dump(x)

   Show all user-visible structure of a value.

"),

("Base","xdump","xdump(x)

   Show all structure of a value, including all fields of objects.

"),

("Base","readall","readall(stream::IO)

   Read the entire contents of an I/O stream as a string.

"),

("Base","readall","readall(filename::AbstractString)

   Open \"filename\", read the entire contents as a string, then close
   the file. Equivalent to \"open(readall, filename)\".

"),

("Base","readline","readline(stream=STDIN)

   Read a single line of text, including a trailing newline character
   (if one is reached before the end of the input), from the given
   \"stream\" (defaults to \"STDIN\"),

"),

("Base","readuntil","readuntil(stream, delim)

   Read a string, up to and including the given delimiter byte.

"),

("Base","readlines","readlines(stream)

   Read all lines as an array.

"),

("Base","eachline","eachline(stream)

   Create an iterable object that will yield each line from a stream.

"),

("Base","readdlm","readdlm(source, delim::Char, T::Type, eol::Char; header=false, skipstart=0, skipblanks=true, use_mmap, ignore_invalid_chars=false, quotes=true, dims, comments=true, comment_char='#')

   Read a matrix from the source where each line (separated by
   \"eol\") gives one row, with elements separated by the given
   delimeter. The source can be a text file, stream or byte array.
   Memory mapped files can be used by passing the byte array
   representation of the mapped segment as source.

   If \"T\" is a numeric type, the result is an array of that type,
   with any non-numeric elements as \"NaN\" for floating-point types,
   or zero. Other useful values of \"T\" include \"ASCIIString\",
   \"AbstractString\", and \"Any\".

   If \"header\" is \"true\", the first row of data will be read as
   header and the tuple \"(data_cells, header_cells)\" is returned
   instead of only \"data_cells\".

   Specifying \"skipstart\" will ignore the corresponding number of
   initial lines from the input.

   If \"skipblanks\" is \"true\", blank lines in the input will be
   ignored.

   If \"use_mmap\" is \"true\", the file specified by \"source\" is
   memory mapped for potential speedups. Default is \"true\" except on
   Windows. On Windows, you may want to specify \"true\" if the file
   is large, and is only read once and not written to.

   If \"ignore_invalid_chars\" is \"true\", bytes in \"source\" with
   invalid character encoding will be ignored. Otherwise an error is
   thrown indicating the offending character position.

   If \"quotes\" is \"true\", column enclosed within double-quote (``)
   characters are allowed to contain new lines and column delimiters.
   Double-quote characters within a quoted field must be escaped with
   another double-quote.

   Specifying \"dims\" as a tuple of the expected rows and columns
   (including header, if any) may speed up reading of large files.

   If \"comments\" is \"true\", lines beginning with \"comment_char\"
   and text following \"comment_char\" in any line are ignored.

"),

("Base","readdlm","readdlm(source, delim::Char, eol::Char; options...)

   If all data is numeric, the result will be a numeric array. If some
   elements cannot be parsed as numbers, a cell array of numbers and
   strings is returned.

"),

("Base","readdlm","readdlm(source, delim::Char, T::Type; options...)

   The end of line delimiter is taken as \"\\n\".

"),

("Base","readdlm","readdlm(source, delim::Char; options...)

   The end of line delimiter is taken as \"\\n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","readdlm","readdlm(source, T::Type; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"\\n\".

"),

("Base","readdlm","readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"\\n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","writedlm","writedlm(f, A, delim='t')

   Write \"A\" (a vector, matrix or an iterable collection of iterable
   rows) as text to \"f\" (either a filename string or an \"IO\"
   stream) using the given delimeter \"delim\" (which defaults to tab,
   but can be any printable Julia object, typically a \"Char\" or
   \"AbstractString\").

   For example, two vectors \"x\" and \"y\" of the same length can be
   written as two columns of tab-delimited text to \"f\" by either
   \"writedlm(f, [x y])\" or by \"writedlm(f, zip(x, y))\".

"),

("Base","readcsv","readcsv(source, [T::Type]; options...)

   Equivalent to \"readdlm\" with \"delim\" set to comma.

"),

("Base","writecsv","writecsv(filename, A)

   Equivalent to \"writedlm\" with \"delim\" set to comma.

"),

("Base","Base64Pipe","Base64Pipe(ostream)

   Returns a new write-only I/O stream, which converts any bytes
   written to it into base64-encoded ASCII bytes written to
   \"ostream\".  Calling \"close\" on the \"Base64Pipe\" stream is
   necessary to complete the encoding (but does not close
   \"ostream\").

"),

("Base","base64","base64(writefunc, args...)
base64(args...)

   Given a \"write\"-like function \"writefunc\", which takes an I/O
   stream as its first argument, \"base64(writefunc, args...)\" calls
   \"writefunc\" to write \"args...\" to a base64-encoded string, and
   returns the string.  \"base64(args...)\" is equivalent to
   \"base64(write, args...)\": it converts its arguments into bytes
   using the standard \"write\" functions and returns the
   base64-encoded string.

"),

("Base","display","display(x)
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
   using the requested MIME type *only*, throwing a \"MethodError\" if
   this type is not supported by either the display(s) or by \"x\".
   With these variants, one can also supply the \"raw\" data in the
   requested MIME type by passing \"x::AbstractString\" (for MIME
   types with text-based storage, such as text/html or
   application/postscript) or \"x::Vector{UInt8}\" (for binary MIME
   types).

"),

("Base","redisplay","redisplay(x)
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

("Base","displayable","displayable(mime) -> Bool
displayable(d::Display, mime) -> Bool

   Returns a boolean value indicating whether the given \"mime\" type
   (string) is displayable by any of the displays in the current
   display stack, or specifically by the display \"d\" in the second
   variant.

"),

("Base","writemime","writemime(stream, mime, x)

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

("Base","mimewritable","mimewritable(mime, x)

   Returns a boolean value indicating whether or not the object \"x\"
   can be written as the given \"mime\" type.  (By default, this is
   determined automatically by the existence of the corresponding
   \"writemime\" function for \"typeof(x)\".)

"),

("Base","reprmime","reprmime(mime, x)

   Returns a \"AbstractString\" or \"Vector{UInt8}\" containing the
   representation of \"x\" in the requested \"mime\" type, as written
   by \"writemime\" (throwing a \"MethodError\" if no appropriate
   \"writemime\" is available).  A \"AbstractString\" is returned for
   MIME types with textual representations (such as \"\"text/html\"\"
   or \"\"application/postscript\"\"), whereas binary data is returned
   as \"Vector{UInt8}\".  (The function \"istext(mime)\" returns
   whether or not Julia treats a given \"mime\" type as text.)

   As a special case, if \"x\" is a \"AbstractString\" (for textual
   MIME types) or a \"Vector{UInt8}\" (for binary MIME types), the
   \"reprmime\" function assumes that \"x\" is already in the
   requested \"mime\" format and simply returns \"x\".

"),

("Base","stringmime","stringmime(mime, x)

   Returns a \"AbstractString\" containing the representation of \"x\"
   in the requested \"mime\" type.  This is similar to \"reprmime\"
   except that binary data is base64-encoded as an ASCII string.

"),

("Base","pushdisplay","pushdisplay(d::Display)

   Pushes a new display \"d\" on top of the global display-backend
   stack.  Calling \"display(x)\" or \"display(mime, x)\" will display
   \"x\" on the topmost compatible backend in the stack (i.e., the
   topmost backend that does not throw a \"MethodError\").

"),

("Base","popdisplay","popdisplay()
popdisplay(d::Display)

   Pop the topmost backend off of the display-backend stack, or the
   topmost copy of \"d\" in the second variant.

"),

("Base","TextDisplay","TextDisplay(stream)

   Returns a \"TextDisplay <: Display\", which can display any object
   as the text/plain MIME type (only), writing the text representation
   to the given I/O stream.  (The text representation is the same as
   the way an object is printed in the Julia REPL.)

"),

("Base","istext","istext(m::MIME)

   Determine whether a MIME type is text data.

"),

("Base","mmap_array","mmap_array(type, dims, stream[, offset])

   Create an \"Array\" whose values are linked to a file, using
   memory-mapping. This provides a convenient way of working with data
   too large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted.
   Note that the file must be stored in binary format, and no format
   conversions are possible (this is a limitation of operating
   systems, not Julia).

   \"dims\" is a tuple specifying the size of the array.

   The file is passed via the stream argument.  When you initialize
   the stream, use \"\"r\"\" for a \"read-only\" array, and \"\"w+\"\"
   to create a new array used to write values to disk.

   Optionally, you can specify an offset (in bytes) if, for example,
   you want to skip over a header in the file. The default value for
   the offset is the current stream position.

   For example, the following code:

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

   creates a \"m\"-by-\"n\" \"Matrix{Int}\", linked to the file
   associated with stream \"s\".

   A more portable file would need to encode the word size---32 bit or
   64 bit---and endianness information in the header. In practice,
   consider encoding binary data using standard formats like HDF5
   (which can be used with memory-mapping).

"),

("Base","mmap_bitarray","mmap_bitarray([type], dims, stream[, offset])

   Create a \"BitArray\" whose values are linked to a file, using
   memory-mapping; it has the same purpose, works in the same way, and
   has the same arguments, as \"mmap_array()\", but the byte
   representation is different. The \"type\" parameter is optional,
   and must be \"Bool\" if given.

   **Example**:  \"B = mmap_bitarray((25,30000), s)\"

   This would create a 25-by-30000 \"BitArray\", linked to the file
   associated with stream \"s\".

"),

("Base","msync","msync(array)

   Forces synchronization between the in-memory version of a memory-
   mapped \"Array\" or \"BitArray\" and the on-disk version.

"),

("Base","msync","msync(ptr, len[, flags])

   Forces synchronization of the \"mmap()\"ped memory region from
   \"ptr\" to \"ptr+len\". Flags defaults to \"MS_SYNC\", but can be a
   combination of \"MS_ASYNC\", \"MS_SYNC\", or \"MS_INVALIDATE\". See
   your platform man page for specifics. The flags argument is not
   valid on Windows.

   You may not need to call \"msync\", because synchronization is
   performed at intervals automatically by the operating system.
   However, you can call this directly if, for example, you are
   concerned about losing the result of a long-running calculation.

"),

("Base","MS_ASYNC","MS_ASYNC

   Enum constant for \"msync()\". See your platform man page for
   details. (not available on Windows).

"),

("Base","MS_SYNC","MS_SYNC

   Enum constant for \"msync()\". See your platform man page for
   details. (not available on Windows).

"),

("Base","MS_INVALIDATE","MS_INVALIDATE

   Enum constant for \"msync()\". See your platform man page for
   details. (not available on Windows).

"),

("Base","mmap","mmap(len, prot, flags, fd, offset)

   Low-level interface to the \"mmap\" system call. See the man page.

"),

("Base","munmap","munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With
   \"mmap_array()\" you do not need to call this directly; the memory
   is unmapped for you when the array goes out of scope.

"),

("Base","-","-(x)

   Unary minus operator.

"),

("Base","+","+(x, y...)

   Addition operator. \"x+y+z+...\" calls this function with all
   arguments, i.e. \"+(x, y, z, ...)\".

"),

("Base","-","-(x, y)

   Subtraction operator.

"),

("Base","*","*(x, y...)

   Multiplication operator. \"x*y*z*...\" calls this function with all
   arguments, i.e. \"*(x, y, z, ...)\".

"),

("Base","/","/(x, y)

   Right division operator: multiplication of \"x\" by the inverse of
   \"y\" on the right. Gives floating-point results for integer
   arguments.

"),

("Base","\\","\\(x, y)

   Left division operator: multiplication of \"y\" by the inverse of
   \"x\" on the left. Gives floating-point results for integer
   arguments.

"),

("Base","^","^(x, y)

   Exponentiation operator.

"),

("Base",".+",".+(x, y)

   Element-wise addition operator.

"),

("Base",".-",".-(x, y)

   Element-wise subtraction operator.

"),

("Base",".*",".*(x, y)

   Element-wise multiplication operator.

"),

("Base","./","./(x, y)

   Element-wise right division operator.

"),

("Base",".\\",".\\(x, y)

   Element-wise left division operator.

"),

("Base",".^",".^(x, y)

   Element-wise exponentiation operator.

"),

("Base","div","div(x, y)
÷(x, y)

   The quotient from Euclidean division. Computes \"x/y\", truncated
   to an integer.

"),

("Base","fld","fld(x, y)

   Largest integer less than or equal to \"x/y\".

"),

("Base","cld","cld(x, y)

   Smallest integer larger than or equal to \"x/y\".

"),

("Base","mod","mod(x, y)

   Modulus after division, returning in the range [0,``y``), if \"y\"
   is positive, or (\"y\",0] if \"y\" is negative.

"),

("Base","mod2pi","mod2pi(x)

   Modulus after division by 2pi, returning in the range [0,2pi).

   This function computes a floating point representation of the
   modulus after division by numerically exact 2pi, and is therefore
   not exactly the same as mod(x,2pi), which would compute the modulus
   of x relative to division by the floating-point number 2pi.

"),

("Base","rem","rem(x, y)
%(x, y)

   Remainder from Euclidean division, returning a value of the same
   sign as``x``, and smaller in magnitude than \"y\". This value is
   always exact.

"),

("Base","divrem","divrem(x, y)

   The quotient and remainder from Euclidean division. Equivalent to
   \"(x÷y, x%y)\".

"),

("Base","mod1","mod1(x, m)

   Modulus after division, returning in the range (0,m]

"),

("Base","rem1","rem1(x, m)

   Remainder after division, returning in the range (0,m]

"),

("Base","//","//(num, den)

   Divide two integers or rational numbers, giving a \"Rational\"
   result.

"),

("Base","rationalize","rationalize([Type=Int], x; tol=eps(x))

   Approximate floating point number \"x\" as a Rational number with
   components of the given integer type. The result will differ from
   \"x\" by no more than \"tol\".

"),

("Base","num","num(x)

   Numerator of the rational representation of \"x\"

"),

("Base","den","den(x)

   Denominator of the rational representation of \"x\"

"),

("Base","<<","<<(x, n)

   Left bit shift operator.

"),

("Base",">>",">>(x, n)

   Right bit shift operator, preserving the sign of \"x\".

"),

("Base",">>>",">>>(x, n)

   Unsigned right bit shift operator.

"),

("Base",":",":(start[, step], stop)

   Range operator. \"a:b\" constructs a range from \"a\" to \"b\" with
   a step size of 1, and \"a:s:b\" is similar but uses a step size of
   \"s\". These syntaxes call the function \"colon\". The colon is
   also used in indexing to select whole dimensions.

"),

("Base","colon","colon(start[, step], stop)

   Called by \":\" syntax for constructing ranges.

"),

("Base","range","range(start[, step], length)

   Construct a range by length, given a starting value and optional
   step (defaults to 1).

"),

("Base","linrange","linrange(start, end, length)

   Construct a range by length, given a starting and ending value.

"),

("Base","==","==(x, y)

   Generic equality operator, giving a single \"Bool\" result. Falls
   back to \"===\". Should be implemented for all types with a notion
   of equality, based on the abstract value that an instance
   represents. For example, all numeric types are compared by numeric
   value, ignoring type. Strings are compared as sequences of
   characters, ignoring encoding.

   Follows IEEE semantics for floating-point numbers.

   Collections should generally implement \"==\" by calling \"==\"
   recursively on all contents.

   New numeric types should implement this function for two arguments
   of the new type, and handle comparison to other types via promotion
   rules where possible.

"),

("Base","!=","!=(x, y)
≠(x, y)

   Not-equals comparison operator. Always gives the opposite answer as
   \"==\". New types should generally not implement this, and rely on
   the fallback definition \"!=(x,y) = !(x==y)\" instead.

"),

("Base","===","===(x, y)
≡(x, y)

   See the \"is()\" operator

"),

("Base","!==","!==(x, y)
≢(x, y)

   Equivalent to \"!is(x, y)\"

"),

("Base","<","<(x, y)

   Less-than comparison operator. New numeric types should implement
   this function for two arguments of the new type. Because of the
   behavior of floating-point NaN values, \"<\" implements a partial
   order. Types with a canonical partial order should implement \"<\",
   and types with a canonical total order should implement \"isless\".

"),

("Base","<=","<=(x, y)
≤(x, y)

   Less-than-or-equals comparison operator.

"),

("Base",">",">(x, y)

   Greater-than comparison operator. Generally, new types should
   implement \"<\" instead of this function, and rely on the fallback
   definition \">(x,y) = y<x\".

"),

("Base",">=",">=(x, y)
≥(x, y)

   Greater-than-or-equals comparison operator.

"),

("Base",".==",".==(x, y)

   Element-wise equality comparison operator.

"),

("Base",".!=",".!=(x, y)
.≠(x, y)

   Element-wise not-equals comparison operator.

"),

("Base",".<",".<(x, y)

   Element-wise less-than comparison operator.

"),

("Base",".<=",".<=(x, y)
.≤(x, y)

   Element-wise less-than-or-equals comparison operator.

"),

("Base",".>",".>(x, y)

   Element-wise greater-than comparison operator.

"),

("Base",".>=",".>=(x, y)
.≥(x, y)

   Element-wise greater-than-or-equals comparison operator.

"),

("Base","cmp","cmp(x, y)

   Return -1, 0, or 1 depending on whether \"x\" is less than, equal
   to, or greater than \"y\", respectively. Uses the total order
   implemented by \"isless\". For floating-point numbers, uses \"<\"
   but throws an error for unordered arguments.

"),

("Base","~","~(x)

   Bitwise not

"),

("Base","&","&(x, y)

   Bitwise and

"),

("Base","|","|(x, y)

   Bitwise or

"),

("Base","\$","\$(x, y)

   Bitwise exclusive or

"),

("Base","!","!(x)

   Boolean not

"),

("","x && y","x && y

   Short-circuiting boolean and

"),

("","x || y","x || y

   Short-circuiting boolean or

"),

("Base","A_ldiv_Bc","A_ldiv_Bc(a, b)

   Matrix operator A \\ B^H

"),

("Base","A_ldiv_Bt","A_ldiv_Bt(a, b)

   Matrix operator A \\ B^T

"),

("Base","A_mul_B","A_mul_B(...)

   Matrix operator A B

"),

("Base","A_mul_Bc","A_mul_Bc(...)

   Matrix operator A B^H

"),

("Base","A_mul_Bt","A_mul_Bt(...)

   Matrix operator A B^T

"),

("Base","A_rdiv_Bc","A_rdiv_Bc(...)

   Matrix operator A / B^H

"),

("Base","A_rdiv_Bt","A_rdiv_Bt(a, b)

   Matrix operator A / B^T

"),

("Base","Ac_ldiv_B","Ac_ldiv_B(...)

   Matrix operator A^H \\ B

"),

("Base","Ac_ldiv_Bc","Ac_ldiv_Bc(...)

   Matrix operator A^H \\ B^H

"),

("Base","Ac_mul_B","Ac_mul_B(...)

   Matrix operator A^H B

"),

("Base","Ac_mul_Bc","Ac_mul_Bc(...)

   Matrix operator A^H B^H

"),

("Base","Ac_rdiv_B","Ac_rdiv_B(a, b)

   Matrix operator A^H / B

"),

("Base","Ac_rdiv_Bc","Ac_rdiv_Bc(a, b)

   Matrix operator A^H / B^H

"),

("Base","At_ldiv_B","At_ldiv_B(...)

   Matrix operator A^T \\ B

"),

("Base","At_ldiv_Bt","At_ldiv_Bt(...)

   Matrix operator A^T \\ B^T

"),

("Base","At_mul_B","At_mul_B(...)

   Matrix operator A^T B

"),

("Base","At_mul_Bt","At_mul_Bt(...)

   Matrix operator A^T B^T

"),

("Base","At_rdiv_B","At_rdiv_B(a, b)

   Matrix operator A^T / B

"),

("Base","At_rdiv_Bt","At_rdiv_Bt(a, b)

   Matrix operator A^T / B^T

"),

("Base","isapprox","isapprox(x::Number, y::Number; rtol::Real=cbrt(maxeps), atol::Real=sqrt(maxeps))

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

("Base","sin","sin(x)

   Compute sine of \"x\", where \"x\" is in radians

"),

("Base","cos","cos(x)

   Compute cosine of \"x\", where \"x\" is in radians

"),

("Base","tan","tan(x)

   Compute tangent of \"x\", where \"x\" is in radians

"),

("Base","sind","sind(x)

   Compute sine of \"x\", where \"x\" is in degrees

"),

("Base","cosd","cosd(x)

   Compute cosine of \"x\", where \"x\" is in degrees

"),

("Base","tand","tand(x)

   Compute tangent of \"x\", where \"x\" is in degrees

"),

("Base","sinpi","sinpi(x)

   Compute \\sin(\\pi x) more accurately than \"sin(pi*x)\",
   especially for large \"x\".

"),

("Base","cospi","cospi(x)

   Compute \\cos(\\pi x) more accurately than \"cos(pi*x)\",
   especially for large \"x\".

"),

("Base","sinh","sinh(x)

   Compute hyperbolic sine of \"x\"

"),

("Base","cosh","cosh(x)

   Compute hyperbolic cosine of \"x\"

"),

("Base","tanh","tanh(x)

   Compute hyperbolic tangent of \"x\"

"),

("Base","asin","asin(x)

   Compute the inverse sine of \"x\", where the output is in radians

"),

("Base","acos","acos(x)

   Compute the inverse cosine of \"x\", where the output is in radians

"),

("Base","atan","atan(x)

   Compute the inverse tangent of \"x\", where the output is in
   radians

"),

("Base","atan2","atan2(y, x)

   Compute the inverse tangent of \"y/x\", using the signs of both
   \"x\" and \"y\" to determine the quadrant of the return value.

"),

("Base","asind","asind(x)

   Compute the inverse sine of \"x\", where the output is in degrees

"),

("Base","acosd","acosd(x)

   Compute the inverse cosine of \"x\", where the output is in degrees

"),

("Base","atand","atand(x)

   Compute the inverse tangent of \"x\", where the output is in
   degrees

"),

("Base","sec","sec(x)

   Compute the secant of \"x\", where \"x\" is in radians

"),

("Base","csc","csc(x)

   Compute the cosecant of \"x\", where \"x\" is in radians

"),

("Base","cot","cot(x)

   Compute the cotangent of \"x\", where \"x\" is in radians

"),

("Base","secd","secd(x)

   Compute the secant of \"x\", where \"x\" is in degrees

"),

("Base","cscd","cscd(x)

   Compute the cosecant of \"x\", where \"x\" is in degrees

"),

("Base","cotd","cotd(x)

   Compute the cotangent of \"x\", where \"x\" is in degrees

"),

("Base","asec","asec(x)

   Compute the inverse secant of \"x\", where the output is in radians

"),

("Base","acsc","acsc(x)

   Compute the inverse cosecant of \"x\", where the output is in
   radians

"),

("Base","acot","acot(x)

   Compute the inverse cotangent of \"x\", where the output is in
   radians

"),

("Base","asecd","asecd(x)

   Compute the inverse secant of \"x\", where the output is in degrees

"),

("Base","acscd","acscd(x)

   Compute the inverse cosecant of \"x\", where the output is in
   degrees

"),

("Base","acotd","acotd(x)

   Compute the inverse cotangent of \"x\", where the output is in
   degrees

"),

("Base","sech","sech(x)

   Compute the hyperbolic secant of \"x\"

"),

("Base","csch","csch(x)

   Compute the hyperbolic cosecant of \"x\"

"),

("Base","coth","coth(x)

   Compute the hyperbolic cotangent of \"x\"

"),

("Base","asinh","asinh(x)

   Compute the inverse hyperbolic sine of \"x\"

"),

("Base","acosh","acosh(x)

   Compute the inverse hyperbolic cosine of \"x\"

"),

("Base","atanh","atanh(x)

   Compute the inverse hyperbolic tangent of \"x\"

"),

("Base","asech","asech(x)

   Compute the inverse hyperbolic secant of \"x\"

"),

("Base","acsch","acsch(x)

   Compute the inverse hyperbolic cosecant of \"x\"

"),

("Base","acoth","acoth(x)

   Compute the inverse hyperbolic cotangent of \"x\"

"),

("Base","sinc","sinc(x)

   Compute \\sin(\\pi x) / (\\pi x) if x \\neq 0, and 1 if x = 0.

"),

("Base","cosc","cosc(x)

   Compute \\cos(\\pi x) / x - \\sin(\\pi x) / (\\pi x^2) if x \\neq
   0, and 0 if x = 0. This is the derivative of \"sinc(x)\".

"),

("Base","deg2rad","deg2rad(x)

   Convert \"x\" from degrees to radians

"),

("Base","rad2deg","rad2deg(x)

   Convert \"x\" from radians to degrees

"),

("Base","hypot","hypot(x, y)

   Compute the \\sqrt{x^2+y^2} avoiding overflow and underflow

"),

("Base","log","log(x)

   Compute the natural logarithm of \"x\". Throws \"DomainError\" for
   negative \"Real\" arguments. Use complex negative arguments
   instead.

"),

("Base","log","log(b, x)

   Compute the base \"b\" logarithm of \"x\". Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Base","log2","log2(x)

   Compute the logarithm of \"x\" to base 2. Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Base","log10","log10(x)

   Compute the logarithm of \"x\" to base 10. Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Base","log1p","log1p(x)

   Accurate natural logarithm of \"1+x\".  Throws \"DomainError\" for
   \"Real\" arguments less than -1.

"),

("Base","frexp","frexp(val)

   Return \"(x,exp)\" such that \"x\" has a magnitude in the interval
   \"[1/2, 1)\" or 0, and val = x \\times 2^{exp}.

"),

("Base","exp","exp(x)

   Compute e^x

"),

("Base","exp2","exp2(x)

   Compute 2^x

"),

("Base","exp10","exp10(x)

   Compute 10^x

"),

("Base","ldexp","ldexp(x, n)

   Compute x \\times 2^n

"),

("Base","modf","modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts
   of a number. Both parts have the same sign as the argument.

"),

("Base","expm1","expm1(x)

   Accurately compute e^x-1

"),

("Base","round","round([T], x[, digits[, base]][, r::RoundingMode])

   \"round(x)\" returns an integral value of the same type as \"x\" to
   \"x\", according to the default rounding mode (see
   \"get_rounding\"). By default, this will round to the nearest
   integer, with ties (fractional values of 0.5) being rounded to the
   even integer.

      julia> round(1.7)
      2.0

      julia> round(1.5)
      2.0

      julia> round(2.5)
      2.0

   The optional \"roundingmode\" argument will change this behaviour.
   Currently supported are

   * *RoundNearestTiesAway*: this emulates C-style \"round\"
     behaviour, by rounding ties away from zero.

   * *RoundNearestTiesUp*: this emulates Java-style \"round\"
     behaviour, by rounding ties toward positive infinity.

   * *RoundToZero*: an alias for *trunc*

   * *RoundUp*: an alias for *ceil*

   * *RoundDown*: an alias for *floor*

   \"round(T, x, [r::RoundingMode])\" converts the result to type
   \"T\", throwing an \"InexactError\" if the value is not
   representable.

   \"round(x, digits)\" rounds to the specified number of digits after
   the decimal place, or before if negative, e.g., \"round(pi,2)\" is
   \"3.14\". \"round(x, digits, base)\" rounds using a different base,
   defaulting to 10, e.g., \"round(pi, 1, 8)\" is \"3.125\".

"),

("Base","ceil","ceil([T], x[, digits[, base]])

   \"ceil(x)\" returns the nearest integral value of the same type as
   \"x\" that is greater than or equal to \"x\".

   \"ceil(T, x)\" converts the result to type \"T\", throwing an
   \"InexactError\" if the value is not representable.

   \"digits\" and \"base\" work as for \"round\".

"),

("Base","floor","floor([T], x[, digits[, base]])

   \"floor(x)\" returns the nearest integral value of the same type as
   \"x\" that is less than or equal to \"x\".

   \"floor(T, x)\" converts the result to type \"T\", throwing an
   \"InexactError\" if the value is not representable.

   \"digits\" and \"base\" work as above.

"),

("Base","trunc","trunc([T], x[, digits[, base]])

   \"trunc(x)\" returns the nearest integral value of the same type as
   \"x\" whose absolute value is less than or equal to \"x\".

   \"trunc(T, x)\" converts the result to type \"T\", throwing an
   \"InexactError\" if the value is not representable.

   \"digits\" and \"base\" work as above.

"),

("Base","unsafe_trunc","unsafe_trunc(T, x)

   \"unsafe_trunc(T, x)\" returns the nearest integral value of type
   \"T\" whose absolute value is less than or equal to \"x\". If the
   value is not representable by \"T\", an arbitrary value will be
   returned.

"),

("Base","signif","signif(x, digits[, base])

   Rounds (in the sense of \"round\") \"x\" so that there are
   \"digits\" significant digits, under a base \"base\"
   representation, default 10. E.g., \"signif(123.456, 2)\" is
   \"120.0\", and \"signif(357.913, 4, 2)\" is \"352.0\".

"),

("Base","min","min(x, y, ...)

   Return the minimum of the arguments. Operates elementwise over
   arrays.

"),

("Base","max","max(x, y, ...)

   Return the maximum of the arguments. Operates elementwise over
   arrays.

"),

("Base","minmax","minmax(x, y)

   Return \"(min(x,y), max(x,y))\". See also: \"extrema()\" that
   returns \"(minimum(x), maximum(x))\"

"),

("Base","clamp","clamp(x, lo, hi)

   Return x if \"lo <= x <= hi\". If \"x < lo\", return \"lo\". If \"x
   > hi\", return \"hi\". Arguments are promoted to a common type.
   Operates elementwise over \"x\" if it is an array.

"),

("Base","abs","abs(x)

   Absolute value of \"x\"

"),

("Base","abs2","abs2(x)

   Squared absolute value of \"x\"

"),

("Base","copysign","copysign(x, y)

   Return \"x\" such that it has the same sign as \"y\"

"),

("Base","sign","sign(x)

   Return \"+1\" if \"x\" is positive, \"0\" if \"x == 0\", and \"-1\"
   if \"x\" is negative.

"),

("Base","signbit","signbit(x)

   Returns \"true\" if the value of the sign of \"x\" is negative,
   otherwise \"false\".

"),

("Base","flipsign","flipsign(x, y)

   Return \"x\" with its sign flipped if \"y\" is negative. For
   example \"abs(x) = flipsign(x,x)\".

"),

("Base","sqrt","sqrt(x)

   Return \\sqrt{x}. Throws \"DomainError\" for negative \"Real\"
   arguments. Use complex negative arguments instead.  The prefix
   operator \"√\" is equivalent to \"sqrt\".

"),

("Base","isqrt","isqrt(n)

   Integer square root: the largest integer \"m\" such that \"m*m <=
   n\".

"),

("Base","cbrt","cbrt(x)

   Return x^{1/3}.  The prefix operator \"∛\" is equivalent to
   \"cbrt\".

"),

("Base","erf","erf(x)

   Compute the error function of \"x\", defined by
   \\frac{2}{\\sqrt{\\pi}} \\int_0^x e^{-t^2} dt for arbitrary complex
   \"x\".

"),

("Base","erfc","erfc(x)

   Compute the complementary error function of \"x\", defined by 1 -
   \\operatorname{erf}(x).

"),

("Base","erfcx","erfcx(x)

   Compute the scaled complementary error function of \"x\", defined
   by e^{x^2} \\operatorname{erfc}(x).  Note also that
   \\operatorname{erfcx}(-ix) computes the Faddeeva function w(x).

"),

("Base","erfi","erfi(x)

   Compute the imaginary error function of \"x\", defined by -i
   \\operatorname{erf}(ix).

"),

("Base","dawson","dawson(x)

   Compute the Dawson function (scaled imaginary error function) of
   \"x\", defined by \\frac{\\sqrt{\\pi}}{2} e^{-x^2}
   \\operatorname{erfi}(x).

"),

("Base","erfinv","erfinv(x)

   Compute the inverse error function of a real \"x\", defined by
   \\operatorname{erf}(\\operatorname{erfinv}(x)) = x.

"),

("Base","erfcinv","erfcinv(x)

   Compute the inverse error complementary function of a real \"x\",
   defined by \\operatorname{erfc}(\\operatorname{erfcinv}(x)) = x.

"),

("Base","real","real(z)

   Return the real part of the complex number \"z\"

"),

("Base","imag","imag(z)

   Return the imaginary part of the complex number \"z\"

"),

("Base","reim","reim(z)

   Return both the real and imaginary parts of the complex number
   \"z\"

"),

("Base","conj","conj(z)

   Compute the complex conjugate of a complex number \"z\"

"),

("Base","angle","angle(z)

   Compute the phase angle of a complex number \"z\"

"),

("Base","cis","cis(z)

   Return \\exp(iz).

"),

("Base","binomial","binomial(n, k)

   Number of ways to choose \"k\" out of \"n\" items

"),

("Base","factorial","factorial(n)

   Factorial of n

"),

("Base","factorial","factorial(n, k)

   Compute \"factorial(n)/factorial(k)\"

"),

("Base","factor","factor(n) -> Dict

   Compute the prime factorization of an integer \"n\". Returns a
   dictionary. The keys of the dictionary correspond to the factors,
   and hence are of the same type as \"n\". The value associated with
   each key indicates the number of times the factor appears in the
   factorization.

      julia> factor(100) # == 2*2*5*5
      Dict{Int64,Int64} with 2 entries:
        2 => 2
        5 => 2

"),

("Base","gcd","gcd(x, y)

   Greatest common (positive) divisor (or zero if x and y are both
   zero).

"),

("Base","lcm","lcm(x, y)

   Least common (non-negative) multiple.

"),

("Base","gcdx","gcdx(x, y)

   Computes the greatest common (positive) divisor of \"x\" and \"y\"
   and their Bézout coefficients, i.e. the integer coefficients \"u\"
   and \"v\" that satisfy ux+vy = d = gcd(x,y).

      julia> gcdx(12, 42)
      (6,-3,1)

      julia> gcdx(240, 46)
      (2,-9,47)

   Note: Bézout coefficients are *not* uniquely defined. \"gcdx\"
     returns the minimal Bézout coefficients that are computed by the
     extended Euclid algorithm. (Ref: D. Knuth, TAoCP, 2/e, p. 325,
     Algorithm X.) These coefficients \"u\" and \"v\" are minimal in
     the sense that |u| < |\\frac y d and |v| < |\\frac x d.
     Furthermore, the signs of \"u\" and \"v\" are chosen so that
     \"d\" is positive.

"),

("Base","ispow2","ispow2(n) -> Bool

   Test whether \"n\" is a power of two

"),

("Base","nextpow2","nextpow2(n)

   The smallest power of two not less than \"n\". Returns 0 for
   \"n==0\", and returns \"-nextpow2(-n)\" for negative arguments.

"),

("Base","prevpow2","prevpow2(n)

   The largest power of two not greater than \"n\". Returns 0 for
   \"n==0\", and returns \"-prevpow2(-n)\" for negative arguments.

"),

("Base","nextpow","nextpow(a, x)

   The smallest \"a^n\" not less than \"x\", where \"n\" is a non-
   negative integer. \"a\" must be greater than 1, and \"x\" must be
   greater than 0.

"),

("Base","prevpow","prevpow(a, x)

   The largest \"a^n\" not greater than \"x\", where \"n\" is a non-
   negative integer. \"a\" must be greater than 1, and \"x\" must not
   be less than 1.

"),

("Base","nextprod","nextprod([k_1, k_2, ...], n)

   Next integer not less than \"n\" that can be written as \\prod
   k_i^{p_i} for integers p_1, p_2, etc.

"),

("Base","prevprod","prevprod([k_1, k_2, ...], n)

   Previous integer not greater than \"n\" that can be written as
   \\prod k_i^{p_i} for integers p_1, p_2, etc.

"),

("Base","invmod","invmod(x, m)

   Take the inverse of \"x\" modulo \"m\": \"y\" such that xy = 1
   \\pmod m

"),

("Base","powermod","powermod(x, p, m)

   Compute x^p \\pmod m

"),

("Base","gamma","gamma(x)

   Compute the gamma function of \"x\"

"),

("Base","lgamma","lgamma(x)

   Compute the logarithm of absolute value of \"gamma(x)\"

"),

("Base","lfact","lfact(x)

   Compute the logarithmic factorial of \"x\"

"),

("Base","digamma","digamma(x)

   Compute the digamma function of \"x\" (the logarithmic derivative
   of \"gamma(x)\")

"),

("Base","invdigamma","invdigamma(x)

   Compute the inverse digamma function of \"x\".

"),

("Base","trigamma","trigamma(x)

   Compute the trigamma function of \"x\" (the logarithmic second
   derivative of \"gamma(x)\")

"),

("Base","polygamma","polygamma(m, x)

   Compute the polygamma function of order \"m\" of argument \"x\"
   (the \"(m+1)th\" derivative of the logarithm of \"gamma(x)\")

"),

("Base","airy","airy(k, x)

   kth derivative of the Airy function \\operatorname{Ai}(x).

"),

("Base","airyai","airyai(x)

   Airy function \\operatorname{Ai}(x).

"),

("Base","airyprime","airyprime(x)

   Airy function derivative \\operatorname{Ai}'(x).

"),

("Base","airyaiprime","airyaiprime(x)

   Airy function derivative \\operatorname{Ai}'(x).

"),

("Base","airybi","airybi(x)

   Airy function \\operatorname{Bi}(x).

"),

("Base","airybiprime","airybiprime(x)

   Airy function derivative \\operatorname{Bi}'(x).

"),

("Base","airyx","airyx(k, x)

   scaled kth derivative of the Airy function, return
   \\operatorname{Ai}(x) e^{\\frac{2}{3} x \\sqrt{x}} for \"k == 0 ||
   k == 1\", and \\operatorname{Ai}(x) e^{- \\left| \\operatorname{Re}
   \\left( \\frac{2}{3} x \\sqrt{x} \\right) \\right|} for \"k == 2 ||
   k == 3\".

"),

("Base","besselj0","besselj0(x)

   Bessel function of the first kind of order 0, J_0(x).

"),

("Base","besselj1","besselj1(x)

   Bessel function of the first kind of order 1, J_1(x).

"),

("Base","besselj","besselj(nu, x)

   Bessel function of the first kind of order \"nu\", J_\\nu(x).

"),

("Base","besseljx","besseljx(nu, x)

   Scaled Bessel function of the first kind of order \"nu\", J_\\nu(x)
   e^{- | \\operatorname{Im}(x) |}.

"),

("Base","bessely0","bessely0(x)

   Bessel function of the second kind of order 0, Y_0(x).

"),

("Base","bessely1","bessely1(x)

   Bessel function of the second kind of order 1, Y_1(x).

"),

("Base","bessely","bessely(nu, x)

   Bessel function of the second kind of order \"nu\", Y_\\nu(x).

"),

("Base","besselyx","besselyx(nu, x)

   Scaled Bessel function of the second kind of order \"nu\",
   Y_\\nu(x) e^{- | \\operatorname{Im}(x) |}.

"),

("Base","hankelh1","hankelh1(nu, x)

   Bessel function of the third kind of order \"nu\", H^{(1)}_\\nu(x).

"),

("Base","hankelh1x","hankelh1x(nu, x)

   Scaled Bessel function of the third kind of order \"nu\",
   H^{(1)}_\\nu(x) e^{-x i}.

"),

("Base","hankelh2","hankelh2(nu, x)

   Bessel function of the third kind of order \"nu\", H^{(2)}_\\nu(x).

"),

("Base","hankelh2x","hankelh2x(nu, x)

   Scaled Bessel function of the third kind of order \"nu\",
   H^{(2)}_\\nu(x) e^{x i}.

"),

("Base","besselh","besselh(nu, k, x)

   Bessel function of the third kind of order \"nu\" (Hankel
   function). \"k\" is either 1 or 2, selecting \"hankelh1\" or
   \"hankelh2\", respectively.

"),

("Base","besseli","besseli(nu, x)

   Modified Bessel function of the first kind of order \"nu\",
   I_\\nu(x).

"),

("Base","besselix","besselix(nu, x)

   Scaled modified Bessel function of the first kind of order \"nu\",
   I_\\nu(x) e^{- | \\operatorname{Re}(x) |}.

"),

("Base","besselk","besselk(nu, x)

   Modified Bessel function of the second kind of order \"nu\",
   K_\\nu(x).

"),

("Base","besselkx","besselkx(nu, x)

   Scaled modified Bessel function of the second kind of order \"nu\",
   K_\\nu(x) e^x.

"),

("Base","beta","beta(x, y)

   Euler integral of the first kind \\operatorname{B}(x,y) =
   \\Gamma(x)\\Gamma(y)/\\Gamma(x+y).

"),

("Base","lbeta","lbeta(x, y)

   Natural logarithm of the absolute value of the beta function
   \\log(|\\operatorname{B}(x,y)|).

"),

("Base","eta","eta(x)

   Dirichlet eta function \\eta(s) =
   \\sum^\\infty_{n=1}(-)^{n-1}/n^{s}.

"),

("Base","zeta","zeta(s)

   Riemann zeta function \\zeta(s).

"),

("Base","zeta","zeta(s, z)

   Hurwitz zeta function \\zeta(s, z).  (This is equivalent to the
   Riemann zeta function \\zeta(s) for the case of \"z=1\".)

"),

("Base","ndigits","ndigits(n, b)

   Compute the number of digits in number \"n\" written in base \"b\".

"),

("Base","widemul","widemul(x, y)

   Multiply \"x\" and \"y\", giving the result as a larger type.

"),

("Base","@evalpoly","@evalpoly(z, c...)

   Evaluate the polynomial \\sum_k c[k] z^{k-1} for the coefficients
   \"c[1]\", \"c[2]\", ...; that is, the coefficients are given in
   ascending order by power of \"z\".  This macro expands to efficient
   inline code that uses either Horner's method or, for complex \"z\",
   a more efficient Goertzel-like algorithm.

"),

("Base","bin","bin(n[, pad])

   Convert an integer to a binary string, optionally specifying a
   number of digits to pad to.

"),

("Base","hex","hex(n[, pad])

   Convert an integer to a hexadecimal string, optionally specifying a
   number of digits to pad to.

"),

("Base","dec","dec(n[, pad])

   Convert an integer to a decimal string, optionally specifying a
   number of digits to pad to.

"),

("Base","oct","oct(n[, pad])

   Convert an integer to an octal string, optionally specifying a
   number of digits to pad to.

"),

("Base","base","base(base, n[, pad])

   Convert an integer to a string in the given base, optionally
   specifying a number of digits to pad to. The base can be specified
   as either an integer, or as a \"UInt8\" array of character values
   to use as digit symbols.

"),

("Base","digits","digits(n[, base][, pad])

   Returns an array of the digits of \"n\" in the given base,
   optionally padded with zeros to a specified size. More significant
   digits are at higher indexes, such that \"n ==
   sum([digits[k]*base^(k-1) for k=1:length(digits)])\".

"),

("Base","digits!","digits!(array, n[, base])

   Fills an array of the digits of \"n\" in the given base. More
   significant digits are at higher indexes. If the array length is
   insufficient, the least significant digits are filled up to the
   array length. If the array length is excessive, the excess portion
   is filled with zeros.

"),

("Base","bits","bits(n)

   A string giving the literal bit representation of a number.

"),

("Base","parseint","parseint([type], str[, base])

   Parse a string as an integer in the given base (default 10),
   yielding a number of the specified type (default \"Int\").

"),

("Base","parsefloat","parsefloat([type], str)

   Parse a string as a decimal floating point number, yielding a
   number of the specified type.

"),

("Base","big","big(x)

   Convert a number to a maximum precision representation (typically
   \"BigInt\" or \"BigFloat\"). See \"BigFloat\" for information about
   some pitfalls with floating-point numbers.

"),

("Base","bool","bool(x)

   Convert a number or numeric array to boolean

"),

("Base","int","int(x)

   Convert a number or array to the default integer type on your
   platform. Alternatively, \"x\" can be a string, which is parsed as
   an integer.

"),

("Base","uint","uint(x)

   Convert a number or array to the default unsigned integer type on
   your platform. Alternatively, \"x\" can be a string, which is
   parsed as an unsigned integer.

"),

("Base","integer","integer(x)

   Convert a number or array to integer type. If \"x\" is already of
   integer type it is unchanged, otherwise it converts it to the
   default integer type on your platform.

"),

("Base","signed","signed(x)

   Convert a number to a signed integer. If the argument is unsigned,
   it is reinterpreted as signed without checking for overflow.

"),

("Base","unsigned","unsigned(x) -> Unsigned

   Convert a number to an unsigned integer. If the argument is signed,
   it is reinterpreted as unsigned without checking for negative
   values.

"),

("Base","int8","int8(x)

   Convert a number or array to \"Int8\" data type

"),

("Base","int16","int16(x)

   Convert a number or array to \"Int16\" data type

"),

("Base","int32","int32(x)

   Convert a number or array to \"Int32\" data type

"),

("Base","int64","int64(x)

   Convert a number or array to \"Int64\" data type

"),

("Base","int128","int128(x)

   Convert a number or array to \"Int128\" data type

"),

("Base","uint8","uint8(x)

   Convert a number or array to \"UInt8\" data type

"),

("Base","uint16","uint16(x)

   Convert a number or array to \"UInt16\" data type

"),

("Base","uint32","uint32(x)

   Convert a number or array to \"UInt32\" data type

"),

("Base","uint64","uint64(x)

   Convert a number or array to \"UInt64\" data type

"),

("Base","uint128","uint128(x)

   Convert a number or array to \"UInt128\" data type

"),

("Base","float16","float16(x)

   Convert a number or array to \"Float16\" data type

"),

("Base","float32","float32(x)

   Convert a number or array to \"Float32\" data type

"),

("Base","float64","float64(x)

   Convert a number or array to \"Float64\" data type

"),

("Base","float32_isvalid","float32_isvalid(x, out::Vector{Float32}) -> Bool

   Convert a number or array to \"Float32\" data type, returning true
   if successful. The result of the conversion is stored in
   \"out[1]\".

"),

("Base","float64_isvalid","float64_isvalid(x, out::Vector{Float64}) -> Bool

   Convert a number or array to \"Float64\" data type, returning true
   if successful. The result of the conversion is stored in
   \"out[1]\".

"),

("Base","float","float(x)

   Convert a number, array, or string to a \"FloatingPoint\" data
   type. For numeric data, the smallest suitable \"FloatingPoint\"
   type is used. Converts strings to \"Float64\".

   This function is not recommended for arrays. It is better to use a
   more specific function such as \"float32\" or \"float64\".

"),

("Base","significand","significand(x)

   Extract the significand(s) (a.k.a. mantissa), in binary
   representation, of a floating-point number or array.

      julia> significand(15.2)/15.2
      0.125

      julia> significand(15.2)*8
      15.2

"),

("Base","exponent","exponent(x) -> Int

   Get the exponent of a normalized floating-point number.

"),

("Base","complex64","complex64(r[, i])

   Convert to \"r + i*im\" represented as a \"Complex64\" data type.
   \"i\" defaults to zero.

"),

("Base","complex128","complex128(r[, i])

   Convert to \"r + i*im\" represented as a \"Complex128\" data type.
   \"i\" defaults to zero.

"),

("Base","complex","complex(r[, i])

   Convert real numbers or arrays to complex. \"i\" defaults to zero.

"),

("Base","char","char(x)

   Convert a number or array to \"Char\" data type

"),

("Base","bswap","bswap(n)

   Byte-swap an integer

"),

("Base","num2hex","num2hex(f)

   Get a hexadecimal string of the binary representation of a floating
   point number

"),

("Base","hex2num","hex2num(str)

   Convert a hexadecimal string to the floating point number it
   represents

"),

("Base","hex2bytes","hex2bytes(s::ASCIIString)

   Convert an arbitrarily long hexadecimal string to its binary
   representation. Returns an Array{UInt8, 1}, i.e. an array of bytes.

"),

("Base","bytes2hex","bytes2hex(bin_arr::Array{UInt8, 1})

   Convert an array of bytes to its hexadecimal representation. All
   characters are in lower-case. Returns an ASCIIString.

"),

("Base","one","one(x)

   Get the multiplicative identity element for the type of x (x can
   also specify the type itself). For matrices, returns an identity
   matrix of the appropriate size and type.

"),

("Base","zero","zero(x)

   Get the additive identity element for the type of x (x can also
   specify the type itself).

"),

("Base","pi","pi
π

   The constant pi

"),

("Base","im","im

   The imaginary unit

"),

("Base","e","e

   The constant e

"),

("Base","catalan","catalan

   Catalan's constant

"),

("Base","γ","γ

   Euler's constant

"),

("Base","φ","φ

   The golden ratio

"),

("Base","Inf","Inf

   Positive infinity of type Float64

"),

("Base","Inf32","Inf32

   Positive infinity of type Float32

"),

("Base","Inf16","Inf16

   Positive infinity of type Float16

"),

("Base","NaN","NaN

   A not-a-number value of type Float64

"),

("Base","NaN32","NaN32

   A not-a-number value of type Float32

"),

("Base","NaN16","NaN16

   A not-a-number value of type Float16

"),

("Base","issubnormal","issubnormal(f) -> Bool

   Test whether a floating point number is subnormal

"),

("Base","isfinite","isfinite(f) -> Bool

   Test whether a number is finite

"),

("Base","isinf","isinf(f) -> Bool

   Test whether a number is infinite

"),

("Base","isnan","isnan(f) -> Bool

   Test whether a floating point number is not a number (NaN)

"),

("Base","inf","inf(f)

   Returns positive infinity of the floating point type \"f\" or of
   the same floating point type as \"f\"

"),

("Base","nan","nan(f)

   Returns NaN (not-a-number) of the floating point type \"f\" or of
   the same floating point type as \"f\"

"),

("Base","nextfloat","nextfloat(f)

   Get the next floating point number in lexicographic order

"),

("Base","prevfloat","prevfloat(f) -> FloatingPoint

   Get the previous floating point number in lexicographic order

"),

("Base","isinteger","isinteger(x) -> Bool

   Test whether \"x\" or all its elements are numerically equal to
   some integer

"),

("Base","isreal","isreal(x) -> Bool

   Test whether \"x\" or all its elements are numerically equal to
   some real number

"),

("Base","Float32","Float32(x[, mode::RoundingMode])

   Create a Float32 from \"x\". If \"x\" is not exactly representable
   then \"mode\" determines how \"x\" is rounded.

      julia> Float32(1/3, RoundDown)
      0.3333333f0

      julia> Float32(1/3, RoundUp)
      0.33333334f0

   See \"get_rounding\" for available rounding modes.

"),

("Base","Float64","Float64(x[, mode::RoundingMode])

   Create a Float64 from \"x\". If \"x\" is not exactly representable
   then \"mode\" determines how \"x\" is rounded.

      julia> Float64(pi, RoundDown)
      3.141592653589793

      julia> Float64(pi, RoundUp)
      3.1415926535897936

   See \"get_rounding\" for available rounding modes.

"),

("Base","BigInt","BigInt(x)

   Create an arbitrary precision integer. \"x\" may be an \"Int\" (or
   anything that can be converted to an \"Int\") or a
   \"AbstractString\". The usual mathematical operators are defined
   for this type, and results are promoted to a \"BigInt\".

"),

("Base","BigFloat","BigFloat(x)

   Create an arbitrary precision floating point number. \"x\" may be
   an \"Integer\", a \"Float64\", a \"AbstractString\" or a
   \"BigInt\". The usual mathematical operators are defined for this
   type, and results are promoted to a \"BigFloat\". Note that because
   floating-point numbers are not exactly-representable in decimal
   notation, \"BigFloat(2.1)\" may not yield what you expect. You may
   prefer to initialize constants using strings, e.g.,
   \"BigFloat(\"2.1\")\".

"),

("Base","get_rounding","get_rounding(T)

   Get the current floating point rounding mode for type \"T\". Valid
   modes are \"RoundNearest\", \"RoundToZero\", \"RoundUp\",
   \"RoundDown\", and \"RoundFromZero\" (\"BigFloat\" only).

"),

("Base","set_rounding","set_rounding(T, mode)

   Set the rounding mode of floating point type \"T\". Note that this
   may affect other types, for instance changing the rounding mode of
   \"Float64\" will change the rounding mode of \"Float32\". See
   \"get_rounding\" for available modes

"),

("Base","with_rounding","with_rounding(f::Function, T, mode)

   Change the rounding mode of floating point type \"T\" for the
   duration of \"f\". It is logically equivalent to:

      old = get_rounding(T)
      set_rounding(T, mode)
      f()
      set_rounding(T, old)

   See \"get_rounding\" for available rounding modes.

"),

("Base","count_ones","count_ones(x::Integer) -> Integer

   Number of ones in the binary representation of \"x\".

      julia> count_ones(7)
      3

"),

("Base","count_zeros","count_zeros(x::Integer) -> Integer

   Number of zeros in the binary representation of \"x\".

      julia> count_zeros(int32(2 ^ 16 - 1))
      16

"),

("Base","leading_zeros","leading_zeros(x::Integer) -> Integer

   Number of zeros leading the binary representation of \"x\".

      julia> leading_zeros(int32(1))
      31

"),

("Base","leading_ones","leading_ones(x::Integer) -> Integer

   Number of ones leading the binary representation of \"x\".

      julia> leading_ones(uint32(2 ^ 32 - 2))
      31

"),

("Base","trailing_zeros","trailing_zeros(x::Integer) -> Integer

   Number of zeros trailing the binary representation of \"x\".

      julia> trailing_zeros(2)
      1

"),

("Base","trailing_ones","trailing_ones(x::Integer) -> Integer

   Number of ones trailing the binary representation of \"x\".

      julia> trailing_ones(3)
      2

"),

("Base","isprime","isprime(x::Integer) -> Bool

   Returns \"true\" if \"x\" is prime, and \"false\" otherwise.

      julia> isprime(3)
      true

"),

("Base","primes","primes(n)

   Returns a collection of the prime numbers <= \"n\".

"),

("Base","isodd","isodd(x::Integer) -> Bool

   Returns \"true\" if \"x\" is odd (that is, not divisible by 2), and
   \"false\" otherwise.

      julia> isodd(9)
      true

      julia> isodd(10)
      false

"),

("Base","iseven","iseven(x::Integer) -> Bool

   Returns \"true\" is \"x\" is even (that is, divisible by 2), and
   \"false\" otherwise.

      julia> iseven(9)
      false

      julia> iseven(10)
      true

"),

("Base","precision","precision(num::FloatingPoint)

   Get the precision of a floating point number, as defined by the
   effective number of bits in the mantissa.

"),

("Base","get_bigfloat_precision","get_bigfloat_precision()

   Get the precision (in bits) currently used for BigFloat arithmetic.

"),

("Base","set_bigfloat_precision","set_bigfloat_precision(x::Int64)

   Set the precision (in bits) to be used to BigFloat arithmetic.

"),

("Base","with_bigfloat_precision","with_bigfloat_precision(f::Function, precision::Integer)

   Change the BigFloat arithmetic precision (in bits) for the duration
   of \"f\". It is logically equivalent to:

      old = get_bigfloat_precision()
      set_bigfloat_precision(precision)
      f()
      set_bigfloat_precision(old)

"),

("Base","srand","srand([rng][, seed])

   Reseed the random number generator. If a \"seed\" is provided, the
   RNG will give a reproducible sequence of numbers, otherwise Julia
   will get entropy from the system. The \"seed\" may be a non-
   negative integer, a vector of \"UInt32\" integers or a filename, in
   which case the seed is read from a file.

"),

("Base","MersenneTwister","MersenneTwister([seed])

   Create a \"MersenneTwister\" RNG object. Different RNG objects can
   have their own seeds, which may be useful for generating different
   streams of random numbers.

"),

("Base","rand","rand([rng][, S][, dims...])

   Pick a random element or array of random elements from the set of
   values specified by \"S\"; \"S\" can be

   * an indexable collection (for example \"1:n\" or
     \"['x','y','z']\"), or

   * a type: the set of values to pick from is then equivalent to
     \"typemin(S):typemax(S)\" for integers (this is not applicable to
     \"BigInt\"), and to [0,1) for floating point numbers;

   \"S\" defaults to \"Float64\".

"),

("Base","rand!","rand!([rng], A[, coll])

   Populate the array A with random values. If the indexable
   collection \"coll\" is specified, the values are picked randomly
   from \"coll\". This is equivalent to \"copy!(A, rand(rng, coll,
   size(A)))\" or \"copy!(A, rand(rng, eltype(A), size(A)))\" but
   without allocating a new array.

"),

("Base","randbool","randbool([rng][, dims...])

   Generate a random boolean value. Optionally, generate a
   \"BitArray\" of random boolean values.

"),

("Base","randn","randn([rng][, dims...])

   Generate a normally-distributed random number with mean 0 and
   standard deviation 1. Optionally generate an array of normally-
   distributed random numbers.

"),

("Base","randn!","randn!([rng], A::Array{Float64, N})

   Fill the array A with normally-distributed (mean 0, standard
   deviation 1) random numbers. Also see the rand function.

"),

("Base","randexp","randexp([rng][, dims...])

   Generate a random number according to the exponential distribution
   with scale 1. Optionally generate an array of such random numbers.

"),

("Base","randexp!","randexp!([rng], A::Array{Float64, N})

   Fill the array A with random numbers following the exponential
   distribution (with scale 1).

"),

("Base","ndims","ndims(A) -> Integer

   Returns the number of dimensions of A

"),

("Base","size","size(A[, dim...])

   Returns a tuple containing the dimensions of A. Optionally you can
   specify the dimension(s) you want the length of, and get the length
   of that dimension, or a tuple of the lengths of dimensions you
   asked for.:

      julia> A = rand(2,3,4);

      julia> size(A, 2)
      3

      julia> size(A,3,2)
      (4,3)

"),

("Base","iseltype","iseltype(A, T)

   Tests whether A or its elements are of type T

"),

("Base","length","length(A) -> Integer

   Returns the number of elements in A

"),

("Base","eachindex","eachindex(A)

   Creates an iterable object for visiting each multi-dimensional
   index of the AbstractArray \"A\".  Example for a 2-d array:

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

"),

("Base","countnz","countnz(A)

   Counts the number of nonzero values in array A (dense or sparse).
   Note that this is not a constant-time operation. For sparse
   matrices, one should usually use \"nnz\", which returns the number
   of stored values.

"),

("Base","conj!","conj!(A)

   Convert an array to its complex conjugate in-place

"),

("Base","stride","stride(A, k)

   Returns the distance in memory (in number of elements) between
   adjacent elements in dimension k

"),

("Base","strides","strides(A)

   Returns a tuple of the memory strides in each dimension

"),

("Base","ind2sub","ind2sub(dims, index) -> subscripts

   Returns a tuple of subscripts into an array with dimensions
   \"dims\", corresponding to the linear index \"index\"

   **Example** \"i, j, ... = ind2sub(size(A), indmax(A))\" provides
   the indices of the maximum element

"),

("Base","ind2sub","ind2sub(a, index) -> subscripts

   Returns a tuple of subscripts into array \"a\" corresponding to the
   linear index \"index\"

"),

("Base","sub2ind","sub2ind(dims, i, j, k...) -> index

   The inverse of \"ind2sub\", returns the linear index corresponding
   to the provided subscripts

"),

("Base","Array","Array(type, dims)

   Construct an uninitialized dense array. \"dims\" may be a tuple or
   a series of integer arguments.

"),

("Base","getindex","getindex(type[, elements...])

   Construct a 1-d array of the specified type. This is usually called
   with the syntax \"Type[]\". Element values can be specified using
   \"Type[a,b,c,...]\".

"),

("Base","cell","cell(dims)

   Construct an uninitialized cell array (heterogeneous array).
   \"dims\" can be either a tuple or a series of integer arguments.

"),

("Base","zeros","zeros(type, dims)

   Create an array of all zeros of specified type. The type defaults
   to Float64 if not specified.

"),

("Base","zeros","zeros(A)

   Create an array of all zeros with the same element type and shape
   as A.

"),

("Base","ones","ones(type, dims)

   Create an array of all ones of specified type. The type defaults to
   Float64 if not specified.

"),

("Base","ones","ones(A)

   Create an array of all ones with the same element type and shape as
   A.

"),

("Base","trues","trues(dims)

   Create a \"BitArray\" with all values set to true

"),

("Base","falses","falses(dims)

   Create a \"BitArray\" with all values set to false

"),

("Base","fill","fill(x, dims)

   Create an array filled with the value \"x\". For example,
   \"fill(1.0, (10,10))\" returns a  10x10 array of floats, with each
   element initialized to 1.0.

   If \"x\" is an object reference, all elements will refer to the
   same object. \"fill(Foo(), dims)\" will return an array filled with
   the result of evaluating \"Foo()\" once.

"),

("Base","fill!","fill!(A, x)

   Fill array \"A\" with the value \"x\". If \"x\" is an object
   reference, all elements will refer to the same object. \"fill!(A,
   Foo())\" will return \"A\" filled with the result of evaluating
   \"Foo()\" once.

"),

("Base","reshape","reshape(A, dims)

   Create an array with the same data as the given array, but with
   different dimensions. An implementation for a particular type of
   array may choose whether the data is copied or shared.

"),

("Base","similar","similar(array, element_type, dims)

   Create an uninitialized array of the same type as the given array,
   but with the specified element type and dimensions. The second and
   third arguments are both optional. The \"dims\" argument may be a
   tuple or a series of integer arguments.

"),

("Base","reinterpret","reinterpret(type, A)

   Change the type-interpretation of a block of memory. For example,
   \"reinterpret(Float32, uint32(7))\" interprets the 4 bytes
   corresponding to \"uint32(7)\" as a \"Float32\". For arrays, this
   constructs an array with the same binary data as the given array,
   but with the specified element type.

"),

("Base","eye","eye(n)

   n-by-n identity matrix

"),

("Base","eye","eye(m, n)

   m-by-n identity matrix

"),

("Base","eye","eye(A)

   Constructs an identity matrix of the same dimensions and type as
   \"A\".

"),

("Base","linspace","linspace(start, stop, n)

   Construct a vector of \"n\" linearly-spaced elements from \"start\"
   to \"stop\". See also: \"linrange()\" that constructs a range
   object.

"),

("Base","logspace","logspace(start, stop, n)

   Construct a vector of \"n\" logarithmically-spaced numbers from
   \"10^start\" to \"10^stop\".

"),

("Base","broadcast","broadcast(f, As...)

   Broadcasts the arrays \"As\" to a common size by expanding
   singleton dimensions, and returns an array of the results
   \"f(as...)\" for each position.

"),

("Base","broadcast!","broadcast!(f, dest, As...)

   Like \"broadcast\", but store the result of \"broadcast(f, As...)\"
   in the \"dest\" array. Note that \"dest\" is only used to store the
   result, and does not supply arguments to \"f\" unless it is also
   listed in the \"As\", as in \"broadcast!(f, A, A, B)\" to perform
   \"A[:] = broadcast(f, A, B)\".

"),

("Base","bitbroadcast","bitbroadcast(f, As...)

   Like \"broadcast\", but allocates a \"BitArray\" to store the
   result, rather then an \"Array\".

"),

("Base","broadcast_function","broadcast_function(f)

   Returns a function \"broadcast_f\" such that
   \"broadcast_function(f)(As...) === broadcast(f, As...)\". Most
   useful in the form \"const broadcast_f = broadcast_function(f)\".

"),

("Base","broadcast!_function","broadcast!_function(f)

   Like \"broadcast_function\", but for \"broadcast!\".

"),

("Base","getindex","getindex(A, inds...)

   Returns a subset of array \"A\" as specified by \"inds\", where
   each \"ind\" may be an \"Int\", a \"Range\", or a \"Vector\".

"),

("Base","sub","sub(A, inds...)

   Returns a SubArray, which stores the input \"A\" and \"inds\"
   rather than computing the result immediately. Calling \"getindex\"
   on a SubArray computes the indices on the fly.

"),

("Base","parent","parent(A)

   Returns the \"parent array\" of an array view type (e.g.,
   SubArray), or the array itself if it is not a view

"),

("Base","parentindexes","parentindexes(A)

   From an array view \"A\", returns the corresponding indexes in the
   parent

"),

("Base","slicedim","slicedim(A, d, i)

   Return all the data of \"A\" where the index for dimension \"d\"
   equals \"i\". Equivalent to \"A[:,:,...,i,:,:,...]\" where \"i\" is
   in position \"d\".

"),

("Base","slice","slice(A, inds...)

   Create a view of the given indexes of array \"A\", dropping
   dimensions indexed with scalars.

"),

("Base","setindex!","setindex!(A, X, inds...)

   Store values from array \"X\" within some subset of \"A\" as
   specified by \"inds\".

"),

("Base","broadcast_getindex","broadcast_getindex(A, inds...)

   Broadcasts the \"inds\" arrays to a common size like \"broadcast\",
   and returns an array of the results \"A[ks...]\", where \"ks\" goes
   over the positions in the broadcast.

"),

("Base","broadcast_setindex!","broadcast_setindex!(A, X, inds...)

   Broadcasts the \"X\" and \"inds\" arrays to a common size and
   stores the value from each position in \"X\" at the indices given
   by the same positions in \"inds\".

"),

("Base","cat","cat(dims, A...)

   Concatenate the input arrays along the specified dimensions in the
   iterable \"dims\". For dimensions not in \"dims\", all input arrays
   should have the same size, which will also be the size of the
   output array along that dimension. For dimensions in \"dims\", the
   size of the output array is the sum of the sizes of the input
   arrays along that dimension. If \"dims\" is a single number, the
   different arrays are tightly stacked along that dimension. If
   \"dims\" is an iterable containing several dimensions, this allows
   to construct block diagonal matrices and their higher-dimensional
   analogues by simultaneously increasing several dimensions for every
   new input array and putting zero blocks elsewhere. For example,
   *cat([1,2], matrices...)* builds a block diagonal matrix, i.e. a
   block matrix with *matrices[1]*, *matrices[2]*, ... as diagonal
   blocks and matching zero blocks away from the diagonal.

"),

("Base","vcat","vcat(A...)

   Concatenate along dimension 1

"),

("Base","hcat","hcat(A...)

   Concatenate along dimension 2

"),

("Base","hvcat","hvcat(rows::(Int...), values...)

   Horizontal and vertical concatenation in one call. This function is
   called for block matrix syntax. The first argument specifies the
   number of arguments to concatenate in each block row. For example,
   \"[a b;c d e]\" calls \"hvcat((2,3),a,b,c,d,e)\".

   If the first argument is a single integer \"n\", then all block
   rows are assumed to have \"n\" block columns.

"),

("Base","flipdim","flipdim(A, d)

   Reverse \"A\" in dimension \"d\".

"),

("Base","flipud","flipud(A)

   Equivalent to \"flipdim(A,1)\".

"),

("Base","fliplr","fliplr(A)

   Equivalent to \"flipdim(A,2)\".

"),

("Base","circshift","circshift(A, shifts)

   Circularly shift the data in an array. The second argument is a
   vector giving the amount to shift in each dimension.

"),

("Base","find","find(A)

   Return a vector of the linear indexes of the non-zeros in \"A\"
   (determined by \"A[i]!=0\").  A common use of this is to convert a
   boolean array to an array of indexes of the \"true\" elements.

"),

("Base","find","find(f, A)

   Return a vector of the linear indexes of  \"A\" where \"f\" returns
   true.

"),

("Base","findn","findn(A)

   Return a vector of indexes for each dimension giving the locations
   of the non-zeros in \"A\" (determined by \"A[i]!=0\").

"),

("Base","findnz","findnz(A)

   Return a tuple \"(I, J, V)\" where \"I\" and \"J\" are the row and
   column indexes of the non-zero values in matrix \"A\", and \"V\" is
   a vector of the non-zero values.

"),

("Base","findfirst","findfirst(A)

   Return the index of the first non-zero value in \"A\" (determined
   by \"A[i]!=0\").

"),

("Base","findfirst","findfirst(A, v)

   Return the index of the first element equal to \"v\" in \"A\".

"),

("Base","findfirst","findfirst(predicate, A)

   Return the index of the first element of \"A\" for which
   \"predicate\" returns true.

"),

("Base","findnext","findnext(A, i)

   Find the next index >= \"i\" of a non-zero element of \"A\", or
   \"0\" if not found.

"),

("Base","findnext","findnext(predicate, A, i)

   Find the next index >= \"i\" of an element of \"A\" for which
   \"predicate\" returns true, or \"0\" if not found.

"),

("Base","findnext","findnext(A, v, i)

   Find the next index >= \"i\" of an element of \"A\" equal to \"v\"
   (using \"==\"), or \"0\" if not found.

"),

("Base","permutedims","permutedims(A, perm)

   Permute the dimensions of array \"A\". \"perm\" is a vector
   specifying a permutation of length \"ndims(A)\". This is a
   generalization of transpose for multi-dimensional arrays. Transpose
   is equivalent to \"permutedims(A, [2,1])\".

"),

("Base","ipermutedims","ipermutedims(A, perm)

   Like \"permutedims()\", except the inverse of the given permutation
   is applied.

"),

("Base","permutedims!","permutedims!(dest, src, perm)

   Permute the dimensions of array \"src\" and store the result in the
   array \"dest\". \"perm\" is a vector specifying a permutation of
   length \"ndims(src)\". The preallocated array \"dest\" should have
   \"size(dest) == size(src)[perm]\" and is completely overwritten. No
   in-place permutation is supported and unexpected results will
   happen if *src* and *dest* have overlapping memory regions.

"),

("Base","squeeze","squeeze(A, dims)

   Remove the dimensions specified by \"dims\" from array \"A\".
   Elements of \"dims\" must be unique and within the range
   \"1:ndims(A)\".

"),

("Base","vec","vec(Array) -> Vector

   Vectorize an array using column-major convention.

"),

("Base","promote_shape","promote_shape(s1, s2)

   Check two array shapes for compatibility, allowing trailing
   singleton dimensions, and return whichever shape has more
   dimensions.

"),

("Base","checkbounds","checkbounds(array, indexes...)

   Throw an error if the specified indexes are not in bounds for the
   given array.

"),

("Base","randsubseq","randsubseq(A, p) -> Vector

   Return a vector consisting of a random subsequence of the given
   array \"A\", where each element of \"A\" is included (in order)
   with independent probability \"p\".   (Complexity is linear in
   \"p*length(A)\", so this function is efficient even if \"p\" is
   small and \"A\" is large.)  Technically, this process is known as
   \"Bernoulli sampling\" of \"A\".

"),

("Base","randsubseq!","randsubseq!(S, A, p)

   Like \"randsubseq\", but the results are stored in \"S\" (which is
   resized as needed).

"),

("Base","cumprod","cumprod(A[, dim])

   Cumulative product along a dimension. The dimension defaults to 1.

"),

("Base","cumprod!","cumprod!(B, A[, dim])

   Cumulative product of \"A\" along a dimension, storing the result
   in \"B\". The dimension defaults to 1.

"),

("Base","cumsum","cumsum(A[, dim])

   Cumulative sum along a dimension. The dimension defaults to 1.

"),

("Base","cumsum!","cumsum!(B, A[, dim])

   Cumulative sum of \"A\" along a dimension, storing the result in
   \"B\". The dimension defaults to 1.

"),

("Base","cumsum_kbn","cumsum_kbn(A[, dim])

   Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier
   compensated summation algorithm for additional accuracy. The
   dimension defaults to 1.

"),

("Base","cummin","cummin(A[, dim])

   Cumulative minimum along a dimension. The dimension defaults to 1.

"),

("Base","cummax","cummax(A[, dim])

   Cumulative maximum along a dimension. The dimension defaults to 1.

"),

("Base","diff","diff(A[, dim])

   Finite difference operator of matrix or vector.

"),

("Base","gradient","gradient(F[, h])

   Compute differences along vector \"F\", using \"h\" as the spacing
   between points. The default spacing is one.

"),

("Base","rot180","rot180(A)

   Rotate matrix \"A\" 180 degrees.

"),

("Base","rotl90","rotl90(A)

   Rotate matrix \"A\" left 90 degrees.

"),

("Base","rotr90","rotr90(A)

   Rotate matrix \"A\" right 90 degrees.

"),

("Base","reducedim","reducedim(f, A, dims, initial)

   Reduce 2-argument function \"f\" along dimensions of \"A\".
   \"dims\" is a vector specifying the dimensions to reduce, and
   \"initial\" is the initial value to use in the reductions.

   The associativity of the reduction is implementation-dependent; if
   you need a particular associativity, e.g. left-to-right, you should
   write your own loop. See documentation for \"reduce\".

"),

("Base","mapslices","mapslices(f, A, dims)

   Transform the given dimensions of array \"A\" using function \"f\".
   \"f\" is called on each slice of \"A\" of the form
   \"A[...,:,...,:,...]\". \"dims\" is an integer vector specifying
   where the colons go in this expression. The results are
   concatenated along the remaining dimensions. For example, if
   \"dims\" is \"[1,2]\" and A is 4-dimensional, \"f\" is called on
   \"A[:,:,i,j]\" for all \"i\" and \"j\".

"),

("Base","sum_kbn","sum_kbn(A)

   Returns the sum of all array elements, using the Kahan-Babuska-
   Neumaier compensated summation algorithm for additional accuracy.

"),

("Base","cartesianmap","cartesianmap(f, dims)

   Given a \"dims\" tuple of integers \"(m, n, ...)\", call \"f\" on
   all combinations of integers in the ranges \"1:m\", \"1:n\", etc.

      julia> cartesianmap(println, (2,2))
      11
      21
      12
      22

"),

("Base","bitpack","bitpack(A::AbstractArray{T, N}) -> BitArray

   Converts a numeric array to a packed boolean array

"),

("Base","bitunpack","bitunpack(B::BitArray{N}) -> Array{Bool,N}

   Converts a packed boolean array to an array of booleans

"),

("Base","flipbits!","flipbits!(B::BitArray{N}) -> BitArray{N}

   Performs a bitwise not operation on B. See *~ operator*.

"),

("Base","rol","rol(B::BitArray{1}, i::Integer) -> BitArray{1}

   Left rotation operator.

"),

("Base","ror","ror(B::BitArray{1}, i::Integer) -> BitArray{1}

   Right rotation operator.

"),

("Base","nthperm","nthperm(v, k)

   Compute the kth lexicographic permutation of a vector.

"),

("Base","nthperm","nthperm(p)

   Return the \"k\" that generated permutation \"p\". Note that
   \"nthperm(nthperm([1:n], k)) == k\" for \"1 <= k <= factorial(n)\".

"),

("Base","nthperm!","nthperm!(v, k)

   In-place version of \"nthperm()\".

"),

("Base","randperm","randperm(n)

   Construct a random permutation of the given length.

"),

("Base","invperm","invperm(v)

   Return the inverse permutation of v.

"),

("Base","isperm","isperm(v) -> Bool

   Returns true if v is a valid permutation.

"),

("Base","permute!","permute!(v, p)

   Permute vector \"v\" in-place, according to permutation \"p\".  No
   checking is done to verify that \"p\" is a permutation.

   To return a new permutation, use \"v[p]\".  Note that this is
   generally faster than \"permute!(v,p)\" for large vectors.

"),

("Base","ipermute!","ipermute!(v, p)

   Like permute!, but the inverse of the given permutation is applied.

"),

("Base","randcycle","randcycle(n)

   Construct a random cyclic permutation of the given length.

"),

("Base","shuffle","shuffle(v)

   Return a randomly permuted copy of \"v\".

"),

("Base","shuffle!","shuffle!(v)

   In-place version of \"shuffle()\".

"),

("Base","reverse","reverse(v[, start=1[, stop=length(v)]])

   Return a copy of \"v\" reversed from start to stop.

"),

("Base","reverseind","reverseind(v, i)

   Given an index \"i\" in \"reverse(v)\", return the corresponding
   index in \"v\" so that \"v[reverseind(v,i)] == reverse(v)[i]\".
   (This can be nontrivial in the case where \"v\" is a Unicode
   string.)

"),

("Base","reverse!","reverse!(v[, start=1[, stop=length(v)]]) -> v

   In-place version of \"reverse()\".

"),

("Base","combinations","combinations(array, n)

   Generate all combinations of \"n\" elements from an indexable
   object.  Because the number of combinations can be very large, this
   function returns an iterator object. Use
   \"collect(combinations(array,n))\" to get an array of all
   combinations.

"),

("Base","permutations","permutations(array)

   Generate all permutations of an indexable object.  Because the
   number of permutations can be very large, this function returns an
   iterator object. Use \"collect(permutations(array))\" to get an
   array of all permutations.

"),

("Base","partitions","partitions(n)

   Generate all integer arrays that sum to \"n\". Because the number
   of partitions can be very large, this function returns an iterator
   object. Use \"collect(partitions(n))\" to get an array of all
   partitions. The number of partitions to generate can be efficiently
   computed using \"length(partitions(n))\".

"),

("Base","partitions","partitions(n, m)

   Generate all arrays of \"m\" integers that sum to \"n\". Because
   the number of partitions can be very large, this function returns
   an iterator object. Use \"collect(partitions(n,m))\" to get an
   array of all partitions. The number of partitions to generate can
   be efficiently computed using \"length(partitions(n,m))\".

"),

("Base","partitions","partitions(array)

   Generate all set partitions of the elements of an array,
   represented as arrays of arrays. Because the number of partitions
   can be very large, this function returns an iterator object. Use
   \"collect(partitions(array))\" to get an array of all partitions.
   The number of partitions to generate can be efficiently computed
   using \"length(partitions(array))\".

"),

("Base","partitions","partitions(array, m)

   Generate all set partitions of the elements of an array into
   exactly m subsets, represented as arrays of arrays. Because the
   number of partitions can be very large, this function returns an
   iterator object. Use \"collect(partitions(array,m))\" to get an
   array of all partitions. The number of partitions into m subsets is
   equal to the Stirling number of the second kind and can be
   efficiently computed using \"length(partitions(array,m))\".

"),

("Base","mean","mean(v[, region])

   Compute the mean of whole array \"v\", or optionally along the
   dimensions in \"region\". Note: Julia does not ignore \"NaN\"
   values in the computation. For applications requiring the handling
   of missing data, the \"DataArray\" package is recommended.

"),

("Base","mean!","mean!(r, v)

   Compute the mean of \"v\" over the singleton dimensions of \"r\",
   and write results to \"r\".

"),

("Base","std","std(v[, region])

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

("Base","stdm","stdm(v, m)

   Compute the sample standard deviation of a vector \"v\" with known
   mean \"m\". Note: Julia does not ignore \"NaN\" values in the
   computation.

"),

("Base","var","var(v[, region])

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

("Base","varm","varm(v, m)

   Compute the sample variance of a vector \"v\" with known mean
   \"m\". Note: Julia does not ignore \"NaN\" values in the
   computation.

"),

("Base","middle","middle(x)

   Compute the middle of a scalar value, which is equivalent to \"x\"
   itself, but of the type of \"middle(x, x)\" for consistency.

"),

("Base","middle","middle(x, y)

   Compute the middle of two reals \"x\" and \"y\", which is
   equivalent in both value and type to computing their mean (\"(x +
   y) / 2\").

"),

("Base","middle","middle(range)

   Compute the middle of a range, which consists in computing the mean
   of its extrema. Since a range is sorted, the mean is performed with
   the first and last element.

"),

("Base","middle","middle(array)

   Compute the middle of an array, which consists in finding its
   extrema and then computing their mean.

"),

("Base","median","median(v)

   Compute the median of a vector \"v\". \"NaN\" is returned if the
   data contains any \"NaN\" values. For applications requiring the
   handling of missing data, the \"DataArrays\" package is
   recommended.

"),

("Base","median!","median!(v)

   Like \"median\", but may overwrite the input vector.

"),

("Base","hist","hist(v[, n]) -> e, counts

   Compute the histogram of \"v\", optionally using approximately
   \"n\" bins. The return values are a range \"e\", which correspond
   to the edges of the bins, and \"counts\" containing the number of
   elements of \"v\" in each bin. Note: Julia does not ignore \"NaN\"
   values in the computation.

"),

("Base","hist","hist(v, e) -> e, counts

   Compute the histogram of \"v\" using a vector/range \"e\" as the
   edges for the bins. The result will be a vector of length
   \"length(e) - 1\", such that the element at location \"i\"
   satisfies \"sum(e[i] .< v .<= e[i+1])\". Note: Julia does not
   ignore \"NaN\" values in the computation.

"),

("Base","hist!","hist!(counts, v, e) -> e, counts

   Compute the histogram of \"v\", using a vector/range \"e\" as the
   edges for the bins. This function writes the resultant counts to a
   pre-allocated array \"counts\".

"),

("Base","hist2d","hist2d(M, e1, e2) -> (edge1, edge2, counts)

   Compute a \"2d histogram\" of a set of N points specified by N-by-2
   matrix \"M\". Arguments \"e1\" and \"e2\" are bins for each
   dimension, specified either as integer bin counts or vectors of bin
   edges. The result is a tuple of \"edge1\" (the bin edges used in
   the first dimension), \"edge2\" (the bin edges used in the second
   dimension), and \"counts\", a histogram matrix of size
   \"(length(edge1)-1, length(edge2)-1)\". Note: Julia does not ignore
   \"NaN\" values in the computation.

"),

("Base","hist2d!","hist2d!(counts, M, e1, e2) -> (e1, e2, counts)

   Compute a \"2d histogram\" with respect to the bins delimited by
   the edges given in \"e1\" and \"e2\". This function writes the
   results to a pre-allocated array \"counts\".

"),

("Base","histrange","histrange(v, n)

   Compute *nice* bin ranges for the edges of a histogram of \"v\",
   using approximately \"n\" bins. The resulting step sizes will be 1,
   2 or 5 multiplied by a power of 10. Note: Julia does not ignore
   \"NaN\" values in the computation.

"),

("Base","midpoints","midpoints(e)

   Compute the midpoints of the bins with edges \"e\". The result is a
   vector/range of length \"length(e) - 1\". Note: Julia does not
   ignore \"NaN\" values in the computation.

"),

("Base","quantile","quantile(v, p)

   Compute the quantiles of a vector \"v\" at a specified set of
   probability values \"p\". Note: Julia does not ignore \"NaN\"
   values in the computation.

"),

("Base","quantile","quantile(v, p)

   Compute the quantile of a vector \"v\" at the probability \"p\".
   Note: Julia does not ignore \"NaN\" values in the computation.

"),

("Base","quantile!","quantile!(v, p)

   Like \"quantile\", but overwrites the input vector.

"),

("Base","cov","cov(v1[, v2][, vardim=1, corrected=true, mean=nothing])

   Compute the Pearson covariance between the vector(s) in \"v1\" and
   \"v2\". Here, \"v1\" and \"v2\" can be either vectors or matrices.

   This function accepts three keyword arguments:

   * \"vardim\": the dimension of variables. When \"vardim = 1\",
     variables are considered in columns while observations in rows;
     when \"vardim = 2\", variables are in rows while observations in
     columns. By default, it is set to \"1\".

   * \"corrected\": whether to apply Bessel's correction (divide by
     \"n-1\" instead of \"n\"). By default, it is set to \"true\".

   * \"mean\": allow users to supply mean values that are known. By
     default, it is set to \"nothing\", which indicates that the
     mean(s) are unknown, and the function will compute the mean.
     Users can use \"mean=0\" to indicate that the input data are
     centered, and hence there's no need to subtract the mean.

   The size of the result depends on the size of \"v1\" and \"v2\".
   When both \"v1\" and \"v2\" are vectors, it returns the covariance
   between them as a scalar. When either one is a matrix, it returns a
   covariance matrix of size \"(n1, n2)\", where \"n1\" and \"n2\" are
   the numbers of slices in \"v1\" and \"v2\", which depend on the
   setting of \"vardim\".

   Note: \"v2\" can be omitted, which indicates \"v2 = v1\".

"),

("Base","cor","cor(v1[, v2][, vardim=1, mean=nothing])

   Compute the Pearson correlation between the vector(s) in \"v1\" and
   \"v2\".

   Users can use the keyword argument \"vardim\" to specify the
   variable dimension, and \"mean\" to supply pre-computed mean
   values.

"),

("Base","fft","fft(A[, dims])

   Performs a multidimensional FFT of the array \"A\".  The optional
   \"dims\" argument specifies an iterable subset of dimensions (e.g.
   an integer, range, tuple, or array) to transform along.  Most
   efficient if the size of \"A\" along the transformed dimensions is
   a product of small primes; see \"nextprod()\".  See also
   \"plan_fft()\" for even greater efficiency.

   A one-dimensional FFT computes the one-dimensional discrete Fourier
   transform (DFT) as defined by

      \\operatorname{DFT}(A)[k] =
      \\sum_{n=1}^{\\operatorname{length}(A)}
      \\exp\\left(-i\\frac{2\\pi
      (n-1)(k-1)}{\\operatorname{length}(A)} \\right) A[n].

   A multidimensional FFT simply performs this operation along each
   transformed dimension of \"A\".

   Higher performance is usually possible with multi-threading. Use
   *FFTW.set_num_threads(np)* to use *np* threads, if you have *np*
   processors.

"),

("Base","fft!","fft!(A[, dims])

   Same as \"fft()\", but operates in-place on \"A\", which must be an
   array of complex floating-point numbers.

"),

("Base","ifft","ifft(A[, dims])

   Multidimensional inverse FFT.

   A one-dimensional inverse FFT computes

      \\operatorname{IDFT}(A)[k] =
      \\frac{1}{\\operatorname{length}(A)}
      \\sum_{n=1}^{\\operatorname{length}(A)}
      \\exp\\left(+i\\frac{2\\pi (n-1)(k-1)}
      {\\operatorname{length}(A)} \\right) A[n].

   A multidimensional inverse FFT simply performs this operation along
   each transformed dimension of \"A\".

"),

("Base","ifft!","ifft!(A[, dims])

   Same as \"ifft()\", but operates in-place on \"A\".

"),

("Base","bfft","bfft(A[, dims])

   Similar to \"ifft()\", but computes an unnormalized inverse
   (backward) transform, which must be divided by the product of the
   sizes of the transformed dimensions in order to obtain the inverse.
   (This is slightly more efficient than \"ifft()\" because it omits a
   scaling step, which in some applications can be combined with other
   computational steps elsewhere.)

      \\operatorname{BDFT}(A)[k] = \\operatorname{length}(A)
      \\operatorname{IDFT}(A)[k]

"),

("Base","bfft!","bfft!(A[, dims])

   Same as \"bfft()\", but operates in-place on \"A\".

"),

("Base","plan_fft","plan_fft(A[, dims[, flags[, timelimit]]])

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

("Base","plan_ifft","plan_ifft(A[, dims[, flags[, timelimit]]])

   Same as \"plan_fft()\", but produces a plan that performs inverse
   transforms \"ifft()\".

"),

("Base","plan_bfft","plan_bfft(A[, dims[, flags[, timelimit]]])

   Same as \"plan_fft()\", but produces a plan that performs an
   unnormalized backwards transform \"bfft()\".

"),

("Base","plan_fft!","plan_fft!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_fft()\", but operates in-place on \"A\".

"),

("Base","plan_ifft!","plan_ifft!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_ifft()\", but operates in-place on \"A\".

"),

("Base","plan_bfft!","plan_bfft!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_bfft()\", but operates in-place on \"A\".

"),

("Base","rfft","rfft(A[, dims])

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

("Base","irfft","irfft(A, d[, dims])

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

("Base","brfft","brfft(A, d[, dims])

   Similar to \"irfft()\" but computes an unnormalized inverse
   transform (similar to \"bfft()\"), which must be divided by the
   product of the sizes of the transformed dimensions (of the real
   output array) in order to obtain the inverse transform.

"),

("Base","plan_rfft","plan_rfft(A[, dims[, flags[, timelimit]]])

   Pre-plan an optimized real-input FFT, similar to \"plan_fft()\"
   except for \"rfft()\" instead of \"fft()\".  The first two
   arguments, and the size of the transformed result, are the same as
   for \"rfft()\".

"),

("Base","plan_brfft","plan_brfft(A, d[, dims[, flags[, timelimit]]])

   Pre-plan an optimized real-input unnormalized transform, similar to
   \"plan_rfft()\" except for \"brfft()\" instead of \"rfft()\". The
   first two arguments and the size of the transformed result, are the
   same as for \"brfft()\".

"),

("Base","plan_irfft","plan_irfft(A, d[, dims[, flags[, timelimit]]])

   Pre-plan an optimized inverse real-input FFT, similar to
   \"plan_rfft()\" except for \"irfft()\" and \"brfft()\",
   respectively.  The first three arguments have the same meaning as
   for \"irfft()\".

"),

("Base","dct","dct(A[, dims])

   Performs a multidimensional type-II discrete cosine transform (DCT)
   of the array \"A\", using the unitary normalization of the DCT. The
   optional \"dims\" argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of \"A\" along the transformed
   dimensions is a product of small primes; see \"nextprod()\".  See
   also \"plan_dct()\" for even greater efficiency.

"),

("Base","dct!","dct!(A[, dims])

   Same as \"dct!()\", except that it operates in-place on \"A\",
   which must be an array of real or complex floating-point values.

"),

("Base","idct","idct(A[, dims])

   Computes the multidimensional inverse discrete cosine transform
   (DCT) of the array \"A\" (technically, a type-III DCT with the
   unitary normalization). The optional \"dims\" argument specifies an
   iterable subset of dimensions (e.g. an integer, range, tuple, or
   array) to transform along.  Most efficient if the size of \"A\"
   along the transformed dimensions is a product of small primes; see
   \"nextprod()\".  See also \"plan_idct()\" for even greater
   efficiency.

"),

("Base","idct!","idct!(A[, dims])

   Same as \"idct!()\", but operates in-place on \"A\".

"),

("Base","plan_dct","plan_dct(A[, dims[, flags[, timelimit]]])

   Pre-plan an optimized discrete cosine transform (DCT), similar to
   \"plan_fft()\" except producing a function that computes \"dct()\".
   The first two arguments have the same meaning as for \"dct()\".

"),

("Base","plan_dct!","plan_dct!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_dct()\", but operates in-place on \"A\".

"),

("Base","plan_idct","plan_idct(A[, dims[, flags[, timelimit]]])

   Pre-plan an optimized inverse discrete cosine transform (DCT),
   similar to \"plan_fft()\" except producing a function that computes
   \"idct()\". The first two arguments have the same meaning as for
   \"idct()\".

"),

("Base","plan_idct!","plan_idct!(A[, dims[, flags[, timelimit]]])

   Same as \"plan_idct()\", but operates in-place on \"A\".

"),

("Base","fftshift","fftshift(x)

   Swap the first and second halves of each dimension of \"x\".

"),

("Base","fftshift","fftshift(x, dim)

   Swap the first and second halves of the given dimension of array
   \"x\".

"),

("Base","ifftshift","ifftshift(x[, dim])

   Undoes the effect of \"fftshift\".

"),

("Base","filt","filt(b, a, x[, si])

   Apply filter described by vectors \"a\" and \"b\" to vector \"x\",
   with an optional initial filter state vector \"si\" (defaults to
   zeros).

"),

("Base","filt!","filt!(out, b, a, x[, si])

   Same as \"filt()\" but writes the result into the \"out\" argument,
   which may alias the input \"x\" to modify it in-place.

"),

("Base","deconv","deconv(b, a)

   Construct vector \"c\" such that \"b = conv(a,c) + r\". Equivalent
   to polynomial division.

"),

("Base","conv","conv(u, v)

   Convolution of two vectors. Uses FFT algorithm.

"),

("Base","conv2","conv2(u, v, A)

   2-D convolution of the matrix \"A\" with the 2-D separable kernel
   generated by the vectors \"u\" and \"v\".  Uses 2-D FFT algorithm

"),

("Base","conv2","conv2(B, A)

   2-D convolution of the matrix \"B\" with the matrix \"A\".  Uses
   2-D FFT algorithm

"),

("Base","xcorr","xcorr(u, v)

   Compute the cross-correlation of two vectors.

"),

("Base.FFTW","r2r","r2r(A, kind[, dims])

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

("Base.FFTW","r2r!","r2r!(A, kind[, dims])

   Same as \"r2r()\", but operates in-place on \"A\", which must be an
   array of real or complex floating-point numbers.

"),

("Base.FFTW","plan_r2r","plan_r2r(A, kind[, dims[, flags[, timelimit]]])

   Pre-plan an optimized r2r transform, similar to \"Base.plan_fft()\"
   except that the transforms (and the first three arguments)
   correspond to \"r2r()\" and \"r2r!()\", respectively.

"),

("Base.FFTW","plan_r2r!","plan_r2r!(A, kind[, dims[, flags[, timelimit]]])

   Similar to \"Base.plan_fft()\", but corresponds to \"r2r!()\".

"),

("Base","quadgk","quadgk(f, a, b, c...; reltol=sqrt(eps), abstol=0, maxevals=10^7, order=7, norm=vecnorm)

   Numerically integrate the function \"f(x)\" from \"a\" to \"b\",
   and optionally over additional intervals \"b\" to \"c\" and so on.
   Keyword options include a relative error tolerance \"reltol\"
   (defaults to \"sqrt(eps)\" in the precision of the endpoints), an
   absolute error tolerance \"abstol\" (defaults to 0), a maximum
   number of function evaluations \"maxevals\" (defaults to \"10^7\"),
   and the \"order\" of the integration rule (defaults to 7).

   Returns a pair \"(I,E)\" of the estimated integral \"I\" and an
   estimated upper bound on the absolute error \"E\".  If \"maxevals\"
   is not exceeded then \"E <= max(abstol, reltol*norm(I))\" will
   hold. (Note that it is useful to specify a positive \"abstol\" in
   cases where \"norm(I)\" may be zero.)

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
   vector space). Alternatively, a different norm can be specified by
   passing a *norm*-like function as the *norm* keyword argument
   (which defaults to *vecnorm*).

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

("Base","addprocs","addprocs(n; manager::ClusterManager=LocalManager()) -> List of process identifiers

   \"addprocs(4)\" will add 4 processes on the local machine. This can
   be used to take advantage of multiple cores.

   Keyword argument \"manager\" can be used to provide a custom
   cluster manager to start workers. For example Beowulf clusters are
   supported via a custom cluster manager implemented in  package
   \"ClusterManagers\".

   See the documentation for package \"ClusterManagers\" for more
   information on how to write a custom cluster manager.

"),

("Base","addprocs","addprocs(machines; tunnel=false, dir=JULIA_HOME, sshflags::Cmd=``) -> List of process identifiers

   Add processes on remote machines via SSH. Requires julia to be
   installed in the same location on each node, or to be available via
   a shared file system.

   \"machines\" is a vector of host definitions of the form
   \"[user@]host[:port] [bind_addr[:port]]\". \"user\" defaults to
   current user, \"port\" to the standard ssh port. A worker is
   started at each host definition. If the optional
   \"[bind_addr[:port]]\" is specified, other workers will connect to
   this worker at the specified \"bind_addr\" and \"port\".

   Keyword arguments:

   \"tunnel\" : if \"true\" then SSH tunneling will be used to connect
   to the worker.

   \"dir\" :  specifies the location of the julia binaries on the
   worker nodes.

   \"sshflags\" : specifies additional ssh options, e.g.
   \"sshflags=`-i /home/foo/bar.pem`\" .

   \"max_parallel\" : specifies the maximum number of workers being
   launched in parallel at a host. Defaults to 10.

"),

("Base","nprocs","nprocs()

   Get the number of available processes.

"),

("Base","nworkers","nworkers()

   Get the number of available worker processes. This is one less than
   nprocs(). Equal to nprocs() if nprocs() == 1.

"),

("Base","procs","procs()

   Returns a list of all process identifiers.

"),

("Base","workers","workers()

   Returns a list of all worker process identifiers.

"),

("Base","rmprocs","rmprocs(pids...)

   Removes the specified workers.

"),

("Base","interrupt","interrupt([pids...])

   Interrupt the current executing task on the specified workers. This
   is equivalent to pressing Ctrl-C on the local machine. If no
   arguments are given, all workers are interrupted.

"),

("Base","myid","myid()

   Get the id of the current process.

"),

("Base","pmap","pmap(f, lsts...; err_retry=true, err_stop=false)

   Transform collections \"lsts\" by applying \"f\" to each element in
   parallel. If \"nprocs() > 1\", the calling process will be
   dedicated to assigning tasks. All other available processes will be
   used as parallel workers.

   If \"err_retry\" is true, it retries a failed application of \"f\"
   on a different worker. If \"err_stop\" is true, it takes precedence
   over the value of \"err_retry\" and \"pmap\" stops execution on the
   first error.

"),

("Base","remotecall","remotecall(id, func, args...)

   Call a function asynchronously on the given arguments on the
   specified process. Returns a \"RemoteRef\".

"),

("Base","wait","wait([x])

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

   If no argument is passed, the task blocks for an undefined period.
   If the task's state is set to \":waiting\", it can only be
   restarted by an explicit call to \"schedule\" or \"yieldto\". If
   the task's state is \":runnable\", it might be restarted
   unpredictably.

   Often \"wait\" is called within a \"while\" loop to ensure a
   waited-for condition is met before proceeding.

"),

("Base","fetch","fetch(RemoteRef)

   Wait for and get the value of a remote reference.

"),

("Base","remotecall_wait","remotecall_wait(id, func, args...)

   Perform \"wait(remotecall(...))\" in one message.

"),

("Base","remotecall_fetch","remotecall_fetch(id, func, args...)

   Perform \"fetch(remotecall(...))\" in one message.

"),

("Base","put!","put!(RemoteRef, value)

   Store a value to a remote reference. Implements \"shared queue of
   length 1\" semantics: if a value is already present, blocks until
   the value is removed with \"take!\". Returns its first argument.

"),

("Base","take!","take!(RemoteRef)

   Fetch the value of a remote reference, removing it so that the
   reference is empty again.

"),

("Base","isready","isready(r::RemoteRef)

   Determine whether a \"RemoteRef\" has a value stored to it. Note
   that this function can cause race conditions, since by the time you
   receive its result it may no longer be true. It is recommended that
   this function only be used on a \"RemoteRef\" that is assigned
   once.

   If the argument \"RemoteRef\" is owned by a different node, this
   call will block to wait for the answer. It is recommended to wait
   for \"r\" in a separate task instead, or to use a local
   \"RemoteRef\" as a proxy:

      rr = RemoteRef()
      @async put!(rr, remotecall_fetch(p, long_computation))
      isready(rr)  # will not block

"),

("Base","RemoteRef","RemoteRef()

   Make an uninitialized remote reference on the local machine.

"),

("Base","RemoteRef","RemoteRef(n)

   Make an uninitialized remote reference on process \"n\".

"),

("Base","timedwait","timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

   Waits till \"testcb\" returns \"true\" or for \"secs`\" seconds,
   whichever is earlier. \"testcb\" is polled every \"pollint\"
   seconds.

"),

("Base","@spawn","@spawn()

   Execute an expression on an automatically-chosen process, returning
   a \"RemoteRef\" to the result.

"),

("Base","@spawnat","@spawnat()

   Accepts two arguments, \"p\" and an expression, and runs the
   expression asynchronously on process \"p\", returning a
   \"RemoteRef\" to the result.

"),

("Base","@fetch","@fetch()

   Equivalent to \"fetch(@spawn expr)\".

"),

("Base","@fetchfrom","@fetchfrom()

   Equivalent to \"fetch(@spawnat p expr)\".

"),

("Base","@async","@async()

   Schedule an expression to run on the local machine, also adding it
   to the set of items that the nearest enclosing \"@sync\" waits for.

"),

("Base","@sync","@sync()

   Wait until all dynamically-enclosed uses of \"@async\", \"@spawn\",
   \"@spawnat\" and \"@parallel\" are complete.

"),

("Base","@parallel","@parallel()

   A parallel for loop of the form

      @parallel [reducer] for var = range
          body
      end

   The specified range is partitioned and locally executed across all
   workers. In case an optional reducer function is specified,
   @parallel performs local reductions on each worker with a final
   reduction on the calling process.

   Note that without a reducer function, @parallel executes
   asynchronously, i.e. it spawns independent tasks on all available
   workers and returns immediately without waiting for completion. To
   wait for completion, prefix the call with \"@sync\", like

      @sync @parallel for var = range
          body
      end

"),

("Base","DArray","DArray(init, dims[, procs, dist])

   Construct a distributed array. The parameter \"init\" is a function
   that accepts a tuple of index ranges. This function should allocate
   a local chunk of the distributed array and initialize it for the
   specified indices. \"dims\" is the overall size of the distributed
   array. \"procs\" optionally specifies a vector of process IDs to
   use. If unspecified, the array is distributed over all worker
   processes only. Typically, when running in distributed mode, i.e.,
   \"nprocs() > 1\", this would mean that no chunk of the distributed
   array exists on the process hosting the interactive julia prompt.
   \"dist\" is an integer vector specifying how many chunks the
   distributed array should be divided into in each dimension.

   For example, the \"dfill\" function that creates a distributed
   array and fills it with a value \"v\" is implemented as:

   \"dfill(v, args...) = DArray(I->fill(v, map(length,I)), args...)\"

"),

("Base","dzeros","dzeros(dims, ...)

   Construct a distributed array of zeros. Trailing arguments are the
   same as those accepted by \"DArray()\".

"),

("Base","dones","dones(dims, ...)

   Construct a distributed array of ones. Trailing arguments are the
   same as those accepted by \"DArray()\".

"),

("Base","dfill","dfill(x, dims, ...)

   Construct a distributed array filled with value \"x\". Trailing
   arguments are the same as those accepted by \"DArray()\".

"),

("Base","drand","drand(dims, ...)

   Construct a distributed uniform random array. Trailing arguments
   are the same as those accepted by \"DArray()\".

"),

("Base","drandn","drandn(dims, ...)

   Construct a distributed normal random array. Trailing arguments are
   the same as those accepted by \"DArray()\".

"),

("Base","distribute","distribute(a)

   Convert a local array to distributed.

"),

("Base","localpart","localpart(d)

   Get the local piece of a distributed array. Returns an empty array
   if no local part exists on the calling process.

"),

("Base","localindexes","localindexes(d)

   A tuple describing the indexes owned by the local process. Returns
   a tuple with empty ranges if no local part exists on the calling
   process.

"),

("Base","procs","procs(d)

   Get the vector of processes storing pieces of \"d\".

"),

("Base","SharedArray","SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])

   Construct a SharedArray of a bitstype \"T\"  and size \"dims\"
   across the processes specified by \"pids\" - all of which have to
   be on the same host.

   If \"pids\" is left unspecified, the shared array will be mapped
   across all processes on the current host, including the master.
   But, \"localindexes\" and \"indexpids\" will only refer to worker
   processes. This facilitates work distribution code to use workers
   for actual computation with the master process acting as a driver.

   If an \"init\" function of the type \"initfn(S::SharedArray)\" is
   specified, it is called on all the participating workers.

"),

("Base","procs","procs(S::SharedArray)

   Get the vector of processes that have mapped the shared array

"),

("Base","sdata","sdata(S::SharedArray)

   Returns the actual \"Array\" object backing \"S\"

"),

("Base","indexpids","indexpids(S::SharedArray)

   Returns the index of the current worker into the \"pids\" vector,
   i.e., the list of workers mapping the SharedArray

"),

("Base","run","run(command)

   Run a command object, constructed with backticks. Throws an error
   if anything goes wrong, including the process exiting with a non-
   zero status.

"),

("Base","spawn","spawn(command)

   Run a command object asynchronously, returning the resulting
   \"Process\" object.

"),

("Base","DevNull","DevNull

   Used in a stream redirect to discard all data written to it.
   Essentially equivalent to /dev/null on Unix or NUL on Windows.
   Usage: \"run(`cat test.txt` |> DevNull)\"

"),

("Base","success","success(command)

   Run a command object, constructed with backticks, and tell whether
   it was successful (exited with a code of 0). An exception is raised
   if the process cannot be started.

"),

("Base","process_running","process_running(p::Process)

   Determine whether a process is currently running.

"),

("Base","process_exited","process_exited(p::Process)

   Determine whether a process has exited.

"),

("Base","kill","kill(p::Process, signum=SIGTERM)

   Send a signal to a process. The default is to terminate the
   process.

"),

("Base","open","open(command, mode::AbstractString=\"r\", stdio=DevNull)

   Start running \"command\" asynchronously, and return a tuple
   \"(stream,process)\".  If \"mode\" is \"\"r\"\", then \"stream\"
   reads from the process's standard output and \"stdio\" optionally
   specifies the process's standard input stream.  If \"mode\" is
   \"\"w\"\", then \"stream\" writes to the process's standard input
   and \"stdio\" optionally specifies the process's standard output
   stream.

"),

("Base","open","open(f::Function, command, mode::AbstractString=\"r\", stdio=DevNull)

   Similar to \"open(command, mode, stdio)\", but calls \"f(stream)\"
   on the resulting read or write stream, then closes the stream and
   waits for the process to complete.  Returns the value returned by
   \"f\".

"),

("Base","readandwrite","readandwrite(command)

   Starts running a command asynchronously, and returns a tuple
   (stdout,stdin,process) of the output stream and input stream of the
   process, and the process object itself.

"),

("Base","ignorestatus","ignorestatus(command)

   Mark a command object so that running it will not throw an error if
   the result code is non-zero.

"),

("Base","detach","detach(command)

   Mark a command object so that it will be run in a new process
   group, allowing it to outlive the julia process, and not have
   Ctrl-C interrupts passed to it.

"),

("Base","setenv","setenv(command, env; dir=working_dir)

   Set environment variables to use when running the given command.
   \"env\" is either a dictionary mapping strings to strings, or an
   array of strings of the form \"\"var=val\"\".

   The \"dir\" keyword argument can be used to specify a working
   directory for the command.

"),

("Base","|>","|>(command, command)
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

("Base",">>",">>(command, filename)

   Redirect standard output of a process, appending to the destination
   file.

"),

("Base",".>",".>(command, filename)

   Redirect the standard error stream of a process.

"),

("Base","gethostname","gethostname() -> AbstractString

   Get the local machine's host name.

"),

("Base","getipaddr","getipaddr() -> AbstractString

   Get the IP address of the local machine, as a string of the form
   \"x.x.x.x\".

"),

("Base","pwd","pwd() -> AbstractString

   Get the current working directory.

"),

("Base","cd","cd(dir::AbstractString)

   Set the current working directory.

"),

("Base","cd","cd(f[, dir])

   Temporarily changes the current working directory (HOME if not
   specified) and applies function f before returning.

"),

("Base","mkdir","mkdir(path[, mode])

   Make a new directory with name \"path\" and permissions \"mode\".
   \"mode\" defaults to 0o777, modified by the current file creation
   mask.

"),

("Base","mkpath","mkpath(path[, mode])

   Create all directories in the given \"path\", with permissions
   \"mode\". \"mode\" defaults to 0o777, modified by the current file
   creation mask.

"),

("Base","symlink","symlink(target, link)

   Creates a symbolic link to \"target\" with the name \"link\".

   Note: This function raises an error under operating systems that
     do not support soft symbolic links, such as Windows XP.

"),

("Base","chmod","chmod(path, mode)

   Change the permissions mode of \"path\" to \"mode\". Only integer
   \"mode\"s (e.g. 0o777) are currently supported.

"),

("Base","getpid","getpid() -> Int32

   Get julia's process ID.

"),

("Base","time","time([t::TmStruct])

   Get the system time in seconds since the epoch, with fairly high
   (typically, microsecond) resolution. When passed a \"TmStruct\",
   converts it to a number of seconds since the epoch.

"),

("Base","time_ns","time_ns()

   Get the time in nanoseconds. The time corresponding to 0 is
   undefined, and wraps every 5.8 years.

"),

("Base","strftime","strftime([format], time)

   Convert time, given as a number of seconds since the epoch or a
   \"TmStruct\", to a formatted string using the given format.
   Supported formats are the same as those in the standard C library.

"),

("Base","strptime","strptime([format], timestr)

   Parse a formatted time string into a \"TmStruct\" giving the
   seconds, minute, hour, date, etc. Supported formats are the same as
   those in the standard C library. On some platforms, timezones will
   not be parsed correctly. If the result of this function will be
   passed to \"time\" to convert it to seconds since the epoch, the
   \"isdst\" field should be filled in manually. Setting it to \"-1\"
   will tell the C library to use the current system settings to
   determine the timezone.

"),

("Base","TmStruct","TmStruct([seconds])

   Convert a number of seconds since the epoch to broken-down format,
   with fields \"sec\", \"min\", \"hour\", \"mday\", \"month\",
   \"year\", \"wday\", \"yday\", and \"isdst\".

"),

("Base","tic","tic()

   Set a timer to be read by the next call to \"toc()\" or \"toq()\".
   The macro call \"@time expr\" can also be used to time evaluation.

"),

("Base","toc","toc()

   Print and return the time elapsed since the last \"tic()\".

"),

("Base","toq","toq()

   Return, but do not print, the time elapsed since the last
   \"tic()\".

"),

("Base","@time","@time()

   A macro to execute an expression, printing the time it took to
   execute and the total number of bytes its execution caused to be
   allocated, before returning the value of the expression.

"),

("Base","@elapsed","@elapsed()

   A macro to evaluate an expression, discarding the resulting value,
   instead returning the number of seconds it took to execute as a
   floating-point number.

"),

("Base","@allocated","@allocated()

   A macro to evaluate an expression, discarding the resulting value,
   instead returning the total number of bytes allocated during
   evaluation of the expression.

"),

("Base","EnvHash","EnvHash() -> EnvHash

   A singleton of this type provides a hash table interface to
   environment variables.

"),

("Base","ENV","ENV

   Reference to the singleton \"EnvHash\", providing a dictionary
   interface to system environment variables.

"),

("Base","@unix","@unix()

   Given \"@unix? a : b\", do \"a\" on Unix systems (including Linux
   and OS X) and \"b\" elsewhere. See documentation for Handling
   Platform Variations in the Calling C and Fortran Code section of
   the manual.

"),

("Base","@osx","@osx()

   Given \"@osx? a : b\", do \"a\" on OS X and \"b\" elsewhere. See
   documentation for Handling Platform Variations in the Calling C and
   Fortran Code section of the manual.

"),

("Base","@linux","@linux()

   Given \"@linux? a : b\", do \"a\" on Linux and \"b\" elsewhere. See
   documentation for Handling Platform Variations in the Calling C and
   Fortran Code section of the manual.

"),

("Base","@windows","@windows()

   Given \"@windows? a : b\", do \"a\" on Windows and \"b\" elsewhere.
   See documentation for Handling Platform Variations in the Calling C
   and Fortran Code section of the manual.

"),

("Base","ccall","ccall((symbol, library) or fptr, RetType, (ArgType1, ...), ArgVar1, ...)

   Call function in C-exported shared library, specified by
   \"(function name, library)\" tuple, where each component is a
   AbstractString or :Symbol. Alternatively, ccall may be used to call
   a function pointer returned by dlsym, but note that this usage is
   generally discouraged to facilitate future static compilation. Note
   that the argument type tuple must be a literal tuple, and not a
   tuple-valued variable or expression.

"),

("Base","cglobal","cglobal((symbol, library) or ptr[, Type=Void])

   Obtain a pointer to a global variable in a C-exported shared
   library, specified exactly as in \"ccall\".  Returns a
   \"Ptr{Type}\", defaulting to \"Ptr{Void}\" if no Type argument is
   supplied.  The values can be read or written by \"unsafe_load\" or
   \"unsafe_store!\", respectively.

"),

("Base","cfunction","cfunction(fun::Function, RetType::Type, (ArgTypes...))

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

("Base","dlopen","dlopen(libfile::AbstractString[, flags::Integer])

   Load a shared library, returning an opaque handle.

   The optional flags argument is a bitwise-or of zero or more of
   \"RTLD_LOCAL\", \"RTLD_GLOBAL\", \"RTLD_LAZY\", \"RTLD_NOW\",
   \"RTLD_NODELETE\", \"RTLD_NOLOAD\", \"RTLD_DEEPBIND\", and
   \"RTLD_FIRST\".  These are converted to the corresponding flags of
   the POSIX (and/or GNU libc and/or MacOS) dlopen command, if
   possible, or are ignored if the specified functionality is not
   available on the current platform.  The default is
   \"RTLD_LAZY|RTLD_DEEPBIND|RTLD_LOCAL\".  An important usage of
   these flags, on POSIX platforms, is to specify
   \"RTLD_LAZY|RTLD_DEEPBIND|RTLD_GLOBAL\" in order for the library's
   symbols to be available for usage in other shared libraries, in
   situations where there are dependencies between shared libraries.

"),

("Base","dlopen_e","dlopen_e(libfile::AbstractString[, flags::Integer])

   Similar to \"dlopen()\", except returns a \"NULL\" pointer instead
   of raising errors.

"),

("Base","RTLD_DEEPBIND","RTLD_DEEPBIND

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","RTLD_FIRST","RTLD_FIRST

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","RTLD_GLOBAL","RTLD_GLOBAL

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","RTLD_LAZY","RTLD_LAZY

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","RTLD_LOCAL","RTLD_LOCAL

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","RTLD_NODELETE","RTLD_NODELETE

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","RTLD_NOLOAD","RTLD_NOLOAD

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","RTLD_NOW","RTLD_NOW

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Base","dlsym","dlsym(handle, sym)

   Look up a symbol from a shared library handle, return callable
   function pointer on success.

"),

("Base","dlsym_e","dlsym_e(handle, sym)

   Look up a symbol from a shared library handle, silently return NULL
   pointer on lookup failure.

"),

("Base","dlclose","dlclose(handle)

   Close shared library referenced by handle.

"),

("Base","find_library","find_library(names, locations)

   Searches for the first library in \"names\" in the paths in the
   \"locations\" list, \"DL_LOAD_PATH\", or system library paths (in
   that order) which can successfully be dlopen'd. On success, the
   return value will be one of the names (potentially prefixed by one
   of the paths in locations). This string can be assigned to a
   \"global const\" and used as the library name in future
   \"ccall\"'s. On failure, it returns the empty string.

"),

("Base","DL_LOAD_PATH","DL_LOAD_PATH

   When calling \"dlopen\", the paths in this list will be searched
   first, in order, before searching the system locations for a valid
   library handle.

"),

("Base","c_malloc","c_malloc(size::Integer) -> Ptr{Void}

   Call \"malloc\" from the C standard library.

"),

("Base","c_calloc","c_calloc(num::Integer, size::Integer) -> Ptr{Void}

   Call \"calloc\" from the C standard library.

"),

("Base","c_realloc","c_realloc(addr::Ptr, size::Integer) -> Ptr{Void}

   Call \"realloc\" from the C standard library.

"),

("Base","c_free","c_free(addr::Ptr)

   Call \"free\" from the C standard library.

"),

("Base","unsafe_load","unsafe_load(p::Ptr{T}, i::Integer)

   Load a value of type \"T\" from the address of the ith element
   (1-indexed) starting at \"p\". This is equivalent to the C
   expression \"p[i-1]\".

"),

("Base","unsafe_store!","unsafe_store!(p::Ptr{T}, x, i::Integer)

   Store a value of type \"T\" to the address of the ith element
   (1-indexed) starting at \"p\". This is equivalent to the C
   expression \"p[i-1] = x\".

"),

("Base","unsafe_copy!","unsafe_copy!(dest::Ptr{T}, src::Ptr{T}, N)

   Copy \"N\" elements from a source pointer to a destination, with no
   checking. The size of an element is determined by the type of the
   pointers.

"),

("Base","unsafe_copy!","unsafe_copy!(dest::Array, do, src::Array, so, N)

   Copy \"N\" elements from a source array to a destination, starting
   at offset \"so\" in the source and \"do\" in the destination
   (1-indexed).

"),

("Base","copy!","copy!(dest, src)

   Copy all elements from collection \"src\" to array \"dest\".
   Returns \"dest\".

"),

("Base","copy!","copy!(dest, do, src, so, N)

   Copy \"N\" elements from collection \"src\" starting at offset
   \"so\", to array \"dest\" starting at offset \"do\". Returns
   \"dest\".

"),

("Base","pointer","pointer(a[, index])

   Get the native address of an array or string element. Be careful to
   ensure that a julia reference to \"a\" exists as long as this
   pointer will be used.

"),

("Base","pointer","pointer(type, int)

   Convert an integer to a pointer of the specified element type.

"),

("Base","pointer_to_array","pointer_to_array(p, dims[, own])

   Wrap a native pointer as a Julia Array object. The pointer element
   type determines the array element type. \"own\" optionally
   specifies whether Julia should take ownership of the memory,
   calling \"free\" on the pointer when the array is no longer
   referenced.

"),

("Base","pointer_from_objref","pointer_from_objref(obj)

   Get the memory address of a Julia object as a \"Ptr\". The
   existence of the resulting \"Ptr\" will not protect the object from
   garbage collection, so you must ensure that the object remains
   referenced for the whole time that the \"Ptr\" will be used.

"),

("Base","unsafe_pointer_to_objref","unsafe_pointer_to_objref(p::Ptr)

   Convert a \"Ptr\" to an object reference. Assumes the pointer
   refers to a valid heap-allocated Julia object. If this is not the
   case, undefined behavior results, hence this function is considered
   \"unsafe\" and should be used with care.

"),

("Base","disable_sigint","disable_sigint(f::Function)

   Disable Ctrl-C handler during execution of a function, for calling
   external code that is not interrupt safe. Intended to be called
   using \"do\" block syntax as follows:

      disable_sigint() do
          # interrupt-unsafe code
          ...
      end

"),

("Base","reenable_sigint","reenable_sigint(f::Function)

   Re-enable Ctrl-C handler during execution of a function.
   Temporarily reverses the effect of \"disable_sigint\".

"),

("Base","errno","errno([code])

   Get the value of the C library's \"errno\". If an argument is
   specified, it is used to set the value of \"errno\".

   The value of \"errno\" is only valid immediately after a \"ccall\"
   to a C library routine that sets it. Specifically, you cannot call
   \"errno\" at the next prompt in a REPL, because lots of code is
   executed between prompts.

"),

("Base","systemerror","systemerror(sysfunc, iftrue)

   Raises a \"SystemError\" for \"errno\" with the descriptive string
   \"sysfunc\" if \"bool\" is true

"),

("Base","strerror","strerror(n)

   Convert a system call error code to a descriptive string

"),

("Base","Cchar","Cchar

   Equivalent to the native \"char\" c-type

"),

("Base","Cuchar","Cuchar

   Equivalent to the native \"unsigned char\" c-type (UInt8)

"),

("Base","Cshort","Cshort

   Equivalent to the native \"signed short\" c-type (Int16)

"),

("Base","Cushort","Cushort

   Equivalent to the native \"unsigned short\" c-type (UInt16)

"),

("Base","Cint","Cint

   Equivalent to the native \"signed int\" c-type (Int32)

"),

("Base","Cuint","Cuint

   Equivalent to the native \"unsigned int\" c-type (UInt32)

"),

("Base","Clong","Clong

   Equivalent to the native \"signed long\" c-type

"),

("Base","Culong","Culong

   Equivalent to the native \"unsigned long\" c-type

"),

("Base","Clonglong","Clonglong

   Equivalent to the native \"signed long long\" c-type (Int64)

"),

("Base","Culonglong","Culonglong

   Equivalent to the native \"unsigned long long\" c-type (UInt64)

"),

("Base","Cintmax_t","Cintmax_t

   Equivalent to the native \"intmax_t\" c-type (Int64)

"),

("Base","Cuintmax_t","Cuintmax_t

   Equivalent to the native \"uintmax_t\" c-type (UInt64)

"),

("Base","Csize_t","Csize_t

   Equivalent to the native \"size_t\" c-type (UInt)

"),

("Base","Cssize_t","Cssize_t

   Equivalent to the native \"ssize_t\" c-type

"),

("Base","Cptrdiff_t","Cptrdiff_t

   Equivalent to the native \"ptrdiff_t\" c-type (Int)

"),

("Base","Coff_t","Coff_t

   Equivalent to the native \"off_t\" c-type

"),

("Base","Cwchar_t","Cwchar_t

   Equivalent to the native \"wchar_t\" c-type (Int32)

"),

("Base","Cfloat","Cfloat

   Equivalent to the native \"float\" c-type (Float32)

"),

("Base","Cdouble","Cdouble

   Equivalent to the native \"double\" c-type (Float64)

"),

("Base","error","error(message::AbstractString)

   Raise an error with the given message

"),

("Base","throw","throw(e)

   Throw an object as an exception

"),

("Base","rethrow","rethrow([e])

   Throw an object without changing the current exception backtrace.
   The default argument is the current exception (if called within a
   \"catch\" block).

"),

("Base","backtrace","backtrace()

   Get a backtrace object for the current program point.

"),

("Base","catch_backtrace","catch_backtrace()

   Get the backtrace of the current exception, for use within
   \"catch\" blocks.

"),

("Base","assert","assert(cond[, text])

   Raise an error if \"cond\" is false. Also available as the macro
   \"@assert expr\".

"),

("Base","@assert","@assert()

   Raise an error if \"cond\" is false. Preferred syntax for writings
   assertions.

"),

("Base","ArgumentError","ArgumentError

   The parameters given to a function call are not valid.

"),

("Base","BoundsError","BoundsError

   An indexing operation into an array tried to access an out-of-
   bounds element.

"),

("Base","EOFError","EOFError

   No more data was available to read from a file or stream.

"),

("Base","ErrorException","ErrorException

   Generic error type. The error message, in the *.msg* field, may
   provide more specific details.

"),

("Base","KeyError","KeyError

   An indexing operation into an \"Associative\" (\"Dict\") or \"Set\"
   like object tried to access or delete a non-existent element.

"),

("Base","LoadError","LoadError

   An error occurred while *including*, *requiring*, or *using* a
   file. The error specifics should be available in the *.error*
   field.

"),

("Base","MethodError","MethodError

   A method with the required type signature does not exist in the
   given generic function.

"),

("Base","ParseError","ParseError

   The expression passed to the *parse* function could not be
   interpreted as a valid Julia expression.

"),

("Base","ProcessExitedException","ProcessExitedException

   After a client Julia process has exited, further attempts to
   reference the dead child will throw this exception.

"),

("Base","SystemError","SystemError

   A system call failed with an error code (in the \"errno\" global
   variable).

"),

("Base","TypeError","TypeError

   A type assertion failure, or calling an intrinsic function with an
   incorrect argument type.

"),

("Base","Task","Task(func)

   Create a \"Task\" (i.e. thread, or coroutine) to execute the given
   function (which must be callable with no arguments). The task exits
   when this function returns.

"),

("Base","yieldto","yieldto(task, args...)

   Switch to the given task. The first time a task is switched to, the
   task's function is called with no arguments. On subsequent
   switches, \"args\" are returned from the task's last call to
   \"yieldto\". This is a low-level call that only switches tasks, not
   considering states or scheduling in any way.

"),

("Base","current_task","current_task()

   Get the currently running Task.

"),

("Base","istaskdone","istaskdone(task) -> Bool

   Tell whether a task has exited.

"),

("Base","consume","consume(task, values...)

   Receive the next value passed to \"produce\" by the specified task.
   Additional arguments may be passed, to be returned from the last
   \"produce\" call in the producer.

"),

("Base","produce","produce(value)

   Send the given value to the last \"consume\" call, switching to the
   consumer task. If the next \"consume\" call passes any values, they
   are returned by \"produce\".

"),

("Base","yield","yield()

   Switch to the scheduler to allow another scheduled task to run. A
   task that calls this function is still runnable, and will be
   restarted immediately if there are no other runnable tasks.

"),

("Base","task_local_storage","task_local_storage(symbol)

   Look up the value of a symbol in the current task's task-local
   storage.

"),

("Base","task_local_storage","task_local_storage(symbol, value)

   Assign a value to a symbol in the current task's task-local
   storage.

"),

("Base","task_local_storage","task_local_storage(body, symbol, value)

   Call the function \"body\" with a modified task-local storage, in
   which \"value\" is assigned to \"symbol\"; the previous value of
   \"symbol\", or lack thereof, is restored afterwards. Useful for
   emulating dynamic scoping.

"),

("Base","Condition","Condition()

   Create an edge-triggered event source that tasks can wait for.
   Tasks that call \"wait\" on a \"Condition\" are suspended and
   queued. Tasks are woken up when \"notify\" is later called on the
   \"Condition\". Edge triggering means that only tasks waiting at the
   time \"notify\" is called can be woken up. For level-triggered
   notifications, you must keep extra state to keep track of whether a
   notification has happened. The \"RemoteRef\" type does this, and so
   can be used for level-triggered events.

"),

("Base","notify","notify(condition, val=nothing; all=true, error=false)

   Wake up tasks waiting for a condition, passing them \"val\". If
   \"all\" is true (the default), all waiting tasks are woken,
   otherwise only one is. If \"error\" is true, the passed value is
   raised as an exception in the woken tasks.

"),

("Base","schedule","schedule(t::Task, [val]; error=false)

   Add a task to the scheduler's queue. This causes the task to run
   constantly when the system is otherwise idle, unless the task
   performs a blocking operation such as \"wait\".

   If a second argument is provided, it will be passed to the task
   (via the return value of \"yieldto\") when it runs again. If
   \"error\" is true, the value is raised as an exception in the woken
   task.

"),

("Base","@schedule","@schedule()

   Wrap an expression in a Task and add it to the scheduler's queue.

"),

("Base","@task","@task()

   Wrap an expression in a Task executing it, and return the Task.
   This only creates a task, and does not run it.

"),

("Base","sleep","sleep(seconds)

   Block the current task for a specified number of seconds. The
   minimum sleep time is 1 millisecond or input of \"0.001\".

"),

("Base","Timer","Timer(f::Function)

   Create a timer to call the given callback function. The callback is
   passed one argument, the timer object itself. The timer can be
   started and stopped with \"start_timer\" and \"stop_timer\".

"),

("Base","start_timer","start_timer(t::Timer, delay, repeat)

   Start invoking the callback for a \"Timer\" after the specified
   initial delay, and then repeating with the given interval. Times
   are in seconds. If \"repeat\" is \"0\", the timer is only triggered
   once.

"),

("Base","stop_timer","stop_timer(t::Timer)

   Stop invoking the callback for a timer.

"),

("Base","module_name","module_name(m::Module) -> Symbol

   Get the name of a module as a symbol.

"),

("Base","module_parent","module_parent(m::Module) -> Module

   Get a module's enclosing module. \"Main\" is its own parent.

"),

("Base","current_module","current_module() -> Module

   Get the *dynamically* current module, which is the module code is
   currently being read from. In general, this is not the same as the
   module containing the call to this function.

"),

("Base","fullname","fullname(m::Module)

   Get the fully-qualified name of a module as a tuple of symbols. For
   example, \"fullname(Base.Pkg)\" gives \"(:Base,:Pkg)\", and
   \"fullname(Main)\" gives \"()\".

"),

("Base","names","names(x::Module[, all=false[, imported=false]])

   Get an array of the names exported by a module, with optionally
   more module globals according to the additional parameters.

"),

("Base","names","names(x::DataType)

   Get an array of the fields of a data type.

"),

("Base","isconst","isconst([m::Module], s::Symbol) -> Bool

   Determine whether a global is declared \"const\" in a given module.
   The default module argument is \"current_module()\".

"),

("Base","isgeneric","isgeneric(f::Function) -> Bool

   Determine whether a function is generic.

"),

("Base","function_name","function_name(f::Function) -> Symbol

   Get the name of a generic function as a symbol, or \":anonymous\".

"),

("Base","function_module","function_module(f::Function, types) -> Module

   Determine the module containing a given definition of a generic
   function.

"),

("Base","functionloc","functionloc(f::Function, types)

   Returns a tuple \"(filename,line)\" giving the location of a method
   definition.

"),

("Base","functionlocs","functionlocs(f::Function, types)

   Returns an array of the results of \"functionloc\" for all matching
   definitions.

"),

("Base","gc","gc()

   Perform garbage collection. This should not generally be used.

"),

("Base","gc_disable","gc_disable()

   Disable garbage collection. This should be used only with extreme
   caution, as it can cause memory use to grow without bound.

"),

("Base","gc_enable","gc_enable()

   Re-enable garbage collection after calling \"gc_disable\".

"),

("Base","macroexpand","macroexpand(x)

   Takes the expression x and returns an equivalent expression with
   all macros removed (expanded).

"),

("Base","expand","expand(x)

   Takes the expression x and returns an equivalent expression in
   lowered form

"),

("Base","code_lowered","code_lowered(f, types)

   Returns an array of lowered ASTs for the methods matching the given
   generic function and type signature.

"),

("Base","@code_lowered","@code_lowered()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"code_lowered\" function on the resulting
   expression

"),

("Base","code_typed","code_typed(f, types)

   Returns an array of lowered and type-inferred ASTs for the methods
   matching the given generic function and type signature.

"),

("Base","@code_typed","@code_typed()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"code_typed\" function on the resulting
   expression

"),

("Base","code_llvm","code_llvm(f, types)

   Prints the LLVM bitcodes generated for running the method matching
   the given generic function and type signature to STDOUT.

"),

("Base","@code_llvm","@code_llvm()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"code_llvm\" function on the resulting
   expression

"),

("Base","code_native","code_native(f, types)

   Prints the native assembly instructions generated for running the
   method matching the given generic function and type signature to
   STDOUT.

"),

("Base","@code_native","@code_native()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"code_native\" function on the resulting
   expression

"),

("Base","precompile","precompile(f, args::(Any..., ))

   Compile the given function \"f\" for the argument tuple (of types)
   \"args\", but do not execute it.

"),

("Base.Collections","PriorityQueue","PriorityQueue(K, V[, ord])

   Construct a new PriorityQueue, with keys of type \"K\" and
   values/priorites of type \"V\". If an order is not given, the
   priority queue is min-ordered using the default comparison for
   \"V\".

"),

("Base.Collections","enqueue!","enqueue!(pq, k, v)

   Insert the a key \"k\" into a priority queue \"pq\" with priority
   \"v\".

"),

("Base.Collections","dequeue!","dequeue!(pq)

   Remove and return the lowest priority key from a priority queue.

"),

("Base.Collections","peek","peek(pq)

   Return the lowest priority key from a priority queue without
   removing that key from the queue.

"),

("Base.Collections","heapify","heapify(v[, ord])

   Return a new vector in binary heap order, optionally using the
   given ordering.

"),

("Base.Collections","heapify!","heapify!(v[, ord])

   In-place heapify.

"),

("Base.Collections","isheap","isheap(v[, ord])

   Return true iff an array is heap-ordered according to the given
   order.

"),

("Base.Collections","heappush!","heappush!(v, x[, ord])

   Given a binary heap-ordered array, push a new element \"x\",
   preserving the heap property. For efficiency, this function does
   not check that the array is indeed heap-ordered.

"),

("Base.Collections","heappop!","heappop!(v[, ord])

   Given a binary heap-ordered array, remove and return the lowest
   ordered element. For efficiency, this function does not check that
   the array is indeed heap-ordered.

"),

("Base","OS_NAME","OS_NAME

   A symbol representing the name of the operating system. Possible
   values are \":Linux\", \":Darwin\" (OS X), or \":Windows\".

"),

("Base","ARGS","ARGS

   An array of the command line arguments passed to Julia, as strings.

"),

("Base","C_NULL","C_NULL

   The C null pointer constant, sometimes used when calling external
   code.

"),

("Base","CPU_CORES","CPU_CORES

   The number of CPU cores in the system.

"),

("Base","WORD_SIZE","WORD_SIZE

   Standard word size on the current machine, in bits.

"),

("Base","VERSION","VERSION

   An object describing which version of Julia is in use.

"),

("Base","LOAD_PATH","LOAD_PATH

   An array of paths (as strings) where the \"require\" function looks
   for code.

"),

("Base","JULIA_HOME","JULIA_HOME

   A string containing the full path to the directory containing the
   \"julia\" executable.

"),

("Base","ANY","ANY

   Equivalent to \"Any\" for dispatch purposes, but signals the
   compiler to skip code generation specialization for that field

"),

("Dates","Period","Period

"),

("Dates","Year","Year

"),

("Dates","Month","Month

"),

("Dates","Week","Week

"),

("Dates","Day","Day

"),

("Dates","Hour","Hour

"),

("Dates","Minute","Minute

"),

("Dates","Second","Second

"),

("Dates","Millisecond","Millisecond

   \"Period\" types represent discrete, human representations of time.

"),

("Dates","Instant","Instant

   \"Instant\" types represent integer-based, machine representations
   of time as continuous timelines starting from an epoch.

"),

("Dates","UTInstant{T}","UTInstant{T}

   The \"UTInstant\" represents a machine timeline based on *UT* time
   (1 day = one revolution of the earth). The \"{T}\" is a \"Period\"
   parameter that indicates the resolution or precision of the
   instant.

"),

("Dates","TimeType","TimeType

   \"TimeType\" types wrap \"Instant\" machine instances to provide
   human representations of the machine instant.

"),

("Dates","DateTime","DateTime

   \"DateTime\" wraps a \"UTInstant{Millisecond}\" and interprets it
   according to the proleptic Gregorian calendar.

"),

("Dates","Date","Date

   \"Date\" wraps a \"UTInstant{Day}\" and interprets it according to
   the proleptic Gregorian calendar.

"),

("Dates","DateTime","DateTime(y[, m, d, h, mi, s, ms]) -> DateTime

   Construct a DateTime type by parts. Arguments must be convertible
   to \"Int64\".

"),

("Dates","DateTime","DateTime(periods::Period...) -> DateTime

   Constuct a DateTime type by \"Period\" type parts. Arguments may be
   in any order. DateTime parts not provided will default to the value
   of \"Dates.default(period)\".

"),

("Dates","DateTime","DateTime(f::Function, y[, m, d, h, mi, s]; step=Day(1), negate=false, limit=10000) -> DateTime

   Create a DateTime through the adjuster API. The starting point will
   be constructed from the provided \"y, m, d...\" arguments, and will
   be adjusted until \"f::Function\" returns true. The step size in
   adjusting can be provided manually through the \"step\" keyword. If
   \"negate=true\", then the adjusting will stop when \"f::Function\"
   returns false instead of true. \"limit\" provides a limit to the
   max number of iterations the adjustment API will pursue before
   throwing an error (in the case that \"f::Function\" is never
   satisfied).

"),

("Dates","DateTime","DateTime(dt::Date) -> DateTime

   Converts a \"Date\" type to a \"DateTime\". The hour, minute,
   second, and millisecond parts of the new \"DateTime\" are assumed
   to be zero.

"),

("Dates","DateTime","DateTime(dt::AbstractString, format::AbstractString; locale=\"english\") -> DateTime

   Construct a DateTime type by parsing the \"dt\" date string
   following the pattern given in the \"format\" string. The following
   codes can be used for constructing format strings:

   +-----------------+-----------+-----------------------------------------------------------------+
   | Code            | Matches   | Comment                                                         |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"y\\\"           | 1996, 96  | Returns year of 1996, 0096                                      |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"m\\\"           | 1, 01     | Matches 1 or 2-digit months                                     |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"u\\\"           | Jan       | Matches abbreviated months according to the \\\"locale\\\" keyword  |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"U\\\"           | January   | Matches full month names according to the \\\"locale\\\" keyword    |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"d\\\"           | 1, 01     | Matches 1 or 2-digit days                                       |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"H\\\"           | 00        | Matches hours                                                   |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"M\\\"           | 00        | Matches minutes                                                 |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"S\\\"           | 00        | Matches seconds                                                 |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"s\\\"           | .500      | Matches milliseconds                                            |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"e\\\"           | Mon, Tues | Matches abbreviated days of the week                            |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"E\\\"           | Monday    | Matches full name days of the week                              |
   +-----------------+-----------+-----------------------------------------------------------------+
   | \\\"yyyymmdd\\\"    | 19960101  | Matches fixed-width year, month, and day                        |
   +-----------------+-----------+-----------------------------------------------------------------+

   All characters not listed above are treated as delimiters between
   date and time slots. So a \"dt\" string of
   \"1996-01-15T00:00:00.0\" would have a \"format\" string like
   \"y-m-dTH:M:S.s\".

"),

("Dates","Date","Date(y[, m, d]) -> Date

   Construct a \"Date\" type by parts. Arguments must be convertible
   to \"Int64\".

"),

("Dates","Date","Date(period::Period...) -> Date

   Constuct a Date type by \"Period\" type parts. Arguments may be in
   any order. Date parts not provided will default to the value of
   \"Dates.default(period)\".

"),

("Dates","Date","Date(f::Function, y[, m]; step=Day(1), negate=false, limit=10000) -> Date

   Create a Date through the adjuster API. The starting point will be
   constructed from the provided \"y, m\" arguments, and will be
   adjusted until \"f::Function\" returns true. The step size in
   adjusting can be provided manually through the \"step\" keyword. If
   \"negate=true\", then the adjusting will stop when \"f::Function\"
   returns false instead of true. \"limit\" provides a limit to the
   max number of iterations the adjustment API will pursue before
   throwing an error (given that \"f::Function\" is never satisfied).

"),

("Dates","Date","Date(dt::DateTime) -> Date

   Converts a \"DateTime\" type to a \"Date\". The hour, minute,
   second, and millisecond parts of the \"DateTime\" are truncated, so
   only the year, month and day parts are used in construction.

"),

("Dates","Date","Date(dt::AbstractString, format::AbstractString; locale=\"english\") -> Date

   Construct a Date type by parsing a \"dt\" date string following the
   pattern given in the \"format\" string. Follows the same
   conventions as \"DateTime\" above.

"),

("Dates","now","now() -> DateTime

   Returns a DateTime corresponding to the user's system time
   including the system timezone locale.

"),

("Dates","nowutc","nowutc() -> DateTime

   Returns a DateTime corresponding to the user's system time as
   UTC/GMT.

"),

("Dates","year","year(dt::TimeType) -> Int64
month(dt::TimeType) -> Int64
week(dt::TimeType) -> Int64
day(dt::TimeType) -> Int64
hour(dt::TimeType) -> Int64
minute(dt::TimeType) -> Int64
second(dt::TimeType) -> Int64
millisecond(dt::TimeType) -> Int64

   Return the field part of a Date or DateTime as an \"Int64\".

"),

("Dates","Year","Year(dt::TimeType) -> Year
Month(dt::TimeType) -> Month
Week(dt::TimeType) -> Week
Day(dt::TimeType) -> Day
Hour(dt::TimeType) -> Hour
Minute(dt::TimeType) -> Minute
Second(dt::TimeType) -> Second
Millisecond(dt::TimeType) -> Millisecond

   Return the field part of a Date or DateTime as a \"Period\" type.

"),

("Dates","yearmonth","yearmonth(dt::TimeType) -> (Int64, Int64)

   Simultaneously return the year and month parts of a Date or
   DateTime.

"),

("Dates","monthday","monthday(dt::TimeType) -> (Int64, Int64)

   Simultaneously return the month and day parts of a Date or
   DateTime.

"),

("Dates","yearmonthday","yearmonthday(dt::TimeType) -> (Int64, Int64, Int64)

   Simultaneously return the year, month, and day parts of a Date or
   DateTime.

"),

("Dates","dayname","dayname(dt::TimeType; locale=\"english\") -> AbstractString

   Return the full day name corresponding to the day of the week of
   the Date or DateTime in the given \"locale\".

"),

("Dates","dayabbr","dayabbr(dt::TimeType; locale=\"english\") -> AbstractString

   Return the abbreviated name corresponding to the day of the week of
   the Date or DateTime in the given \"locale\".

"),

("Dates","dayofweek","dayofweek(dt::TimeType) -> Int64

   Returns the day of the week as an \"Int64\" with \"1 = Monday, 2 =
   Tuesday, etc.\".

"),

("Dates","dayofweekofmonth","dayofweekofmonth(dt::TimeType) -> Int

   For the day of week of \"dt\", returns which number it is in
   \"dt\"'s month. So if the day of the week of \"dt\" is Monday, then
   \"1 = First Monday of the month, 2 = Second Monday of the month,
   etc.\" In the range 1:5.

"),

("Dates","daysofweekinmonth","daysofweekinmonth(dt::TimeType) -> Int

   For the day of week of \"dt\", returns the total number of that day
   of the week in \"dt\"'s month. Returns 4 or 5. Useful in temporal
   expressions for specifying the last day of a week in a month by
   including \"dayofweekofmonth(dt) == daysofweekinmonth(dt)\" in the
   adjuster function.

"),

("Dates","monthname","monthname(dt::TimeType; locale=\"english\") -> AbstractString

   Return the full name of the month of the Date or DateTime in the
   given \"locale\".

"),

("Dates","monthabbr","monthabbr(dt::TimeType; locale=\"english\") -> AbstractString

   Return the abbreviated month name of the Date or DateTime in the
   given \"locale\".

"),

("Dates","daysinmonth","daysinmonth(dt::TimeType) -> Int

   Returns the number of days in the month of \"dt\". Value will be
   28, 29, 30, or 31.

"),

("Dates","isleapyear","isleapyear(dt::TimeType) -> Bool

   Returns true if the year of \"dt\" is a leap year.

"),

("Dates","dayofyear","dayofyear(dt::TimeType) -> Int

   Returns the day of the year for \"dt\" with January 1st being day
   1.

"),

("Dates","daysinyear","daysinyear(dt::TimeType) -> Int

   Returns 366 if the year of \"dt\" is a leap year, otherwise returns
   365.

"),

("Dates","quarterofyear","quarterofyear(dt::TimeType) -> Int

   Returns the quarter that \"dt\" resides in. Range of value is 1:4.

"),

("Dates","dayofquarter","dayofquarter(dt::TimeType) -> Int

   Returns the day of the current quarter of \"dt\". Range of value is
   1:92.

"),

("Dates","trunc","trunc(dt::TimeType, ::Type{Period}) -> TimeType

   Truncates the value of \"dt\" according to the provided \"Period\"
   type. E.g. if \"dt\" is \"1996-01-01T12:30:00\", then
   \"trunc(dt,Day) == 1996-01-01T00:00:00\".

"),

("Dates","firstdayofweek","firstdayofweek(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the Monday of it's week.

"),

("Dates","lastdayofweek","lastdayofweek(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the Sunday of it's week.

"),

("Dates","firstdayofmonth","firstdayofmonth(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the first day of it's month.

"),

("Dates","lastdayofmonth","lastdayofmonth(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the last day of it's month.

"),

("Dates","firstdayofyear","firstdayofyear(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the first day of it's year.

"),

("Dates","lastdayofyear","lastdayofyear(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the last day of it's year.

"),

("Dates","firstdayofquarter","firstdayofquarter(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the first day of it's quarter.

"),

("Dates","lastdayofquarter","lastdayofquarter(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the last day of it's quarter.

"),

("Dates","tonext","tonext(dt::TimeType, dow::Int;same::Bool=false) -> TimeType

   Adjusts \"dt\" to the next day of week corresponding to \"dow\"
   with \"1 = Monday, 2 = Tuesday, etc\". Setting \"same=true\" allows
   the current \"dt\" to be considered as the next \"dow\", allowing
   for no adjustment to occur.

"),

("Dates","toprev","toprev(dt::TimeType, dow::Int;same::Bool=false) -> TimeType

   Adjusts \"dt\" to the previous day of week corresponding to \"dow\"
   with \"1 = Monday, 2 = Tuesday, etc\". Setting \"same=true\" allows
   the current \"dt\" to be considered as the previous \"dow\",
   allowing for no adjustment to occur.

"),

("Dates","tofirst","tofirst(dt::TimeType, dow::Int;of=Month) -> TimeType

   Adjusts \"dt\" to the first \"dow\" of it's month. Alternatively,
   \"of=Year\" will adjust to the first \"dow\" of the year.

"),

("Dates","tolast","tolast(dt::TimeType, dow::Int;of=Month) -> TimeType

   Adjusts \"dt\" to the last \"dow\" of it's month. Alternatively,
   \"of=Year\" will adjust to the last \"dow\" of the year.

"),

("Dates","tonext","tonext(func::Function, dt::TimeType;step=Day(1), negate=false, limit=10000, same=false) -> TimeType

   Adjusts \"dt\" by iterating at most \"limit\" iterations by
   \"step\" increments until \"func\" returns true. \"func\" must take
   a single \"TimeType\" argument and return a \"Bool\". \"same\"
   allows \"dt\" to be considered in satisfying \"func\". \"negate\"
   will make the adjustment process terminate when \"func\" returns
   false instead of true.

"),

("Dates","toprev","toprev(func::Function, dt::TimeType;step=Day(-1), negate=false, limit=10000, same=false) -> TimeType

   Adjusts \"dt\" by iterating at most \"limit\" iterations by
   \"step\" increments until \"func\" returns true. \"func\" must take
   a single \"TimeType\" argument and return a \"Bool\". \"same\"
   allows \"dt\" to be considered in satisfying \"func\". \"negate\"
   will make the adjustment process terminate when \"func\" returns
   false instead of true.

"),

("Dates","recur{T<:TimeType}","recur{T<:TimeType}(func::Function, dr::StepRange{T};negate=false, limit=10000) -> Vector{T}

   \"func\" takes a single TimeType argument and returns a \"Bool\"
   indicating whether the input should be \"included\" in the final
   set. \"recur\" applies \"func\" over each element in the range of
   \"dr\", including those elements for which \"func\" returns
   \"true\" in the resulting Array, unless \"negate=true\", then only
   elements where \"func\" returns \"false\" are included.

"),

("Dates","Year","Year(v)
Month(v)
Week(v)
Day(v)
Hour(v)
Minute(v)
Second(v)
Millisecond(v)

   Construct a \"Period\" type with the given \"v\" value. Input must
   be losslessly convertible to an \"Int64\".

"),

("","default(p::Period) => Period","default(p::Period) => Period

   Returns a sensible \"default\" value for the input Period by
   returning \"one(p)\" for Year, Month, and Day, and \"zero(p)\" for
   Hour, Minute, Second, and Millisecond.

"),

("Dates","today","today() -> Date

   Returns the date portion of \"now()\".

"),

("Dates","unix2datetime","unix2datetime(x) -> DateTime

   Takes the number of seconds since unix epoch
   \"1970-01-01T00:00:00\" and converts to the corresponding DateTime.

"),

("Dates","datetime2unix","datetime2unix(dt::DateTime) -> Float64

   Takes the given DateTime and returns the number of seconds since
   the unix epoch as a \"Float64\".

"),

("Dates","julian2datetime","julian2datetime(julian_days) -> DateTime

   Takes the number of Julian calendar days since epoch
   \"-4713-11-24T12:00:00\" and returns the corresponding DateTime.

"),

("Dates","datetime2julian","datetime2julian(dt::DateTime) -> Float64

   Takes the given DateTime and returns the number of Julian calendar
   days since the julian epoch as a \"Float64\".

"),

("Dates","rata2datetime","rata2datetime(days) -> DateTime

   Takes the number of Rata Die days since epoch
   \"0000-12-31T00:00:00\" and returns the corresponding DateTime.

"),

("Dates","datetime2rata","datetime2rata(dt::TimeType) -> Int64

   Returns the number of Rata Die days since epoch from the given Date
   or DateTime.

"),

("Base","isblockdev","isblockdev(path) -> Bool

   Returns \"true\" if \"path\" is a block device, \"false\"
   otherwise.

"),

("Base","ischardev","ischardev(path) -> Bool

   Returns \"true\" if \"path\" is a character device, \"false\"
   otherwise.

"),

("Base","isdir","isdir(path) -> Bool

   Returns \"true\" if \"path\" is a directory, \"false\" otherwise.

"),

("Base","isexecutable","isexecutable(path) -> Bool

   Returns \"true\" if the current user has permission to execute
   \"path\", \"false\" otherwise.

"),

("Base","isfifo","isfifo(path) -> Bool

   Returns \"true\" if \"path\" is a FIFO, \"false\" otherwise.

"),

("Base","isfile","isfile(path) -> Bool

   Returns \"true\" if \"path\" is a regular file, \"false\"
   otherwise.

"),

("Base","islink","islink(path) -> Bool

   Returns \"true\" if \"path\" is a symbolic link, \"false\"
   otherwise.

"),

("Base","ispath","ispath(path) -> Bool

   Returns \"true\" if \"path\" is a valid filesystem path, \"false\"
   otherwise.

"),

("Base","isreadable","isreadable(path) -> Bool

   Returns \"true\" if the current user has permission to read
   \"path\", \"false\" otherwise.

"),

("Base","issetgid","issetgid(path) -> Bool

   Returns \"true\" if \"path\" has the setgid flag set, \"false\"
   otherwise.

"),

("Base","issetuid","issetuid(path) -> Bool

   Returns \"true\" if \"path\" has the setuid flag set, \"false\"
   otherwise.

"),

("Base","issocket","issocket(path) -> Bool

   Returns \"true\" if \"path\" is a socket, \"false\" otherwise.

"),

("Base","issticky","issticky(path) -> Bool

   Returns \"true\" if \"path\" has the sticky bit set, \"false\"
   otherwise.

"),

("Base","iswritable","iswritable(path) -> Bool

   Returns \"true\" if the current user has permission to write to
   \"path\", \"false\" otherwise.

"),

("Base","homedir","homedir() -> AbstractString

   Return the current user's home directory.

"),

("Base","dirname","dirname(path::AbstractString) -> AbstractString

   Get the directory part of a path.

"),

("Base","basename","basename(path::AbstractString) -> AbstractString

   Get the file name part of a path.

"),

("Base","@__FILE__","@__FILE__() -> AbstractString

   \"@__FILE__\" expands to a string with the absolute path and file
   name of the script being run. Returns \"nothing\" if run from a
   REPL or an empty string if evaluated by \"julia -e <expr>\".

"),

("Base","isabspath","isabspath(path::AbstractString) -> Bool

   Determines whether a path is absolute (begins at the root
   directory).

"),

("Base","isdirpath","isdirpath(path::AbstractString) -> Bool

   Determines whether a path refers to a directory (for example, ends
   with a path separator).

"),

("Base","joinpath","joinpath(parts...) -> AbstractString

   Join path components into a full path. If some argument is an
   absolute path, then prior components are dropped.

"),

("Base","abspath","abspath(path::AbstractString) -> AbstractString

   Convert a path to an absolute path by adding the current directory
   if necessary.

"),

("Base","normpath","normpath(path::AbstractString) -> AbstractString

   Normalize a path, removing \".\" and \"..\" entries.

"),

("Base","realpath","realpath(path::AbstractString) -> AbstractString

   Canonicalize a path by expanding symbolic links and removing \".\"
   and \"..\" entries.

"),

("Base","expanduser","expanduser(path::AbstractString) -> AbstractString

   On Unix systems, replace a tilde character at the start of a path
   with the current user's home directory.

"),

("Base","splitdir","splitdir(path::AbstractString) -> (AbstractString, AbstractString)

   Split a path into a tuple of the directory name and file name.

"),

("Base","splitdrive","splitdrive(path::AbstractString) -> (AbstractString, AbstractString)

   On Windows, split a path into the drive letter part and the path
   part. On Unix systems, the first component is always the empty
   string.

"),

("Base","splitext","splitext(path::AbstractString) -> (AbstractString, AbstractString)

   If the last component of a path contains a dot, split the path into
   everything before the dot and everything including and after the
   dot. Otherwise, return a tuple of the argument unmodified and the
   empty string.

"),

("Base","tempname","tempname()

   Generate a unique temporary file path.

"),

("Base","tempdir","tempdir()

   Obtain the path of a temporary directory (possibly shared with
   other processes).

"),

("Base","mktemp","mktemp()

   Returns \"(path, io)\", where \"path\" is the path of a new
   temporary file and \"io\" is an open file object for this path.

"),

("Base","mktempdir","mktempdir()

   Create a temporary directory and return its path.

"),

("Base.Graphics","Vec2","Vec2(x, y)

   Creates a point in two dimensions

"),

("Base.Graphics","BoundingBox","BoundingBox(xmin, xmax, ymin, ymax)

   Creates a box in two dimensions with the given edges

"),

("Base.Graphics","BoundingBox","BoundingBox(objs...)

   Creates a box in two dimensions that encloses all objects

"),

("Base.Graphics","width","width(obj)

   Computes the width of an object

"),

("Base.Graphics","height","height(obj)

   Computes the height of an object

"),

("Base.Graphics","xmin","xmin(obj)

   Computes the minimum x-coordinate contained in an object

"),

("Base.Graphics","xmax","xmax(obj)

   Computes the maximum x-coordinate contained in an object

"),

("Base.Graphics","ymin","ymin(obj)

   Computes the minimum y-coordinate contained in an object

"),

("Base.Graphics","ymax","ymax(obj)

   Computes the maximum y-coordinate contained in an object

"),

("Base.Graphics","diagonal","diagonal(obj)

   Return the length of the diagonal of an object

"),

("Base.Graphics","aspect_ratio","aspect_ratio(obj)

   Compute the height/width of an object

"),

("Base.Graphics","center","center(obj)

   Return the point in the center of an object

"),

("Base.Graphics","xrange","xrange(obj)

   Returns a tuple \"(xmin(obj), xmax(obj))\"

"),

("Base.Graphics","yrange","yrange(obj)

   Returns a tuple \"(ymin(obj), ymax(obj))\"

"),

("Base.Graphics","rotate","rotate(obj, angle, origin) -> newobj

   Rotates an object around origin by the specified angle (radians),
   returning a new object of the same type.  Because of type-
   constancy, this new object may not always be a strict geometric
   rotation of the input; for example, if \"obj\" is a \"BoundingBox\"
   the return is the smallest \"BoundingBox\" that encloses the
   rotated input.

"),

("Base.Graphics","shift","shift(obj, dx, dy)

   Returns an object shifted horizontally and vertically by the
   indicated amounts

"),

("Base.Graphics","*","*(obj, s::Real)

   Scale the width and height of a graphics object, keeping the center
   fixed

"),

("Base.Graphics","+","+(bb1::BoundingBox, bb2::BoundingBox) -> BoundingBox

   Returns the smallest box containing both boxes

"),

("Base.Graphics","&","&(bb1::BoundingBox, bb2::BoundingBox) -> BoundingBox

   Returns the intersection, the largest box contained in both boxes

"),

("Base.Graphics","deform","deform(bb::BoundingBox, dxmin, dxmax, dymin, dymax)

   Returns a bounding box with all edges shifted by the indicated
   amounts

"),

("Base.Graphics","isinside","isinside(bb::BoundingBox, x, y)

   True if the given point is inside the box

"),

("Base.Graphics","isinside","isinside(bb::BoundingBox, point)

   True if the given point is inside the box

"),


("Base","*","*(A, B)

   Matrix multiplication

"),

("Base","\\","\\(A, B)

   Matrix division using a polyalgorithm. For input matrices \"A\" and
   \"B\", the result \"X\" is such that \"A*X == B\" when \"A\" is
   square.  The solver that is used depends upon the structure of
   \"A\".  A direct solver is used for upper- or lower triangular
   \"A\".  For Hermitian \"A\" (equivalent to symmetric \"A\" for non-
   complex \"A\") the \"BunchKaufman\" factorization is used.
   Otherwise an LU factorization is used. For rectangular \"A\" the
   result is the minimum-norm least squares solution computed by a
   pivoted QR factorization of \"A\" and a rank estimate of A based on
   the R factor. For sparse, square \"A\" the LU factorization (from
   UMFPACK) is used.

"),

("Base","dot","dot(x, y)
⋅(x, y)

   Compute the dot product. For complex vectors, the first vector is
   conjugated.

"),

("Base","cross","cross(x, y)
×(x, y)

   Compute the cross product of two 3-vectors.

"),

("Base","rref","rref(A)

   Compute the reduced row echelon form of the matrix A.

"),

("Base","factorize","factorize(A)

   Compute a convenient factorization (including LU, Cholesky, Bunch-
   Kaufman, Triangular) of A, based upon the type of the input matrix.
   The return value can then be reused for efficient solving of
   multiple systems. For example: \"A=factorize(A); x=A\\\\b;
   y=A\\\\C\".

"),

("Base","factorize!","factorize!(A)

   \"factorize!\" is the same as \"factorize()\", but saves space by
   overwriting the input \"A\", instead of creating a copy.

"),

("Base","lu","lu(A) -> L, U, p

   Compute the LU factorization of \"A\", such that \"A[p,:] = L*U\".

"),

("Base","lufact","lufact(A[, pivot=true]) -> F

   Compute the LU factorization of \"A\". The return type of \"F\"
   depends on the type of \"A\". In most cases, if \"A\" is a subtype
   \"S\" of AbstractMatrix with an element type \"T`\" supporting
   \"+\", \"-\", \"*\" and \"/\" the return type is \"LU{T,S{T}}\". If
   pivoting is chosen (default) the element type should also support
   \"abs\" and \"<\". When \"A\" is sparse and have element of type
   \"Float32\", \"Float64\", \"Complex{Float32}\", or
   \"Complex{Float64}\" the return type is \"UmfpackLU\". Some
   examples are shown in the table below.

      +-------------------------+---------------------------+----------------------------------------------+
      | Type of input \\\"A\\\"     | Type of output \\\"F\\\"      | Relationship between \\\"F\\\" and \\\"A\\\"         |
      +-------------------------+---------------------------+----------------------------------------------+
      | \\\"Matrix()\\\"            | \\\"LU\\\"                    | \\\"F[:L]*F[:U] == A[F[:p], :]\\\"               |
      +-------------------------+---------------------------+----------------------------------------------+
      | \\\"Tridiagonal()\\\"       | \\\"LU{T,Tridiagonal{T}}\\\"  | N/A                                          |
      +-------------------------+---------------------------+----------------------------------------------+
      | \\\"SparseMatrixCSC()\\\"   | \\\"UmfpackLU\\\"             | \\\"F[:L]*F[:U] == F[:Rs] .* A[F[:p], F[:q]]\\\" |
      +-------------------------+---------------------------+----------------------------------------------+

   The individual components of the factorization \"F\" can be
   accessed by indexing:

      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | Component   | Description                             | \\\"LU\\\" | \\\"LU{T,Tridiagonal{T}}\\\" | \\\"UmfpackLU\\\" |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \\\"F[:L]\\\"   | \\\"L\\\" (lower triangular) part of \\\"LU\\\" | ✓      |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \\\"F[:U]\\\"   | \\\"U\\\" (upper triangular) part of \\\"LU\\\" | ✓      |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \\\"F[:p]\\\"   | (right) permutation \\\"Vector\\\"          | ✓      |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \\\"F[:P]\\\"   | (right) permutation \\\"Matrix\\\"          | ✓      |                          |               |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \\\"F[:q]\\\"   | left permutation \\\"Vector\\\"             |        |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \\\"F[:Rs]\\\"  | \\\"Vector\\\" of scaling factors           |        |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \\\"F[:(:)]\\\" | \\\"(L,U,p,q,Rs)\\\" components             |        |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+

      +--------------------+--------+--------------------------+---------------+
      | Supported function | \\\"LU\\\" | \\\"LU{T,Tridiagonal{T}}\\\" | \\\"UmfpackLU\\\" |
      +--------------------+--------+--------------------------+---------------+
      | \\\"/\\\"              | ✓      |                          |               |
      +--------------------+--------+--------------------------+---------------+
      | \\\"\\\\\\\"             | ✓      | ✓                        | ✓             |
      +--------------------+--------+--------------------------+---------------+
      | \\\"cond\\\"           | ✓      |                          | ✓             |
      +--------------------+--------+--------------------------+---------------+
      | \\\"det\\\"            | ✓      | ✓                        | ✓             |
      +--------------------+--------+--------------------------+---------------+
      | \\\"size\\\"           | ✓      | ✓                        |               |
      +--------------------+--------+--------------------------+---------------+

"),

("Base","lufact!","lufact!(A) -> LU

   \"lufact!\" is the same as \"lufact()\", but saves space by
   overwriting the input A, instead of creating a copy.  For sparse
   \"A\" the \"nzval\" field is not overwritten but the index fields,
   \"colptr\" and \"rowval\" are decremented in place, converting from
   1-based indices to 0-based indices.

"),

("Base","chol","chol(A[, LU]) -> F

   Compute the Cholesky factorization of a symmetric positive definite
   matrix \"A\" and return the matrix \"F\". If \"LU\" is \":L\"
   (Lower), \"A = L*L'\". If \"LU\" is \":U\" (Upper), \"A = R'*R\".

"),

("Base","cholfact","cholfact(A, [LU,][pivot=false,][tol=-1.0]) -> Cholesky

   Compute the Cholesky factorization of a dense symmetric positive
   (semi)definite matrix \"A\" and return either a \"Cholesky\" if
   \"pivot=false\" or \"CholeskyPivoted\" if \"pivot=true\". \"LU\"
   may be \":L\" for using the lower part or \":U\" for the upper
   part. The default is to use \":U\". The triangular matrix can be
   obtained from the factorization \"F\" with: \"F[:L]\" and
   \"F[:U]\". The following functions are available for \"Cholesky\"
   objects: \"size\", \"\\\", \"inv\", \"det\". For
   \"CholeskyPivoted\" there is also defined a \"rank\". If
   \"pivot=false\" a \"PosDefException\" exception is thrown in case
   the matrix is not positive definite. The argument \"tol\"
   determines the tolerance for determining the rank. For negative
   values, the tolerance is the machine precision.

"),

("Base","cholfact","cholfact(A[, ll]) -> CholmodFactor

   Compute the sparse Cholesky factorization of a sparse matrix \"A\".
   If \"A\" is Hermitian its Cholesky factor is determined.  If \"A\"
   is not Hermitian the Cholesky factor of \"A*A'\" is determined. A
   fill-reducing permutation is used.  Methods for \"size\",
   \"solve\", \"\\\", \"findn_nzs\", \"diag\", \"det\" and \"logdet\"
   are available for \"CholmodFactor\" objects.  One of the solve
   methods includes an integer argument that can be used to solve
   systems involving parts of the factorization only.  The optional
   boolean argument, \"ll\" determines whether the factorization
   returned is of the \"A[p,p] = L*L'\" form, where \"L\" is lower
   triangular or \"A[p,p] = L*Diagonal(D)*L'\" form where \"L\" is
   unit lower triangular and \"D\" is a non-negative vector.  The
   default is LDL. The symbolic factorization can also be reused for
   other matrices with the same structure as \"A\" by calling
   \"cholfact!\".

"),

("Base","cholfact!","cholfact!(A, [LU,][pivot=false,][tol=-1.0]) -> Cholesky

   \"cholfact!\" is the same as \"cholfact()\", but saves space by
   overwriting the input \"A\", instead of creating a copy.
   \"cholfact!\" can also reuse the symbolic factorization from a
   different matrix \"F\" with the same structure when used as:
   \"cholfact!(F::CholmodFactor, A)\".

"),

("Base","ldltfact","ldltfact(A) -> LDLtFactorization

   Compute a factorization of a positive definite matrix \"A\" such
   that \"A=L*Diagonal(d)*L'\" where \"L\" is a unit lower triangular
   matrix and \"d\" is a vector with non-negative elements.

"),

("Base","qr","qr(A, [pivot=false,][thin=true]) -> Q, R, [p]

   Compute the (pivoted) QR factorization of \"A\" such that either
   \"A = Q*R\" or \"A[:,p] = Q*R\". Also see \"qrfact\". The default
   is to compute a thin factorization. Note that \"R\" is not extended
   with zeros when the full \"Q\" is requested.

"),

("Base","qrfact","qrfact(A[, pivot=false]) -> F

   Computes the QR factorization of \"A\". The return type of \"F\"
   depends on the element type of \"A\" and whether pivoting is
   specified (with \"pivot=true\").

      +------------------+-------------------+-----------+---------------------------------------+
      | Return type      | \\\"eltype(A)\\\"     | \\\"pivot\\\" | Relationship between \\\"F\\\" and \\\"A\\\"  |
      +------------------+-------------------+-----------+---------------------------------------+
      | \\\"QR\\\"           | not \\\"BlasFloat\\\" | either    | \\\"A==F[:Q]*F[:R]\\\"                    |
      +------------------+-------------------+-----------+---------------------------------------+
      | \\\"QRCompactWY\\\"  | \\\"BlasFloat\\\"     | \\\"false\\\" | \\\"A==F[:Q]*F[:R]\\\"                    |
      +------------------+-------------------+-----------+---------------------------------------+
      | \\\"QRPivoted\\\"    | \\\"BlasFloat\\\"     | \\\"true\\\"  | \\\"A[:,F[:p]]==F[:Q]*F[:R]\\\"           |
      +------------------+-------------------+-----------+---------------------------------------+

   \"BlasFloat\" refers to any of: \"Float32\", \"Float64\",
   \"Complex64\" or \"Complex128\".

   The individual components of the factorization \"F\" can be
   accessed by indexing:

      +-------------+-----------------------------------------------+--------------------+-----------------------+--------------------+
      | Component   | Description                                   | \\\"QR\\\"             | \\\"QRCompactWY\\\"       | \\\"QRPivoted\\\"      |
      +-------------+-----------------------------------------------+--------------------+-----------------------+--------------------+
      | \\\"F[:Q]\\\"   | \\\"Q\\\" (orthogonal/unitary) part of \\\"QR\\\"     | ✓ (\\\"QRPackedQ\\\")  | ✓ (\\\"QRCompactWYQ\\\")  | ✓ (\\\"QRPackedQ\\\")  |
      +-------------+-----------------------------------------------+--------------------+-----------------------+--------------------+
      | \\\"F[:R]\\\"   | \\\"R\\\" (upper right triangular) part of \\\"QR\\\" | ✓                  | ✓                     | ✓                  |
      +-------------+-----------------------------------------------+--------------------+-----------------------+--------------------+
      | \\\"F[:p]\\\"   | pivot \\\"Vector\\\"                              |                    |                       | ✓                  |
      +-------------+-----------------------------------------------+--------------------+-----------------------+--------------------+
      | \\\"F[:P]\\\"   | (pivot) permutation \\\"Matrix\\\"                |                    |                       | ✓                  |
      +-------------+-----------------------------------------------+--------------------+-----------------------+--------------------+

   The following functions are available for the \"QR\" objects:
   \"size\", \"\\\". When \"A\" is rectangular, \"\\\" will return a
   least squares solution and if the solution is not unique, the one
   with smallest norm is returned.

   Multiplication with respect to either thin or full \"Q\" is
   allowed, i.e. both \"F[:Q]*F[:R]\" and \"F[:Q]*A\" are supported. A
   \"Q\" matrix can be converted into a regular matrix with \"full()\"
   which has a named argument \"thin\".

   Note: \"qrfact\" returns multiple types because LAPACK uses
     several representations that minimize the memory storage
     requirements of products of Householder elementary reflectors, so
     that the \"Q\" and \"R\" matrices can be stored compactly rather
     as two separate dense matrices.The data contained in \"QR\" or
     \"QRPivoted\" can be used to construct the \"QRPackedQ\" type,
     which is a compact representation of the rotation matrix:

           Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T)

     where \\tau_i is the scale factor and v_i is the projection
     vector associated with the i^{th} Householder elementary
     reflector.The data contained in \"QRCompactWY\" can be used to
     construct the \"QRCompactWYQ\" type, which is a compact
     representation of the rotation matrix

           Q = I + Y T Y^T

     where \"Y\" is m \\times r lower trapezoidal and \"T\" is r
     \\times r upper triangular. The *compact WY* representation
     [Schreiber1989] is not to be confused with the older, *WY*
     representation [Bischof1987]. (The LAPACK documentation uses
     \"V\" in lieu of \"Y\".)

   [Bischof1987] C Bischof and C Van Loan, The WY
                 representation for products of Householder matrices,
                 SIAM J Sci Stat Comput 8 (1987), s2-s13.
                 doi:10.1137/0908009

   [Schreiber1989] R Schreiber and C Van Loan, A
                   storage-efficient WY representation for products of
                   Householder transformations, SIAM J Sci Stat Comput
                   10 (1989), 53-57. doi:10.1137/0910005

"),

("Base","qrfact!","qrfact!(A[, pivot=false])

   \"qrfact!\" is the same as \"qrfact()\", but saves space by
   overwriting the input \"A\", instead of creating a copy.

"),

("Base","bkfact","bkfact(A) -> BunchKaufman

   Compute the Bunch-Kaufman [Bunch1977] factorization of a real
   symmetric or complex Hermitian matrix \"A\" and return a
   \"BunchKaufman\" object. The following functions are available for
   \"BunchKaufman\" objects: \"size\", \"\\\", \"inv\", \"issym\",
   \"ishermitian\".

"),

("Base","bkfact!","bkfact!(A) -> BunchKaufman

   \"bkfact!\" is the same as \"bkfact()\", but saves space by
   overwriting the input \"A\", instead of creating a copy.

"),

("Base","sqrtm","sqrtm(A)

   Compute the matrix square root of \"A\". If \"B = sqrtm(A)\", then
   \"B*B == A\" within roundoff error.

   \"sqrtm\" uses a polyalgorithm, computing the matrix square root
   using Schur factorizations (\"schurfact()\") unless it detects the
   matrix to be Hermitian or real symmetric, in which case it computes
   the matrix square root from an eigendecomposition (\"eigfact()\").
   In the latter situation for positive definite matrices, the matrix
   square root has \"Real\" elements, otherwise it has \"Complex\"
   elements.

"),

("Base","eig","eig(A,[irange,][vl,][vu,][permute=true,][scale=true]) -> D, V

   Computes eigenvalues and eigenvectors of \"A\". See \"eigfact()\"
   for details on the \"balance\" keyword argument.

      julia> eig([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
      ([1.0,3.0,18.0],
      3x3 Array{Float64,2}:
       1.0  0.0  0.0
       0.0  1.0  0.0
       0.0  0.0  1.0)

   \"eig\" is a wrapper around \"eigfact()\", extracting all parts of
   the factorization to a tuple; where possible, using \"eigfact()\"
   is recommended.

"),

("Base","eig","eig(A, B) -> D, V

   Computes generalized eigenvalues and vectors of \"A\" with respect
   to \"B\".

   \"eig\" is a wrapper around \"eigfact()\", extracting all parts of
   the factorization to a tuple; where possible, using \"eigfact()\"
   is recommended.

"),

("Base","eigvals","eigvals(A,[irange,][vl,][vu])

   Returns the eigenvalues of \"A\". If \"A\" is \"Symmetric()\",
   \"Hermitian()\" or \"SymTridiagonal()\", it is possible to
   calculate only a subset of the eigenvalues by specifying either a
   \"UnitRange()\" \"irange\" covering indices of the sorted
   eigenvalues, or a pair \"vl\" and \"vu\" for the lower and upper
   boundaries of the eigenvalues.

   For general non-symmetric matrices it is possible to specify how
   the matrix is balanced before the eigenvector calculation. The
   option \"permute=true\" permutes the matrix to become closer to
   upper triangular, and \"scale=true\" scales the matrix by its
   diagonal elements to make rows and columns more equal in norm. The
   default is \"true\" for both options.

"),

("Base","eigmax","eigmax(A)

   Returns the largest eigenvalue of \"A\".

"),

("Base","eigmin","eigmin(A)

   Returns the smallest eigenvalue of \"A\".

"),

("Base","eigvecs","eigvecs(A, [eigvals,][permute=true,][scale=true]) -> Matrix

   Returns a matrix \"M\" whose columns are the eigenvectors of \"A\".
   (The \"k``th eigenvector can be obtained from the slice ``M[:,
   k]\".) The \"permute\" and \"scale\" keywords are the same as for
   \"eigfact()\".

   For \"SymTridiagonal()\" matrices, if the optional vector of
   eigenvalues \"eigvals\" is specified, returns the specific
   corresponding eigenvectors.

"),

("Base","eigfact","eigfact(A,[irange,][vl,][vu,][permute=true,][scale=true]) -> Eigen

   Computes the eigenvalue decomposition of \"A\", returning an
   \"Eigen\" factorization object \"F\" which contains the eigenvalues
   in \"F[:values]\" and the eigenvectors in the columns of the matrix
   \"F[:vectors]\". (The \"k``th eigenvector can be obtained from the
   slice ``F[:vectors][:, k]\".)

   The following functions are available for \"Eigen\" objects:
   \"inv\", \"det\".

   If \"A\" is \"Symmetric()\", \"Hermitian()\" or
   \"SymTridiagonal()\", it is possible to calculate only a subset of
   the eigenvalues by specifying either a \"UnitRange()\" \"irange\"
   covering indices of the sorted eigenvalues or a pair \"vl\" and
   \"vu\" for the lower and upper boundaries of the eigenvalues.

   For general nonsymmetric matrices it is possible to specify how the
   matrix is balanced before the eigenvector calculation. The option
   \"permute=true\" permutes the matrix to become closer to upper
   triangular, and \"scale=true\" scales the matrix by its diagonal
   elements to make rows and columns more equal in norm. The default
   is \"true\" for both options.

"),

("Base","eigfact","eigfact(A, B) -> GeneralizedEigen

   Computes the generalized eigenvalue decomposition of \"A\" and
   \"B\", returning a \"GeneralizedEigen\" factorization object \"F\"
   which contains the generalized eigenvalues in \"F[:values]\" and
   the generalized eigenvectors in the columns of the matrix
   \"F[:vectors]\". (The \"k``th generalized eigenvector can be
   obtained from the slice ``F[:vectors][:, k]\".)

"),

("Base","eigfact!","eigfact!(A[, B])

   Same as \"eigfact()\", but saves space by overwriting the input
   \"A\" (and \"B\"), instead of creating a copy.

"),

("Base","hessfact","hessfact(A)

   Compute the Hessenberg decomposition of \"A\" and return a
   \"Hessenberg\" object. If \"F\" is the factorization object, the
   unitary matrix can be accessed with \"F[:Q]\" and the Hessenberg
   matrix with \"F[:H]\". When \"Q\" is extracted, the resulting type
   is the \"HessenbergQ\" object, and may be converted to a regular
   matrix with \"full()\".

"),

("Base","hessfact!","hessfact!(A)

   \"hessfact!\" is the same as \"hessfact()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Base","schurfact","schurfact(A) -> Schur

   Computes the Schur factorization of the matrix \"A\". The (quasi)
   triangular Schur factor can be obtained from the \"Schur\" object
   \"F\" with either \"F[:Schur]\" or \"F[:T]\" and the
   unitary/orthogonal Schur vectors can be obtained with
   \"F[:vectors]\" or \"F[:Z]\" such that
   \"A=F[:vectors]*F[:Schur]*F[:vectors]'\". The eigenvalues of \"A\"
   can be obtained with \"F[:values]\".

"),

("Base","schurfact!","schurfact!(A)

   Computes the Schur factorization of \"A\", overwriting \"A\" in the
   process. See \"schurfact()\"

"),

("Base","schur","schur(A) -> Schur[:T], Schur[:Z], Schur[:values]

   See \"schurfact()\"

"),

("Base","ordschur","ordschur(Q, T, select) -> Schur

   Reorders the Schur factorization of a real matrix \"A=Q*T*Q'\"
   according to the logical array \"select\" returning a Schur object
   \"F\". The selected eigenvalues appear in the leading diagonal of
   \"F[:Schur]\" and the the corresponding leading columns of
   \"F[:vectors]\" form an orthonormal basis of the corresponding
   right invariant subspace. A complex conjugate pair of eigenvalues
   must be either both included or excluded via \"select\".

"),

("Base","ordschur!","ordschur!(Q, T, select) -> Schur

   Reorders the Schur factorization of a real matrix \"A=Q*T*Q'\",
   overwriting \"Q\" and \"T\" in the process. See \"ordschur()\"

"),

("Base","ordschur","ordschur(S, select) -> Schur

   Reorders the Schur factorization \"S\" of type \"Schur\".

"),

("Base","ordschur!","ordschur!(S, select) -> Schur

   Reorders the Schur factorization \"S\" of type \"Schur\",
   overwriting \"S\" in the process. See \"ordschur()\"

"),

("Base","schurfact","schurfact(A, B) -> GeneralizedSchur

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

("Base","schur","schur(A, B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See \"schurfact()\"

"),

("Base","svdfact","svdfact(A[, thin=true]) -> SVD

   Compute the Singular Value Decomposition (SVD) of \"A\" and return
   an \"SVD\" object. \"U\", \"S\", \"V\" and \"Vt\" can be obtained
   from the factorization \"F\" with \"F[:U]\", \"F[:S]\", \"F[:V]\"
   and \"F[:Vt]\", such that \"A = U*diagm(S)*Vt\". If \"thin\" is
   \"true\", an economy mode decomposition is returned. The algorithm
   produces \"Vt\" and hence \"Vt\" is more efficient to extract than
   \"V\". The default is to produce a thin decomposition.

"),

("Base","svdfact!","svdfact!(A[, thin=true]) -> SVD

   \"svdfact!\" is the same as \"svdfact()\", but saves space by
   overwriting the input A, instead of creating a copy. If \"thin\" is
   \"true\", an economy mode decomposition is returned. The default is
   to produce a thin decomposition.

"),

("Base","svd","svd(A[, thin=true]) -> U, S, V

   Wrapper around \"svdfact\" extracting all parts the factorization
   to a tuple. Direct use of \"svdfact\" is therefore generally more
   efficient. Computes the SVD of A, returning \"U\", vector \"S\",
   and \"V\" such that \"A == U*diagm(S)*V'\". If \"thin\" is
   \"true\", an economy mode decomposition is returned. The default is
   to produce a thin decomposition.

"),

("Base","svdvals","svdvals(A)

   Returns the singular values of \"A\".

"),

("Base","svdvals!","svdvals!(A)

   Returns the singular values of \"A\", while saving space by
   overwriting the input.

"),

("Base","svdfact","svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of \"A\" and \"B\", returning a
   \"GeneralizedSVD\" Factorization object \"F\", such that \"A =
   F[:U]*F[:D1]*F[:R0]*F[:Q]'\" and \"B =
   F[:V]*F[:D2]*F[:R0]*F[:Q]'\".

"),

("Base","svd","svd(A, B) -> U, V, Q, D1, D2, R0

   Wrapper around \"svdfact\" extracting all parts the factorization
   to a tuple. Direct use of \"svdfact\" is therefore generally more
   efficient. The function returns the generalized SVD of \"A\" and
   \"B\", returning \"U\", \"V\", \"Q\", \"D1\", \"D2\", and \"R0\"
   such that \"A = U*D1*R0*Q'\" and \"B = V*D2*R0*Q'\".

"),

("Base","svdvals","svdvals(A, B)

   Return only the singular values from the generalized singular value
   decomposition of \"A\" and \"B\".

"),

("Base","triu","triu(M)

   Upper triangle of a matrix.

"),

("Base","triu!","triu!(M)

   Upper triangle of a matrix, overwriting \"M\" in the process.

"),

("Base","tril","tril(M)

   Lower triangle of a matrix.

"),

("Base","tril!","tril!(M)

   Lower triangle of a matrix, overwriting \"M\" in the process.

"),

("Base","diagind","diagind(M[, k])

   A \"Range\" giving the indices of the \"k\"-th diagonal of the
   matrix \"M\".

"),

("Base","diag","diag(M[, k])

   The \"k\"-th diagonal of a matrix, as a vector. Use \"diagm\" to
   construct a diagonal matrix.

"),

("Base","diagm","diagm(v[, k])

   Construct a diagonal matrix and place \"v\" on the \"k\"-th
   diagonal.

"),

("Base","scale","scale(A, b)

"),

("Base","scale","scale(b, A)

   Scale an array \"A\" by a scalar \"b\", returning a new array.

   If \"A\" is a matrix and \"b\" is a vector, then \"scale(A,b)\"
   scales each column \"i\" of \"A\" by \"b[i]\" (similar to
   \"A*diagm(b)\"), while \"scale(b,A)\" scales each row \"i\" of
   \"A\" by \"b[i]\" (similar to \"diagm(b)*A\"), returning a new
   array.

   Note: for large \"A\", \"scale\" can be much faster than \"A .* b\"
   or \"b .* A\", due to the use of BLAS.

"),

("Base","scale!","scale!(A, b)

"),

("Base","scale!","scale!(b, A)

   Scale an array \"A\" by a scalar \"b\", similar to \"scale()\" but
   overwriting \"A\" in-place.

   If \"A\" is a matrix and \"b\" is a vector, then \"scale!(A,b)\"
   scales each column \"i\" of \"A\" by \"b[i]\" (similar to
   \"A*diagm(b)\"), while \"scale!(b,A)\" scales each row \"i\" of
   \"A\" by \"b[i]\" (similar to \"diagm(b)*A\"), again operating in-
   place on \"A\".

"),

("Base","Tridiagonal","Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal,
   and upper diagonal, respectively.  The result is of type
   \"Tridiagonal\" and provides efficient specialized linear solvers,
   but may be converted into a regular matrix with \"full()\".

"),

("Base","Bidiagonal","Bidiagonal(dv, ev, isupper)

   Constructs an upper (\"isupper=true\") or lower (\"isupper=false\")
   bidiagonal matrix using the given diagonal (\"dv\") and off-
   diagonal (\"ev\") vectors.  The result is of type \"Bidiagonal\"
   and provides efficient specialized linear solvers, but may be
   converted into a regular matrix with \"full()\".

"),

("Base","SymTridiagonal","SymTridiagonal(d, du)

   Construct a real symmetric tridiagonal matrix from the diagonal and
   upper diagonal, respectively. The result is of type
   \"SymTridiagonal\" and provides efficient specialized eigensolvers,
   but may be converted into a regular matrix with \"full()\".

"),

("Base","Woodbury","Woodbury(A, U, C, V)

   Construct a matrix in a form suitable for applying the Woodbury
   matrix identity.

"),

("Base","rank","rank(M)

   Compute the rank of a matrix.

"),

("Base","norm","norm(A[, p])

   Compute the \"p\"-norm of a vector or the operator norm of a matrix
   \"A\", defaulting to the \"p=2\"-norm.

   For vectors, \"p\" can assume any numeric value (even though not
   all values produce a mathematically valid vector norm). In
   particular, \"norm(A, Inf)\" returns the largest value in
   \"abs(A)\", whereas \"norm(A, -Inf)\" returns the smallest.

   For matrices, valid values of \"p\" are \"1\", \"2\", or \"Inf\".
   (Note that for sparse matrices, \"p=2\" is currently not
   implemented.) Use \"vecnorm()\" to compute the Frobenius norm.

"),

("Base","vecnorm","vecnorm(A[, p])

   For any iterable container \"A\" (including arrays of any
   dimension) of numbers, compute the \"p\"-norm (defaulting to
   \"p=2\") as if \"A\" were a vector of the corresponding length.

   For example, if \"A\" is a matrix and \"p=2\", then this is
   equivalent to the Frobenius norm.

"),

("Base","cond","cond(M[, p])

   Condition number of the matrix \"M\", computed using the operator
   \"p\"-norm. Valid values for \"p\" are \"1\", \"2\" (default), or
   \"Inf\".

"),

("Base","condskeel","condskeel(M[, x, p])

      \\kappa_S(M, p) & = \\left\\Vert \\left\\vert M \\right\\vert
      \\left\\vert M^{-1} \\right\\vert  \\right\\Vert_p \\\\
      \\kappa_S(M, x, p) & = \\left\\Vert \\left\\vert M \\right\\vert
      \\left\\vert M^{-1} \\right\\vert \\left\\vert x \\right\\vert
      \\right\\Vert_p

   Skeel condition number \\kappa_S of the matrix \"M\", optionally
   with respect to the vector \"x\", as computed using the operator
   \"p\"-norm. \"p\" is \"Inf\" by default, if not provided. Valid
   values for \"p\" are \"1\", \"2\", or \"Inf\".

   This quantity is also known in the literature as the Bauer
   condition number, relative condition number, or componentwise
   relative condition number.

"),

("Base","trace","trace(M)

   Matrix trace

"),

("Base","det","det(M)

   Matrix determinant

"),

("Base","logdet","logdet(M)

   Log of matrix determinant. Equivalent to \"log(det(M))\", but may
   provide increased accuracy and/or speed.

"),

("Base","inv","inv(M)

   Matrix inverse

"),

("Base","pinv","pinv(M)

   Moore-Penrose pseudoinverse

"),

("Base","null","null(M)

   Basis for nullspace of \"M\".

"),

("Base","repmat","repmat(A, n, m)

   Construct a matrix by repeating the given matrix \"n\" times in
   dimension 1 and \"m\" times in dimension 2.

"),

("Base","repeat","repeat(A, inner = Int[], outer = Int[])

   Construct an array by repeating the entries of \"A\". The i-th
   element of \"inner\" specifies the number of times that the
   individual entries of the i-th dimension of \"A\" should be
   repeated. The i-th element of \"outer\" specifies the number of
   times that a slice along the i-th dimension of \"A\" should be
   repeated.

"),

("Base","kron","kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

"),

("Base","blkdiag","blkdiag(A...)

   Concatenate matrices block-diagonally. Currently only implemented
   for sparse matrices.

"),

("Base","linreg","linreg(x, y) -> [a; b]

   Linear Regression. Returns \"a\" and \"b\" such that \"a+b*x\" is
   the closest line to the given points \"(x,y)\". In other words,
   this function determines parameters \"[a, b]\" that minimize the
   squared error between \"y\" and \"a+b*x\".

   **Example**:

      using PyPlot;
      x = float([1:12])
      y = [5.5; 6.3; 7.6; 8.8; 10.9; 11.79; 13.48; 15.02; 17.77; 20.81; 22.0; 22.99]
      a, b = linreg(x,y) # Linear regression
      plot(x, y, \"o\") # Plot (x,y) points
      plot(x, [a+b*i for i in x]) # Plot the line determined by the linear regression

"),

("Base","linreg","linreg(x, y, w)

   Weighted least-squares linear regression.

"),

("Base","expm","expm(A)

   Matrix exponential.

"),

("Base","lyap","lyap(A, C)

   Computes the solution \"X\" to the continuous Lyapunov equation
   \"AX + XA' + C = 0\", where no eigenvalue of \"A\" has a zero real
   part and no two eigenvalues are negative complex conjugates of each
   other.

"),

("Base","sylvester","sylvester(A, B, C)

   Computes the solution \"X\" to the Sylvester equation \"AX + XB + C
   = 0\", where \"A\", \"B\" and \"C\" have compatible dimensions and
   \"A\" and \"-B\" have no eigenvalues with equal real part.

"),

("Base","issym","issym(A) -> Bool

   Test whether a matrix is symmetric.

"),

("Base","isposdef","isposdef(A) -> Bool

   Test whether a matrix is positive definite.

"),

("Base","isposdef!","isposdef!(A) -> Bool

   Test whether a matrix is positive definite, overwriting \"A\" in
   the processes.

"),

("Base","istril","istril(A) -> Bool

   Test whether a matrix is lower triangular.

"),

("Base","istriu","istriu(A) -> Bool

   Test whether a matrix is upper triangular.

"),

("Base","ishermitian","ishermitian(A) -> Bool

   Test whether a matrix is Hermitian.

"),

("Base","transpose","transpose(A)

   The transposition operator (\".'\").

"),

("Base","transpose!","transpose!(dest, src)

   Transpose array \"src\" and store the result in the preallocated
   array \"dest\", which should have a size corresponding to
   \"(size(src,2),size(src,1))\". No in-place transposition is
   supported and unexpected results will happen if *src* and *dest*
   have overlapping memory regions.

"),

("Base","ctranspose","ctranspose(A)

   The conjugate transposition operator (\"'\").

"),

("Base","ctranspose!","ctranspose!(dest, src)

   Conjugate transpose array \"src\" and store the result in the
   preallocated array \"dest\", which should have a size corresponding
   to \"(size(src,2),size(src,1))\". No in-place transposition is
   supported and unexpected results will happen if *src* and *dest*
   have overlapping memory regions.

"),

("Base","eigs","eigs(A[, B], ; nev=6, which=\"LM\", tol=0.0, maxiter=1000, sigma=nothing, ritzvec=true, v0=zeros((0, ))) -> (d[, v], nconv, niter, nmult, resid)

   \"eigs\" computes eigenvalues \"d\" of \"A\" using Lanczos or
   Arnoldi iterations for real symmetric or general nonsymmetric
   matrices respectively. If \"B\" is provided, the generalized eigen-
   problem is solved.  The following keyword arguments are supported:
      * \"nev\": Number of eigenvalues

      * \"ncv\": Number of Krylov vectors used in the computation;
        should satisfy \"nev+1 <= ncv <= n\" for real symmetric
        problems and \"nev+2 <= ncv <= n\" for other problems; default
        is \"ncv = max(20,2*nev+1)\".

      * \"which\": type of eigenvalues to compute. See the note
        below.

        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\"which\\\" | type of eigenvalues                                                                                                         |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\":LM\\\"   | eigenvalues of largest magnitude (default)                                                                                  |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\":SM\\\"   | eigenvalues of smallest magnitude                                                                                           |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\":LR\\\"   | eigenvalues of largest real part                                                                                            |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\":SR\\\"   | eigenvalues of smallest real part                                                                                           |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\":LI\\\"   | eigenvalues of largest imaginary part (nonsymmetric or complex \\\"A\\\" only)                                                  |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\":SI\\\"   | eigenvalues of smallest imaginary part (nonsymmetric or complex \\\"A\\\" only)                                                 |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \\\":BE\\\"   | compute half of the eigenvalues from each end of the spectrum, biased in favor of the high end. (real symmetric \\\"A\\\" only) |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+

      * \"tol\": tolerance (tol \\le 0.0 defaults to
        \"DLAMCH('EPS')\")

      * \"maxiter\": Maximum number of iterations (default = 300)

      * \"sigma\": Specifies the level shift used in inverse
        iteration. If \"nothing\" (default), defaults to ordinary
        (forward) iterations. Otherwise, find eigenvalues close to
        \"sigma\" using shift and invert iterations.

      * \"ritzvec\": Returns the Ritz vectors \"v\" (eigenvectors)
        if \"true\"

      * \"v0\": starting vector from which to start the iterations

   \"eigs\" returns the \"nev\" requested eigenvalues in \"d\", the
   corresponding Ritz vectors \"v\" (only if \"ritzvec=true\"), the
   number of converged eigenvalues \"nconv\", the number of iterations
   \"niter\" and the number of matrix vector multiplications
   \"nmult\", as well as the final residual vector \"resid\".

   Note: The \"sigma\" and \"which\" keywords interact: the
     description of eigenvalues searched for by \"which\" do _not_
     necessarily refer to the eigenvalues of \"A\", but rather the
     linear operator constructed by the specification of the iteration
     mode implied by \"sigma\".

     +-----------------+------------------------------------+------------------------------------+
     | \\\"sigma\\\"       | iteration mode                     | \\\"which\\\" refers to eigenvalues of |
     +-----------------+------------------------------------+------------------------------------+
     | \\\"nothing\\\"     | ordinary (forward)                 | A                                  |
     +-----------------+------------------------------------+------------------------------------+
     | real or complex | inverse with level shift \\\"sigma\\\" | (A - \\\\sigma I )^{-1}              |
     +-----------------+------------------------------------+------------------------------------+

"),

("Base","peakflops","peakflops(n; parallel=false)

   \"peakflops\" computes the peak flop rate of the computer by using
   double precision \"Base.LinAlg.BLAS.gemm!()\". By default, if no
   arguments are specified, it multiplies a matrix of size \"n x n\",
   where \"n = 2000\". If the underlying BLAS is using multiple
   threads, higher flop rates are realized. The number of BLAS threads
   can be set with \"blas_set_num_threads(n)\".

   If the keyword argument \"parallel\" is set to \"true\",
   \"peakflops\" is run in parallel on all the worker processors. The
   flop rate of the entire parallel computer is returned. When running
   in parallel, only 1 BLAS thread is used. The argument \"n\" still
   refers to the size of the problem that is solved on each processor.

"),

("Base.LinAlg.BLAS","dot","dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of \"n\" elements of array
   \"X\" with stride \"incx\" and \"n\" elements of array \"Y\" with
   stride \"incy\".

"),

("Base.LinAlg.BLAS","dotu","dotu(n, X, incx, Y, incy)

   Dot function for two complex vectors.

"),

("Base.LinAlg.BLAS","dotc","dotc(n, X, incx, U, incy)

   Dot function for two complex vectors conjugating the first vector.

"),

("Base.LinAlg.BLAS","blascopy!","blascopy!(n, X, incx, Y, incy)

   Copy \"n\" elements of array \"X\" with stride \"incx\" to array
   \"Y\" with stride \"incy\".  Returns \"Y\".

"),

("Base.LinAlg.BLAS","nrm2","nrm2(n, X, incx)

   2-norm of a vector consisting of \"n\" elements of array \"X\" with
   stride \"incx\".

"),

("Base.LinAlg.BLAS","asum","asum(n, X, incx)

   sum of the absolute values of the first \"n\" elements of array
   \"X\" with stride \"incx\".

"),

("Base.LinAlg.BLAS","axpy!","axpy!(n, a, X, incx, Y, incy)

   Overwrite \"Y\" with \"a*X + Y\".  Returns \"Y\".

"),

("Base.LinAlg.BLAS","scal!","scal!(n, a, X, incx)

   Overwrite \"X\" with \"a*X\".  Returns \"X\".

"),

("Base.LinAlg.BLAS","scal","scal(n, a, X, incx)

   Returns \"a*X\".

"),

("Base.LinAlg.BLAS","syrk!","syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix \"C\" as \"alpha*A*A.' +
   beta*C\" or \"alpha*A.'*A + beta*C\" according to whether \"trans\"
   is 'N' or 'T'.  When \"uplo\" is 'U' the upper triangle of \"C\" is
   updated ('L' for lower triangle).  Returns \"C\".

"),

("Base.LinAlg.BLAS","syrk","syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according
   to \"uplo\" ('U' or 'L'), of \"alpha*A*A.'\" or \"alpha*A.'*A\",
   according to \"trans\" ('N' or 'T').

"),

("Base.LinAlg.BLAS","herk!","herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian
   matrix \"C\" as \"alpha*A*A' + beta*C\" or \"alpha*A'*A + beta*C\"
   according to whether \"trans\" is 'N' or 'T'.  When \"uplo\" is 'U'
   the upper triangle of \"C\" is updated ('L' for lower triangle).
   Returns \"C\".

"),

("Base.LinAlg.BLAS","herk","herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle
   or the lower triangle, according to \"uplo\" ('U' or 'L'), of
   \"alpha*A*A'\" or \"alpha*A'*A\", according to \"trans\" ('N' or
   'T').

"),

("Base.LinAlg.BLAS","gbmv!","gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" or \"alpha*A'*x +
   beta*y\" according to \"trans\" ('N' or 'T').  The matrix \"A\" is
   a general band matrix of dimension \"m\" by \"size(A,2)\" with
   \"kl\" sub-diagonals and \"ku\" super-diagonals. Returns the
   updated \"y\".

"),

("Base.LinAlg.BLAS","gbmv","gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns \"alpha*A*x\" or \"alpha*A'*x\" according to \"trans\" ('N'
   or 'T'). The matrix \"A\" is a general band matrix of dimension
   \"m\" by \"size(A,2)\" with \"kl\" sub-diagonals and \"ku\" super-
   diagonals.

"),

("Base.LinAlg.BLAS","sbmv!","sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" where \"A\" is a a
   symmetric band matrix of order \"size(A,2)\" with \"k\" super-
   diagonals stored in the argument \"A\".  The storage layout for
   \"A\" is described the reference BLAS module, level-2 BLAS at
   http://www.netlib.org/lapack/explore-html/.

   Returns the updated \"y\".

"),

("Base.LinAlg.BLAS","sbmv","sbmv(uplo, k, alpha, A, x)

   Returns \"alpha*A*x\" where \"A\" is a symmetric band matrix of
   order \"size(A,2)\" with \"k\" super-diagonals stored in the
   argument \"A\".

"),

("Base.LinAlg.BLAS","sbmv","sbmv(uplo, k, A, x)

   Returns \"A*x\" where \"A\" is a symmetric band matrix of order
   \"size(A,2)\" with \"k\" super-diagonals stored in the argument
   \"A\".

"),

("Base.LinAlg.BLAS","gemm!","gemm!(tA, tB, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or the other three variants
   according to \"tA\" (transpose \"A\") and \"tB\".  Returns the
   updated \"C\".

"),

("Base.LinAlg.BLAS","gemm","gemm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","gemm","gemm(tA, tB, A, B)

   Returns \"A*B\" or the other three variants according to \"tA\"
   (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","gemv!","gemv!(tA, alpha, A, x, beta, y)

   Update the vector \"y\" as \"alpha*A*x + beta*y\" or \"alpha*A'x +
   beta*y\" according to \"tA\" (transpose \"A\"). Returns the updated
   \"y\".

"),

("Base.LinAlg.BLAS","gemv","gemv(tA, alpha, A, x)

   Returns \"alpha*A*x\" or \"alpha*A'x\" according to \"tA\"
   (transpose \"A\").

"),

("Base.LinAlg.BLAS","gemv","gemv(tA, A, x)

   Returns \"A*x\" or \"A'x\" according to \"tA\" (transpose \"A\").

"),

("Base.LinAlg.BLAS","symm!","symm!(side, ul, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or \"alpha*B*A + beta*C\"
   according to \"side\". \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.  Returns the updated \"C\".

"),

("Base.LinAlg.BLAS","symm","symm(side, ul, alpha, A, B)

   Returns \"alpha*A*B\" or \"alpha*B*A\" according to \"side\". \"A\"
   is assumed to be symmetric.  Only the \"ul\" triangle of \"A\" is
   used.

"),

("Base.LinAlg.BLAS","symm","symm(side, ul, A, B)

   Returns \"A*B\" or \"B*A\" according to \"side\".  \"A\" is assumed
   to be symmetric.  Only the \"ul\" triangle of \"A\" is used.

"),

("Base.LinAlg.BLAS","symm","symm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","symv!","symv!(ul, alpha, A, x, beta, y)

   Update the vector \"y\" as \"alpha*A*x + beta*y\". \"A\" is assumed
   to be symmetric.  Only the \"ul\" triangle of \"A\" is used.
   Returns the updated \"y\".

"),

("Base.LinAlg.BLAS","symv","symv(ul, alpha, A, x)

   Returns \"alpha*A*x\". \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.

"),

("Base.LinAlg.BLAS","symv","symv(ul, A, x)

   Returns \"A*x\".  \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.

"),

("Base.LinAlg.BLAS","trmm!","trmm!(side, ul, tA, dA, alpha, A, B)

   Update \"B\" as \"alpha*A*B\" or one of the other three variants
   determined by \"side\" (A on left or right) and \"tA\" (transpose
   A). Only the \"ul\" triangle of \"A\" is used.  \"dA\" indicates if
   \"A\" is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated \"B\".

"),

("Base.LinAlg.BLAS","trmm","trmm(side, ul, tA, dA, alpha, A, B)

   Returns \"alpha*A*B\" or one of the other three variants determined
   by \"side\" (A on left or right) and \"tA\" (transpose A). Only the
   \"ul\" triangle of \"A\" is used.  \"dA\" indicates if \"A\" is
   unit-triangular (the diagonal is assumed to be all ones).

"),

("Base.LinAlg.BLAS","trsm!","trsm!(side, ul, tA, dA, alpha, A, B)

   Overwrite \"B\" with the solution to \"A*X = alpha*B\" or one of
   the other three variants determined by \"side\" (A on left or right
   of \"X\") and \"tA\" (transpose A). Only the \"ul\" triangle of
   \"A\" is used.  \"dA\" indicates if \"A\" is unit-triangular (the
   diagonal is assumed to be all ones).  Returns the updated \"B\".

"),

("Base.LinAlg.BLAS","trsm","trsm(side, ul, tA, dA, alpha, A, B)

   Returns the solution to \"A*X = alpha*B\" or one of the other three
   variants determined by \"side\" (A on left or right of \"X\") and
   \"tA\" (transpose A). Only the \"ul\" triangle of \"A\" is used.
   \"dA\" indicates if \"A\" is unit-triangular (the diagonal is
   assumed to be all ones).

"),

("Base.LinAlg.BLAS","trmv!","trmv!(side, ul, tA, dA, alpha, A, b)

   Update \"b\" as \"alpha*A*b\" or one of the other three variants
   determined by \"side\" (A on left or right) and \"tA\" (transpose
   A). Only the \"ul\" triangle of \"A\" is used.  \"dA\" indicates if
   \"A\" is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated \"b\".

"),

("Base.LinAlg.BLAS","trmv","trmv(side, ul, tA, dA, alpha, A, b)

   Returns \"alpha*A*b\" or one of the other three variants determined
   by \"side\" (A on left or right) and \"tA\" (transpose A). Only the
   \"ul\" triangle of \"A\" is used.  \"dA\" indicates if \"A\" is
   unit-triangular (the diagonal is assumed to be all ones).

"),

("Base.LinAlg.BLAS","trsv!","trsv!(ul, tA, dA, A, b)

   Overwrite \"b\" with the solution to \"A*x = b\" or one of the
   other two variants determined by \"tA\" (transpose A) and \"ul\"
   (triangle of \"A\" used).  \"dA\" indicates if \"A\" is unit-
   triangular (the diagonal is assumed to be all ones).  Returns the
   updated \"b\".

"),

("Base.LinAlg.BLAS","trsv","trsv(ul, tA, dA, A, b)

   Returns the solution to \"A*x = b\" or one of the other two
   variants determined by \"tA\" (transpose A) and \"ul\" (triangle of
   \"A\" is used.) \"dA\" indicates if \"A\" is unit-triangular (the
   diagonal is assumed to be all ones).

"),

("Base.LinAlg.BLAS","blas_set_num_threads","blas_set_num_threads(n)

   Set the number of threads the BLAS library should use.

"),

("Base.Pkg","dir","dir() -> AbstractString

   Returns the absolute path of the package directory. This defaults
   to \"joinpath(homedir(),\".julia\")\" on all platforms (i.e.
   \"~/.julia\" in UNIX shell syntax). If the \"JULIA_PKGDIR\"
   environment variable is set, that path is used instead. If
   \"JULIA_PKGDIR\" is a relative path, it is interpreted relative to
   whatever the current working directory is.

"),

("Base.Pkg","dir","dir(names...) -> AbstractString

   Equivalent to \"normpath(Pkg.dir(),names...)\" – i.e. it appends
   path components to the package directory and normalizes the
   resulting path. In particular, \"Pkg.dir(pkg)\" returns the path to
   the package \"pkg\".

"),

("Base.Pkg","init","init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)

   Initialize \"Pkg.dir()\" as a package directory. This will be done
   automatically when the \"JULIA_PKGDIR\" is not set and
   \"Pkg.dir()\" uses its default value. As part of this process,
   clones a local METADATA git repository from the site and branch
   specified by its arguments, which are typically not provided.
   Explicit (non-default) arguments can be used to support a custom
   METADATA setup.

"),

("Base.Pkg","resolve","resolve()

   Determines an optimal, consistent set of package versions to
   install or upgrade to. The optimal set of package versions is based
   on the contents of \"Pkg.dir(\"REQUIRE\")\" and the state of
   installed packages in \"Pkg.dir()\", Packages that are no longer
   required are moved into \"Pkg.dir(\".trash\")\".

"),

("Base.Pkg","edit","edit()

   Opens \"Pkg.dir(\"REQUIRE\")\" in the editor specified by the
   \"VISUAL\" or \"EDITOR\" environment variables; when the editor
   command returns, it runs \"Pkg.resolve()\" to determine and install
   a new optimal set of installed package versions.

"),

("Base.Pkg","add","add(pkg, vers...)

   Add a requirement entry for \"pkg\" to \"Pkg.dir(\"REQUIRE\")\" and
   call \"Pkg.resolve()\". If \"vers\" are given, they must be
   \"VersionNumber\" objects and they specify acceptable version
   intervals for \"pkg\".

"),

("Base.Pkg","rm","rm(pkg)

   Remove all requirement entries for \"pkg\" from
   \"Pkg.dir(\"REQUIRE\")\" and call \"Pkg.resolve()\".

"),

("Base.Pkg","clone","clone(url[, pkg])

   Clone a package directly from the git URL \"url\". The package does
   not need to be a registered in \"Pkg.dir(\"METADATA\")\". The
   package repo is cloned by the name \"pkg\" if provided; if not
   provided, \"pkg\" is determined automatically from \"url\".

"),

("Base.Pkg","clone","clone(pkg)

   If \"pkg\" has a URL registered in \"Pkg.dir(\"METADATA\")\", clone
   it from that URL on the default branch. The package does not need
   to have any registered versions.

"),

("Base.Pkg","available","available() -> Vector{ASCIIString}

   Returns the names of available packages.

"),

("Base.Pkg","available","available(pkg) -> Vector{VersionNumber}

   Returns the version numbers available for package \"pkg\".

"),

("Base.Pkg","installed","installed() -> Dict{ASCIIString,VersionNumber}

   Returns a dictionary mapping installed package names to the
   installed version number of each package.

"),

("Base.Pkg","installed","installed(pkg) -> Nothing | VersionNumber

   If \"pkg\" is installed, return the installed version number,
   otherwise return \"nothing\".

"),

("Base.Pkg","status","status()

   Prints out a summary of what packages are installed and what
   version and state they're in.

"),

("Base.Pkg","update","update()

   Update package the metadata repo – kept in
   \"Pkg.dir(\"METADATA\")\" – then update any fixed packages that can
   safely be pulled from their origin; then call \"Pkg.resolve()\" to
   determine a new optimal set of packages versions.

"),

("Base.Pkg","checkout","checkout(pkg[, branch=\"master\"])

   Checkout the \"Pkg.dir(pkg)\" repo to the branch \"branch\".
   Defaults to checking out the \"master\" branch. To go back to using
   the newest compatible released version, use \"Pkg.free(pkg)\"

"),

("Base.Pkg","pin","pin(pkg)

   Pin \"pkg\" at the current version. To go back to using the newest
   compatible released version, use \"Pkg.free(pkg)\"

"),

("Base.Pkg","pin","pin(pkg, version)

   Pin \"pkg\" at registered version \"version\".

"),

("Base.Pkg","free","free(pkg)

   Free the package \"pkg\" to be managed by the package manager
   again. It calls \"Pkg.resolve()\" to determine optimal package
   versions after. This is an inverse for both \"Pkg.checkout\" and
   \"Pkg.pin\".

"),

("Base.Pkg","build","build()

   Run the build scripts for all installed packages in depth-first
   recursive order.

"),

("Base.Pkg","build","build(pkgs...)

   Run the build script in \"deps/build.jl\" for each package in
   \"pkgs\" and all of their dependencies in depth-first recursive
   order. This is called automatically by \"Pkg.resolve()\" on all
   installed or updated packages.

"),

("Base.Pkg","generate","generate(pkg, license)

   Generate a new package named \"pkg\" with one of these license
   keys: \"\"MIT\"\" or \"\"BSD\"\". If you want to make a package
   with a different license, you can edit it afterwards. Generate
   creates a git repo at \"Pkg.dir(pkg)\" for the package and inside
   it \"LICENSE.md\", \"README.md\", the julia entrypoint
   \"\$pkg/src/\$pkg.jl\", and a travis test file, \".travis.yml\".

"),

("Base.Pkg","register","register(pkg[, url])

   Register \"pkg\" at the git URL \"url\", defaulting to the
   configured origin URL of the git repo \"Pkg.dir(pkg)\".

"),

("Base.Pkg","tag","tag(pkg[, ver[, commit]])

   Tag \"commit\" as version \"ver\" of package \"pkg\" and create a
   version entry in \"METADATA\". If not provided, \"commit\" defaults
   to the current commit of the \"pkg\" repo. If \"ver\" is one of the
   symbols \":patch\", \":minor\", \":major\" the next patch, minor or
   major version is used. If \"ver\" is not provided, it defaults to
   \":patch\".

"),

("Base.Pkg","publish","publish()

   For each new package version tagged in \"METADATA\" not already
   published, make sure that the tagged package commits have been
   pushed to the repo at the registered URL for the package and if
   they all have, open a pull request to \"METADATA\".

"),

("Base.Pkg","test","test()

   Run the tests for all installed packages ensuring that each
   package's test dependencies are installed for the duration of the
   test. A package is tested by running its \"test/runtests.jl\" file
   and test dependencies are specified in \"test/REQUIRE\".

"),

("Base.Pkg","test","test(pkgs...)

   Run the tests for each package in \"pkgs\" ensuring that each
   package's test dependencies are installed for the duration of the
   test. A package is tested by running its \"test/runtests.jl\" file
   and test dependencies are specified in \"test/REQUIRE\".

"),

("Base","@profile","@profile()

   \"@profile <expression>\" runs your expression while taking
   periodic backtraces.  These are appended to an internal buffer of
   backtraces.

"),

("Base.Profile","clear","clear()

   Clear any existing backtraces from the internal buffer.

"),

("Base.Profile","print","print([io::IO = STDOUT], [data::Vector]; format = :tree, C = false, combine = true, cols = tty_cols())

   Prints profiling results to \"io\" (by default, \"STDOUT\"). If you
   do not supply a \"data\" vector, the internal buffer of accumulated
   backtraces will be used.  \"format\" can be \":tree\" or \":flat\".
   If \"C==true\", backtraces from C and Fortran code are shown.
   \"combine==true\" merges instruction pointers that correspond to
   the same line of code.  \"cols\" controls the width of the display.

"),

("Base.Profile","print","print([io::IO = STDOUT], data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   Prints profiling results to \"io\". This variant is used to examine
   results exported by a previous call to \"Profile.retrieve()\".
   Supply the vector \"data\" of backtraces and a dictionary
   \"lidict\" of line information.

"),

("Base.Profile","init","init(; n::Integer, delay::Float64)

   Configure the \"delay\" between backtraces (measured in seconds),
   and the number \"n\" of instruction pointers that may be stored.
   Each instruction pointer corresponds to a single line of code;
   backtraces generally consist of a long list of instruction
   pointers. Default settings can be obtained by calling this function
   with no arguments, and each can be set independently using keywords
   or in the order \"(n, delay)\".

"),

("Base.Profile","fetch","fetch() -> data

   Returns a reference to the internal buffer of backtraces. Note that
   subsequent operations, like \"Profile.clear()\", can affect
   \"data\" unless you first make a copy. Note that the values in
   \"data\" have meaning only on this machine in the current session,
   because it depends on the exact memory addresses used in JIT-
   compiling. This function is primarily for internal use;
   \"Profile.retrieve()\" may be a better choice for most users.

"),

("Base.Profile","retrieve","retrieve() -> data, lidict

   \"Exports\" profiling results in a portable format, returning the
   set of all backtraces (\"data\") and a dictionary that maps the
   (session-specific) instruction pointers in \"data\" to \"LineInfo\"
   values that store the file name, function name, and line number.
   This function allows you to save profiling results for future
   analysis.

"),

("Base.Profile","clear_malloc_data","clear_malloc_data()

   Clears any stored memory allocation data when running julia with \"
   --track-allocation\".  Execute the command(s) you want to test (to
   force JIT-compilation), then call \"clear_malloc_data()\". Then
   execute your command(s) again, quit julia, and examine the
   resulting \"*.mem\" files.

"),


("Base","sort!","sort!(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

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

("Base","sort","sort(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of \"sort!\" that returns a sorted copy of \"v\" leaving
   \"v\" itself unmodified.

"),

("Base","sort","sort(A, dim, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort a multidimensional array \"A\" along the given dimension.

"),

("Base","sortperm","sortperm(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

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

   See also \"sortperm!()\"

"),

("Base","sortperm!","sortperm!(ix, v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false,] [initialized=false])

   Like \"sortperm\", but accepts a preallocated index vector \"ix\".
   If \"initialized\" is \"false\" (the default), ix is initialized to
   contain the values \"1:length(v)\".

   See also \"sortperm()\"

"),

("Base","sortrows","sortrows(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the rows of matrix \"A\" lexicographically.

"),

("Base","sortcols","sortcols(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the columns of matrix \"A\" lexicographically.

"),

("Base","issorted","issorted(v, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Test whether a vector is in sorted order. The \"by\", \"lt\" and
   \"rev\" keywords modify what order is considered to be sorted just
   as they do for \"sort\".

"),

("Base","searchsorted","searchsorted(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the range of indices of \"a\" which compare as equal to
   \"x\" according to the order specified by the \"by\", \"lt\" and
   \"rev\" keywords, assuming that \"a\" is already sorted in that
   order. Returns an empty range located at the insertion point if
   \"a\" does not contain values equal to \"x\".

"),

("Base","searchsortedfirst","searchsortedfirst(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the first value in \"a\" greater than or equal
   to \"x\", according to the specified order. Returns \"length(a)+1\"
   if \"x\" is greater than all values in \"a\".

"),

("Base","searchsortedlast","searchsortedlast(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the last value in \"a\" less than or equal to
   \"x\", according to the specified order. Returns \"0\" if \"x\" is
   less than all values in \"a\".

"),

("Base","select!","select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Partially sort the vector \"v\" in place, according to the order
   specified by \"by\", \"lt\" and \"rev\" so that the value at index
   \"k\" (or range of adjacent values if \"k\" is a range) occurs at
   the position where it would appear if the array were fully sorted
   via a non-stable algorithm. If \"k\" is a single index, that value
   is returned; if \"k\" is a range, an array of values at those
   indices is returned. Note that \"select!\" does not fully sort the
   input array.

"),

("Base","select","select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of \"select!\" which copies \"v\" before partially sorting
   it, thereby returning the same thing as \"select!\" but leaving
   \"v\" unmodified.

"),

("Base","sparse","sparse(I, J, V[, m, n, combine])

   Create a sparse matrix \"S\" of dimensions \"m x n\" such that
   \"S[I[k], J[k]] = V[k]\". The \"combine\" function is used to
   combine duplicates. If \"m\" and \"n\" are not specified, they are
   set to \"max(I)\" and \"max(J)\" respectively. If the \"combine\"
   function is not supplied, duplicates are added by default.

"),

("Base","sparsevec","sparsevec(I, V[, m, combine])

   Create a sparse matrix \"S\" of size \"m x 1\" such that \"S[I[k]]
   = V[k]\". Duplicates are combined using the \"combine\" function,
   which defaults to \"+\" if it is not provided. In julia, sparse
   vectors are really just sparse matrices with one column. Given
   Julia's Compressed Sparse Columns (CSC) storage format, a sparse
   column matrix with one column is sparse, whereas a sparse row
   matrix with one row ends up being dense.

"),

("Base","sparsevec","sparsevec(D::Dict[, m])

   Create a sparse matrix of size \"m x 1\" where the row values are
   keys from the dictionary, and the nonzero values are the values
   from the dictionary.

"),

("Base","issparse","issparse(S)

   Returns \"true\" if \"S\" is sparse, and \"false\" otherwise.

"),

("Base","sparse","sparse(A)

   Convert a dense matrix \"A\" into a sparse matrix.

"),

("Base","sparsevec","sparsevec(A)

   Convert a dense vector \"A\" into a sparse matrix of size \"m x
   1\". In julia, sparse vectors are really just sparse matrices with
   one column.

"),

("Base","full","full(S)

   Convert a sparse matrix \"S\" into a dense matrix.

"),

("Base","nnz","nnz(A)

   Returns the number of stored (filled) elements in a sparse matrix.

"),

("Base","spzeros","spzeros(m, n)

   Create an empty sparse matrix of size \"m x n\".

"),

("Base","spones","spones(S)

   Create a sparse matrix with the same structure as that of \"S\",
   but with every nonzero element having the value \"1.0\".

"),

("Base","speye","speye(type, m[, n])

   Create a sparse identity matrix of specified type of size \"m x
   m\". In case \"n\" is supplied, create a sparse identity matrix of
   size \"m x n\".

"),

("Base","spdiagm","spdiagm(B, d[, m, n])

   Construct a sparse diagonal matrix. \"B\" is a tuple of vectors
   containing the diagonals and \"d\" is a tuple containing the
   positions of the diagonals. In the case the input contains only one
   diagonaly, \"B\" can be a vector (instead of a tuple) and \"d\" can
   be the diagonal position (instead of a tuple), defaulting to 0
   (diagonal). Optionally, \"m\" and \"n\" specify the size of the
   resulting sparse matrix.

"),

("Base","sprand","sprand(m, n, p[, rng])

   Create a random \"m\" by \"n\" sparse matrix, in which the
   probability of any element being nonzero is independently given by
   \"p\" (and hence the mean density of nonzeros is also exactly
   \"p\"). Nonzero values are sampled from the distribution specified
   by \"rng\". The uniform distribution is used in case \"rng\" is not
   specified.

"),

("Base","sprandn","sprandn(m, n, p)

   Create a random \"m\" by \"n\" sparse matrix with the specified
   (independent) probability \"p\" of any entry being nonzero, where
   nonzero values are sampled from the normal distribution.

"),

("Base","sprandbool","sprandbool(m, n, p)

   Create a random \"m\" by \"n\" sparse boolean matrix with the
   specified (independent) probability \"p\" of any entry being
   \"true\".

"),

("Base","etree","etree(A[, post])

   Compute the elimination tree of a symmetric sparse matrix \"A\"
   from \"triu(A)\" and, optionally, its post-ordering permutation.

"),

("Base","symperm","symperm(A, p)

   Return the symmetric permutation of A, which is \"A[p,p]\". A
   should be symmetric and sparse, where only the upper triangular
   part of the matrix is stored. This algorithm ignores the lower
   triangular part of the matrix. Only the upper triangular part of
   the result is returned as well.

"),

("Base","nonzeros","nonzeros(A)

   Return a vector of the structural nonzero values in sparse matrix
   \"A\". This includes zeros that are explicitly stored in the sparse
   matrix. The returned vector points directly to the internal nonzero
   storage of \"A\", and any modifications to the returned vector will
   mutate \"A\" as well. See \"rowvals(A)\" and \"nzrange(A, col)\".

"),

("Base","rowvals","rowvals(A)

   Return a vector of the row indices of \"A\", and any modifications
   to the returned vector will mutate \"A\" as well. Given the
   internal storage format of sparse matrices, providing access to how
   the row indices are stored internally can be useful in conjuction
   with iterating over structural nonzero values. See \"nonzeros(A)\"
   and \"nzrange(A, col)\".

"),

("Base","nzrange","nzrange(A, col)

   Return the range of indices to the structural nonzero values of a
   sparse matrix column. In conjunction with \"nonzeros(A)\" and
   \"rowvals(A)\", this allows for convenient iterating over a sparse
   matrix

      A = sparse(I,J,V)
      rows = rowvals(A)
      vals = nonzeros(A)
      m, n = size(A)
      for i = 1:n
         for j in nzrange(A, i)
            row = rows[j]
            val = vals[j]
            # perform sparse wizardry...
         end
      end

"),

("Base.Test","@test","@test(ex)

   Test the expression \"ex\" and calls the current handler to handle
   the result.

"),

("Base.Test","@test_throws","@test_throws(extype, ex)

   Test that the expression \"ex\" throws an exception of type
   \"extype\" and calls the current handler to handle the result.

"),

("Base.Test","@test_approx_eq","@test_approx_eq(a, b)

   Test two floating point numbers \"a\" and \"b\" for equality taking
   in account small numerical errors.

"),

("Base.Test","@test_approx_eq_eps","@test_approx_eq_eps(a, b, tol)

   Test two floating point numbers \"a\" and \"b\" for equality taking
   in account a margin of tolerance given by \"tol\".

"),

("Base.Test","with_handler","with_handler(f, handler)

   Run the function \"f\" using the \"handler\" as the handler.

"),

("Base","runtests","runtests([tests=[\"all\"][, numcores=iceil(CPU_CORES/2)]])

   Run the Julia unit tests listed in \"tests\", which can be either a
   string or an array of strings, using \"numcores\" processors.

"),


]
