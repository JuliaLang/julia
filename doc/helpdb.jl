# automatically generated -- do not edit

{

("ArgParse","ArgParse","parse_args","parse_args([args], settings)

   This is the central function of the \"ArgParse\" module. It takes a
   \"Vector\" of arguments and an \"ArgParseSettings\" objects (see
   *this section*), and returns a \"Dict{String,Any}\". If \"args\" is
   not provided, the global variable \"ARGS\" will be used.

   The returned \"Dict\" keys are defined (possibly implicitly) in
   \"settings\", and their associated values are parsed from \"args\".
   Special keys are used for more advanced purposes; at the moment,
   one such key exists: \"%COMMAND%\" (see *this section*).

   Arguments are parsed in sequence and matched against the argument
   table in \"settings\" to determine whether they are long options,
   short options, option arguments or positional arguments:

   * long options begin with a doule dash \"\"--\"\"; if a \"'='\"
     character is found, the remainder is the option argument;
     therefore, \"[\"--opt=arg\"]\" and \"[\"--opt\", \"arg\"]\" are
     equivalent if \"--opt\" takes at least one argument. Long options
     can be abbreviated (e.g. \"--opt\" instead of \"--option\") as
     long as there is no ambiguity.

   * short options begin with a single dash \"\"-\"\" and their name
     consists of a single character; they can be grouped togheter
     (e.g. \"[\"-x\", \"-y\"]\" can become \"[\"-xy\"]\"), but in that
     case only the last option in the group can take an argument
     (which can also be grouped, e.g. \"[\"-a\", \"-f\",
     \"file.txt\"]\" can be passed as \"[\"-affile.txt\"]\" if \"-a\"
     does not take an argument and \"-f\" does). The \"'='\" character
     can be used to separate option names from option arguments as
     well (e.g. \"-af=file.txt\").

   * positional arguments are anything else; they can appear anywhere.

   The special string \"\"--\"\" can be used to signal the end of all
   options; after that, everything is considered as a positional
   argument (e.g. if \"args = [\"--opt1\", \"--\", \"--opt2\"]\", the
   parser will recognize \"--opt1\" as a long option without argument,
   and \"--opt2\" as a positional argument).

   The special string \"\"-\"\" is always parsed as a positional
   argument.

   The parsing can stop early if a \":show_help\" or \":show_version\"
   action is triggered, or if a parsing error is found.

   Some ambiguities can arise in parsing, see *this section* for a
   detailed description of how they're solved.

"),

("ArgParse","ArgParse","@add_arg_table","@add_arg_table(settings, table...)

   This macro adds a table of arguments and options to the given
   \"settings\". It can be invoked multiple times. The arguments
   groups are determined automatically, or the current default group
   is used if specified (see *this section* for more details).

   The \"table\" is a list in which each element can be either
   \"String\", or a tuple or a vector of \"String\", or an assigmment
   expression, or a block:

   * a \"String\", a tuple or a vector introduces a new positional
     argument or option. Tuples and vectors are only allowed for
     options and provide alternative names (e.g. \"[\"--opt\",
     \"-o\"]\")

   * assignment expressions (i.e. expressions using \"=\", \":=\" or
     \"=>\") describe the previous argument behavior (e.g. \"help =
     \"an option\"\" or \"required => false\").  See *this section*
     for a complete description

   * blocks (\"begin...end\" or lists of expressions in parentheses
     separated by semicolons) are useful to group entries and span
     multiple lines.

   These rules allow for a variety usage styles, which are discussed
   in *this section*. In the rest of this document, we will mostly use
   this style:

      @add_arg_table settings begin
          \"--opt1\", \"-o\"
              help = \"an option with an argument\"
          \"--opt2\"
          \"arg1\"
              help = \"a positional argument\"
              required = true
      end

   In the above example, the \"table\" is put in a single
   \"begin...end\" block and the line \"\"-opt1\", \"-o\"\" is parsed
   as a tuple; indentation is used to help readability.

"),

("ArgParse","ArgParse","add_arg_table","add_arg_table(settings, [arg_name [,arg_options]]...)

   This function is almost equivalent to the macro version. Its syntax
   is stricter (tuples and blocks are not allowed and argument options
   are explicitly specified as \"Options\" objects) but the
   \"arg_name\" entries need not be explicit, they can be anything
   which evaluates to a \"String\" or a \"Vector{String}\".

   Example:

      add_arg_table(settings,
          [\"--opt1\", \"-o\"],
          @options begin
              help = \"an option with an argument\"
          end,
          \"--opt2\",
          \"arg1\",
          @options begin
              help = \"a positional argument\"
              required = true
          end)

   Note that the \"OptionsMod\" module must be imported in order to
   use this function.

"),

("ArgParse","ArgParse","add_arg_group","add_arg_group(settings, description[, name[, set_as_default]])

   This function adds an argument group to the argument table in
   \"settings\". The \"description\" is a \"String\" used in the help
   screen as a title for that group. The \"name\" is a unique name
   which can be provided to refer to that group at a later time.

   After invoking this function, all subsequent invocations of the
   \"@add_arg_table\" macro and \"add_arg_table\" function will use
   the new group as the default, unless \"set_as_default\" is set to
   \"false\" (the default is \"true\", and the option can only be set
   if providing a \"name\"). Therefore, the most obvious usage pattern
   is: for each group, add it and populate the argument table of that
   group. Example:

      julia> settings = ArgParseSettings();

      julia> add_arg_group(settings, \"custom group\");

      julia> @add_arg_table settings begin
                \"--opt\"
                \"arg\"
             end;

      julia> parse_args([\"--help\"], settings)
      usage: <command> [--opt OPT] [-h] [arg]

      optional arguments:
        -h, --help  show this help message and exit

      custom group:
        --opt OPT
        arg

   As seen from the example, new groups are always added at the end of
   existing ones.

   The \"name\" can also be passed as a \"Symbol\". Forbidden names
   are the standard groups names (\"\"command\"\", \"\"positional\"\"
   and \"\"optional\"\") and those beginning with a hash character
   \"'#'\".

"),

("ArgParse","ArgParse","set_default_arg_group","set_default_arg_group(settings[, name])

   Set the default group for subsequent invocations of the
   \"@add_arg_table\" macro and \"add_arg_table\" function. \"name\"
   is a \"String\", and must be one of the standard group names
   (\"\"command\"\", \"\"positional\"\" or \"\"optional\"\") or one of
   the user-defined names given in \"add_arg_group\" (groups with no
   assigned name cannot be used with this function).

   If \"name\" is not provided or is the empty string \"\"\"\", then
   the default behavior is reset (i.e. arguments will be automatically
   assigned to the standard groups). The \"name\" can also be passed
   as a \"Symbol\".

"),

("ArgParse","ArgParse","import_settings","import_settings(settings, other_settings[, args_only])

   Imports \"other_settings\" into \"settings\", where both are
   \"ArgParseSettings\" objects. If \"args_only\" is \"true\" (this is
   the default), only the argument table will be imported; otherwise,
   the default argument group will also be imported, and all general
   settings except \"prog\", \"description\", \"epilog\" and
   \"usage\".

   Sub-settings associated with commands will also be imported
   recursively; the \"args_only\" setting applies to those as well. If
   there are common commands, their sub-settings will be merged.

   While importing, conflicts may arise: if
   \"settings.error_on_conflict\" is \"true\", this will result in an
   error, otherwise conflicts will be resolved in favor of
   \"other_settings\" (see *this section* for a detailed discussion of
   how conflicts are handled).

   Argument groups will also be imported; if two groups in
   \"settings\" and \"other_settings\" match, they are merged (groups
   match either by name, or, if unnamed, by their description).

   Note that the import will have effect immediately: any subsequent
   modification of \"other_settings\" will not have any effect on
   \"settings\".

   This function can be used at any time.

"),

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

("Associative Collections","Base","empty!","empty!(collection)

   Delete all keys from a collection.

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

("Strings","Base","collect","collect(string)

   Return an array of the characters in \"string\".

"),

("Strings","Base","*","*(s, t)

   Concatenate strings.

   **Example**: \"\"Hello \" * \"world\" == \"Hello world\"\"

"),

("Strings","Base","^","^(s, n)

   Repeat string \"s\" \"n\" times.

   **Example**: \"\"Julia \"^3 == \"Julia Julia Julia \"\"

"),

("Strings","Base","string","string(char...)

   Create a string with the given characters.

"),

("Strings","Base","string","string(x)

   Create a string from any value using the \"print\" function.

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

   Returns true if the string is valid ASCII, false otherwise.

"),

("Strings","Base","is_valid_utf8","is_valid_utf8(s) -> Bool

   Returns true if the string is valid UTF-8, false otherwise.

"),

("Strings","Base","check_ascii","check_ascii(s)

   Calls \"is_valid_ascii()\" on string. Throws error if it is not
   valid.

"),

("Strings","Base","check_utf8","check_utf8(s)

   Calls \"is_valid_utf8()\" on string. Throws error if it is not
   valid.

"),

("Strings","Base","byte_string_classify","byte_string_classify(s)

   Returns 0 if the string is neither valid ASCII nor UTF-8, 1 if it
   is valid ASCII, and 2 if it is valid UTF-8.

"),

("Strings","Base","search","search(string, char[, i])

   Return the index of \"char\" in \"string\", giving 0 if not found.
   The second argument may also be a vector or a set of characters.
   The third argument optionally specifies a starting index.

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
   characters, a string, or a regular expression (but regular
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

"),

("Strings","Base","replace","replace(string, pat, f[, n])

   Search for the given pattern \"pat\", and replace each occurance
   with \"f(pat)\". If \"n\" is provided, replace at most \"n\"
   occurances.  As with search, the second argument may be a single
   character, a vector or a set of characters, a string, or a regular
   expression.

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

("I/O","Base","open","open(file_name) -> IOStream

   Open a file in read mode.

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

("I/O","Base","fdio","fdio(fd::Integer[, own::Bool]) -> IOStream
fdio(name::String, fd::Integer, [own::Bool]]) -> IOStream

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

("Text I/O","Base","each_line","each_line(stream)

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

("Mathematical Functions","Base","+",".+(x, y)

   Element-wise binary addition operator.

"),

("Mathematical Functions","Base","-",".-(x, y)

   Element-wise binary subtraction operator.

"),

("Mathematical Functions","Base","*",".*(x, y)

   Element-wise binary multiplication operator.

"),

("Mathematical Functions","Base","/","./(x, y)

   Element-wise binary left division operator.

"),

("Mathematical Functions","Base","\\",".\\(x, y)

   Element-wise binary right division operator.

"),

("Mathematical Functions","Base","^",".^(x, y)

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

("Mathematical Functions","Base","exponent","exponent(x)

   Return the exponent of x, represented as a floating-point number

"),

("Mathematical Functions","Base","ilogb","ilogb(x)

   Return the exponent of x, represented as a signed integer value

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

("Mathematical Functions","Base","airyai","airy(x)
airyai(x)

   Airy function \\operatorname{Ai}(x).

"),

("Mathematical Functions","Base","airyaiprime","airyprime(x)
airyaiprime(x)

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

("Data Formats","Base","base","base(b, n[, pad])

   Convert an integer to a string in the given base, optionally
   specifying a number of digits to pad to.

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

("Data Formats","Base","safe_char","safe_char(x)

   Convert to \"Char\", checking for invalid code points

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

("Numbers","Base","exponent","exponent(f)

   Get the exponent of a floating-point number

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

   Generate a \"Float64\" random number in (0,1)

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

("Random Numbers","Base","rand","rand(dims...)

   Generate a random \"Float64\" array of the size specified by dims

"),

("Random Numbers","Base","rand","rand(Int32|Uint32|Int64|Uint64|Int128|Uint128[, dims...])

   Generate a random integer of the given type. Optionally, generate
   an array of random integers of the given type by specifying dims.

"),

("Random Numbers","Base","rand","rand(r[, dims...])

   Generate a random integer from \"1\":\"n\" inclusive. Optionally,
   generate a random integer array.

"),

("Random Numbers","Base","randbool","randbool([dims...])

   Generate a random boolean value. Optionally, generate an array of
   random boolean values.

"),

("Random Numbers","Base","randbool!","randbool!(A)

   Fill an array with random boolean values. A may be an \"Array\" or
   a \"BitArray\".

"),

("Random Numbers","Base","randn","randn([dims...])

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

   Counts the number of nonzero values in A

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

("Arrays","Base","getindex","getindex(type)

   Construct an empty 1-d array of the specified type. This is usually
   called with the syntax \"Type[]\". Element values can be specified
   using \"Type[a,b,c,...]\".

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

("Arrays","Base","copy","copy(A)

   Create a copy of \"A\"

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

("Arrays","Base","rand","rand(dims)

   Create a random array with Float64 random values in (0,1)

"),

("Arrays","Base","randf","randf(dims)

   Create a random array with Float32 random values in (0,1)

"),

("Arrays","Base","randn","randn(dims)

   Create a random array with Float64 normally-distributed random
   values with a mean of 0 and standard deviation of 1

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

   Returns a subset of \"A\" as specified by \"ind\", which may be an
   \"Int\", a \"Range\", or a \"Vector\".

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

   Store an input array \"X\" within some subset of \"A\" as specified
   by \"ind\".

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

("Arrays","Base","hvcat","hvcat()

   Horizontal and vertical concatenation in one call

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
   which defaults to *+* if it is not provided. In julia, sparse
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

("Sparse Matrices","Base","nnz","nnz(S)

   Return the number of nonzeros in \"S\".

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

("Sparse Matrices","Base","speye","speye(type, m[, n])

   Create a sparse identity matrix of specified type of size \"m x
   m\". In case \"n\" is supplied, create a sparse identity matrix of
   size \"m x n\".

"),

("Sparse Matrices","Base","spones","spones(S)

   Create a sparse matrix with the same structure as that of \"S\",
   but with every nonzero element having the value \"1.0\".

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

("Linear Algebra","Base","*","*(A, B)

   Matrix multiplication

"),

("Linear Algebra","Base","\\","\\(A, B)

   Matrix division using a polyalgorithm. For input matrices \"A\" and
   \"B\", the result \"X\" is such that \"A*X == B\". For rectangular
   \"A\", QR factorization is used. For triangular \"A\", a triangular
   solve is performed. For square \"A\", Cholesky factorization is
   tried if the input is symmetric with a heavy diagonal. LU
   factorization is used in case Cholesky factorization fails or for
   general square inputs. If \"size(A,1) > size(A,2)\", the result is
   a least squares solution of \"A*X+eps=B\" using the singular value
   decomposition. \"A\" does not need to have full rank.

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

("Linear Algebra","Base","factors","factors(F)

   Return the factors of a factorization \"F\". For example, in the
   case of an LU decomposition, factors(LU) -> L, U, P

"),

("Linear Algebra","Base","lu","lu(A) -> L, U, P

   Compute the LU factorization of \"A\", such that \"A[P,:] = L*U\".

"),

("Linear Algebra","Base","lufact","lufact(A) -> LUDense

   Compute the LU factorization of \"A\" and return a \"LUDense\"
   object. \"factors(lufact(A))\" returns the triangular matrices
   containing the factorization. The following functions are available
   for \"LUDense\" objects: \"size\", \"factors\", \"\\\", \"inv\",
   \"det\".

"),

("Linear Algebra","Base","lufact!","lufact!(A) -> LUDense

   \"lufact!\" is the same as \"lufact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","chol","chol(A[, LU]) -> F

   Compute Cholesky factorization of a symmetric positive-definite
   matrix \"A\" and return the matrix \"F\". If \"LU\" is \"L\"
   (Lower), \"A = L*L'\". If \"LU\" is \"U\" (Upper), \"A = R'*R\".

"),

("Linear Algebra","Base","cholfact","cholfact(A[, LU]) -> CholeskyDense

   Compute the Cholesky factorization of a symmetric positive-definite
   matrix \"A\" and return a \"CholeskyDense\" object. \"LU\" may be
   'L' for using the lower part or 'U' for the upper part. The default
   is to use 'U'. \"factors(cholfact(A))\" returns the triangular
   matrix containing the factorization. The following functions are
   available for \"CholeskyDense\" objects: \"size\", \"factors\",
   \"\\\", \"inv\", \"det\". A \"LAPACK.PosDefException\" error is
   thrown in case the matrix is not positive definite.

"),

("Linear Algebra","Base","cholpfact","cholpfact(A[, LU]) -> CholeskyPivotedDense

   Compute the pivoted Cholesky factorization of a symmetric positive
   semi-definite matrix \"A\" and return a \"CholeskyDensePivoted\"
   object. \"LU\" may be 'L' for using the lower part or 'U' for the
   upper part. The default is to use 'U'. \"factors(cholpfact(A))\"
   returns the triangular matrix containing the factorization. The
   following functions are available for \"CholeskyDensePivoted\"
   objects: \"size\", \"factors\", \"\\\", \"inv\", \"det\". A
   \"LAPACK.RankDeficientException\" error is thrown in case the
   matrix is rank deficient.

"),

("Linear Algebra","Base","cholpfact!","cholpfact!(A[, LU]) -> CholeskyPivotedDense

   \"cholpfact!\" is the same as \"cholpfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","qr","qr(A) -> Q, R

   Compute the QR factorization of \"A\" such that \"A = Q*R\". Also
   see \"qrd\".

"),

("Linear Algebra","Base","qrfact","qrfact(A)

   Compute the QR factorization of \"A\" and return a \"QRDense\"
   object. \"factors(qrfact(A))\" returns \"Q\" and \"R\". The
   following functions are available for \"QRDense\" objects:
   \"size\", \"factors\", \"qmulQR\", \"qTmulQR\", \"\\\".

"),

("Linear Algebra","Base","qrfact!","qrfact!(A)

   \"qrfact!\" is the same as \"qrfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","qrp","qrp(A) -> Q, R, P

   Compute the QR factorization of \"A\" with pivoting, such that
   \"A*I[:,P] = Q*R\", where \"I\" is the identity matrix. Also see
   \"qrpfact\".

"),

("Linear Algebra","Base","qrpfact","qrpfact(A) -> QRPivotedDense

   Compute the QR factorization of \"A\" with pivoting and return a
   \"QRDensePivoted\" object. \"factors(qrpfact(A))\" returns \"Q\"
   and \"R\". The following functions are available for
   \"QRDensePivoted\" objects: \"size\", \"factors\", \"qmulQR\",
   \"qTmulQR\", \"\\\".

"),

("Linear Algebra","Base","qrpfact!","qrpfact!(A) -> QRPivotedDense

   \"qrpfact!\" is the same as \"qrpfact\" but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Linear Algebra","Base","qmulQR","qmulQR(QR, A)

   Perform \"Q*A\" efficiently, where Q is a an orthogonal matrix
   defined as the product of k elementary reflectors from the QR
   decomposition.

"),

("Linear Algebra","Base","qTmulQR","qTmulQR(QR, A)

   Perform \"Q'*A\" efficiently, where Q is a an orthogonal matrix
   defined as the product of k elementary reflectors from the QR
   decomposition.

"),

("Linear Algebra","Base","sqrtm","sqrtm(A)

   Compute the matrix square root of \"A\". If \"B = sqrtm(A)\", then
   \"B*B == A\" within roundoff error.

"),

("Linear Algebra","Base","eig","eig(A) -> D, V

   Compute eigenvalues and eigenvectors of A

"),

("Linear Algebra","Base","eigvals","eigvals(A)

   Returns the eigenvalues of \"A\".

"),

("Linear Algebra","Base","svdfact","svdfact(A[, thin]) -> SVDDense

   Compute the Singular Value Decomposition (SVD) of \"A\" and return
   an \"SVDDense\" object. \"factors(svdfact(A))\" returns \"U\",
   \"S\", and \"Vt\", such that \"A = U*diagm(S)*Vt\". If \"thin\" is
   \"true\", an economy mode decomposition is returned.

"),

("Linear Algebra","Base","svdfact!","svdfact!(A[, thin]) -> SVDDense

   \"svdfact!\" is the same as \"svdfact\" but saves space by
   overwriting the input A, instead of creating a copy. If \"thin\" is
   \"true\", an economy mode decomposition is returned.

"),

("Linear Algebra","Base","svd","svd(A[, thin]) -> U, S, V

   Compute the SVD of A, returning \"U\", vector \"S\", and \"V\" such
   that \"A == U*diagm(S)*V'\". If \"thin\" is \"true\", an economy
   mode decomposition is returned.

"),

("Linear Algebra","Base","svdt","svdt(A[, thin]) -> U, S, Vt

   Compute the SVD of A, returning \"U\", vector \"S\", and \"Vt\"
   such that \"A = U*diagm(S)*Vt\". If \"thin\" is \"true\", an
   economy mode decomposition is returned.

"),

("Linear Algebra","Base","svdvals","svdvals(A)

   Returns the singular values of \"A\".

"),

("Linear Algebra","Base","svdvals!","svdvals!(A)

   Returns the singular values of \"A\", while saving space by
   overwriting the input.

"),

("Linear Algebra","Base","svdfact","svdfact(A, B) -> GSVDDense

   Compute the generalized SVD of \"A\" and \"B\", returning a
   \"GSVDDense\" Factorization object. \"factors(svdfact(A,b))\"
   returns \"U\", \"V\", \"Q\", \"D1\", \"D2\", and \"R0\" such that
   \"A = U*D1*R0*Q'\" and \"B = V*D2*R0*Q'\".

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

   Upper triangle of a matrix

"),

("Linear Algebra","Base","tril","tril(M)

   Lower triangle of a matrix

"),

("Linear Algebra","Base","diag","diag(M[, k])

   The \"k\"-th diagonal of a matrix, as a vector

"),

("Linear Algebra","Base","diagm","diagm(v[, k])

   Construct a diagonal matrix and place \"v\" on the \"k\"-th
   diagonal

"),

("Linear Algebra","Base","diagmm","diagmm(matrix, vector)

   Multiply matrices, interpreting the vector argument as a diagonal
   matrix. The arguments may occur in the other order to multiply with
   the diagonal matrix on the left.

"),

("Linear Algebra","Base","Tridiagonal","Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal,
   and upper diagonal

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

   The transpose operator (.').

"),

("Linear Algebra","Base","ctranspose","ctranspose(A)

   The conjugate transpose operator (').

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

("Statistics","Base","mean","mean(v[, dim])

   Compute the mean of whole array \"v\", or optionally along
   dimension \"dim\"

"),

("Statistics","Base","std","std(v[, corrected])

   Compute the sample standard deviation of a vector \"v\". If the
   optional argument \"corrected\" is either left unspecified or is
   explicitly set to the default value of \"true\", then the algorithm
   will return an estimator of the generative distribution's standard
   deviation under the assumption that each entry of \"v\" is an IID
   draw from that generative distribution. This computation is
   equivalent to calculating \"sqrt(sum((v .- mean(v)).^2) /
   (length(v) - 1))\" and involves an implicit correction term
   sometimes called the Bessel correction which insures that the
   estimator of the variance is unbiased. If, instead, the optional
   argument \"corrected\" is set to \"false\", then the algorithm will
   produce the equivalent of \"sqrt(sum((v .- mean(v)).^2) /
   length(v))\", which is the empirical standard deviation of the
   sample.

"),

("Statistics","Base","std","std(v, m[, corrected])

   Compute the sample standard deviation of a vector \"v\" with known
   mean \"m\". If the optional argument \"corrected\" is either left
   unspecified or is explicitly set to the default value of \"true\",
   then the algorithm will return an estimator of the generative
   distribution's standard deviation under the assumption that each
   entry of \"v\" is an IID draw from that generative distribution.
   This computation is equivalent to calculating \"sqrt(sum((v .-
   m).^2) / (length(v) - 1))\" and involves an implicit correction
   term sometimes called the Bessel correction which insures that the
   estimator of the variance is unbiased. If, instead, the optional
   argument \"corrected\" is set to \"false\", then the algorithm will
   produce the equivalent of \"sqrt(sum((v .- m).^2) / length(v))\",
   which is the empirical standard deviation of the sample.

"),

("Statistics","Base","var","var(v[, corrected])

   Compute the sample variance of a vector \"v\". If the optional
   argument \"corrected\" is either left unspecified or is explicitly
   set to the default value of \"true\", then the algorithm will
   return an unbiased estimator of the generative distribution's
   variance under the assumption that each entry of \"v\" is an IID
   draw from that generative distribution. This computation is
   equivalent to calculating \"sum((v .- mean(v)).^2) / (length(v) -
   1)\" and involves an implicit correction term sometimes called the
   Bessel correction. If, instead, the optional argument \"corrected\"
   is set to \"false\", then the algorithm will produce the equivalent
   of \"sum((v .- mean(v)).^2) / length(v)\", which is the empirical
   variance of the sample.

"),

("Statistics","Base","var","var(v, m[, corrected])

   Compute the sample variance of a vector \"v\" with known mean
   \"m\". If the optional argument \"corrected\" is either left
   unspecified or is explicitly set to the default value of \"true\",
   then the algorithm will return an unbiased estimator of the
   generative distribution's variance under the assumption that each
   entry of \"v\" is an IID draw from that generative distribution.
   This computation is equivalent to calculating \"sum((v .- m)).^2) /
   (length(v) - 1)\" and involves an implicit correction term
   sometimes called the Bessel correction. If, instead, the optional
   argument \"corrected\" is set to \"false\", then the algorithm will
   produce the equivalent of \"sum((v .- m)).^2) / length(v)\", which
   is the empirical variance of the sample.

"),

("Statistics","Base","median","median(v)

   Compute the median of a vector \"v\"

"),

("Statistics","Base","hist","hist(v[, n])

   Compute the histogram of \"v\", optionally using \"n\" bins

"),

("Statistics","Base","hist","hist(v, e)

   Compute the histogram of \"v\" using a vector \"e\" as the edges
   for the bins

"),

("Statistics","Base","quantile","quantile(v, p)

   Compute the quantiles of a vector \"v\" at a specified set of
   probability values \"p\".

"),

("Statistics","Base","quantile","quantile(v)

   Compute the quantiles of a vector \"v\" at the probability values
   \"[.0, .2, .4, .6, .8, 1.0]\".

"),

("Statistics","Base","cov","cov(v)

   Compute the Pearson covariance between two vectors \"v1\" and
   \"v2\".

"),

("Statistics","Base","cor","cor(v)

   Compute the Pearson correlation between two vectors \"v1\" and
   \"v2\".

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

("Signal Processing","","ifft(A [, dims]), bfft, bfft!","ifft(A [, dims]), bfft, bfft!

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

("Signal Processing","","plan_fft(A [, dims [, flags [, timelimit]]]),  plan_ifft, plan_bfft","plan_fft(A [, dims [, flags [, timelimit]]]),  plan_ifft, plan_bfft

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

("Signal Processing","","plan_irfft(A, d [, dims [, flags [, timelimit]]]), plan_bfft","plan_irfft(A, d [, dims [, flags [, timelimit]]]), plan_bfft

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

("Signal Processing","","FFTW.r2r(A, kind [, dims]), FFTW.r2r!","FFTW.r2r(A, kind [, dims]), FFTW.r2r!

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

   \"FFTW.r2r!()\" is the same as \"FFTW.r2r()\", but operates in-
   place on \"A\", which must be an array of real or complex floating-
   point numbers.

"),

("Signal Processing","","FFTW.plan_r2r(A, kind [, dims [, flags [, timelimit]]]), FFTW.plan_r2r!","FFTW.plan_r2r(A, kind [, dims [, flags [, timelimit]]]), FFTW.plan_r2r!

   Pre-plan an optimized r2r transform, similar to \"plan_fft()\"
   except that the transforms (and the first three arguments)
   correspond to \"FFTW.r2r()\" and \"FFTW.r2r!()\", respectively.

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

("System","","> < >> .>","> < >> .>

   \">\" \"<\" and \">>\" work exactly as in bash, and \".>\"
   redirects STDERR.

   **Example**: \"run((`ls` > \"out.log\") .> \"err.log\")\"

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

   Reference to the singleton \"EnvHash\".

"),

("C Interface","Base","ccall","ccall((symbol, library), RetType, (ArgType1, ...), ArgVar1, ...)
ccall(fptr::Ptr{Void}, RetType, (ArgType1, ...), ArgVar1, ...)

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

("Errors","Base","error","error(message::String)
error(Exception)

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

("BLAS","BLAS","copy!","copy!(n, X, incx, Y, incy)

   Copy \"n\" elements of array \"X\" with stride \"incx\" to array
   \"Y\" with stride \"incy\".  Returns \"Y\".

"),

("BLAS","BLAS","dot","dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of \"n\" elements of array
   \"X\" with stride \"incx\" and \"n\" elements of array \"Y\" with
   stride \"incy\".  There are no \"dot\" methods for \"Complex\"
   arrays.

"),

("BLAS","BLAS","nrm2","nrm2(n, X, incx)

   2-norm of a vector consisting of \"n\" elements of array \"X\" with
   stride \"incx\".

"),

("BLAS","BLAS","axpy!","axpy!(n, a, X, incx, Y, incy)

   Overwrite \"Y\" with \"a*X + Y\".  Returns \"Y\".

"),

("BLAS","BLAS","syrk!","syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix \"C\" as \"alpha*A*A.' +
   beta*C\" or \"alpha*A.'*A + beta*C\" according to whether \"trans\"
   is 'N' or 'T'.  When \"uplo\" is 'U' the upper triangle of \"C\" is
   updated ('L' for lower triangle).  Returns \"C\".

"),

("BLAS","BLAS","syrk","syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according
   to \"uplo\" ('U' or 'L'), of \"alpha*A*A.'\" or \"alpha*A.'*A\",
   according to \"trans\" ('N' or 'T').

"),

("BLAS","BLAS","herk!","herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian
   matrix \"C\" as \"alpha*A*A' + beta*C\" or \"alpha*A'*A + beta*C\"
   according to whether \"trans\" is 'N' or 'T'.  When \"uplo\" is 'U'
   the upper triangle of \"C\" is updated ('L' for lower triangle).
   Returns \"C\".

"),

("BLAS","BLAS","herk","herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle
   or the lower triangle, according to \"uplo\" ('U' or 'L'), of
   \"alpha*A*A'\" or \"alpha*A'*A\", according to \"trans\" ('N' or
   'T').

"),

("BLAS","BLAS","gbmv!","gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" or \"alpha*A'*x +
   beta*y\" according to \"trans\" ('N' or 'T').  The matrix \"A\" is
   a general band matrix of dimension \"m\" by \"size(A,2)\" with
   \"kl\" sub-diagonals and \"ku\" super-diagonals. Returns the
   updated \"y\".

"),

("BLAS","BLAS","gbmv","gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns \"alpha*A*x\" or \"alpha*A'*x\" according to \"trans\" ('N'
   or 'T'). The matrix \"A\" is a general band matrix of dimension
   \"m\" by \"size(A,2)\" with \"kl\" sub-diagonals and \"ku\" super-
   diagonals.

"),

("BLAS","BLAS","sbmv!","sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" where \"A\" is a a
   symmetric band matrix of order \"size(A,2)\" with \"k\" super-
   diagonals stored in the argument \"A\".  The storage layout for
   \"A\" is described the reference BLAS module, level-2 BLAS at
   *<http://www.netlib.org/lapack/explore-html/>*.

   Returns the updated \"y\".

"),

("BLAS","BLAS","sbmv","sbmv(uplo, k, alpha, A, x)

   Returns \"alpha*A*x\" where \"A\" is a symmetric band matrix of
   order \"size(A,2)\" with \"k\" super-diagonals stored in the
   argument \"A\".

"),

("BLAS","BLAS","gemm!","gemm!(tA, tB, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or the other three variants
   according to \"tA\" (transpose \"A\") and \"tB\".  Returns the
   updated \"C\".

"),

("BLAS","BLAS","gemm","gemm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("Constants","Base","OS_NAME","OS_NAME

   A symbol representing the name of the operating system. Possible
   values are \":Linux\", \":Darwin\" (OS X), or \":Windows\".

"),

("cpp.jl","","@cpp","@cpp(ccall_expression)

   Suppose you have a C++ shared library, \"libdemo\", which contains
   a function \"timestwo\":

      int timestwo(int x) {
        return 2*x;
      }

      double timestwo(double x) {
        return 2*x;
      }

   You can use these functions by placing the \"@cpp\" macro prior to
   a ccall, for example:

      mylib = dlopen(\"libdemo\")
      x = 3.5
      x2 = @cpp ccall(dlsym(mylib, :timestwo), Float64, (Float64,), x)
      y = 3
      y2 = @cpp ccall(dlsym(mylib, :timestwo), Int, (Int,), y)

   The macro performs C++ ABI name-mangling (using the types of the
   parameters) to determine the correct library symbol.

   Like \"ccall\", this performs library calls without overhead.
   However, currently it has a number of limitations:

   * It does not support pure-header libraries

   * The restrictions of \"ccall\" apply here; for example, there is
     no support for \"struct\". Consequently it is not possible to use
     C++ objects.

   * Currently there is no C++ namespace support

   * Currently there is no support for templated functions

   * Currently only g++ is supported

   The latter three may not be difficult to fix.

"),

("GLPK","GLPK","set_prob_name","set_prob_name(glp_prob, name)

   Assigns a name to the problem object (or deletes it if \"name\" is
   empty or \"nothing\").

"),

("GLPK","GLPK","set_obj_name","set_obj_name(glp_prob, name)

   Assigns a name to the objective function (or deletes it if \"name\"
   is empty or \"nothing\").

"),

("GLPK","GLPK","set_obj_dir","set_obj_dir(glp_prob, dir)

   Sets the optimization direction, \"GLPK.MIN\" (minimization) or
   \"GLPK.MAX\" (maximization).

"),

("GLPK","GLPK","add_rows","add_rows(glp_prob, rows)

   Adds the given number of rows (constraints) to the problem object;
   returns the number of the first new row added.

"),

("GLPK","GLPK","add_cols","add_cols(glp_prob, cols)

   Adds the given number of columns (structural variables) to the
   problem object; returns the number of the first new column added.

"),

("GLPK","GLPK","set_row_name","set_row_name(glp_prob, row, name)

   Assigns a name to the specified row (or deletes it if \"name\" is
   empty or \"nothing\").

"),

("GLPK","GLPK","set_col_name","set_col_name(glp_prob, col, name)

   Assigns a name to the specified column (or deletes it if \"name\"
   is empty or \"nothing\").

"),

("GLPK","GLPK","set_row_bnds","set_row_bnds(glp_prob, row, bounds_type, lb, ub)

   Sets the type and bounds on a row. \"type\" must be one of
   \"GLPK.FR\" (free), \"GLPK.LO\" (lower bounded), \"GLPK.UP\" (upper
   bounded), \"GLPK.DB\" (double bounded), \"GLPK.FX\" (fixed).

   At initialization, each row is free.

"),

("GLPK","GLPK","set_col_bnds","set_col_bnds(glp_prob, col, bounds_type, lb, ub)

   Sets the type and bounds on a column. \"type\" must be one of
   \"GLPK.FR\" (free), \"GLPK.LO\" (lower bounded), \"GLPK.UP\" (upper
   bounded), \"GLPK.DB\" (double bounded), \"GLPK.FX\" (fixed).

   At initialization, each column is fixed at 0.

"),

("GLPK","GLPK","set_obj_coef","set_obj_coef(glp_prob, col, coef)

   Sets the objective coefficient to a column (\"col\" can be 0 to
   indicate the constant term of the objective function).

"),

("GLPK","GLPK","set_mat_row","set_mat_row(glp_prob, row[, len], ind, val)

   Sets (replaces) the content of a row. The content is specified in
   sparse format: \"ind\" is a vector of indices, \"val\" is the
   vector of corresponding values. \"len\" is the number of vector
   elements which will be considered, and must be less or equal to the
   length of both \"ind\" and \"val\".  If \"len\" is 0, \"ind\"
   and/or \"val\" can be \"nothing\".

   In Julia, \"len\" can be omitted, and then it is inferred from
   \"ind\" and \"val\" (which need to have the same length in such
   case).

"),

("GLPK","GLPK","set_mat_col","set_mat_col(glp_prob, col[, len], ind, val)

   Sets (replaces) the content of a column. Everything else is like
   \"set_mat_row\".

"),

("GLPK","GLPK","load_matrix","load_matrix(glp_prob[, numel], ia, ja, ar)
load_matrix(glp_prob, A)

   Sets (replaces) the content matrix (i.e. sets all  rows/coluns at
   once). The matrix is passed in sparse format.

   In the first form (original C API), it's passed via 3 vectors:
   \"ia\" and \"ja\" are for rows/columns indices, \"ar\" is for
   values. \"numel\" is the number of elements which will be read and
   must be less or equal to the length of any of the 3 vectors. If
   \"numel\" is 0, any of the vectors can be passed as \"nothing\".

   In Julia, \"numel\" can be omitted, and then it is inferred from
   \"ia\", \"ja\" and \"ar\" (which need to have the same length in
   such case).

   Also, in Julia there's a second, simpler calling form, in which the
   matrix is passed as a \"SparseMatrixCSC\" object.

"),

("GLPK","GLPK","check_dup","check_dup(rows, cols[, numel], ia, ja)

   Check for duplicates in the indices vectors \"ia\" and \"ja\".
   \"numel\" has the same meaning and (optional) use as in
   \"load_matrix\". Returns 0 if no duplicates/out-of-range indices
   are found, or a positive number indicating where a duplicate
   occurs, or a negative number indicating an out-of-bounds index.

"),

("GLPK","GLPK","sort_matrix","sort_matrix(glp_prob)

   Sorts the elements of the problem object's matrix.

"),

("GLPK","GLPK","del_rows","del_rows(glp_prob[, num_rows], rows_ids)

   Deletes rows from the problem object. Rows are specified in the
   \"rows_ids\" vector. \"num_rows\" is the number of elements of
   \"rows_ids\" which will be considered, and must be less or equal to
   the length id \"rows_ids\". If \"num_rows\" is 0, \"rows_ids\" can
   be \"nothing\". In Julia, \"num_rows\" is optional (it's inferred
   from \"rows_ids\" if not given).

"),

("GLPK","GLPK","del_cols","del_cols(glp_prob, cols_ids)

   Deletes columns from the problem object. See \"del_rows\".

"),

("GLPK","GLPK","copy_prob","copy_prob(glp_prob_dest, glp_prob, copy_names)

   Makes a copy of the problem object. The flag \"copy_names\"
   determines if names are copied, and must be either \"GLPK.ON\" or
   \"GLPK.OFF\".

"),

("GLPK","GLPK","erase_prob","erase_prob(glp_prob)

   Resets the problem object.

"),

("GLPK","GLPK","get_prob_name","get_prob_name(glp_prob)

   Returns the problem object's name. Unlike the C version, if the
   problem has no assigned name, returns an empty string.

"),

("GLPK","GLPK","get_obj_name","get_obj_name(glp_prob)

   Returns the objective function's name. Unlike the C version, if the
   objective has no assigned name, returns an empty string.

"),

("GLPK","GLPK","get_obj_dir","get_obj_dir(glp_prob)

   Returns the optimization direction, \"GLPK.MIN\" (minimization) or
   \"GLPK.MAX\" (maximization).

"),

("GLPK","GLPK","get_num_rows","get_num_rows(glp_prob)

   Returns the current number of rows.

"),

("GLPK","GLPK","get_num_cols","get_num_cols(glp_prob)

   Returns the current number of columns.

"),

("GLPK","GLPK","get_row_name","get_row_name(glp_prob, row)

   Returns the name of the specified row. Unlike the C version, if the
   row has no assigned name, returns an empty string.

"),

("GLPK","GLPK","get_col_name","get_col_name(glp_prob, col)

   Returns the name of the specified column. Unlike the C version, if
   the column has no assigned name, returns an empty string.

"),

("GLPK","GLPK","get_row_type","get_row_type(glp_prob, row)

   Returns the type of the specified row: \"GLPK.FR\" (free),
   \"GLPK.LO\" (lower bounded), \"GLPK.UP\" (upper bounded),
   \"GLPK.DB\" (double bounded), \"GLPK.FX\" (fixed).

"),

("GLPK","GLPK","get_row_lb","get_row_lb(glp_prob, row)

   Returns the lower bound of the specified row, \"-DBL_MAX\" if
   unbounded.

"),

("GLPK","GLPK","get_row_ub","get_row_ub(glp_prob, row)

   Returns the upper bound of the specified row, \"+DBL_MAX\" if
   unbounded.

"),

("GLPK","GLPK","get_col_type","get_col_type(glp_prob, col)

   Returns the type of the specified column: \"GLPK.FR\" (free),
   \"GLPK.LO\" (lower bounded), \"GLPK.UP\" (upper bounded),
   \"GLPK.DB\" (double bounded), \"GLPK.FX\" (fixed).

"),

("GLPK","GLPK","get_col_lb","get_col_lb(glp_prob, col)

   Returns the lower bound of the specified column, \"-DBL_MAX\" if
   unbounded.

"),

("GLPK","GLPK","get_col_ub","get_col_ub(glp_prob, col)

   Returns the upper bound of the specified column, \"+DBL_MAX\" if
   unbounded.

"),

("GLPK","GLPK","get_obj_coef","get_obj_coef(glp_prob, col)

   Return the objective coefficient to a column (\"col\" can be 0 to
   indicate the constant term of the objective function).

"),

("GLPK","GLPK","get_num_nz","get_num_nz(glp_prob)

   Return the number of non-zero elements in the constraint matrix.

"),

("GLPK","GLPK","get_mat_row","get_mat_row(glp_prob, row, ind, val)
get_mat_row(glp_prob, row)

   Returns the contents of a row. In the first form (original C API),
   it fills the \"ind\" and \"val\" vectors provided, which must be of
   type \"Vector{Int32}\" and \"Vector{Float64}\" respectively, and
   have a sufficient length to hold the result (or they can be empty
   or \"nothing\", and then they're not filled). It returns the length
   of the result.

   In Julia, there's a second, simpler calling form which allocates
   and returns the two vectors as \"(ind, val)\".

"),

("GLPK","GLPK","get_mat_col","get_mat_col(glp_prob, col, ind, val)
get_mat_col(glp_prob, col)

   Returns the contents of a column. See \"get_mat_row\".

"),

("GLPK","GLPK","create_index","create_index(glp_prob)

   Creates the name index (used by \"find_row\", \"find_col\") for the
   problem object.

"),

("GLPK","GLPK","find_row","find_row(glp_prob, name)

   Finds the numeric id of a row by name. Returns 0 if no row with the
   given name is found.

"),

("GLPK","GLPK","find_col","find_col(glp_prob, name)

   Finds the numeric id of a column by name. Returns 0 if no column
   with the given name is found.

"),

("GLPK","GLPK","delete_index","delete_index(glp_prob)

   Deletes the name index for the problem object.

"),

("GLPK","GLPK","set_rii","set_rii(glp_prob, row, rii)

   Sets the rii scale factor for the specified row.

"),

("GLPK","GLPK","set_sjj","set_sjj(glp_prob, col, sjj)

   Sets the sjj scale factor for the specified column.

"),

("GLPK","GLPK","get_rii","get_rii(glp_prob, row)

   Returns the rii scale factor for the specified row.

"),

("GLPK","GLPK","get_sjj","get_sjj(glp_prob, col)

   Returns the sjj scale factor for the specified column.

"),

("GLPK","GLPK","scale_prob","scale_prob(glp_prob, flags)

   Performs automatic scaling of problem data for the problem object.
   The parameter \"flags\" can be \"GLPK.SF_AUTO\" (automatic) or a
   bitwise OR of the forllowing: \"GLPK.SF_GM\" (geometric mean),
   \"GLPK.SF_EQ\" (equilibration), \"GLPK.SF_2N\" (nearest power of
   2), \"GLPK.SF_SKIP\" (skip if well scaled).

"),

("GLPK","GLPK","unscale_prob","unscale_prob(glp_prob)

   Unscale the problem data (cancels the scaling effect).

"),

("GLPK","GLPK","set_row_stat","set_row_stat(glp_prob, row, stat)

   Sets the status of the specified row. \"stat\" must be one of:
   \"GLPK.BS\" (basic), \"GLPK.NL\" (non-basic lower bounded),
   \"GLPK.NU\" (non-basic upper-bounded), \"GLPK.NF\" (non-basic
   free), \"GLPK.NS\" (non-basic fixed).

"),

("GLPK","GLPK","set_col_stat","set_col_stat(glp_prob, col, stat)

   Sets the status of the specified column. \"stat\" must be one of:
   \"GLPK.BS\" (basic), \"GLPK.NL\" (non-basic lower bounded),
   \"GLPK.NU\" (non-basic upper-bounded), \"GLPK.NF\" (non-basic
   free), \"GLPK.NS\" (non-basic fixed).

"),

("GLPK","GLPK","std_basis","std_basis(glp_prob)

   Constructs the standard (trivial) initial LP basis for the problem
   object.

"),

("GLPK","GLPK","adv_basis","adv_basis(glp_prob[, flags])

   Constructs an advanced initial LP basis for the problem object. The
   flag \"flags\" is optional; it must be 0 if given.

"),

("GLPK","GLPK","cpx_basis","cpx_basis(glp_prob)

   Constructs an initial LP basis for the problem object with the
   algorithm proposed by R. Bixby.

"),

("GLPK","GLPK","simplex","simplex(glp_prob[, glp_param])

   The routine \"simplex\" is a driver to the LP solver based on the
   simplex method. This routine retrieves problem data from the
   specified problem object, calls the solver to solve the problem
   instance, and stores results of computations back into the problem
   object.

   The parameters are specified via the optional \"glp_param\"
   argument, which is of type \"GLPK.SimplexParam\" (or \"nothing\" to
   use the default settings).

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: \"GLPK.EBADB\" (invalid base), \"GLPK.ESING\"
   (singular matrix), \"GLPK.ECOND\" (ill-conditioned matrix),
   \"GLPK.EBOUND\" (incorrect bounds), \"GLPK.EFAIL\" (solver
   failure), \"GLPK.EOBJLL\" (lower limit reached), \"GLPK.EOBJUL\"
   (upper limit reached), \"GLPK.ITLIM\" (iterations limit exceeded),
   \"GLPK.ETLIM\" (time limit exceeded), \"GLPK.ENOPFS\" (no primal
   feasible solution), \"GLPK.ENODFS\" (no dual feasible solution).

"),

("GLPK","GLPK","exact","exact(glp_prob[, glp_param])

   A tentative implementation of the primal two-phase simplex method
   based on exact (rational) arithmetic. Similar to \"simplex\". The
   optional \"glp_param\" is of type \"GLPK.SimplexParam\".

   The possible return values are \"0\" (success) or \"GLPK.EBADB\",
   \"GLPK.ESING\", \"GLPK.EBOUND\", \"GLPK.EFAIL\", \"GLPK.ITLIM\",
   \"GLPK.ETLIM\" (see \"simplex()\").

"),

("GLPK","GLPK","init_smcp","init_smcp(glp_param)

   Initializes a \"GLPK.SimplexParam\" object with the default values.
   In Julia, this is done at object creation time; this function can
   be used to reset the object.

"),

("GLPK","GLPK","get_status","get_status(glp_prob)

   Returns the generic status of the current basic solution:
   \"GLPK.OPT\" (optimal), \"GLPK.FEAS\" (feasible), \"GLPK.INFEAS\"
   (infeasible), \"GLPK.NOFEAS\" (no feasible solution),
   \"GLPK.UNBND\" (unbounded solution), \"GLPK.UNDEF\" (undefined).

"),

("GLPK","GLPK","get_prim_stat","get_prim_stat(glp_prob)

   Returns the status of the primal basic solution: \"GLPK.FEAS\",
   \"GLPK.INFEAS\", \"GLPK.NOFEAS\", \"GLPK.UNDEF\" (see
   \"get_status()\").

"),

("GLPK","GLPK","get_dual_stat","get_dual_stat(glp_prob)

   Returns the status of the dual basic solution: \"GLPK.FEAS\",
   \"GLPK.INFEAS\", \"GLPK.NOFEAS\", \"GLPK.UNDEF\" (see
   \"get_status()\").

"),

("GLPK","GLPK","get_obj_val","get_obj_val(glp_prob)

   Returns the current value of the objective function.

"),

("GLPK","GLPK","get_row_stat","get_row_stat(glp_prob, row)

   Returns the status of the specified row: \"GLPK.BS\", \"GLPK.NL\",
   \"GLPK.NU\", \"GLPK.NF\", \"GLPK.NS\" (see \"set_row_stat()\").

"),

("GLPK","GLPK","get_row_prim","get_row_prim(glp_prob, row)

   Returns the primal value of the specified row.

"),

("GLPK","GLPK","get_row_dual","get_row_dual(glp_prob, row)

   Returns the dual value (reduced cost) of the specified row.

"),

("GLPK","GLPK","get_col_stat","get_col_stat(glp_prob, col)

   Returns the status of the specified column: \"GLPK.BS\",
   \"GLPK.NL\", \"GLPK.NU\", \"GLPK.NF\", \"GLPK.NS\" (see
   \"set_row_stat()\").

"),

("GLPK","GLPK","get_col_prim","get_col_prim(glp_prob, col)

   Returns the primal value of the specified column.

"),

("GLPK","GLPK","get_col_dual","get_col_dual(glp_prob, col)

   Returns the dual value (reduced cost) of the specified column.

"),

("GLPK","GLPK","get_unbnd_ray","get_unbnd_ray(glp_prob)

   Returns the number k of a variable, which causes primal or dual
   unboundedness (if 1 <= k <= rows it's row k; if rows+1 <= k <=
   rows+cols it's column k-rows, if k=0 such variable is not defined).

"),

("GLPK","GLPK","interior","interior(glp_prob[, glp_param])

   The routine \"interior\" is a driver to the LP solver based on the
   primal-dual interior-point method. This routine retrieves problem
   data from the specified problem object, calls the solver to solve
   the problem instance, and stores results of computations back into
   the problem object.

   The parameters are specified via the optional \"glp_param\"
   argument, which is of type \"GLPK.InteriorParam\" (or \"nothing\"
   to use the default settings).

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: \"GLPK.EFAIL\" (solver failure),
   \"GLPK.ENOCVG\" (very slow convergence, or divergence),
   \"GLPK.ITLIM\" (iterations limit exceeded), \"GLPK.EINSTAB\"
   (numerical instability).

"),

("GLPK","GLPK","init_iptcp","init_iptcp(glp_param)

   Initializes a \"GLPK.InteriorParam\" object with the default
   values. In Julia, this is done at object creation time; this
   function can be used to reset the object.

"),

("GLPK","GLPK","ipt_status","ipt_status(glp_prob)

   Returns the status of the interior-point solution: \"GLPK.OPT\"
   (optimal), \"GLPK.INFEAS\" (infeasible), \"GLPK.NOFEAS\" (no
   feasible solution), \"GLPK.UNDEF\" (undefined).

"),

("GLPK","GLPK","ipt_obj_val","ipt_obj_val(glp_prob)

   Returns the current value of the objective function for the
   interior-point solution.

"),

("GLPK","GLPK","ipt_row_prim","ipt_row_prim(glp_prob, row)

   Returns the primal value of the specified row for the interior-
   point solution.

"),

("GLPK","GLPK","ipt_row_dual","ipt_row_dual(glp_prob, row)

   Returns the dual value (reduced cost) of the specified row for the
   interior-point solution.

"),

("GLPK","GLPK","ipt_col_prim","ipt_col_prim(glp_prob, col)

   Returns the primal value of the specified column for the interior-
   point solution.

"),

("GLPK","GLPK","ipt_col_dual","ipt_col_dual(glp_prob, col)

   Returns the dual value (reduced cost) of the specified column for
   the interior-point solution.

"),

("GLPK","GLPK","set_col_kind","set_col_kind(glp_prob, col, kind)

   Sets the kind for the specified column (for mixed-integer
   programming). \"kind\" must be one of: \"GLPK.CV\" (continuous),
   \"GLPK.IV\" (integer), \"GLPK.BV\" (binary, 0/1).

"),

("GLPK","GLPK","get_col_kind","get_col_kind(glp_prob, col)

   Returns the kind for the specified column (see \"set_col_kind()\").

"),

("GLPK","GLPK","get_num_int","get_num_int(glp_prob)

   Returns the number of columns marked as integer (including binary).

"),

("GLPK","GLPK","get_num_bin","get_num_bin(glp_prob)

   Returns the number of columns marked binary.

"),

("GLPK","GLPK","intopt","intopt(glp_prob[, glp_param])

   The routine \"intopt\" is a driver to the mixed-integer-programming
   (MIP) solver based on the branch- and-cut method, which is a hybrid
   of branch-and-bound and cutting plane methods.

   The parameters are specified via the optional \"glp_param\"
   argument, which is of type \"GLPK.IntoptParam\" (or \"nothing\" to
   use the default settings).

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: \"GLPK.EBOUND\" (incorrect bounds),
   \"GLPK.EROOT\" (no optimal LP basis given), \"GLPK.ENOPFS\" (no
   primal feasible LP solution), \"GLPK.ENODFS\" (no dual feasible LP
   solution), \"GLPK.EFAIL\" (solver failure), \"GLPK.EMIPGAP\" (mip
   gap tolearance reached), \"GLPK.ETLIM\" (time limit exceeded),
   \"GLPK.ESTOP\" (terminated by application).

"),

("GLPK","GLPK","init_iocp","init_iocp(glp_param)

   Initializes a \"GLPK.IntoptParam\" object with the default values.
   In Julia, this is done at object creation time; this function can
   be used to reset the object.

"),

("GLPK","GLPK","mip_status","mip_status(glp_prob)

   Returns the generic status of the MIP solution: \"GLPK.OPT\"
   (optimal), \"GLPK.FEAS\" (feasible), \"GLPK.NOFEAS\" (no feasible
   solution), \"GLPK.UNDEF\" (undefined).

"),

("GLPK","GLPK","mip_obj_val","mip_obj_val(glp_prob)

   Returns the current value of the objective function for the MIP
   solution.

"),

("GLPK","GLPK","mip_row_val","mip_row_val(glp_prob, row)

   Returns the value of the specified row for the MIP solution.

"),

("GLPK","GLPK","mip_col_val","mip_col_val(glp_prob, col)

   Returns the value of the specified column for the MIP solution.

"),

("GLPK","GLPK","read_mps","read_mps(glp_prob, format[, param], filename)

   Reads problem data in MPS format from a text file. \"format\" must
   be one of \"GLPK.MPS_DECK\" (fixed, old) or \"GLPK.MPS_FILE\"
   (free, modern). \"param\" is optional; if given it must be
   \"nothing\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","write_mps","write_mps(glp_prob, format[, param], filename)

   Writes problem data in MPS format from a text file. See
   \"read_mps\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","read_lp","read_lp(glp_prob[, param], filename)

   Reads problem data in CPLEX LP format from a text file. \"param\"
   is optional; if given it must be \"nothing\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","write_lp","write_lp(glp_prob[, param], filename)

   Writes problem data in CPLEX LP format from a text file. See
   \"read_lp\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","read_prob","read_prob(glp_prob[, flags], filename)

   Reads problem data in GLPK LP/MIP format from a text file.
   \"flags\" is optional; if given it must be 0.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","write_prob","write_prob(glp_prob[, flags], filename)

   Writes problem data in GLPK LP/MIP format from a text file. See
   \"read_prob\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","mpl_read_model","mpl_read_model(glp_tran, filename, skip)

   Reads the model section and, optionally, the data section, from a
   text file in MathProg format, and stores it in \"glp_tran\", which
   is a \"GLPK.MathProgWorkspace\" object. If \"skip\" is nonzero, the
   data section is skipped if present.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","mpl_read_data","mpl_read_data(glp_tran, filename)

   Reads data section from a text file in MathProg format and stores
   it in \"glp_tran\", which is a \"GLPK.MathProgWorkspace\" object.
   May be called more than once.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","mpl_generate","mpl_generate(glp_tran[, filename])

   Generates the model using its description stored in the
   \"GLPK.MathProgWorkspace\" translator workspace \"glp_tran\". The
   optional \"filename\" specifies an output file; if not given or
   \"nothing\", the terminal is used.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","mpl_build_prob","mpl_build_prob(glp_tran, glp_prob)

   Transfer information from the \"GLPK.MathProgWorkspace\" translator
   workspace \"glp_tran\" to the \"GLPK.Prob\" problem object
   \"glp_prob\".

"),

("GLPK","GLPK","mpl_postsolve","mpl_postsolve(glp_tran, glp_prob, sol)

   Copies the solution from the \"GLPK.Prob\" problem object
   \"glp_prob\" to the \"GLPK.MathProgWorkspace\" translator workspace
   \"glp_tran\" and then executes all the remaining model statements,
   which follow the solve statement.

   The parameter \"sol\" specifies which solution should be copied
   from the problem object to the workspace: \"GLPK.SOL\" (basic),
   \"GLPK.IPT\" (interior-point), \"GLPK.MIP\" (MIP).

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","print_sol","print_sol(glp_prob, filename)

   Writes the current basic solution to a text file, in printable
   format.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","read_sol","read_sol(glp_prob, filename)

   Reads the current basic solution from a text file, in the format
   used by \"write_sol\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","write_sol","write_sol(glp_prob, filename)

   Writes the current basic solution from a text file, in a format
   which can be read by \"read_sol\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","print_ipt","print_ipt(glp_prob, filename)

   Writes the current interior-point solution to a text file, in
   printable format.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","read_ipt","read_ipt(glp_prob, filename)

   Reads the current interior-point solution from a text file, in the
   format used by \"write_ipt\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","write_ipt","write_ipt(glp_prob, filename)

   Writes the current interior-point solution from a text file, in a
   format which can be read by \"read_ipt\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","print_mip","print_mip(glp_prob, filename)

   Writes the current MIP solution to a text file, in printable
   format.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","read_mip","read_mip(glp_prob, filename)

   Reads the current MIP solution from a text file, in the format used
   by \"write_mip\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","write_mip","write_mip(glp_prob, filename)

   Writes the current MIP solution from a text file, in a format which
   can be read by \"read_mip\".

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","print_ranges","print_ranges(glp_prob, [[len,] list,] [flags,] filename)

   Performs sensitivity analysis of current optimal basic solution and
   writes the analysis report in human-readable format to a text file.
   \"list\" is a vector specifying the rows/columns to analyze (if 1
   <= list[i] <= rows, analyzes row list[i]; if rows+1 <= list[i] <=
   rows+cols, analyzes column list[i]-rows). \"len\" is the number of
   elements of \"list\" which will be consideres, and must be smaller
   or equal to the length of the list. In Julia, \"len\" is optional
   (it's inferred from \"len\" if not given). \"list\" can be empty of
   \"nothing\" or not given at all, implying all indices will be
   analyzed. \"flags\" is optional, and must be 0 if given.

   To call this function, the current basic solution must be optimal,
   and the basis factorization must exist.

   Returns 0 upon success, non-zero otherwise.

"),

("GLPK","GLPK","bf_exists","bf_exists(glp_prob)

   Returns non-zero if the basis fatorization for the current basis
   exists, 0 otherwise.

"),

("GLPK","GLPK","factorize","factorize(glp_prob)

   Computes the basis factorization for the current basis.

   Returns 0 if successful, otherwise: \"GLPK.EBADB\" (invalid
   matrix), \"GLPK.ESING\" (singluar matrix), \"GLPK.ECOND\" (ill-
   conditioned matrix).

"),

("GLPK","GLPK","bf_updated","bf_updated(glp_prob)

   Returns 0 if the basis factorization was computed from scratch,
   non-zero otherwise.

"),

("GLPK","GLPK","get_bfcp","get_bfcp(glp_prob, glp_param)

   Retrieves control parameters, which are used on computing and
   updating the basis factorization associated with the problem
   object, and stores them in the \"GLPK.BasisFactParam\" object
   \"glp_param\".

"),

("GLPK","GLPK","set_bfcp","set_bfcp(glp_prob[, glp_param])

   Sets the control parameters stored in the \"GLPK.BasisFactParam\"
   object \"glp_param\" into the problem object. If \"glp_param\" is
   \"nothing\" or is omitted, resets the parameters to their defaults.

   The \"glp_param\" should always be retreived via \"get_bfcp\"
   before changing its values and calling this function.

"),

("GLPK","GLPK","get_bhead","get_bhead(glp_prob, k)

   Returns the basis header information for the current basis. \"k\"
   is a row index.

   Returns either i such that 1 <= i <= rows, if \"k\" corresponds to
   i-th auxiliary variable, or rows+j such that 1 <= j <= columns, if
   \"k\" corresponds to the j-th structural variable.

"),

("GLPK","GLPK","get_row_bind","get_row_bind(glp_prob, row)

   Returns the index of the basic variable \"k\" which is associated
   with the specified row, or \"0\" if the variable is non-basic. If
   \"GLPK.get_bhead(glp_prob, k) == row\", then
   \"GLPK.get_bind(glp_prob, row) = k\".

"),

("GLPK","GLPK","get_col_bind","get_col_bind(glp_prob, col)

   Returns the index of the basic variable \"k\" which is associated
   with the specified column, or \"0\" if the variable is non-basic.
   If \"GLPK.get_bhead(glp_prob, k) == rows+col\", then
   \"GLPK.get_bind(glp_prob, col) = k\".

"),

("GLPK","GLPK","ftran","ftran(glp_prob, v)

   Performs forward transformation (FTRAN), i.e. it solves the system
   Bx = b, where B is the basis matrix, x is the vector of unknowns to
   be computed, b is the vector of right-hand sides. At input, \"v\"
   represents the vector b; at output, it contains the vector x. \"v\"
   must be a \"Vector{Float64}\" whose length is the number of rows.

"),

("GLPK","GLPK","btran","btran(glp_prob, v)

   Performs backward transformation (BTRAN), i.e. it solves the system
   \"B'x = b\", where \"B\" is the transposed of the basis matrix,
   \"x\" is the vector of unknowns to be computed, \"b\" is the vector
   of right-hand sides. At input, \"v\" represents the vector \"b\";
   at output, it contains the vector \"x\". \"v\" must be a
   \"Vector{Float64}\" whose length is the number of rows.

"),

("GLPK","GLPK","warm_up","warm_up(glp_prob)

   \"Warms up\" the LP basis using current statuses assigned to rows
   and columns, i.e. computes factorization of the basis matrix (if it
   does not exist), computes primal and dual components of basic
   solution, and determines the solution status.

   Returns 0 if successful, otherwise: \"GLPK.EBADB\" (invalid
   matrix), \"GLPK.ESING\" (singluar matrix), \"GLPK.ECOND\" (ill-
   conditioned matrix).

"),

("GLPK","GLPK","eval_tab_row","eval_tab_row(glp_prob, k, ind, val)
eval_tab_row(glp_prob, k)

   Computes a row of the current simplex tableau which corresponds to
   some basic variable specified by the parameter \"k\". If 1 <= \"k\"
   <= rows, uses \"k\"-th auxiliary variable; if rows+1 <= \"k\" <=
   rows+cols, uses (\"k\"-rows)-th structural variable. The basis
   factorization must exist.

   In the first form, stores the result in the provided vectors
   \"ind\" and \"val\", which must be of type \"Vector{Int32}\" and
   \"Vector{Float64}\", respectively, and returns the length of the
   outcome; in Julia, the vectors will be resized as needed to hold
   the result.

   In the second, simpler form, \"ind\" and \"val\" are returned in a
   tuple as the output of the function.

"),

("GLPK","GLPK","eval_tab_col","eval_tab_col(glp_prob, k, ind, val)
eval_tab_col(glp_prob, k)

   Computes a column of the current simplex tableau which corresponds
   to some non-basic variable specified by the parameter \"k\". See
   \"eval_tab_row\".

"),

("GLPK","GLPK","transform_row","transform_row(glp_prob[, len], ind, val)

   Performs the same operation as \"eval_tab_row\" with the exception
   that the row to be transformed is specified explicitly as a sparse
   vector. The parameter \"len\" is the number of elements of \"ind\"
   and \"val\" which will be used, and must be smaller or equal to the
   length of both vectors; in Julia it is optional (and the \"ind\"
   and \"val\" must have the same length). The vectors \"int\" and
   \"val\" must be of type \"Vector{Int32}\" and \"Vector{Float64}\",
   respectively, since they will also hold the result; in Julia, they
   will be resized to the resulting required length.

   Returns the length if the resulting vectors \"ind\" and \"val\".

"),

("GLPK","GLPK","transform_col","transform_col(glp_prob[, len], ind, val)

   Performs the same operation as \"eval_tab_col\" with the exception
   that the row to be transformed is specified explicitly as a sparse
   vector. See \"transform_row\".

"),

("GLPK","GLPK","prim_rtest","prim_rtest(glp_prob[, len], ind, val, dir, eps)

   Performs the primal ratio test using an explicitly specified column
   of the simplex table. The current basic solution must be primal
   feasible. The column is specified in sparse format by \"len\"
   (length of the vector), \"ind\" and \"val\" (indices and values of
   the vector). \"len\" is the number of elements which will be
   considered and must be smaller or equal to the length of both
   \"ind\" and \"val\"; in Julia, it can be omitted (and then \"ind\"
   and \"val\" must have the same length). The indices in \"ind\" must
   be between 1 and rows+cols; they must correspond to basic
   variables. \"dir\" is a direction parameter which must be either +1
   (increasing) or -1 (decreasing). \"eps\" is a tolerance parameter
   and must be positive. See the GLPK manual for a detailed
   explanation.

   Returns the position in \"ind\" and \"val\" which corresponds to
   the pivot element, or 0 if the choice cannot be made.

"),

("GLPK","GLPK","dual_rtest","dual_rtest(glp_prob[, len], ind, val, dir, eps)

   Performs the dual ratio test using an explicitly specified row of
   the simplex table. The current basic solution must be dual
   feasible. The indices in \"ind\" must correspond to non-basic
   variables. Everything else is like in \"prim_rtest\".

"),

("GLPK","GLPK","analyze_bound","analyze_bound(glp_prob, k)

   Analyzes the effect of varying the active bound of specified non-
   basic variable. See the GLPK manual for a detailed explanation. In
   Julia, this function has a different API then C. It returns
   \"(limit1, var1, limit2, var2)\" rather then taking them as
   pointers in the argument list.

"),

("GLPK","GLPK","analyze_coef","analyze_coef(glp_prob, k)

   Analyzes the effect of varying the objective coefficient at
   specified basic variable. See the GLPK manual for a detailed
   explanation. In Julia, this function has a different API then C. It
   returns \"(coef1, var1, value1, coef2, var2, value2)\" rather then
   taking them as pointers in the argument list.

"),

("GLPK","GLPK","init_env","init_env()

   Initializes the GLPK environment. Not normally needed.

   Returns 0 (initilization successful), 1 (environment already
   initialized), 2 (failed, insufficient memory) or 3 (failed,
   unsupported programming model).

"),

("GLPK","GLPK","version","version()

   Returns the GLPK version number. In Julia, instead of returning a
   string as in C, it returns a tuple of integer values, containing
   the major and the minor number.

"),

("GLPK","GLPK","free_env","free_env()

   Frees all resources used by GLPK routines (memory blocks, etc.)
   which are currently still in use. Not normally needed.

   Returns 0 if successful, 1 if envirnoment is inactive.

"),

("GLPK","GLPK","term_out","term_out(flag)

   Enables/disables the terminal output of glpk routines. \"flag\" is
   either \"GLPK.ON\" (output enabled) or \"GLPK.OFF\" (output
   disabled).

   Returns the previous status of the terminal output.

"),

("GLPK","GLPK","open_tee","open_tee(filename)

   Starts copying all the terminal output to an output text file.

   Returns 0 if successful, 1 if already active, 2 if it fails
   creating the output file.

"),

("GLPK","GLPK","close_tee","close_tee()

   Stops copying the terminal output to the output text file
   previously open by the \"open_tee\".

   Return 0 if successful, 1 if copying terminal output was not
   started.

"),

("GLPK","GLPK","malloc","malloc(size)

   Replacement of standard C \"malloc\". Allocates uninitialized
   memeory which must freed with \"free\".

   Returns a pointer to the allocated memory.

"),

("GLPK","GLPK","calloc","calloc(n, size)

   Replacement of standard C \"calloc\", but does not initialize the
   memeory. Allocates uninitialized memeory which must freed with
   \"free\".

   Returns a pointer to the allocated memory.

"),

("GLPK","GLPK","free","free(ptr)

   Deallocates a memory block previously allocated by \"malloc\" or
   \"calloc\".

"),

("GLPK","GLPK","mem_usage","mem_usage()

   Reports some information about utilization of the memory by the
   routines \"malloc\", \"calloc\", and \"free\". In Julia, this
   function has a different API then C. It returns \"(count, cpeak,
   total, tpeak)\" rather then taking them as pointers in the argument
   list.

"),

("GLPK","GLPK","mem_limit","mem_limit(limit)

   Limits the amount of memory avaliable for dynamic allocation to a
   value in megabyes given by the integer parameter \"limit\".

"),

("GLPK","GLPK","time","time()

   Returns the current universal time (UTC), in milliseconds.

"),

("GLPK","GLPK","difftime","difftime(t1, t0)

   Returns the difference between two time values \"t1\" and \"t0\",
   expressed in seconds.

"),

("GLPK","GLPK","sdf_open_file","sdf_open_file(filename)

   Opens a plain data file.

   If successful, returns a \"GLPK.Data\" object, otherwise throws an
   error.

"),

("GLPK","GLPK","sdf_read_int","sdf_read_int(glp_data)

   Reads an integer number from the plain data file specified by the
   \"GLPK.Data\" parameter \"glp_data\", skipping initial whitespace.

"),

("GLPK","GLPK","sdf_read_num","sdf_read_num(glp_data)

   Reads a floating point number from the plain data file specified by
   the \"GLPK.Data\" parameter \"glp_data\", skipping initial
   whitespace.

"),

("GLPK","GLPK","sdf_read_item","sdf_read_item(glp_data)

   Reads a data item (a String) from the plain data file specified by
   the \"GLPK.Data\" parameter \"glp_data\", skipping initial
   whitespace.

"),

("GLPK","GLPK","sdf_read_text","sdf_read_text(glp_data)

   Reads a line of text from the plain data file specified by the
   \"GLPK.Data\" parameter \"glp_data\", skipping initial and final
   whitespace.

"),

("GLPK","GLPK","sdf_line","sdf_line(glp_data)

   Returns the current line in the \"GLPK.Data\" object \"glp_data\"

"),

("GLPK","GLPK","sdf_close_file","sdf_close_file(glp_data)

   Closes the file associated to \"glp_data\" and frees the resources.

"),

("GLPK","GLPK","read_cnfsat","read_cnfsat(glp_prob, filename)

   Reads the CNF-SAT problem data in DIMACS format from a text file.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","check_cnfsat","check_cnfsat(glp_prob)

   Checks if the problem object encodes a CNF-SAT problem instance, in
   which case it returns 0, otherwise returns non-zero.

"),

("GLPK","GLPK","write_cnfsat","write_cnfsat(glp_prob, filename)

   Writes the CNF-SAT problem data in DIMACS format into a text file.

   Returns 0 upon success; throws an error in case of failure.

"),

("GLPK","GLPK","minisat1","minisat1(glp_prob)

   The routine \"minisat1\" is a driver to MiniSat, a CNF-SAT solver
   developed by Niklas Eén and Niklas Sörensson, Chalmers University
   of Technology, Sweden.

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: \"GLPK.EDATA\" (problem is not CNF-SAT),
   \"GLPK.EFAIL\" (solver failure).

"),

("GLPK","GLPK","intfeas1","intfeas1(glp_prob, use_bound, obj_bound)

   The routine \"glp_intfeas1\" is a tentative implementation of an
   integer feasibility solver based on a CNF-SAT solver (currently
   MiniSat). \"use_bound\" is a flag: if zero, any feasible solution
   is seeked, otherwise seraches for an integer feasible solution.
   \"obj_bound\" is used only if \"use_bound\" is non-zero, and
   specifies an upper/lower bound (for maximization/minimazion
   respectively) to the objective function.

   All variables (columns) must either be binary or fixed. All
   constraint and objective coeffient must be integer.

   Returns 0 in case of success, or a non-zero flag specifying the
   reason for failure: \"GLPK.EDATA\" (problem data is not valid),
   \"GLPK.ERANGE\" (integer overflow occurred), \"GLPK.EFAIL\" (solver
   failure).

"),

("GZip","GZip","gzopen","gzopen(fname[, gzmode[, buf_size]])

   Opens a file with mode (default \"\"r\"\"), setting internal buffer
   size to buf_size (default \"Z_DEFAULT_BUFSIZE=8192\"), and returns
   a the file as a \"GZipStream\".

   \"gzmode\" must contain one of

   +------+-----------------------------------+
   | r    | read                              |
   +------+-----------------------------------+
   | w    | write, create, truncate           |
   +------+-----------------------------------+
   | a    | write, create, append             |
   +------+-----------------------------------+

   In addition, gzmode may also contain

   +-------+-----------------------------------+
   | x     | create the file exclusively       |
   +-------+-----------------------------------+
   | 0-9   | compression level                 |
   +-------+-----------------------------------+

   and/or a compression strategy:

   +------+-----------------------------------+
   | f    | filtered data                     |
   +------+-----------------------------------+
   | h    | Huffman-only compression          |
   +------+-----------------------------------+
   | R    | run-length encoding               |
   +------+-----------------------------------+
   | F    | fixed code compression            |
   +------+-----------------------------------+

   Note that \"+\" is not allowed in gzmode.

   If an error occurs, \"gzopen\" throws a \"GZError\"

"),

("GZip","GZip","gzdopen","gzdopen(fd[, gzmode[, buf_size]])

   Create a \"GZipStream\" object from an integer file descriptor. See
   \"gzopen()\" for \"gzmode\" and \"buf_size\" descriptions.

"),

("GZip","GZip","gzdopen","gzdopen(s[, gzmode[, buf_size]])

   Create a \"GZipStream\" object from \"IOStream\" \"s\".

"),

("GZip","GZip","GZipStream","type GZipStream(name, gz_file[, buf_size[, fd[, s]]])

   Subtype of \"IO\" which wraps a gzip stream.  Returned by
   \"gzopen()\" and \"gzdopen()\".

"),

("GZip","GZip","GZError","type GZError(err, err_str)

   gzip error number and string.  Possible error values:

   +-----------------------+------------------------------------------+
   | \\\"Z_OK\\\"              | No error                                 |
   +-----------------------+------------------------------------------+
   | \\\"Z_ERRNO\\\"           | Filesystem error (consult \\\"errno()\\\")   |
   +-----------------------+------------------------------------------+
   | \\\"Z_STREAM_ERROR\\\"    | Inconsistent stream state                |
   +-----------------------+------------------------------------------+
   | \\\"Z_DATA_ERROR\\\"      | Compressed data error                    |
   +-----------------------+------------------------------------------+
   | \\\"Z_MEM_ERROR\\\"       | Out of memory                            |
   +-----------------------+------------------------------------------+
   | \\\"Z_BUF_ERROR\\\"       | Input buffer full/output buffer empty    |
   +-----------------------+------------------------------------------+
   | \\\"Z_VERSION_ERROR\\\"   | zlib library version is incompatible     |
   +-----------------------+------------------------------------------+

"),


("OptionsMod","OptionsMod","@options","@options([check_flag], assignments...)

   Use the \"@options\" macro to set the value of optional parameters
   for a function that has been written to use them (see
   \"defaults()\" to learn how to write such functions).  The syntax
   is:

      opts = @options a=5 b=7

   For a function that uses optional parameters \"a\" and \"b\", this
   will override the default settings for these parameters. You would
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

("OptionsMod","OptionsMod","@set_options","@set_options(opts, assigments...)

   The \"@set_options\" macro lets you add new parameters to an
   existing options structure.  For example:

      @set_options opts d=99

   would add \"d\" to the set of parameters in \"opts\", or re-set its
   value if it was already supplied.

"),

("OptionsMod","OptionsMod","@defaults","@defaults(opts, assignments...)

   The \"@defaults\" macro is for writing functions that take optional
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

   Note the function calls \"@check_used()\" at the end.

   It is possible to have more than one Options parameter to a
   function, for example:

      function twinopts(x, plotopts::Options, calcopts::Options)
          @defaults plotopts linewidth=1
          @defaults calcopts n_iter=100
          # Do stuff
          @check_used plotopts
          @check_used calcopts
      end

   Within a given scope, you should only have one call to
   \"@defaults\" per options variable.

"),

("OptionsMod","OptionsMod","@check_used","@check_used(opts)

   The \"@check_used\" macro tests whether user-supplied parameters
   were ever accessed by the \"@defaults()\" macro. The test is
   performed at the end of the function body, so that subfunction
   handling parameters not used by the parent function may be
   \"credited\" for their usage. Each sub-function should also call
   \"@check_used\", for example:

      function complexfun(x, opts::Options)
          @defaults opts parent=3 both=7
          println(parent)
          println(both)
          subfun1(x, opts)
          subfun2(x, opts)
          @check_used opts
      end

      function subfun1(x, opts::Options)
          @defaults opts sub1=\"sub1 default\" both=0
          println(sub1)
          println(both)
          @check_used opts
      end

      function subfun2(x, opts::Options)
          @defaults opts sub2=\"sub2 default\" both=22
          println(sub2)
          println(both)
          @check_used opts
      end

"),

("OptionsMod","OptionsMod","Options","type Options(OptionsChecking, param1, val1, param2, val2, ...)

   \"Options\" is the central type used for handling optional
   arguments. Its fields are briefly described below.

   key2index

      A \"Dict\" that looks up an integer index, given the symbol for
      a variable (e.g., \"key2index[:a]\" for the variable \"a\")

   vals

      \"vals[key2index[:a]]\" is the value to be assigned to the
      variable \"a\"

   used

      A vector of booleans, one per variable, with
      \"used[key2index[:a]]\" representing the value for variable
      \"a\". These all start as \"false\", but access by a
      \"@defaults\" command sets the corresponding value to \"true\".
      This marks the variable as having been used in the function.

   check_lock

      A vector of booleans, one per variable. This is a \"lock\" that
      prevents sub-functions from complaining that they did not access
      variables that were intended for the parent function.
      \"@defaults()\" sets the lock to true for any options variables
      that have already been defined; new variables added through
      \"@set_options()\" will start with their \"check_lock\" set to
      \"false\", to be handled by a subfunction.

"),

("profile.jl","","@profile","@profile()

   Profiling is controlled via the \"@profile\" macro. Your first step
   is to determine which code you want to profile and encapsulate it
   inside a \"@profile begin ... end\" block, like this:

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

   To view the execution times, type \"@profile report\".

   Here are the various options you have for controlling profiling:

   * \"@profile report\": display cumulative profiling results

   * \"@profile clear\": clear all timings accumulated thus far (start
     from zero)

   * \"@profile off\": turn profiling off (there is no need to remove
     \"@profile begin ... end\" blocks)

   * \"@profile on\": turn profiling back on

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

("Sound","Sound","wavread","wavread(io[, options])

   Reads and returns the samples from a RIFF/WAVE file. The samples
   are converted to floating point values in the range from -1.0 to
   1.0 by default. The \"io\" argument accepts either an \"IO\" object
   or a filename (\"String\"). The options are passed via an
   \"Options\" object (see the \"OptionsMod\" module).

   The available options, and the default values, are:

   * \"format\" (default = \"double\"): changes the format of the
     returned samples. The string \"double\" returns double precision
     floating point values in the range -1.0 to 1.0. The string
     \"native\" returns the values as encoded in the file. The string
     \"size\" returns the number of samples in the file, rather than
     the actual samples.

   * \"subrange\" (default = \"Any\"): controls which samples are
     returned. The default, \"Any\" returns all of the samples.
     Passing a number (\"Real\"), \"N\", will return the first \"N\"
     samples of each channel. Passing a range (\"Range1{Real}\"),
     \"R\", will return the samples in that range of each channel.

   The returned values are:

   * \"y\": The acoustic samples; A matrix is returned for files that
     contain multiple channels.

   * \"Fs\": The sampling frequency

   * \"nbits\": The number of bits used to encode each sample

   * \"extra\": Any additional bytes used to encode the samples (is
     always \"None\")

   The following functions are also defined to make this function
   compatible with MATLAB:

      wavread(filename::String) = wavread(filename, @options)
      wavread(filename::String, fmt::String) = wavread(filename, @options format=fmt)
      wavread(filename::String, N::Int) = wavread(filename, @options subrange=N)
      wavread(filename::String, N::Range1{Int}) = wavread(filename, @options subrange=N)
      wavread(filename::String, N::Int, fmt::String) = wavread(filename, @options subrange=N format=fmt)
      wavread(filename::String, N::Range1{Int}, fmt::String) = wavread(filename, @options subrange=N format=fmt)

"),

("Sound","Sound","wavwrite","wavwrite(samples, io[, options])

      Writes samples to a RIFF/WAVE file io object. The \"io\"
      argument accepts either an \"IO\" object or a filename
      (\"String\"). The function assumes that the sample rate is 8 kHz
      and uses 16 bits to encode each sample. Both of these values can
      be changed with the options parameter. Each column of the data
      represents a different channel. Stereo files should contain two
      columns. The options are passed via an \"Options\" object (see
      the \"OptionsMod\" module).

      The available options, and the default values, are:

   * \"sample_rate\" (default = \"8000\"): sampling frequency

   * \"nbits\" (default = \"16\"): number of bits used to encode each
     sample

   * \"compression\" (default = \"WAVE_FORMAT_PCM\"): The desired
     compression technique; accepted values are: WAVE_FORMAT_PCM,
     WAVE_FORMAT_IEEE_FLOAT

   The type of the input array, samples, also affects the generated
   file. \"Native\" WAVE files are written when integers are passed
   into wavwrite. This means that the literal values are written into
   the file. The input ranges are as follows for integer samples.

   +--------+-------------+------------------------+---------------+
   | N Bits | y Data Type | y Data Range           | Output Format |
   +========+=============+========================+===============+
   | 8      | uint8       | 0 <= y <= 255          | uint8         |
   +--------+-------------+------------------------+---------------+
   | 16     | int16       | –32768 <= y <= +32767  | int16         |
   +--------+-------------+------------------------+---------------+
   | 24     | int32       | –2^23 <= y <= 2^23 – 1 | int32         |
   +--------+-------------+------------------------+---------------+

   If samples contains floating point values, the input data ranges
   are the following.

   +--------+------------------+-------------------+---------------+
   | N Bits | y Data Type      | y Data Range      | Output Format |
   +========+==================+===================+===============+
   | 8      | single or double | –1.0 <= y < +1.0  | uint8         |
   +--------+------------------+-------------------+---------------+
   | 16     | single or double | –1.0 <= y < +1.0  | int16         |
   +--------+------------------+-------------------+---------------+
   | 24     | single or double | –1.0 <= y < +1.0  | int32         |
   +--------+------------------+-------------------+---------------+
   | 32     | single or double | –1.0 <= y <= +1.0 | single        |
   +--------+------------------+-------------------+---------------+

   The following functions are also defined to make this function
   compatible with MATLAB:

      wavwrite(y::Array) = wavwrite(y, @options)
      wavwrite(y::Array, Fs::Real, filename::String) = wavwrite(y, filename, @options sample_rate=Fs)
      wavwrite(y::Array, Fs::Real, N::Real, filename::String) = wavwrite(y, filename, @options sample_rate=Fs nbits=N)

"),

("strpack.jl","","pack","pack(io, composite[, strategy])

   Create a packed buffer representation of \"composite\" in stream
   \"io\", using data alignment coded by \"strategy\". This buffer is
   suitable to pass as a \"struct\" argument in a \"ccall\".

"),

("strpack.jl","","unpack","unpack(io, T[, strategy])

   Extract an instance of the Julia composite type \"T\" from the
   packed representation in the stream \"io\". \"io\" must be
   positioned at the beginning (using \"seek\"). This allows you to
   read C \"struct\" outputs from \"ccall\".

"),

("TextWrap","TextWrap","wrap","wrap(string[, options])

   Returns a string in which newlines are inserted as appropriate in
   order for each line to fit within a specified width.

   The options are passed via an \"Options\" object (provided by the
   \"OptionsMod\" module). The available options, and their default
   values, are:

   * \"width\" (default = \"70\"): the maximum width of the wrapped
     text, including indentation.

   * \"initial_indent\" (default = \"\"\"\"): indentation of the first
     line. This can be any string (shorter than \"width\"), or it can
     be an integer number (lower than \"width\").

   * \"subsequent_indent\" (default = \"\"\"\"): indentation of all
     lines except the first. Works the same as \"initial_indent\".

   * \"break_on_hyphens\" (default = \"true\"): this flag determines
     whether words can be broken on hyphens, e.g. whether \"high-
     precision\" can be split into \"high-\" and \"precision\".

   * \"break_long_words\" (default = \"true\"): this flag determines
     what to do when a word is too long to fit in any line. If
     \"true\", the word will be broken, otherwise it will go beyond
     the desired text width.

   * \"replace_whitespace\" (default = \"true\"): if this flag is
     true, all whitespace characters in the original text (including
     newlines) will be replaced by spaces.

   * \"expand_tabs\" (default = \"true\"): if this flag is true, tabs
     will be expanded in-place into spaces. The expansion happens
     before whitespace replacement.

   * \"fix_sentence_endings\" (default = \"false\"): if this flag is
     true, the wrapper will try to recognize sentence endings in the
     middle of a paragraph and put two spaces before the next sentence
     in case only one is present.

"),

("TextWrap","TextWrap","println_wrapped","print_wrapped(text...[, options])
print_wrapped(io, text...[, options])
println_wrapped(text...[, options])
println_wrapped(io, text...[, options])

   These are just like the standard \"print()\" and \"println()\"
   functions (they print multiple arguments and accept an optional
   \"IO\" first argument), except that they wrap the result, and
   accept an optional last argument with the options to pass to
   \"wrap()\".

"),

("Zlib","Zlib","compress_bound","compress_bound(input_size)

   Returns the maximum size of the compressed output buffer for a
   given uncompressed input size.

"),

("Zlib","Zlib","compress","compress(source[, level])

   Compresses source using the given compression level, and returns
   the compressed buffer (\"Array{Uint8,1}\").  \"level\" is an
   integer between 0 and 9, or one of \"Z_NO_COMPRESSION\",
   \"Z_BEST_SPEED\", \"Z_BEST_COMPRESSION\", or
   \"Z_DEFAULT_COMPRESSION\".  It defaults to
   \"Z_DEFAULT_COMPRESSION\".

   If an error occurs, \"compress\" throws a \"ZError\" with more
   information about the error.

"),

("Zlib","Zlib","compress_to_buffer","compress_to_buffer(source, dest, level=Z_DEFAULT_COMPRESSION)

   Compresses the source buffer into the destination buffer, and
   returns the number of bytes written into dest.

   If an error occurs, \"uncompress\" throws a \"ZError\" with more
   information about the error.

"),

("Zlib","Zlib","uncompress","uncompress(source[, uncompressed_size])

   Allocates a buffer of size \"uncompressed_size\", uncompresses
   source to this buffer using the given compression level, and
   returns the compressed buffer.  If \"uncompressed_size\" is not
   given, the size of the output buffer is estimated as
   \"2*length(source)\".  If the uncompressed_size is larger than
   uncompressed_size, the allocated buffer is grown and the
   uncompression is retried.

   If an error occurs, \"uncompress\" throws a \"ZError\" with more
   information about the error.

"),

("Zlib","Zlib","uncompress_to_buffer","uncompress_to_buffer(source, dest)

   Uncompresses the source buffer into the destination buffer. Returns
   the number of bytes written into dest.  An error is thrown if the
   destination buffer does not have enough space.

   If an error occurs, \"uncompress_to_buffer\" throws a \"ZError\"
   with more information about the error.

"),


}
