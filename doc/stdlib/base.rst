.. currentmodule:: Base

************
 Essentials
************

Introduction
------------

The Julia standard library contains a range of functions and macros appropriate for performing scientific and numerical computing, but is also as broad as those of many general purpose programming languages.  Additional functionality is available from a growing collection of available packages. Functions are grouped by topic below.

Some general notes:

* Except for functions in built-in modules (:mod:`~Base.Pkg`, :mod:`~Base.Collections`,
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

.. function:: atreplinit(f)

   Register a one-argument function to be called before the REPL interface is initialized in interactive sessions; this is useful to customize the interface. The argument of ``f`` is the REPL object.
   This function should be called from within the ``.juliarc.jl`` initialization file.

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

   Returns the method of ``f`` (a ``Method`` object) that would be called for arguments of the given types.

   If ``types`` is an abstract type, then the method that would be called by ``invoke``
   is returned.

.. function:: which(symbol)

   Return the module in which the binding for the variable referenced
   by ``symbol`` was created.

.. function:: @which

   Applied to a function call, it evaluates the arguments to the
   specified function call, and returns the ``Method`` object for the
   method that would be called for those arguments.  Applied to a
   variable, it returns the module in which the variable was bound. It
   calls out to the ``which`` function.

.. function:: methods(f, [types])

   Returns the method table for ``f``.

   If ``types`` is specified, returns an array of methods whose types match.

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

.. data:: ans

   A variable referring to the last computed value, automatically set at the
   interactive prompt.

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

   If ``T`` is an ``Integer`` type, an :exc:`InexactError` will be raised if
   ``x`` is not representable by ``T``, for example if ``x`` is not
   integer-valued, or is outside the range supported by ``T``.

   .. doctest::

      julia> convert(Int, 3.0)
      3

      julia> convert(Int, 3.5)
      ERROR: InexactError()
       in convert at int.jl:196

   If ``T`` is a :obj:`FloatingPoint` or :obj:`Rational` type, then it will return
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

      julia> structinfo(T) = [zip(fieldoffsets(T),fieldnames(T),T.types)...];

      julia> structinfo(StatStruct)
      12-element Array{Tuple{Int64,Symbol,DataType},1}:
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

.. function:: Union(Ts...)

   Construct a special abstract type that behaves as though all of the types in ``Ts`` are its subtypes.

.. function:: Val{c}

   Create a "value type" out of ``c``, which must be an ``isbits``
   value. The intent of this construct is to be able to dispatch on
   constants, e.g., ``f(Val{false})`` allows you to dispatch directly
   (at compile-time) to an implementation ``f(::Type{Val{false}})``,
   without having to test the boolean value at runtime.

.. function:: @enum EnumName EnumValue1[=x] EnumValue2[=y]

   Create an :obj:`Enum` type with name ``EnumName`` and enum member values of ``EnumValue1`` and ``EnumValue2`` with optional assigned values of ``x`` and ``y``, respectively. ``EnumName`` can be used just like other types and enum member values as regular values, such as

   .. doctest::

      julia> @enum FRUIT apple=1 orange=2 kiwi=3

      julia> f(x::FRUIT) = "I'm a FRUIT with value: $(int(x))"
      f (generic function with 1 method)

      julia> f(apple)
      "I'm a FRUIT with value: 1"

Generic Functions
-----------------

.. function:: method_exists(f, Tuple type) -> Bool

   Determine whether the given generic function has a method matching the given :obj:`Tuple` of argument types.

   .. doctest::

   	julia> method_exists(length, Tuple{Array})
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

      julia> [1:5;] |> x->x.^2 |> sum |> inv
      0.01818181818181818

.. function:: call(x, args...)

   If ``x`` is not a ``Function``, then ``x(args...)`` is equivalent to
   ``call(x, args...)``.  This means that function-like behavior can be
   added to any type by defining new ``call`` methods.

Syntax
------

.. function:: eval([m::Module], expr::Expr)

   Evaluate an expression in the given module and return the result.
   Every module (except those defined with ``baremodule``) has its own 1-argument definition
   of ``eval``, which evaluates expressions in that module.

.. function:: @eval

   Evaluate an expression and return the value.

.. function:: evalfile(path::AbstractString)

   Load the file using ``include``, evaluate all expressions, and return the value of the last one.

.. function:: esc(e::ANY)

   Only valid in the context of an Expr returned from a macro. Prevents the macro hygiene pass from turning embedded variables into gensym variables. See the :ref:`man-macros`
   section of the Metaprogramming chapter of the manual for more details and examples.

.. function:: gensym([tag])

   Generates a symbol which will not conflict with other variable names.

.. function:: @gensym

   Generates a gensym symbol for a variable. For example, ``@gensym x y`` is transformed into ``x = gensym("x"); y = gensym("y")``.

.. function:: parse(str, start; greedy=true, raise=true)

   Parse the expression string and return an expression (which could later be passed to eval for execution). Start is the index of the first character to start parsing. If ``greedy`` is true (default), ``parse`` will try to consume as much input as it can; otherwise, it will stop as soon as it has parsed a valid expression. Incomplete but otherwise syntactically valid expressions will return ``Expr(:incomplete, "(error message)")``. If ``raise`` is true (default), syntax errors other than incomplete expressions will raise an error. If ``raise`` is false, ``parse`` will return an expression that will raise an error upon evaluation.

.. function:: parse(str; raise=true)

   Parse the whole string greedily, returning a single expression.  An error is thrown if there are additional characters after the first expression. If ``raise`` is true (default), syntax errors will raise an error; otherwise, ``parse`` will return an expression that will raise an error upon evaluation.


Nullables
---------

.. function:: Nullable(x)

   Wrap value ``x`` in an object of type ``Nullable``, which indicates whether a value is present.
   ``Nullable(x)`` yields a non-empty wrapper, and ``Nullable{T}()`` yields an empty instance
   of a wrapper that might contain a value of type ``T``.

.. function:: get(x)

   Attempt to access the value of the ``Nullable`` object, ``x``. Returns the
   value if it is present; otherwise, throws a ``NullException``.

.. function:: get(x, y)

   Attempt to access the value of the ``Nullable{T}`` object, ``x``. Returns
   the value if it is present; otherwise, returns ``convert(T, y)``.

.. function:: isnull(x)

   Is the ``Nullable`` object ``x`` null, i.e. missing a value?


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

.. function:: Sys.set_process_title(title::AbstractString)

   Set the process title. No-op on some operating systems. (not exported)

.. function:: Sys.get_process_title()

   Get the process title. On some systems, will always return empty string. (not exported)

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

   Set environment variables to use when running the given
   command. ``env`` is either a dictionary mapping strings to strings,
   an array of strings of the form ``"var=val"``, or zero or more
   ``"var"=>val`` pair arguments.  In order to modify (rather than
   replace) the existing environment, create ``env`` by ``copy(ENV)``
   and then setting ``env["var"]=val`` as desired, or use ``withenv``.

   The ``dir`` keyword argument can be used to specify a working
   directory for the command.

.. function:: withenv(f::Function, kv::Pair...)

   Execute ``f()`` in an environment that is temporarily modified (not replaced as in ``setenv``) by zero or more ``"var"=>val`` arguments ``kv``.  ``withenv`` is generally used via the ``withenv(kv...) do ... end`` syntax.  A value of ``nothing`` can be used to temporarily unset an environment variable (if it is set).  When ``withenv`` returns, the original environment has been restored.

.. function:: pipe(from, to, ...)

   Create a pipeline from a data source to a destination. The source and destination can
   be commands, I/O streams, strings, or results of other ``pipe`` calls. At least one
   argument must be a command. Strings refer to filenames.
   When called with more than two arguments, they are chained together from left to right.
   For example ``pipe(a,b,c)`` is equivalent to ``pipe(pipe(a,b),c)``. This provides a more
   concise way to specify multi-stage pipelines.

   **Examples**:
     * ``run(pipe(`ls`, `grep xyz`))``
     * ``run(pipe(`ls`, "out.txt"))``
     * ``run(pipe("out.txt", `grep xyz`))``

.. function:: pipe(command; stdin, stdout, stderr, append=false)

   Redirect I/O to or from the given ``command``. Keyword arguments specify which of
   the command's streams should be redirected. ``append`` controls whether file output
   appends to the file.
   This is a more general version of the 2-argument ``pipe`` function.
   ``pipe(from, to)`` is equivalent to ``pipe(from, stdout=to)`` when ``from`` is a
   command, and to ``pipe(to, stdin=from)`` when ``from`` is another kind of
   data source.

   **Examples**:
     * ``run(pipe(`dothings`, stdout="out.txt", stderr="errs.txt"))``
     * ``run(pipe(`update`, stdout="log.txt", append=true))``

.. function:: gethostname() -> AbstractString

   Get the local machine's host name.

.. function:: getipaddr() -> AbstractString

   Get the IP address of the local machine, as a string of the form "x.x.x.x".

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

.. function:: @time

   A macro to execute an expression, printing the time it took to execute, the number of allocations, and the total number of bytes its execution caused to be allocated, before returning the value of the expression.

.. function:: @timev

   This is a verbose version of the ``@time`` macro, it first prints the same information as ``@time``, then any non-zero memory allocation counters, and then returns the value of the expression.

.. function:: @timed

   A macro to execute an expression, and return the value of the expression, elapsed time, total bytes allocated, garbage collection time, and an object with various memory allocation counters.

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


Errors
------

.. function:: error(message::AbstractString)

   Raise an ``ErrorException`` with the given message

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

.. function:: assert(cond)

   Throw an ``AssertionError`` if ``cond`` is false. Also available as the macro ``@assert expr``.

.. function:: @assert cond [text]

   Throw an ``AssertionError`` if ``cond`` is false. Preferred syntax for writing assertions.

.. function:: ArgumentError(msg)

   The parameters to a function call do not match a valid signature.

.. function:: AssertionError([msg])

   The asserted condition did not evalutate to ``true``.

.. function:: BoundsError([a],[i])

   An indexing operation into an array, ``a``, tried to access an out-of-bounds element, ``i``.

.. function:: DimensionMismatch([msg])

   The objects called do not have matching dimensionality.

.. function:: DivideError()

   Integer division was attempted with a denominator value of 0.

.. function:: DomainError()

   The arguments to a function or constructor are outside the valid domain.

.. function:: EOFError()

   No more data was available to read from a file or stream.

.. function:: ErrorException(msg)

   Generic error type. The error message, in the `.msg` field, may provide more specific details.

.. function:: InexactError()

   Type conversion cannot be done exactly.

.. function:: InterruptException()

   The process was stopped by a terminal interrupt (CTRL+C).

.. function:: KeyError(key)

   An indexing operation into an ``Associative`` (``Dict``) or ``Set`` like object tried to access or delete a non-existent element.

.. function:: LoadError(file::AbstractString, line::Int, error)

   An error occurred while `including`, `requiring`, or `using` a file. The error specifics should be available in the `.error` field.

.. function:: MethodError(f, args)

   A method with the required type signature does not exist in the given generic function.

.. function:: NullException()

   An attempted access to a ``Nullable`` with no defined value.

.. function:: OutOfMemoryError()

   An operation allocated too much memory for either the system or the garbage collector to handle properly.

.. function:: OverflowError()

   The result of an expression is too large for the specified type and will cause a wraparound.

.. function:: ParseError(msg)

   The expression passed to the `parse` function could not be interpreted as a valid Julia expression.

.. function:: ProcessExitedException()

   After a client Julia process has exited, further attempts to reference the dead child will throw this exception.

.. function:: StackOverflowError()

   The function call grew beyond the size of the call stack. This usually happens when a call recurses infinitely.

.. function:: SystemError(prefix::AbstractString, [errnum::Int32])

   A system call failed with an error code (in the ``errno`` global variable).

.. function:: TypeError(func::Symbol, context::AbstractString, expected::Type, got)

   A type assertion failure, or calling an intrinsic function with an incorrect argument type.

.. function:: UndefRefError()

   The item or field is not defined for the given object.

.. function:: UndefVarError(var::Symbol)

   A symbol in the current scope is not defined.

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

.. function:: nfields(x::DataType) -> Int

   Get the number of fields of a data type.

.. function:: fieldnames(x::DataType)

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

.. function:: functionloc(m::Method)

   Returns a tuple ``(filename,line)`` giving the location of a method definition.

Internals
---------

.. function:: gc()

   Perform garbage collection. This should not generally be used.

.. function:: gc_disable()

   Disable garbage collection. This should be used only with extreme
   caution, as it can cause memory use to grow without bound.
   Returns previous GC state.

.. function:: gc_enable()

   Re-enable garbage collection after calling :func:`gc_disable`. Returns previous GC state.

.. function:: macroexpand(x)

   Takes the expression x and returns an equivalent expression with all macros removed (expanded).

.. function:: expand(x)

   Takes the expression x and returns an equivalent expression in lowered form

.. function:: code_lowered(f, types)

   Returns an array of lowered ASTs for the methods matching the given generic function and type signature.

.. function:: @code_lowered

   Evaluates the arguments to the function call, determines their types, and calls :func:`code_lowered` on the resulting expression

.. function:: code_typed(f, types; optimize=true)

   Returns an array of lowered and type-inferred ASTs for the methods matching the given generic function and type signature. The keyword argument ``optimize`` controls whether additional optimizations, such as inlining, are also applied.

.. function:: @code_typed

   Evaluates the arguments to the function call, determines their types, and calls :func:`code_typed` on the resulting expression

.. function:: code_warntype(f, types)

   Displays lowered and type-inferred ASTs for the methods matching the given generic function and type signature. The ASTs are annotated in such a way as to cause "non-leaf" types to be emphasized (if color is available, displayed in red). This serves as a warning of potential type instability. Not all non-leaf types are particularly problematic for performance, so the results need to be used judiciously. See :ref:`man-code-warntype` for more information.

.. function:: @code_warntype

   Evaluates the arguments to the function call, determines their types, and calls :func:`code_warntype` on the resulting expression

.. function:: code_llvm(f, types)

   Prints the LLVM bitcodes generated for running the method matching the given generic function and type signature to :const:`STDOUT`.

   All metadata and dbg.* calls are removed from the printed bitcode. Use code_llvm_raw for the full IR.

.. function:: @code_llvm

   Evaluates the arguments to the function call, determines their types, and calls :func:`code_llvm` on the resulting expression

.. function:: code_native(f, types)

   Prints the native assembly instructions generated for running the method matching the given generic function and type signature to STDOUT.

.. function:: @code_native

   Evaluates the arguments to the function call, determines their types, and calls :func:`code_native` on the resulting expression

.. function:: precompile(f,args::Tuple{Vararg{Any}})

   Compile the given function ``f`` for the argument tuple (of types) ``args``, but do not execute it.
