Julia v0.7.0 Release Notes
==========================

New language features
---------------------

  * Local variables can be tested for being defined
    using the new `@isdefined variable` macro ([#22281]).

  * Destructuring in function arguments: when an expression such as `(x, y)` is used as
    a function argument name, the argument is unpacked into local variables `x` and `y`
    as in the assignment `(x, y) = arg` ([#6614]).

  * Named tuples, with the syntax `(a=1, b=2)`. These behave very similarly to tuples,
    except components can also be accessed by name using dot syntax `t.a` ([#22194]).

  * Keyword argument containers (`kw` in `f(; kw...)`) are now named tuples. Dictionary
    functions like `haskey` and indexing can be used on them, and name-value pairs can be
    iterated using `pairs(kw)`. `kw` can no longer contain multiple entries for the same
    argument name ([#4916]).

  * Custom infix operators can now be defined by appending Unicode
    combining marks, primes, and sub/superscripts to other operators.
    For example, `+̂ₐ″` is parsed as an infix operator with the same
    precedence as `+` ([#22089]).

  * The macro call syntax `@macroname[args]` is now available and is parsed
    as `@macroname([args])` ([#23519]).

  * The construct `if @generated ...; else ...; end` can be used to provide both
    `@generated` and normal implementations of part of a function. Surrounding code
    will be common to both versions ([#23168]).

  * Added `⟂` (`\perp`) operator with comparison precedence ([#24404]).

  * The `missing` singleton object (of type `Missing`) has been added to represent
    missing values ([#24653]). It propagates through standard operators and mathematical functions,
    and implements three-valued logic, similar to SQLs `NULL` and R's `NA`.

  * Field access via dot-syntax can now be overloaded by adding methods to
    `Base.getproperty` and `Base.setproperty!` ([#1974]), optionally along with
    a corresponding `Base.propertynames` method for reflection ([#25311]).

  * Values for `Enum`s can now be specified inside of a `begin` block when using the
    `@enum` macro ([#25424]).

  * Keyword arguments can be required: if a default value is omitted, then an
    exception is thrown if the caller does not assign the keyword a value ([#25830]).

Language changes
----------------

  * The syntax for parametric methods, `function f{T}(x::T)`, has been
    changed to `function f(x::T) where {T}` ([#11310]).

  * The fallback constructor that calls `convert` is deprecated. Instead, new types should
    prefer to define constructors, and add `convert` methods that call those constructors
    only as necessary ([#15120]).

  * The syntax `1.+2` is deprecated, since it is ambiguous: it could mean either
    `1 .+ 2` (the current meaning) or `1. + 2` ([#19089]).

  * Mutable structs with no fields are no longer singletons; it is now possible to make
    multiple instances of them that can be distinguished by `===` ([#25854]).
    Zero-size immutable structs are still singletons.

  * In string and character literals, backslash `\` may no longer
    precede unrecognized escape characters ([#22800]).

  * Juxtaposing binary, octal, and hexadecimal literals is deprecated, since it can lead to
    confusing code such as `0xapi == 0xa * pi` ([#16356]).

  * Declaring arguments as `x::ANY` to avoid specialization has been replaced
    by `@nospecialize x`. ([#22666]).

  * Keyword argument default values are now evaluated in successive scopes ---
    the scope for each expression includes only previous keyword arguments, in
    left-to-right order ([#17240]).

  * The parsing of `1<<2*3` as `1<<(2*3)` is deprecated, and will change to
    `(1<<2)*3` in a future version ([#13079]).

  * The parsing of `<|` is now right associative. `|>` remains left associative ([#24153]).

  * `:` now parses like other operators, as a call to a function named `:`, instead of
    calling `colon` ([#25947]).

  * `{ }` expressions now use `braces` and `bracescat` as expression heads instead
    of `cell1d` and `cell2d`, and parse similarly to `vect` and `vcat` ([#8470]).

  * Nested `if` expressions that arise from the keyword `elseif` now use `elseif`
    as their expression head instead of `if` ([#21774]).

  * `let` blocks now parse the same as `for` loops; the first argument is either an
    assignment or `block` of assignments, and the second argument is a block of
    statements ([#21774]).

  * `do` syntax now parses to an expression with head `:do`, instead of as a function
    call ([#21774]).

  * Parsed and lowered forms of type definitions have been synchronized with their
    new keywords ([#23157]). Expression heads are renamed as follows:

    + `type`           => `struct`

    + `bitstype`       => `primitive` (order of arguments is also reversed, to match syntax)

    + `composite_type` => `struct_type`

    + `bits_type`      => `primitive_type`

  * The `global` keyword now only introduces a new binding if one doesn't already exist
    in the module.
    This means that assignment to a global (`global sin = 3`) may now throw the error:
    "cannot assign variable Base.sin from module Main", rather than emitting a warning.
    Additionally, the new bindings are now created before the statement is executed.
    For example, `f() = (global sin = "gluttony"; nothing)` will now resolve which module
    contains `sin` eagerly, rather than delaying that decision until `f` is run. ([#22984]).

  * `global const` declarations may no longer appear inside functions ([#12010]).

  * Uninitialized `BitArray` constructors of the form `BitArray[{N}](shape...)` have been
    deprecated in favor of equivalents accepting `undef` (an alias for
    `UndefInitializer()`) as their first argument, as in
    `BitArray[{N}](undef, shape...)`. For example, `BitVector(3)` is now
    `BitVector(undef, 3)`, `BitMatrix((2, 4))` is now
    `BitMatrix(undef, (2, 4))`, and `BitArray{3}(11, 13, 17)` is now
    `BitArray{3}(undef, 11, 14, 17)` ([#24785]).

  * Dispatch rules have been simplified:
    method matching is now determined exclusively by subtyping;
    the rule that method type parameters must also be captured has been removed.
    Instead, attempting to access the unconstrained parameters will throw an `UndefVarError`.
    Linting in package tests is recommended to confirm that the set of methods
    which might throw `UndefVarError` when accessing the static parameters
    (`need_to_handle_undef_sparam = Set{Any}(m.sig for m in Test.detect_unbound_args(Base, recursive=true))`)
    is equal (`==`) to some known set (`expected = Set()`). ([#23117])

  * `const` declarations on local variables were previously ignored. They now give a
    warning, so that this syntax can be disallowed or given a new meaning in a
    future version ([#5148]).

  * Placing an expression after `catch`, as in `catch f(x)`, is deprecated.
    Use `catch; f(x)` instead ([#19987]).

  * In `for i = ...`, if a local variable `i` already existed it would be overwritten
    during the loop. This behavior is deprecated, and in the future `for` loop variables
    will always be new variables local to the loop ([#22314]).
    The old behavior of overwriting an existing variable is available via `for outer i = ...`.

  * In `for i in x`, `x` used to be evaluated in a new scope enclosing the `for` loop.
    Now it is evaluated in the scope outside the `for` loop.

  * In `for i in x, j in y`, all variables now have fresh bindings on each iteration of the
    innermost loop. For example, an assignment to `i` will not be visible on the next `j`
    loop iteration ([#330]).

  * Variable bindings local to `while` loop bodies are now freshly allocated on each loop iteration,
    matching the behavior of `for` loops.

  * Prefix `&` for by-reference arguments to `ccall` has been deprecated in favor of
    `Ref` argument types ([#6080]).

  * The constructor `Ref(x::T)` now always returns a `Ref{T}` ([#21527]).

  * All line numbers in ASTs are represented by `LineNumberNode`s; the `:line` expression
    head is no longer used. `QuoteNode`s are also consistently used for quoted symbols instead
    of the `:quote` expression head (though `:quote` `Expr`s are still used for quoted
    expressions) ([#23885]).

  * The `+` and `-` methods for `Number` and `UniformScaling` are not ambiguous anymore since `+`
    and `-` no longer do automatic broadcasting. Hence, the methods for `UniformScaling` and `Number` are
    no longer deprecated ([#23923]).

  * The keyword `importall` is deprecated. Use `using` and/or individual `import` statements
    instead ([#22789]).

  * `reduce(+, [...])` and `reduce(*, [...])` no longer widen the iterated over arguments to
    system word size. `sum` and `prod` still preserve this behavior. ([#22825])

  * Like `_`, variable names consisting only of underscores can be assigned,
    but accessing their values is deprecated ([#24221]).

  * Raw string literal escaping rules have been changed to make it possible to write all strings.
    The rule is that backslashes escape both quotes and other backslashes, but only when a sequence
    of backslashes precedes a quote character. Thus, 2n backslashes followed by a quote encodes n
    backslashes and the end of the literal while 2n+1 backslashes followed by a quote encodes n
    backslashes followed by a quote character ([#22926]).

  * `reprmime(mime, x)` has been renamed to `repr(mime, x)`, and along with `repr(x)`
    and `sprint` it now accepts an optional `context` keyword for `IOContext` attributes.
    `stringmime` has been moved to the Base64 stdlib package ([#25990]).

  * The syntax `(x...)` for constructing a tuple is deprecated; use `(x...,)` instead ([#24452]).

  * Non-parenthesized interpolated variables in strings, e.g. `"$x"`, must be followed
    by a character that will never be an allowed identifier character (currently
    operators, space/control characters, or common punctuation characters) ([#25231]).

  * The syntax `using A.B` can now only be used when `A.B` is a module, and the syntax
    `using A: B` can only be used for adding single bindings ([#8000]).

  * `=>` now has its own precedence level, giving it strictly higher precedence than
    `=` and `,` ([#25391]).

  * The conditions under which unary operators followed by `(` are parsed as prefix function
    calls have changed ([#26154]).

  * `begin` is disallowed inside indexing expressions, in order to enable the syntax
    `a[begin]` (for selecting the first element) in the future ([#23354]).

  * Underscores for `_italics_` and `__bold__` are now supported by the Base Markdown
    parser. ([#25564])

  * `…` (`\dots`) and `⁝` (`\tricolon`) are now parsed as binary operators ([#26262]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * `replace(s::AbstractString, pat=>repl)` for function `repl` arguments formerly
    passed a substring to `repl` in all cases.  It now passes substrings for
    string patterns `pat`, but a `Char` for character patterns (when `pat` is a
    `Char`, collection of `Char`, or a character predicate) ([#25815]).

  * `readuntil` now does *not* include the delimiter in its result, matching the
    behavior of `readline`. Pass `keep=true` to get the old behavior ([#25633]).

  * `countlines` now always counts the last non-empty line even if it does not
    end with EOL, matching the behavior of `eachline` and `readlines` ([#25845]).

  * `getindex(s::String, r::UnitRange{Int})` now throws `UnicodeError` if `last(r)`
    is not a valid index into `s` ([#22572]).

  * `ntuple(f, n::Integer)` throws `ArgumentError` if `n` is negative.
    Previously an empty tuple was returned ([#21697]).

  * `⋮`, `⋱`, `⋰`, and `⋯` are now parsed as binary operators, not ordinary
    identifiers.  `≔`, `≕`, and `⩴` now parse with assignment rather than comparison
    precedence ([#26262]).

  * Juxtaposing string literals (e.g. `"x"y`) is now a syntax error ([#20575]).

  * `finalizer(function, object)` now returns `object` rather than `nothing` ([#24679]).

  * The constructor of `SubString` now checks if the requested view range
    is defined by valid indices in the parent `AbstractString` ([#22511]).

  * Macro calls with `for` expressions are now parsed as generators inside
    function argument lists ([#18650]). Examples:

    + `sum(@inbounds a[i] for i = 1:n)` used to give a syntax error, but is now
      parsed as `sum(@inbounds(a[i]) for i = 1:n)`.

    + `sum(@m x for i = 1:n end)` used to parse the argument to `sum` as a 2-argument
      call to macro `@m`, but now parses it as a generator plus a syntax error
      for the dangling `end`.

  * `@__DIR__` returns the current working directory rather than `nothing` when not run
    from a file ([#21759]).

  * `@__FILE__` and `@__DIR__` return information relative to the file that it was parsed from,
    rather than from the task-local `SOURCE_PATH` global when it was expanded.

  * All macros receive an extra argument `__source__::LineNumberNode` which describes the
    parser location in the source file for the `@` of the macro call.
    It can be accessed as a normal argument variable in the body of the macro.
    This is implemented by inserting an extra leading argument into the
    `Expr(:macrocall, :@name, LineNumberNode(...), args...)`
    surface syntax. ([#21746])

  * Passing the same keyword argument multiple times is now a syntax error ([#16937]).

  * `getsockname` on a `TCPSocket` now returns the locally bound address and port
    of the socket. Previously the address of the remote endpoint was being
    returned ([#21825]).

  * The `~/.juliarc.jl` file has been moved to `~/.julia/config/startup.jl` and
    `/etc/julia/juliarc.jl` file has been renamed to `/etc/julia/startup.jl` ([#26161]).

  * Using `ARGS` within `startup.jl` files or within a .jl file loaded with `--load` will no
    longer contain the script name as the first argument. Instead, the script name will be
    assigned to `PROGRAM_FILE`. ([#22092])

  * The format for a `ClusterManager` specifying the cookie on the command line is now
    `--worker=<cookie>`. `--worker <cookie>` will not work as it is now an optional argument.

  * The representation of `CartesianRange` has changed to a
    tuple-of-AbstractUnitRanges; the `start` and `stop` fields are no
    longer present. Use `first(R)` and `last(R)` to obtain
    start/stop. ([#20974])

  * The `Diagonal`, `Bidiagonal`, `Tridiagonal` and `SymTridiagonal` type definitions have
    changed from `Diagonal{T}`, `Bidiagonal{T}`, `Tridiagonal{T}` and `SymTridiagonal{T}`
    to `Diagonal{T,V<:AbstractVector{T}}`, `Bidiagonal{T,V<:AbstractVector{T}}`,
    `Tridiagonal{T,V<:AbstractVector{T}}` and `SymTridiagonal{T,V<:AbstractVector{T}}`
    respectively ([#22718], [#22925], [#23035], [#23154]).

  * The immediate supertype of `BitArray` is now simply `AbstractArray`. `BitArray` is no longer
    considered a subtype of `DenseArray` and `StridedArray` ([#25858]).

  * When called with an argument that contains `NaN` elements, `findmin` and `findmax` now return the
    first `NaN` found and its corresponding index. Previously, `NaN` elements were ignored.
    The new behavior matches that of `min`, `max`, `minimum`, and `maximum`.

  * `isapprox(x,y)` now tests `norm(x-y) <= max(atol, rtol*max(norm(x), norm(y)))`
    rather than `norm(x-y) <= atol + ...`, and `rtol` defaults to zero
    if an `atol > 0` is specified ([#22742]).

  * Spaces are no longer allowed between `@` and the name of a macro in a macro call ([#22868]).

  * Juxtaposition of a non-literal with a macro call (`x@macro`) is no longer valid syntax ([#22868]).

  * On a cluster, all files are now loaded from the local file system rather than node 1 ([#22588]).
    To load the same file everywhere from node 1, one possible alternative is to broadcast a call to `include_string`:
    `@everywhere include_string(Main, $(read("filename", String)), "filename")`.
    Improving upon this API is left as an opportunity for packages.

  * `randperm(n)` and `randcycle(n)` now always return a `Vector{Int}` (independent of
    the type of `n`). Use the corresponding mutating functions `randperm!` and `randcycle!`
    to control the array type ([#22723]).

  * Hermitian now ignores any imaginary components in the diagonal instead of checking
    the diagonal. ([#17367])

  * Worker-worker connections are setup lazily for an `:all_to_all` topology. Use keyword
    arg `lazy=false` to force all connections to be setup during a `addprocs` call. ([#22814])

  * In `joinpath(a, b)` on Windows, if the drive specifications of `a` and `b` do not match,
    `joinpath` now returns `b` instead of throwing an `ArgumentError`. `joinpath(path...)` is
    defined to be left associative, so if any argument has a drive path which does not match
    the drive of the join of the preceding paths, the prior ones are dropped. ([#20912])

  * `^(A::AbstractMatrix{<:Integer}, p::Integer)` now throws a `DomainError`
    if `p < 0`, unless `A == one(A)` or `A == -one(A)` (same as for
    `^(A::Integer, p::Integer)`) ([#23366]).

  * `^(A::AbstractMatrix{<:Integer}, p::Integer)` now promotes the element type in the same
    way as `^(A::Integer, p::Integer)`. This means, for instance, that `[1 1; 0 1]^big(1)`
    will return a `Matrix{BigInt}` instead of a `Matrix{Int}` ([#23366]).

  * The element type of the input is now preserved in `unique`. Previously the element type
    of the output was shrunk to fit the union of the type of each element in the input.
    ([#22696])

  * The `promote` function now raises an error if its arguments are of different types
    and if attempting to convert them to a common type fails to change any of their types.
    This avoids stack overflows in the common case of definitions like
    `f(x, y) = f(promote(x, y)...)` ([#22801]).

  * `indmin` and `indmax` have been renamed to `argmin` and `argmax`, respectively ([#25654]).

  * `findmin`, `findmax`, `argmin`, and `argmax` used to always return linear indices.
    They now return `CartesianIndex`es for all but 1-d arrays, and in general return
    the `keys` of indexed collections (e.g. dictionaries) ([#22907]).

  * The `openspecfun` library is no longer built and shipped with Julia, as it is no longer
    used internally ([#22390]).

  * All loaded packages used to have bindings in `Main` (e.g. `Main.Package`). This is no
    longer the case; now bindings will only exist for packages brought into scope by
    typing `using Package` or `import Package` ([#17997]).

  * The rules for mixed-signedness integer arithmetic (e.g. `Int32(1) + UInt64(1)`) have been
    simplified: if the arguments have different sizes (in bits), then the type of the larger
    argument is used. If the arguments have the same size, the unsigned type is used ([#9292]).

  * All command line arguments passed via `-e`, `-E`, and `-L` will be executed in the order
    given on the command line ([#23665]).

  * `I` now yields `UniformScaling{Bool}(true)` rather than `UniformScaling{Int64}(1)`
    to better preserve types in operations involving `I` ([#24396]).

  * The return type of `reinterpret` has changed to `ReinterpretArray`. `reinterpret` on sparse
    arrays has been discontinued.

  * `Base.find_in_path` is now `Base.find_package` or `Base.find_source_file` ([#24320]).

  * `finalizer` now takes functions or pointers as its first argument, and the object being
    finalized as its second (rather than the reverse). For the majority of use cases
    deprecation warnings will be triggered. However, deprecation warnings will not trigger where
    (1) the callable argument is not a subtype of `Function`; or (2) both arguments are
    `Function`s or `Ptr{Cvoid}`s ([#24605]).

  * The `kill` function now throws errors on user error (e.g. on permission
    errors), but returns successfully if the process had previously exited.
    Its return value has been removed. Use the `process_running` function
    to determine if a process has already exited.

  * Broadcasting has been redesigned with an extensible public interface. The new API is
    documented at https://docs.julialang.org/en/latest/manual/interfaces/#Interfaces-1.
    `AbstractArray` types that specialized broadcasting using the old internal API will
    need to switch to the new API. ([#20740])

  * The logging system has been redesigned - `info` and `warn` are deprecated
    and replaced with the logging macros `@info`, `@warn`, `@debug` and
    `@error`.  The `logging` function is also deprecated and replaced with
    `AbstractLogger` and the functions from the new standard `Logging` library.
    ([#24490])

  * The `RevString` type has been removed from the language; `reverse(::String)` returns
    a `String` with code points (or fragments thereof) in reverse order. In general,
    `reverse(s)` should return a string of the same type and encoding as `s` with code
    points in reverse order; any string type overrides `reverse` to return a different
    type of string must also override `reverseind` to compute reversed indices correctly.

  * `eachindex(A, B...)` now requires that all inputs have the same number of elements.
    When the chosen indexing is Cartesian, they must have the same axes.

  * `AbstractRange` objects are now considered as equal to other `AbstractArray` objects
    by `==` and `isequal` if all of their elements are equal ([#16401]).
    This has required changing the hashing algorithm: ranges now use an O(N) fallback
    instead of a O(1) specialized method unless they define the `Base.RangeStepStyle`
    trait; see its documentation for details. Types which support subtraction (operator
    `-`) must now implement `widen` for hashing to work inside heterogeneous arrays.

  * `findn(x::AbstractArray)` has been deprecated in favor of `findall(!iszero, x)`, which
    now returns cartesian indices for multidimensional arrays (see below, [#25532]).

  * `find` has been renamed to `findall`. `findall`, `findfirst`, `findlast`, `findnext`
    now take and/or return the same type of indices as `keys`/`pairs` for `AbstractArray`,
    `AbstractDict`, `AbstractString`, `Tuple` and `NamedTuple` objects ([#24774], [#25545]).
    In particular, this means that they use `CartesianIndex` objects for matrices
    and higher-dimensional arrays instead of linear indices as was previously the case.
    Use `LinearIndices(a)[findall(f, a)]` and similar constructs to compute linear indices.

  * The `find*` functions, i.e. `findnext`, `findprev`, `findfirst`,
    and `findlast`, as well as `indexin`, now return `nothing` when no match is found rather
    than `0` or `0:-1` ([#25472], [#25662], [#26149])

  * The `Base.HasShape` iterator trait has gained a type parameter `N` indicating the
    number of dimensions, which must correspond to the length of the tuple returned by
    `size` ([#25655]).

 * `AbstractSet` objects are now considered equal by `==` and `isequal` if all of their
    elements are equal ([#25368]). This has required changing the hashing algorithm
    for `BitSet`.

  * the default behavior of `titlecase` is changed in two ways ([#23393]):
    + characters not starting a word are converted to lowercase;
      a new keyword argument `strict` is added which
      allows to get the old behavior when it's `false`.
    + any non-letter character is considered as a word separator;
      to get the old behavior (only "space" characters are considered as
      word separators), use the keyword `wordsep=isspace`.

  * `writedlm` in the standard library module DelimitedFiles now writes numeric values
    using `print` rather than `print_shortest` ([#25745]).

  * The `tempname` function used to create a file on Windows but not on other
    platforms. It now never creates a file ([#9053]).

  * The `fieldnames` and `propertynames` functions now return a tuple rather than
    an array ([#25725]).

  * `indexin` now returns the first rather than the last matching index ([#25998]).

  * `parse(::Type, ::Char)` now uses a default base of 10, like other number parsing
    methods, instead of 36 ([#26576]).

Library improvements
--------------------

  * The function `thisind(s::AbstractString, i::Integer)` returns the largest valid index
    less or equal than `i` in the string `s` or `0` if no such index exists ([#24414]).

  * `Char` is now a subtype of `AbstractChar`, and most of the functions that
    take character arguments now accept any `AbstractChar` ([#26286]).

  * `String(array)` now accepts an arbitrary `AbstractVector{UInt8}`. For `Vector`
    inputs, it "steals" the memory buffer, leaving them with an empty buffer which
    is guaranteed not to be shared with the `String` object. For other types of vectors
    (in particular immutable vectors), a copy is made and the input is not truncated ([#26093]).

  * `Irrational` is now a subtype of `AbstractIrrational` ([#24245]).

  * Introduced the `empty` function, the functional pair to `empty!` which returns a new,
    empty container ([#24390]).

  * Jump to first/last history entries in the REPL via "Alt-<" and "Alt->" ([#22829]).

  * REPL LaTeX-like tab completions have been simplified for several Unicode characters,
    e.g. `𝔸` is now `\bbA` rather than `\BbbA` ([#25980]).

  * The function `chop` now accepts two arguments `head` and `tail` allowing to specify
    number of characters to remove from the head and tail of the string ([#24126]).

  * `get(io, :color, false)` can now be used to query whether a stream `io` supports
    [ANSI color codes](https://en.wikipedia.org/wiki/ANSI_escape_code) ([#25067]),
    rather than using the undocumented `Base.have_color` global flag.

  * Functions `first` and `last` now accept `nchar` argument for `AbstractString`.
    If this argument is used they return a string consisting of first/last `nchar`
    characters from the original string ([#23960]).

  * Expressions `x^-n` where `n` is an *integer literal* now correspond to `inv(x)^n`.
    For example, `x^-1` is now essentially a synonym for `inv(x)`, and works
    in a type-stable way even if `typeof(x) != typeof(inv(x))` ([#24240]).

  * New `Iterators.reverse(itr)` for reverse-order iteration ([#24187]).  Iterator
    types `T` can implement `start` etc. for `Iterators.Reverse{T}` to support this.

  * The functions `nextind` and `prevind` now accept `nchar` argument that indicates
    the number of characters to move ([#23805]).

  * The functions `strip`, `lstrip` and `rstrip` now return `SubString` ([#22496]).

  * The functions `strwidth` and `charwidth` have been merged into `textwidth`([#20816]).

  * The functions `base` and `digits` digits now accept a negative
    base (like `ndigits` did) ([#21692]).

  * The function `randn` now accepts complex arguments (`Complex{T <: AbstractFloat}`)
    ([#21973]).

  * `parse(Complex{T}, string)` can parse complex numbers in some common formats ([#24713]).

  * The function `rand` can now pick up random elements from strings, associatives
    and sets ([#22228], [#21960], [#18155], [#22224]).

  * Method lists are now printed as a numbered list. In addition, the source code of a
    method can be opened in an editor by entering the corresponding number in the REPL
    and pressing `^Q` ([#22007]).

  * `getpeername` on a `TCPSocket` returns the address and port of the remote
    endpoint of the TCP connection ([#21825]).

  * `resize!` and `sizehint!` methods no longer over-reserve memory when the
    requested array size is more than double of its current size ([#22038]).

  * The `crc32c` function for CRC-32c checksums is now exported ([#22274]).

  * `eye(::Type{Diagonal{T}}, m::Integer)` has been deprecated in favor of
    `Diagonal{T}(I, m)` ([#24413]).

  * The output of `versioninfo` is now controlled with keyword arguments ([#21974]).

  * The function `LibGit2.set_remote_url` now always sets both the fetch and push URLs for a
    git repo. Additionally, the argument order was changed to be consistent with the git
    command line tool ([#22062]).

  * Added `unique!` which is an inplace version of `unique` ([#20549]).

  * `@test isequal(x, y)` and `@test isapprox(x, y)` now prints an evaluated expression when
    the test fails ([#22296]).

  * Uses of `Val{c}` in `Base` has been replaced with `Val{c}()`, which is now easily
    accessible via the `@pure` constructor `Val(c)`. Functions are defined as
    `f(::Val{c}) = ...` and called by `f(Val(c))`. Notable affected functions include:
    `ntuple`, `Base.literal_pow`, `sqrtm`, `lufact`, `lufact!`, `qrfact`, `qrfact!`,
    `cholfact`, `cholfact!`, `_broadcast!`, `reshape`, `cat` and `cat_t`.

  * A new `@macroexpand1` macro for non recursive macro expansion ([#21662]).

  * `Char`s can now be concatenated with `String`s and/or other `Char`s using `*` ([#22532]).

  * `Diagonal`, `Bidiagonal`, `Tridiagonal` and `SymTridiagonal` are now parameterized on
    the type of the wrapped vectors, allowing `Diagonal`, `Bidiagonal`, `Tridiagonal` and
    `SymTridiagonal` matrices with arbitrary `AbstractVector`s
    ([#22718], [#22925], [#23035], [#23154]).

  * Mutating versions of `randperm` and `randcycle` have been added:
    `randperm!` and `randcycle!` ([#22723]).

  * `BigFloat` random numbers can now be generated ([#22720]).

  * REPL Undo via Ctrl-/ and Ctrl-_

  * `diagm` now accepts several diagonal index/vector `Pair`s ([#24047]).

  * `isequal`, `==`, and `in` have one argument "curried" forms. For example `isequal(x)`
    returns a function that compares its argument to `x` using `isequal` ([#26436]).

  * `reinterpret` now works on any AbstractArray using the new `ReinterpretArray` type.
    This supersedes the old behavior of reinterpret on Arrays. As a result, reinterpreting
    arrays with different alignment requirements (removed in 0.6) is once again allowed ([#23750]).

  * The `keys` of an `Associative` are now an `AbstractSet`. `Base.KeyIterator{<:Associative}`
    has been changed to `KeySet{K, <:Associative{K}} <: AbstractSet{K}` ([#24580]).

  * New function `ncodeunits(s::AbstractString)` gives the number of code units in a string.
    The generic definition is constant time but calls `lastindex(s)` which may be inefficient.
    Therefore custom string types may want to define direct `ncodeunits` methods.

  * `reverseind(s::AbstractString, i::Integer)` now has an efficient generic fallback, so
    custom string types do not need to provide their own efficient defintions. The generic
    definition relies on `ncodeunits` however, so for optimal performance you may need to
    define a custom method for that function.

  * The global RNG is being re-seeded with its own seed at the beginning of each `@testset`,
    and have its original state restored at the end ([#24445]). This is breaking for testsets
    relying implicitly on the global RNG being in a specific state.

  * `permutedims(m::AbstractMatrix)` is now short for `permutedims(m, (2,1))`, and is now a
    more convenient way of making a "shallow transpose" of a 2D array. This is the
    recommended approach for manipulating arrays of data, rather than the recursively
    defined, linear-algebra function `transpose`. Similarly,
    `permutedims(v::AbstractVector)` will create a row matrix ([#24839]).

  * A new `replace(A, old=>new)` function is introduced to replace `old` by `new` in
    collection `A`. There are also two other methods with a different API, and
    a mutating variant, `replace!` ([#22324]).

  * Adding integers to `CartesianIndex` objects is now deprecated. Instead of
    `i::Int + x::CartesianIndex`, use `i*one(x) + x` ([#26284]).

  * `CartesianRange` changes ([#24715]):
    - Inherits from `AbstractArray`, and linear indexing can be used to provide
      linear-to-cartesian conversion ([#24715])
    - It has a new constructor taking an array

  * several missing set-like operations have been added ([#23528]):
    `union`, `intersect`, `symdiff`, `setdiff` are now implemented for
    all collections with arbitrary many arguments, as well as the
    mutating counterparts (`union!` etc.). The performance is also
    much better in many cases. Note that this change is slightly
    breaking: all the non-mutating functions always return a new
    object even if only one argument is passed. Moreover the semantics
    of `intersect` and `symdiff` is changed for vectors:
    + `intersect` doesn't preserve the multiplicity anymore (use `filter` for
      the old behavior)
    + `symdiff` has been made consistent with the corresponding methods for
      other containers, by taking the multiplicity of the arguments into account.
      Use `unique` to get the old behavior.

  * The type `LinearIndices` has been added, providing conversion from
    cartesian indices to linear indices using the normal indexing operation. ([#24715])

  * `IdDict{K,V}` replaces `ObjectIdDict`.  It has type parameters
    like other `AbstractDict` subtypes and its constructors mirror the
    ones of `Dict`. ([#25210])

  * `IOBuffer` can take the `sizehint` keyword argument to suggest a capacity of
    the buffer ([#25944]).

  * `trunc`, `floor`, `ceil`, `round`, and `signif` specify `base` using a
    keyword argument. ([#26156])

Compiler/Runtime improvements
-----------------------------

  * The inlining heuristic now models the approximate runtime cost of
    a method (using some strongly-simplifying assumptions). Functions
    are inlined unless their estimated runtime cost substantially
    exceeds the cost of setting up and issuing a subroutine
    call. ([#22210], [#22732])

  * Inference recursion-detection heuristics are now more precise,
    allowing them to be triggered less often, but being more agressive when they
    are triggered to drive the inference computation to a solution ([#23912]).

  * Inference now propagates constants inter-procedurally, and can compute
    various constants expressions at compile-time ([#24362]).

Deprecated or removed
---------------------

  * The `JULIA_HOME` environment variable has been renamed to `JULIA_BINDIR` and
    `Base.JULIA_HOME` has been moved to `Sys.BINDIR` ([#20899]).

  * The keyword `immutable` is fully deprecated to `struct`, and
    `type` is fully deprecated to `mutable struct` ([#19157], [#20418]).

  * Indexing into multidimensional arrays with more than one index but fewer indices than there are
    dimensions is no longer permitted when those trailing dimensions have lengths greater than 1.
    Instead, reshape the array or add trailing indices so the dimensionality and number of indices
    match ([#14770], [#23628]).

  * `indices(a)` and `indices(a,d)` have been deprecated in favor of `axes(a)` and
    `axes(a, d)` ([#25057]).

  * `EnvHash` has been renamed to `EnvDict` ([#24167]).

  * Uninitialized `Array` constructors of the form
    `Array[{T,N}](shape...)` have been deprecated in favor of equivalents
    accepting `undef` (an alias for `UndefInitializer()`) as their first argument,
    as in `Array[{T,N}](undef, shape...)`. For example,
    `Vector(3)` is now `Vector(undef, 3)`, `Matrix{Int}((2, 4))` is now,
    `Matrix{Int}(undef, (2, 4))`, and `Array{Float32,3}(11, 13, 17)` is now
    `Array{Float32,3}(undef, 11, 13, 17)` ([#24781]).

  * `LinAlg.fillslots!` has been renamed `LinAlg.fillstored!` ([#25030]).

  * `fill!(A::Diagonal, x)` and `fill!(A::AbstractTriangular, x)` have been deprecated
    in favor of `Base.LinAlg.fillstored!(A, x)` ([#24413]).

  * `eye` has been deprecated in favor of `I` and `Matrix` constructors. Please see the
    deprecation warnings for replacement details ([#24438]).

  * `zeros(D::Diagonal[, opts...])` has been deprecated ([#24654]).

  * Using Bool values directly as indices is now deprecated and will be an error in the future. Convert
    them to `Int` before indexing if you intend to access index `1` for `true` and `0` for `false`.

  * `slicedim(A, d, i)` has been deprecated in favor of `copy(selectdim(A, d, i))`. The new
    `selectdim` function now always returns a view into `A`; in many cases the `copy` is
    not necessary. Previously, `slicedim` on a vector `V` over dimension `d=1` and scalar
	index `i` would return the just selected element (unless `V` was a `BitVector`). This
	has now been made consistent: `selectdim` now always returns a view into the original
	array, with a zero-dimensional view in this specific case ([#26009]).

  * `whos` has been renamed `varinfo`, and now returns a markdown table instead of printing
    output ([#12131]).

  * Uninitialized `RowVector` constructors of the form `RowVector{T}(shape...)` have been
    deprecated in favor of equivalents accepting `undef` (an alias for
    `UndefInitializer()`) as their first argument, as in
    `RowVector{T}(undef, shape...)`. For example, `RowVector{Int}(3)` is now
    `RowVector{Int}(undef, 3)`, and `RowVector{Float32}((1, 4))` is now
    `RowVector{Float32}(undef, (1, 4))` ([#24786]).

  * `writecsv(io, a; opts...)` has been deprecated in favor of
    `writedlm(io, a, ','; opts...)` ([#23529]).

  * The method `srand(rng, filename, n=4)` has been deprecated ([#21359]).

  * `readcsv(io[, T::Type]; opts...)` has been deprecated in favor of
    `readdlm(io, ','[, T]; opts...)` ([#23530]).

  * `sparse(s::UniformScaling, m::Integer)` has been deprecated in favor of the
    three-argument equivalent `sparse(s::UniformScaling, m, n)` ([#24472]).

  * The `cholfact`/`cholfact!` methods that accepted an `uplo` symbol have been deprecated
    in favor of using `Hermitian` (or `Symmetric`) views ([#22187], [#22188]).

  * The `thin` keyword argument for orthogonal decomposition methods has
    been deprecated in favor of `full`, which has the opposite meaning:
    `thin == true` if and only if `full == false` ([#24279]).

  * `isposdef(A::AbstractMatrix, UL::Symbol)` and `isposdef!(A::AbstractMatrix, UL::Symbol)`
    have been deprecated in favor of `isposdef(Hermitian(A, UL))` and `isposdef!(Hermitian(A, UL))`
    respectively ([#22245]).

  * The `bkfact`/`bkfact!` methods that accepted `uplo` and `issymmetric` symbols have been deprecated
    in favor of using `Hermitian` (or `Symmetric`) views ([#22605]).

  * The function `current_module` is deprecated and replaced with `@__MODULE__`.
    This caused the deprecation of some reflection methods (such as `macroexpand` and
    `isconst`), which now require a module argument. And it caused the bugfix of other
    default arguments to use the Main module (including `whos`, `which`)  ([#22064]).

  * `expand(ex)` and `expand(module, ex)` have been deprecated in favor of
    `Meta.lower(module, ex)` ([#22064], [#24278]).

  * `ones(A::AbstractArray[, opts...])` and `zeros(A::AbstractArray[, opts...])` methods
    have been deprecated. For `zeros(A)`, consider `zero(A)`. For `ones(A)` or `zeros(A)`,
    consider `ones(size(A))`, `zeros(size(A))`, `fill(v, size(A))` for `v` an appropriate
    one or zero, `fill!(copy(A), {1|0})`, `fill!(similar(A), {1|0})`, or any of the preceding
    with different element type and/or shape depending on `opts...`. Where strictly
    necessary, consider `fill!(similar(A[, opts...]), {one(eltype(A)) | zero(eltype(A))})`.
    For an algebraic multiplicative identity, consider `one(A)` ([#24656]).

  * The `Operators` module is deprecated. Instead, import required operators explicitly
    from `Base`, e.g. `import Base: +, -, *, /` ([#22251]).

  * Bindings to the FFTW library have been removed from Base. The DFT framework for building FFT
    implementations is now in AbstractFFTs.jl, the bindings to the FFTW library are in FFTW.jl,
    and the Base signal processing functions which used FFTs are now in DSP.jl ([#21956]).

  * The `corrected` positional argument to `cov` has been deprecated in favor of
    a keyword argument with the same name ([#21709]).

  * Omitting spaces around the `?` and the `:` tokens in a ternary expression has been deprecated.
    Ternaries must now include some amount of whitespace, e.g. `x ? a : b` rather than
    `x?a:b` ([#22523] and [#22712]).

  * `?` can no longer be used as an identifier name ([#22712])

  * The method `replace(s::AbstractString, pat, r, [count])` is deprecated
    in favor of `replace(s::AbstractString, pat => r; [count])` ([#25165]).
    Moreover, `count` cannot be negative anymore (use `typemax(Int)` instead ([#22325]).

  * `read(io, type, dims)` is deprecated to `read!(io, Array{type}(undef, dims))` ([#21450]).

  * `read(::IO, ::Ref)` is now a method of `read!`, since it mutates its `Ref` argument ([#21592]).

  * `nb_available` is now `bytesavailable` ([#25634]).

  * `skipchars(io::IO, predicate; linecomment=nothing)` is deprecated in favor of
    `skipchars(predicate, io::IO; linecomment=nothing)` ([#25667]).

  * `Bidiagonal` constructors now use a `Symbol` (`:U` or `:L`) for the upper/lower
    argument, instead of a `Bool` or a `Char` ([#22703]).

  * `Bidiagonal`, `Tridiagonal` and `SymTridiagonal` constructors that automatically
    converted the input vectors to the same type are deprecated in favor of explicit
    conversion ([#22925], [#23035], [#23154].

  * Calling `nfields` on a type to find out how many fields its instances have is deprecated.
    Use `fieldcount` instead. Use `nfields` only to get the number of fields in a specific object ([#22350]).

  * `fieldnames` now operates only on types. To get the names of fields in an object, use
    `fieldnames(typeof(x))` ([#22350]).

  * `InexactError`, `DomainError`, and `OverflowError` now take
    arguments. `InexactError(func::Symbol, type, -3)` now prints as
    "ERROR: InexactError: func(type, -3)", `DomainError(val,
    [msg])` prints as "ERROR: DomainError with val:\nmsg",
    and `OverflowError(msg)` prints as "ERROR: OverflowError: msg".
    ([#20005], [#22751], [#22761])

  * The operating system identification functions: `is_linux`, `is_bsd`, `is_apple`, `is_unix`,
    and `is_windows`, have been deprecated in favor of `Sys.islinux`, `Sys.isbsd`, `Sys.isapple`,
    `Sys.isunix`, and `Sys.iswindows`, respectively ([#22182]).

  * The forms of `read`, `readstring`, and `eachline` that accepted both a `Cmd` object and an
    input stream are deprecated. Use e.g. `read(pipeline(stdin, cmd))` instead ([#22762]).

  * The unexported type `AbstractIOBuffer` has been renamed to `GenericIOBuffer` ([#17360] [#22796]).

  * `IOBuffer(data::AbstractVector{UInt8}, read::Bool, write::Bool, maxsize::Integer)`,
    `IOBuffer(read::Bool, write::Bool)`, and `IOBuffer(maxsize::Integer)` are
    deprecated in favor of constructors taking keyword arguments ([#25872]).

  * `Display` has been renamed to `AbstractDisplay` ([#24831]).

  * Remaining vectorized methods over `SparseVector`s, particularly `floor`, `ceil`,
    `trunc`, `round`, and most common transcendental functions such as `exp`, `log`, and
    `sin` variants, have been deprecated in favor of dot-syntax ([#22961]).

  * The method `String(io::IOBuffer)` is deprecated to `String(take!(copy(io)))` ([#21438]).

  * The function `readstring` is deprecated in favor of `read(io, String)` ([#22793])

  * The function `showall` is deprecated. Showing entire values is the default, unless an
    `IOContext` specifying `:limit=>true` is in use ([#22847]).

  * `issubtype` has been deprecated in favor of `<:` (which used to be an alias for `issubtype`).

  * Calling `write` on non-isbits arrays is deprecated in favor of explicit loops or
    `serialize` ([#6466]).

  * The default `startup.jl` file on Windows has been removed. Now must explicitly include the
    full path if you need access to executables or libraries in the `Sys.BINDIR` directory, e.g.
    `joinpath(Sys.BINDIR, "7z.exe")` for `7z.exe` ([#21540]).

  * `sqrtm` has been deprecated in favor of `sqrt` ([#23504]).

  * `expm` has been deprecated in favor of `exp` ([#23233]).

  * `logm` has been deprecated in favor of `log` ([#23505]).

  * `full` has been deprecated in favor of more specific, better defined alternatives.
    On structured matrices `A`, consider instead `Matrix(A)`, `Array(A)`,
    `SparseMatrixCSC(A)`, or `sparse(A)`. On sparse arrays `S`, consider instead
    `Vector(S)`, `Matrix(S)`, or `Array(S)` as appropriate. On factorizations `F`,
    consider instead `Matrix(F)`, `Array(F)`, `AbstractMatrix(F)`, or `AbstractArray(F)`.
    On implicit orthogonal factors `Q`, consider instead `Matrix(Q)` or `Array(Q)`; for
    implicit orthogonal factors that can be recovered in square or truncated form,
    see the deprecation message for square recovery instructions. On `Symmetric`,
    `Hermitian`, or `AbstractTriangular` matrices `A`, consider instead `Matrix(S)`,
    `Array(S)`, `SparseMatrixCSC(S)`, or `sparse(S)`. On `Symmetric` matrices `A`
    particularly, consider instead `LinAlg.copytri!(copy(parent(A)), A.uplo)`. On
    `Hermitian` matrices `A` particularly, consider instead
    `LinAlg.copytri!(copy(parent(A)), A.uplo, true)`. On `UpperTriangular` matrices `A`
    particularly, consider instead `triu!(copy(parent(A)))`. On `LowerTriangular` matrices
    `A` particularly, consider instead `tril!(copy(parent(A)))` ([#24250]).

  * `speye` has been deprecated in favor of `I`, `sparse`, and `SparseMatrixCSC`
    constructor methods ([#24356]).

  * Calling `union` with no arguments is deprecated; construct an empty set with an appropriate
    element type using `Set{T}()` instead ([#23144]).

  * Vectorized `DateTime`, `Date`, and `format` methods have been deprecated in favor of
    dot-syntax ([#23207]).

  * `Base.cpad` has been removed; use an appropriate combination of `rpad` and `lpad`
    instead ([#23187]).

  * `ctranspose` and `ctranspose!` have been deprecated in favor of `adjoint` and `adjoint!`,
    respectively ([#23235]).

  * `filter` and `filter!` on dictionaries now pass a single `key=>value` pair to the
    argument function, instead of two arguments ([#17886]).

  * `rol`, `rol!`, `ror`, and `ror!` have been deprecated in favor of specialized methods for
    `circshift`/`circshift!` ([#23404]).

  * `Base.SparseArrays.SpDiagIterator` has been removed ([#23261]).

  * The tuple-of-types form of `cfunction`, `cfunction(f, returntype, (types...))`, has been deprecated
    in favor of the tuple-type form `cfunction(f, returntype, Tuple{types...})` ([#23066]).

  * `diagm(v::AbstractVector, k::Integer=0)` has been deprecated in favor of
    `diagm(k => v)` ([#24047]).

  * `diagm(x::Number)` has been deprecated in favor of `fill(x, 1, 1)` ([#24047]).

  * `diagm(A::SparseMatrixCSC)` has been deprecated in favor of
    `spdiagm(sparsevec(A))` ([#23341]).

  * `diagm(A::BitMatrix)` has been deprecated, use `diagm(0 => vec(A))` or
    `BitMatrix(Diagonal(vec(A)))` instead ([#23373], [#24047]).

  * `ℯ` (written as `\mscre<TAB>` or `\euler<TAB>`) is now the only (by default) exported
    name for Euler's number, and the type has changed from `Irrational{:e}` to
    `Irrational{:ℯ}` ([#23427]).

  * The mathematical constants `π`, `pi`, `ℯ`, `e`, `γ`, `eulergamma`, `catalan`, `φ` and
    `golden` have been moved from `Base` to a new module; `Base.MathConstants`.
    Only `π`, `pi` and `ℯ` are now exported by default from `Base` ([#23427]).

  * `eu` (previously an alias for `ℯ`) has been deprecated in favor of `ℯ` (or `MathConstants.e`) ([#23427]).

  * `GMP.gmp_version()`, `GMP.GMP_VERSION`, `GMP.gmp_bits_per_limb()`, and `GMP.GMP_BITS_PER_LIBM`
    have been renamed to `GMP.version()`, `GMP.VERSION`, `GMP.bits_per_libm()`, and `GMP.BITS_PER_LIBM`,
    respectively. Similarly, `MPFR.get_version()`, has been renamed to `MPFR.version()` ([#23323]). Also,
    `LinAlg.LAPACK.laver()` has been renamed to `LinAlg.LAPACK.version()` and now returns a `VersionNumber`.

  * `select`, `select!`, `selectperm` and `selectperm!` have been renamed respectively to
    `partialsort`, `partialsort!`, `partialsortperm` and `partialsortperm!` ([#23051]).

  * The `Range` abstract type has been renamed to `AbstractRange` ([#23570]).

  * `map` on dictionaries previously operated on `key=>value` pairs. This behavior is deprecated,
    and in the future `map` will operate only on values ([#5794]).

  * Previously, broadcast defaulted to treating its arguments as scalars if they were not
    arrays. This behavior is deprecated, and in the future `broadcast` will default to
    iterating over all its arguments. Wrap arguments you wish to be treated as scalars with
    `Ref()` or a 1-tuple. Package developers can choose to allow a non-iterable type `T` to
    always behave as a scalar by implementing `broadcastable(x::T) = Ref(x)` ([#26212]).

  * Automatically broadcasted `+` and `-` for `array + scalar`, `scalar - array`, and so-on have
    been deprecated due to inconsistency with linear algebra. Use `.+` and `.-` for these operations
    instead ([#22880], [#22932]).

  * `isleaftype` is deprecated in favor of the simpler predicates `isconcretetype` and `isdispatchtuple`.
    Concrete types are those that might equal `typeof(x)` for some `x`;
    `isleaftype` included some types for which this is not true. Those are now categorized more precisely
    as "dispatch tuple types" and "!has_free_typevars" (not exported). ([#17086], [#25496])

  * `contains(eq, itr, item)` is deprecated in favor of `any` with a predicate ([#23716]).

  * `spdiagm(x::AbstractVector)` has been deprecated in favor of `sparse(Diagonal(x))`
    alternatively `spdiagm(0 => x)` ([#23757]).

  * `spdiagm(x::AbstractVector, d::Integer)` and `spdiagm(x::Tuple{<:AbstractVector}, d::Tuple{<:Integer})`
    have been deprecated in favor of `spdiagm(d => x)` and `spdiagm(d[1] => x[1], d[2] => x[2], ...)`
    respectively. The new `spdiagm` implementation now always returns a square matrix ([#23757]).

  * `spones(A::AbstractSparseArray)` has been deprecated in favor of
    `LinAlg.fillstored!(copy(A), 1)` ([#25037]).

  * Constructors for `LibGit2.UserPasswordCredentials` and `LibGit2.SSHCredentials` which take a
    `prompt_if_incorrect` argument are deprecated. Instead, prompting behavior is controlled using
    the `allow_prompt` keyword in the `LibGit2.CredentialPayload` constructor ([#23690]).

  * `gradient` is deprecated and will be removed in the next release ([#23816]).

  * The timing functions `tic`, `toc`, and `toq` are deprecated in favor of `@time` and `@elapsed`
    ([#17046]).

  * Methods of `findfirst`, `findnext`, `findlast`, and `findprev` that accept a value to
    search for are deprecated in favor of passing a predicate ([#19186], [#10593]).

  * `find` functions now operate only on booleans by default. To look for non-zeros, use
    `x->x!=0` or `!iszero` ([#23120]).

  * The ability of `reinterpret` to yield `Array`s of different type than the underlying storage
    has been removed. The `reinterpret` function is still available, but now returns a
    `ReinterpretArray`. The three argument form of `reinterpret` that implicitly reshapes
    has been deprecated ([#23750]).

  * `bits` has been deprecated in favor of `bitstring` ([#24281], [#24263]).

  * `num2hex` and `hex2num` have been deprecated in favor of `reinterpret` combined with `parse`/`hex` ([#22088]).

  * `copy!` is deprecated for `AbstractSet` and `AbstractDict`, with the intention to re-enable
    it with a cleaner meaning in a future version ([#24844]).

  * `copy!` (resp. `unsafe_copy!`) is deprecated for `AbstractArray` and is renamed `copyto!`
    (resp. `unsafe_copyto!`); it will be re-introduced with a different meaning in a future
    version ([#24808]).

  * `a:b` is deprecated for constructing a `StepRange` when `a` and `b` have physical units
    (Dates and Times). Use `a:s:b`, where `s = Dates.Day(1)` or `s = Dates.Second(1)`.

  * `trues(A::AbstractArray)` and `falses(A::AbstractArray)` are deprecated in favor of
    `trues(size(A))` and `falses(size(A))` respectively ([#24595]).

  * `workspace` is discontinued, check out [Revise.jl](https://github.com/timholy/Revise.jl)
    for an alternative workflow ([#25046]).

  * `cumsum`, `cumprod`, `accumulate`, their mutating versions, and `diff` all now require a `dim`
    argument instead of defaulting to using the first dimension unless there is only
    one dimension ([#24684], [#25457]).

  * The `sum_kbn` and `cumsum_kbn` functions have been moved to the
    [KahanSummation](https://github.com/JuliaMath/KahanSummation.jl) package ([#24869]).

  * `isnumber` has been renamed to `isnumeric` ([#25021]).

  * `is_assigned_char` and `normalize_string` have been renamed to `isassigned` and
    `normalize`, and moved to the new `Unicode` standard library module.
    `graphemes` has also been moved to that module ([#25021]).

  * The functions `eigs` and `svds` have been moved to the `IterativeEigensolvers` standard
    library module ([#24714]).

  * Sparse array functionality has moved to the `SparseArrays` standard library module ([#25249]).

  * Linear algebra functionality, and specifically the `LinAlg` module has moved to the
    `LinearAlgebra` standard library module ([#25571]).

  * `@printf` and `@sprintf` have been moved to the `Printf` standard library ([#23929],[#25056]).

  * The `Libdl` module has moved to the `Libdl` standard library module ([#25459]).

  * The aliases `Complex32`, `Complex64` and `Complex128` have been deprecated in favor of `ComplexF16`,
    `ComplexF32` and `ComplexF64` respectively ([#24647]).

  * `Base.parentindexes` and `SharedArrays.localindexes` have been renamed to `parentindices`
    and `localindices`, respectively. Similarly, the `indexes` field in the `SubArray` type
    has been renamed to `indices` without deprecation ([#25088]).

  * `Associative` has been deprecated in favor of `AbstractDict` ([#25012]).

  * `Void` has been renamed back to `Nothing` with an alias `Cvoid` for use when calling C
    with a return type of `Cvoid` or a return or argument type of `Ptr{Cvoid}` ([#25162]).

  * `Nullable{T}` has been deprecated and moved to the Nullables package ([#23642]). Use
    `Union{T, Nothing}` instead, or `Union{Some{T}, Nothing}` if `nothing` is a possible
    value (i.e. `Nothing <: T`). `isnull(x)` can be replaced with `x === nothing` and
    `unsafe_get`/`get` can be dropped or replaced with `coalesce`.
    `NullException` has been removed.

  * `unshift!` and `shift!` have been renamed to `pushfirst!` and `popfirst!` ([#23902])

  * `ipermute!` has been deprecated in favor of `invpermute!` ([#25168]).

  * `CartesianRange` has been renamed `CartesianIndices` ([#24715]).

  * `sub2ind` and `ind2sub` are deprecated in favor of using `CartesianIndices` and `LinearIndices` ([#24715]).

  * `getindex(F::Factorization, s::Symbol)` (usually seen as e.g. `F[:Q]`) is deprecated
    in favor of dot overloading (`getproperty`) so factors should now be accessed as e.g.
    `F.Q` instead of `F[:Q]` ([#25184]).

  * `search` and `rsearch` have been deprecated in favor of `findfirst`/`findnext` and
    `findlast`/`findprev` respectively, in combination with curried `isequal` and `in`
    predicates for some methods ([#24673]).

  * `search(buf::IOBuffer, delim::UInt8)` has been deprecated in favor of either `occursin(delim, buf)`
    (to test containment) or `readuntil(buf, delim)` (to read data up to `delim`) ([#26600]).

  * `ismatch(regex, str)` has been deprecated in favor of `contains(str, regex)` ([#24673]).

  * `matchall` has been deprecated in favor of `collect(m.match for m in eachmatch(r, s))` ([#26071]).

  * `similar(::Associative)` has been deprecated in favor of `empty(::Associative)`, and
    `similar(::Associative, ::Pair{K, V})` has been deprecated in favour of
    `empty(::Associative, K, V)` ([#24390]).

  * `findin(a, b)` has been deprecated in favor of `findall(in(b), a)` ([#24673]).

  * `module_name` has been deprecated in favor of a new, general `nameof` function. Similarly,
    the unexported `Base.function_name` and `Base.datatype_name` have been deprecated in favor
    of `nameof` methods ([#25622]).

  * The module `Random.dSFMT` is renamed `Random.DSFMT` ([#25567]).

  * `Random.RandomDevice(unlimited::Bool)` (on non-Windows systems) is deprecated in favor of
    `Random.RandomDevice(; unlimited=unlimited)` ([#25668]).

  * The generic implementations of `strides(::AbstractArray)` and `stride(::AbstractArray, ::Int)`
     have been deprecated. Subtypes of `AbstractArray` that implement the newly introduced strided
     array interface should define their own `strides` method ([#25321]).

  * `module_parent`, `Base.datatype_module`, and `Base.function_module` have been deprecated
    in favor of `parentmodule` ([#TODO]).

  * `rand(t::Tuple{Vararg{Int}})` is deprecated in favor of `rand(Float64, t)` or `rand(t...)`;
    `rand(::Tuple)` will have another meaning in the future ([#25429], [#25278]).

  * The `assert` function (and `@assert` macro) have been documented that they are not guaranteed to run under various optimization levels and should therefore not be used to e.g. verify passwords.

  * `ObjectIdDict` has been deprecated in favor of `IdDict{Any,Any}` ([#25210]).

  * `gc` and `gc_enable` have been deprecated in favor of `GC.gc` and `GC.enable` ([#25616]).

  * `Base.@gc_preserve` has been deprecated in favor of `GC.@preserve` ([#25616]).

  * `print_shortest` has been discontinued, but is still available in the `Base.Grisu`
    submodule ([#25745]).

  * `scale!` has been deprecated in favor of `mul!`, `lmul!`, and `rmul!` ([#25701], [#25812]).

  * The `remove_destination` keyword argument to `cp`, `mv`, and the unexported `cptree`
    has been renamed to `force` ([#25979]).

  * `contains` has been deprecated in favor of a more general `occursin` function, which
    takes its arguments in reverse order from `contains` ([#26283]).

  * `Regex` objects are no longer callable. Use `occursin` instead ([#26283]).

  * The methods of `range` based on positional arguments have been deprecated in favor of
    keyword arguments ([#25896]).

  * `linspace` has been deprecated in favor of `range` with `stop` and `length` keyword
    arguments ([#25896]).

  * `LinSpace` has been renamed to `LinRange` ([#25896]).

  * `logspace` has been deprecated to its definition ([#25896]).

  * `endof(a)` has been renamed to `lastindex(a)`, and the `end` keyword in indexing expressions now
    lowers to either `lastindex(a)` (in the case with only one index) or `lastindex(a, d)` (in cases
    where there is more than one index and `end` appears at dimension `d`) ([#23554], [#25763]).

  * `DateTime()`, `Date()`, and `Time()` have been deprecated, instead use `DateTime(1)`, `Date(1)`
    and `Time(0)` respectively ([#23724]).

  * The fallback method `^(x, p::Integer)` is deprecated. If your type relied on this definition,
    add a method such as `^(x::MyType, p::Integer) = Base.power_by_squaring(x, p)` ([#23332]).

  * `DevNull`, `STDIN`, `STDOUT`, and `STDERR` have been renamed to `devnull`, `stdin`, `stdout`,
    and `stderr`, respectively ([#25786]).

  * `wait` and `fetch` on `Task` now resemble the interface of `Future`.

  * `showcompact(io, x...)` has been deprecated in favor of
    `show(IOContext(io, :compact => true), x...)` ([#26080]).
    Use `sprint(show, x..., context=:compact => true)` instead of `sprint(showcompact, x...)`.

  * `isupper`, `islower`, `ucfirst` and `lcfirst` have been deprecated in favor of `isuppercase`,
    `islowercase`, `uppercasefirst` and `lowercasefirst`, respectively ([#26442]).

Command-line option changes
---------------------------

  * New option `--warn-overwrite={yes|no}` to control the warning for overwriting method
    definitions. The default is `no` ([#23002]).

  * New option `--banner={yes,no}` allows suppressing or forcing the printing of the
    startup banner, overriding the default behavior (banner in REPL, no banner otherwise).
    The `--quiet` option implies `--banner=no` even in REPL mode but can be overridden by
    passing `--quiet` together with `--banner=yes` ([#23342]).

  * The option `--precompiled` has been renamed to `--sysimage-native-code` ([#23054]).

  * The option `--compilecache` has been renamed to `--compiled-modules` ([#23054]).

<!--- generated by NEWS-update.jl: -->
[#1974]: https://github.com/JuliaLang/julia/issues/1974
[#4916]: https://github.com/JuliaLang/julia/issues/4916
[#5148]: https://github.com/JuliaLang/julia/issues/5148
[#5794]: https://github.com/JuliaLang/julia/issues/5794
[#6080]: https://github.com/JuliaLang/julia/issues/6080
[#6466]: https://github.com/JuliaLang/julia/issues/6466
[#6614]: https://github.com/JuliaLang/julia/issues/6614
[#8000]: https://github.com/JuliaLang/julia/issues/8000
[#8470]: https://github.com/JuliaLang/julia/issues/8470
[#9053]: https://github.com/JuliaLang/julia/issues/9053
[#9292]: https://github.com/JuliaLang/julia/issues/9292
[#10593]: https://github.com/JuliaLang/julia/issues/10593
[#11310]: https://github.com/JuliaLang/julia/issues/11310
[#12010]: https://github.com/JuliaLang/julia/issues/12010
[#12131]: https://github.com/JuliaLang/julia/issues/12131
[#13079]: https://github.com/JuliaLang/julia/issues/13079
[#14770]: https://github.com/JuliaLang/julia/issues/14770
[#15120]: https://github.com/JuliaLang/julia/issues/15120
[#16356]: https://github.com/JuliaLang/julia/issues/16356
[#16401]: https://github.com/JuliaLang/julia/issues/16401
[#16937]: https://github.com/JuliaLang/julia/issues/16937
[#17046]: https://github.com/JuliaLang/julia/issues/17046
[#17086]: https://github.com/JuliaLang/julia/issues/17086
[#17240]: https://github.com/JuliaLang/julia/issues/17240
[#17360]: https://github.com/JuliaLang/julia/issues/17360
[#17367]: https://github.com/JuliaLang/julia/issues/17367
[#17886]: https://github.com/JuliaLang/julia/issues/17886
[#17997]: https://github.com/JuliaLang/julia/issues/17997
[#18155]: https://github.com/JuliaLang/julia/issues/18155
[#18650]: https://github.com/JuliaLang/julia/issues/18650
[#19089]: https://github.com/JuliaLang/julia/issues/19089
[#19157]: https://github.com/JuliaLang/julia/issues/19157
[#19186]: https://github.com/JuliaLang/julia/issues/19186
[#19987]: https://github.com/JuliaLang/julia/issues/19987
[#20005]: https://github.com/JuliaLang/julia/issues/20005
[#20418]: https://github.com/JuliaLang/julia/issues/20418
[#20549]: https://github.com/JuliaLang/julia/issues/20549
[#20575]: https://github.com/JuliaLang/julia/issues/20575
[#20740]: https://github.com/JuliaLang/julia/issues/20740
[#20816]: https://github.com/JuliaLang/julia/issues/20816
[#20899]: https://github.com/JuliaLang/julia/issues/20899
[#20912]: https://github.com/JuliaLang/julia/issues/20912
[#20974]: https://github.com/JuliaLang/julia/issues/20974
[#21359]: https://github.com/JuliaLang/julia/issues/21359
[#21438]: https://github.com/JuliaLang/julia/issues/21438
[#21450]: https://github.com/JuliaLang/julia/issues/21450
[#21527]: https://github.com/JuliaLang/julia/issues/21527
[#21540]: https://github.com/JuliaLang/julia/issues/21540
[#21592]: https://github.com/JuliaLang/julia/issues/21592
[#21662]: https://github.com/JuliaLang/julia/issues/21662
[#21692]: https://github.com/JuliaLang/julia/issues/21692
[#21697]: https://github.com/JuliaLang/julia/issues/21697
[#21709]: https://github.com/JuliaLang/julia/issues/21709
[#21746]: https://github.com/JuliaLang/julia/issues/21746
[#21759]: https://github.com/JuliaLang/julia/issues/21759
[#21774]: https://github.com/JuliaLang/julia/issues/21774
[#21825]: https://github.com/JuliaLang/julia/issues/21825
[#21956]: https://github.com/JuliaLang/julia/issues/21956
[#21960]: https://github.com/JuliaLang/julia/issues/21960
[#21973]: https://github.com/JuliaLang/julia/issues/21973
[#21974]: https://github.com/JuliaLang/julia/issues/21974
[#22007]: https://github.com/JuliaLang/julia/issues/22007
[#22038]: https://github.com/JuliaLang/julia/issues/22038
[#22062]: https://github.com/JuliaLang/julia/issues/22062
[#22064]: https://github.com/JuliaLang/julia/issues/22064
[#22088]: https://github.com/JuliaLang/julia/issues/22088
[#22089]: https://github.com/JuliaLang/julia/issues/22089
[#22092]: https://github.com/JuliaLang/julia/issues/22092
[#22182]: https://github.com/JuliaLang/julia/issues/22182
[#22187]: https://github.com/JuliaLang/julia/issues/22187
[#22188]: https://github.com/JuliaLang/julia/issues/22188
[#22194]: https://github.com/JuliaLang/julia/issues/22194
[#22210]: https://github.com/JuliaLang/julia/issues/22210
[#22224]: https://github.com/JuliaLang/julia/issues/22224
[#22228]: https://github.com/JuliaLang/julia/issues/22228
[#22245]: https://github.com/JuliaLang/julia/issues/22245
[#22251]: https://github.com/JuliaLang/julia/issues/22251
[#22274]: https://github.com/JuliaLang/julia/issues/22274
[#22281]: https://github.com/JuliaLang/julia/issues/22281
[#22296]: https://github.com/JuliaLang/julia/issues/22296
[#22314]: https://github.com/JuliaLang/julia/issues/22314
[#22324]: https://github.com/JuliaLang/julia/issues/22324
[#22325]: https://github.com/JuliaLang/julia/issues/22325
[#22350]: https://github.com/JuliaLang/julia/issues/22350
[#22390]: https://github.com/JuliaLang/julia/issues/22390
[#22496]: https://github.com/JuliaLang/julia/issues/22496
[#22511]: https://github.com/JuliaLang/julia/issues/22511
[#22523]: https://github.com/JuliaLang/julia/issues/22523
[#22532]: https://github.com/JuliaLang/julia/issues/22532
[#22572]: https://github.com/JuliaLang/julia/issues/22572
[#22588]: https://github.com/JuliaLang/julia/issues/22588
[#22605]: https://github.com/JuliaLang/julia/issues/22605
[#22666]: https://github.com/JuliaLang/julia/issues/22666
[#22696]: https://github.com/JuliaLang/julia/issues/22696
[#22703]: https://github.com/JuliaLang/julia/issues/22703
[#22712]: https://github.com/JuliaLang/julia/issues/22712
[#22718]: https://github.com/JuliaLang/julia/issues/22718
[#22720]: https://github.com/JuliaLang/julia/issues/22720
[#22723]: https://github.com/JuliaLang/julia/issues/22723
[#22732]: https://github.com/JuliaLang/julia/issues/22732
[#22742]: https://github.com/JuliaLang/julia/issues/22742
[#22751]: https://github.com/JuliaLang/julia/issues/22751
[#22761]: https://github.com/JuliaLang/julia/issues/22761
[#22762]: https://github.com/JuliaLang/julia/issues/22762
[#22789]: https://github.com/JuliaLang/julia/issues/22789
[#22793]: https://github.com/JuliaLang/julia/issues/22793
[#22796]: https://github.com/JuliaLang/julia/issues/22796
[#22800]: https://github.com/JuliaLang/julia/issues/22800
[#22801]: https://github.com/JuliaLang/julia/issues/22801
[#22814]: https://github.com/JuliaLang/julia/issues/22814
[#22825]: https://github.com/JuliaLang/julia/issues/22825
[#22829]: https://github.com/JuliaLang/julia/issues/22829
[#22847]: https://github.com/JuliaLang/julia/issues/22847
[#22868]: https://github.com/JuliaLang/julia/issues/22868
[#22880]: https://github.com/JuliaLang/julia/issues/22880
[#22907]: https://github.com/JuliaLang/julia/issues/22907
[#22925]: https://github.com/JuliaLang/julia/issues/22925
[#22926]: https://github.com/JuliaLang/julia/issues/22926
[#22932]: https://github.com/JuliaLang/julia/issues/22932
[#22961]: https://github.com/JuliaLang/julia/issues/22961
[#22984]: https://github.com/JuliaLang/julia/issues/22984
[#23002]: https://github.com/JuliaLang/julia/issues/23002
[#23035]: https://github.com/JuliaLang/julia/issues/23035
[#23051]: https://github.com/JuliaLang/julia/issues/23051
[#23054]: https://github.com/JuliaLang/julia/issues/23054
[#23066]: https://github.com/JuliaLang/julia/issues/23066
[#23117]: https://github.com/JuliaLang/julia/issues/23117
[#23120]: https://github.com/JuliaLang/julia/issues/23120
[#23144]: https://github.com/JuliaLang/julia/issues/23144
[#23154]: https://github.com/JuliaLang/julia/issues/23154
[#23157]: https://github.com/JuliaLang/julia/issues/23157
[#23168]: https://github.com/JuliaLang/julia/issues/23168
[#23187]: https://github.com/JuliaLang/julia/issues/23187
[#23207]: https://github.com/JuliaLang/julia/issues/23207
[#23233]: https://github.com/JuliaLang/julia/issues/23233
[#23235]: https://github.com/JuliaLang/julia/issues/23235
[#23261]: https://github.com/JuliaLang/julia/issues/23261
[#23323]: https://github.com/JuliaLang/julia/issues/23323
[#23332]: https://github.com/JuliaLang/julia/issues/23332
[#23341]: https://github.com/JuliaLang/julia/issues/23341
[#23342]: https://github.com/JuliaLang/julia/issues/23342
[#23354]: https://github.com/JuliaLang/julia/issues/23354
[#23366]: https://github.com/JuliaLang/julia/issues/23366
[#23373]: https://github.com/JuliaLang/julia/issues/23373
[#23393]: https://github.com/JuliaLang/julia/issues/23393
[#23404]: https://github.com/JuliaLang/julia/issues/23404
[#23427]: https://github.com/JuliaLang/julia/issues/23427
[#23504]: https://github.com/JuliaLang/julia/issues/23504
[#23505]: https://github.com/JuliaLang/julia/issues/23505
[#23519]: https://github.com/JuliaLang/julia/issues/23519
[#23528]: https://github.com/JuliaLang/julia/issues/23528
[#23529]: https://github.com/JuliaLang/julia/issues/23529
[#23530]: https://github.com/JuliaLang/julia/issues/23530
[#23554]: https://github.com/JuliaLang/julia/issues/23554
[#23570]: https://github.com/JuliaLang/julia/issues/23570
[#23628]: https://github.com/JuliaLang/julia/issues/23628
[#23642]: https://github.com/JuliaLang/julia/issues/23642
[#23665]: https://github.com/JuliaLang/julia/issues/23665
[#23690]: https://github.com/JuliaLang/julia/issues/23690
[#23716]: https://github.com/JuliaLang/julia/issues/23716
[#23724]: https://github.com/JuliaLang/julia/issues/23724
[#23750]: https://github.com/JuliaLang/julia/issues/23750
[#23757]: https://github.com/JuliaLang/julia/issues/23757
[#23805]: https://github.com/JuliaLang/julia/issues/23805
[#23816]: https://github.com/JuliaLang/julia/issues/23816
[#23885]: https://github.com/JuliaLang/julia/issues/23885
[#23902]: https://github.com/JuliaLang/julia/issues/23902
[#23912]: https://github.com/JuliaLang/julia/issues/23912
[#23923]: https://github.com/JuliaLang/julia/issues/23923
[#23929]: https://github.com/JuliaLang/julia/issues/23929
[#23960]: https://github.com/JuliaLang/julia/issues/23960
[#24047]: https://github.com/JuliaLang/julia/issues/24047
[#24126]: https://github.com/JuliaLang/julia/issues/24126
[#24153]: https://github.com/JuliaLang/julia/issues/24153
[#24167]: https://github.com/JuliaLang/julia/issues/24167
[#24187]: https://github.com/JuliaLang/julia/issues/24187
[#24221]: https://github.com/JuliaLang/julia/issues/24221
[#24240]: https://github.com/JuliaLang/julia/issues/24240
[#24245]: https://github.com/JuliaLang/julia/issues/24245
[#24250]: https://github.com/JuliaLang/julia/issues/24250
[#24263]: https://github.com/JuliaLang/julia/issues/24263
[#24278]: https://github.com/JuliaLang/julia/issues/24278
[#24279]: https://github.com/JuliaLang/julia/issues/24279
[#24281]: https://github.com/JuliaLang/julia/issues/24281
[#24320]: https://github.com/JuliaLang/julia/issues/24320
[#24356]: https://github.com/JuliaLang/julia/issues/24356
[#24362]: https://github.com/JuliaLang/julia/issues/24362
[#24390]: https://github.com/JuliaLang/julia/issues/24390
[#24396]: https://github.com/JuliaLang/julia/issues/24396
[#24404]: https://github.com/JuliaLang/julia/issues/24404
[#24413]: https://github.com/JuliaLang/julia/issues/24413
[#24414]: https://github.com/JuliaLang/julia/issues/24414
[#24438]: https://github.com/JuliaLang/julia/issues/24438
[#24445]: https://github.com/JuliaLang/julia/issues/24445
[#24452]: https://github.com/JuliaLang/julia/issues/24452
[#24472]: https://github.com/JuliaLang/julia/issues/24472
[#24490]: https://github.com/JuliaLang/julia/issues/24490
[#24580]: https://github.com/JuliaLang/julia/issues/24580
[#24595]: https://github.com/JuliaLang/julia/issues/24595
[#24605]: https://github.com/JuliaLang/julia/issues/24605
[#24647]: https://github.com/JuliaLang/julia/issues/24647
[#24653]: https://github.com/JuliaLang/julia/issues/24653
[#24654]: https://github.com/JuliaLang/julia/issues/24654
[#24656]: https://github.com/JuliaLang/julia/issues/24656
[#24673]: https://github.com/JuliaLang/julia/issues/24673
[#24679]: https://github.com/JuliaLang/julia/issues/24679
[#24684]: https://github.com/JuliaLang/julia/issues/24684
[#24713]: https://github.com/JuliaLang/julia/issues/24713
[#24714]: https://github.com/JuliaLang/julia/issues/24714
[#24715]: https://github.com/JuliaLang/julia/issues/24715
[#24774]: https://github.com/JuliaLang/julia/issues/24774
[#24781]: https://github.com/JuliaLang/julia/issues/24781
[#24785]: https://github.com/JuliaLang/julia/issues/24785
[#24786]: https://github.com/JuliaLang/julia/issues/24786
[#24808]: https://github.com/JuliaLang/julia/issues/24808
[#24831]: https://github.com/JuliaLang/julia/issues/24831
[#24839]: https://github.com/JuliaLang/julia/issues/24839
[#24844]: https://github.com/JuliaLang/julia/issues/24844
[#24869]: https://github.com/JuliaLang/julia/issues/24869
[#25012]: https://github.com/JuliaLang/julia/issues/25012
[#25021]: https://github.com/JuliaLang/julia/issues/25021
[#25030]: https://github.com/JuliaLang/julia/issues/25030
[#25037]: https://github.com/JuliaLang/julia/issues/25037
[#25046]: https://github.com/JuliaLang/julia/issues/25046
[#25056]: https://github.com/JuliaLang/julia/issues/25056
[#25057]: https://github.com/JuliaLang/julia/issues/25057
[#25067]: https://github.com/JuliaLang/julia/issues/25067
[#25088]: https://github.com/JuliaLang/julia/issues/25088
[#25162]: https://github.com/JuliaLang/julia/issues/25162
[#25165]: https://github.com/JuliaLang/julia/issues/25165
[#25168]: https://github.com/JuliaLang/julia/issues/25168
[#25184]: https://github.com/JuliaLang/julia/issues/25184
[#25210]: https://github.com/JuliaLang/julia/issues/25210
[#25231]: https://github.com/JuliaLang/julia/issues/25231
[#25249]: https://github.com/JuliaLang/julia/issues/25249
[#25278]: https://github.com/JuliaLang/julia/issues/25278
[#25311]: https://github.com/JuliaLang/julia/issues/25311
[#25321]: https://github.com/JuliaLang/julia/issues/25321
[#25368]: https://github.com/JuliaLang/julia/issues/25368
[#25391]: https://github.com/JuliaLang/julia/issues/25391
[#25424]: https://github.com/JuliaLang/julia/issues/25424
[#25429]: https://github.com/JuliaLang/julia/issues/25429
[#25457]: https://github.com/JuliaLang/julia/issues/25457
[#25459]: https://github.com/JuliaLang/julia/issues/25459
[#25472]: https://github.com/JuliaLang/julia/issues/25472
[#25496]: https://github.com/JuliaLang/julia/issues/25496
[#25532]: https://github.com/JuliaLang/julia/issues/25532
[#25545]: https://github.com/JuliaLang/julia/issues/25545
[#25564]: https://github.com/JuliaLang/julia/issues/25564
[#25567]: https://github.com/JuliaLang/julia/issues/25567
[#25571]: https://github.com/JuliaLang/julia/issues/25571
[#25616]: https://github.com/JuliaLang/julia/issues/25616
[#25622]: https://github.com/JuliaLang/julia/issues/25622
[#25633]: https://github.com/JuliaLang/julia/issues/25633
[#25634]: https://github.com/JuliaLang/julia/issues/25634
[#25654]: https://github.com/JuliaLang/julia/issues/25654
[#25655]: https://github.com/JuliaLang/julia/issues/25655
[#25662]: https://github.com/JuliaLang/julia/issues/25662
[#25667]: https://github.com/JuliaLang/julia/issues/25667
[#25668]: https://github.com/JuliaLang/julia/issues/25668
[#25701]: https://github.com/JuliaLang/julia/issues/25701
[#25725]: https://github.com/JuliaLang/julia/issues/25725
[#25745]: https://github.com/JuliaLang/julia/issues/25745
[#25763]: https://github.com/JuliaLang/julia/issues/25763
[#25786]: https://github.com/JuliaLang/julia/issues/25786
[#25812]: https://github.com/JuliaLang/julia/issues/25812
[#25815]: https://github.com/JuliaLang/julia/issues/25815
[#25830]: https://github.com/JuliaLang/julia/issues/25830
[#25845]: https://github.com/JuliaLang/julia/issues/25845
[#25854]: https://github.com/JuliaLang/julia/issues/25854
[#25858]: https://github.com/JuliaLang/julia/issues/25858
[#25872]: https://github.com/JuliaLang/julia/issues/25872
[#25896]: https://github.com/JuliaLang/julia/issues/25896
[#25944]: https://github.com/JuliaLang/julia/issues/25944
[#25947]: https://github.com/JuliaLang/julia/issues/25947
[#25979]: https://github.com/JuliaLang/julia/issues/25979
[#25980]: https://github.com/JuliaLang/julia/issues/25980
[#25990]: https://github.com/JuliaLang/julia/issues/25990
[#25998]: https://github.com/JuliaLang/julia/issues/25998
[#26009]: https://github.com/JuliaLang/julia/issues/26009
[#26071]: https://github.com/JuliaLang/julia/issues/26071
[#26080]: https://github.com/JuliaLang/julia/issues/26080
[#26093]: https://github.com/JuliaLang/julia/issues/26093
[#26149]: https://github.com/JuliaLang/julia/issues/26149
[#26154]: https://github.com/JuliaLang/julia/issues/26154
[#26156]: https://github.com/JuliaLang/julia/issues/26156
[#26161]: https://github.com/JuliaLang/julia/issues/26161
[#26262]: https://github.com/JuliaLang/julia/issues/26262
[#26284]: https://github.com/JuliaLang/julia/issues/26284
[#26286]: https://github.com/JuliaLang/julia/issues/26286
[#26436]: https://github.com/JuliaLang/julia/issues/26436
[#26442]: https://github.com/JuliaLang/julia/issues/26442
[#26600]: https://github.com/JuliaLang/julia/issues/26600