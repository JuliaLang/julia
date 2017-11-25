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

 * Custom infix operators can now be defined by appending Unicode
   combining marks, primes, and sub/superscripts to other operators.
   For example, `+̂ₐ″` is parsed as an infix operator with the same
   precedence as `+` ([#22089]).

 * The macro call syntax `@macroname[args]` is now available and is parsed
   as `@macroname([args])` ([#23519]).

  * The construct `if @generated ...; else ...; end` can be used to provide both
    `@generated` and normal implementations of part of a function. Surrounding code
    will be common to both versions ([#23168]).

Language changes
----------------

  * The syntax for parametric methods, `function f{T}(x::T)`, has been
    changed to `function f(x::T) where {T}` ([#11310]).

  * The syntax `1.+2` is deprecated, since it is ambiguous: it could mean either
    `1 .+ 2` (the current meaning) or `1. + 2` ([#19089]).

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

  * `{ }` expressions now use `braces` and `bracescat` as expression heads instead
    of `cell1d` and `cell2d`, and parse similarly to `vect` and `vcat` ([#8470]).

  * Nested `if` expressions that arise from the keyword `elseif` now use `elseif`
    as their expression head instead of `if` ([#21774]).

  * `let` blocks now parse the same as `for` loops; the first argument is either an
    assignment or `block` of assignments, and the second argument is a block of
    statements ([#21774]).

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

  * Variable bindings local to `while` loop bodies are now freshly allocated on each loop iteration,
    matching the behavior of `for` loops.

  * Prefix `&` for by-reference arguments to `ccall` has been deprecated in favor of
    `Ref` argument types ([#6080]).

  * All line numbers in ASTs are represented by `LineNumberNode`s; the `:line` expression
    head is no longer used. `QuoteNode`s are also consistently used for quoted symbols instead
    of the `:quote` expression head (though `:quote` `Expr`s are still used for quoted
    expressions) ([#23885]).

  * The `+` and `-` methods for `Number` and `UniformScaling` are not ambiguous anymore since `+`
    and `-` no longer do automatic broadcasting. Hence the methods for `UniformScaling` and `Number` are
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

  * The syntax `(x...)` for constructing a tuple is deprecated; use `(x...,)` instead (#24452).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * `getindex(s::String, r::UnitRange{Int})` now throws `UnicodeError` if `last(r)`
    is not a valid index into `s` ([#22572]).

  * `ntuple(f, n::Integer)` throws `ArgumentError` if `n` is negative.
    Previously an empty tuple was returned ([#21697]).

  * Juxtaposing string literals (e.g. `"x"y`) is now a syntax error ([#20575]).

  * `finalizer(function, object)` now returns `object` rather than `nothing` ([#24679]).

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

  * Using `ARGS` within the ~/.juliarc.jl or within a .jl file loaded with `--load` will no
    longer contain the script name as the first argument. Instead the script name will be
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

  * `findmin`, `findmax`, `indmin`, and `indmax` used to always return linear indices.
    They now return `CartesianIndex`es for all but 1-d arrays, and in general return
    the `keys` of indexed collections (e.g. dictionaries) ([#22907]).

  * The `openspecfun` library is no longer built and shipped with Julia, as it is no longer
    used internally ([#22390]).

  * All loaded packges used to have bindings in `Main` (e.g. `Main.Package`). This is no
    longer the case; now bindings will only exist for packages brought into scope by
    typing `using Package` or `import Package` ([#17997]).

  * `slicedim(b::BitVector, 1, x)` now consistently returns the same thing that `b[x]` would,
    consistent with its documentation. Previously it would return a `BitArray{0}` for scalar
    `x` ([#20233]).

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
    `Function`s or `Ptr{Void}`s ([#24605]).

  * The `kill` function now throws errors on user error (e.g. on permission
    errors), but returns successfully if the process had previously exited.
    Its return value has been removed. Use the `process_running` function
    to determine if a process has already exited.

  * `transpose` and `transpose!` no longer recursively transpose the elements of the
    container. Similarly, `RowVector` no longer provides a transposed view of the elements.
    Transposition now simply rearranges the elements of containers of data, such as arrays
    of strings. Note that the renamed `adjoint` method (formerly `ctranspose`) does still
    act in a recursive manner, and that (very occassionally) `conj(adjoint(...))` will be
    preferrable to `transpose` for linear algebra problems using nested arrays as "block
    matrices". ([#23424])

Library improvements
--------------------

  * The function `thisind(s::AbstractString, i::Integer)` returns the largest valid index
    less or equal than `i` in the string `s` or `0` if no such index exists ([#24414]).

  * `Irrational` is now a subtype of `AbstractIrrational` ([#24245]).

  * The function `chop` now accepts two arguments `head` and `tail` allowing to specify
    number of characters to remove from the head and tail of the string ([#24126]).

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

  * `logspace` now accepts a `base` keyword argument to specify the base of the logarithmic
    range. The base defaults to 10 ([#22310]).

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

  * New function `equalto(x)`, which returns a function that compares its argument to `x`
    using `isequal` ([#23812]).

  * `reinterpret` now works on any AbstractArray using the new `ReinterpretArray` type.
    This supersedes the old behavior of reinterpret on Arrays. As a result, reinterpreting
    arrays with different alignment requirements (removed in 0.6) is once again allowed ([#23750]).

Compiler/Runtime improvements
-----------------------------

  * The inlining heuristic now models the approximate runtime cost of
    a method (using some strongly-simplifying assumptions). Functions
    are inlined unless their estimated runtime cost substantially
    exceeds the cost of setting up and issuing a subroutine
    call. ([#22210], [#22732])

Deprecated or removed
---------------------

  * The keyword `immutable` is fully deprecated to `struct`, and
    `type` is fully deprecated to `mutable struct` ([#19157], [#20418]).

  * Indexing into multidimensional arrays with more than one index but fewer indices than there are
    dimensions is no longer permitted when those trailing dimensions have lengths greater than 1.
    Instead, reshape the array or add trailing indices so the dimensionality and number of indices
    match ([#14770], [#23628]).

  * `fill!(A::Diagonal, x)` and `fill!(A::AbstractTriangular, x)` have been deprecated
    in favor of `Base.LinAlg.fillslots!(A, x)` ([#24413]).

  * `eye` has been deprecated in favor of `I` and `Matrix` constructors. Please see the
    deprecation warnings for replacement details ([#24438]).

  * `zeros(D::Diagonal[, opts...])` has been deprecated ([#24654]).

  * Using Bool values directly as indices is now deprecated and will be an error in the future. Convert
    them to `Int` before indexing if you intend to access index `1` for `true` and `0` for `false`.

  * `whos` has been renamed `varinfo`, and now returns a markdown table instead of printing
    output ([#12131]).

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
    `Meta.lower(module, ex)` ([#22064, #24278]).

  * `ones(A::AbstractArray[, opts...])` and `zeros(A::AbstractArray[, opts...])` methods
    have been deprecated. The general replacement is `fill!(similar(A[, opts...]), {1|0})`,
    though in most use cases simpler alternatives are better: For `zeros(A)`, consider
    `zero(A)`. For `ones(A)` or `zeros(A)`, consider `fill(v, size(A))` for `v` an
    appropriate one or zero, `fill!(copy(A), {1|0})`, `ones(size(A))` or
    `zeros(size(A))`, or any of the preceding with different element type
    and/or shape depending on `opts...`. For an algebraic multiplicative identity,
    consider `one(A)` ([#24656]).

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

  * The method `replace(s::AbstractString, pat, r, count)` with `count <= 0` is deprecated
    in favor of `replace(s::AbstractString, pat, r, typemax(Int))` ([#22325]).

  * `read(io, type, dims)` is deprecated to `read!(io, Array{type}(dims))` ([#21450]).

  * `read(::IO, ::Ref)` is now a method of `read!`, since it mutates its `Ref` argument ([#21592]).

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

  * The default `juliarc.jl` file on Windows has been removed. Now must explicitly include the
    full path if you need access to executables or libraries in the `JULIA_HOME` directory, e.g.
    `joinpath(JULIA_HOME, "7z.exe")` for `7z.exe` ([#21540]).

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

  * Automatically broadcasted `+` and `-` for `array + scalar`, `scalar - array`, and so-on have
    been deprecated due to inconsistency with linear algebra. Use `.+` and `.-` for these operations
    instead ([#22880], [#22932]).

  * `isleaftype` is deprecated in favor of a simpler predicate `isconcrete`. Concrete types are
    those that might equal `typeof(x)` for some `x`; `isleaftype` includes some types for which
    this is not true. If you are certain you need the old behavior, it is temporarily available
    as `Base._isleaftype` ([#17086]).

  * `contains(eq, itr, item)` is deprecated in favor of `any` with a predicate ([#23716]).

  * `spdiagm(x::AbstractVector)` has been deprecated in favor of `sparse(Diagonal(x))`
    alternatively `spdiagm(0 => x)` ([#23757]).

  * `spdiagm(x::AbstractVector, d::Integer)` and `spdiagm(x::Tuple{<:AbstractVector}, d::Tuple{<:Integer})`
    have been deprecated in favor of `spdiagm(d => x)` and `spdiagm(d[1] => x[1], d[2] => x[2], ...)`
    respectively. The new `spdiagm` implementation now always returns a square matrix ([#23757]).

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

  * `a:b` is deprecated for constructing a `StepRange` when `a` and `b` have physical units
    (Dates and Times). Use `a:s:b`, where `s = Dates.Day(1)` or `s = Dates.Second(1)`.

  * `cumsum`, `cumprod`, `accumulate`, and their mutating versions now require a `dim`
    argument instead of defaulting to using the first dimension ([#24684]).

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

Julia v0.6.0 Release Notes
==========================

New language features
---------------------

  * New type system capabilities ([#8974], [#18457])

    + Type parameter constraints can refer to previous parameters, e.g.
      `type Foo{R<:Real, A<:AbstractArray{R}}`. Can also be used in method definitions.

    + New syntax `Array{T} where T<:Integer`, indicating a union of types over all
      specified values of `T` (represented by a `UnionAll` type). This provides behavior
      similar to parametric methods or `typealias`, but can be used anywhere a type is
      accepted. This syntax can also be used in method definitions, e.g.
      `function inv(M::Matrix{T}) where T<:AbstractFloat`.
      Anonymous functions can have type parameters via the syntax
      `((x::Array{T}) where T<:Real) -> 2x`.

    + Implicit type parameters, e.g. `Vector{<:Real}` is equivalent to
      `Vector{T} where T<:Real`, and similarly for `Vector{>:Int}` ([#20414]).

    + Much more accurate subtype and type intersection algorithms. Method sorting and
      identification of equivalent and ambiguous methods are improved as a result.

Language changes
----------------

  * "Inner constructor" syntax for parametric types is deprecated. For example,
    in this definition:
    ```
    type Foo{T,S<:Real}
        x
        Foo(x) = new(x)
    end
    ```
    the syntax `Foo(x) = new(x)` actually defined a constructor for `Foo{T,S}`,
    i.e. the case where the type parameters are specified. For clarity, this
    definition now must be written as `Foo{T,S}(x) where {T,S<:Real} = new(x)`
    ([#11310], [#20308]).

  * The keywords used to define types have changed ([#19157], [#20418]).

    + `immutable` changes to `struct`

    + `type` changes to `mutable struct`

    + `abstract` changes to `abstract type ... end`

    + `bitstype 32 Char` changes to `primitive type Char 32 end`

    In 0.6, `immutable` and `type` are still allowed as synonyms without a deprecation
    warning.

  * Multi-line and single-line nonstandard command literals have been added. A
    nonstandard command literal is like a nonstandard string literal, but the
    syntax uses backquotes (``` ` ```) instead of double quotes, and the
    resulting macro called is suffixed with `_cmd`. For instance, the syntax
    ``` q`xyz` ``` is equivalent to `@q_cmd "xyz"` ([#18644]).

  * Nonstandard string and command literals can now be qualified with their
    module. For instance, `Base.r"x"` is now parsed as `Base.@r_str "x"`.
    Previously, this syntax parsed as an implicit multiplication ([#18690]).

  * For every binary operator `⨳`, `a .⨳ b` is now automatically equivalent to
    the `broadcast` call `(⨳).(a, b)`.  Hence, one no longer defines methods
    for `.*` etcetera.  This also means that "dot operations" automatically
    fuse into a single loop, along with other dot calls `f.(x)` ([#17623]).
    Similarly for unary operators ([#20249]).

  * Newly defined methods are no longer callable from the same dynamic runtime
    scope they were defined in ([#17057]).

  * `isa` is now parsed as an infix operator with the same precedence as `in`
    ([#19677]).

  * `@.` is now parsed as `@__dot__`, and can be used to add dots to
    every function call, operator, and assignment in an expression ([#20321]).

  * The identifier `_` can be assigned, but accessing its value is deprecated,
    allowing this syntax to be used in the future for discarding values ([#9343],
    [#18251], [#20328]).

  * The `typealias` keyword is deprecated, and should be replaced with
    `Vector{T} = Array{T,1}` or a `const` assignment ([#20500]).

  * Experimental feature: `x^n` for integer literals `n` (e.g. `x^3`
    or `x^-3`) is now lowered to `Base.literal_pow(^, x, Val{n})`, to enable
    compile-time specialization for literal integer exponents ([#20530], [#20889]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * The constructor of `SubString` now checks if the requsted view range
    is defined by valid indices in the parent `AbstractString` ([#22511]).

  * `readline`, `readlines` and `eachline` return lines without line endings by default.
    You *must* use `readline(s, chomp=false)`, etc. to get the old behavior where
    returned lines include trailing end-of-line character(s) ([#19944]).

  * `String`s no longer have a `.data` field (as part of a significant performance
    improvement). Use `Vector{UInt8}(str)` to access a string as a byte array.
    However, allocating the `Vector` object has overhead. You can also use
    `codeunit(str, i)` to access the `i`th byte of a `String`.
    Use `sizeof(str)` instead of `length(str.data)`, and `pointer(str)` instead of
    `pointer(str.data)` ([#19449]).

  * Operations between `Float16` and `Integers` now return `Float16` instead of `Float32` ([#17261]).

  * Keyword arguments are processed left-to-right: if the same keyword is specified more than
    once, the rightmost occurrence takes precedence ([#17785]).

  * The `lgamma(z)` function now uses a different (more standard) branch cut
    for `real(z) < 0`, which differs from `log(gamma(z))` by multiples of 2π
    in the imaginary part ([#18330]).

  * `broadcast` now handles tuples, and treats any argument that is not a tuple
    or an array as a "scalar" ([#16986]).

  * `broadcast` now produces a `BitArray` instead of `Array{Bool}` for
    functions yielding a boolean result.  If you want `Array{Bool}`, use
    `broadcast!` or `.=` ([#17623]).

  * Broadcast `A[I...] .= X` with entirely scalar indices `I` is deprecated as
    its behavior will change in the future.  Use `A[I...] = X` instead.

  * Operations like `.+` and `.*` on `Range` objects are now generic
    `broadcast` calls (see [above](#language-changes)) and produce an `Array`.
    If you want a `Range` result, use `+` and `*`, etcetera ([#17623]).

  * `broadcast` now treats `Ref` (except for `Ptr`) arguments as 0-dimensional
    arrays ([#18965]).

  * `broadcast` now handles missing data (`Nullable`s) allowing operations to
    be lifted over mixtures of `Nullable`s and scalars, as if the `Nullable`
    were like an array with zero or one element ([#16961], [#19787]).

  * The runtime now enforces when new method definitions can take effect ([#17057]).
    The flip-side of this is that new method definitions should now reliably actually
    take effect, and be called when evaluating new code ([#265]).

  * The array-scalar methods of `/`, `\`, `*`, `+`, and `-` now follow broadcast promotion
    rules. (Likewise for the now-deprecated array-scalar methods of `div`, `mod`, `rem`,
    `&`, `|`, and `xor`; see "Deprecated or removed" below.) ([#19692]).

  * `broadcast!(f, A)` now calls `f()` for each element of `A`, rather than doing `fill!(A, f())` ([#19722]).

  * `rmprocs` now throws an exception if requested workers have not been completely
    removed before `waitfor` seconds. With a `waitfor=0`, `rmprocs` returns immediately
    without waiting for worker exits.

  * `quadgk` has been moved from Base into a separate package ([#19741]).

  * The `Collections` module has been removed, and all functions defined therein have been
    moved to the `DataStructures` package ([#19800]).

  * The `RepString` type has been moved to the
    [LegacyStrings.jl package](https://github.com/JuliaArchive/LegacyStrings.jl).

  * In macro calls with parentheses, e.g. `@m(a=1)`, assignments are now parsed as
    `=` expressions, instead of as `kw` expressions ([#7669]).

  * When used as an infix operator, `~` is now parsed as a call to an ordinary operator
    with assignment precedence, instead of as a macro call ([#20406]).

  * (µ "micro" and ɛ "latin epsilon") are considered equivalent to
    the corresponding Greek characters in identifiers.  `\varepsilon`
    now tab-completes to U+03B5 (greek small letter epsilon) ([#19464]).

  * `retry` now inputs the keyword arguments `delays` and `check` instead of
    `n` and `max_delay`.  The previous functionality can be achieved setting
    `delays` to `ExponentialBackOff` ([#19331]).

  * `transpose(::AbstractVector)` now always returns a `RowVector` view of the input (which is a
     special 1×n-sized `AbstractMatrix`), not a `Matrix`, etc. In particular, for
     `v::AbstractVector` we now have `(v.').' === v` and `v.' * v` is a scalar ([#19670]).

  * Parametric types with "unspecified" parameters, such as `Array`, are now represented
    as `UnionAll` types instead of `DataType`s ([#18457]).

  * `Union` types have two fields, `a` and `b`, instead of a single `types` field.
    The empty type `Union{}` is represented by a singleton of type `TypeofBottom` ([#18457]).

  * The type `NTuple{N}` now refers to tuples where every element has the same type
    (since it is shorthand for `NTuple{N,T} where T`). To get the old behavior of matching
    any tuple, use `NTuple{N,Any}` ([#18457]).

  * `FloatRange` has been replaced by `StepRangeLen`, and the internal
    representation of `LinSpace` has changed. Aside from changes in
    the internal field names, this leads to several differences in
    behavior ([#18777]):

    + Both `StepRangeLen` and `LinSpace` can represent ranges of
      arbitrary object types---they are no longer limited to
      floating-point numbers.

    + For ranges that produce `Float64`, `Float32`, or `Float16`
      numbers, `StepRangeLen` can be used to produce values with
      little or no roundoff error due to internal arithmetic that is
      typically twice the precision of the output result.

    + To take advantage of this precision, `linspace(start, stop,
      len)` now returns a range of type `StepRangeLen` rather than
      `LinSpace` when `start` and `stop` are
      `FloatNN`. `LinSpace(start, stop, len)` always returns a
      `LinSpace`.

    + `StepRangeLen(a, step, len)` constructs an ordinary-precision range
      using the values and types of `a` and `step` as given, whereas
      `range(a, step, len)` will attempt to match inputs `a::FloatNN`
      and `step::FloatNN` to rationals and construct a `StepRangeLen`
      that internally uses twice-precision arithmetic.  These two
      outcomes exhibit differences in both precision and speed.

  * `A=>B` expressions are now parsed as calls instead of using `=>` as the
    expression head ([#20327]).

  * The `count` function no longer sums non-boolean values ([#20404])

  * The generic `getindex(::AbstractString, ::AbstractVector)` method's signature has been
    tightened to `getindex(::AbstractString, ::AbstractVector{<:Integer})`. Consequently,
    indexing into `AbstractString`s with non-`AbstractVector{<:Integer}` `AbstractVector`s
    now throws a `MethodError` in the absence of an appropriate specialization.
    (Previously such cases failed less explicitly with the exception of
    `AbstractVector{Bool}`, which now throws an `ArgumentError` noting that
    logical indexing into strings is not supported.)  ([#20248])

  * Bessel, Hankel, Airy, error, Dawson, eta, zeta, digamma, inverse digamma,
    trigamma, and polygamma special functions have been moved from Base to
    the
    [SpecialFunctions.jl package](https://github.com/JuliaMath/SpecialFunctions.jl)
    ([#20427]).  Note that `airy`, `airyx` and `airyprime` have been deprecated
    in favor of more specific functions (`airyai`, `airybi`, `airyaiprime`,
    `airybiprimex`, `airyaix`, `airybix`, `airyaiprimex`, `airybiprimex`)
    ([#18050]).

  * When a macro is called in the module in which that macro is defined, global variables
    in the macro are now correctly resolved in the macro definition environment. Breakage
    from this change commonly manifests as undefined variable errors that do not occur
    under 0.5. Fixing such breakage typically requires sprinkling additional `esc`s in
    the offending macro ([#15850]).

  * `write` on an `IOBuffer` now returns a signed integer in order to be
    consistent with other buffers ([#20609]).

  * The `<:Integer` division fallback `/(::Integer, ::Integer)`, which formerly
    inappropriately took precedence over other division methods for some
    mixed-integer-type division calls, has been removed ([#19779]).

  * `@async`, `@spawn`, `@spawnat`, `@fetch` and `@fetchfrom` no longer implicitly
    localize variables. Previously, the expression would be wrapped in an implicit
    `let` block  ([#19594]).

  * `parse` no longer accepts IPv4 addresses including leading zeros, octal, or hexadecimal.
    Convert IPv4 addresses including octal or hexadecimal to decimal, and remove leading
    zeros in decimal addresses ([#19811]).

  * Closures shipped for remote execution via `@spawn` or `remotecall` now automatically
    serialize globals defined under Main. For details, please refer to the paragraph
    on "Global variables" under the "Parallel computing" chapter in the manual ([#19594]).

  * `homedir` now determines the user's home directory via `libuv`'s `uv_os_homedir`,
    rather than from environment variables ([#19636]).

  * Workers now listen on an ephemeral port assigned by the OS. Previously workers would
    listen on the first free port available from 9009 ([#21818]).


Library improvements
--------------------

  * A new `@views` macro was added to convert a whole expression or block of code to
    use views for all slices ([#20164]).

  * `max`, `min`, and related functions (`minmax`, `maximum`, `minimum`, `extrema`)
     now return `NaN` for `NaN` arguments ([#12563]).

  * `oneunit(x)` function to return a dimensionful version of `one(x)`
    (which is clarified to mean a dimensionless quantity if `x` is dimensionful) ([#20268]).

  * The `chop` and `chomp` functions now return a `SubString` ([#18339]).

  * Numbered stackframes printed in stacktraces can now be opened in an editor by
    entering the corresponding number in the REPL and pressing `^Q` ([#19680]).

  * The REPL now supports something called *prompt pasting* ([#17599]).
    This activates when pasting text that starts with `julia> ` into the REPL.
    In that case, only expressions starting with `julia> ` are parsed, the rest are removed.
    This makes it possible to paste a chunk of code that has been copied from a REPL session
    without having to scrub away prompts and outputs.
    This can be disabled or enabled at will with `Base.REPL.enable_promptpaste(::Bool)`.

  * The function `print_with_color` can now take a color
    represented by an integer between 0 and 255 inclusive
    as its first argument ([#18473]). For a number-to-color mapping, please refer to
    [this chart](https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg).
    It is also possible to use numbers as colors in environment variables that customizes colors in the REPL.
    For example, to get orange warning messages, simply set `ENV["JULIA_WARN_COLOR"] = 208`.
    Please note that not all terminals support 256 colors.

  * The function `print_with_color` no longer prints text in bold by default ([#18628]).
    Instead, the function now take a keyword argument `bold::Bool`
    which determines whether to print in bold or not. On some terminals, printing a color in non bold
    results in slightly darker colors being printed than when printing in bold.
    Therefore, light versions of the colors are now supported.
    For the available colors see the help entry on `print_with_color`.

  * The default text style for REPL input and answers has been changed from bold to normal ([#11250]).
    They can be changed back to bold by setting the environment variables
    `JULIA_INPUT_COLOR` and `JULIA_ANSWER_COLOR` to `"bold"`.
    For example, one way of doing this is adding `ENV["JULIA_INPUT_COLOR"] = :bold`
    and `ENV["JULIA_ANSWER_COLOR"] = :bold` to the `.juliarc.jl` file. See the
    [manual section on customizing colors](https://docs.julialang.org/en/latest/manual/interacting-with-julia#Customizing-Colors-1)
    for more information.

  * The default color for info messages has been changed from blue to cyan
    ([#18442]), and for warning messages from red to yellow ([#18453]).  This
    can be changed back to the original colors by setting the environment
    variables `JULIA_INFO_COLOR` to `"blue"` and `JULIA_WARN_COLOR` to `"red"`.

  * Iteration utilities that wrap iterators and return other iterators (`enumerate`, `zip`, `rest`,
    `countfrom`, `take`, `drop`, `cycle`, `repeated`, `product`, `flatten`, `partition`) have been
    moved to the module `Base.Iterators` ([#18839]).

  * BitArrays can now be constructed from arbitrary iterables, in particular from generator expressions,
    e.g. `BitArray(isodd(x) for x = 1:100)` ([#19018]).

  * `hcat`, `vcat`, and `hvcat` now work with `UniformScaling` objects, so
    you can now do e.g. `[A I]` and it will concatenate an appropriately sized
    identity matrix ([#19305]).

  * New `accumulate` and `accumulate!` functions were added, which generalize `cumsum` and `cumprod`.
    Also known as a [scan](https://en.wikipedia.org/wiki/Prefix_sum) operation ([#18931]).

  * `reshape` now allows specifying one dimension with a `Colon()` (`:`) for the new shape, in which case
    that dimension's length will be computed such that its product with all the other dimensions is equal
    to the length of the original array ([#19919]).

  * The new `to_indices` function provides a uniform interface for index conversions,
    taking an array and a tuple of indices as arguments and returning a tuple of
    integers and/or arrays of supported scalar indices. It will throw an `ArgumentError`
    for any unsupported indices, and the returned arrays should be iterated over (and
    not indexed into) to support more efficient logical indexing ([#19730]).

    + Using colons (`:`) to represent a collection of indices is deprecated. They now must be
      explicitly converted to a specialized array of integers with the `to_indices` function.
      As a result, the type of `SubArray`s that represent views over colon indices has changed.

    + Logical indexing is now more efficient. Logical arrays are converted by `to_indices` to
      a lazy, iterable collection of indices that doesn't support indexing. A deprecation
      provides indexing support with O(n) lookup.

    + The performance of indexing with `CartesianIndex`es is also improved in many situations.

  * A new `titlecase` function was added, to capitalize the first character of each word within a string ([#19469]).

  * `any` and `all` now always short-circuit, and `mapreduce` never short-circuits ([#19543]).
    That is, not every member of the input iterable will be visited if a `true` (in the case of `any`) or
    `false` (in the case of `all`) value is found, and `mapreduce` will visit all members of the iterable.

  * Additional methods for `ones` and `zeros` functions were added
    to support the same signature as the `similar` function ([#19635]).

  * `count` now has a `count(itr)` method equivalent to `count(identity, itr)` ([#20403]).

  * Methods for `map` and `filter` with `Nullable` arguments have been implemented;
    the semantics are as if the `Nullable` were a container with zero or one elements ([#16961]).

  * New `@test_warn` and `@test_nowarn` macros were added in the `Base.Test` module to
    test for the presence or absence of warning messages ([#19903]).

  * `logging` can now be used to redirect `info`, `warn`, and `error` messages
    either universally or on a per-module/function basis ([#16213]).

  * New function `Base.invokelatest(f, args...)` to call the latest version
    of a function in circumstances where an older version may be called
    instead (e.g. in a function calling `eval`) ([#19784]).

  * A new `iszero(x)` function was added, to quickly check whether `x` is zero
    (or is all zeros, for an array) ([#19950]).

  * `notify` now returns a count of tasks woken up ([#19841]).

  * A new nonstandard string literal `raw"..."` was added,
    for creating strings with no interpolation or unescaping ([#19900]).

  * A new `Dates.Time` type was added that supports representing the time of day
    with up to nanosecond resolution ([#12274]).

  * Raising one or negative one to a negative integer power formerly threw a `DomainError`.
    One raised to any negative integer power now yields one, negative one raised to any
    negative even integer power now yields one, and negative one raised to any negative
    odd integer power now yields negative one. Similarly, raising `true` to any negative
    integer power now yields `true` rather than throwing a `DomainError` ([#18342]).

  * A new `@macroexpand` macro was added as a convenient alternative to the `macroexpand` function ([#18660]).

  * `invoke` now supports keyword arguments ([#20345]).

  * A new `ConjArray` type was added, as a wrapper type for lazy complex conjugation of arrays.
    Currently, it is used by default for the new `RowVector` type only, and
    enforces that both `transpose(vec)` and `ctranspose(vec)` are views not copies ([#20047]).

  * `rem` now accepts a `RoundingMode` argument via `rem(x, y, r::RoundingMode)`, yielding
    `x - y*round(x/y, r)` without intermediate rounding. In particular, `rem(x, y, RoundNearest)`
    yields a value in the interval `[-abs(y)/2, abs(y)/2]`), which corresponds to the IEE754
    `remainder` function. Similarly, `rem2pi(x, r::RoundingMode)` now exists as well, yielding
    `rem(x, 2pi, r::RoundingMode)` but with greater accuracy ([#10946]).

  * `map[!]` and `broadcast[!]` now have dedicated methods for sparse/structured
    vectors/matrices. Specifically, `map[!]` and `broadcast[!]` over combinations including
    one or more `SparseVector`, `SparseMatrixCSC`, `Diagonal`, `Bidiagonal`, `Tridiagonal`,
    or `SymTridiagonal`, and any number of `broadcast` scalars, `Vector`s, or `Matrix`s,
    now efficiently yield `SparseVector`s or `SparseMatrix`s as appropriate ([#19239],
    [#19371], [#19518], [#19438], [#19690], [#19724], [#19926], [#19934], [#20009]).

  * The operators `!` and `∘` (`\circ<tab>` at the REPL and in most code editors) now
    respectively perform predicate function negation and function composition. For example,
    `map(!iszero, (0, 1))` is now equivalent to `map(x -> !iszero(x), (0, 1))` and
    `map(uppercase ∘ hex, 250:255)` is now equivalent to
    `map(x -> uppercase(hex(x)), 250:255)` ([#17155]).

  * `enumerate` now supports the two-argument form `enumerate(::IndexStyle, iterable)`.
    This form allows specification of the returned indices' style. For example,
    `enumerate(IndexLinear, iterable)` yields linear indices and
    `enumerate(IndexCartesian, iterable)` yields cartesian indices ([#16378]).

  * Jump to first/last history entries in the REPL via "Alt-<" and "Alt->" ([#22829]).

Compiler/Runtime improvements
-----------------------------

  * `ccall` is now implemented as a macro,
    removing the need for special code-generator support for `Intrinsics` ([#18754]).

  * `ccall` gained limited support for a `llvmcall` calling-convention.
    This can replace many uses of `llvmcall` with a simpler, shorter declaration ([#18754]).

  * All `Intrinsics` are now `Builtin` functions instead and have proper error checking
    and fall-back static compilation support ([#18754]).

Deprecated or removed
---------------------

  * `ipermutedims(A::AbstractArray, p)` has been deprecated in favor of
    `permutedims(A, invperm(p))` ([#18891]).

  * Linear indexing is now only supported when there is exactly one
    non-cartesian index provided. Allowing a trailing index at dimension `d` to
    linearly access the higher dimensions from array `A` (beyond `size(A, d)`)
    has been deprecated as a stricter constraint during bounds checking.
    Instead, `reshape` the array such that its dimensionality matches the
    number of indices ([#20079]).

  * `Multimedia.@textmime "mime"` has been deprecated. Instead define
    `Multimedia.istextmime(::MIME"mime") = true` ([#18441]).

  * `isdefined(a::Array, i::Int)` has been deprecated in favor of `isassigned` ([#18346]).

  * The three-argument `SubArray` constructor (which accepts `dims::Tuple` as its third
    argument) has been deprecated in favor of the two-argument equivalent (the
    `dims::Tuple` argument being superfluous) ([#19259]).

  * `is` has been deprecated in favor of `===` (which used to be an alias for `is`) ([#17758]).

  * Ambiguous methods for addition and subtraction between `UniformScaling`s and `Number`s,
    for example `(+)(J::UniformScaling, x::Number)`, have been deprecated in favor of
    unambiguous, explicit equivalents, for example `J.λ + x` ([#17607]).

  * `num` and `den` have been deprecated in favor of `numerator` and `denominator` respectively ([#19233],[#19246]).

  * `delete!(ENV::EnvDict, k::AbstractString, def)` has been deprecated in favor of
    `pop!(ENV, k, def)`. Be aware that `pop!` returns `k` or `def`, whereas `delete!`
    returns `ENV` or `def` ([#18012]).

  * infix operator `$` has been deprecated in favor of infix `⊻` or function `xor` ([#18977]).

  * The single-argument form of `write` (`write(x)`, with implicit `STDOUT` output stream),
    has been deprecated in favor of the explicit equivalent `write(STDOUT, x)` ([#17654]).

  * `Dates.recur` has been deprecated in favor of `filter` ([#19288])

  * A number of ambiguous `convert` operations between `Number`s (especially `Real`s)
    and `Date`, `DateTime`, and `Period` types have been deprecated in favor of
    unambiguous `convert` and explicit constructor calls. Additionally, ambiguous colon
    construction of `<:Period` ranges without step specification, for example
    `Dates.Hour(1):Dates.Hour(2)`, has been deprecated in favor of such construction
    including step specification, for example `Dates.Hour(1):Dates.Hour(1):Dates.Hour(2)`
    ([#19920]).

  * `cummin` and `cummax` have been deprecated in favor of `accumulate` ([#18931]).

  * The `Array` constructor syntax `Array(T, dims...)` has been deprecated
    in favor of the forms `Array{T,N}(dims...)` (where `N` is known, or
    particularly `Vector{T}(dims...)` for `N = 1` and `Matrix{T}(dims...)` for `N = 2`),
    and `Array{T}(dims...)` (where `N` is not known). Likewise for `SharedArray`s ([#19989]).

  * `sumabs` and `sumabs2` have been deprecated in favor of `sum(abs, x)` and `sum(abs2, x)`, respectively.
    `maxabs` and `minabs` have similarly been deprecated in favor of `maximum(abs, x)` and `minimum(abs, x)`.
    Likewise for the in-place counterparts of these functions ([#19598]).

  * The array-reducing form of `isinteger` (`isinteger(x::AbstractArray)`) has been
    deprecated in favor of `all(isinteger, x)` ([#19925]).

  * `produce`, `consume` and iteration over a Task object have been deprecated in favor of
    using Channels for inter-task communication  ([#19841]).

  * The `negate` keyword has been deprecated from all functions in the `Dates` adjuster
    API (`adjust`, `tonext`, `toprev`, `Date`, `Time`, and `DateTime`). Instead use
    predicate function negation via the `!` operator
    (see [Library Improvements](#library-improvements)) ([#20213]).

  * `@test_approx_eq x y` has been deprecated in favor of `@test isapprox(x,y)` or `@test x ≈ y` ([#4615]).

  * `Matrix()` and `Matrix{T}()` have been deprecated in favor of the explicit forms
    `Matrix(0, 0)` and `Matrix{T}(0, 0)` ([#20330]).

  * Vectorized functions have been deprecated in favor of dot syntax ([#17302], [#17265],
    [#18558], [#19711], [#19712], [#19791], [#19802], [#19931], [#20543], [#20228]).

  *  All methods of character predicates (`isalnum`, `isalpha`, `iscntrl`, `isdigit`,
     `isnumber`, `isgraph`, `islower`, `isprint`, `ispunct`, `isspace`, `isupper`,
     `isxdigit`) that accept `AbstractStrings` have been deprecated in favor of `all`.
     For example, `isnumber("123")` should now be expressed `all(isnumber, "123")`
     ([#20342]).

  * A few names related to indexing traits have been changed: `LinearIndexing` and
    `linearindexing` have been deprecated in favor of `IndexStyle`. `LinearFast` has
    been deprecated in favor of `IndexLinear`, and `LinearSlow` has been deprecated in
    favor of `IndexCartesian` ([#16378]).

  * The two-argument forms of `map` (`map!(f, A)`) and `asyncmap!` (`asyncmap!(f, A)`)
    have been deprecated in anticipation of future semantic changes ([#19721]).

  * `unsafe_wrap(String, ...)` has been deprecated in favor of `unsafe_string` ([#19449]).

  * `zeros` and `ones` methods accepting an element type as the first argument and an
    array as the second argument, for example `zeros(Float64, [1, 2, 3])`, have been
    deprecated in favor of equivalent methods with the second argument instead the
    size of the array, for example `zeros(Float64, size([1, 2, 3]))` ([#21183]).

  * `Base.promote_eltype_op` has been deprecated ([#19669], [#19814], [#19937]).

  * `isimag` has been deprecated ([#19949]).

  * The tuple-of-types form of `invoke`, `invoke(f, (types...), ...)`, has been deprecated
    in favor of the tuple-type form `invoke(f, Tuple{types...}, ...)` ([#18444]).

  * `Base._promote_array_type` has been deprecated ([#19766]).

  * `broadcast_zpreserving` has been deprecated ([#19533], [#19720]).

  * Methods allowing indexing of tuples by `AbstractArray`s with more than one dimension
    have been deprecated. (Indexing a tuple by such a higher-dimensional `AbstractArray`
    should yield a tuple with more than one dimension, but tuples are one-dimensional.)
    ([#19737]).

  * `@test_approx_eq a b` has been deprecated in favor of `@test a ≈ b` (or,
    equivalently, `@test ≈(a, b)` or `@test isapprox(a, b)`).
    `@test_approx_eq_eps` has been deprecated in favor of new `@test` syntax:
    `@test` now supports the syntax `@test f(args...) key=val ...` for
    `@test f(args..., key=val...)`. This syntax allows, for example, writing
    `@test a ≈ b atol=c` in place of `@test ≈(a, b, atol=c)` (and hence
    `@test_approx_eq_eps a b c`) ([#19901]).

  * `takebuf_array` has been deprecated in favor of `take!`, and `takebuf_string(x)`
    has been deprecated in favor of `String(take!(x))` ([#19088]).

  * `convert` methods from `Diagonal` and `Bidiagonal` to subtypes of
    `AbstractTriangular` have been deprecated ([#17723]).

  * `Base.LinAlg.arithtype` has been deprecated. If you were using `arithtype` within a
    `promote_op` call, instead use `promote_op(Base.LinAlg.matprod, Ts...)`. Otherwise,
    consider defining equivalent functionality locally ([#18218]).

  * Special characters (`#{}()[]<>|&*?~;`) should now be quoted in commands. For example,
    ``` `export FOO=1\;` ``` should replace ``` `export FOO=1;` ``` and
    ``` `cd $dir '&&' $thingie` ``` should replace ``` `cd $dir && $thingie` ``` ([#19786]).

  * Zero-argument `Channel` constructors (`Channel()`, `Channel{T}()`) have been deprecated
    in favor of equivalents accepting an explicit `Channel` size
    (`Channel(2)`, `Channel{T}(2)`) ([#18832]).

  * The zero-argument constructor `MersenneTwister()` has been
    deprecated in favor of the explicit `MersenneTwister(0)` ([#16984]).

  * `Base.promote_type(op::Type, Ts::Type...)` has been removed as part of an overhaul
    of `broadcast`'s promotion mechanism. If you need the functionality of that
    `Base.promote_type` method, consider defining it locally via
    `Core.Inference.return_type(op, Tuple{Ts...})` ([#18642]).

  * `bitbroadcast` has been deprecated in favor of `broadcast`, which now produces a
    `BitArray` instead of `Array{Bool}` for functions yielding a boolean result ([#19771]).

  * To complete the deprecation of histogram-related functions, `midpoints` has been
    deprecated. Instead use the
    [StatsBase.jl package](https://github.com/JuliaStats/StatsBase.jl)'s
    `midpoints` function ([#20058]).

  * Passing a type argument to `LibGit2.cat` has been deprecated in favor of a simpler,
    two-argument method for `LibGit2.cat` ([#20435]).

  * The `LibGit2.owner` function for finding the repository which owns a given Git object
    has been deprecated in favor of `LibGit2.repository` ([#20135]).

  * The `LibGit2.GitAnyObject` type has been renamed to `LibGit2.GitUnknownObject` to
    clarify its intent ([#19935]).

  * The `LibGit2.GitOid` type has been renamed to `LibGit2.GitHash` for clarity ([#19878]).

  * Finalizing `LibGit2` objects with `finalize` has been deprecated in favor of using `close`
    ([#19660]).

  * Parsing string dates from a `Dates.DateFormat` object has been deprecated as part of a
    larger effort toward faster, more extensible date parsing ([#20952]).

  * `EnvHash` has been renamed to `EnvDict` ([#24167]).

Command-line option changes
---------------------------

  * In `polly` builds (`USE_POLLY := 1`), the new flag `--polly={yes|no}` controls whether
    `@polly` declarations are respected. (With `--polly=no`, `@polly` declarations are
    ignored.) This flag is also available in non-`polly` builds (`USE_POLLY := 0`),
    but has no effect ([#18159]).

<!--- generated by NEWS-update.jl: -->
[#265]: https://github.com/JuliaLang/julia/issues/265
[#4615]: https://github.com/JuliaLang/julia/issues/4615
[#5148]: https://github.com/JuliaLang/julia/issues/5148
[#5794]: https://github.com/JuliaLang/julia/issues/5794
[#6080]: https://github.com/JuliaLang/julia/issues/6080
[#6466]: https://github.com/JuliaLang/julia/issues/6466
[#6614]: https://github.com/JuliaLang/julia/issues/6614
[#7669]: https://github.com/JuliaLang/julia/issues/7669
[#8470]: https://github.com/JuliaLang/julia/issues/8470
[#8974]: https://github.com/JuliaLang/julia/issues/8974
[#9292]: https://github.com/JuliaLang/julia/issues/9292
[#9343]: https://github.com/JuliaLang/julia/issues/9343
[#10593]: https://github.com/JuliaLang/julia/issues/10593
[#10946]: https://github.com/JuliaLang/julia/issues/10946
[#11250]: https://github.com/JuliaLang/julia/issues/11250
[#11310]: https://github.com/JuliaLang/julia/issues/11310
[#12274]: https://github.com/JuliaLang/julia/issues/12274
[#12563]: https://github.com/JuliaLang/julia/issues/12563
[#13079]: https://github.com/JuliaLang/julia/issues/13079
[#14770]: https://github.com/JuliaLang/julia/issues/14770
[#15850]: https://github.com/JuliaLang/julia/issues/15850
[#16213]: https://github.com/JuliaLang/julia/issues/16213
[#16356]: https://github.com/JuliaLang/julia/issues/16356
[#16378]: https://github.com/JuliaLang/julia/issues/16378
[#16937]: https://github.com/JuliaLang/julia/issues/16937
[#16961]: https://github.com/JuliaLang/julia/issues/16961
[#16984]: https://github.com/JuliaLang/julia/issues/16984
[#16986]: https://github.com/JuliaLang/julia/issues/16986
[#17046]: https://github.com/JuliaLang/julia/issues/17046
[#17057]: https://github.com/JuliaLang/julia/issues/17057
[#17086]: https://github.com/JuliaLang/julia/issues/17086
[#17155]: https://github.com/JuliaLang/julia/issues/17155
[#17240]: https://github.com/JuliaLang/julia/issues/17240
[#17261]: https://github.com/JuliaLang/julia/issues/17261
[#17265]: https://github.com/JuliaLang/julia/issues/17265
[#17302]: https://github.com/JuliaLang/julia/issues/17302
[#17360]: https://github.com/JuliaLang/julia/issues/17360
[#17367]: https://github.com/JuliaLang/julia/issues/17367
[#17599]: https://github.com/JuliaLang/julia/issues/17599
[#17607]: https://github.com/JuliaLang/julia/issues/17607
[#17623]: https://github.com/JuliaLang/julia/issues/17623
[#17654]: https://github.com/JuliaLang/julia/issues/17654
[#17723]: https://github.com/JuliaLang/julia/issues/17723
[#17758]: https://github.com/JuliaLang/julia/issues/17758
[#17785]: https://github.com/JuliaLang/julia/issues/17785
[#17886]: https://github.com/JuliaLang/julia/issues/17886
[#17997]: https://github.com/JuliaLang/julia/issues/17997
[#18012]: https://github.com/JuliaLang/julia/issues/18012
[#18050]: https://github.com/JuliaLang/julia/issues/18050
[#18155]: https://github.com/JuliaLang/julia/issues/18155
[#18159]: https://github.com/JuliaLang/julia/issues/18159
[#18218]: https://github.com/JuliaLang/julia/issues/18218
[#18251]: https://github.com/JuliaLang/julia/issues/18251
[#18330]: https://github.com/JuliaLang/julia/issues/18330
[#18339]: https://github.com/JuliaLang/julia/issues/18339
[#18342]: https://github.com/JuliaLang/julia/issues/18342
[#18346]: https://github.com/JuliaLang/julia/issues/18346
[#18441]: https://github.com/JuliaLang/julia/issues/18441
[#18442]: https://github.com/JuliaLang/julia/issues/18442
[#18444]: https://github.com/JuliaLang/julia/issues/18444
[#18453]: https://github.com/JuliaLang/julia/issues/18453
[#18457]: https://github.com/JuliaLang/julia/issues/18457
[#18473]: https://github.com/JuliaLang/julia/issues/18473
[#18558]: https://github.com/JuliaLang/julia/issues/18558
[#18628]: https://github.com/JuliaLang/julia/issues/18628
[#18642]: https://github.com/JuliaLang/julia/issues/18642
[#18644]: https://github.com/JuliaLang/julia/issues/18644
[#18650]: https://github.com/JuliaLang/julia/issues/18650
[#18660]: https://github.com/JuliaLang/julia/issues/18660
[#18690]: https://github.com/JuliaLang/julia/issues/18690
[#18754]: https://github.com/JuliaLang/julia/issues/18754
[#18777]: https://github.com/JuliaLang/julia/issues/18777
[#18832]: https://github.com/JuliaLang/julia/issues/18832
[#18839]: https://github.com/JuliaLang/julia/issues/18839
[#18891]: https://github.com/JuliaLang/julia/issues/18891
[#18931]: https://github.com/JuliaLang/julia/issues/18931
[#18965]: https://github.com/JuliaLang/julia/issues/18965
[#18977]: https://github.com/JuliaLang/julia/issues/18977
[#19018]: https://github.com/JuliaLang/julia/issues/19018
[#19088]: https://github.com/JuliaLang/julia/issues/19088
[#19089]: https://github.com/JuliaLang/julia/issues/19089
[#19157]: https://github.com/JuliaLang/julia/issues/19157
[#19186]: https://github.com/JuliaLang/julia/issues/19186
[#19233]: https://github.com/JuliaLang/julia/issues/19233
[#19239]: https://github.com/JuliaLang/julia/issues/19239
[#19246]: https://github.com/JuliaLang/julia/issues/19246
[#19259]: https://github.com/JuliaLang/julia/issues/19259
[#19288]: https://github.com/JuliaLang/julia/issues/19288
[#19305]: https://github.com/JuliaLang/julia/issues/19305
[#19331]: https://github.com/JuliaLang/julia/issues/19331
[#19371]: https://github.com/JuliaLang/julia/issues/19371
[#19438]: https://github.com/JuliaLang/julia/issues/19438
[#19449]: https://github.com/JuliaLang/julia/issues/19449
[#19464]: https://github.com/JuliaLang/julia/issues/19464
[#19469]: https://github.com/JuliaLang/julia/issues/19469
[#19518]: https://github.com/JuliaLang/julia/issues/19518
[#19533]: https://github.com/JuliaLang/julia/issues/19533
[#19543]: https://github.com/JuliaLang/julia/issues/19543
[#19594]: https://github.com/JuliaLang/julia/issues/19594
[#19598]: https://github.com/JuliaLang/julia/issues/19598
[#19635]: https://github.com/JuliaLang/julia/issues/19635
[#19636]: https://github.com/JuliaLang/julia/issues/19636
[#19660]: https://github.com/JuliaLang/julia/issues/19660
[#19669]: https://github.com/JuliaLang/julia/issues/19669
[#19670]: https://github.com/JuliaLang/julia/issues/19670
[#19677]: https://github.com/JuliaLang/julia/issues/19677
[#19680]: https://github.com/JuliaLang/julia/issues/19680
[#19690]: https://github.com/JuliaLang/julia/issues/19690
[#19692]: https://github.com/JuliaLang/julia/issues/19692
[#19711]: https://github.com/JuliaLang/julia/issues/19711
[#19712]: https://github.com/JuliaLang/julia/issues/19712
[#19720]: https://github.com/JuliaLang/julia/issues/19720
[#19721]: https://github.com/JuliaLang/julia/issues/19721
[#19722]: https://github.com/JuliaLang/julia/issues/19722
[#19724]: https://github.com/JuliaLang/julia/issues/19724
[#19730]: https://github.com/JuliaLang/julia/issues/19730
[#19737]: https://github.com/JuliaLang/julia/issues/19737
[#19741]: https://github.com/JuliaLang/julia/issues/19741
[#19766]: https://github.com/JuliaLang/julia/issues/19766
[#19771]: https://github.com/JuliaLang/julia/issues/19771
[#19779]: https://github.com/JuliaLang/julia/issues/19779
[#19784]: https://github.com/JuliaLang/julia/issues/19784
[#19786]: https://github.com/JuliaLang/julia/issues/19786
[#19787]: https://github.com/JuliaLang/julia/issues/19787
[#19791]: https://github.com/JuliaLang/julia/issues/19791
[#19800]: https://github.com/JuliaLang/julia/issues/19800
[#19802]: https://github.com/JuliaLang/julia/issues/19802
[#19811]: https://github.com/JuliaLang/julia/issues/19811
[#19814]: https://github.com/JuliaLang/julia/issues/19814
[#19841]: https://github.com/JuliaLang/julia/issues/19841
[#19878]: https://github.com/JuliaLang/julia/issues/19878
[#19900]: https://github.com/JuliaLang/julia/issues/19900
[#19901]: https://github.com/JuliaLang/julia/issues/19901
[#19903]: https://github.com/JuliaLang/julia/issues/19903
[#19919]: https://github.com/JuliaLang/julia/issues/19919
[#19920]: https://github.com/JuliaLang/julia/issues/19920
[#19925]: https://github.com/JuliaLang/julia/issues/19925
[#19926]: https://github.com/JuliaLang/julia/issues/19926
[#19931]: https://github.com/JuliaLang/julia/issues/19931
[#19934]: https://github.com/JuliaLang/julia/issues/19934
[#19935]: https://github.com/JuliaLang/julia/issues/19935
[#19937]: https://github.com/JuliaLang/julia/issues/19937
[#19944]: https://github.com/JuliaLang/julia/issues/19944
[#19949]: https://github.com/JuliaLang/julia/issues/19949
[#19950]: https://github.com/JuliaLang/julia/issues/19950
[#19987]: https://github.com/JuliaLang/julia/issues/19987
[#19989]: https://github.com/JuliaLang/julia/issues/19989
[#20005]: https://github.com/JuliaLang/julia/issues/20005
[#20009]: https://github.com/JuliaLang/julia/issues/20009
[#20047]: https://github.com/JuliaLang/julia/issues/20047
[#20058]: https://github.com/JuliaLang/julia/issues/20058
[#20079]: https://github.com/JuliaLang/julia/issues/20079
[#20135]: https://github.com/JuliaLang/julia/issues/20135
[#20164]: https://github.com/JuliaLang/julia/issues/20164
[#20213]: https://github.com/JuliaLang/julia/issues/20213
[#20228]: https://github.com/JuliaLang/julia/issues/20228
[#20233]: https://github.com/JuliaLang/julia/issues/20233
[#20248]: https://github.com/JuliaLang/julia/issues/20248
[#20249]: https://github.com/JuliaLang/julia/issues/20249
[#20268]: https://github.com/JuliaLang/julia/issues/20268
[#20308]: https://github.com/JuliaLang/julia/issues/20308
[#20321]: https://github.com/JuliaLang/julia/issues/20321
[#20327]: https://github.com/JuliaLang/julia/issues/20327
[#20328]: https://github.com/JuliaLang/julia/issues/20328
[#20330]: https://github.com/JuliaLang/julia/issues/20330
[#20342]: https://github.com/JuliaLang/julia/issues/20342
[#20345]: https://github.com/JuliaLang/julia/issues/20345
[#20403]: https://github.com/JuliaLang/julia/issues/20403
[#20404]: https://github.com/JuliaLang/julia/issues/20404
[#20406]: https://github.com/JuliaLang/julia/issues/20406
[#20414]: https://github.com/JuliaLang/julia/issues/20414
[#20418]: https://github.com/JuliaLang/julia/issues/20418
[#20427]: https://github.com/JuliaLang/julia/issues/20427
[#20435]: https://github.com/JuliaLang/julia/issues/20435
[#20500]: https://github.com/JuliaLang/julia/issues/20500
[#20530]: https://github.com/JuliaLang/julia/issues/20530
[#20543]: https://github.com/JuliaLang/julia/issues/20543
[#20549]: https://github.com/JuliaLang/julia/issues/20549
[#20575]: https://github.com/JuliaLang/julia/issues/20575
[#20609]: https://github.com/JuliaLang/julia/issues/20609
[#20816]: https://github.com/JuliaLang/julia/issues/20816
[#20889]: https://github.com/JuliaLang/julia/issues/20889
[#20912]: https://github.com/JuliaLang/julia/issues/20912
[#20952]: https://github.com/JuliaLang/julia/issues/20952
[#20974]: https://github.com/JuliaLang/julia/issues/20974
[#21183]: https://github.com/JuliaLang/julia/issues/21183
[#21359]: https://github.com/JuliaLang/julia/issues/21359
[#21438]: https://github.com/JuliaLang/julia/issues/21438
[#21450]: https://github.com/JuliaLang/julia/issues/21450
[#21540]: https://github.com/JuliaLang/julia/issues/21540
[#21592]: https://github.com/JuliaLang/julia/issues/21592
[#21662]: https://github.com/JuliaLang/julia/issues/21662
[#21692]: https://github.com/JuliaLang/julia/issues/21692
[#21697]: https://github.com/JuliaLang/julia/issues/21697
[#21709]: https://github.com/JuliaLang/julia/issues/21709
[#21746]: https://github.com/JuliaLang/julia/issues/21746
[#21759]: https://github.com/JuliaLang/julia/issues/21759
[#21774]: https://github.com/JuliaLang/julia/issues/21774
[#21818]: https://github.com/JuliaLang/julia/issues/21818
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
[#22310]: https://github.com/JuliaLang/julia/issues/22310
[#22314]: https://github.com/JuliaLang/julia/issues/22314
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
[#22907]: https://github.com/JuliaLang/julia/issues/22907
[#22925]: https://github.com/JuliaLang/julia/issues/22925
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
[#23341]: https://github.com/JuliaLang/julia/issues/23341
[#23342]: https://github.com/JuliaLang/julia/issues/23342
[#23366]: https://github.com/JuliaLang/julia/issues/23366
[#23373]: https://github.com/JuliaLang/julia/issues/23373
[#23404]: https://github.com/JuliaLang/julia/issues/23404
[#23427]: https://github.com/JuliaLang/julia/issues/23427
[#23504]: https://github.com/JuliaLang/julia/issues/23504
[#23505]: https://github.com/JuliaLang/julia/issues/23505
[#23519]: https://github.com/JuliaLang/julia/issues/23519
[#23529]: https://github.com/JuliaLang/julia/issues/23529
[#23530]: https://github.com/JuliaLang/julia/issues/23530
[#23570]: https://github.com/JuliaLang/julia/issues/23570
[#23628]: https://github.com/JuliaLang/julia/issues/23628
[#23665]: https://github.com/JuliaLang/julia/issues/23665
[#23690]: https://github.com/JuliaLang/julia/issues/23690
[#23716]: https://github.com/JuliaLang/julia/issues/23716
[#23750]: https://github.com/JuliaLang/julia/issues/23750
[#23757]: https://github.com/JuliaLang/julia/issues/23757
[#23805]: https://github.com/JuliaLang/julia/issues/23805
[#23812]: https://github.com/JuliaLang/julia/issues/23812
[#23816]: https://github.com/JuliaLang/julia/issues/23816
[#23885]: https://github.com/JuliaLang/julia/issues/23885
[#23923]: https://github.com/JuliaLang/julia/issues/23923
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
[#24279]: https://github.com/JuliaLang/julia/issues/24279
[#24281]: https://github.com/JuliaLang/julia/issues/24281
[#24320]: https://github.com/JuliaLang/julia/issues/24320
[#24396]: https://github.com/JuliaLang/julia/issues/24396
[#24413]: https://github.com/JuliaLang/julia/issues/24413
