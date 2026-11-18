Julia v1.15 Release Notes
=========================

New language features
---------------------

Language changes
----------------

* `Slices` exposes `dataids` of its parent array to properly identify aliasing in broadcasting ([#49182]).

Compiler/Runtime improvements
-----------------------------

* After a sweep, Julia's default garbage collector now lets any thread reuse the free cells of
  pages that still hold live objects, instead of only the thread that allocated the page. This
  reduces the memory footprint of workloads that allocate many small objects on many threads and
  keep few of them, such as a language server that runs type inference repeatedly ([#63512]).

Command-line option changes
---------------------------

* `--code-coverage=-<paths>` records no coverage for files under the given paths, and
  optimizes their code as if coverage were off, so that instrumenting a test suite does not change
  how its own functions are compiled.
* `--code-coverage=@<paths>` and `--track-allocation=@<paths>` accept a list of paths ([#54698]).

Multi-threading changes
-----------------------

* `--threads=auto`, `--procs=auto`, `JULIA_NUM_THREADS=auto` and the default number of GC threads
  now honour `JULIA_CPU_THREADS`, matching `Sys.EFFECTIVE_CPU_THREADS` ([#64613]).
* With `JULIA_EXCLUSIVE=1`, default-pool threads are pinned to the CPUs of the inherited affinity
  mask, in order, instead of to CPUs `0` to `N-1`. Their number is limited by that mask rather
  than by `Sys.EFFECTIVE_CPU_THREADS` ([#64613]).
* The floating-point control state (the rounding mode set by `setrounding`, together with the
  exception masks and flush-to-zero bits) is now saved and restored on every task switch, so it
  behaves as task-local state: a new task inherits the state of the task that created it, and a
  task resumes with the state it had when it last yielded, regardless of which thread runs it.
  Type inference, code generation, generated-function generators and finalizers always run under
  the default control state, so for example unmasking floating-point exceptions with
  `feenableexcept` no longer makes compilation fail. The sticky exception status flags are not
  preserved across task switches, nor across any of those runtime entries ([#51277], [#63570]).

Build system changes
--------------------

* `base/userimg.jl` is no longer supported, and the build now fails if it exists. To build a
  system image with additional code, use PackageCompiler.jl, or run a script on top of an existing
  system image with `julia --sysimage <image> --output-o <file> <script>`.

New library functions
---------------------

* `Base.decompose(x::Real)` has been made `public` (but not exported); it is the point
  where rational-valued `Real` subtypes that support hashing hook into the hashing protocol ([#63262]).
* `tap(f)` creates a function that calls `f(x)` for side effects and returns `x` ([#61340]).
* `unsplat(f)` creates a function that bundles its arguments into a tuple and passes them to `f`;
  it is the inverse of `splat` ([#62714]).
* `Base.set_binding_visibility!` sets the declared visibility (`:none`, `:public`, or `:export`) of a name
  in a module, allowing an `export` or `public` declaration to be retracted programmatically ([#62131]).
* `Base.generating_output()` has been made `public` (but not exported) to allow checking whether the current
  process is performing compilation for a pkgimage/sysimage ([#61224]).
* `Base.isfieldatomic(t, s)` has been made `public` (but not exported); it reports whether a field `s` of a
  type `t` is declared `@atomic`.
* `Base.raw_substring` is an unexported, public constructor to build a `SubString` without checking for
  valid string indices.
* `Base.unannotate(::AnnotatedString)` returns the underlying un-annotated string of the input string.
* `Base.include_mapexprs(mod)` is an unexported, public function returning the non-identity
  `mapexpr` functions used by `include(mapexpr, …)` calls while loading the package rooted at
  `mod`, keyed by `(including_module, absolute_path)`. The table is stored inside the package
  image, so it survives precompilation; revision tools (e.g. Revise) use it to re-apply the
  original transform when an `include(mapexpr, …)`-ed file is edited.
* `bitsizeof(x)` returns the declared width in bits of a primitive type, or of the primitive type
  of `x`, excluding the padding that `sizeof` includes. It throws for any other type or value ([#61359]).
* `filterfirst(f, itr)` returns the first element of `itr` for which `f` returns `true` ([#60913]).
* `growat!(a::Vector, i, delta)` grows `a` by `delta` uninitialized elements inserted at index `i` ([#63573]).

New library features
--------------------

* An unquoted `*`, `?` or `[` in a command literal makes a glob, which expands to the matching
  paths when the command is created. Julia calls a `glob` function provided by a package,
  such as Glob.jl, to match the pattern, so globbing works after `using Glob` ([#20401]).
* `Base.enumerate(itr; from=1)` now accepts an optional starting point `from` for the enumeration.

Standard library changes
------------------------


#### Dates

#### InteractiveUtils

#### JuliaSyntaxHighlighting

#### Libdl

#### LinearAlgebra

#### Markdown

#### Mmap

#### Profile

#### Random

#### REPL

* In shell mode, commands can be joined with `&&`, `||` and `;`, which run them as in a shell,
  so that, for example, `cd build && make` works. In command literals these operators still
  have to be quoted. Shell mode now also requires the characters that command literals reject
  unquoted to be quoted, such as `(`, `)`, `#` and a single `&`.
* Shell mode now runs every command directly, as Julia's command literals do, instead of running
  commands without pipes or redirections through `$SHELL -c` on systems other than Windows. It no
  longer reads the `JULIA_SHELL` environment variable. The new `export NAME=value` and `unset NAME`
  commands in shell mode set and remove environment variables for later commands ([#20401]).

#### SharedArrays

#### Sockets

#### Test

#### TOML

#### UUIDs

* `uuid5` now hashes the bytes of `name` as specified by RFC 4122, instead of first processing
  backslash escape sequences in it with `unescape_string`. This changes the result for names
  containing a backslash, which previously could also throw an `ArgumentError`.

External dependencies
---------------------

Tooling Improvements
--------------------

Deprecated or removed
---------------------

<!--- generated by NEWS-update.jl: -->
