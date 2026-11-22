Julia v1.15 Release Notes
=========================

New language features
---------------------

Language changes
----------------

* `Slices` exposes `dataids` of its parent array to properly identify aliasing in broadcasting ([#49182]).

Compiler/Runtime improvements
-----------------------------

* Large vectors of plain data now grow in place: once `push!`, `append!` or `resize!` has outgrown
  a `Vector`'s memory, growing it further extends that memory instead of copying it, and
  `sizehint!` shrinks it without copying. This includes the memory of an `IOBuffer`, so building
  a large `String` copies it neither while growing nor when taking it. Peak memory while growing
  falls from almost twice the final size to the final size.

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

* `bitsizeof(x)` returns the declared width in bits of a primitive type, or of the primitive type
  of `x`, excluding the padding that `sizeof` includes. It throws for any other type or value ([#61359]).
* `Base.expand(mem, n)` returns a `Memory` of `n` elements that starts with those of `mem`. Large
  memory grows in place, without copying, once `expand` or a growing array has allocated it.
* `filterfirst(f, itr)` returns the first element of `itr` for which `f` returns `true` ([#60913]).
* `growat!(a::Vector, i, delta)` grows `a` by `delta` uninitialized elements inserted at index `i` ([#63573]).

New library features
--------------------

* An unquoted `*`, `?` or `[` in a command literal makes a glob, which expands to the matching
  paths when the command is created. Julia calls a `glob` function provided by a package,
  such as Glob.jl, to match the pattern, so globbing works after `using Glob` ([#20401]).
* `unescape_string` accepts a `hex_as_char` keyword argument that decodes a `\x` escape
  sequence as the Unicode code point U+0000 to U+00FF instead of as a raw byte ([#65023]).
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
