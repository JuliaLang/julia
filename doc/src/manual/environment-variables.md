# Environment Variables

Julia can be configured with a number of environment variables, set either in
the usual way for each operating system, or in a portable way from within Julia.
Supposing that you want to set the environment variable [`JULIA_EDITOR`](@ref JULIA_EDITOR) to `vim`,
you can type `ENV["JULIA_EDITOR"] = "vim"` (for instance, in the REPL) to make
this change on a case by case basis, or add the same to the user configuration
file `~/.julia/config/startup.jl` in the user's home directory to have a
permanent effect. The current value of the same environment variable can be
determined by evaluating `ENV["JULIA_EDITOR"]`.

The environment variables that Julia uses generally start with `JULIA`. If
[`InteractiveUtils.versioninfo`](@ref) is called with the keyword `verbose=true`, then the
output will list any defined environment variables relevant for Julia,
including those which include `JULIA` in their names.

!!! note

    It is recommended to avoid changing environment variables during runtime,
    such as within a `~/.julia/config/startup.jl`.

    One reason is that some julia language variables, such as [`JULIA_NUM_THREADS`](@ref JULIA_NUM_THREADS)
    and [`JULIA_PROJECT`](@ref JULIA_PROJECT), need to be set before Julia starts.

    Similarly, `__init__()` functions of user modules in the sysimage (via PackageCompiler) are
    run before `startup.jl`, so setting environment variables in a `startup.jl` may be too late for
    user code.

    Further, changing environment variables during runtime can introduce data races into
    otherwise benign code.

    In Bash, environment variables can either be set manually by running, e.g.,
    `export JULIA_NUM_THREADS=4` before starting Julia, or by adding the same command to
    `~/.bashrc` or `~/.bash_profile` to set the variable each time Bash is started.

## File locations

### [`JULIA_BINDIR`](@id JULIA_BINDIR)

The absolute path of the directory containing the Julia executable, which sets
the global variable [`Sys.BINDIR`](@ref). If `$JULIA_BINDIR` is not set, then
Julia determines the value `Sys.BINDIR` at run-time.

The executable itself is one of

```
$JULIA_BINDIR/julia
$JULIA_BINDIR/julia-debug
```

by default.

The global variable `Base.DATAROOTDIR` determines a relative path from
`Sys.BINDIR` to the data directory associated with Julia. Then the path

```
$JULIA_BINDIR/$DATAROOTDIR/julia/base
```

determines the directory in which Julia initially searches for source files (via
`Base.find_source_file()`).

Likewise, the global variable `Base.SYSCONFDIR` determines a relative path to the
configuration file directory. Then Julia searches for a `startup.jl` file at

```
$JULIA_BINDIR/$SYSCONFDIR/julia/startup.jl
$JULIA_BINDIR/../etc/julia/startup.jl
```

by default (via `Base.load_julia_startup()`).

For example, a Linux installation with a Julia executable located at
`/bin/julia`, a `DATAROOTDIR` of `../share`, and a `SYSCONFDIR` of `../etc` will
have [`JULIA_BINDIR`](@ref JULIA_BINDIR) set to `/bin`, a source-file search path of

```
/share/julia/base
```

and a global configuration search path of

```
/etc/julia/startup.jl
```

### [`JULIA_PROJECT`](@id JULIA_PROJECT)

A directory path that indicates which project should be the initial active project.
Setting this environment variable has the same effect as specifying the `--project`
start-up option, but `--project` has higher precedence. If the variable is set to `@.`
(note the trailing dot)
then Julia tries to find a project directory that contains `Project.toml` or
`JuliaProject.toml` file from the current directory and its parents. See also
the chapter on [Code Loading](@ref code-loading).

!!! note

    [`JULIA_PROJECT`](@ref JULIA_PROJECT) must be defined before starting julia; defining it in `startup.jl`
    is too late in the startup process.

### [`JULIA_LOAD_PATH`](@id JULIA_LOAD_PATH)

The [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH) environment variable is used to populate the global Julia
[`LOAD_PATH`](@ref) variable, which determines which packages can be loaded via
`import` and `using` (see [Code Loading](@ref code-loading)).

Unlike the shell `PATH` variable, empty entries in [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH) are expanded to
the default value of `LOAD_PATH`, `["@", "@v#.#", "@stdlib"]` when populating
`LOAD_PATH`. This allows easy appending, prepending, etc. of the load path value in
shell scripts regardless of whether [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH) is already set or not. For
example, to prepend the directory `/foo/bar` to `LOAD_PATH` just do
```sh
export JULIA_LOAD_PATH="/foo/bar:$JULIA_LOAD_PATH"
```
If the [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH) environment variable is already set, its old value will be
prepended with `/foo/bar`. On the other hand, if [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH) is not set, then
it will be set to `/foo/bar:` which will expand to a `LOAD_PATH` value of
`["/foo/bar", "@", "@v#.#", "@stdlib"]`. If [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH) is set to the empty
string, it expands to an empty `LOAD_PATH` array. In other words, the empty string
is interpreted as a zero-element array, not a one-element array of the empty string.
This behavior was chosen so that it would be possible to set an empty load path via
the environment variable. If you want the default load path, either unset the
environment variable or if it must have a value, set it to the string `:`.

!!! note

    On Windows, path elements are separated by the `;` character, as is the case with
    most path lists on Windows. Replace `:` with `;` in the above paragraph.

### [`JULIA_DEPOT_PATH`](@id JULIA_DEPOT_PATH)

The [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH) environment variable is used to populate the global Julia
[`DEPOT_PATH`](@ref) variable, which controls where the package manager, as well
as Julia's code loading mechanisms, look for package registries, installed
packages, named environments, repo clones, cached compiled package images,
configuration files, and the default location of the REPL's history file.

Unlike the shell `PATH` variable but similar to [`JULIA_LOAD_PATH`](@ref JULIA_LOAD_PATH),
empty entries in [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH) are expanded to the default
value of `DEPOT_PATH`, excluding the user depot. This allows easy overriding of the user
depot, while still retaining access to resources that are bundled with Julia, like cache
files, artifacts, etc. For example, to switch the user depot to `/foo/bar` just do
```sh
export JULIA_DEPOT_PATH="/foo/bar:"
```
All package operations, like cloning registrise or installing packages, will now write to
`/foo/bar`, but since the empty entry is expanded to the default system depot, any bundled
resources will still be available. If you really only want to use the depot at `/foo/bar`,
and not load any bundled resources, simply set the environment variable to `/foo/bar`
without the trailing colon.

There are two exceptions to the above rule. First, if [`JULIA_DEPOT_PATH`](@ref
JULIA_DEPOT_PATH) is set to the empty string, it expands to an empty `DEPOT_PATH` array. In
other words, the empty string is interpreted as a zero-element array, not a one-element
array of the empty string. This behavior was chosen so that it would be possible to set an
empty depot path via the environment variable.

Second, if no user depot is specified in [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH), then
the empty entry is expanded to the default depot *including* the user depot. This makes
it possible to use the default depot, as if the environment variable was unset, by setting
it to the string `:`.

!!! note

    On Windows, path elements are separated by the `;` character, as is the case with
    most path lists on Windows. Replace `:` with `;` in the above paragraph.

!!! note
    [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH) must be defined before starting julia; defining it in
    `startup.jl` is too late in the startup process; at that point you can instead
    directly modify the `DEPOT_PATH` array, which is populated from the environment
    variable.

### [`JULIA_HISTORY`](@id JULIA_HISTORY)

The absolute path `REPL.find_hist_file()` of the REPL's history file. If
`$JULIA_HISTORY` is not set, then `REPL.find_hist_file()` defaults to

```
$(DEPOT_PATH[1])/logs/repl_history.jl
```

### [`JULIA_MAX_NUM_PRECOMPILE_FILES`](@id JULIA_MAX_NUM_PRECOMPILE_FILES)

Sets the maximum number of different instances of a single package that are to be stored in the precompile cache (default = 10).

### [`JULIA_VERBOSE_LINKING`](@id JULIA_VERBOSE_LINKING)

If set to true, linker commands will be displayed during precompilation.

## Pkg.jl

### [`JULIA_CI`](@id JULIA_CI)

If set to `true`, this indicates to the package server that any package operations are part of a continuous integration (CI) system for the purposes of gathering package usage statistics.

### [`JULIA_NUM_PRECOMPILE_TASKS`](@id JULIA_NUM_PRECOMPILE_TASKS)

The number of parallel tasks to use when precompiling packages. See [`Pkg.precompile`](https://pkgdocs.julialang.org/v1/api/#Pkg.precompile).

### [`JULIA_PKG_DEVDIR`](@id JULIA_PKG_DEVDIR)

The default directory used by [`Pkg.develop`](https://pkgdocs.julialang.org/v1/api/#Pkg.develop) for downloading packages.

### [`JULIA_PKG_IGNORE_HASHES`](@id JULIA_PKG_IGNORE_HASHES)

If set to `1`, this will ignore incorrect hashes in artifacts. This should be used carefully, as it disables verification of downloads, but can resolve issues when moving files across different types of file systems. See [Pkg.jl issue #2317](https://github.com/JuliaLang/Pkg.jl/issues/2317) for more details.

!!! compat "Julia 1.6"
    This is only supported in Julia 1.6 and above.

### [`JULIA_PKG_OFFLINE`](@id JULIA_PKG_OFFLINE)

If set to `true`, this will enable offline mode: see [`Pkg.offline`](https://pkgdocs.julialang.org/v1/api/#Pkg.offline).

!!! compat "Julia 1.5"
    Pkg's offline mode requires Julia 1.5 or later.

### [`JULIA_PKG_PRECOMPILE_AUTO`](@id JULIA_PKG_PRECOMPILE_AUTO)

If set to `0`, this will disable automatic precompilation by package actions which change the manifest. See [`Pkg.precompile`](https://pkgdocs.julialang.org/v1/api/#Pkg.precompile).

### [`JULIA_PKG_SERVER`](@id JULIA_PKG_SERVER)

Specifies the URL of the package registry to use. By default, `Pkg` uses
`https://pkg.julialang.org` to fetch Julia packages. In addition, you can disable the use of the PkgServer
protocol, and instead access the packages directly from their hosts (GitHub, GitLab, etc.)
by setting: ``` export JULIA_PKG_SERVER="" ```

### [`JULIA_PKG_SERVER_REGISTRY_PREFERENCE`](@id JULIA_PKG_SERVER_REGISTRY_PREFERENCE)

Specifies the preferred registry flavor. Currently supported values are `conservative`
(the default), which will only publish resources that have been processed by the storage
server (and thereby have a higher probability of being available from the PkgServers),
whereas `eager` will publish registries whose resources have not necessarily been
processed by the storage servers.  Users behind restrictive firewalls that do not allow
downloading from arbitrary servers should not use the `eager` flavor.

!!! compat "Julia 1.7"
    This only affects Julia 1.7 and above.

### [`JULIA_PKG_UNPACK_REGISTRY`](@id JULIA_PKG_UNPACK_REGISTRY)

If set to `true`, this will unpack the registry instead of storing it as a compressed tarball.

!!! compat "Julia 1.7"
    This only affects Julia 1.7 and above. Earlier versions will always unpack the registry.

### [`JULIA_PKG_USE_CLI_GIT`](@id JULIA_PKG_USE_CLI_GIT)

If set to `true`, Pkg operations which use the git protocol will use an external `git` executable instead of the default libgit2 library.

!!! compat "Julia 1.7"
    Use of the `git` executable is only supported on Julia 1.7 and above.

### [`JULIA_PKGRESOLVE_ACCURACY`](@id JULIA_PKGRESOLVE_ACCURACY)

The accuracy of the package resolver. This should be a positive integer, the default is `1`.

### [`JULIA_PKG_PRESERVE_TIERED_INSTALLED`](@id JULIA_PKG_PRESERVE_TIERED_INSTALLED)

Change the default package installation strategy to `Pkg.PRESERVE_TIERED_INSTALLED`
to let the package manager try to install versions of packages while keeping as many
versions of packages already installed as possible.

!!! compat "Julia 1.9"
    This only affects Julia 1.9 and above.

## Network transport

### [`JULIA_NO_VERIFY_HOSTS`](@id JULIA_NO_VERIFY_HOSTS)
### [`JULIA_SSL_NO_VERIFY_HOSTS`](@id JULIA_SSL_NO_VERIFY_HOSTS)
### [`JULIA_SSH_NO_VERIFY_HOSTS`](@id JULIA_SSH_NO_VERIFY_HOSTS)
### [`JULIA_ALWAYS_VERIFY_HOSTS`](@id JULIA_ALWAYS_VERIFY_HOSTS)

Specify hosts whose identity should or should not be verified for specific transport layers. See [`NetworkOptions.verify_host`](https://github.com/JuliaLang/NetworkOptions.jl#verify_host)

### [`JULIA_SSL_CA_ROOTS_PATH`](@id JULIA_SSL_CA_ROOTS_PATH)

Specify the file or directory containing the certificate authority roots. See [`NetworkOptions.ca_roots`](https://github.com/JuliaLang/NetworkOptions.jl#ca_roots)

## External applications

### [`JULIA_SHELL`](@id JULIA_SHELL)

The absolute path of the shell with which Julia should execute external commands
(via `Base.repl_cmd()`). Defaults to the environment variable `$SHELL`, and
falls back to `/bin/sh` if `$SHELL` is unset.

!!! note

    On Windows, this environment variable is ignored, and external commands are
    executed directly.

### [`JULIA_EDITOR`](@id JULIA_EDITOR)

The editor returned by `InteractiveUtils.editor()` and used in, e.g., [`InteractiveUtils.edit`](@ref),
referring to the command of the preferred editor, for instance `vim`.

`$JULIA_EDITOR` takes precedence over `$VISUAL`, which in turn takes precedence
over `$EDITOR`. If none of these environment variables is set, then the editor
is taken to be `open` on Windows and OS X, or `/etc/alternatives/editor` if it
exists, or `emacs` otherwise.

To use Visual Studio Code on Windows, set `$JULIA_EDITOR` to `code.cmd`.

## Parallelization

### [`JULIA_CPU_THREADS`](@id JULIA_CPU_THREADS)

Overrides the global variable [`Base.Sys.CPU_THREADS`](@ref), the number of
logical CPU cores available.

### [`JULIA_WORKER_TIMEOUT`](@id JULIA_WORKER_TIMEOUT)

A [`Float64`](@ref) that sets the value of `Distributed.worker_timeout()` (default: `60.0`).
This function gives the number of seconds a worker process will wait for
a master process to establish a connection before dying.

### [`JULIA_NUM_THREADS`](@id JULIA_NUM_THREADS)

An unsigned 64-bit integer (`uint64_t`) that sets the maximum number of threads
available to Julia.  If `$JULIA_NUM_THREADS` is not positive or is not set, or
if the number of CPU threads cannot be determined through system calls, then the
number of threads is set to `1`.

If `$JULIA_NUM_THREADS` is set to `auto`, then the number of threads will be set
to the number of CPU threads.

!!! note
    `JULIA_NUM_THREADS` must be defined before starting julia; defining it in
    `startup.jl` is too late in the startup process.

!!! compat "Julia 1.5"
    In Julia 1.5 and above the number of threads can also be specified on startup
    using the `-t`/`--threads` command line argument.

!!! compat "Julia 1.7"
    The `auto` value for `$JULIA_NUM_THREADS` requires Julia 1.7 or above.

### [`JULIA_THREAD_SLEEP_THRESHOLD`](@id JULIA_THREAD_SLEEP_THRESHOLD)

If set to a string that starts with the case-insensitive substring `"infinite"`,
then spinning threads never sleep. Otherwise, `$JULIA_THREAD_SLEEP_THRESHOLD` is
interpreted as an unsigned 64-bit integer (`uint64_t`) and gives, in
nanoseconds, the amount of time after which spinning threads should sleep.

### [`JULIA_NUM_GC_THREADS`](@id JULIA_NUM_GC_THREADS)

Sets the number of threads used by Garbage Collection. If unspecified is set to
half of the number of worker threads.

!!! compat "Julia 1.10"
    The environment variable was added in 1.10

### [`JULIA_IMAGE_THREADS`](@id JULIA_IMAGE_THREADS)

An unsigned 32-bit integer that sets the number of threads used by image
compilation in this Julia process. The value of this variable may be
ignored if the module is a small module. If left unspecified, the smaller
of the value of [`JULIA_CPU_THREADS`](@ref JULIA_CPU_THREADS) or half the
number of logical CPU cores is used in its place.

### [`JULIA_IMAGE_TIMINGS`](@id JULIA_IMAGE_TIMINGS)

A boolean value that determines if detailed timing information is printed during
during image compilation. Defaults to 0.

### [`JULIA_EXCLUSIVE`](@id JULIA_EXCLUSIVE)

If set to anything besides `0`, then Julia's thread policy is consistent with
running on a dedicated machine: the master thread is on proc 0, and threads are
affinitized. Otherwise, Julia lets the operating system handle thread policy.

## REPL formatting

Environment variables that determine how REPL output should be formatted at the
terminal. The `JULIA_*_COLOR` variables should be set to [ANSI terminal escape
sequences](https://en.wikipedia.org/wiki/ANSI_escape_code). Julia provides
a high-level interface with much of the same functionality; see the section on
[The Julia REPL](@ref).

### [`JULIA_ERROR_COLOR`](@id JULIA_ERROR_COLOR)

The formatting `Base.error_color()` (default: light red, `"\033[91m"`) that
errors should have at the terminal.

### [`JULIA_WARN_COLOR`](@id JULIA_WARN_COLOR)

The formatting `Base.warn_color()` (default: yellow, `"\033[93m"`) that warnings
should have at the terminal.

### [`JULIA_INFO_COLOR`](@id JULIA_INFO_COLOR)

The formatting `Base.info_color()` (default: cyan, `"\033[36m"`) that info
should have at the terminal.

### [`JULIA_INPUT_COLOR`](@id JULIA_INPUT_COLOR)

The formatting `Base.input_color()` (default: normal, `"\033[0m"`) that input
should have at the terminal.

### [`JULIA_ANSWER_COLOR`](@id JULIA_ANSWER_COLOR)

The formatting `Base.answer_color()` (default: normal, `"\033[0m"`) that output
should have at the terminal.

### `NO_COLOR`

If set to anything besides `""`, then colored text will be disabled on the REPL. Can be overridden with the `--color=yes` flag. This environmental variable is [commonly recognized by command-line applications](https://no-color.org/).

### `FORCE_COLOR`

If set to anything besides `""`, then colored text will be forced on the REPL. Can *not* be overridden with the `--color` flag. This environmental variable is [commonly recognized by command-line applications](https://force-color.org/).

## System and Package Image Building

### [`JULIA_CPU_TARGET`](@id JULIA_CPU_TARGET)

Modify the target machine architecture for (pre)compiling
[system](@ref sysimg-multi-versioning) and [package images](@ref pkgimgs-multi-versioning).
`JULIA_CPU_TARGET` only affects machine code image generation being output to a disk cache.
Unlike the `--cpu-target`, or `-C`, [command line option](@ref cli), it does not influence
just-in-time (JIT) code generation within a Julia session where machine code is only
stored in memory.

Valid values for [`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET) can be obtained by executing `julia -C help`.

Setting [`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET) is important for heterogeneous compute systems where processors of
distinct types or features may be present. This is commonly encountered in high performance
computing (HPC) clusters since the component nodes may be using distinct processors.

The CPU target string is a list of strings separated by `;` each string starts with a CPU
or architecture name and followed by an optional list of features separated by `,`.
A `generic` or empty CPU name means the basic required feature set of the target ISA
which is at least the architecture the C/C++ runtime is compiled with. Each string
is interpreted by LLVM.

A few special features are supported:
1. `clone_all`

     This forces the target to have all functions in sysimg cloned.
     When used in negative form (i.e. `-clone_all`), this disables full clone that's
     enabled by default for certain targets.

2. `base([0-9]*)`

     This specifies the (0-based) base target index. The base target is the target
     that the current target is based on, i.e. the functions that are not being cloned
     will use the version in the base target. This option causes the base target to be
     fully cloned (as if `clone_all` is specified for it) if it is not the default target (0).
     The index can only be smaller than the current index.

3. `opt_size`

     Optimize for size with minimum performance impact. Clang/GCC's `-Os`.

4. `min_size`

     Optimize only for size. Clang's `-Oz`.


## Debugging and profiling

### [`JULIA_DEBUG`](@id JULIA_DEBUG)

Enable debug logging for a file or module, see [`Logging`](@ref man-logging) for more information.

### [`JULIA_PROFILE_PEEK_HEAP_SNAPSHOT`](@id JULIA_PROFILE_PEEK_HEAP_SNAPSHOT)

Enable collecting of a heap snapshot during execution via the profiling peek mechanism.
See [Triggered During Execution](@ref).

### [`JULIA_TIMING_SUBSYSTEMS`](@id JULIA_TIMING_SUBSYSTEMS)

Allows you to enable or disable zones for a specific Julia run.
For instance, setting the variable to `+GC,-INFERENCE` will enable the `GC` zones and disable
the `INFERENCE` zones. See [Dynamically Enabling and Disabling Zones](@ref).

### [`JULIA_GC_ALLOC_POOL`](@id JULIA_GC_ALLOC_POOL)
### [`JULIA_GC_ALLOC_OTHER`](@id JULIA_GC_ALLOC_OTHER)
### [`JULIA_GC_ALLOC_PRINT`](@id JULIA_GC_ALLOC_PRINT)

If set, these environment variables take strings that optionally start with the
character `'r'`, followed by a string interpolation of a colon-separated list of
three signed 64-bit integers (`int64_t`). This triple of integers `a:b:c`
represents the arithmetic sequence `a`, `a + b`, `a + 2*b`, ... `c`.

*   If it's the `n`th time that `jl_gc_pool_alloc()` has been called, and `n`
    belongs to the arithmetic sequence represented by `$JULIA_GC_ALLOC_POOL`,
    then garbage collection is forced.
*   If it's the `n`th time that `maybe_collect()` has been called, and `n` belongs
    to the arithmetic sequence represented by `$JULIA_GC_ALLOC_OTHER`, then garbage
    collection is forced.
*   If it's the `n`th time that `jl_gc_collect()` has been called, and `n` belongs
    to the arithmetic sequence represented by `$JULIA_GC_ALLOC_PRINT`, then counts
    for the number of calls to `jl_gc_pool_alloc()` and `maybe_collect()` are
    printed.

If the value of the environment variable begins with the character `'r'`, then
the interval between garbage collection events is randomized.

!!! note

    These environment variables only have an effect if Julia was compiled with
    garbage-collection debugging (that is, if `WITH_GC_DEBUG_ENV` is set to `1`
    in the build configuration).

### [`JULIA_GC_NO_GENERATIONAL`](@id JULIA_GC_NO_GENERATIONAL)

If set to anything besides `0`, then the Julia garbage collector never performs
"quick sweeps" of memory.

!!! note

    This environment variable only has an effect if Julia was compiled with
    garbage-collection debugging (that is, if `WITH_GC_DEBUG_ENV` is set to `1`
    in the build configuration).

### [`JULIA_GC_WAIT_FOR_DEBUGGER`](@id JULIA_GC_WAIT_FOR_DEBUGGER)

If set to anything besides `0`, then the Julia garbage collector will wait for
a debugger to attach instead of aborting whenever there's a critical error.

!!! note

    This environment variable only has an effect if Julia was compiled with
    garbage-collection debugging (that is, if `WITH_GC_DEBUG_ENV` is set to `1`
    in the build configuration).

### [`ENABLE_JITPROFILING`](@id ENABLE_JITPROFILING)

If set to anything besides `0`, then the compiler will create and register an
event listener for just-in-time (JIT) profiling.

!!! note

    This environment variable only has an effect if Julia was compiled with JIT
    profiling support, using either
    * Intel's [VTune™ Amplifier](https://software.intel.com/en-us/vtune)
      (`USE_INTEL_JITEVENTS` set to `1` in the build configuration), or
    * [OProfile](https://oprofile.sourceforge.io/news/) (`USE_OPROFILE_JITEVENTS` set to `1`
      in the build configuration).
    * [Perf](https://perf.wiki.kernel.org) (`USE_PERF_JITEVENTS` set to `1`
      in the build configuration). This integration is enabled by default.

### [`ENABLE_GDBLISTENER`](@id ENABLE_GDBLISTENER)

If set to anything besides `0` enables GDB registration of Julia code on release builds.
On debug builds of Julia this is always enabled. Recommended to use with `-g 2`.


### [`JULIA_LLVM_ARGS`](@id JULIA_LLVM_ARGS)

Arguments to be passed to the LLVM backend.

### `JULIA_FALLBACK_REPL`

Forces the fallback repl instead of REPL.jl.
