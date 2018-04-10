# Environment Variables

Julia may be configured with a number of environment variables, either in the
usual way of the operating system, or in a portable way from within Julia.
Suppose you want to set the environment variable `JULIA_EDITOR` to
`vim`, then either type `ENV["JULIA_EDITOR"] = "vim"` for instance in the REPL
to make this change on a case by case basis, or add the same to the user
configuration file `~/.julia/config/startup.jl` in the user's home directory to have
a permanent effect. The current value of the same environment variable is
determined by evaluating `ENV["JULIA_EDITOR"]`.

The environment variables that Julia uses generally start with `JULIA`. If
[`InteractiveUtils.versioninfo`](@ref) is called with `verbose` equal to `true`, then the
output will list defined environment variables relevant for Julia, including
those for which `JULIA` appears in the name.

## File locations

### `JULIA_BINDIR`

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
have `JULIA_BINDIR` set to `/bin`, a source-file search path of

```
/share/julia/base
```

and a global configuration search path of

```
/etc/julia/startup.jl
```

### `JULIA_LOAD_PATH`

A separated list of absolute paths that are to be appended to the variable
[`LOAD_PATH`](@ref). (In Unix-like systems, the path separator is `:`; in Windows
systems, the path separator is `;`.) The `LOAD_PATH` variable is where
[`Base.require`](@ref) and `Base.load_in_path()` look for code; it defaults to the absolute
paths

```
$JULIA_HOME/../local/share/julia/site/v$(VERSION.major).$(VERSION.minor)
$JULIA_HOME/../share/julia/site/v$(VERSION.major).$(VERSION.minor)
```

so that, e.g., version 0.6 of Julia on a Linux system with a Julia executable at
`/bin/julia` will have a default `LOAD_PATH` of

```
/local/share/julia/site/v0.6
/share/julia/site/v0.6
```

### `JULIA_PKGDIR`

The path of the parent directory `Pkg.Dir._pkgroot()` for the version-specific
Julia package repositories. If the path is relative, then it is taken with
respect to the working directory. If `$JULIA_PKGDIR` is not set, then
`Pkg.Dir._pkgroot()` defaults to

```
$HOME/.julia
```

Then the repository location `Pkg.dir` for a given Julia version is

```
$JULIA_PKGDIR/v$(VERSION.major).$(VERSION.minor)
```

For example, for a Linux user whose home directory is `/home/alice`, the directory
containing the package repositories would by default be

```
/home/alice/.julia
```

and the package repository for version 0.6 of Julia would be

```
/home/alice/.julia/v0.6
```

### `JULIA_HISTORY`

The absolute path `Base.REPL.find_hist_file()` of the REPL's history file. If
`$JULIA_HISTORY` is not set, then `Base.REPL.find_hist_file()` defaults to

```
$HOME/.julia_history
```

### `JULIA_PKGRESOLVE_ACCURACY`

A positive `Int` that determines how much time the max-sum subroutine
`MaxSum.maxsum()` of the package dependency resolver `Pkg.resolve`
will devote to attempting satisfying constraints before giving up: this value is
by default `1`, and larger values correspond to larger amounts of time.

Suppose the value of `$JULIA_PKGRESOLVE_ACCURACY` is `n`. Then

*   the number of pre-decimation iterations is `20*n`,
*   the number of iterations between decimation steps is `10*n`, and
*   at decimation steps, at most one in every `20*n` packages is decimated.

## External applications

### `JULIA_SHELL`

The absolute path of the shell with which Julia should execute external commands
(via `Base.repl_cmd()`). Defaults to the environment variable `$SHELL`, and
falls back to `/bin/sh` if `$SHELL` is unset.

!!! note

    On Windows, this environment variable is ignored, and external commands are
    executed directly.

### `JULIA_EDITOR`

The editor returned by `InteractiveUtils.editor()` and used in, e.g., [`InteractiveUtils.edit`](@ref),
referring to the command of the preferred editor, for instance `vim`.

`$JULIA_EDITOR` takes precedence over `$VISUAL`, which in turn takes precedence
over `$EDITOR`. If none of these environment variables is set, then the editor
is taken to be `open` on Windows and OS X, or `/etc/alternatives/editor` if it
exists, or `emacs` otherwise.

!!! note

    `$JULIA_EDITOR` is *not* used in the determination of the editor for
    `Pkg.edit`: this function checks `$VISUAL` and `$EDITOR` alone.

## Parallelization

### `JULIA_CPU_CORES`

Overrides the global variable [`Base.Sys.CPU_CORES`](@ref), the number of
logical CPU cores available.

### `JULIA_WORKER_TIMEOUT`

A [`Float64`](@ref) that sets the value of `Base.worker_timeout()` (default: `60.0`).
This function gives the number of seconds a worker process will wait for
a master process to establish a connection before dying.

### `JULIA_NUM_THREADS`

An unsigned 64-bit integer (`uint64_t`) that sets the maximum number of threads
available to Julia. If `$JULIA_NUM_THREADS` exceeds the number of available
physical CPU cores, then the number of threads is set to the number of cores. If
`$JULIA_NUM_THREADS` is not positive or is not set, or if the number of CPU
cores cannot be determined through system calls, then the number of threads is
set to `1`.

### `JULIA_THREAD_SLEEP_THRESHOLD`

If set to a string that starts with the case-insensitive substring `"infinite"`,
then spinning threads never sleep. Otherwise, `$JULIA_THREAD_SLEEP_THRESHOLD` is
interpreted as an unsigned 64-bit integer (`uint64_t`) and gives, in
nanoseconds, the amount of time after which spinning threads should sleep.

### `JULIA_EXCLUSIVE`

If set to anything besides `0`, then Julia's thread policy is consistent with
running on a dedicated machine: the master thread is on proc 0, and threads are
affinitized. Otherwise, Julia lets the operating system handle thread policy.

## REPL formatting

Environment variables that determine how REPL output should be formatted at the
terminal. Generally, these variables should be set to [ANSI terminal escape
sequences](http://ascii-table.com/ansi-escape-sequences.php). Julia provides
a high-level interface with much of the same functionality: see the section on
[The Julia REPL](@ref).

### `JULIA_ERROR_COLOR`

The formatting `Base.error_color()` (default: light red, `"\033[91m"`) that
errors should have at the terminal.

### `JULIA_WARN_COLOR`

The formatting `Base.warn_color()` (default: yellow, `"\033[93m"`) that warnings
should have at the terminal.

### `JULIA_INFO_COLOR`

The formatting `Base.info_color()` (default: cyan, `"\033[36m"`) that info
should have at the terminal.

### `JULIA_INPUT_COLOR`

The formatting `Base.input_color()` (default: normal, `"\033[0m"`) that input
should have at the terminal.

### `JULIA_ANSWER_COLOR`

The formatting `Base.answer_color()` (default: normal, `"\033[0m"`) that output
should have at the terminal.

### `JULIA_STACKFRAME_LINEINFO_COLOR`

The formatting `Base.stackframe_lineinfo_color()` (default: bold, `"\033[1m"`)
that line info should have during a stack trace at the terminal.

### `JULIA_STACKFRAME_FUNCTION_COLOR`

The formatting `Base.stackframe_function_color()` (default: bold, `"\033[1m"`)
that function calls should have during a stack trace at the terminal.

## Debugging and profiling

### `JULIA_GC_ALLOC_POOL`, `JULIA_GC_ALLOC_OTHER`, `JULIA_GC_ALLOC_PRINT`

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

### `JULIA_GC_NO_GENERATIONAL`

If set to anything besides `0`, then the Julia garbage collector never performs
"quick sweeps" of memory.

!!! note

    This environment variable only has an effect if Julia was compiled with
    garbage-collection debugging (that is, if `WITH_GC_DEBUG_ENV` is set to `1`
    in the build configuration).

### `JULIA_GC_WAIT_FOR_DEBUGGER`

If set to anything besides `0`, then the Julia garbage collector will wait for
a debugger to attach instead of aborting whenever there's a critical error.

!!! note

    This environment variable only has an effect if Julia was compiled with
    garbage-collection debugging (that is, if `WITH_GC_DEBUG_ENV` is set to `1`
    in the build configuration).

### `ENABLE_JITPROFILING`

If set to anything besides `0`, then the compiler will create and register an
event listener for just-in-time (JIT) profiling.

!!! note

    This environment variable only has an effect if Julia was compiled with JIT
    profiling support, using either

*   Intel's [VTune™ Amplifier](https://software.intel.com/en-us/intel-vtune-amplifier-xe)
    (`USE_INTEL_JITEVENTS` set to `1` in the build configuration), or
*   [OProfile](http://oprofile.sourceforge.net/news/) (`USE_OPROFILE_JITEVENTS` set to `1`
    in the build configuration).

### `JULIA_LLVM_ARGS`

Arguments to be passed to the LLVM backend.

!!! note

    This environment variable has an effect only if Julia was compiled with
    `JL_DEBUG_BUILD` set — in particular, the `julia-debug` executable is always
    compiled with this build variable.

### `JULIA_DEBUG_LOADING`

If set, then Julia prints detailed information about the cache in the loading
process of [`Base.require`](@ref).

