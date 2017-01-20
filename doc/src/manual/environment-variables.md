# Environment Variables in Julia

The environment variables that Julia uses generally start with `JULIA`. If [`Base.versioninfo()`](https://github.com/JuliaLang/julia/blob/master/base/interactiveutil.jl#L247) is called with `verbose` equal to `true`, then the output will list all environment variables for which `JULIA` appears in the name.

## File locations

### `JULIA_HOME`
The absolute path of the directory containing the Julia executable, which sets the global variable [`Base.JULIA_HOME`](https://github.com/JuliaLang/julia/blob/master/base/initdefs.jl#L90).  If `$JULIA_HOME` is not set, then `Base.JULIA_HOME` defaults to its compile-time value.

The executable itself is one of 
```
$JULIA_HOME/julia
$JULIA_HOME/julia-debug
```
by default.

The global variable [`Base.DATAROOTDIR`](https://github.com/JuliaLang/julia/blob/master/base/Makefile#L62)  determines a relative path from `Base.JULIA_HOME` to the data directory associated with Julia; then the directory.  Then the path 
```
$JULIA_HOME/$DATAROOTDIR/julia/base
```
determines the directory in which Julia initially searches for source files (via [`Base.find_source_file()`](https://github.com/JuliaLang/julia/blob/master/base/loading.jl#L124)).

Likewise, the global variable [`Base.SYSCONFDIR`](https://github.com/JuliaLang/julia/blob/master/base/Makefile#L61) internal variable determines a relative path to the configuration file directory. Then Julia searches for a `juliarc.jl` file at

```
$JULIA_HOME/$SYSCONFDIR/julia/juliarc.jl
$JULIA_HOME/../etc/julia/juliarc.jl
```
by default (via [`Base.load_juliarc()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L316) ).

For example, a Linux installation with a Julia executable located at `/bin/julia`, a `DATAROOTDIR` of `../share`, and a `SYSCONFDIR` of `../etc` will have `JULIA_HOME` set to `/bin`, a source-file search path of 
```
/share/julia/base
```
and a global configuration search path of
```
/etc/julia/juliarc.jl
```

### `JULIA_LOAD_PATH`
A separated list of absolute paths that are to be appended to the variable  [`Base.LOAD_PATH`](https://github.com/JuliaLang/julia/blob/master/base/initdefs.jl#L35). (In Unix-like systems, the path separator is `:`; in windows systems, the path separator is `;`.) The `Base.LOAD_PATH` variable is where [`Base.require()`](https://github.com/JuliaLang/julia/blob/master/base/require.jl#L41) and [`Base.load_in_path()`](https://github.com/JuliaLang/julia/blob/master/base/require.jl#L6) look for code;  it defaults to the absolute paths

```
$JULIA_HOME/../local/share/julia/site/v$(Base.VERSION.major).$(Base.VERSION.minor)
$JULIA_HOME/../share/julia/site/v$(Base.VERSION.major).$(Base.VERSION.minor)
```
so that, e.g., version 0.5 of Julia on a Linux system with a Julia executable at `/bin/julia` will have a default `Base.LOAD_PATH` of
```
/local/share/julia/site/v0.5
/share/julia/site/v0.5
```

### `JULIA_PKGDIR`
The path of the parent directory [`Dir._pkgroot()`](https://github.com/JuliaLang/julia/blob/master/base/pkg/dir.jl#L10) for the version-specific Julia package repositories. If the path is relative, then it is taken with respect to the working directory. If `JULIA_PKGIR` is not specified, then `_pkgroot()` defaults to
```
$HOME/.julia
```
Then the repository location [`Pkg.dir()`](https://github.com/JuliaLang/julia/blob/master/base/pkg/pkg.jl#L50) for a given Julia version is
```
$JULIA_PKGDIR/v$(Base.VERSION.major).$(Base.VERSION.minor)
```

For example, for a Linux user whose home directory is `/home/alice`, the directory containing the package repositories would by default be
```
/home/alice/.julia
```
and the package repository for version 0.5 of Julia would be
```
/home/alice/.julia/v0.5
```

### `JULIA_HISTORY`
The absolute path [`Base.find_hist_file()`](https://github.com/JuliaLang/julia/blob/master/base/REPL.jl#L615) of the REPL's history file. If `JULIA_HISTORY` is not specified, then `Base.find_hist_file()` defaults to
```
$HOME/.julia_history
```

### `JULIA_PKGRESOLVE_ACCURACY`
A positive `Int` that determines how much time the max-sum subroutine [`MaxSum.maxsum()`](https://github.com/JuliaLang/julia/blob/master/base/pkg/resolve/maxsum.jl#L442) of the package dependency resolver [`Resolve.resolve()`](https://github.com/JuliaLang/julia/blob/master/base/pkg/resolve.jl#L15)  will devote to attempting satisfying constraints before giving up: this value is by default `1`, and larger values correspond to larger amounts of time.

Suppose the value of `JULIA_PKGRESOLVE_ACCURACY` is `n`. Then

*   the number of pre-decimation iterations is `20*n`,
*   the number of iterations between decimation steps is `10*n`, and
*   at decimation steps, at most one in every `20*n` packages is decimated. 

## External applications

### `JULIA_SHELL`
The absolute path of the shell with which Julia should execute external commands (via [`Base.repl_cmd()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L91)). Defaults to the environment variable `$SHELL`, and falls back to `/bin/sh` if `$SHELL` is unset. 

*Note:* the `fish` shell is unsupported.

### `JULIA_EDITOR`, `VISUAL`, `EDITOR`
The editor returned by [`Base.editor()`](https://github.com/JuliaLang/julia/blob/master/base/interactiveutil.jl#L12) and used in, e.g., [`Base.edit()`](https://github.com/JuliaLang/julia/blob/master/base/interactiveutil.jl#L33). `JULIA_EDITOR` takes precedence over `VISUAL`, which in turn takes precedence over `EDITOR`. If none of these environment variables is set, then the editor is taken to be `open` on Windows and OS X, or `/etc/alternatives/editor` if it exists, or `emacs` otherwise.

**Warning:** `JULIA_EDITOR` is *not* used in the determination of the editor for [`Pkg.edit()`](https://github.com/JuliaLang/julia/blob/master/base/pkg/entry.jl#L36): this function checks `VISUAL` and `EDITOR` alone.

## Parallelization

### `JULIA_CPU_CORES`
Overrides the global variable [`Sys.CPU_CORES`](https://github.com/JuliaLang/julia/blob/master/base/sysinfo.jl#L62), the number of logical CPU cores available. 

### `JULIA_WORKER_TIMEOUT`
A `Float64` that sets the value of [`Base.worker_timeout()`](https://github.com/JuliaLang/julia/blob/master/base/multi.jl#L1566) (default: `60.0`). This function gives the number of a seconds a worker process will wait for a master process to establish a connection before dying.

## REPL formatting
Environment variables that determine how REPL output should be formatted at the terminal. Generally, these variables should be set to [ANSI terminal escape sequences](http://ascii-table.com/ansi-escape-sequences.php).

###   `JULIA_ERROR_COLOR`
The formatting [`Base.error_color()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L80) (default: light red, `"\033[91m"`) that errors should have at the terminal.

###   `JULIA_WARN_COLOR`
The formatting [`Base.warn_color()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L81) (default: yellow, `"\033[93m"`) that warnings should have at the terminal.

###   `JULIA_INFO_COLOR`
The formatting [`Base.info_color()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L82) (default: cyan, `"\033[36m"`) that info should have at the terminal.

###   `JULIA_INPUT_COLOR`
The formatting [`Base.input_color()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L85) (default: normal, `"\033[0m"`) that input should have at the terminal. 

*Note:* Input is hardcoded to be bold (`"\033[1m"`).

###   `JULIA_ANSWER_COLOR`
The formatting [`Base.answer_color()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L86) (default: normal, `"\033[0m"`) that output should have at the terminal.

*Note:* Output is hardcoded to be bold (`"\033[1m"`).

###   `JULIA_STACKFRAME_LINEINFO_COLOR`
The formatting [`Base.stackframe_lineinfo_color()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L88) (default: bold, `"\033[1m"`) that line info should have during a stack trace at the terminal.

###   `JULIA_STACKFRAME_FUNCTION_COLOR`
The formatting [`Base.stackframe_function_color()`](https://github.com/JuliaLang/julia/blob/master/base/client.jl#L89) (default: bold, `"\033[1m"`) that function calls should have during a stack trace at the terminal.

## Debugging

### `JULIA_DEBUG_LOADING`
If set, then Julia prints detailed information about the cache in the loading process of [`Base.require()`](https://github.com/JuliaLang/julia/blob/master/base/loading.jl#L373).
