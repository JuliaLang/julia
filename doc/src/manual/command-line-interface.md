# [Command-line Interface](@id cli)

## Using arguments inside scripts

When running a script using `julia`, you can pass additional arguments to your script:

```
$ julia script.jl arg1 arg2...
```

These additional command-line arguments are passed in the global constant `ARGS`. The
name of the script itself is passed in as the global `PROGRAM_FILE`. Note that `ARGS` is
also set when a Julia expression is given using the `-e` option on the command line (see the
`julia` help output below) but `PROGRAM_FILE` will be empty. For example, to just print the
arguments given to a script, you could do this:

```
$ julia -e 'println(PROGRAM_FILE); for x in ARGS; println(x); end' foo bar

foo
bar
```

Or you could put that code into a script and run it:

```
$ echo 'println(PROGRAM_FILE); for x in ARGS; println(x); end' > script.jl
$ julia script.jl foo bar
script.jl
foo
bar
```

The `--` delimiter can be used to separate command-line arguments intended for the script file from arguments intended for Julia:

```
$ julia --color=yes -O -- script.jl arg1 arg2..
```

See also [Scripting](@ref man-scripting) for more information on writing Julia scripts.

## The `Main.main` entry point

As of Julia, 1.11, `Base` exports the macro `@main`. This macro expands to the symbol `main`,
but at the conclusion of executing a script or expression, `julia` will attempt to execute
`Main.main(Base.ARGS)` if such a function `Main.main` has been defined and this behavior was opted into
by using the `@main` macro.

This feature is intended to aid in the unification
of compiled and interactive workflows. In compiled workflows, loading the code that defines the `main`
function may be spatially and temporally separated from the invocation. However, for interactive workflows,
the behavior is equivalent to explicitly calling `exit(main(ARGS))` at the end of the evaluated script or
expression.

!!! compat "Julia 1.11"
    The special entry point `Main.main` was added in Julia 1.11. For compatibility with prior julia versions,
    add an explicit `@isdefined(var"@main") ? (@main) : exit(main(ARGS))` at the end of your scripts.

To see this feature in action, consider the following definition, which will execute the print function despite there being no explicit call to `main`:

```
$ julia -e '(@main)(args) = println("Hello World!")'
Hello World!
$
```

Only the `main` binding in the `Main` module has this behavior and only if
the macro `@main` was used within the defining module.

For example, using `hello` instead of `main` will not result in the `hello` function executing:

```
$ julia -e 'hello(args) = println("Hello World!")'
$
```

and neither will a plain definition of `main`:
```
$ julia -e 'main(args) = println("Hello World!")'
$
```

However, the opt-in need not occur at definition time:
```
$ julia -e 'main(args) = println("Hello World!"); @main'
Hello World!
$
```

The `main` binding may be imported from a package. A *hello world* package defined as

```
module Hello

export main
(@main)(args) = println("Hello from the package!")

end
```

may be used as:

```
$ julia -e 'using Hello'
Hello from the package!
$ julia -e 'import Hello' # N.B.: Execution depends on the binding not whether the package is loaded
$
```

However, note that the current best practice recommendation is to not mix application and reusable library
code in the same package. Helper applications may be distributed as separate packages or as scripts with
separate `main` entry points in a package's `bin` folder.

## Parallel mode

Julia can be started in parallel mode with either the `-p` or the `--machine-file` options. `-p n`
will launch an additional `n` worker processes, while `--machine-file file` will launch a worker
for each line in file `file`. The machines defined in `file` must be accessible via a password-less
`ssh` login, with Julia installed at the same location as the current host. Each machine definition
takes the form `[count*][user@]host[:port] [bind_addr[:port]]`. `user` defaults to current user,
`port` to the standard ssh port. `count` is the number of workers to spawn on the node, and defaults
to 1. The optional `bind-to bind_addr[:port]` specifies the IP address and port that other workers
should use to connect to this worker.


## Startup file

If you have code that you want executed whenever Julia is run, you can put it in
`~/.julia/config/startup.jl`:

```
$ echo 'println("Greetings! 你好! 안녕하세요?")' > ~/.julia/config/startup.jl
$ julia
Greetings! 你好! 안녕하세요?

...
```

Note that although you should have a `~/.julia` directory once you've run Julia for the
first time, you may need to create the `~/.julia/config` folder and the
`~/.julia/config/startup.jl` file if you use it.

To have startup code run only in [The Julia REPL](@ref) (and not when `julia` is *e.g.* run
on a script), use [`atreplinit`](@ref) in `startup.jl`:

```julia
atreplinit() do repl
    # ...
end
```
If [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH) is set, the startup file should be located there
`$JULIA_DEPOT_PATH/config/startup.jl`.

## [Command-line switches for Julia](@id command-line-interface)

There are various ways to run Julia code and provide options, similar to those available for the
`perl` and `ruby` programs:

```
julia [switches] -- [programfile] [args...]
```

The following is a complete list of command-line switches available when launching julia (a '*' marks the default value, if applicable; settings marked '($)' may trigger package precompilation):

|Switch                                 |Description|
|:---                                   |:---|
|`-v`, `--version`                      |Display version information|
|`-h`, `--help`                         |Print command-line options (this message)|
|`--help-hidden`                        |Print uncommon options not shown by `-h`|
|`--project[={<dir>\|@temp\|@.}]`       |Set `<dir>` as the active project/environment. Or, create a temporary environment with `@temp`. The default `@.` option will search through parent directories until a `Project.toml` or `JuliaProject.toml` file is found.|
|`-J`, `--sysimage <file>`              |Start up with the given system image file|
|`-H`, `--home <dir>`                   |Set location of `julia` executable|
|`--startup-file={yes*\|no}`            |Load `JULIA_DEPOT_PATH/config/startup.jl`; if [`JULIA_DEPOT_PATH`](@ref JULIA_DEPOT_PATH) environment variable is unset, load `~/.julia/config/startup.jl`|
|`--handle-signals={yes*\|no}`          |Enable or disable Julia's default signal handlers|
|`--sysimage-native-code={yes*\|no}`    |Use native code from system image if available|
|`--compiled-modules={yes*\|no\|existing\|strict}` |Enable or disable incremental precompilation of modules. The `existing` option allows use of existing compiled modules that were previously precompiled, but disallows creation of new precompile files. The `strict` option is similar, but will error if no precompile file is found. |
|`--pkgimages={yes*\|no\|existing}`     |Enable or disable usage of native code caching in the form of pkgimages. The `existing` option allows use of existing pkgimages but disallows creation of new ones|
|`-e`, `--eval <expr>`                  |Evaluate `<expr>`|
|`-E`, `--print <expr>`                 |Evaluate `<expr>` and display the result|
|`-m`, `--module <Package> [args]`      |Run entry point of `Package` (`@main` function) with `args'|
|`-L`, `--load <file>`                  |Load `<file>` immediately on all processors|
|`-t`, `--threads {auto\|N[,auto\|M]}`  |Enable N[+M] threads; N threads are assigned to the `default` threadpool, and if M is specified, M threads are assigned to the `interactive` threadpool; `auto` tries to infer a useful default number of threads to use but the exact behavior might change in the future. Currently sets N to the number of CPUs assigned to this Julia process based on the OS-specific affinity assignment interface if supported (Linux and Windows) or to the number of CPU threads if not supported (MacOS) or if process affinity is not configured, and sets M to 1.|
| `--gcthreads=N[,M]`                   |Use N threads for the mark phase of GC and M (0 or 1) threads for the concurrent sweeping phase of GC. N is set to the number of compute threads and M is set to 0 if unspecified.|
|`-p`, `--procs {N\|auto}`              |Integer value N launches N additional local worker processes; `auto` launches as many workers as the number of local CPU threads (logical cores)|
|`--machine-file <file>`                |Run processes on hosts listed in `<file>`|
|`-i`, `--interactive`                  |Interactive mode; REPL runs and `isinteractive()` is true|
|`-q`, `--quiet`                        |Quiet startup: no banner, suppress REPL warnings|
|`--banner={yes\|no\|short\|auto*}`     |Enable or disable startup banner|
|`--color={yes\|no\|auto*}`             |Enable or disable color text|
|`--history-file={yes*\|no}`            |Load or save history|
|`--depwarn={yes\|no*\|error}`          |Enable or disable syntax and method deprecation warnings (`error` turns warnings into errors)|
|`--warn-overwrite={yes\|no*}`          |Enable or disable method overwrite warnings|
|`--warn-scope={yes*\|no}`              |Enable or disable warning for ambiguous top-level scope|
|`-C`, `--cpu-target <target>`          |Limit usage of CPU features up to `<target>`; set to `help` to see the available options|
|`-O`, `--optimize={0\|1\|2*\|3}`       |Set the optimization level (level is 3 if `-O` is used without a level) ($)|
|`--min-optlevel={0*\|1\|2\|3}`         |Set the lower bound on per-module optimization|
|`-g`, `--debug-info={0\|1*\|2}`        |Set the level of debug info generation (level is 2 if `-g` is used without a level) ($)|
|`--inline={yes*\|no}`                  |Control whether inlining is permitted, including overriding `@inline` declarations|
|`--check-bounds={yes\|no\|auto*}`      |Emit bounds checks always, never, or respect `@inbounds` declarations ($)|
|`--math-mode={ieee\|user*}`            |Always follow `ieee` floating point semantics or respect `@fastmath` declarations|
|`--polly={yes*\|no}`                   |Enable or disable the polyhedral optimizer Polly (overrides @polly declaration)|
|`--code-coverage[={none*\|user\|all}]` |Count executions of source lines (omitting setting is equivalent to `user`)|
|`--code-coverage=@<path>`              |Count executions but only in files that fall under the given file path/directory. The `@` prefix is required to select this option. A `@` with no path will track the current directory.|
|`--code-coverage=tracefile.info`       |Append coverage information to the LCOV tracefile (filename supports format tokens).|
|`--track-allocation[={none*\|user\|all}]` |Count bytes allocated by each source line (omitting setting is equivalent to "user")|
|`--track-allocation=@<path>`           |Count bytes but only in files that fall under the given file path/directory. The `@` prefix is required to select this option. A `@` with no path will track the current directory.|
|`--task-metrics={yes\|no*}`             |Enable the collection of per-task metrics|
|`--bug-report=KIND`                    |Launch a bug report session. It can be used to start a REPL, run a script, or evaluate expressions. It first tries to use BugReporting.jl installed in current environment and falls back to the latest compatible BugReporting.jl if not. For more information, see `--bug-report=help`.|
|`--heap-size-hint=<size>`              |Forces garbage collection if memory usage is higher than the given value. The value may be specified as a number of bytes, optionally in units of KB, MB, GB, or TB, or as a percentage of physical memory with %.|
|`--compile={yes*\|no\|all\|min}`       |Enable or disable JIT compiler, or request exhaustive or minimal compilation|
|`--output-o <name>`                    |Generate an object file (including system image data)|
|`--output-ji <name>`                   |Generate a system image data file (.ji)|
|`--strip-metadata`                     |Remove docstrings and source location info from system image|
|`--strip-ir`                           |Remove IR (intermediate representation) of compiled functions|
|`--output-unopt-bc <name>`             |Generate unoptimized LLVM bitcode (.bc)|
|`--output-bc <name>`                   |Generate LLVM bitcode (.bc)|
|`--output-asm <name>`                  |Generate an assembly file (.s)|
|`--output-incremental={yes\|no*}`      |Generate an incremental output file (rather than complete)|
|`--trace-compile={stderr\|name}`       |Print precompile statements for methods compiled during execution or save to stderr or a path. Methods that were recompiled are printed in yellow or with a trailing comment if color is not supported|
|`--trace-compile-timing`               |If `--trace-compile` is enabled show how long each took to compile in ms|
|`--trace-dispatch={stderr\|name}`      |Print precompile statements for methods dispatched during execution or save to stderr or a path.|
|`--image-codegen`                      |Force generate code in imaging mode|
|`--permalloc-pkgimg={yes\|no*}`        |Copy the data section of package images into memory|
|`--trim={no*\|safe\|unsafe\|unsafe-warn}` |Build a sysimage including only code provably reachable from methods marked by calling `entrypoint`. The three non-default options differ in how they handle dynamic call sites. In safe mode, such sites result in compile-time errors. In unsafe mode, such sites are allowed but the resulting binary might be missing needed code and can throw runtime errors. With unsafe-warn, such sites will trigger warnings at compile-time and might error at runtime.|

Options that have the form `--option={...}` can be specified either as `--option=value` or as `--option value`. For example, `julia --banner=no` is equivalent to `julia --banner no`. This is especially relevant for options that take a filename for output, because forgetting to specifying the argument for (say) `--trace-compile` will cause the option following it to be interpreted as the filename, possibly unintentionally overwriting it.

Note that options of the form `--option[=...]` can **not** be specified as `--option value`, but only as `--option=value` (or simply `--option`, when no argument is provided).

!!! compat "Julia 1.1"
    In Julia 1.0, the default `--project=@.` option did not search up from the root
    directory of a Git repository for the `Project.toml` file. From Julia 1.1 forward, it
    does.
