# Getting Started

Julia installation is straightforward, whether using precompiled binaries or compiling from source.
Download and install Julia by following the instructions at [http://julialang.org/downloads/](http://julialang.org/downloads/).

The easiest way to learn and experiment with Julia is by starting an interactive session (also
known as a read-eval-print loop or "repl") by double-clicking the Julia executable or running
`julia` from the command line:

```
$ julia
               _
   _       _ _(_)_     |  A fresh approach to technical computing
  (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
   _ _   _| |_  __ _   |  Type "?help" for help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 0.5.0-dev+2440 (2016-02-01 02:22 UTC)
 _/ |\__'_|_|_|\__'_|  |  Commit 2bb94d6 (11 days old master)
|__/                   |  x86_64-apple-darwin13.1.0

julia> 1 + 2
3

julia> ans
3
```

To exit the interactive session, type `^D` -- the control key together with the `d` key or type
`quit()`. When run in interactive mode, `julia` displays a banner and prompts the user for input.
Once the user has entered a complete expression, such as `1 + 2`, and hits enter, the interactive
session evaluates the expression and shows its value. If an expression is entered into an interactive
session with a trailing semicolon, its value is not shown. The variable `ans` is bound to the
value of the last evaluated expression whether it is shown or not. The `ans` variable is only
bound in interactive sessions, not when Julia code is run in other ways.

To evaluate expressions written in a source file `file.jl`, write `include("file.jl")`.

To run code in a file non-interactively, you can give it as the first argument to the `julia`
command:

```
$ julia script.jl arg1 arg2...
```

As the example implies, the following command-line arguments to `julia` are taken as command-line
arguments to the program `script.jl`, passed in the global constant `ARGS`. The name of the script
itself is passed in as the global `PROGRAM_FILE`. Note that `ARGS` is also set when script code
is given using the `-e` option on the command line (see the `julia` help output below) but `PROGRAM_FILE`
will be empty. For example, to just print the arguments given to a script, you could do this:

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

The `--` delimiter can be used to separate command-line args to the scriptfile from args to Julia:

```
$ julia --color=yes -O -- foo.jl arg1 arg2..
```

Julia can be started in parallel mode with either the `-p` or the `--machinefile` options. `-p n`
will launch an additional `n` worker processes, while `--machinefile file` will launch a worker
for each line in file `file`. The machines defined in `file` must be accessible via a passwordless
`ssh` login, with Julia installed at the same location as the current host. Each machine definition
takes the form `[count*][user@]host[:port] [bind_addr[:port]]` . `user` defaults to current user,
`port` to the standard ssh port. `count` is the number of workers to spawn on the node, and defaults
to 1. The optional `bind-to bind_addr[:port]` specifies the ip-address and port that other workers
should use to connect to this worker.

If you have code that you want executed whenever Julia is run, you can put it in `~/.juliarc.jl`:

```
$ echo 'println("Greetings! 你好! 안녕하세요?")' > ~/.juliarc.jl
$ julia
Greetings! 你好! 안녕하세요?

...
```

There are various ways to run Julia code and provide options, similar to those available for the
`perl` and `ruby` programs:

```
julia [switches] -- [programfile] [args...]
 -v, --version             Display version information
 -h, --help                Print this message

 -J, --sysimage <file>     Start up with the given system image file
 --precompiled={yes|no}    Use precompiled code from system image if available
 --compilecache={yes|no}   Enable/disable incremental precompilation of modules
 -H, --home <dir>          Set location of `julia` executable
 --startup-file={yes|no}   Load ~/.juliarc.jl
 --handle-signals={yes|no} Enable or disable Julia's default signal handlers

 -e, --eval <expr>         Evaluate <expr>
 -E, --print <expr>        Evaluate and show <expr>
 -L, --load <file>         Load <file> immediately on all processors

 -p, --procs {N|auto}      Integer value N launches N additional local worker processes
                           "auto" launches as many workers as the number of local cores
 --machinefile <file>      Run processes on hosts listed in <file>

 -i                        Interactive mode; REPL runs and isinteractive() is true
 -q, --quiet               Quiet startup (no banner)
 --color={yes|no}          Enable or disable color text
 --history-file={yes|no}   Load or save history

 --compile={yes|no|all|min}Enable or disable JIT compiler, or request exhaustive compilation
 -C, --cpu-target <target> Limit usage of cpu features up to <target>
 -O, --optimize={0,1,2,3}  Set the optimization level (default is 2 if unspecified or 3 if specified as -O)
 -g, -g <level>            Enable / Set the level of debug info generation (default is 1 if unspecified or 2 if specified as -g)
 --inline={yes|no}         Control whether inlining is permitted (overrides functions declared as @inline)
 --check-bounds={yes|no}   Emit bounds checks always or never (ignoring declarations)
 --math-mode={ieee,fast}   Disallow or enable unsafe floating point optimizations (overrides @fastmath declaration)

 --depwarn={yes|no|error}  Enable or disable syntax and method deprecation warnings ("error" turns warnings into errors)

 --output-o name           Generate an object file (including system image data)
 --output-ji name          Generate a system image data file (.ji)
 --output-bc name          Generate LLVM bitcode (.bc)
 --output-incremental=no   Generate an incremental output file (rather than complete)

 --code-coverage={none|user|all}, --code-coverage
                           Count executions of source lines (omitting setting is equivalent to "user")
 --track-allocation={none|user|all}, --track-allocation
                           Count bytes allocated by each source line
```

## Resources

In addition to this manual, there are various other resources that may help new users get started
with Julia:

  * [Julia and IJulia cheatsheet](http://math.mit.edu/~stevenj/Julia-cheatsheet.pdf)
  * [Learn Julia in a few minutes](https://learnxinyminutes.com/docs/julia/)
  * [Learn Julia the Hard Way](https://github.com/chrisvoncsefalvay/learn-julia-the-hard-way)
  * [Julia by Example](http://samuelcolvin.github.io/JuliaByExample/)
  * [Hands-on Julia](https://github.com/dpsanders/hands_on_julia)
  * [Tutorial for Homer Reid's numerical analysis class](http://homerreid.dyndns.org/teaching/18.330/JuliaProgramming.shtml)
  * [An introductory presentation](https://raw.githubusercontent.com/ViralBShah/julia-presentations/master/Fifth-Elephant-2013/Fifth-Elephant-2013.pdf)
  * [Videos from the Julia tutorial at MIT](http://julialang.org/blog/2013/03/julia-tutorial-MIT)
  * [Forio Julia Tutorials](http://forio.com/about/labs/julia-studio/tutorials/)
  * [YouTube videos from the JuliaCons](https://www.youtube.com/user/JuliaLanguage/playlists)
