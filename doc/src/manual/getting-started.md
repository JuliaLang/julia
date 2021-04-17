# [Getting Started](@id man-getting-started)

Julia installation is straightforward, whether using precompiled binaries or compiling from source.
Download and install Julia by following the instructions at [https://julialang.org/downloads/](https://julialang.org/downloads/).

If you are coming to Julia from one of the following languages, then you should start by reading the section on noteworthy differences from [MATLAB](@ref Noteworthy-differences-from-MATLAB), [R](@ref Noteworthy-differences-from-R), [Python](@ref Noteworthy-differences-from-Python), [C/C++](@ref Noteworthy-differences-from-C/C) or [Common Lisp](@ref Noteworthy-differences-from-Common-Lisp). This will help you avoid some common pitfalls since Julia differs from those languages in many subtle ways.

The easiest way to learn and experiment with Julia is by starting an interactive session (also
known as a read-eval-print loop or "REPL") by double-clicking the Julia executable or running
`julia` from the command line:

```@eval
io = IOBuffer()
Base.banner(io)
banner = String(take!(io))
import Markdown
Markdown.parse("```\n\$ julia\n\n$(banner)\njulia> 1 + 2\n3\n\njulia> ans\n3\n```")
```

To exit the interactive session, type `CTRL-D` (press the Control/`^` key together with the `d` key), or type
`exit()`. When run in interactive mode, `julia` displays a banner and prompts the user for input.
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

As the example implies, the following command-line arguments to `julia` are interpreted as
command-line arguments to the program `script.jl`, passed in the global constant `ARGS`. The
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
$ julia --color=yes -O -- foo.jl arg1 arg2..
```

See also [Scripting](@ref man-scripting) for more information on writing Julia scripts.

Julia can be started in parallel mode with either the `-p` or the `--machine-file` options. `-p n`
will launch an additional `n` worker processes, while `--machine-file file` will launch a worker
for each line in file `file`. The machines defined in `file` must be accessible via a password-less
`ssh` login, with Julia installed at the same location as the current host. Each machine definition
takes the form `[count*][user@]host[:port] [bind_addr[:port]]`. `user` defaults to current user,
`port` to the standard ssh port. `count` is the number of workers to spawn on the node, and defaults
to 1. The optional `bind-to bind_addr[:port]` specifies the IP address and port that other workers
should use to connect to this worker.

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

There are various ways to run Julia code and provide options, similar to those available for the
`perl` and `ruby` programs:

```
julia [switches] -- [programfile] [args...]
```

A detailed list of all the available switches can be found at [Command-line Options](@ref
command-line-options).

## Resources

A curated list of useful learning resources to help new users get started can be found on the [learning](https://julialang.org/learning/) page of the main Julia web site.

If you already know Julia a bit, you might want to peek ahead at [Performance Tips](@ref man-performance-tips) and [Workflow Tips](@ref man-workflow-tips).
