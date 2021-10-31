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
$ julia script.jl
```

You can pass additional arguments to Julia, and to your program `script.jl`. A detailed list of all the available switches can be found at [Command-line Options](@ref
command-line-options).

## Resources

A curated list of useful learning resources to help new users get started can be found on the [learning](https://julialang.org/learning/) page of the main Julia website.

You can use the REPL as a learning resource by switching into the help mode.
Switch to help mode by pressing `?` at an empty `julia> ` prompt, before typing
anything else. Typing a keyword in help mode will fetch the documentation for
it, along with examples. Similarly for most functions or other objects you
might encounter!

```
help?> begin
search: begin disable_sigint reenable_sigint

  begin

  begin...end denotes a block of code.
```

If you already know Julia a bit, you might want to peek ahead at [Performance Tips](@ref man-performance-tips) and [Workflow Tips](@ref man-workflow-tips).
