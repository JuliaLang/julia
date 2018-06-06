# [Workflow Tips](@id man-workflow-tips)

Here are some tips for working with Julia efficiently.

## REPL-based workflow

As already elaborated in [The Julia REPL](@ref), Julia's REPL provides rich functionality
that facilitates an efficient interactive workflow. Here are some tips that might further enhance
your experience at the command line.

### A basic editor/REPL workflow

The most basic Julia workflows involve using a text editor in conjunction with the `julia` command
line. A common pattern includes the following elements:

  * **Put code under development in a temporary module.** Create a file, say `Tmp.jl`, and include
    within it

    ```
    module Tmp

    <your definitions here>

    end
    ```
  * **Put your test code in another file.** Create another file, say `tst.jl`, which begins with

    ```julia
    import Tmp
    ```

    and includes tests for the contents of `Tmp`.
    Alternatively, you can wrap the contents of your test file in a module, as

    ```
    module Tst
        using Tmp

        <scratch work>

    end
    ```

    The advantage is that you can now do `using Tmp` in your test code and can therefore avoid prepending
    `Tmp.` everywhere. The disadvantage is that code can no longer be selectively copied to the REPL
    without some tweaking.
  * **Lather. Rinse. Repeat.** Explore ideas at the `julia` command prompt. Save good ideas in `tst.jl`.

### Simplify initialization

To simplify restarting the REPL, put project-specific initialization code in a file, say `_init.jl`,
which you can run on startup by issuing the command:

```
julia -L _init.jl
```

If you further add the following to your `~/.julia/config/startup.jl` file

```julia
isfile("_init.jl") && include(joinpath(pwd(), "_init.jl"))
```

then calling `julia` from that directory will run the initialization code without the additional
command line argument.

## More intensive software development with the REPL

To configure your environment for more intensive software development, you should first install
the `Revise` package, which scans your source files continuously for changes and reloads and recompiles any changed
files automatically.  Install it as follows:
```julia
using Pkg
Pkg.add("Revise.jl")
```
Most likely you will want to run this package for every invocation of the REPL, in which case
you can insert the statement `using Revise` in an init file or the startup file described in the preceding
subsection.

Next, to begin a new project with the REPL, change to your working development directory.  Again, there
are several ways to make sure Julia starts in a certain directory.  This working development
directory should be in the [`LOAD_PATH`](@ref) variable; refer to the chapter on Environment Variables.
Enter `pkg` mode by pressing `]` at the REPL prompt. From this prompt, you can initialize a new project by
typing `generate <PROJNAME>`.  This will create a skeletal file hierarchy for your project. Put your source
files in the `<PROJNAME>/src` directory.  The benefits of following this structure are (1) if your project
needs external packages, which almost certainly it will, then the Package manager understands the file
hierarchy created by `generate`, and (2) if you ever decide to release your project as a downloadable package,
it is already in the correct format.  Refer to the chapter on `Pkg` of the standard library documentation.


## Browser-based workflow

It is also possible to interact with a Julia REPL in the browser via [IJulia](https://github.com/JuliaLang/IJulia.jl).
See the package home for details.
