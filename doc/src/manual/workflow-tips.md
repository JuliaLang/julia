# [Workflow Tips](@id man-workflow-tips)

Here are some tips for working with Julia efficiently.

## REPL-based workflow

As already elaborated in [The Julia REPL](@ref), Julia's REPL provides rich functionality
that facilitates an efficient interactive workflow. Here are some tips that might further enhance
your experience at the command line.

## Command-line-based basic editor/REPL workflow

The most basic Julia workflows involve using a text editor in conjunction with the `julia` command
line. A common pattern includes the following elements:

  * **Generate a new project**

  ```
  $ julia -e 'using Pkg;Pkg.generate("Tmp")'
Generating project Tmp:
    Tmp/Project.toml
    Tmp/src/Tmp.jl
  $ ls -R Tmp
Tmp:
Project.toml  src

Tmp/src:
Tmp.jl
  $ cat -n Tmp/src/Tmp.jl
     1	module Tmp
     2
     3	greet() = print("Hello World!")
     4
     5	end # module
  ```

  * **Create a test folder**
  ```
  $ mkdir Tmp/test
  ```
  * **Put your test code in `test/runtests.jl` file.**

    ```
    $ cat -n Tmp/test/runtests.jl
     1	using Tmp
     2	Tmp.greet()
    ```

  * **Run test**
  ```
  $ julia  -e 'using Pkg;Pkg.activate("Tmp");Pkg.test()'
  Updating registry at `~/.julia/registries/General`
  Updating git-repo `https://github.com/JuliaRegistries/General.git`
 Resolving package versions...
  Updating `~/Tmp/Project.toml`
 [no changes]
   Testing Tmp
 Resolving package versions...
Hello World!   Testing Tmp tests passed
  ```
  * **Lather. Rinse. Repeat.** Explore ideas at the `julia` command prompt. Save good ideas in `Tmp.jl` and test with `runtests.jl`.

## Simplify initialization

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

## Browser-based workflow

It is also possible to interact with a Julia REPL in the browser via [IJulia](https://github.com/JuliaLang/IJulia.jl).
See the package home for details.
