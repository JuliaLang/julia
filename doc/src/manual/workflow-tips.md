# [Workflow Tips](@id man-workflow-tips)

Here are some tips for working with Julia efficiently.

## REPL-based workflow

As already elaborated in [Interacting With Julia](@ref), Julia's REPL provides rich functionality
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

    and includes tests for the contents of `Tmp`. The value of using `import` versus `using` is that
    you can call `reload("Tmp")` instead of having to restart the REPL when your definitions change.
    Of course, the cost is the need to prepend `Tmp.` to uses of names defined in your module. (You
    can lower that cost by keeping your module name short.)

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
    Occasionally restart the REPL, issuing

    ```julia
    reload("Tmp")
    include("tst.jl")
    ```

### Simplify initialization

To simplify restarting the REPL, put project-specific initialization code in a file, say `_init.jl`,
which you can run on startup by issuing the command:

```
julia -L _init.jl
```

If you further add the following to your `.juliarc.jl` file

```julia
isfile("_init.jl") && include(joinpath(pwd(), "_init.jl"))
```

then calling `julia` from that directory will run the initialization code without the additional
command line argument.

## Browser-based workflow

It is also possible to interact with a Julia REPL in the browser via [IJulia](https://github.com/JuliaLang/IJulia.jl).
See the package home for details.
