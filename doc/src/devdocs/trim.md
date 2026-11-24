# Trimming

System images are typically quite large, since Base includes a lot of functionality, and by
default system images also include several packages such as LinearAlgebra for convenience
and backwards compatibility. Most programs will use only a fraction of the functions in
these packages. Therefore it makes sense to build binaries that exclude unused functions
to save space, referred to as "trimming".

While the basic idea of trimming is sound, Julia has dynamic and reflective features that make it
difficult (or impossible) to know in general which functions are unused. As an extreme example,
consider code like

```
getglobal(Base, Symbol(readchomp(stdin)))(1)
```

This code reads a function name from `stdin` and calls the named function from Base on the value
`1`. In this case it is impossible to predict which function will be called, so no functions
can reliably be considered "unused". With some noteworthy exceptions (Julia's own REPL being
one of them), most real-world programs do not do things like this.

Less extreme cases occur, for example, when there are type instabilities that make it impossible
for the compiler to predict which method will be called. However, if code is well-typed and does
not use reflection, a complete and (hopefully) relatively small set of needed methods can be
determined, and the rest can be removed. The `--trim` command-line option requests this kind of
compilation.

When `--trim` is specified in a command used to build a system image, the compiler begins
tracing calls starting at methods marked using `Base.Experimental.entrypoint`. If a call is too
dynamic to reasonably narrow down the possible call targets, an error is given at compile
time showing the location of the call. For testing purposes, it is possible to skip these
errors by specifying `--trim=unsafe` or `--trim=unsafe-warn`. Then you will get a system
image built, but it may crash at run time if needed code is not present.

It typically makes sense to specify `--strip-ir` along with `--trim`, since trimmed binaries
are fully compiled and therefore don't need Julia IR. At some point we may make `--trim` imply
`--strip-ir`, but for now we have kept them orthogonal.

To get the smallest possible binary, it will also help to specify `--strip-metadata` and
run the Unix `strip` utility. However, those steps remove Julia-specific and native (DWARF format)
debug info, respectively, and so will make debugging more difficult.

## Common problems

- The Base global variables `stdin`, `stdout`, and `stderr` are non-constant and so their
  types are not known. All printing should use a specific IO object with a known type.
  The easiest substitution is to use `print(Core.stdout, x)` instead of `print(x)` or
  `print(stdout, x)`.
- Use tools like [JET.jl](https://github.com/aviatesk/JET.jl),
  [Cthulhu.jl](https://github.com/JuliaDebug/Cthulhu.jl), and/or
  [SnoopCompile](https://github.com/timholy/SnoopCompile.jl)
  to identify failures of type-inference, and follow our [Performance Tips](@ref man-performance-tips) to fix them.

## Compatibility concerns

We have identified many small changes to Base that significantly increase the set of programs
that can be reliably trimmed. Unfortunately some of those changes would be considered breaking,
and so are only applied when trimming is requested (this is done by an external build script,
currently maintained inside the test suite as `contrib/juliac/juliac-buildscript.jl`).
Therefore in many cases trimming will require you to opt in to new variants of Base and some
standard libraries.

If you want to use trimming, it is important to set up continuous integration testing that
performs a trimmed build and fully tests the resulting program.
Fortunately, if your program successfully compiles with `--trim` then it is very likely to work
the same as it did before. However, CI is needed to ensure that your program continues to build
with trimming as you develop it.

Package authors may wish to test that their package is "trimming safe", however this is impossible
in general. Trimming is only expected to work given concrete entry points such as `main()` and
library entry points meant to be called from outside Julia. For generic packages, existing tests
for type stability like `@inferred` and `JET.@report_call` are about as close as you can get to checking
trim compatibility.

Trimming also introduces new compatibility issues between minor versions of Julia. At this time,
we are not able to guarantee that a program that can be trimmed in one version of Julia
can also be trimmed in all future versions of Julia. However, breakage of that kind is expected
to be rare. We also plan to try to *increase* the set of programs that can be trimmed over time.
