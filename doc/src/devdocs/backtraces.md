# Reporting and analyzing crashes (segfaults)

So you managed to break Julia.  Congratulations!  Collected here are some general procedures you
can undergo for common symptoms encountered when something goes awry.  Including the information
from these debugging steps can greatly help the maintainers when tracking down a segfault or trying
to figure out why your script is running slower than expected.

If you've been directed to this page, find the symptom that best matches what you're experiencing
and follow the instructions to generate the debugging information requested.  Table of symptoms:

  * [Segfaults during bootstrap (`sysimg.jl`)](@ref)
  * [Segfaults when running a script](@ref)
  * [Errors during Julia startup](@ref)

## [Version/Environment info](@id dev-version-info)

No matter the error, we will always need to know what version of Julia you are running. When Julia
first starts up, a header is printed out with a version number and date. Please also include the output of `versioninfo()` (exported from the [`InteractiveUtils`](@ref InteractiveUtils.versioninfo) standard library) in any report you create:

```@repl
using InteractiveUtils
versioninfo()
```

## Segfaults during bootstrap (`sysimg.jl`)

Segfaults toward the end of the `make` process of building Julia are a common symptom of something
going wrong while Julia is preparsing the corpus of code in the `base/` folder.  Many factors
can contribute toward this process dying unexpectedly, however it is as often as not due to an
error in the C-code portion of Julia, and as such must typically be debugged with a debug build
inside of `gdb`.  Explicitly:

Create a debug build of Julia:

```
$ cd <julia_root>
$ make debug
```

Note that this process will likely fail with the same error as a normal `make` incantation, however
this will create a debug executable that will offer `gdb` the debugging symbols needed to get
accurate backtraces.  Next, manually run the bootstrap process inside of `gdb`:

```
$ cd base/
$ gdb -x ../contrib/debug_bootstrap.gdb
```

This will start `gdb`, attempt to run the bootstrap process using the debug build of Julia, and
print out a backtrace if (when) it segfaults.  You may need to hit `<enter>` a few times to get
the full backtrace.  Create a [gist](https://gist.github.com) with the backtrace, the [version info](@ref dev-version-info),
and any other pertinent information you can think of and open a new [issue](https://github.com/JuliaLang/julia/issues?q=is%3Aopen)
on Github with a link to the gist.

## Segfaults when running a script

The procedure is very similar to [Segfaults during bootstrap (`sysimg.jl`)](@ref).  Create a debug
build of Julia, and run your script inside of a debugged Julia process:

```
$ cd <julia_root>
$ make debug
$ gdb --args usr/bin/julia-debug <path_to_your_script>
```

Note that `gdb` will sit there, waiting for instructions.  Type `r` to run the process, and `bt`
to generate a backtrace once it segfaults:

```
(gdb) r
Starting program: /home/sabae/src/julia/usr/bin/julia-debug ./test.jl
...
(gdb) bt
```

Create a [gist](https://gist.github.com) with the backtrace, the [version info](@ref dev-version-info), and any
other pertinent information you can think of and open a new [issue](https://github.com/JuliaLang/julia/issues?q=is%3Aopen)
on Github with a link to the gist.

## Errors during Julia startup

Occasionally errors occur during Julia's startup process (especially when using binary distributions,
as opposed to compiling from source) such as the following:

```julia
$ julia
exec: error -5
```

These errors typically indicate something is not getting loaded properly very early on in the
bootup phase, and our best bet in determining what's going wrong is to use external tools to audit
the disk activity of the `julia` process:

  * On Linux, use `strace`:

    ```
    $ strace julia
    ```
  * On OSX, use `dtruss`:

    ```
    $ dtruss -f julia
    ```

Create a [gist](https://gist.github.com) with the `strace`/ `dtruss` output, the [version info](@ref dev-version-info),
and any other pertinent information and open a new [issue](https://github.com/JuliaLang/julia/issues?q=is%3Aopen)
on Github with a link to the gist.

## Glossary

A few terms have been used as shorthand in this guide:

  * `<julia_root>` refers to the root directory of the Julia source tree; e.g. it should contain folders
    such as `base`, `deps`, `src`, `test`, etc.....
