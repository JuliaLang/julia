# System Image Building

## Building the Julia system image

Julia ships with a preparsed system image containing the contents of the `Base` module, named
`sys.ji`.  This file is also precompiled into a shared library called `sys.{so,dll,dylib}` on
as many platforms as possible, so as to give vastly improved startup times.  On systems that do
not ship with a precompiled system image file, one can be generated from the source files shipped
in Julia's `DATAROOTDIR/julia/base` folder.

This operation is useful for multiple reasons.  A user may:

  * Build a precompiled shared library system image on a platform that did not ship with one, thereby
    improving startup times.
  * Modify `Base`, rebuild the system image and use the new `Base` next time Julia is started.
  * Include a `userimg.jl` file that includes packages into the system image, thereby creating a system
    image that has packages embedded into the startup environment.

Julia now ships with a script that automates the tasks of building the system image, wittingly
named `build_sysimg.jl` that lives in `DATAROOTDIR/julia/`.  That is, to include it into a current
Julia session, type:

```julia
include(joinpath(JULIA_HOME, Base.DATAROOTDIR, "julia", "build_sysimg.jl"))
```

This will include a `build_sysimg()` function:

```@docs
BuildSysImg.build_sysimg
```

Note that this file can also be run as a script itself, with command line arguments taking the
place of arguments passed to the `build_sysimg` function.  For example, to build a system image
in `/tmp/sys.{so,dll,dylib}`, with the `core2` CPU instruction set, a user image of `~/userimg.jl`
and `force` set to `true`, one would execute:

```
julia build_sysimg.jl /tmp/sys core2 ~/userimg.jl --force
```
