## Juliac prototype

This implements a prototype driver for standalone compilation of julia packages, with the goals of building small binaries
and improving giving more static guarantees on the code.

# Implementation
Currently this works by collecting MethodInstances to serve as roots for the image, usually in the form of precompile statements,
and then serializing only what's reachable from these roots pruning the rest.

# Usage
For now, the driver expects either a package with a `@ccallable main` entry point,
```shell
    julia driver.jl test2.jl
```
or for shared libraries `@ccallable` entry points for the desired symbols
```shell
    julia driver.jl lib.jl
```
