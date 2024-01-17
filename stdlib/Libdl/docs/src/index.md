```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/Libdl/docs/src/index.md"
```

The Libdl module in Julia provides specialized and lower-level facilities for dynamic linking with shared libraries. While Julia
inherently supports linking to runtime shared libraries through the `ccall intrinsic, `Libdl` extends this capability by offering additional, more
granular control. It enables users to search for shared libraries both in memory and the filesystem, manually load them with specific runtime linker options, and look up
library symbols as low-level pointers.

# Dynamic Linker

```@docs
Libdl.dlopen
Libdl.dlopen_e
Libdl.RTLD_NOW
Libdl.dlsym
Libdl.dlsym_e
Libdl.dlclose
Libdl.dlext
Libdl.dllist
Libdl.dlpath
Libdl.find_library
Libdl.DL_LOAD_PATH
```
