# This file is a part of Julia. License is MIT: https://julialang.org/license
"""
    Libdl

Lower-level control of shared libraries than `ccall` gives. Find libraries with
[`Libdl.find_library`](@ref) and [`Libdl.dlpath`](@ref), load them with runtime linker
options using [`Libdl.dlopen`](@ref), and look up symbols as pointers with
[`Libdl.dlsym`](@ref).
"""
module Libdl
# Just re-export Base.Libc.Libdl:
export DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL,
    RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlopen, dlopen_e, dlsym, dlsym_e,
    dlpath, find_library, dlext, dllist, LazyLibrary, LazyLibraryPath, BundledLazyLibraryPath

public add_dependency!, LazyLibraryCallback

import Base.Libc.Libdl: DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL,
                        RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlopen, dlopen_e, dlsym, dlsym_e,
                        dlpath, find_library, dlext, dllist, LazyLibrary, LazyLibraryPath,
                        BundledLazyLibraryPath, LazyLibraryCallback, default_rtld_flags, add_dependency!

end # module
