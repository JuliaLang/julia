# This file is a part of Julia. License is MIT: http://julialang.org/license

# Libdl

"""
    dlopen(libfile::AbstractString [, flags::Integer])

Load a shared library, returning an opaque handle.

The optional flags argument is a bitwise-or of zero or more of `RTLD_LOCAL`, `RTLD_GLOBAL`,
`RTLD_LAZY`, `RTLD_NOW`, `RTLD_NODELETE`, `RTLD_NOLOAD`, `RTLD_DEEPBIND`, and `RTLD_FIRST`.
These are converted to the corresponding flags of the POSIX (and/or GNU libc and/or MacOS)
dlopen command, if possible, or are ignored if the specified functionality is not available
on the current platform. The default flags are platform specific. On MacOS the default
`dlopen` flags are `RTLD_LAZY|RTLD_DEEPBIND|RTLD_GLOBAL` while on other platforms the
defaults are `RTLD_LAZY|RTLD_DEEPBIND|RTLD_LOCAL`. An important usage of these flags is to
specify non default behavior for when the dynamic library loader binds library references to
exported symbols and if the bound references are put into process local or global scope. For
instance `RTLD_LAZY|RTLD_DEEPBIND|RTLD_GLOBAL` allows the library's symbols to be available
for usage in other shared libraries, addressing situations where there are dependencies
between shared libraries.
"""
Libdl.dlopen

"""
    dlclose(handle)

Close shared library referenced by handle.
"""
Libdl.dlclose

"""
    dlsym_e(handle, sym)

Look up a symbol from a shared library handle, silently return `NULL` pointer on lookup failure.
"""
Libdl.dlsym_e

"""
    dlopen_e(libfile::AbstractString [, flags::Integer])

Similar to [`dlopen`](:func:`dlopen`), except returns a `NULL` pointer instead of raising errors.
"""
Libdl.dlopen_e

"""
    find_library(names, locations)

Searches for the first library in `names` in the paths in the `locations` list,
`DL_LOAD_PATH`, or system library paths (in that order) which can successfully be dlopen'd.
On success, the return value will be one of the names (potentially prefixed by one of the
paths in locations). This string can be assigned to a `global const` and used as the library
name in future `ccall`'s. On failure, it returns the empty string.
"""
Libdl.find_library

"""
    dlsym(handle, sym)

Look up a symbol from a shared library handle, return callable function pointer on success.
"""
Libdl.dlsym
