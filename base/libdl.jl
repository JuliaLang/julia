# This file is a part of Julia. License is MIT: http://julialang.org/license

module Libdl

export DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL,
    RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlopen, dlopen_e, dlsym, dlsym_e,
    dlpath, find_library, dlext, dllist

"""
    DL_LOAD_PATH

When calling [`dlopen`](@ref), the paths in this list will be searched first, in
order, before searching the system locations for a valid library handle.
"""
const DL_LOAD_PATH = String[]
if is_apple()
    push!(DL_LOAD_PATH, "@loader_path/julia")
    push!(DL_LOAD_PATH, "@loader_path")
end

# note: constants to match JL_RTLD_* in src/julia.h, translated
#       to system-specific values by JL_RTLD macro in src/dlload.c
const RTLD_LOCAL     = 0x00000001
const RTLD_GLOBAL    = 0x00000002
const RTLD_LAZY      = 0x00000004
const RTLD_NOW       = 0x00000008
const RTLD_NODELETE  = 0x00000010
const RTLD_NOLOAD    = 0x00000020
const RTLD_DEEPBIND  = 0x00000040
const RTLD_FIRST     = 0x00000080

@doc """
    RTLD_DEEPBIND
    RTLD_FIRST
    RTLD_GLOBAL
    RTLD_LAZY
    RTLD_LOCAL
    RTLD_NODELETE
    RTLD_NOLOAD
    RTLD_NOW

Enum constant for [`dlopen`](@ref). See your platform man page for details, if
applicable.
""" ->
(RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL, RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW)


"""
    dlsym(handle, sym)

Look up a symbol from a shared library handle, return callable function pointer on success.
"""
function dlsym(hnd::Ptr, s::Union{Symbol,AbstractString})
    hnd == C_NULL && throw(ArgumentError("NULL library handle"))
    ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Cstring), hnd, s)
end

"""
    dlsym_e(handle, sym)

Look up a symbol from a shared library handle, silently return `NULL` pointer on lookup failure.
"""
function dlsym_e(hnd::Ptr, s::Union{Symbol,AbstractString})
    hnd == C_NULL && throw(ArgumentError("NULL library handle"))
    ccall(:jl_dlsym_e, Ptr{Void}, (Ptr{Void}, Cstring), hnd, s)
end

"""
    dlopen(libfile::AbstractString [, flags::Integer])

Load a shared library, returning an opaque handle.

The extension given by the constant `dlext` (`.so`, `.dll`, or `.dylib`)
can be omitted from the `libfile` string, as it is automatically appended
if needed.   If `libfile` is not an absolute path name, then the paths
in the array `DL_LOAD_PATH` are searched for `libfile`, followed by the
system load path.

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
function dlopen end

dlopen(s::Symbol, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    dlopen(string(s), flags)

dlopen(s::AbstractString, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    ccall(:jl_load_dynamic_library, Ptr{Void}, (Cstring,UInt32), s, flags)

"""
    dlopen_e(libfile::AbstractString [, flags::Integer])

Similar to [`dlopen`](@ref), except returns a `NULL` pointer instead of raising errors.
"""
function dlopen_e end

dlopen_e(s::Symbol, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    dlopen_e(string(s), flags)

dlopen_e(s::AbstractString, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) =
    ccall(:jl_load_dynamic_library_e, Ptr{Void}, (Cstring,UInt32), s, flags)

"""
    dlclose(handle)

Close shared library referenced by handle.
"""
function dlclose(p::Ptr)
    0 == ccall(:jl_dlclose, Cint, (Ptr{Void},), p)
end

"""
    find_library(names, locations)

Searches for the first library in `names` in the paths in the `locations` list,
`DL_LOAD_PATH`, or system library paths (in that order) which can successfully be dlopen'd.
On success, the return value will be one of the names (potentially prefixed by one of the
paths in locations). This string can be assigned to a `global const` and used as the library
name in future `ccall`'s. On failure, it returns the empty string.
"""
function find_library(libnames, extrapaths=String[])
    for lib in libnames
        for path in extrapaths
            l = joinpath(path, lib)
            p = dlopen_e(l, RTLD_LAZY)
            if p != C_NULL
                dlclose(p)
                return l
            end
        end
        p = dlopen_e(lib, RTLD_LAZY)
        if p != C_NULL
            dlclose(p)
            return lib
        end
    end
    return ""
end
find_library(libname::Union{Symbol,AbstractString}, extrapaths=String[]) =
    find_library([string(libname)], extrapaths)

function dlpath(handle::Ptr{Void})
    p = ccall(:jl_pathname_for_handle, Cstring, (Ptr{Void},), handle)
    s = unsafe_string(p)
    is_windows() && Libc.free(p)
    return s
end

function dlpath(libname::Union{AbstractString, Symbol})
    handle = dlopen(libname)
    path = dlpath(handle)
    dlclose(handle)
    return path
end

if is_apple()
    const dlext = "dylib"
elseif is_windows()
    const dlext = "dll"
else
    #assume is_linux, or similar
    const dlext = "so"
end

"""
    dlext

File extension for dynamic libraries (e.g. dll, dylib, so) on the current platform.
"""
dlext

if is_linux()
    immutable dl_phdr_info
        # Base address of object
        addr::Cuint

        # Null-terminated name of object
        name::Ptr{UInt8}

        # Pointer to array of ELF program headers for this object
        phdr::Ptr{Void}

        # Number of program headers for this object
        phnum::Cshort
    end

    # This callback function called by dl_iterate_phdr() on Linux
    function dl_phdr_info_callback(di::dl_phdr_info, size::Csize_t, dynamic_libraries::Array{AbstractString,1})
        # Skip over objects without a path (as they represent this own object)
        name = unsafe_string(di.name)
        if !isempty(name)
            push!(dynamic_libraries, name)
        end
        return convert(Cint, 0)::Cint
    end
end # linux-only

function dllist()
    dynamic_libraries = Array{AbstractString}(0)

    @static if is_linux()
        const callback = cfunction(dl_phdr_info_callback, Cint,
                                   (Ref{dl_phdr_info}, Csize_t, Ref{Array{AbstractString,1}} ))
        ccall(:dl_iterate_phdr, Cint, (Ptr{Void}, Ref{Array{AbstractString,1}}), callback, dynamic_libraries)
    end

    @static if is_apple()
        numImages = ccall(:_dyld_image_count, Cint, (), )

        # start at 1 instead of 0 to skip self
        for i in 1:numImages-1
            name = unsafe_string(ccall(:_dyld_get_image_name, Cstring, (UInt32,), i))
            push!(dynamic_libraries, name)
        end
    end

    @static if is_windows()
        ccall(:jl_dllist, Cint, (Any,), dynamic_libraries)
    end

    return dynamic_libraries
end

end # module
