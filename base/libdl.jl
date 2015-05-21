# This file is a part of Julia. License is MIT: http://julialang.org/license

module Libdl

export DL_LOAD_PATH, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL,
    RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, dlclose, dlopen, dlopen_e, dlsym, dlsym_e,
    dlpath, find_library, dlext, dllist

const DL_LOAD_PATH = ByteString[]
@osx_only push!(DL_LOAD_PATH, "@executable_path/../lib/julia")
@osx_only push!(DL_LOAD_PATH, "@executable_path/../lib")

# constants to match JL_RTLD_* in src/julia.h
const RTLD_LOCAL     = 0x00000000
const RTLD_GLOBAL    = 0x00000001
const RTLD_LAZY      = 0x00000002
const RTLD_NOW       = 0x00000004
const RTLD_NODELETE  = 0x00000008
const RTLD_NOLOAD    = 0x00000010
const RTLD_DEEPBIND  = 0x00000020
const RTLD_FIRST     = 0x00000040

type DLHandle
    ptr::Ptr{Void}
end

==(hnd1::DLHandle, hnd2::DLHandle) = hnd1.ptr == hnd2.ptr

isopen(hnd::DLHandle) = hnd.ptr != C_NULL

function dlsym(hnd::DLHandle, s::Union(Symbol,AbstractString))
    isopen(hnd) || error("NULL library handle")
    ccall(:jl_dlsym, Ptr{Void}, (Ptr{Void}, Cstring), hnd.ptr, s)
end

function dlsym_e(hnd::DLHandle, s::Union(Symbol,AbstractString))
    isopen(hnd) || error("NULL library handle")
    ccall(:jl_dlsym_e, Ptr{Void}, (Ptr{Void}, Cstring), hnd.ptr, s)
end

dlopen(s::Symbol, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) = dlopen(string(s), flags)

function dlopen(s::AbstractString, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND)
    hnd = DLHandle(ccall(:jl_load_dynamic_library, Ptr{Void}, (Cstring,UInt32), s, flags))
    finalizer(hnd, finalize_dlhandle)
    return hnd
end

function dlopen_e(s::AbstractString, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND)
    hnd = DLHandle(ccall(:jl_load_dynamic_library_e, Ptr{Void}, (Cstring,UInt32), s, flags))
    finalizer(hnd, finalize_dlhandle)
    return hnd
end

dlopen_e(s::Symbol, flags::Integer = RTLD_LAZY | RTLD_DEEPBIND) = dlopen_e(string(s), flags)

function finalize_dlhandle(hnd::DLHandle)
    if hnd.ptr != C_NULL
        Libc.free(hnd.ptr)
        hnd.ptr = C_NULL
    end
end

function dlclose(hnd::DLHandle)
    if hnd.ptr != C_NULL
        ccall(:uv_dlclose,Void,(Ptr{Void},),hnd.ptr)
        Libc.free(hnd.ptr)
        hnd.ptr = C_NULL
    end
end

function find_library{T<:ByteString, S<:ByteString}(libnames::Array{T,1}, extrapaths::Array{S,1}=ASCIIString[])
    for lib in libnames
        for path in extrapaths
            l = joinpath(path, lib)
            p = dlopen_e(l, RTLD_LAZY)
            if isopen(p)
                dlclose(p)
                return l
            end
        end
        p = dlopen_e(lib, RTLD_LAZY)
        if isopen(p)
            dlclose(p)
            return lib
        end
    end
    return ""
end

function dlpath(handle::DLHandle)
    p = ccall( :jl_pathname_for_handle, Ptr{UInt8}, (Ptr{Void},), handle.ptr )
    s = bytestring(p)
    @windows_only Libc.free(p)
    return s
end

function dlpath(libname::Union(AbstractString, Symbol))
    handle = dlopen(libname)
    path = dlpath(handle)
    dlclose(handle)
    return path
end

# For now we support the old API that used a raw uv_lib_t* as the
# handle.  In particular, BinDeps currently relies on this API.
# TODO: Deprecate these methods.
dlsym(hnd::Ptr, s::Union(Symbol,AbstractString)) = dlsym(DLHandle(hnd), s)
dlsym_e(hnd::Ptr, s::Union(Symbol,AbstractString)) = dlsym_e(DLHandle(hnd), s)
dlclose(hnd::Ptr) = dlclose(DLHandle(hnd))
dlpath(hnd::Ptr) = dlpath(DLHandle(hnd))
# (Typically used for comparison with C_NULL)
==(hnd::DLHandle, ptr::Ptr{Void}) = hnd.ptr == ptr
==(ptr::Ptr{Void}, hnd::DLHandle) = hnd.ptr == ptr

if OS_NAME === :Darwin
    const dlext = "dylib"
elseif OS_NAME === :Windows
    const dlext = "dll"
else
    #assume OS_NAME === :Linux, or similar
    const dlext = "so"
end

@linux_only begin
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
        name = bytestring(di.name)
        if !isempty(name)
            push!(dynamic_libraries, name)
        end
        return convert(Cint, 0)::Cint
    end
end #@linux_only

function dllist()
    dynamic_libraries = Array(AbstractString,0)

    @linux_only begin
        const callback = cfunction(dl_phdr_info_callback, Cint,
                                   (Ref{dl_phdr_info}, Csize_t, Ref{Array{AbstractString,1}} ))
        ccall(:dl_iterate_phdr, Cint, (Ptr{Void}, Ref{Array{AbstractString,1}}), callback, dynamic_libraries)
    end

    @osx_only begin
        numImages = ccall(:_dyld_image_count, Cint, (), )

        # start at 1 instead of 0 to skip self
        for i in 1:numImages-1
            name = bytestring(ccall(:_dyld_get_image_name, Ptr{UInt8}, (UInt32,), i))
            push!(dynamic_libraries, name)
        end
    end

    @windows_only begin
        ccall(:jl_dllist, Cint, (Any,), dynamic_libraries)
    end

    dynamic_libraries
end

end # module
