# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/SuiteSparse_jll.jl
baremodule SuiteSparse_jll
using Base, Libdl, libblastrampoline_jll

const PATH_list = String[]
const LIBPATH_list = String[]

export libamd, libbtf, libcamd, libccolamd, libcholmod, libcolamd, libklu, libldl, librbio, libspqr, libsuitesparseconfig, libumfpack

# These get calculated in __init__()
# Man I can't wait until these are automatically handled by an in-Base JLLWrappers clone.
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir::String = ""
libamd_handle::Ptr{Cvoid} = C_NULL
libamd_path::String = ""
libbtf_handle::Ptr{Cvoid} = C_NULL
libbtf_path::String = ""
libcamd_handle::Ptr{Cvoid} = C_NULL
libcamd_path::String = ""
libccolamd_handle::Ptr{Cvoid} = C_NULL
libccolamd_path::String = ""
libcholmod_handle::Ptr{Cvoid} = C_NULL
libcholmod_path::String = ""
libcolamd_handle::Ptr{Cvoid} = C_NULL
libcolamd_path::String = ""
libklu_handle::Ptr{Cvoid} = C_NULL
libklu_path::String = ""
libldl_handle::Ptr{Cvoid} = C_NULL
libldl_path::String = ""
librbio_handle::Ptr{Cvoid} = C_NULL
librbio_path::String = ""
libspqr_handle::Ptr{Cvoid} = C_NULL
libspqr_path::String = ""
libsuitesparseconfig_handle::Ptr{Cvoid} = C_NULL
libsuitesparseconfig_path::String = ""
libumfpack_handle::Ptr{Cvoid} = C_NULL
libumfpack_path::String = ""

if Sys.iswindows()
    const libamd = "libamd.dll"
    const libbtf = "libbtf.dll"
    const libcamd = "libcamd.dll"
    const libccolamd = "libccolamd.dll"
    const libcholmod = "libcholmod.dll"
    const libcolamd = "libcolamd.dll"
    const libklu = "libklu.dll"
    const libldl = "libldl.dll"
    const librbio = "librbio.dll"
    const libspqr = "libspqr.dll"
    const libsuitesparseconfig = "libsuitesparseconfig.dll"
    const libumfpack = "libumfpack.dll"
elseif Sys.isapple()
    const libamd = "@rpath/libamd.3.dylib"
    const libbtf = "@rpath/libbtf.2.dylib"
    const libcamd = "@rpath/libcamd.3.dylib"
    const libccolamd = "@rpath/libccolamd.3.dylib"
    const libcholmod = "@rpath/libcholmod.5.dylib"
    const libcolamd = "@rpath/libcolamd.3.dylib"
    const libklu = "@rpath/libklu.2.dylib"
    const libldl = "@rpath/libldl.3.dylib"
    const librbio = "@rpath/librbio.4.dylib"
    const libspqr = "@rpath/libspqr.4.dylib"
    const libsuitesparseconfig = "@rpath/libsuitesparseconfig.7.dylib"
    const libumfpack = "@rpath/libumfpack.6.dylib"
else
    const libamd = "libamd.so.3"
    const libbtf = "libbtf.so.2"
    const libcamd = "libcamd.so.3"
    const libccolamd = "libccolamd.so.3"
    const libcholmod = "libcholmod.so.5"
    const libcolamd = "libcolamd.so.3"
    const libklu = "libklu.so.2"
    const libldl = "libldl.so.3"
    const librbio = "librbio.so.4"
    const libspqr = "libspqr.so.4"
    const libsuitesparseconfig = "libsuitesparseconfig.so.7"
    const libumfpack = "libumfpack.so.6"
end

function __init__()
    libblastrampoline_jll.eager_mode()

    # BSD-3-Clause
    global libamd_handle = dlopen(libamd)
    global libamd_path = dlpath(libamd_handle)
    global libcamd_handle = dlopen(libcamd)
    global libcamd_path = dlpath(libcamd_handle)
    global libccolamd_handle = dlopen(libccolamd)
    global libccolamd_path = dlpath(libccolamd_handle)
    global libcolamd_handle = dlopen(libcolamd)
    global libcolamd_path = dlpath(libcolamd_handle)
    global libsuitesparseconfig_handle = dlopen(libsuitesparseconfig)
    global libsuitesparseconfig_path = dlpath(libsuitesparseconfig_handle)

    # LGPL-2.1+
    global libbtf_handle = dlopen(libbtf)
    global libbtf_path = dlpath(libbtf_handle)
    global libklu_handle = dlopen(libklu)
    global libklu_path = dlpath(libklu_handle)
    global libldl_handle = dlopen(libldl)
    global libldl_path = dlpath(libldl_handle)

    # GPL-2.0+
    if Base.USE_GPL_LIBS
        global libcholmod_handle = dlopen(libcholmod)
        global libcholmod_path = dlpath(libcholmod_handle)
        global librbio_handle = dlopen(librbio)
        global librbio_path = dlpath(librbio_handle)
        global libspqr_handle = dlopen(libspqr)
        global libspqr_path = dlpath(libspqr_handle)
        global libumfpack_handle = dlopen(libumfpack)
        global libumfpack_path = dlpath(libumfpack_handle)
    end
    global artifact_dir = dirname(Sys.BINDIR)
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libamd_path() = libamd_path
get_libbtf_path() = libbtf_path
get_libcamd_path() = libcamd_path
get_libccolamd_path() = libccolamd_path
get_libcholmod_path() = libcholmod_path
get_libcolamd_path() = libcolamd_path
get_libklu_path() = libklu_path
get_libldl_path() = libldl_path
get_librbio_path() = librbio_path
get_libspqr_path() = libspqr_path
get_libsuitesparseconfig_path() = libsuitesparseconfig_path
get_libumfpack_path() = libumfpack_path

end  # module SuiteSparse_jll
