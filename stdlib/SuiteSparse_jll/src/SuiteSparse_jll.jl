# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/SuiteSparse_jll.jl
baremodule SuiteSparse_jll
using Base, Libdl, libblastrampoline_jll
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libamd, libbtf, libcamd, libccolamd, libcholmod, libcolamd, libklu, libldl, librbio, libspqr, libsuitesparseconfig, libumfpack

# These get calculated in __init__()
# Man I can't wait until these are automatically handled by an in-Base JLLWrappers clone.
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
libamd_handle = C_NULL
libamd_path = ""
libbtf_handle = C_NULL
libbtf_path = ""
libcamd_handle = C_NULL
libcamd_path = ""
libccolamd_handle = C_NULL
libccolamd_path = ""
libcholmod_handle = C_NULL
libcholmod_path = ""
libcolamd_handle = C_NULL
libcolamd_path = ""
libklu_handle = C_NULL
libklu_path = ""
libldl_handle = C_NULL
libldl_path = ""
librbio_handle = C_NULL
librbio_path = ""
libspqr_handle = C_NULL
libspqr_path = ""
libsuitesparseconfig_handle = C_NULL
libsuitesparseconfig_path = ""
libumfpack_handle = C_NULL
libumfpack_path = ""

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
    const libamd = "@rpath/libamd.dylib"
    const libbtf = "@rpath/libbtf.dylib"
    const libcamd = "@rpath/libcamd.dylib"
    const libccolamd = "@rpath/libccolamd.dylib"
    const libcholmod = "@rpath/libcholmod.dylib"
    const libcolamd = "@rpath/libcolamd.dylib"
    const libklu = "@rpath/libklu.dylib"
    const libldl = "@rpath/libldl.dylib"
    const librbio = "@rpath/librbio.dylib"
    const libspqr = "@rpath/libspqr.dylib"
    const libsuitesparseconfig = "@rpath/libsuitesparseconfig.dylib"
    const libumfpack = "@rpath/libumfpack.dylib"
else
    const libamd = "libamd.so"
    const libbtf = "libbtf.so"
    const libcamd = "libcamd.so"
    const libccolamd = "libccolamd.so"
    const libcholmod = "libcholmod.so"
    const libcolamd = "libcolamd.so"
    const libklu = "libklu.so"
    const libldl = "libldl.so"
    const librbio = "librbio.so"
    const libspqr = "libspqr.so"
    const libsuitesparseconfig = "libsuitesparseconfig.so"
    const libumfpack = "libumfpack.so"
end

function __init__()
    global libamd_handle = dlopen(libamd)
    global libamd_path = dlpath(libamd_handle)
    global libbtf_handle = dlopen(libbtf)
    global libbtf_path = dlpath(libbtf_handle)
    global libcamd_handle = dlopen(libcamd)
    global libcamd_path = dlpath(libcamd_handle)
    global libccolamd_handle = dlopen(libccolamd)
    global libccolamd_path = dlpath(libccolamd_handle)
    global libcholmod_handle = dlopen(libcholmod)
    global libcholmod_path = dlpath(libcholmod_handle)
    global libcolamd_handle = dlopen(libcolamd)
    global libcolamd_path = dlpath(libcolamd_handle)
    global libklu_handle = dlopen(libklu)
    global libklu_path = dlpath(libklu_handle)
    global libldl_handle = dlopen(libldl)
    global libldl_path = dlpath(libldl_handle)
    global librbio_handle = dlopen(librbio)
    global librbio_path = dlpath(librbio_handle)
    global libspqr_handle = dlopen(libspqr)
    global libspqr_path = dlpath(libspqr_handle)
    global libsuitesparseconfig_handle = dlopen(libsuitesparseconfig)
    global libsuitesparseconfig_path = dlpath(libsuitesparseconfig_handle)
    global libumfpack_handle = dlopen(libumfpack)
    global libumfpack_path = dlpath(libumfpack_handle)
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
