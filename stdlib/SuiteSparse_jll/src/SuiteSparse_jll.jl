# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/SuiteSparse_jll.jl
baremodule SuiteSparse_jll
using Base, Libdl, libblastrampoline_jll, CompilerSupportLibraries_jll

export libamd, libbtf, libcamd, libccolamd, libcholmod, libcolamd, libklu, libldl, librbio, libspqr, libsuitesparseconfig, libumfpack

# These get calculated in __init__()
# Man I can't wait until these are automatically handled by an in-Base JLLWrappers clone.
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""
libamd_path::String = ""
libbtf_path::String = ""
libcamd_path::String = ""
libccolamd_path::String = ""
libcholmod_path::String = ""
libcolamd_path::String = ""
libklu_path::String = ""
libldl_path::String = ""
librbio_path::String = ""
libspqr_path::String = ""
libsuitesparseconfig_path::String = ""
libumfpack_path::String = ""

if Sys.iswindows()
    const _libamd_path = BundledLazyLibraryPath("libamd.dll")
    const _libbtf_path = BundledLazyLibraryPath("libbtf.dll")
    const _libcamd_path = BundledLazyLibraryPath("libcamd.dll")
    const _libccolamd_path = BundledLazyLibraryPath("libccolamd.dll")
    const _libcholmod_path = BundledLazyLibraryPath("libcholmod.dll")
    const _libcolamd_path = BundledLazyLibraryPath("libcolamd.dll")
    const _libklu_path = BundledLazyLibraryPath("libklu.dll")
    const _libldl_path = BundledLazyLibraryPath("libldl.dll")
    const _librbio_path = BundledLazyLibraryPath("librbio.dll")
    const _libspqr_path = BundledLazyLibraryPath("libspqr.dll")
    const _libsuitesparseconfig_path = BundledLazyLibraryPath("libsuitesparseconfig.dll")
    const _libumfpack_path = BundledLazyLibraryPath("libumfpack.dll")
elseif Sys.isapple()
    const _libamd_path = BundledLazyLibraryPath("libamd.3.dylib")
    const _libbtf_path = BundledLazyLibraryPath("libbtf.2.dylib")
    const _libcamd_path = BundledLazyLibraryPath("libcamd.3.dylib")
    const _libccolamd_path = BundledLazyLibraryPath("libccolamd.3.dylib")
    const _libcholmod_path = BundledLazyLibraryPath("libcholmod.5.dylib")
    const _libcolamd_path = BundledLazyLibraryPath("libcolamd.3.dylib")
    const _libklu_path = BundledLazyLibraryPath("libklu.2.dylib")
    const _libldl_path = BundledLazyLibraryPath("libldl.3.dylib")
    const _librbio_path = BundledLazyLibraryPath("librbio.4.dylib")
    const _libspqr_path = BundledLazyLibraryPath("libspqr.4.dylib")
    const _libsuitesparseconfig_path = BundledLazyLibraryPath("libsuitesparseconfig.7.dylib")
    const _libumfpack_path = BundledLazyLibraryPath("libumfpack.6.dylib")
else
    const _libamd_path = BundledLazyLibraryPath("libamd.so.3")
    const _libbtf_path = BundledLazyLibraryPath("libbtf.so.2")
    const _libcamd_path = BundledLazyLibraryPath("libcamd.so.3")
    const _libccolamd_path = BundledLazyLibraryPath("libccolamd.so.3")
    const _libcholmod_path = BundledLazyLibraryPath("libcholmod.so.5")
    const _libcolamd_path = BundledLazyLibraryPath("libcolamd.so.3")
    const _libklu_path = BundledLazyLibraryPath("libklu.so.2")
    const _libldl_path = BundledLazyLibraryPath("libldl.so.3")
    const _librbio_path = BundledLazyLibraryPath("librbio.so.4")
    const _libspqr_path = BundledLazyLibraryPath("libspqr.so.4")
    const _libsuitesparseconfig_path = BundledLazyLibraryPath("libsuitesparseconfig.so.7")
    const _libumfpack_path = BundledLazyLibraryPath("libumfpack.so.6")
end

const libsuitesparseconfig = LazyLibrary(_libsuitesparseconfig_path)
const libldl = LazyLibrary(_libldl_path)
const libbtf = LazyLibrary(_libbtf_path)

_libcolamd_dependencies = LazyLibrary[libsuitesparseconfig]
const libcolamd = LazyLibrary(_libcolamd_path; dependencies=_libcolamd_dependencies)

_libamd_dependencies = LazyLibrary[libsuitesparseconfig]
const libamd = LazyLibrary(_libamd_path; dependencies=_libamd_dependencies)

_libcamd_dependencies = LazyLibrary[libsuitesparseconfig]
const libcamd = LazyLibrary(_libcamd_path; dependencies=_libcamd_dependencies)

_libccolamd_dependencies = LazyLibrary[libsuitesparseconfig]
const libccolamd = LazyLibrary(_libccolamd_path; dependencies=_libccolamd_dependencies)

_librbio_dependencies = LazyLibrary[libsuitesparseconfig]
const librbio = LazyLibrary(_librbio_path; dependencies=_librbio_dependencies)

_libcholmod_dependencies = LazyLibrary[
    libsuitesparseconfig, libamd, libcamd, libccolamd, libcolamd, libblastrampoline
    ]
const libcholmod = LazyLibrary(_libcholmod_path; dependencies=_libcholmod_dependencies)

_libklu_dependencies = LazyLibrary[libsuitesparseconfig, libamd, libcolamd, libbtf]
const libklu = LazyLibrary(_libklu_path; dependencies=_libklu_dependencies)

if Sys.isfreebsd() || Sys.isapple()
    _libspqr_dependencies = LazyLibrary[libsuitesparseconfig, libcholmod, libblastrampoline]
else
    _libspqr_dependencies = LazyLibrary[libsuitesparseconfig, libcholmod, libblastrampoline, libstdcxx, libgcc_s]
end
const libspqr = LazyLibrary(_libspqr_path; dependencies=_libspqr_dependencies)

_libumfpack_dependencies = LazyLibrary[libsuitesparseconfig, libamd, libcholmod, libblastrampoline]
const libumfpack = LazyLibrary(_libumfpack_path; dependencies=_libumfpack_dependencies)

function eager_mode()
    CompilerSupportLibraries_jll.eager_mode()
    libblastrampoline_jll.eager_mode()

    dlopen(libamd)
    dlopen(libbtf)
    dlopen(libcamd)
    dlopen(libccolamd)
    dlopen(libcholmod)
    dlopen(libcolamd)
    dlopen(libklu)
    dlopen(libldl)
    dlopen(librbio)
    dlopen(libspqr)
    dlopen(libsuitesparseconfig)
    dlopen(libumfpack)
end
is_available() = true

function __init__()
    # BSD-3-Clause
    global libamd_path = string(_libamd_path)
    global libcamd_path = string(_libcamd_path)
    global libccolamd_path = string(_libccolamd_path)
    global libcolamd_path = string(_libcolamd_path)
    global libsuitesparseconfig_path = string(_libsuitesparseconfig_path)

    # LGPL-2.1+
    global libbtf_path = string(_libbtf_path)
    global libklu_path = string(_libklu_path)
    global libldl_path = string(_libldl_path)

    # GPL-2.0+
    if Base.USE_GPL_LIBS
        global libcholmod_path = string(_libcholmod_path)
        global librbio_path = string(_librbio_path)
        global libspqr_path = string(_libspqr_path)
        global libumfpack_path = string(_libumfpack_path)
    end
    global artifact_dir = dirname(Sys.BINDIR)
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module SuiteSparse_jll
