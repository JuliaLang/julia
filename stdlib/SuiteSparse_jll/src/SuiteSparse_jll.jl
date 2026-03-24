# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/SuiteSparse_jll.jl
baremodule SuiteSparse_jll
using Base, Libdl
using libblastrampoline_jll
if !(Sys.isfreebsd() || Sys.isapple())
    using CompilerSupportLibraries_jll
end

export libamd, libbtf, libcamd, libccolamd, libcholmod, libcolamd, libklu, libldl, librbio, libspqr, libsuitesparseconfig, libumfpack

# These get calculated in __init__()
# Man I can't wait until these are automatically handled by an in-Base JLLWrappers clone.
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libsuitesparseconfig_path::String = ""
const libsuitesparseconfig = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libsuitesparseconfig.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libsuitesparseconfig.7.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libsuitesparseconfig.so.7")
    else
        error("SuiteSparse_jll: Library 'libsuitesparseconfig' is not available for $(Sys.KERNEL)")
    end
)

libldl_path::String = ""
const libldl = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libldl.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libldl.3.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libldl.so.3")
    else
        error("SuiteSparse_jll: Library 'libldl' is not available for $(Sys.KERNEL)")
    end
)

libbtf_path::String = ""
const libbtf = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libbtf.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libbtf.2.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libbtf.so.2")
    else
        error("SuiteSparse_jll: Library 'libbtf' is not available for $(Sys.KERNEL)")
    end
)

libcolamd_path::String = ""
const libcolamd = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libcolamd.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libcolamd.3.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libcolamd.so.3")
    else
        error("SuiteSparse_jll: Library 'libcolamd' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libsuitesparseconfig, libgcc_s]
    else
        LazyLibrary[libsuitesparseconfig]
    end
)

libamd_path::String = ""
const libamd = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libamd.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libamd.3.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libamd.so.3")
    else
        error("SuiteSparse_jll: Library 'libamd' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libsuitesparseconfig, libgcc_s]
    else
        LazyLibrary[libsuitesparseconfig]
    end
)

libcamd_path::String = ""
const libcamd = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libcamd.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libcamd.3.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libcamd.so.3")
    else
        error("SuiteSparse_jll: Library 'libcamd' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libsuitesparseconfig, libgcc_s]
    else
        LazyLibrary[libsuitesparseconfig]
    end
)

libccolamd_path::String = ""
const libccolamd = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libccolamd.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libccolamd.3.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libccolamd.so.3")
    else
        error("SuiteSparse_jll: Library 'libccolamd' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libsuitesparseconfig, libgcc_s]
    else
        LazyLibrary[libsuitesparseconfig]
    end
)

librbio_path::String = ""
const librbio = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("librbio.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("librbio.4.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("librbio.so.4")
    else
        error("SuiteSparse_jll: Library 'librbio' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libsuitesparseconfig, libgcc_s]
    else
        LazyLibrary[libsuitesparseconfig]
    end
)

libcholmod_path::String = ""
const libcholmod = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libcholmod.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libcholmod.5.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libcholmod.so.5")
    else
        error("SuiteSparse_jll: Library 'libcholmod' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows()
        LazyLibrary[
            libsuitesparseconfig, libamd, libcamd, libccolamd, libcolamd, libblastrampoline, libgcc_s
        ]
    else
        LazyLibrary[
            libsuitesparseconfig, libamd, libcamd, libccolamd, libcolamd, libblastrampoline
        ]
    end
)

libklu_path::String = ""
const libklu = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libklu.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libklu.2.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libklu.so.2")
    else
        error("SuiteSparse_jll: Library 'libklu' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libsuitesparseconfig, libamd, libcolamd, libbtf, libgcc_s]
    else
        LazyLibrary[libsuitesparseconfig, libamd, libcolamd, libbtf]
    end
)

libspqr_path::String = ""
const libspqr = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libspqr.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libspqr.4.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libspqr.so.4")
    else
        error("SuiteSparse_jll: Library 'libspqr' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows()
        LazyLibrary[libsuitesparseconfig, libcholmod, libblastrampoline, libgcc_s]
    elseif Sys.isfreebsd() || Sys.isapple()
        LazyLibrary[libsuitesparseconfig, libcholmod, libblastrampoline]
    else
        LazyLibrary[libsuitesparseconfig, libcholmod, libblastrampoline, libstdcxx, libgcc_s]
    end
)

libumfpack_path::String = ""
const libumfpack = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libumfpack.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libumfpack.6.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libumfpack.so.6")
    else
        error("SuiteSparse_jll: Library 'libumfpack' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows() && Sys.WORD_SIZE == 32
        LazyLibrary[libsuitesparseconfig, libamd, libcholmod, libblastrampoline, libgcc_s]
    else
        LazyLibrary[libsuitesparseconfig, libamd, libcholmod, libblastrampoline]
    end
)

function eager_mode()
    @static if @isdefined CompilerSupportLibraries_jll
        CompilerSupportLibraries_jll.eager_mode()
    end
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
    global libamd_path = string(libamd.path)
    global libcamd_path = string(libcamd.path)
    global libccolamd_path = string(libccolamd.path)
    global libcolamd_path = string(libcolamd.path)
    global libsuitesparseconfig_path = string(libsuitesparseconfig.path)

    # LGPL-2.1+
    global libbtf_path = string(libbtf.path)
    global libklu_path = string(libklu.path)
    global libldl_path = string(libldl.path)

    # GPL-2.0+
    if Base.USE_GPL_LIBS
        global libcholmod_path = string(libcholmod.path)
        global librbio_path = string(librbio.path)
        global libspqr_path = string(libspqr.path)
        global libumfpack_path = string(libumfpack.path)
    end
    global artifact_dir = dirname(Sys.BINDIR)
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module SuiteSparse_jll
