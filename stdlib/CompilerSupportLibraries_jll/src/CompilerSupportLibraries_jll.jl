# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl

baremodule CompilerSupportLibraries_jll
using Base, Libdl, Base.BinaryPlatforms

export libgfortran, libstdcxx, libgomp, libatomic, libgcc_s

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libatomic_path::String = ""
const libatomic = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libatomic-1.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libatomic.1.dylib")
    elseif Sys.isfreebsd()
        BundledLazyLibraryPath("libatomic.so.3")
    elseif Sys.islinux()
        BundledLazyLibraryPath("libatomic.so.1")
    else
        error("CompilerSupportLibraries_jll: Library 'libatomic' is not available for $(Sys.KERNEL)")
    end
)

if Sys.iswindows() || Sys.isapple() || arch(HostPlatform()) ∈ ("x86_64", "i686")
    global libquadmath_path::String = ""
    const libquadmath = LazyLibrary(
        if Sys.iswindows()
            BundledLazyLibraryPath("libquadmath-0.dll")
        elseif Sys.isapple()
            BundledLazyLibraryPath("libquadmath.0.dylib")
        elseif (Sys.islinux() || Sys.isfreebsd()) && arch(HostPlatform()) ∈ ("x86_64", "i686")
            BundledLazyLibraryPath("libquadmath.so.0")
        else
            error("CompilerSupportLibraries_jll: Library 'libquadmath' is not available for $(Sys.KERNEL)")
        end
    )
end

libgcc_s_path::String = ""
const libgcc_s = LazyLibrary(
    if Sys.iswindows()
        if arch(HostPlatform()) == "x86_64"
            BundledLazyLibraryPath("libgcc_s_seh-1.dll")
        else
            BundledLazyLibraryPath("libgcc_s_sjlj-1.dll")
        end
    elseif Sys.isapple()
        if arch(HostPlatform()) == "aarch64" || libgfortran_version(HostPlatform()) == v"5"
            BundledLazyLibraryPath("libgcc_s.1.1.dylib")
        else
            BundledLazyLibraryPath("libgcc_s.1.dylib")
        end
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libgcc_s.so.1")
    else
        error("CompilerSupportLibraries_jll: Library 'libgcc_s' is not available for $(Sys.KERNEL)")
    end
)

libgfortran_path::String = ""
const libgfortran = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath(string("libgfortran-", libgfortran_version(HostPlatform()).major, ".dll"))
    elseif Sys.isapple()
        BundledLazyLibraryPath(string("libgfortran.", libgfortran_version(HostPlatform()).major, ".dylib"))
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath(string("libgfortran.so.", libgfortran_version(HostPlatform()).major))
    else
        error("CompilerSupportLibraries_jll: Library 'libgfortran' is not available for $(Sys.KERNEL)")
    end;
    dependencies = @static if @isdefined(libquadmath)
        LazyLibrary[libgcc_s, libquadmath]
    else
        LazyLibrary[libgcc_s]
    end
)

libstdcxx_path::String = ""
const libstdcxx = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libstdc++-6.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libstdc++.6.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libstdc++.so.6")
    else
        error("CompilerSupportLibraries_jll: Library 'libstdcxx' is not available for $(Sys.KERNEL)")
    end;
    dependencies = LazyLibrary[libgcc_s]
)

libgomp_path::String = ""
const libgomp = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libgomp-1.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libgomp.1.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libgomp.so.1")
    else
        error("CompilerSupportLibraries_jll: Library 'libgomp' is not available for $(Sys.KERNEL)")
    end;
    dependencies = if Sys.iswindows()
        LazyLibrary[libgcc_s]
    else
        LazyLibrary[]
    end
)

# only define if isfile
let
    if Sys.iswindows() || Sys.isapple() || libc(HostPlatform()) != "musl"
        _libssp_path = if Sys.iswindows()
            BundledLazyLibraryPath("libssp-0.dll")
        elseif Sys.isapple()
            BundledLazyLibraryPath("libssp.0.dylib")
        elseif Sys.islinux() && libc(HostPlatform()) != "musl"
            BundledLazyLibraryPath("libssp.so.0")
        end
        if isfile(string(_libssp_path))
            global libssp_path::String = ""
            @eval const libssp = LazyLibrary($(_libssp_path))
        end
    end
end

# Conform to LazyJLLWrappers API
function eager_mode()
    if @isdefined(libatomic)
        dlopen(libatomic)
    end
    dlopen(libgcc_s)
    dlopen(libgomp)
    if @isdefined libquadmath
        dlopen(libquadmath)
    end
    if @isdefined libssp
        dlopen(libssp)
    end
    dlopen(libgfortran)
    dlopen(libstdcxx)
end
is_available() = true

function __init__()
    global libatomic_path = string(libatomic.path)
    global libgcc_s_path = string(libgcc_s.path)
    global libgomp_path = string(libgomp.path)
    if @isdefined libquadmath_path
        global libquadmath_path = string(libquadmath.path)
    end
    if @isdefined libssp_path
        global libssp_path = string(libssp.path)
    end
    global libgfortran_path = string(libgfortran.path)
    global libstdcxx_path = string(libstdcxx.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libgcc_s_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module CompilerSupportLibraries_jll
