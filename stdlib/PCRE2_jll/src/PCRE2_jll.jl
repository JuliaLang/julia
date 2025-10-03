# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/PCRE2_jll.jl
baremodule PCRE2_jll
using Base, Libdl

export libpcre2_8

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

libpcre2_8_path::String = ""
const libpcre2_8 = LazyLibrary(
    if Sys.iswindows()
        BundledLazyLibraryPath("libpcre2-8.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libpcre2-8.0.dylib")
    elseif Sys.islinux() || Sys.isfreebsd()
        BundledLazyLibraryPath("libpcre2-8.so.0")
    else
        error("PCRE2_jll: Library 'libpcre2_8' is not available for $(Sys.KERNEL)")
    end
)

function eager_mode()
    dlopen(libpcre2_8)
end
is_available() = true

function __init__()
    global libpcre2_8_path = string(libpcre2_8.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libpcre2_8_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module PCRE2_jll
