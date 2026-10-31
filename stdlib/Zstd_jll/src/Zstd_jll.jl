# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/Zstd_jll.jl
baremodule Zstd_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export libzstd, zstd, zstdmt

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir::String = ""
libzstd_handle::Ptr{Cvoid} = C_NULL
libzstd_path::String = ""
zstd_path::String = ""
zstdmt_path::String = ""

if Sys.iswindows()
    const libzstd = "libzstd-1.dll"
elseif Sys.isapple()
    const libzstd = "@rpath/libzstd.1.dylib"
else
    const libzstd = "libzstd.so.1"
end

if Sys.iswindows()
    const zstd_exe = "zstd.exe"
    const zstdmt_exe = "zstdmt.exe"
else
    const zstd_exe = "zstd"
    const zstdmt_exe = "zstdmt"
end

if Sys.iswindows()
    const LIBPATH_env = "PATH"
    const LIBPATH_default = ""
    const pathsep = ';'
elseif Sys.isapple()
    const LIBPATH_env = "DYLD_FALLBACK_LIBRARY_PATH"
    const LIBPATH_default = "~/lib:/usr/local/lib:/lib:/usr/lib"
    const pathsep = ':'
else
    const LIBPATH_env = "LD_LIBRARY_PATH"
    const LIBPATH_default = ""
    const pathsep = ':'
end

function adjust_ENV!(env::Dict{keytype(Base.EnvDict),valtype(Base.EnvDict)}, PATH::String, LIBPATH::String, adjust_PATH::Bool, adjust_LIBPATH::Bool)
    if adjust_LIBPATH
        LIBPATH_base = get(env, LIBPATH_env, expanduser(LIBPATH_default))
        if !isempty(LIBPATH_base)
            env[LIBPATH_env] = string(LIBPATH, pathsep, LIBPATH_base)
        else
            env[LIBPATH_env] = LIBPATH
        end
    end
    if adjust_PATH && (LIBPATH_env != "PATH" || !adjust_LIBPATH)
        if !isempty(get(env, "PATH", ""))
            env["PATH"] = string(PATH, pathsep, env["PATH"])
        else
            env["PATH"] = PATH
        end
    end
    return env
end

function zstd(f::Function; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    withenv(env...) do
        return f(zstd_path)
    end
end
function zstd(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([zstd_path]); env)
end
function zstdmt(f::Function; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    withenv(env...) do
        return f(zstdmt_path)
    end
end
function zstdmt(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([zstdmt_path]); env)
end

function find_bundled_exe(exe::String)
    # Prefer our own bundled executable, but if we don't have one, pick it up off of the PATH
    # If this is an in-tree build, it will live in `bindir`.  Otherwise, it'll be in `private_libexecdir`
    for bundled_path in (joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, exe),
                         joinpath(Sys.BINDIR, exe))
        if isfile(bundled_path)
            return abspath(bundled_path)
        end
    end
    return something(Sys.which(exe), exe)
end

function __init__()
    global libzstd_handle = dlopen(libzstd)
    global libzstd_path = dlpath(libzstd_handle)
    global artifact_dir = dirname(Sys.BINDIR)
    global zstd_path = find_bundled_exe(zstd_exe)
    global zstdmt_path = find_bundled_exe(zstdmt_exe)
    PATH[] = dirname(zstd_path)
    push!(PATH_list, PATH[])
    push!(LIBPATH_list, dirname(libzstd_path))
    LIBPATH[] = join(LIBPATH_list, pathsep)
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing
get_libzstd_path() = libzstd_path

end  # module Zstd_jll
