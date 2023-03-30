# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/LLD_jll.jl

baremodule LLD_jll
using Base, Libdl
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export lld

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
lld_path = ""
if Sys.iswindows()
    const lld_exe = "lld.exe"
else
    const lld_exe = "lld"
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

function adjust_ENV!(env::Dict, PATH::String, LIBPATH::String, adjust_PATH::Bool, adjust_LIBPATH::Bool)
    if adjust_LIBPATH
        LIBPATH_base = get(env, LIBPATH_env, expanduser(LIBPATH_default))
        if !isempty(LIBPATH_base)
            env[LIBPATH_env] = string(LIBPATH, pathsep, LIBPATH_base)
        else
            env[LIBPATH_env] = LIBPATH
        end
    end
    if adjust_PATH && (LIBPATH_env != "PATH" || !adjust_LIBPATH)
        if adjust_PATH
            if !isempty(get(env, "PATH", ""))
                env["PATH"] = string(PATH, pathsep, env["PATH"])
            else
                env["PATH"] = PATH
            end
        end
    end
    return env
end

function lld(f::Function; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    withenv(env...) do
        return f(lld_path)
    end
end
function lld(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([lld_path]); env)
end

function init_lld_path()
    # Prefer our own bundled lld, but if we don't have one, pick it up off of the PATH
    # If this is an in-tree build, `lld` will live in `tools`.  Otherwise, it'll be in `private_libexecdir`
    for bundled_lld_path in (joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, lld_exe),
                             joinpath(Sys.BINDIR, "..", "tools", lld_exe),
                             joinpath(Sys.BINDIR, lld_exe))
        if isfile(bundled_lld_path)
            global lld_path = abspath(bundled_lld_path)
            return
        end
    end
    global lld_path = something(Sys.which(lld_exe), lld_exe)
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    init_lld_path()
    PATH[] = dirname(lld_path)
    push!(PATH_list, PATH[])
    if Sys.iswindows()
        # On windows, the dynamic libraries (.dll) are in Sys.BINDIR ("usr\\bin")
        append!(LIBPATH_list, [joinpath(Sys.BINDIR, Base.LIBDIR, "julia"), Sys.BINDIR])
    else
        append!(LIBPATH_list, [joinpath(Sys.BINDIR, Base.LIBDIR, "julia"), joinpath(Sys.BINDIR, Base.LIBDIR)])
    end
    LIBPATH[] = join(LIBPATH_list, pathsep)
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing

end  # module libLLD_jll
