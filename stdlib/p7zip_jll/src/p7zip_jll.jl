# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/p7zip_jll.jl
baremodule p7zip_jll
using Base
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

export p7zip

# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
artifact_dir = ""
p7zip_path = ""
if Sys.iswindows()
    const p7zip_exe = "7z.exe"
else
    const p7zip_exe = "7z"
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

function p7zip(f::Function; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    withenv(env...) do
        return f(p7zip_path)
    end
end
function p7zip(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([p7zip_path]); env)
end

function init_p7zip_path()
    # Prefer our own bundled p7zip, but if we don't have one, pick it up off of the PATH
    # If this is an in-tree build, `7z` will live in `bin`.  Otherwise, it'll be in `libexec`
    for bundled_p7zip_path in (joinpath(Sys.BINDIR, Base.LIBEXECDIR, p7zip_exe),
                               joinpath(Sys.BINDIR, p7zip_exe))
        if isfile(bundled_p7zip_path)
            global p7zip_path = abspath(bundled_p7zip_path)
            return
        end
    end
    global p7zip_path = something(Sys.which(p7zip_exe), p7zip_exe)
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    init_p7zip_path()
    PATH[] = dirname(p7zip_path)
    push!(PATH_list, PATH[])
    append!(LIBPATH_list, [joinpath(Sys.BINDIR, Base.LIBDIR, "julia"), joinpath(Sys.BINDIR, Base.LIBDIR)])
    LIBPATH[] = join(LIBPATH_list, pathsep)
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing

end  # module p7zip_jll
