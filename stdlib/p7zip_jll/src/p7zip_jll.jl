# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/p7zip_jll.jl
baremodule p7zip_jll
using Base

export p7zip

# These get calculated in __init__()
const PATH = Ref("")
artifact_dir::String = ""
p7zip_path::String = ""
if Sys.iswindows()
    const p7zip_exe = "7z.exe"
else
    const p7zip_exe = "7z"
end

if Sys.iswindows()
    const pathsep = ';'
elseif Sys.isapple()
    const pathsep = ':'
else
    const pathsep = ':'
end

function adjust_ENV()
    addPATH = PATH[]
    oldPATH = get(ENV, "PATH", "")
    newPATH = isempty(oldPATH) ? addPATH : "$addPATH$pathsep$oldPATH"
    return ("PATH"=>newPATH,)
end

function p7zip(f::Function; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true) # deprecated, for compat only
    withenv((adjust_PATH ? adjust_ENV() : ())...) do
        return f(p7zip())
    end
end
# the 7z.exe we ship has no dependencies, so it needs no PATH adjustment
p7zip(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true) = `$p7zip_path`

function init_p7zip_path()
    # Prefer our own bundled p7zip, but if we don't have one, pick it up off of the PATH
    # Our `7z` lives in `private_libexecdir`
    bundled_p7zip_path = joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, p7zip_exe)
    if isfile(bundled_p7zip_path)
        global p7zip_path = abspath(bundled_p7zip_path)
    else
        global p7zip_path = something(Sys.which(p7zip_exe), p7zip_exe)
    end
end

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    init_p7zip_path()
    PATH[] = path = dirname(p7zip_path)
    nothing
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing

end  # module p7zip_jll
