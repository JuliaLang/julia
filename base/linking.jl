# This file is a part of Julia. License is MIT: https://julialang.org/license
module Linking

import Base.Libc: Libdl

# inlined LLD_jll
# These get calculated in __init__()
const PATH = Ref("")
const LIBPATH = Ref("")
const PATH_list = String[]
const LIBPATH_list = String[]
const lld_path = Ref{String}()
const lld_exe = Sys.iswindows() ? "lld.exe" : "lld"
const dsymutil_path = Ref{String}()
const dsymutil_exe = Sys.iswindows() ? "dsymutil.exe" : "dsymutil"

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
        if !isempty(get(env, "PATH", ""))
            env["PATH"] = string(PATH, pathsep, env["PATH"])
        else
            env["PATH"] = PATH
        end
    end
    return env
end

function __init_lld_path()
    # Prefer our own bundled lld, but if we don't have one, pick it up off of the PATH
    # If this is an in-tree build, `lld` will live in `tools`.  Otherwise, it'll be in `private_libexecdir`
    for bundled_lld_path in (joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, lld_exe),
                             joinpath(Sys.BINDIR, "..", "tools", lld_exe),
                             joinpath(Sys.BINDIR, lld_exe))
        if isfile(bundled_lld_path)
            lld_path[] = abspath(bundled_lld_path)
            return
        end
    end
    lld_path[] = something(Sys.which(lld_exe), lld_exe)
    return
end

function __init_dsymutil_path()
    #Same as with lld but for dsymutil
    for bundled_dsymutil_path in (joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, dsymutil_exe),
                             joinpath(Sys.BINDIR, "..", "tools", dsymutil_exe),
                             joinpath(Sys.BINDIR, dsymutil_exe))
        if isfile(bundled_dsymutil_path)
            dsymutil_path[] = abspath(bundled_dsymutil_path)
            return
        end
    end
    dsymutil_path[] = something(Sys.which(dsymutil_exe), dsymutil_exe)
    return
end

const VERBOSE = Ref{Bool}(false)

function __init__()
    VERBOSE[] = Base.get_bool_env("JULIA_VERBOSE_LINKING", false)

    __init_lld_path()
    __init_dsymutil_path()
    PATH[] = dirname(lld_path[])
    if Sys.iswindows()
        # On windows, the dynamic libraries (.dll) are in Sys.BINDIR ("usr\\bin")
        append!(LIBPATH_list, [abspath(Sys.BINDIR, Base.LIBDIR, "julia"), Sys.BINDIR])
    else
        append!(LIBPATH_list, [abspath(Sys.BINDIR, Base.LIBDIR, "julia"), abspath(Sys.BINDIR, Base.LIBDIR)])
    end
    LIBPATH[] = join(LIBPATH_list, pathsep)
    return
end

function lld(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([lld_path[]]); env)
end

function dsymutil(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH[], LIBPATH[], adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([dsymutil_path[]]); env)
end

function ld()
    default_args = ``
    @static if Sys.iswindows()
        # LLD supports mingw style linking
        flavor = "gnu"
        m = Sys.ARCH == :x86_64 ? "i386pep" : "i386pe"
        default_args = `-m $m -Bdynamic --enable-auto-image-base --allow-multiple-definition`
    elseif Sys.isapple()
        flavor = "darwin"
        arch = Sys.ARCH == :aarch64 ? :arm64 : Sys.ARCH
        default_args = `-arch $arch -undefined dynamic_lookup -platform_version macos $(Base.MACOS_PRODUCT_VERSION) $(Base.MACOS_PLATFORM_VERSION)`
    else
        flavor = "gnu"
    end

    `$(lld()) -flavor $flavor $default_args`
end

const WHOLE_ARCHIVE = if Sys.isapple()
    "-all_load"
else
    "--whole-archive"
end

const NO_WHOLE_ARCHIVE = if Sys.isapple()
    ""
else
    "--no-whole-archive"
end

const SHARED = if Sys.isapple()
    "-dylib"
else
    "-shared"
end

is_debug() = ccall(:jl_is_debugbuild, Cint, ()) == 1
libdir() = abspath(Sys.BINDIR, Base.LIBDIR)
private_libdir() = abspath(Sys.BINDIR, Base.PRIVATE_LIBDIR)
if Sys.iswindows()
    shlibdir() = Sys.BINDIR
else
    shlibdir() = libdir()
end

function link_image_cmd(path, out)
    LIBDIR = "-L$(libdir())"
    PRIVATE_LIBDIR = "-L$(private_libdir())"
    SHLIBDIR = "-L$(shlibdir())"
    LIBS = is_debug() ? ("-ljulia-debug", "-ljulia-internal-debug") : ("-ljulia", "-ljulia-internal")
    @static if Sys.iswindows()
        LIBS = (LIBS..., "-lopenlibm", "-lssp", "-lgcc_s", "-lgcc", "-lmsvcrt")
    end

    V = VERBOSE[] ? "--verbose" : ""
    `$(ld()) $V $SHARED -o $out $WHOLE_ARCHIVE $path $NO_WHOLE_ARCHIVE $LIBDIR $PRIVATE_LIBDIR $SHLIBDIR $LIBS`
end

function link_image(path, out, internal_stderr::IO=stderr, internal_stdout::IO=stdout)
    run(link_image_cmd(path, out), Base.DevNull(), internal_stderr, internal_stdout)
end

end # module Linking
