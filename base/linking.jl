# This file is a part of Julia. License is MIT: https://julialang.org/license
module Linking

import Base: isdebugbuild
import Base.Libc: Libdl

# from LLD_jll
const lld_exe = Sys.iswindows() ? "lld.exe" : "lld"
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

const lld_path = OncePerProcess{String}() do
    # Prefer our own bundled lld, but if we don't have one, pick it up off of the PATH
    # If this is an in-tree build, `lld` will live in `tools`.  Otherwise, it'll be in `private_libexecdir`
    for bundled_lld_path in (joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, lld_exe),
                             joinpath(Sys.BINDIR, "..", "tools", lld_exe),
                             joinpath(Sys.BINDIR, lld_exe))
        if isfile(bundled_lld_path)
            return abspath(bundled_lld_path)
        end
    end
    return something(Sys.which(lld_exe), lld_exe)
end

const dsymutil_path = OncePerProcess{String}() do
    # Same as with lld but for dsymutil
    for bundled_dsymutil_path in (joinpath(Sys.BINDIR, Base.PRIVATE_LIBEXECDIR, dsymutil_exe),
                             joinpath(Sys.BINDIR, "..", "tools", dsymutil_exe),
                             joinpath(Sys.BINDIR, dsymutil_exe))
        if isfile(bundled_dsymutil_path)
            return abspath(bundled_dsymutil_path)
        end
    end
    return something(Sys.which(dsymutil_exe), dsymutil_exe)
end

PATH() = dirname(lld_path())

const LIBPATH = OncePerProcess{String}() do
    if Sys.iswindows()
        # On windows, the dynamic libraries (.dll) are in Sys.BINDIR ("usr\\bin")
        LIBPATH_list = [abspath(Sys.BINDIR, Base.LIBDIR, "julia"), Sys.BINDIR]
    else
        LIBPATH_list = [abspath(Sys.BINDIR, Base.LIBDIR, "julia"), abspath(Sys.BINDIR, Base.LIBDIR)]
    end
    return join(LIBPATH_list, pathsep)
end

function lld(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH(), LIBPATH(), adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([lld_path()]); env)
end

function dsymutil(; adjust_PATH::Bool = true, adjust_LIBPATH::Bool = true)
    env = adjust_ENV!(copy(ENV), PATH(), LIBPATH(), adjust_PATH, adjust_LIBPATH)
    return Cmd(Cmd([dsymutil_path()]); env)
end

function ld()
    default_args = ``
    @static if Sys.iswindows()
        # From`x86_64-w64-mingw32-gcc -shared -Wl,--verbose`
        flavor = "gnu"
        m = Sys.ARCH == :x86_64 ? "i386pep" : "i386pe"
        default_args = `-m $m -Bdynamic -e DllMainCRTStartup --enable-auto-image-base --allow-multiple-definition --disable-auto-import --disable-runtime-pseudo-reloc`
    elseif Sys.isapple()
        flavor = "darwin"
        arch = Sys.ARCH == :aarch64 ? :arm64 : Sys.ARCH
        default_args = `-arch $arch -undefined dynamic_lookup -platform_version macos $(Base.MACOS_PRODUCT_VERSION) $(Base.MACOS_PLATFORM_VERSION)`
        # due to an lld bug: https://github.com/llvm/llvm-project/issues/193646
        # we must make sure the syslibroot does not point to the system or else
        # it will not respect the provided `libSystem.tbd` file
        default_args = `$default_args -syslibroot $(private_libdir())`
    else
        flavor = "gnu"
        # From `gcc -shared -Wl,--verbose`; `-z defs` added to enforce that all symbols
        # the pkgimage references are resolvable at link time (catches regressions early
        # instead of deferring to first-call crashes at runtime).
        default_args = `--build-id --eh-frame-hdr --hash-style=gnu --as-needed -z relro -z defs`
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

# Prefer whole_archive to WHOLE_ARCHIVE
whole_archive(paths::String; is_cc=false) = whole_archive([paths]; is_cc)
function whole_archive(paths::Vector{String}; is_cc=false)
    cc_arg(a) = is_cc ? "-Wl,$a" : a
    if Sys.isapple()
        Cmd(collect(Iterators.flatmap(p -> (cc_arg("-force_load"), p), paths)))
    else
        `$(cc_arg("--whole-archive")) $paths $(cc_arg("--no-whole-archive"))`
    end
end

const SHARED = if Sys.isapple()
    "-dylib"
else
    "-shared"
end

libdir() = abspath(Sys.BINDIR, Base.LIBDIR)
private_libdir() = abspath(Sys.BINDIR, Base.PRIVATE_LIBDIR)
if Sys.iswindows()
    shlibdir() = Sys.BINDIR
else
    shlibdir() = libdir()
end

verbose_linking() = something(Base.get_bool_env("JULIA_VERBOSE_LINKING", false), false)

function _find_static(lib)
    if isfile(joinpath(private_libdir(), lib))
        return joinpath(private_libdir(), lib)
    else
        return joinpath(libdir(), lib)
    end
end

function _find_loaded(re::Regex)
    for p in Libdl.dllist()
        occursin(re, p) && return p
    end
    error("no loaded shared object matching $re")
end

function link_image_cmd(path, out)
    PRIVATE_LIBDIR = "-L$(private_libdir())"
    LIBDIR = "-L$(libdir())"
    SHLIBDIR = "-L$(shlibdir())"
    LIBS = String[]
    if isdebugbuild()
        push!(LIBS, "-ljulia-debug")
        push!(LIBS, "-ljulia-internal-debug")
    else
        push!(LIBS, "-ljulia")
        push!(LIBS, "-ljulia-internal")
    end
    crtbegin = String[]
    crtend = String[]
    @static if Sys.iswindows()
        # From `x86_64-w64-mingw32-gcc -shared -Wl,--verbose`
        # but without repeated libraries (lld auto-resolves circular library references)
        append!(LIBS,     String["-lopenlibm"])
        append!(LIBS,     String["-lmingw32", "-lgcc_s", "-lgcc", "-lmoldname", "-lmingwex", "-lmsvcrt", "-lkernel32"])
        append!(LIBS,     String["-lpthread", "-ladvapi32", "-lshell32", "-luser32"])
        append!(crtbegin, String[_find_static("dllcrt2.o"), _find_static("crtbegin.o")])
        append!(crtend,   String[_find_static("crtend.o")])
        isdebugbuild() && push!(LIBS, "-lssp")
    elseif Sys.isapple()
        # From `clang -dynamiclib -Wl,-v`
        append!(LIBS,     String[_find_static("libclang_rt.osx.a"), _find_static("libSystem.tbd")])
    else
        # From `gcc -shared -Wl,--verbose`
        # but without repeated libraries (lld auto-resolves circular library references)
        libc           = _find_loaded(r"/libc\.so\.\d+$")                       # system libc
        ld_linux       = _find_loaded(r"/ld-(?:linux|musl|elf)[^/]*\.so\.\d+$") # system ld
        libc_nonshared = _find_static("libc_nonshared.a")
        append!(LIBS,     String["-lgcc", "--as-needed", "-lgcc_s", "-latomic", "-lopenlibm", "--no-as-needed", libc])
        isfile(libc_nonshared) && push!(LIBS, libc_nonshared)
        append!(LIBS,     String["--as-needed", ld_linux, "--no-as-needed"])
        append!(crtbegin, String[_find_static("crti.o"), _find_static("crtbeginS.o")])
        append!(crtend,   String[_find_static("crtendS.o"), _find_static("crtn.o")])
    end

    V = verbose_linking() ? "--verbose" : ""
    `$(ld()) $V $SHARED -o $out $crtbegin $(whole_archive(path)) $PRIVATE_LIBDIR $LIBDIR $SHLIBDIR $LIBS $crtend`
end

function link_image(path, out, internal_stderr::IO=stderr, internal_stdout::IO=stdout)
    run(link_image_cmd(path, out), Base.DevNull(), internal_stderr, internal_stdout)
end

end # module Linking
