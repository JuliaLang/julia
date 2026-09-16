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

# The directories of this Julia installation that shared libraries live in, in the order in
# which a library is looked for. On Windows the dynamic libraries (.dll) are in `Sys.BINDIR`
# ("usr\\bin"), elsewhere the public ones are in the library directory itself; a build from
# source keeps both in the same directory, hence the `unique!`.
const library_dirs = OncePerProcess{Vector{String}}() do
    unique!(String[private_libdir(), shlibdir()])
end

const LIBPATH = OncePerProcess{String}() do
    join(library_dirs(), pathsep)
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
        entry = Sys.ARCH == :x86_64 ? "DllMainCRTStartup" : "_DllMainCRTStartup"
        default_args = `-m $m -Bdynamic -e $entry --enable-auto-image-base --allow-multiple-definition --disable-auto-import --disable-runtime-pseudo-reloc`
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
        # From `x86_64-w64-mingw32-gcc -shared -Wl,--verbose`.
        append!(LIBS,     String["-lopenlibm"])
        # libmsvcrt-os.a contains MinGW CRT objects that can refer back to
        # libmingw32.a/libmingwex.a; keep the selected CRT last.
        append!(LIBS,     String["-lmingw32", "-lgcc_s", "-lgcc", "-lmoldname", "-lmingwex", "-lmsvcrt-os", "-lmingw32", "-lmingwex", "-lmsvcrt-os", "-lkernel32"])
        append!(LIBS,     String["-lpthread", "-ladvapi32", "-lshell32", "-luser32"])
        append!(crtbegin, String[_find_static("dllcrt2.o"), _find_static("crtbegin.o")])
        append!(crtend,   String[_find_static("crtend.o")])
        isdebugbuild() && push!(LIBS, "-lssp")
        append!(LIBS,     String["-lmingwex", "-lmsvcrt-os", "-lmingw32", "-lmingwex", "-lmsvcrt-os"])
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


## Runtime library dependencies ##

public runtime_libraries, library_files, DEFAULT_COMPONENTS

"""
    Base.Linking.DEFAULT_COMPONENTS

The optional components of the Julia runtime that [`runtime_libraries`](@ref) reports the
libraries of by default, i.e. all of them. Do not mutate this; pass the components you want
as the `optional_components` keyword argument instead.

!!! compat "Julia 1.14"
    This constant requires Julia 1.14.
"""
const DEFAULT_COMPONENTS = [:codegen]

# Guards against a misspelled component silently dropping a library set: leaving
# `libjulia-codegen` out of a bundle by accident is exactly what this must not do.
const COMPONENTS = (:codegen,)

# Is `file` the name of a shared library file (or one of the version symlinks that go with
# it) for the extension-less, unversioned library name `name`? A trailing `*` makes `name` a
# prefix, which is how the build system spells the sanitizer runtime, whose name carries the
# architecture.
function _is_library_file(name::AbstractString, file::AbstractString)
    if endswith(name, '*')
        prefix = SubString(name, 1, prevind(name, lastindex(name)))
        return startswith(file, prefix) && occursin(string('.', Libdl.dlext), file)
    end
    parsed = try
        first(Base.BinaryPlatforms.parse_dl_name_version(file))
    catch ex
        ex isa ArgumentError || rethrow()
        return false # not the name of a shared library file
    end
    parsed == name && return true
    # A library may carry its soversion before the extension even on a platform where it
    # normally follows it, as OpenBLAS does in `libopenblas64_.0.3.33.so`, which is the
    # spelling macOS uses; strip such a soversion the same way.
    Sys.isapple() && return false
    parsed = first(Base.BinaryPlatforms.parse_dl_name_version(parsed * ".dylib", "macos"))
    return parsed == name
end

# `readdir` each directory once, so that resolving many names stays two system calls
_library_listings() = Pair{String,Vector{String}}[dir => readdir(dir; sort=true)
                                                  for dir in library_dirs() if isdir(dir)]

# Append the files of `name` to `paths`, and report whether this installation has it at all.
function _library_files!(paths::Vector{String}, name::AbstractString, listings)
    for (dir, files) in listings
        found = false
        for file in files
            _is_library_file(name, file) || continue
            push!(paths, joinpath(dir, file))
            found = true
        end
        # a library is not shipped in more than one of these directories
        found && return true
    end
    return false
end

"""
    Base.Linking.library_files(name::AbstractString) -> Vector{String}
    Base.Linking.library_files(names) -> Vector{String}

The absolute paths of the shared library files that this Julia installation ships under the
extension-less, unversioned library name `name` (e.g. `"libopenblas64_"`), including the
version symlinks that go with them, in the spelling this platform uses (`libfoo.so.1.2`,
`libfoo.1.2.dylib`, `libfoo-1.dll`).

A name this installation does not ship contributes no paths, so an empty result means "not
shipped here" rather than an error. Pass a collection of `names` to read the installation's
directories only once.

This resolves names the way [`runtime_libraries`](@ref) does, for tools that have their own
list of libraries to find in a Julia installation, such as the libraries belonging to
standard libraries. Only the directories this installation keeps shared libraries in are
searched, never the system's.

!!! compat "Julia 1.14"
    This function requires Julia 1.14.
"""
function library_files(names)
    listings = _library_listings()
    paths = String[]
    for name in names
        _library_files!(paths, name, listings)
    end
    return unique!(paths)
end
library_files(name::AbstractString) = library_files((name,))

function _runtime_library_names(optional_components)
    for component in optional_components
        component in COMPONENTS || throw(ArgumentError(
            "unknown optional component $(repr(component)); the optional components are " *
            join(map(repr, COMPONENTS), ", ")))
    end
    debug = isdebugbuild() ? "-debug" : ""
    # Julia's own libraries, which every build has, so that a missing one is an error. In a
    # framework build the runtime is the framework's `Julia` binary rather than a `libjulia`
    # file in these directories.
    required = String[]
    Base.DARWIN_FRAMEWORK || push!(required, "libjulia$debug")
    push!(required, "libjulia-internal$debug")
    # The rest is generated from `JL_RUNTIME_LIBS` in Make.inc, and is optional because
    # whether a library is shipped depends on how Julia was built: it may have been built
    # against the system's copy (`USE_SYSTEM_GMP` and friends), or the library may belong to
    # a build configuration that is off by default (a third party garbage collector, the
    # Tracy profiler, a sanitizer build).
    optional = copy(Base.RUNTIME_LIBRARY_NAMES)
    if :codegen in optional_components
        push!(required, "libjulia-codegen$debug")
        # LLVM is not shipped when Julia is built against a system LLVM or links it
        # statically, so it stays optional
        append!(optional, Base.CODEGEN_LIBRARY_NAMES)
    end
    return required, optional
end

"""
    Base.Linking.runtime_libraries(; optional_components = Base.Linking.DEFAULT_COMPONENTS) -> Vector{String}

The absolute paths of the shared library files that this Julia installation ships and that a
program embedding the Julia runtime needs at run time, including the version symlinks that
go with them, in no particular order. This is the set of libraries that tools such as
`juliac` bundle alongside the binaries they build.

Every path lies inside the Julia installation. A bundle should place each file at the same
location relative to `Sys.BINDIR` that it has here, so that the dependency paths embedded in
`libjulia` keep resolving.

`optional_components` selects the parts of the runtime whose libraries are wanted;
[`DEFAULT_COMPONENTS`](@ref) is all of them. The components are:

  * `:codegen`: generating native code at run time, i.e. `libjulia-codegen` and LLVM. Leave
    it out for a program that never does, such as one built with `--trim`.

Libraries this installation does not ship, because Julia was built to use a copy provided by
the system or because they belong to a build configuration it was not built with, are not
returned: those remain the system's to provide. A library that every build ships, such as
`libjulia-internal`, is instead an error when missing, since that means the installation is
incomplete.

The Julia system image, package images, and the libraries belonging to standard libraries
(OpenBLAS, for example) are not runtime libraries and are not returned; use
[`library_files`](@ref) to find those in this installation.

!!! compat "Julia 1.14"
    This function requires Julia 1.14.
"""
function runtime_libraries(; optional_components::AbstractVector{Symbol} = DEFAULT_COMPONENTS)
    required, optional = _runtime_library_names(optional_components)
    return _runtime_libraries(required, optional, _library_listings())
end

function _runtime_libraries(required, optional, listings)
    paths = String[]
    absent = String[]
    for name in required
        _library_files!(paths, name, listings) || push!(absent, name)
    end
    if !isempty(absent)
        error("this Julia installation does not contain the shared ",
              length(absent) == 1 ? "library " : "libraries ", join(absent, ", "),
              ", which every build of Julia ships; looked in ", join(library_dirs(), ", "),
              ". The installation is incomplete.")
    end
    for name in optional
        _library_files!(paths, name, listings)
    end
    return unique!(paths)
end

end # module Linking
