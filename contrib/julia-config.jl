#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

import Libdl

const options = [
    "--cflags",
    "--ldflags",
    "--ldlibs",
    "--allflags"
];

threadingOn() = ccall(:jl_threading_enabled, Cint, ()) != 0

function shell_escape(str)
    str = replace(str, "'" => "'\''")
    return "'$str'"
end

function libDir()
    return if ccall(:jl_is_debugbuild, Cint, ()) != 0
        dirname(abspath(Libdl.dlpath("libjulia-debug")))
    else
        dirname(abspath(Libdl.dlpath("libjulia")))
    end
end

private_libDir() = abspath(Sys.BINDIR, Base.PRIVATE_LIBDIR)

function includeDir()
    return abspath(Sys.BINDIR, Base.INCLUDEDIR, "julia")
end

function ldflags()
    fl = "-L$(shell_escape(libDir()))"
    if Sys.iswindows()
        fl = fl * " -Wl,--stack,8388608"
    elseif Sys.islinux()
        fl = fl * " -Wl,--export-dynamic"
    end
    return fl
end

function ldlibs()
    libname = if ccall(:jl_is_debugbuild, Cint, ()) != 0
        "julia-debug"
    else
        "julia"
    end
    if Sys.isunix()
        return "-Wl,-rpath,$(shell_escape(libDir())) -Wl,-rpath,$(shell_escape(private_libDir())) -l$libname"
    else
        return "-l$libname -lopenlibm"
    end
end

function cflags()
    flags = IOBuffer()
    print(flags, "-std=gnu99")
    include = shell_escape(includeDir())
    print(flags, " -I", include)
    if threadingOn()
        print(flags, " -DJULIA_ENABLE_THREADING=1")
    end
    if Sys.isunix()
        print(flags, " -fPIC")
    end
    return String(take!(flags))
end

function allflags()
    return "$(cflags()) $(ldflags()) $(ldlibs())"
end

function check_args(args)
    checked = intersect(args, options)
    if length(checked) == 0 || length(checked) != length(args)
        println(stderr, "Usage: julia-config [", join(options, " | "), "]")
        exit(1)
    end
end

function main()
    check_args(ARGS)
    for args in ARGS
        if args == "--ldflags"
            println(ldflags())
        elseif args == "--cflags"
            println(cflags())
        elseif args == "--ldlibs"
            println(ldlibs())
        elseif args == "--allflags"
            println(allflags())
        end
    end
end

main()
