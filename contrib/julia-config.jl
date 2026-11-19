#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

import Libdl

const options = [
    "--cflags",
    "--ldflags",
    "--ldlibs",
    "--allflags"
];

function shell_escape(str)
    str = replace(str, "'" => "'\''")
    return "'$str'"
end

function libDir()
    return if Base.isdebugbuild()
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
    elseif !Sys.isapple()
        fl = fl * " -Wl,--export-dynamic"
    end
    return fl
end

function ldrpath()
    libname = if Base.isdebugbuild()
        "julia-debug"
    else
        "julia"
    end
    return "-Wl,-rpath,$(shell_escape(private_libDir())) -Wl,-rpath,$(shell_escape(libDir())) -l$libname"
end

function ldlibs(; rpath::Bool=true)
    libname = if Base.isdebugbuild()
        "julia-debug"
    else
        "julia"
    end
    if Sys.isunix()
        if rpath
            return "-L$(shell_escape(private_libDir())) $(ldrpath())"
        else
            return "-L$(shell_escape(private_libDir()))"
        end
    else
        return "-l$libname -lopenlibm"
    end
end

function cflags()
    flags = IOBuffer()
    print(flags, "-std=gnu11")
    include = shell_escape(includeDir())
    print(flags, " -I", include)
    if Sys.isunix()
        print(flags, " -fPIC")
    end
    return String(take!(flags))
end

function allflags(; rpath::Bool=true)
    return "$(cflags()) $(ldflags()) $(ldlibs(; rpath))"
end

function check_args(args)
    checked = intersect(args, options)
    if length(checked) == 0 || length(checked) != length(args)
        println(stderr, "Usage: julia-config [", join(options, " | "), "]")
        exit(1)
    end
end

function (@main)(args)
    check_args(args)
    for args in args
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
