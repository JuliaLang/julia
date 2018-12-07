#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

import Libdl

const options = [
    "--cflags",
    "--ldflags",
    "--ldlibs",
    "--allflags",
    "--framework"
];

threadingOn() = ccall(:jl_threading_enabled, Cint, ()) != 0

function shell_escape(str)
    str = replace(str, "'" => "'\''")
    return "'$str'"
end

function libDir()
    return if Base.DARWIN_FRAMEWORK
        joinpath(dirname(abspath(Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME))),"lib")
    elseif ccall(:jl_is_debugbuild, Cint, ()) != 0
        dirname(abspath(Libdl.dlpath("libjulia-debug")))
    else
        dirname(abspath(Libdl.dlpath("libjulia")))
    end
end

function frameworkDir()
  normpath(joinpath(dirname(abspath(Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME))),"..","..",".."))
end

private_libDir() = abspath(Sys.BINDIR, Base.PRIVATE_LIBDIR)

function includeDir()
    return abspath(Sys.BINDIR, Base.INCLUDEDIR, "julia")
end

function ldflags(doframework)
    doframework && return "-F$(shell_escape(frameworkDir()))"
    fl = "-L$(shell_escape(libDir()))"
    if Sys.iswindows()
        fl = fl * " -Wl,--stack,8388608"
    elseif !Sys.isapple()
        fl = fl * " -Wl,--export-dynamic"
    end
    return fl
end

function ldlibs(doframework)
    doframework && return "-framework $(Base.DARWIN_FRAMEWORK_NAME)"
    libname = if ccall(:jl_is_debugbuild, Cint, ()) != 0
        "julia-debug"
    else
        "julia"
    end
    if Sys.isunix()
        return "-Wl,-rpath,$(shell_escape(libDir())) " *
            (Sys.isapple() ? string() : "-Wl,-rpath,$(shell_escape(private_libDir())) ") *
            "-l$libname"
    else
        return "-l$libname -lopenlibm"
    end
end

function cflags(doframework)
    flags = IOBuffer()
    print(flags, "-std=gnu99")
    if doframework
        include = shell_escape(frameworkDir())
        print(flags, " -F", include)
    else
        include = shell_escape(includeDir())
        print(flags, " -I", include)
    end
    if threadingOn()
        print(flags, " -DJULIA_ENABLE_THREADING=1")
    end
    if Sys.isunix()
        print(flags, " -fPIC")
    end
    return String(take!(flags))
end

function allflags(doframework)
    return "$(cflags(doframework)) $(ldflags(doframework)) $(ldlibs(doframework))"
end

function check_args(args)
    checked = intersect(args, options)
    if length(checked) == 0 || length(checked) != length(args)
        println(stderr, "Usage: julia-config [", join(options, " | "), "]")
        exit(1)
    end
end

function check_framework_flag(args)
    doframework = "--framework" in args
    if doframework && !Base.DARWIN_FRAMEWORK
        println(stderr, "NOTICE: Ignoring --framework because Julia is not packaged as a framework.")
        return false
    elseif !doframework && Base.DARWIN_FRAMEWORK
        println(stderr, "NOTICE: Consider using --framework because Julia is packaged as a framework.")
        return false
    end
    return doframework
end

function main()
    check_args(ARGS)
    doframework = check_framework_flag(ARGS)
    for args in ARGS
        if args == "--ldflags"
            println(ldflags(doframework))
        elseif args == "--cflags"
            println(cflags(doframework))
        elseif args == "--ldlibs"
            println(ldlibs(doframework))
        elseif args == "--allflags"
            println(allflags(doframework))
        end
    end
end

main()
