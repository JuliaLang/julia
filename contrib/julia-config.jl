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

function shell_escape(str)
    str = replace(str, "'" => "'\''")
    return "'$str'"
end

function libDir()
    return if Base.isdebugbuild()
        if Base.DARWIN_FRAMEWORK
            joinpath(dirname(abspath(Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME * "_debug"))),"lib")
        else
            dirname(abspath(Libdl.dlpath("libjulia-debug")))
        end
    else
        if Base.DARWIN_FRAMEWORK
            joinpath(dirname(abspath(Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME))),"lib")
        else
            dirname(abspath(Libdl.dlpath("libjulia")))
        end
    end
end

function frameworkDir()
    libjulia = Base.isdebugbuild() ?
        Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME * "_debug") :
        Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME)
    normpath(joinpath(dirname(abspath(libjulia)),"..","..",".."))
end

private_libDir() = abspath(Sys.BINDIR, Base.PRIVATE_LIBDIR)

function includeDir()
    return abspath(Sys.BINDIR, Base.INCLUDEDIR, "julia")
end

function ldflags(; framework::Bool=false)
    framework && return "-F$(shell_escape(frameworkDir()))"
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

function ldlibs(; framework::Bool=false, rpath::Bool=true)
    # Return "Julia" for the framework even if this is a debug build.
    # If the user wants the debug framework, DYLD_IMAGE_SUFFIX=_debug
    # should be used (refer to man 1 dyld).
    framework && return "-framework $(Base.DARWIN_FRAMEWORK_NAME)"
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

function cflags(; framework::Bool=false)
    flags = IOBuffer()
    print(flags, "-std=gnu11")
    if framework
        include = shell_escape(frameworkDir())
        print(flags, " -F", include)
    else
        include = shell_escape(includeDir())
        print(flags, " -I", include)
    end
    if Sys.isunix()
        print(flags, " -fPIC")
    end
    return String(take!(flags))
end

function allflags(; framework::Bool=false, rpath::Bool=true)
    return "$(cflags(; framework)) $(ldflags(; framework)) $(ldlibs(; framework, rpath))"
end

function check_args(args)
    checked = intersect(args, options)
    if length(checked) == 0 || length(checked) != length(args)
        println(stderr, "Usage: julia-config [", join(options, " | "), "]")
        exit(1)
    end
end

function check_framework_flag(args)
    framework = "--framework" in args
    if framework && !Base.DARWIN_FRAMEWORK
        println(stderr, "NOTICE: Ignoring --framework because Julia is not packaged as a framework.")
        return false
    elseif !framework && Base.DARWIN_FRAMEWORK
        println(stderr, "NOTICE: Consider using --framework because Julia is packaged as a framework.")
        return false
    end
    return framework
end

function (@main)(args)
    check_args(args)
    framework = check_framework_flag(args)
    for args in args
        if args == "--ldflags"
            println(ldflags(; framework))
        elseif args == "--cflags"
            println(cflags(; framework))
        elseif args == "--ldlibs"
            println(ldlibs(; framework))
        elseif args == "--allflags"
            println(allflags(; framework))
        end
    end
end
