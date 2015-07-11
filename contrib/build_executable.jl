#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Builds an executable that doesn't require any julia source code.
# The user needs to provide a julia script that contains a function main(),
# taking no argument, which will be run when executing the
# produced executable.

# Note on packages:
# Even if the script contains using statements, exported functions
# will not be available in main(). Full qualification of names is
# required. It's suggested to replace using statements with import
# statements to produce a consistent result between running main() in
# the REPL and running the executable.

# Windows note:
# gcc is required to compile the program. gcc can be installed using WinRPM.
# $ Pkg.add("WinRPM")
# $ using WinRPM
# $ WinRPM.install("gcc")

type Executable
    name
    filename
    buildfile
    targetfile
    libjulia
end

function Executable(exename, targetdir, debug)
    if debug
        exename = exename * "-debug"
    end
    filename = exename
    @windows_only filename = filename * ".exe"
    buildfile = abspath(joinpath(JULIA_HOME, filename))
    targetfile = targetdir == nothing ? buildfile : joinpath(targetdir, filename)
    libjulia = debug ? "-ljulia-debug" : "-ljulia"

    Executable(exename, filename, buildfile, targetfile, libjulia)
end

type SysFile
    buildpath
    buildfile
    buildfile0
end

function SysFile(exename)
    buildpath = abspath(dirname(Libdl.dlpath("libjulia")))
    buildfile = joinpath(buildpath, "lib"*exename)
    buildfile0 = joinpath(buildpath, "sys0")

    SysFile(buildpath, buildfile, buildfile0)
end

function build_executable(exename, script_file, targetdir=nothing, cpu_target="native"; force=false)
    julia = abspath(joinpath(JULIA_HOME, "julia"))
    build_sysimg = abspath(joinpath(dirname(@__FILE__), "build_sysimg.jl"))

    if targetdir != nothing
        patchelf = find_patchelf()
        if patchelf == nothing && !(OS_NAME == :Windows)
            println("ERROR: Using the 'targetdir' option requires the 'patchelf' utility. Please install it.")
            return 1
        end
    end

    if !isfile(script_file)
        println("ERROR: $(script_file) not found.")
        return 1
    end

    tmpdir = mktempdir()
    cfile = joinpath(tmpdir, "start_func.c")
    userimgjl = joinpath(tmpdir, "userimg.jl")
    script_file = abspath(script_file)

    if targetdir != nothing
        targetdir = abspath(targetdir)
        if !isdir(targetdir)
            println("ERROR: targetdir is not a directory.")
            return 1
        end
    end

    exe_release = Executable(exename, targetdir, false)
    exe_debug   = Executable(exename, targetdir, true)
    sys = SysFile(exename)

    if !force
        for f in [cfile, userimgjl, "$(sys.buildfile).$(Libdl.dlext)", "$(sys.buildfile).ji", exe_release.buildfile, exe_debug.buildfile]
            if isfile(f)
                println("ERROR: File '$(f)' already exists. Delete it or use --force.")
                return 1
            end
        end

        if targetdir != nothing && !isempty(readdir(targetdir))
            println("ERROR: targetdir is not an empty diectory. Delete all contained files or use --force.")
            return 1
        end
    end

    emit_cmain(cfile, exename, targetdir != nothing)
    emit_userimgjl(userimgjl, script_file)

    println("running: $(julia) $(build_sysimg) $(sys.buildfile) $(cpu_target) $(userimgjl) --force")
    run(`$(julia) $(build_sysimg) $(sys.buildfile) $(cpu_target) $(userimgjl) --force`)
    println()

    gcc = find_system_gcc()
    # This argument is needed for the gcc, see issue #9973
    win_arg = @windows ? `-D_WIN32_WINNT=0x0502` : ``
    incs = get_includes()
    ENV2 = deepcopy(ENV)
    @windows_only begin
        if contains(gcc, "WinRPM")
            # This should not bee necessary, it is done due to WinRPM's gcc's
            # include paths is not correct see WinRPM.jl issue #38
            ENV2["PATH"] *= ";" * dirname(gcc)
            push!(incs, "-I"*abspath(joinpath(dirname(gcc),"..","include")))
        end
    end

    for exe in [exe_release, exe_debug]
        println("running: $gcc -g -Wl,--no-as-needed $win_arg $(join(incs, " ")) $(cfile) -o $(exe.buildfile) -Wl,-rpath,$(sys.buildpath) -L$(sys.buildpath) $(exe.libjulia) -l$(exename)")
        cmd = setenv(`$gcc -g -Wl,--no-as-needed $win_arg $(incs) $(cfile) -o $(exe.buildfile) -Wl,-rpath,$(sys.buildpath) -L$(sys.buildpath) $(exe.libjulia) -l$(exename)`, ENV2)
        run(cmd)
        println()
    end

    println("running: rm -rf $(tmpdir) $(sys.buildfile).o $(sys.buildfile0).o $(sys.buildfile0).ji")
    map(f-> rm(f, recursive=true), [tmpdir, sys.buildfile*".o", sys.buildfile0*".o", sys.buildfile0*".ji"])
    println()

    if targetdir != nothing
        # Move created files to target directory
        for file in [exe_release.buildfile, exe_debug.buildfile, sys.buildfile*".$(Libdl.dlext)", sys.buildfile*".ji"]
            mv(file,joinpath(targetdir, basename(file)))
        end

        # Copy needed shared libraries to the target directory
        tmp = ".*\.$(Libdl.dlext).*"
        shlibs = filter(Regex(tmp),readdir(sys.buildpath))
        for shlib in shlibs
            cp(joinpath(sys.buildpath, shlib), joinpath(targetdir, shlib))
        end

        @unix_only begin
            # Fix rpath in executable and shared libraries
            shlibs = filter(Regex(tmp),readdir(targetdir))
            push!(shlibs, exe_release.filename)
            push!(shlibs, exe_debug.filename)
            println(shlibs)
            for shlib in shlibs
                rpath = readall(`$(patchelf) --print-rpath $(joinpath(targetdir, shlib))`)[1:end-1]
                if Base.samefile(rpath, sys.buildpath)
                    run(`$(patchelf) --set-rpath $(targetdir) $(joinpath(targetdir, shlib))`)
                end
            end
        end
    end

    for exe in [exe_release, exe_debug]
        println("$(exe.targetfile) successfully created.")
    end
    return 0
end

function find_patchelf()
    for patchelf in [joinpath(JULIA_HOME, "patchelf"), "patchelf"]
        try
            if success(`$(patchelf) --version`)
                return patchelf
            end
        end
    end
end

function get_includes()
    ret = []

    # binary install
    incpath = abspath(joinpath(JULIA_HOME, "..", "include", "julia"))
    push!(ret, "-I$(incpath)")

    # Git checkout
    julia_root = abspath(joinpath(JULIA_HOME, "..", ".."))
    push!(ret, "-I$(julia_root)src")
    push!(ret, "-I$(julia_root)src/support")
    push!(ret, "-I$(julia_root)usr/include")

    ret
end

function emit_cmain(cfile, exename, relocation)
    if relocation
        sysji = joinpath("lib"*exename)
    else
        sysji = joinpath(dirname(Libdl.dlpath("libjulia")), "lib"*exename)
    end
    sysji = escape_string(sysji)
    f = open(cfile, "w")
    write( f, """
        #include <julia.h>
        #include <stdio.h>

        int main(int argc, char *argv[])
        {
            char sysji[] = \"$(sysji).ji\";
            char mainfunc[] = \"main()\";
            jl_init_with_image(NULL, sysji);
            jl_eval_string(mainfunc);

            int ret = 0;
            if (jl_exception_occurred())
            {
                jl_show(jl_stderr_obj(), jl_exception_occurred());
                jl_printf(jl_stderr_stream(), \"\\n\");
                ret = 1;
            }

            jl_atexit_hook();
            return ret;
        }
        """
    )
    close(f)
end

function emit_userimgjl(userimgjl, script_file)
    open(userimgjl, "w") do f
        write( f, "include(\"$(escape_string(script_file))\")")
    end
end

function find_system_gcc()
    # On Windows, check to see if WinRPM is installed, and if so, see if gcc is installed
    @windows_only try
        require("WinRPM")
        winrpmgcc = joinpath(WinRPM.installdir,"usr","$(Sys.ARCH)-w64-mingw32",
            "sys-root","mingw","bin","gcc.exe")
        if success(`$winrpmgcc --version`)
            return winrpmgcc
        end
    end

    # See if `gcc` exists
    @unix_only try
        if success(`gcc -v`)
            return "gcc"
        end
    end

    error( "GCC not found on system: " * @windows? "GCC can be installed via `Pkg.add(\"WinRPM\"); WinRPM.install(\"gcc\")`" : "" )
end

if !isinteractive()
    if length(ARGS) < 2 || ("--help" in ARGS || "-h" in ARGS)
        println("Usage: build_executable.jl <exename> <script_file> [targetdir] <cpu_target> [--help]")
        println("   <exename>        is the filename of the resulting executable and the resulting sysimg")
        println("   <script_file>    is the path to a jl file containing a main() function.")
        println("   [targetdir]     (optional) is the path to a directory to put the executable and other")
        println("   <cpu_target>     is an LLVM cpu target to build the system image against")
        println("                    needed files into (default: julia directory structure)")
        println("   --force          Set if you wish to overwrite existing files")
        println("   --help           Print out this help text and exit")
        println()
        println(" Example:")
        println("   julia build_executable.jl standalone_test hello_world.jl targetdir core2")
        return 0
    end

    force_flag = "--force" in ARGS
    filter!(x -> x != "--force", ARGS)
    build_executable(ARGS..., force=force_flag)
end
