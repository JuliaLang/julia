#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Build a system image binary at sysimg_path.dlext.  By default, put the system image
# next to libjulia (except on Windows, where it goes in $JULIA_HOME\..\lib\julia)
# Allow insertion of a userimg via userimg_path.  If sysimg_path.dlext is currently loaded into memory,
# don't continue unless force is set to true.  Allow targeting of a CPU architecture via cpu_target
@unix_only function default_sysimg_path(debug=false)
    joinpath(dirname(Libdl.dlpath(debug ? "libjulia-debug" : "libjulia")),
             "julia", debug ? "sys-debug" : "sys")
end

@windows_only function default_sysimg_path(debug=false)
    joinpath(JULIA_HOME, "..", "lib", "julia", debug ? "sys-debug" : "sys")
end

function build_sysimg(sysimg_path=nothing, cpu_target="native", userimg_path=nothing; force=false, debug=false)
    if sysimg_path == nothing
        sysimg_path = default_sysimg_path(debug)
    end

    # Quit out if a sysimg is already loaded and is in the same spot as sysimg_path, unless forcing
    sysimg = Libdl.dlopen_e("sys")
    if sysimg != C_NULL
        if !force && Base.samefile(Libdl.dlpath(sysimg), "$(sysimg_path).$(Libdl.dlext)")
            info("System image already loaded at $(Libdl.dlpath(sysimg)), set force to override")
            return
        end
    end

    # Canonicalize userimg_path before we enter the base_dir
    if userimg_path != nothing
        userimg_path = abspath(userimg_path)
    end

    # Enter base/ and setup some useful paths
    base_dir = dirname(Base.find_source_file("sysimg.jl"))
    cd(base_dir) do
        julia = joinpath(JULIA_HOME, debug ? "julia-debug" : "julia")
        cc = find_system_compiler()

        # Ensure we have write-permissions to wherever we're trying to write to
        try
            touch("$sysimg_path.ji")
        catch
            err_msg =  "Unable to modify $sysimg_path.ji, ensure parent directory exists "
            err_msg *= "and is writable. Absolute paths work best.)"
            error( err_msg )
        end

        # Copy in userimg.jl if it exists...
        if userimg_path != nothing
            if !isfile(userimg_path)
                error("$userimg_path is not found, ensure it is an absolute path!")
            end
            if isfile("userimg.jl")
                error("$base_dir/userimg.jl already exists, delete manually to continue.")
            end
            cp(userimg_path, "userimg.jl")
        end
        try
            # Start by building inference0.{ji,o}
            inference0_path = joinpath(dirname(sysimg_path), "inference0")
            info("Building inference0.o...")
            println("$julia -C $cpu_target --output-ji $inference0_path.ji --output-o $inference0_path.o coreimg.jl")
            run(`$julia -C $cpu_target --output-ji $inference0_path.ji --output-o $inference0_path.o coreimg.jl`)

            # Bootstrap off off that to create inference.{ji,o}
            inference_path = joinpath(dirname(sysimg_path), "inference")
            info("Building inference.o...")
            println("$julia -C $cpu_target --output-ji $inference_path.ji --output-o $inference_path.o coreimg.jl")
            run(`$julia -C $cpu_target --output-ji $inference_path.ji --output-o $inference_path.o coreimg.jl`)

            # Bootstrap off off that to create sys.{ji,o}
            info("Building sys.o...")
            println("$julia -C $cpu_target --output-ji $sysimg_path.ji --output-o $sysimg_path.o -J $inference_path.ji --startup-file=no sysimg.jl")
            run(`$julia -C $cpu_target --output-ji $sysimg_path.ji --output-o $sysimg_path.o -J $inference_path.ji --startup-file=no sysimg.jl`)

            if cc != nothing
                link_sysimg(sysimg_path, cc, debug)
            else
                info("System image successfully built at $sysimg_path.ji")
            end

            if !Base.samefile("$default_sysimg_path.ji", "$sysimg_path.ji")
                if Base.isfile("$sysimg_path.$(Libdl.dlext)")
                    info("To run Julia with this image loaded, run: julia -J $sysimg_path.$(Libdl.dlext)")
                else
                    info("To run Julia with this image loaded, run: julia -J $sysimg_path.ji")
                end
            else
                info("Julia will automatically load this system image at next startup")
            end
        finally
            # Cleanup userimg.jl
            if userimg_path != nothing && isfile("userimg.jl")
                rm("userimg.jl")
            end
        end
    end
end

# Search for a compiler to link sys.o into sys.dl_ext.  Honor LD environment variable.
function find_system_compiler()
    if haskey( ENV, "CC" )
        if !success(`$(ENV["CC"]) -v`)
            warn("Using compiler override $(ENV["CC"]), but unable to run `$(ENV["CC"]) -v`")
        end
        return ENV["CC"]
    end

    # On Windows, check to see if WinRPM is installed, and if so, see if gcc is installed
    @windows_only try
        eval(Main, :(using WinRPM))
        winrpmgcc = joinpath(WinRPM.installdir,"usr","$(Sys.ARCH)-w64-mingw32",
            "sys-root","mingw","bin","gcc.exe")
        if success(`$winrpmgcc --version`)
            return winrpmgcc
        else
            throw()
        end
    catch
        warn("Install GCC via `Pkg.add(\"WinRPM\"); WinRPM.install(\"gcc\")` to generate sys.dll for faster startup times")
    end


    # See if `cc` exists
    try
        if success(`cc -v`)
            return "cc"
        end
    end

    warn( "No supported compiler found; startup times will be longer" )
end

# Link sys.o into sys.$(dlext)
function link_sysimg(sysimg_path=nothing, cc=find_system_compiler(), debug=false)
    if sysimg_path == nothing
        sysimg_path = default_sysimg_path(debug)
    end
    julia_libdir = dirname(Libdl.dlpath(debug ? "libjulia-debug" : "libjulia"))

    FLAGS = ["-L$julia_libdir"]

    push!(FLAGS, "-shared")
    push!(FLAGS, debug ? "-ljulia-debug" : "-ljulia")
    @windows_only push!(FLAGS, "-lssp")

    info("Linking sys.$(Libdl.dlext)")
    run(`$cc $FLAGS -o $sysimg_path.$(Libdl.dlext) $sysimg_path.o`)

    info("System image successfully built at $sysimg_path.$(Libdl.dlext)")
    @windows_only begin
        if convert(VersionNumber, Base.libllvm_version) < v"3.5.0"
            LLVM_msg = "Building sys.dll on Windows against LLVM < 3.5.0 can cause incorrect backtraces!"
            LLVM_msg *= " Delete generated sys.dll to avoid these problems"
            warn( LLVM_msg )
        end
    end
end

# When running this file as a script, try to do so with default values.  If arguments are passed
# in, use them as the arguments to build_sysimg above
if !isinteractive()
    if length(ARGS) > 5 || ("--help" in ARGS || "-h" in ARGS)
        println("Usage: build_sysimg.jl <sysimg_path> <cpu_target> <usrimg_path.jl> [--force] [--debug] [--help]")
        println("   <sysimg_path>    is an absolute, extensionless path to store the system image at")
        println("   <cpu_target>     is an LLVM cpu target to build the system image against")
        println("   <usrimg_path.jl> is the path to a user image to be baked into the system image")
        println("   --debug          Using julia-debug instead of julia to build the system image")
        println("   --force          Set if you wish to overwrite the default system image")
        println("   --help           Print out this help text and exit")
        println()
        println(" Example:")
        println("   build_sysimg.jl /usr/local/lib/julia/sys core2 ~/my_usrimg.jl --force")
        println()
        println(" Running this script with no arguments is equivalent to:")
        println("   build_sysimg.jl $(default_sysimg_path) native")
        return 0
    end

    debug_flag = "--debug" in ARGS
    filter!(x -> x != "--debug", ARGS)
    force_flag = "--force" in ARGS
    filter!(x -> x != "--force", ARGS)
    build_sysimg(ARGS...; force=force_flag, debug=debug_flag)
end
