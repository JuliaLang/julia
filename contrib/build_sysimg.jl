#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Build a system image binary at sysimg_path.dlext.  By default, put the system image
# next to libjulia (except on Windows, where it goes in $JULIA_HOME\..\lib\julia)
# Allow insertion of a userimg via userimg_path.  If sysimg_path.dlext is currently loaded into memory,
# don't continue unless force is set to true.  Allow targeting of a CPU architecture via cpu_target
@unix_only const default_sysimg_path = joinpath(dirname(Libdl.dlpath("libjulia")),"sys")
@windows_only const default_sysimg_path = joinpath(JULIA_HOME,"..","lib","julia","sys")
function build_sysimg(sysimg_path=default_sysimg_path, cpu_target="native", userimg_path=nothing; force=false)
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
        julia = joinpath(JULIA_HOME, "julia")
        ld = find_system_linker()

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
            if !isreadable(userimg_path)
                error("$userimg_path is not readable, ensure it is an absolute path!")
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
            println("$julia -C $cpu_target --build $inference0_path coreimg.jl")
            run(`$julia -C $cpu_target --build $inference0_path coreimg.jl`)

            # Bootstrap off off that to create inference.{ji,o}
            inference_path = joinpath(dirname(sysimg_path), "inference")
            info("Building inference.o...")
            println("$julia -C $cpu_target --build $inference_path coreimg.jl")
            run(`$julia -C $cpu_target --build $inference_path coreimg.jl`)

            # Bootstrap off off that to create sys.{ji,o}
            info("Building sys.o...")
            println("$julia -C $cpu_target --build $sysimg_path -J $inference_path.ji -f sysimg.jl")
            run(`$julia -C $cpu_target --build $sysimg_path -J $inference_path.ji -f sysimg.jl`)

            if ld != nothing
                link_sysimg(sysimg_path, ld)
            else
                info("System image successfully built at $sysimg_path.ji")
            end

            if !Base.samefile("$default_sysimg_path.ji", "$sysimg_path.ji")
                info("To run Julia with this image loaded, run: julia -J $sysimg_path.ji")
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

# Search for a linker to link sys.o into sys.dl_ext.  Honor LD environment variable.
function find_system_linker()
    if haskey( ENV, "LD" )
        if !success(`$(ENV["LD"]) -v`)
            warn("Using linker override $(ENV["LD"]), but unable to run `$(ENV["LD"]) -v`")
        end
        return ENV["LD"]
    end

    # On Windows, check to see if WinRPM is installed, and if so, see if gcc is installed
    @windows_only try
        require("WinRPM")
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


    # See if `ld` exists
    try
        if success(`ld -v`)
            return "ld"
        end
    end

    warn( "No supported linker found; startup times will be longer" )
end

# Link sys.o into sys.$(dlext)
function link_sysimg(sysimg_path=default_sysimg_path, ld=find_system_linker())
    julia_libdir = dirname(Libdl.dlpath("libjulia"))

    FLAGS = ["-L$julia_libdir"]
    if OS_NAME == :Darwin
        push!(FLAGS, "-dylib")
        push!(FLAGS, "-undefined")
        push!(FLAGS, "dynamic_lookup")
        push!(FLAGS, "-macosx_version_min")
        push!(FLAGS, "10.7")
    else
        push!(FLAGS, "-shared")
        # on windows we link using gcc for now
        wl = @windows? "-Wl," : ""
        push!(FLAGS, wl * "--unresolved-symbols")
        push!(FLAGS, wl * "ignore-all")
    end
    @windows_only append!(FLAGS, ["-ljulia", "-lssp-0"])

    info("Linking sys.$(Libdl.dlext)")
    run(`$ld $FLAGS -o $sysimg_path.$(Libdl.dlext) $sysimg_path.o`)

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
    if length(ARGS) > 4 || ("--help" in ARGS || "-h" in ARGS)
        println("Usage: build_sysimg.jl <sysimg_path> <cpu_target> <usrimg_path.jl> [--force] [--help]")
        println("   <sysimg_path>    is an absolute, extensionless path to store the system image at")
        println("   <cpu_target>     is an LLVM cpu target to build the system image against")
        println("   <usrimg_path.jl> is the path to a user image to be baked into the system image")
        println("   --force          Set if you wish to overwrite the default system image")
        println("   --help           Print out this help text and exit")
        println()
        println(" Example:")
        println("   build_sysimg.jl /usr/local/lib/julia/sys core2 ~/my_usrimg.jl --force")
        println()
        println(" Running this script with no arguments is equivalent to calling it via")
        println("   build_sysimg.jl $(default_sysimg_path) native")
        return 0
    end

    force_flag = "--force" in ARGS
    filter!(x -> x != "--force", ARGS)
    build_sysimg(ARGS..., force=force_flag)
end
