#!/usr/bin/env julia

# Build a system image binary at sysimg_path.dlext.  By default, put the system image next to libjulia
# Allow insertion of a userimg via userimg_path.  If sysimg_path.dlext is currently loaded into memory,
# don't continue unless force is set to true.  Allow targeting of a CPU architecture via cpu_target
const default_sysimg_path = joinpath(dirname(Sys.dlpath("libjulia")),"sys")
function build_sysimg(sysimg_path=default_sysimg_path, cpu_target="native", userimg_path=nothing; force=false)
    # Quit out if a sysimg is already loaded and is in the same spot as sysimg_path, unless forcing
    sysimg = dlopen_e("sys")
    if sysimg != C_NULL
        if !force && Sys.dlpath(sysimg) == "$(sysimg_path).$(Sys.dlext)"
            println("System image already loaded at $(Sys.dlpath(sysimg)), set force to override")
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
        try
            julia = joinpath(JULIA_HOME, "julia")
            julia_libdir = dirname(Sys.dlpath("libjulia"))
            ld = find_system_linker()

            # Ensure we have write-permissions to wherever we're trying to write to
            try
                touch("$sysimg_path.$(Sys.dlext)")
            catch
                err_msg =  "Unable to modify $sysimg_path.$(Sys.dlext), ensure parent directory exists "
                err_msg *= "and is writable. Absolute paths work best. Do you need to run this with sudo?)"
                error( err_msg )
            end

            # Copy in userimg.jl if it exists...
            if userimg_path != nothing
                if !isreadable(userimg_path)
                    error("$userimg_path is not readable, ensure it is an absolute path!")
                end
                cp(userimg_path, "userimg.jl")
            end

            # Start by building sys0.{ji,o}
            sys0_path = joinpath(dirname(sysimg_path), "sys0")
            println("Building sys0.o...")
            println("$julia -C $cpu_target --build $sys0_path sysimg.jl")
            run(`$julia -C $cpu_target --build $sys0_path sysimg.jl`)

            # Bootstrap off of that to create sys.{ji,o}
            println("Building sys.o...")
            println("$julia -C $cpu_target --build $sysimg_path -J $sys0_path.ji -f sysimg.jl")
            run(`$julia -C $cpu_target --build $sysimg_path -J $sys0_path.ji -f sysimg.jl`)

            # Link sys.o into sys.$(dlext)
            FLAGS = ["-L$julia_libdir"]
            if OS_NAME == :Darwin
                push!(FLAGS, "-dylib")
                push!(FLAGS, "-undefined")
                push!(FLAGS, "dynamic_lookup")
                push!(FLAGS, "-macosx_version_min")
                push!(FLAGS, "10.7")
            else
                if OS_NAME == :Linux
                    push!(FLAGS, "-shared")
                end
                push!(FLAGS, "--unresolved-symbols")
                push!(FLAGS, "ignore-all")
            end
            @windows_only append!(FLAGS, ["-L$JULIA_HOME", "-ljulia", "-lssp"])

            if ld != nothing
                println("Linking sys.$(Sys.dlext)")
                run(`$ld $FLAGS -o $sysimg_path.$(Sys.dlext) $sysimg_path.o`)
            end

            println("System image successfully built at $sysimg_path.$(Sys.dlext)")
            @windows_only begin
                if convert(VersionNumber, Base.libllvm_version) < v"3.5.0"
                    LLVM_msg = "Building sys.dll on Windows against LLVM < 3.5.0 can cause incorrect backtraces!"
                    LLVM_msg *= " Delete generated sys.dll to avoid these problems"
                    warn( LLVM_msg )
                end
            end

            if default_sysimg_path != sysimg_path
                println("To run Julia with this image loaded, run: julia -J $sysimg_path.ji")
            else
                println("Julia will automatically load this system image at next startup")
            end
        finally
            # Cleanup userimg.jl
            if isfile("userimg.jl")
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

    # On Windows, check to see if WinRPM is installed, and if so, see if binutils is installed
    @windows_only try
        using WinRPM
        if WinRPM.installed("binutils")
            ENV["PATH"] = "$(ENV["PATH"]):$(joinpath(WinRPM.installdir,"usr","$(Sys.ARCH)-w64-mingw32","sys-root","mingw","bin"))"
        else
            throw()
        end
    catch
        warn("Install Binutils via WinRPM.install(\"binutils\") to generate sys.dll for faster startup times" )
    end


    # See if `ld` exists
    try
        if success(`ld -v`)
            return "ld"
        end
    end

    warn( "No supported linker found; startup times will be longer" )
end

# When running this file as a script, try to do so with default values.  If arguments are passed
# in, use them as the arguments to build_sysimg above
if !isinteractive()
    if length(ARGS) > 4 || ("--help" in ARGS || "-h" in ARGS)
        println("Usage: build_sysimg.jl <sysimg_path> <cpu_target> <usrimg_path.jl> [--force] [--help]")
        println("   <sysimg_path>    is an absolute, extensionless path to store the system image at")
        println("   <cpu_target>     is an LLVM cpu target to build the system image against")
        println("   <usrimg_path.lj> is the path to a user image to be baked into the system image")
        println("   --force          Set if you wish to overwrite the default system image")
        println("   --help           Print out this help text and exit")
        println()
        println(" Example:")
        println("   build_sysimg.jl /usr/local/lib/julia/sys core2 ~/my_usrimg.jl true")
        println()
        println(" Running this script with no arguments is equivalent to calling it via")
        println("   build_sysimg.jl $(default_sysimg_path) native")
        return 0
    end

    force_flag = "--force" in ARGS
    filter!(x -> x != "--force", ARGS)
    build_sysimg(ARGS..., force=force_flag)
end
