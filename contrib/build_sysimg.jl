#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Build a system image binary at sysimg_path.dlext. Allow insertion of a userimg via
# userimg_path.  If sysimg_path.dlext is currently loaded into memory, don't continue
# unless force is set to true. Allow targeting of a CPU architecture via cpu_target.
function default_sysimg_path(debug=false)
    if Sys.isunix()
        splitext(Libdl.dlpath(debug ? "sys-debug" : "sys"))[1]
    else
        joinpath(dirname(Sys.BINDIR), "lib", "julia", debug ? "sys-debug" : "sys")
    end
end

"""
    build_sysimg(sysimg_path=default_sysimg_path(), cpu_target="native", userimg_path=nothing; force=false)

Rebuild the system image. Store it in `sysimg_path`, which defaults to a file named `sys.ji`
that sits in the same folder as `libjulia.{so,dylib}`, except on Windows where it defaults
to `Sys.BINDIR/../lib/julia/sys.ji`.  Use the cpu instruction set given by `cpu_target`.
Valid CPU targets are the same as for the `-C` option to `julia`, or the `-march` option to
`gcc`.  Defaults to `native`, which means to use all CPU instructions available on the
current processor. Include the user image file given by `userimg_path`, which should contain
directives such as `using MyPackage` to include that package in the new system image. New
system image will not replace an older image unless `force` is set to true.
"""
function build_sysimg(sysimg_path=nothing, cpu_target="native", userimg_path=nothing; force=false, debug=false)
    if sysimg_path === nothing
        sysimg_path = default_sysimg_path(debug)
    end

    # Quit out if a sysimg is already loaded and is in the same spot as sysimg_path, unless forcing
    sysimg = Libdl.dlopen_e("sys")
    if sysimg != C_NULL
        if !force && Base.samefile(Libdl.dlpath(sysimg), "$(sysimg_path).$(Libdl.dlext)")
            info("System image already loaded at $(Libdl.dlpath(sysimg)), set force=true to override.")
            return nothing
        end
    end

    # Canonicalize userimg_path before we enter the base_dir
    if userimg_path !== nothing
        userimg_path = abspath(userimg_path)
    end

    # Enter base and setup some useful paths
    base_dir = dirname(Base.find_source_file("sysimg.jl"))
    cd(base_dir) do
        julia = joinpath(Sys.BINDIR, debug ? "julia-debug" : "julia")
        cc, warn_msg = find_system_compiler()

        # Ensure we have write-permissions to wherever we're trying to write to
        try
            touch("$sysimg_path.ji")
        catch
            err_msg =  "Unable to modify $sysimg_path.ji, ensure parent directory exists "
            err_msg *= "and is writable; absolute paths work best.)"
            error(err_msg)
        end

        # Copy in userimg.jl if it exists
        if userimg_path !== nothing
            if !isfile(userimg_path)
                error("$userimg_path is not found, ensure it is an absolute path.")
            end
            if isfile("userimg.jl")
                error("$(joinpath(base_dir, "userimg.jl")) already exists, delete manually to continue.")
            end
            cp(userimg_path, "userimg.jl")
        end
        try
            # Start by building basecompiler.{ji,o}
            basecompiler_path = joinpath(dirname(sysimg_path), "basecompiler")
            info("Building basecompiler.o")
            info("$julia -C $cpu_target --output-ji $basecompiler_path.ji --output-o $basecompiler_path.o compiler/compiler.jl")
            run(`$julia -C $cpu_target --output-ji $basecompiler_path.ji --output-o $basecompiler_path.o compiler/compiler.jl`)

            # Bootstrap off of that to create sys.{ji,o}
            info("Building sys.o")
            info("$julia -C $cpu_target --output-ji $sysimg_path.ji --output-o $sysimg_path.o -J $basecompiler_path.ji --startup-file=no sysimg.jl")
            run(`$julia -C $cpu_target --output-ji $sysimg_path.ji --output-o $sysimg_path.o -J $basecompiler_path.ji --startup-file=no sysimg.jl`)

            if cc !== nothing
                link_sysimg(sysimg_path, cc, debug)
                !isempty(warn_msg) && foreach(warn, warn_msg)
            else
                !isempty(warn_msg) && foreach(warn, warn_msg)
                info("System image successfully built at $sysimg_path.ji.")
            end

            if !Base.samefile("$(default_sysimg_path(debug)).ji", "$sysimg_path.ji")
                if isfile("$sysimg_path.$(Libdl.dlext)")
                    info("To run Julia with this image loaded, run: `julia -J $sysimg_path.$(Libdl.dlext)`.")
                else
                    info("To run Julia with this image loaded, run: `julia -J $sysimg_path.ji`.")
                end
            else
                info("Julia will automatically load this system image at next startup.")
            end
        finally
            # Cleanup userimg.jl
            if userimg_path !== nothing && isfile("userimg.jl")
                rm("userimg.jl")
            end
        end
    end
end

# Search for a compiler to link sys.o into sys.dl_ext. Honor LD environment variable.
function find_system_compiler()
    cc = nothing
    warn_msg = String[] # save warning messages into an array

    # On Windows, check to see if WinRPM is installed, and if so, see if gcc is installed
    if Sys.iswindows()
        try
            eval(Main, :(using WinRPM))
            winrpmgcc = joinpath(WinRPM.installdir, "usr", "$(Sys.ARCH)-w64-mingw32",
                "sys-root", "mingw", "bin", "gcc.exe")
            if success(`$winrpmgcc --version`)
                cc = winrpmgcc
            else
                throw()
            end
        catch
            push!(warn_msg, "Install GCC via `Pkg.add(\"WinRPM\"); WinRPM.install(\"gcc\")` to generate sys.dll for faster startup times.")
        end
    end

    if haskey(ENV, "CC")
        if !success(`$(ENV["CC"]) -v`)
            push!(warn_msg, "Using compiler override $(ENV["CC"]), but unable to run `$(ENV["CC"]) -v`.")
        end
        cc = ENV["CC"]
    end

    # See if `cc` exists
    try
        if success(`cc -v`)
            cc = "cc"
        end
    end

    if cc === nothing
        push!(warn_msg, "No supported compiler found; startup times will be longer.")
    end

    return cc, warn_msg
end

# Link sys.o into sys.$(dlext)
function link_sysimg(sysimg_path=nothing, cc=find_system_compiler(), debug=false)
    if sysimg_path === nothing
        sysimg_path = default_sysimg_path(debug)
    end
    julia_libdir = dirname(Libdl.dlpath(debug ? "libjulia-debug" : "libjulia"))

    FLAGS = ["-L$julia_libdir"]

    push!(FLAGS, "-shared")
    push!(FLAGS, debug ? "-ljulia-debug" : "-ljulia")
    if Sys.iswindows()
        push!(FLAGS, "-lssp")
    end

    sysimg_file = "$sysimg_path.$(Libdl.dlext)"
    info("Linking sys.$(Libdl.dlext)")
    info("$cc $(join(FLAGS, ' ')) -o $sysimg_file $sysimg_path.o")
    # Windows has difficulties overwriting a file in use so we first link to a temp file
    if Sys.iswindows() && isfile(sysimg_file)
        if success(pipeline(`$cc $FLAGS -o $sysimg_path.tmp $sysimg_path.o`; stdout=stdout, stderr=stderr))
            mv(sysimg_file, "$sysimg_file.old"; force=true)
            mv("$sysimg_path.tmp", sysimg_file; force=true)
        end
    else
        run(`$cc $FLAGS -o $sysimg_file $sysimg_path.o`)
    end
    info("System image successfully built at $sysimg_path.$(Libdl.dlext)")
    return
end

# When running this file as a script, try to do so with default values. If arguments are passed
# in, use them as the arguments to build_sysimg above.
# Also check whether we are running `genstdlib.jl`, in which case we don't want to build a
# system image and instead only need `build_sysimg`'s docstring to be available.
if !isdefined(Main, :GenStdLib) && !isinteractive()
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
        println("   build_sysimg.jl $(default_sysimg_path()) native")
        return 0
    end

    debug_flag = "--debug" in ARGS
    filter!(x -> x != "--debug", ARGS)
    force_flag = "--force" in ARGS
    filter!(x -> x != "--force", ARGS)
    build_sysimg(ARGS...; force=force_flag, debug=debug_flag)
end
