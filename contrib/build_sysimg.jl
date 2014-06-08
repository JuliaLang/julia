# By default, put the system image next to libjulia
build_sysimg(;force=false, cpu_target="native") = build_sysimg(joinpath(dirname(Sys.dlpath("libjulia")),"sys"), force=force, cpu_target=cpu_target)

# Build a system image binary at sysimg_path.dlext.  If a system image is already loaded, error out, or continue if force = true
function build_sysimg(sysimg_path; force=false, cpu_target="native")
    # Unless force == true, quit out if a sysimg is already loadable
    sysimg = dlopen_e("sys")
    if !force && sysimg != C_NULL
        println("System image already loaded at $(Sys.dlpath(sysimg)), pass \"force=true\" to override")
        return;
    end

    # Enter base/ and setup some useful paths
    base_dir = dirname(Base.find_source_file("sysimg.jl"))
    cd(base_dir) do
        julia = joinpath(JULIA_HOME, "julia")
        julia_libdir = dirname(Sys.dlpath("libjulia"))
        ld = find_system_linker()

        # Ensure we have write-permissions to wherever we're trying to write to
        if !success(`touch $sysimg_path.$(Sys.dlext)`)
            error("$sysimg_path unwritable, ensure parent directory exists and is writable! (Do you need to run this with sudo?)")
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
        else
            push!(FLAGS, "--unresolved-symbols")
            push!(FLAGS, "ignore-all")
        end
        @windows_only append!(FLAGS, ["-L$JULIA_HOME", "-ljulia", "-lssp"])

        println("Linking sys.$(Sys.dlext)")
        run(`$ld $FLAGS -o $sysimg_path.$(Sys.dlext) $sysimg_path.o`)
    end
end

# Search for a linker to link sys.o into sys.dl_ext.  Honor LD environment variable, otherwise search for something we know works
function find_system_linker()
    if haskey( ENV, "LD" )
        if !success(`which $(ENV["LD"])`)
            warn("Using linker override $(ENV["LD"]), but unable to find `$(ENV["LD"])`")
        end
        return ENV["LD"]
    end

    poss_linkers = ["ld", "link"]

    for linker in poss_linkers
        try
            if success(`which $linker`)
                return linker
            end
        end
    end

    error( "No supported linker found (tried $(join(poss_linkers, ", "))), override with LD environment variable!" )
end

if !isinteractive()
    build_sysimg()
end
