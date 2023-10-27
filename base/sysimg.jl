# This file is a part of Julia. License is MIT: https://julialang.org/license

Core.include(Main, "Base.jl")

using .Base

# Ensure this file is also tracked
pushfirst!(Base._included_files, (@__MODULE__, abspath(@__FILE__)))

# set up depot & load paths to be able to find stdlib packages
Base.init_depot_path()
Base.init_load_path()

if Base.is_primary_base_module
# load some stdlib packages but don't put their names in Main
let
    # Loading here does not call __init__(). This leads to uninitialized RNG
    # state which causes rand(::UnitRange{Int}) to hang. This is a workaround:
    task = current_task()
    task.rngState0 = 0x5156087469e170ab
    task.rngState1 = 0x7431eaead385992c
    task.rngState2 = 0x503e1d32781c2608
    task.rngState3 = 0x3a77f7189200c20b
    task.rngState4 = 0x5502376d099035ae

    # Stdlibs sorted in dependency, then alphabetical, order by contrib/print_sorted_stdlibs.jl
    # Run with the `--exclude-jlls` option to filter out all JLL packages
    stdlibs = [
        # No dependencies
        :FileWatching, # used by loading.jl -- implicit assumption that init runs
        :Sockets, # used by stream.jl
    ]
    # PackageCompiler can filter out stdlibs so it can be empty
    maxlen = maximum(textwidth.(string.(stdlibs)); init=0)

    tot_time_stdlib = 0.0
    # use a temp module to avoid leaving the type of this closure in Main
    push!(empty!(LOAD_PATH), "@stdlib")
    m = Module()
    GC.@preserve m begin
        print_time = @eval m (mod, t) -> (print(rpad(string(mod) * "  ", $maxlen + 3, "─"));
                                          Base.time_print(stdout, t * 10^9); println())
        print_time(Base, (Base.end_base_include - Base.start_base_include) * 10^(-9))

        Base._track_dependencies[] = true
        tot_time_stdlib = @elapsed for stdlib in stdlibs
            tt = @elapsed Base.require(Base, stdlib)
            print_time(stdlib, tt)
        end
        for dep in Base._require_dependencies
            mod, path, fsize, mtime = dep[1], dep[2], dep[3], dep[5]
            (fsize == 0 || mtime == 0.0) && continue
            push!(Base._included_files, (mod, path))
        end
        empty!(Base._require_dependencies)
        Base._track_dependencies[] = false

        print_time("Stdlibs total", tot_time_stdlib)
    end

    # Clear global state
    empty!(Core.ARGS)
    empty!(Base.ARGS)
    empty!(LOAD_PATH)
    Base.init_load_path() # want to be able to find external packages in userimg.jl

    # Set up Main module
    ccall(:jl_clear_implicit_imports, Cvoid, (Any,), Main)
    eval(Main, :(using Base.MainInclude: eval, include, ans, err))

    tot_time_userimg = @elapsed (isfile("userimg.jl") && Base.include(Main, "userimg.jl"))

    tot_time_base = (Base.end_base_include - Base.start_base_include) * 10.0^(-9)
    tot_time = tot_time_base + tot_time_stdlib + tot_time_userimg

    println("Sysimage built. Summary:")
    print("Base ──────── "); Base.time_print(stdout, tot_time_base    * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_base    / tot_time) * 100); println("%")
    print("Stdlibs ───── "); Base.time_print(stdout, tot_time_stdlib  * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_stdlib  / tot_time) * 100); println("%")
    if isfile("userimg.jl")
    print("Userimg ───── "); Base.time_print(stdout, tot_time_userimg * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_userimg / tot_time) * 100); println("%")
    end
    print("Total ─────── "); Base.time_print(stdout, tot_time         * 10^9); println();

    empty!(LOAD_PATH)
    empty!(DEPOT_PATH)
end

empty!(Base.TOML_CACHE.d)
Base.TOML.reinit!(Base.TOML_CACHE.p, "")
@eval Sys begin
    BINDIR = ""
    STDLIB = ""
end
end
