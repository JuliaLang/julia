# This file is a part of Julia. License is MIT: https://julialang.org/license

# Second-stage sysimage builder that loads all stdlibs from precompiled cache files
# This is used when building a "fat" sysimage with all stdlibs included
# This file follows the same structure as sysimg.jl but loads all available stdlibs

Base.reinit_stdio()
@eval Sys BINDIR = ccall(:jl_get_julia_bindir, Any, ())::String
@eval Sys STDLIB = abspath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "stdlib", string('v', VERSION.major, '.', VERSION.minor))
@eval Base _atexit_hooks_finished = false

# Set up depot & load paths to be able to find stdlib packages
Base.init_depot_path()
Base.init_load_path()

if Base.is_primary_base_module
# Load all stdlib packages (similar to sysimg.jl but loading all stdlibs)
let
    # Collect all stdlibs from the stdlib directory
    stdlibs = Symbol[]
    for entry in readdir(Sys.STDLIB)
        stdlib_path = joinpath(Sys.STDLIB, entry)
        # Check if it's a directory with a Project.toml (indicates a stdlib)
        if isdir(stdlib_path) && isfile(joinpath(stdlib_path, "Project.toml"))
            push!(stdlibs, Symbol(entry))
        end
    end


    maxlen = maximum(textwidth.(string.(stdlibs)); init=0)

    m = Core.Module()
    GC.@preserve m begin
        print_time = @eval m (mod, t) -> (print(rpad(string(mod) * "  ", $maxlen + 3, "─"));
                                          Base.time_print(stdout, t * 10^9); println())

        tot_time_stdlib = @elapsed for stdlib in stdlibs
            tt = @elapsed Base.require(Base, stdlib)
            print_time(stdlib, tt)
        end

        print_time("Stdlibs total", tot_time_stdlib)
    end

    # Clear global state
    empty!(Core.ARGS)
    empty!(Base.ARGS)
    empty!(LOAD_PATH)
    empty!(DEPOT_PATH)
end

empty!(Base.TOML_CACHE.d)
Base.TOML.reinit!(Base.TOML_CACHE.p, "")
@eval Base BUILDROOT = ""
@eval Sys begin
    BINDIR = ""
    STDLIB = ""
end
end
