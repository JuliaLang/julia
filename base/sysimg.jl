# This file is a part of Julia. License is MIT: https://julialang.org/license

Core.include(Main, "Base.jl")

using .Base

# Ensure this file is also tracked
pushfirst!(Base._included_files, (@__MODULE__, joinpath(@__DIR__, "Base.jl")))
pushfirst!(Base._included_files, (@__MODULE__, joinpath(@__DIR__, "sysimg.jl")))

# set up depot & load paths to be able to find stdlib packages
@eval Base creating_sysimg = true
Base.init_depot_path()
Base.init_load_path()

if Base.is_primary_base_module
# load some stdlib packages but don't put their names in Main
let
    # Stdlibs manually sorted in top down order
    stdlibs = [
            # No deps
            :Base64,
            :CRC32c,
            :SHA,
            :FileWatching,
            :Unicode,
            :Mmap,
            :Serialization,
            :Libdl,
            :Printf,
            :Markdown,
            :LibGit2,
            :Logging,
            :Sockets,
            :Profile,
            :Dates,
            :DelimitedFiles,
            :Random,
            :UUIDs,
            :Future,
            :LinearAlgebra,
            :SparseArrays,
            :SuiteSparse,
            :Distributed,
            :SharedArrays,
            :Pkg,
            :Test,
            :REPL,
            :Statistics,

        # Various JLL packages
        :CompilerSupportLibraries_jll,
        :Zlib_jll,
        :p7zip_jll,
        :utf8proc_jll,
        :PCRE2_jll,
        :dSFMT_jll,
        :libLLVM_jll,
        :Libm_jll,
        :OpenLibm_jll,
        :LibUV_jll,
        :MbedTLS_jll,
        :LibSSH2_jll,
        :LibCURL_jll,
        :LibGit2_jll,
        :MPFR_jll,
        :GMP_jll,
        :OpenBLAS_jll,
        :BLAS_jll,
        :SuiteSparse_jll,
        ]

    if Sys.islinux() || Sys.isfreebsd()
        push!(stdlibs, :LibUnwind_jll)
    elseif Sys.isapple()
        push!(stdlibs, :LibOSXUnwind_jll)
    end

    maxlen = reduce(max, textwidth.(string.(stdlibs)); init=0)

    # use a temp module to avoid leaving the type of this closure in Main
    m = Module()
    GC.@preserve m begin
        print_time = @eval m (mod, t) -> (print(rpad(string(mod) * "  ", $maxlen + 3, "─"));
                                          Base.time_print(t * 10^9); println())
        print_time(Base, (Base.end_base_include - Base.start_base_include) * 10^(-9))

        Base._track_dependencies[] = true
        Base.tot_time_stdlib[] = @elapsed for stdlib in stdlibs
            tt = @elapsed Base.require(Base, stdlib)
            print_time(stdlib, tt)
        end
        for dep in Base._require_dependencies
            dep[3] == 0.0 && continue
            push!(Base._included_files, dep[1:2])
        end
        empty!(Base._require_dependencies)
        Base._track_dependencies[] = false

        print_time("Stdlibs total", Base.tot_time_stdlib[])
    end
end
end

# Clear global state
empty!(Core.ARGS)
empty!(Base.ARGS)
empty!(LOAD_PATH)
@eval Base creating_sysimg = false
Base.init_load_path() # want to be able to find external packages in userimg.jl

# Set up Main module
import Base.MainInclude: eval, include

Base.@eval Base let
    ccall(:jl_clear_implicit_imports, Cvoid, (Any,), Main)
    tot_time_userimg = @elapsed (isfile("userimg.jl") && include(Main, "userimg.jl"))

    tot_time_base = (end_base_include - start_base_include) * 10.0^(-9)
    tot_time = tot_time_base + tot_time_stdlib[] + tot_time_userimg

    println("Sysimage built. Summary:")
    print("Total ─────── "); time_print(tot_time               * 10^9); print(" \n");
    print("Base: ─────── "); time_print(tot_time_base          * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_base          / tot_time) * 100); println("%")
    print("Stdlibs: ──── "); time_print(tot_time_stdlib[] * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_stdlib[] / tot_time) * 100); println("%")
    if isfile("userimg.jl")
    print("Userimg: ──── "); time_print(tot_time_userimg       * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_userimg       / tot_time) * 100); println("%")
    end

    empty!(LOAD_PATH)
    empty!(DEPOT_PATH)
end
