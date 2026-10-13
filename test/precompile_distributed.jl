# This file is a part of Julia. License is MIT: https://julialang.org/license

# Precompilation tests that add a worker process. Only node 1 can do that, so
# runtests.jl runs this file there; the rest of the tests are in precompile.jl.

using Test, Distributed

include("tempdepot.jl")

# Issue #19960
(f -> f())() do # wrap in function scope, so we can test world errors
    test_workers = addprocs(1)
    push!(test_workers, myid())
    save_cwd = pwd()
    temp_path = mkdepottempdir()
    try
        cd(temp_path)
        load_path = mktempdir(temp_path)
        load_cache_path = mkdepottempdir(temp_path)

        ModuleA = :Issue19960A
        ModuleB = :Issue19960B

        write(joinpath(load_path, "$ModuleA.jl"),
            """
            module $ModuleA
                import Distributed: myid
                export f
                f() = myid()
            end
            """)

        write(joinpath(load_path, "$ModuleB.jl"),
            """
            module $ModuleB
                using $ModuleA
                export g
                g() = f()
            end
            """)

        @everywhere test_workers begin
            pushfirst!(LOAD_PATH, $load_path)
            pushfirst!(DEPOT_PATH, $load_cache_path)
        end
        try
            @eval using $ModuleB
            invokelatest() do
                uuid = Base.module_build_id(Base.root_module(Main, ModuleB))
                for wid in test_workers
                    @test Distributed.remotecall_eval(Main, wid, quote
                            Base.module_build_id(Base.root_module(Main, $(QuoteNode(ModuleB))))
                        end) == uuid
                    if wid != myid() # avoid world-age errors on the local proc
                        @test remotecall_fetch(g, wid) == wid
                    end
                end
            end
        finally
            @everywhere test_workers begin
                popfirst!(LOAD_PATH)
                popfirst!(DEPOT_PATH)
            end
        end
    finally
        cd(save_cwd)
        pop!(test_workers) # remove myid
        rmprocs(test_workers)
    end
end
