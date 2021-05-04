# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random

function runtests(name, path, isolate=true; seed=nothing)
    old_print_setting = Test.TESTSET_PRINT_ENABLE[]
    Test.TESTSET_PRINT_ENABLE[] = false
    try
        if isolate
            # Simple enough to type and random enough so that no one will hard
            # code it in the test
            mod_name = Symbol("Test", rand(1:100), "Main_", replace(name, '/' => '_'))
            m = @eval(Main, module $mod_name end)
        else
            m = Main
        end
        @eval(m, using Test, Random)
        let id = myid()
            wait(@spawnat 1 print_testworker_started(name, id))
        end
        res_and_time_data = @timed @testset "$name" begin
            # Random.seed!(nothing) will fail
            seed != nothing && Random.seed!(seed)
            Base.include(m, "$path.jl")
        end
        rss = Sys.maxrss()
        #res_and_time_data[1] is the testset
        ts = res_and_time_data[1]
        passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = Test.get_test_counts(ts)
        # simplify our stored data to just contain the counts
        res_and_time_data = (TestSetException(passes+c_passes, fails+c_fails, errors+c_errors, broken+c_broken, Test.filter_errors(ts)),
                             res_and_time_data[2],
                             res_and_time_data[3],
                             res_and_time_data[4],
                             res_and_time_data[5],
                             rss)
        return res_and_time_data
    catch ex
        Test.TESTSET_PRINT_ENABLE[] = old_print_setting
        ex isa TestSetException || rethrow()
        return Any[ex]
    end
end

# looking in . messes things up badly
filter!(x->x!=".", LOAD_PATH)

# Support for Revise
function revise_trackall()
    Revise.track(Core.Compiler)
    Revise.track(Base)
    for (id, mod) in Base.loaded_modules
        if id.name in STDLIBS
            Revise.track(mod)
        end
    end
    Revise.revise()
end

nothing # File is loaded via a remotecall to "include". Ensure it returns "nothing".
