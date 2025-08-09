# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random

include("buildkitetestjson.jl")

function runtests(name, path, isolate=true; seed=nothing)
    old_print_setting = Test.TESTSET_PRINT_ENABLE[]
    Test.TESTSET_PRINT_ENABLE[] = false
    # remove all hint_handlers, so that errorshow tests are not changed by which packages have been loaded on this worker already
    # packages that call register_error_hint should also call this again, and then re-add any hooks they want to test
    empty!(Base.Experimental._hint_handlers)
    withenv("JULIA_TEST_RECORD_PASSES" => Base.get_bool_env("CI", false)) do
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
            seed !== nothing && Random.seed!(seed)

            original_depot_path = copy(Base.DEPOT_PATH)
            original_load_path = copy(Base.LOAD_PATH)
            original_env = copy(ENV)
            original_project = Base.active_project()

            try
                Base.include(m, "$path.jl")
            finally
                if Base.get_bool_env("CI", false)
                    BuildkiteTestJSON.serialize_testset_result_file(@__DIR__, Test.get_testset())
                end
            end

            if Base.DEPOT_PATH != original_depot_path
                msg = "The `$(name)` test set mutated Base.DEPOT_PATH and did not restore the original values"
                @error(
                    msg,
                    original_depot_path,
                    Base.DEPOT_PATH,
                    testset_name = name,
                    testset_path = path,
                )
                error(msg)
            end
            if Base.LOAD_PATH != original_load_path
                msg = "The `$(name)` test set mutated Base.LOAD_PATH and did not restore the original values"
                @error(
                    msg,
                    original_load_path,
                    Base.LOAD_PATH,
                    testset_name = name,
                    testset_path = path,
                )
                error(msg)
            end
            if copy(ENV) != original_env
                throw_error_str = get(ENV, "JULIA_TEST_CHECK_MUTATED_ENV", "true")
                throw_error_b = parse(Bool, throw_error_str)
                if throw_error_b
                    msg = "The `$(name)` test set mutated ENV and did not restore the original values"
                    @error(
                        msg,
                        testset_name = name,
                        testset_path = path,
                    )
                    error(msg)
                end
            end
            if Base.active_project() != original_project
                msg = "The `$(name)` test set changed the active project and did not restore the original value"
                @error(
                    msg,
                    original_project,
                    Base.active_project(),
                    testset_name = name,
                    testset_path = path,
                )
                error(msg)
            end
        end
        rss = Sys.maxrss()
        #res_and_time_data[1] is the testset
        ts = res_and_time_data[1]
        tc = Test.get_test_counts(ts)
        # simplify our stored data to just contain the counts
        res_and_time_data = (TestSetException(tc.passes+tc.cumulative_passes, tc.fails+tc.cumulative_fails,
                             tc.errors+tc.cumulative_errors, tc.broken+tc.cumulative_broken,
                             Test.filter_errors(ts)),
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
    end # withenv
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
