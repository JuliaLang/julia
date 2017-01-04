# This file is a part of Julia. License is MIT: http://julialang.org/license

function runtests(name, isolate=true)
    if isolate
        mod_name = Symbol("TestMain_", replace(name, '/', '_'))
        m = eval(Main, :(module $mod_name end))
    else
        m = Main
    end
    eval(m, :(using Base.Test))
    ex = quote
        @timed @testset $"$name" begin
            include($"$name.jl")
        end
    end
    res_and_time_data = eval(m, ex)
    rss = Sys.maxrss()
    #res_and_time_data[1] is the testset
    passes,fails,error,broken,c_passes,c_fails,c_errors,c_broken = Base.Test.get_test_counts(res_and_time_data[1])
    if res_and_time_data[1].anynonpass == false
        res_and_time_data = (
                             (passes+c_passes,broken+c_broken),
                             res_and_time_data[2],
                             res_and_time_data[3],
                             res_and_time_data[4],
                             res_and_time_data[5])
    end
    vcat(collect(res_and_time_data), rss)
end

# looking in . messes things up badly
filter!(x->x!=".", LOAD_PATH)
nothing
