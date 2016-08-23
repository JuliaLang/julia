# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

function runtests(name)
    @printf("     \033[1m*\033[0m \033[31m%-21s\033[0m", name)
    res_and_time_data = @timed @testset "$name" begin
        include("$name.jl")
    end
    rss = Sys.maxrss()
    @printf(" maxrss %7.2f MB\n", rss / 2^20)
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
