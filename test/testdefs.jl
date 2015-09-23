# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

function runtests(name)
    @printf("     \033[1m*\033[0m \033[31m%-21s\033[0m", name)
    tt = @elapsed include("$name.jl")
    @printf(" in %6.2f seconds\n", tt)
    nothing
end

function propagate_errors(a,b)
    err_stop = haskey(ENV, "JL_TESTFAILURE_STOP")
    err = false
    if isa(a, Exception) || isa(b, Exception)
        err = true
    end
    if isa(a,Exception) && err_stop
        rethrow(a)
    end
    if isa(b,Exception) && err_stop
        rethrow(b)
    end
    err
end

# looking in . messes things up badly
filter!(x->x!=".", LOAD_PATH)
