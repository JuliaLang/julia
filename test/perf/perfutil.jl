print_output = isempty(ARGS) || contains(ARGS, "perf/perf.jl") || contains(ARGS, "perf")

macro timeit(ex,name)
    quote
        t = Inf
        for i=1:5
            t = min(t, @elapsed $ex)
            gc()
        end
        if print_output
            println("julia,", $name, ",", t*1000)
        end
    end
end

macro timeit1(ex,name)
    quote
        @printf "julia,%s,%f\n" $name (@elapsed $ex)*1000
        gc()
    end
end

srand(1776)  # get more consistent times
