print_output = isempty(ARGS) || contains(ARGS, "perf/perf.jl") || contains(ARGS, "perf")

macro timeit(ex,name)
    quote
        t = 0.0
        for i=1:6
            s = (@elapsed $(esc(ex)))
            if i > 1
                t += s
            end
        end
        if print_output
            println("julia,", $name, ",", (t/5)*1000)
        end
        gc()
    end
end

macro timeit1(ex,name)
    quote
        @printf "julia,%s,%f\n" $name (@elapsed $ex)*1000
        gc()
    end
end

srand(1776)  # get more consistent times
