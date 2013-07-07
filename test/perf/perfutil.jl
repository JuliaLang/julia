const ntrials = 5
print_output = isempty(ARGS)

macro timeit(ex,name)
    quote
        # warm up
        $(esc(ex))
        # benchmark
        t = zeros(ntrials)
        for i=1:ntrials
            t[i] = @elapsed $(esc(ex))
        end
        if print_output
            @printf "julia,%s,%f\n" $name median(t)*1000
        end
        gc()
    end
end

macro timeit1(ex,name)
    quote
        # warm up
        $(esc(ex))        
        # benchmark
        t = @elapsed $(esc(ex))
        if print_output
            @printf "julia,%s,%f\n" $name t*1000
        end
        gc()
    end
end

# seed rng for more consistent timings
srand(1776)
