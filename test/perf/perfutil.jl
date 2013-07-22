const ntrials = 5
print_output = isempty(ARGS)

macro timeit(ex,name)
    quote
        t = zeros(ntrials)
        for i=0:ntrials
            e = 1000*(@elapsed $(esc(ex)))
            if i > 0
                # warm up on first iteration
                t[i] = e
            end
        end
        if print_output
            @printf "julia,%s,%f,%f,%f,%f\n" $name min(t) max(t) mean(t) std(t)
        end
        gc()
    end
end

macro timeit1(ex,name)
    quote
        t = 0.0
        for i=0:1
            t = 1000*(@elapsed $(esc(ex)))
        end
        if print_output
            @printf "julia,%s,%f,%f,%f,%f\n" $name t t t NaN
        end
        gc()
    end
end

macro timeit_init(ex,init,name)
    quote
        t = zeros(ntrials)
        for i=0:ntrials
            $(esc(init))
            e = 1000*(@elapsed $(esc(ex)))
            if i > 0
                # warm up on first iteration
                t[i] = e
            end
        end
        if print_output
            @printf "julia,%s,%f,%f,%f,%f\n" $name min(t) max(t) mean(t) std(t)
        end
        gc()
    end
end


# seed rng for more consistent timings
srand(1776)
