# This file is a part of Julia. License is MIT: https://julialang.org/license

const mintrials = 5
const mintime = 2000.0
print_output = isempty(ARGS)
codespeed = length(ARGS) > 0 && ARGS[1] == "codespeed"

if codespeed
    using JSON
    using HTTPClient.HTTPC

    # Ensure that we've got the environment variables we want:
    if !haskey(ENV, "JULIA_FLAVOR")
        error( "You must provide the JULIA_FLAVOR environment variable identifying this julia build!" )
    end

    # Setup codespeed data dict for submissions to codespeed's JSON endpoint.  These parameters
    # are constant across all benchmarks, so we'll just let them sit here for now
    csdata = Dict()
    csdata["commitid"] = Base.GIT_VERSION_INFO.commit
    csdata["project"] = "Julia"
    csdata["branch"] = Base.GIT_VERSION_INFO.branch
    csdata["executable"] = ENV["JULIA_FLAVOR"]
    csdata["environment"] = chomp(readstring(`hostname`))
    csdata["result_date"] = join( split(Base.GIT_VERSION_INFO.date_string)[1:2], " " )    #Cut the timezone out
end

# Takes in the raw array of values in vals, along with the benchmark name, description, unit and whether less is better
function submit_to_codespeed(vals,name,desc,unit,test_group,lessisbetter=true)
    # Points to the server
    codespeed_host = "julia-codespeed.csail.mit.edu"

    csdata["benchmark"] = name
    csdata["description"] = desc
    csdata["result_value"] = mean(vals)
    csdata["std_dev"] = std(vals)
    csdata["min"] = minimum(vals)
    csdata["max"] = maximum(vals)
    csdata["units"] = unit
    csdata["units_title"] = test_group
    csdata["lessisbetter"] = lessisbetter

    println( "$name: $(mean(vals))" )
    ret = post( "http://$codespeed_host/result/add/json/", Dict("json" => json([csdata])) )
    println( json([csdata]) )
    if ret.http_code != 200 && ret.http_code != 202
        error("Error submitting $name [HTTP code $(ret.http_code)], dumping headers and text: $(ret.headers)\n$(String(ret.body))\n\n")
        return false
    end
    return true
end

macro output_timings(t,name,desc,group)
    t = esc(t)
    name = esc(name)
    desc = esc(desc)
    group = esc(group)
    quote
        # If we weren't given anything for the test group, infer off of file path!
        test_group = length($group) == 0 ? basename(dirname(Base.source_path())) : $group[1]
        if codespeed
            submit_to_codespeed( $t, $name, $desc, "seconds", test_group )
        elseif print_output
            @printf "julia,%s,%f,%f,%f,%f\n" $name minimum($t) maximum($t) mean($t) std($t)
        end
        gc()
    end
end

macro timeit(ex,name,desc,group...)
    quote
        t = Float64[]
        tot = 0.0
        i = 0
        while i < mintrials || tot < mintime
            e = 1000*(@elapsed $(esc(ex)))
            tot += e
            if i > 0
                # warm up on first iteration
                push!(t, e)
            end
            i += 1
        end
        @output_timings t $(esc(name)) $(esc(desc)) $(esc(group))
    end
end

macro timeit_init(ex,init,name,desc,group...)
    quote
        t = zeros(mintrials)
        for i=0:mintrials
            $(esc(init))
            e = 1000*(@elapsed $(esc(ex)))
            if i > 0
                # warm up on first iteration
                t[i] = e
            end
        end
        @output_timings t $(esc(name)) $(esc(desc)) $(esc(group))
    end
end

function maxrss(name)
    # FIXME: call uv_getrusage instead here
    @static if is_linux()
        rus = Array{Int64}(div(144,8))
        fill!(rus, 0x0)
        res = ccall(:getrusage, Int32, (Int32, Ptr{Void}), 0, rus)
        if res == 0
            mx = rus[5]/1024
            @printf "julia,%s.mem,%f,%f,%f,%f\n" name mx mx mx 0
        end
    end
end


# seed rng for more consistent timings
srand(1776)
