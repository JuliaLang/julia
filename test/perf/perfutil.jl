const ntrials = 5
print_output = isempty(ARGS)
codespeed = length(ARGS) > 0 && ARGS[1] == "codespeed"

if codespeed
    try
        Pkg.init()
        Pkg.add("JSON")
        Pkg.add("Curl")
    end
    usingmodule("JSON")
    usingmodule("Curl")

    # Ensure that we've got the environment variables we want:
    if !haskey(ENV, "JULIA_FLAVOR")
        error( "You must provide the JULIA_FLAVOR environment variable identifying this julia build!" )
    end

    if !haskey(ENV, "JULIA_BRANCH")
        error( "You must provide the JULIA_BRANCH environment variable identifying the branch this julia build grows on!" )
    end

    if !haskey(ENV, "JULIA_COMMIT_DATE")
        error( "You must provide the JULIA_COMMIT_DATE environment variable in the form YYYY-MM-DD HH:MM:SS[TZ]" )
    end

    # Setup codespeed data dict for submissions to codespeed's JSON endpoint.  These parameters
    # are constant across all benchmarks, so we'll just let them sit here for now
    csdata = Dict()
    csdata["commitid"] = Base.BUILD_INFO.commit
    csdata["project"] = "Julia"
    csdata["branch"] = Base.BUILD_INFO.branch
    csdata["executable"] = ENV["JULIA_FLAVOR"]
    csdata["environment"] = chomp(readall(`hostname`))
    csdata["result_date"] = join( split(Base.BUILD_INFO.date_string)[1:2], " " )    #Cut the timezone out
end

# Takes in the raw array of values in vals, along with the benchmark name, description, unit and whether less is better
function submit_to_codespeed(vals,name,desc,unit,test_group,lessisbetter=true)
    # Points to the server 
    codespeed_host = "128.52.160.154"

    csdata["benchmark"] = name
    csdata["description"] = desc
    csdata["result_value"] = mean(vals)
    csdata["std_dev"] = std(vals)
    csdata["min"] = min(vals)
    csdata["max"] = max(vals)
    csdata["units"] = unit
    csdata["units_title"] = test_group
    csdata["lessisbetter"] = lessisbetter

    println( "$name: $(mean(vals))" )
    ret = Curl.post( "http://$codespeed_host/result/add/json/", {:json => to_json([csdata])} )
    if( !ismatch(r".*202 ACCEPTED.*", ret.headers[1][1]) )
        error("Error submitting $name, dumping headers and text: $(ret.headers[1])\n$(ret.text)\n\n")
        return false
    end
    return true
end

macro output_timings(t,name,desc,group)
    quote
        # If we weren't given anything for the test group, infer off of file path!
        test_group = length($group) == 0 ? split(Base.source_path(), "/")[end-1] : $group[1]
        if codespeed
            submit_to_codespeed( $t, $name, $desc, "seconds", test_group )
        elseif print_output
            @printf "julia,%s,%f,%f,%f,%f\n" $name min($t) max($t) mean($t) std($t)
        end
        gc()        
    end
end

macro timeit(ex,name,desc,group...)
    quote
        t = zeros(ntrials)
        for i=0:ntrials
            e = 1000*(@elapsed $(esc(ex)))
            if i > 0
                # warm up on first iteration
                t[i] = e
            end
        end
        @output_timings t $name $desc $group
    end
end

macro timeit1(ex,name,desc,group...)
    quote
        t = 0.0
        for i=0:1
            t = 1000*(@elapsed $(esc(ex)))
        end
        @output_timings [t] $name $desc $group
    end
end

macro timeit_init(ex,init,name,desc,group...)
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
        @output_timings t $name $desc $group
    end
end


# seed rng for more consistent timings
srand(1776)
