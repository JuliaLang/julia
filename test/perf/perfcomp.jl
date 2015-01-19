# usage: perfcomp.jl <baseline file> [<suite>]
# This will run the specified suite (default "all") and compare it to stored
# results in the baseline file. Only test names present in both will be
# compared.
# The file format is the output of running `make` in this directory.

function readperf(f)
    [ rstrip(l[1:19])=>[float64(l[20:27]),float64(l[29:36]),float64(l[38:45]),float64(l[47:54])] for l in eachline(f) ]
end

function main()
    baseline = readperf(open(ARGS[1]))
    torun = length(ARGS) > 1 ? ARGS[2] : "all"
    io,p = readsfrom(`make -s $torun`)
    newp = readperf(io)

    names = sort(intersect(keys(baseline),keys(newp)))

    means0 = [ baseline[n][3] for n in names ]
    means1 = [ newp[n][3] for n in names ]

    change = (means0 - means1) ./ means0

    println("test name            old       new     % speedup  % st. dev")
    println("-----------------------------------------------------------")
    for i = 1:length(names)
        print(rpad(names[i],19))
        print(lpad(string(means0[i]),8),"  ")
        print(lpad(string(means1[i]),8),"  ")
        dev = baseline[names[i]][4]/means0[i]
        @printf "%7.2f%%  %7.2f%%\n" change[i]*100 dev*100
    end
    println()

    minname = names[indmin(change)]
    maxname = names[indmax(change)]

    minstd = baseline[minname][4]/baseline[minname][3]
    maxstd = baseline[maxname][4]/baseline[maxname][3]

    @printf "min %7.2f%% (std %7.2f%%) %s\n" minimum(change)*100 minstd*100 minname
    @printf "max %7.2f%% (std %7.2f%%) %s\n" maximum(change)*100 maxstd*100 maxname
    @printf "avg %7.2f%%\n" mean(change)*100
end

main()
