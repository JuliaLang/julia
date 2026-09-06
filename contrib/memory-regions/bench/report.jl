# When REGIONS_TSV names a file, append one row of numbers to it; the first
# row into a new file is the header. results/run_all.sh collects every
# measurement this way and results/plot.py draws from the files.
function tsv_row(names, values)
    path = get(ENV, "REGIONS_TSV", "")
    isempty(path) && return nothing
    header = !isfile(path) || filesize(path) == 0
    open(path, "a") do io
        header && println(io, "# ", join(names, '\t'))
        println(io, join(values, '\t'))
    end
    return nothing
end
