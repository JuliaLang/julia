# This file is a part of Julia. License is MIT: http://julialang.org/license

# wordcount.jl
#
# Implementation of parallelized "word-count" of a text, inspired by the
# Hadoop WordCount example. Uses @spawn and fetch() to parallelize
# the "map" task. Reduce is currently done single-threaded.
#
# To run in parallel on a string stored in variable `text`:
#  julia -p <N>
#  julia> require("<julia_doc_dir>/examples/wordcount.jl")
#  julia> ...(define text)...
#  julia> counts=parallel_wordcount(text)
#
# Or to run on a group of files, writing results to an output file:
#  julia -p <N>
#  julia> require("<julia_doc_dir>/examples/wordcount.jl")
#  julia> wordcount_files("/tmp/output.txt", "/tmp/input1.txt","/tmp/input2.txt",...)

# "Map" function.
# Takes a string. Returns a Dict with the number of times each word
# appears in that string.
function wordcount(text)
    words=split(text,[' ','\n','\t','-','.',',',':',';'];keep=false)
    counts=Dict()
    for w = words
        counts[w]=get(counts,w,0)+1
    end
    return counts
end

# "Reduce" function.
# Takes a collection of Dicts in the format returned by wordcount()
# Returns a Dict in which words that appear in multiple inputs
# have their totals added together.
function wcreduce(wcs)
    counts=Dict()
    for c in wcs, (k,v) in c
        counts[k] = get(counts,k,0)+v
    end
    return counts
end

# Splits input string into nprocs() equal-sized chunks (last one rounds up),
# and @spawns wordcount() for each chunk to run in parallel. Then fetch()s
# results and performs wcreduce().
function parallel_wordcount(text)
    lines=split(text,'\n';keep=false)
    np=nprocs()
    unitsize=ceil(length(lines)/np)
    wcounts=[]
    rrefs=[]
    # spawn procs
    for i=1:np
        first=unitsize*(i-1)+1
        last=unitsize*i
        if last>length(lines)
            last=length(lines)
        end
        subtext=join(lines[Int(first):Int(last)],"\n")
        push!(rrefs, @spawn wordcount( subtext ) )
    end
    # fetch results
    while length(rrefs)>0
        push!(wcounts,fetch(pop!(rrefs)))
    end
    # reduce
    count=wcreduce(wcounts)
    return count
end

# Takes the name of a result file, and a list of input file names.
# Combines the contents of all files, then performs a parallel_wordcount
# on the resulting string. Writes the results to result_file.
function wordcount_files(result_file,inputs...)
    text = ""
    for file in inputs
        open(file) do f
            text *= readall(f)
        end
    end
    wc = parallel_wordcount(text)
    open(result_file,"w") do f
        for (k,v) in wc
            println(f, k,"=",v)
        end
    end
end
