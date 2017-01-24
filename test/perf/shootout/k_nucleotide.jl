# This file is a part of Julia. License is MIT: http://julialang.org/license

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by David Campbell
# Based on the Go version

import Base.isless

function count(data::AbstractString, n::Int)
    counts = Dict{AbstractString, Int}()
    top = length(data) - n + 1
    for i = 1:top
        s = data[i : i+n-1]
        if haskey(counts, s)
            counts[s] += 1
        else
            counts[s] = 1
        end
    end
    counts
end

function count_one(data::AbstractString, s::AbstractString)
    count(data, length(s))[s]
end

type KNuc
    name::AbstractString
    count::Int
end

# sort down
function isless(x::KNuc, y::KNuc)
    if x.count == y.count
        return x.name > y.name
    end
    x.count > y.count
end

function sorted_array(m::Dict{AbstractString, Int})
    kn = Array{KNuc}(length(m))
    i = 1
    for elem in m
        kn[i] = KNuc(elem...)
        i += 1
    end
    sort(kn)
end

function print_knucs(a::Array{KNuc, 1})
    sum = 0
    for kn in a
        sum += kn.count
    end
    for kn in a
        @printf("%s %.3f\n", kn.name, 100.0kn.count/sum)
    end
    println()
end

function k_nucleotide(infile="knucleotide-input.txt")
    input = open(infile, "r")
    for line in eachline(input)
        startswith(line, ">THREE ") && break
    end
    data = collect(readstring(input))
    # delete the newlines and convert to upper case
    i, j = 1, 1
    while i <= length(data)
        if data[i] != '\n'
            data[j] = uppercase(data[i])
            j += 1
        end
        i += 1
    end
    str = join(data[1:j-1], "")

    arr1 = sorted_array(count(str, 1))
    arr2 = sorted_array(count(str, 2))
    close(input)
#    print_knucs(arr1)
#    print_knucs(arr2)
#    for s in ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"]
#        @printf("%d\t%s\n", count_one(str, s), s)
#    end
end
