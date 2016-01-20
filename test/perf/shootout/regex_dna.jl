# This file is a part of Julia. License is MIT: http://julialang.org/license

# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by Daniel Jones
# Fix from David Campbell

const variants = [
      "agggtaaa|tttaccct",
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct"
]

const subs = [
    (r"B", "(c|g|t)"),
    (r"D", "(a|g|t)"),
    (r"H", "(a|c|t)"),
    (r"K", "(g|t)"),
    (r"M", "(a|c)"),
    (r"N", "(a|c|g|t)"),
    (r"R", "(a|g)"),
    (r"S", "(c|g)"),
    (r"V", "(a|c|g)"),
    (r"W", "(a|t)"),
    (r"Y", "(c|t)")
]

function regex_dna(infile="regexdna-input.txt")
    seq = readstring(infile)
    l1 = length(seq)

    seq = replace(seq, r">.*\n|\n", "")
    l2 = length(seq)

    for v in variants
        k = 0
        for m in eachmatch(Regex(v), seq)
            k += 1
        end
#        @printf("%s %d\n", v, k)
    end

    for (u, v) in subs
        seq = replace(seq, u, v)
    end

#    println()
#    println(l1)
#    println(l2)
#    println(length(seq))
end

