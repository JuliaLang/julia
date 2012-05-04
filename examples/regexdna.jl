
seq = readall(stdin_stream)
l1 = length(seq)

seq = replace(seq, r">.*\n|\n", "")
l2 = length(seq)

const variants = [
      "agggtaaa|tttaccct",
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct" ]

for variant in variants
    k = sum([1 for mat in each_match(Regex(variant), seq)])
    printf("%s %d\n", variant, k)
end

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
    (r"Y", "(c|t)") ]

for (u, v) in subs
    global seq
    seq = replace(seq, u, v)
end

println()
println(l1)
println(l2)
println(length(seq))
