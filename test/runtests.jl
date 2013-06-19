testnames = ["core", "keywordargs", "numbers", "strings", "unicode",
             "collections", "hashing", "remote", "iostring", "arrayops",
             "linalg", "blas", "fft", "dsp", "sparse", "bitarray",
             "random", "math", "functional", "bigint", "sorting",
             "statistics", "spawn", "parallel", "priorityqueue",
             "arpack", "file", "perf", "suitesparse", "version",
             "resolve", "pollfd", "mpfr", "broadcast", "complex",
             "socket", "floatapprox", "readdlm"]

tests = ARGS==["all"] ? testnames : ARGS
n = min(8, CPU_CORES, length(tests))
@unix_only n > 1 && addprocs(n)

ENV["OPENBLAS_NUM_THREADS"] = 1

@everywhere include("testdefs.jl")


#The parallel tests assume that they run from node 1
run_threads = sum(tests .== "parallel") != 0
tests = filter(x->(x!="parallel"), tests)
reduce(propagate_errors, nothing, pmap(runtests, tests))

@unix_only n > 1 && rmprocs(workers())
if(run_threads)
    runtests("parallel")
end

println("    \033[32;1mSUCCESS\033[0m")
