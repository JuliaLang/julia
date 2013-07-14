testnames = ["core", "keywordargs", "numbers", "strings", "unicode",
             "collections", "hashing", "remote", "iostring", "arrayops",
             "linalg", "blas", "fft", "dsp", "sparse", "bitarray",
             "random", "math", "functional", "bigint", "sorting",
             "statistics", "spawn", "parallel", "priorityqueue",
             "arpack", "file", "suitesparse", "version",
             "resolve", "pollfd", "mpfr", "broadcast", "complex",
             "socket", "floatapprox", "readdlm", "regex"]

tests = ARGS==["all"] ? testnames : ARGS

n = min(8, CPU_CORES, length(tests))

@unix_only n > 1 && addprocs(n)

blas_set_num_threads(1)

@everywhere include("testdefs.jl")

reduce(propagate_errors, nothing, pmap(runtests, tests; err_retry=false, err_stop=true))

@unix_only n > 1 && rmprocs(workers())
println("    \033[32;1mSUCCESS\033[0m")
