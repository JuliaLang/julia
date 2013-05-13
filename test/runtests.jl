testnames = ["core", "keywordargs", "numbers", "strings", "unicode",
             "corelib", "hashing", "remote", "iostring", "arrayops",
             "linalg", "blas", "fft", "dsp", "sparse", "bitarray",
             "random", "math", "functional", "bigint", "sorting",
             "statistics", "spawn", "parallel", "priorityqueue",
             "arpack", "file", "perf", "suitesparse", "version",
             "pollfd", "mpfr"]

# Disabled: "complex"

tests = ARGS==["all"] ? testnames : ARGS
n = min(CPU_CORES,length(tests),8)
@unix_only n > 1 && addprocs(n)

ENV["OPENBLAS_NUM_THREADS"] = 1

@everywhere include("testdefs.jl")

reduce(propagate_errors, nothing, pmap(runtests, tests))

println("    \033[32;1mSUCCESS\033[0m")
