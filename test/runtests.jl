testnames = ["core", "keywordargs", "numbers", "strings", "unicode", "corelib",
             "hashing", "remote", "iostring", "arrayops", "linalg", "blas",
             "fft", "dct", "sparse", "bitarray", "random", "math", "functional",
             "bigint", "sorting", "statistics", "spawn", "parallel",
             "suitesparse", "arpack", "bigfloat", "file", "zlib",
             "perf"]

if ARGS == ["all"]
    tests = testnames
else
    tests = ARGS
end

ENV["OPENBLAS_NUM_THREADS"] = 1

if CPU_CORES > 1 && length(tests)>2
    addprocs(2)
end

require("testdefs.jl")

reduce(propagate_errors, nothing, pmap(runtests, tests))

println("    \033[32;1mSUCCESS\033[0m")
