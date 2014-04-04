# linalg tests take the longest - start them off first
testnames = [
    "linalg", "core", "keywordargs", "numbers", "strings",
    "collections", "hashing", "remote", "iobuffer", "arrayops", "simdloop",
    "blas", "fft", "dsp", "sparse", "bitarray", "random", "math",
    "functional", "bigint", "sorting", "statistics", "spawn",
    "priorityqueue", "arpack", "file", "suitesparse", "version",
    "resolve", "pollfd", "mpfr", "broadcast", "complex", "socket",
    "floatapprox", "readdlm", "regex", "float16", "combinatorics",
    "sysinfo", "rounding", "ranges", "mod2pi", "euler", "show",
    "lineedit", "replcompletions"
]
@unix_only push!(testnames, "unicode")

# parallel tests depend on other workers - do them last
push!(testnames, "parallel")

tests = ARGS==["all"] ? testnames : ARGS

if "linalg" in tests
    # specifically selected case
    filter!(x -> x != "linalg", tests)
    prepend!(tests, ["linalg1", "linalg2", "linalg3"])
end


n = min(8, CPU_CORES, length(tests))

n > 1 && addprocs(n)

blas_set_num_threads(1)

@everywhere include("testdefs.jl")

reduce(propagate_errors, nothing, pmap(runtests, tests; err_retry=false, err_stop=true))

@unix_only n > 1 && rmprocs(workers(), waitfor=5.0)
println("    \033[32;1mSUCCESS\033[0m")
