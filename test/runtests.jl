# linalg tests take the longest - start them off first
testnames = [
    "linalg", "core", "keywordargs", "numbers", "strings", "dates",
    "collections", "hashing", "remote", "iobuffer", "staged", "arrayops",
    "subarray", "reduce", "reducedim", "random", "intfuncs",
    "simdloop", "blas", "fft", "dsp", "sparse", "bitarray", "copy", "math",
    "functional", "bigint", "sorting", "statistics", "spawn",
    "backtrace", "priorityqueue", "arpack", "file", "suitesparse", "version",
    "resolve", "pollfd", "mpfr", "broadcast", "complex", "socket",
    "floatapprox", "readdlm", "reflection", "regex", "float16", "combinatorics",
    "sysinfo", "rounding", "ranges", "mod2pi", "euler", "show",
    "lineedit", "replcompletions", "repl", "test", "goto",
    "llvmcall", "grisu", "nullable", "meta", "profile",
    "libgit2", "docs", "base64"
]

if isdir(joinpath(JULIA_HOME, Base.DOCDIR, "examples"))
    push!(testnames, "examples")
end
@unix_only push!(testnames, "unicode")

# parallel tests depend on other workers - do them last
push!(testnames, "parallel")

tests = (ARGS==["all"] || isempty(ARGS)) ? testnames : ARGS

if "linalg" in tests
    # specifically selected case
    filter!(x -> x != "linalg", tests)
    prepend!(tests, ["linalg1", "linalg2", "linalg3", "linalg4", "linalg/lapack", "linalg/triangular", "linalg/tridiag", "linalg/pinv", "linalg/cholmod", "linalg/umfpack", "linalg/givens"])
end

net_required_for = ["socket", "parallel"]
net_on = true
try
    getipaddr()
catch
    warn("Networking unavailable: Skipping tests [" * join(net_required_for, ", ") * "]")
    net_on = false
end

cd(dirname(@__FILE__)) do
    n = 1
    if net_on
        n = min(8, CPU_CORES, length(tests))
        n > 1 && addprocs(n; exeflags=`--check-bounds=yes`)
        blas_set_num_threads(1)
    else
        filter!(x -> !(x in net_required_for), tests)
    end

    @everywhere include("testdefs.jl")

    reduce(propagate_errors, nothing, pmap(runtests, tests; err_retry=false, err_stop=true))

    @unix_only n > 1 && rmprocs(workers(), waitfor=5.0)
    println("    \033[32;1mSUCCESS\033[0m")
end
