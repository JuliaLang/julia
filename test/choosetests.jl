@doc """

`tests, net_on = choosetests(choices)` selects a set of tests to be
run. `choices` should be a vector of test names; if empty or set to
`["all"]`, all tests are selected.

This function also supports "test collections": specifically, "linalg"
 refers to collections of tests in the correspondingly-named
directories.

Upon return, `tests` is a vector of fully-expanded test names, and
`net_on` is true if networking is available (required for some tests).
""" ->
function choosetests(choices = [])
    testnames = [
        "linalg", "core", "keywordargs", "numbers", "strings",
        "dates", "dict", "hashing", "remote", "iobuffer", "staged",
        "arrayops", "subarray", "reduce", "reducedim", "random",
        "intfuncs", "simdloop", "blas", "fft", "dsp", "sparse",
        "bitarray", "copy", "math", "fastmath", "functional",
        "operators", "path", "ccall",
        "bigint", "sorting", "statistics", "spawn", "backtrace",
        "priorityqueue", "arpack", "file", "version", "resolve",
        "pollfd", "mpfr", "broadcast", "complex", "socket",
        "floatapprox", "readdlm", "reflection", "regex", "float16",
        "combinatorics", "sysinfo", "rounding", "ranges", "mod2pi",
        "euler", "show", "lineedit", "replcompletions", "repl",
        "replutil", "sets", "test", "goto", "llvmcall", "grisu",
        "nullable", "meta", "profile", "libgit2", "docs", "markdown",
        "base64", "parser", "serialize", "functors", "char", "misc",
        "enums", "cmdlineargs", "i18n"
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
        prepend!(tests, ["linalg1", "linalg2", "linalg3", "linalg4",
            "linalg/lapack", "linalg/triangular", "linalg/tridiag",
            "linalg/pinv", "linalg/givens", "linalg/cholesky", "linalg/lu",
            "linalg/arnoldi"])
        end

    net_required_for = ["socket", "parallel"]
    net_on = true
    try
        getipaddr()
    catch
        warn("Networking unavailable: Skipping tests [" * join(net_required_for, ", ") * "]")
        net_on = false
    end

    if ccall(:jl_running_on_valgrind,Cint,()) != 0 && "rounding" in tests
        warn("Running under valgrind: Skipping rounding tests")
        filter!(x -> x != "rounding", tests)
    end

    if !net_on
        filter!(x -> !(x in net_required_for), tests)
    end

    tests, net_on
end
