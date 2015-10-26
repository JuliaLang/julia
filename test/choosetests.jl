# This file is a part of Julia. License is MIT: http://julialang.org/license

@doc """

`tests, codegen_tests, net_on = choosetests(choices)` selects a set of
tests to be run. `choices` should be a vector of test names; if empty
or set to `["all"]`, all tests are selected.

This function also supports "test collections": specifically, "linalg"
refers to collections of tests in the correspondingly-named directories.

Upon return, `tests` and `codegen_tests` are vectors of fully-expanded
test names, and `net_on` is true if networking is available (required
for some tests).

`tests` contains general-purpose tests, while `codegen-tests` contains
tests that require special exeflags to run, eg. inlining, optimisations
etc. should not be disabled.
""" ->
function choosetests(choices = [])
    testnames = [
        "linalg", "core", "keywordargs", "numbers", "printf",
        "char", "string", "triplequote", "unicode",
        "dates", "dict", "hashing", "remote", "iobuffer", "staged",
        "arrayops", "tuple", "subarray", "reduce", "reducedim", "random",
        "abstractarray", "intfuncs", "simdloop", "blas", "sparse",
        "bitarray", "copy", "math", "fastmath", "functional",
        "operators", "path", "ccall", "parse", "loading",
        "bigint", "sorting", "statistics", "spawn", "backtrace",
        "priorityqueue", "file", "mmap", "version", "resolve",
        "pollfd", "mpfr", "broadcast", "complex", "socket",
        "floatapprox", "readdlm", "reflection", "regex", "float16",
        "combinatorics", "sysinfo", "rounding", "ranges", "mod2pi",
        "euler", "show", "lineedit", "replcompletions", "repl",
        "replutil", "sets", "test", "goto", "llvmcall", "grisu",
        "nullable", "meta", "profile", "libgit2", "docs", "markdown",
        "base64", "serialize", "functors", "misc",
        "enums", "cmdlineargs", "i18n", "workspace", "libdl", "int",
        "intset", "floatfuncs", "compile", "parallel"
    ]

    if Base.USE_GPL_LIBS
        testnames = [testnames, "fft", "dsp"; ]
    end

    if isdir(joinpath(JULIA_HOME, Base.DOCDIR, "examples"))
        push!(testnames, "examples")
    end

    tests = []
    skip_tests = []

    for (i, t) in enumerate(choices)
        if t == "--skip"
            skip_tests = choices[i + 1:end]
            break
        else
            push!(tests, t)
        end
    end

    if tests == ["all"] || isempty(tests)
        tests = testnames
    end

    linalgtests = ["linalg/triangular", "linalg/qr", "linalg/dense",
                   "linalg/matmul", "linalg/schur", "linalg/special",
                   "linalg/eigen", "linalg/bunchkaufman", "linalg/svd",
                   "linalg/lapack", "linalg/tridiag", "linalg/bidiag",
                   "linalg/diagonal", "linalg/pinv", "linalg/givens",
                   "linalg/cholesky", "linalg/lu", "linalg/symmetric",
                   "linalg/generic", "linalg/uniformscaling"]
    if Base.USE_GPL_LIBS
        push!(linalgtests, "linalg/arnoldi")
    end

    if "linalg" in skip_tests
        filter!(x -> (x != "linalg" && !(x in linalgtests)), tests)
    elseif "linalg" in tests
        # specifically selected case
        filter!(x -> x != "linalg", tests)
        prepend!(tests, linalgtests)
    end

    codegen_testnames = ["simdloop"]

    # net is required for networking tests and for tests that must be run in a worker process
    # with different exeflags (eg. codegen tests)
    net_required_for = ["socket", "parallel", codegen_testnames...]
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

    filter!(x -> !(x in skip_tests), tests)

    # Separate the code generation tests, they need different exeflags
    codegen_tests = []
    filter!(tests) do t
        if t in codegen_testnames
            push!(codegen_tests, t)
            return false
        end
        return true
    end

    tests, codegen_tests, net_on
end
