# This file is a part of Julia. License is MIT: http://julialang.org/license

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
        "linalg", "subarray", "core", "inference", "worlds",
        "keywordargs", "numbers", "subtype",
        "printf", "char", "strings", "triplequote", "unicode", "intrinsics",
        "dates", "dict", "hashing", "iobuffer", "staged", "offsetarray",
        "arrayops", "tuple", "reduce", "reducedim", "random", "abstractarray",
        "intfuncs", "simdloop", "vecelement", "blas", "sparse",
        "bitarray", "copy", "math", "fastmath", "functional", "iterators",
        "operators", "path", "ccall", "parse", "loading", "bigint",
        "bigfloat", "sorting", "statistics", "spawn", "backtrace",
        "file", "read", "mmap", "version", "resolve",
        "pollfd", "mpfr", "broadcast", "complex", "socket",
        "floatapprox", "datafmt", "reflection", "regex", "float16",
        "combinatorics", "sysinfo", "env", "rounding", "ranges", "mod2pi",
        "euler", "show", "lineedit", "replcompletions", "repl",
        "replutil", "sets", "test", "goto", "llvmcall", "llvmcall2", "grisu",
        "nullable", "meta", "stacktraces", "profile", "libgit2", "docs",
        "markdown", "base64", "serialize", "misc", "threads",
        "enums", "cmdlineargs", "i18n", "workspace", "libdl", "int",
        "checked", "intset", "floatfuncs", "compile", "parallel", "inline",
        "boundscheck", "error", "ambiguous", "cartesian", "asmvariant", "osutils",
        "channels"
    ]
    profile_skipped = false
    if startswith(string(Sys.ARCH), "arm")
        # Remove profile from default tests on ARM since it currently segfaults
        # Allow explicitly adding it for testing
        filter!(x -> (x != "profile"), testnames)
        profile_skipped = true
    end

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
        if profile_skipped
            warn("profile test skipped")
        end
    end

    datestests = ["dates/accessors", "dates/adjusters", "dates/query",
                  "dates/periods", "dates/ranges", "dates/rounding", "dates/types",
                  "dates/io", "dates/arithmetic", "dates/conversions"]
    if "dates" in skip_tests
        filter!(x -> (x != "dates" && !(x in datestests)), tests)
    elseif "dates" in tests
        # specifically selected case
        filter!(x -> x != "dates", tests)
        prepend!(tests, datestests)
    end

    unicodetests = ["unicode/UnicodeError", "unicode/utf8proc", "unicode/utf8"]
    if "unicode" in skip_tests
        filter!(x -> (x != "unicode" && !(x in unicodetests)), tests)
    elseif "unicode" in tests
        # specifically selected case
        filter!(x -> x != "unicode", tests)
        prepend!(tests, unicodetests)
    end

    stringtests = ["strings/basic", "strings/search", "strings/util",
                   "strings/io", "strings/types"]
    if "strings" in skip_tests
        filter!(x -> (x != "strings" && !(x in stringtests)), tests)
    elseif "strings" in tests
        # specifically selected case
        filter!(x -> x != "strings", tests)
        prepend!(tests, stringtests)
    end


    sparsetests = ["sparse/sparse", "sparse/sparsevector", "sparse/higherorderfns"]
    if Base.USE_GPL_LIBS
        append!(sparsetests, ["sparse/umfpack", "sparse/cholmod", "sparse/spqr"])
    end
    if "sparse" in skip_tests
        filter!(x -> (x != "sparse" && !(x in sparsetests)), tests)
    elseif "sparse" in tests
        # specifically selected case
        filter!(x -> x != "sparse", tests)
        prepend!(tests, sparsetests)
    end

    #do subarray before sparse but after linalg
    if "subarray" in skip_tests
        filter!(x -> x != "subarray", tests)
    elseif "subarray" in tests
        filter!(x -> x != "subarray", tests)
        prepend!(tests, ["subarray"])
    end

    linalgtests = ["linalg/triangular", "linalg/qr", "linalg/dense",
                   "linalg/matmul", "linalg/schur", "linalg/special",
                   "linalg/eigen", "linalg/bunchkaufman", "linalg/svd",
                   "linalg/lapack", "linalg/tridiag", "linalg/bidiag",
                   "linalg/diagonal", "linalg/pinv", "linalg/givens",
                   "linalg/cholesky", "linalg/lu", "linalg/symmetric",
                   "linalg/generic", "linalg/uniformscaling", "linalg/lq",
                   "linalg/hessenberg", "linalg/rowvector"]
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

    net_required_for = ["socket", "parallel", "libgit2"]
    net_on = true
    try
        ipa = getipaddr()
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

    tests, net_on
end
