# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random, Sockets

const STDLIB_DIR = Sys.STDLIB
const STDLIBS = filter!(x -> isfile(joinpath(STDLIB_DIR, x, "src", "$(x).jl")), readdir(STDLIB_DIR))

"""

`tests, net_on, exit_on_error, seed = choosetests(choices)` selects a set of tests to be
run. `choices` should be a vector of test names; if empty or set to
`["all"]`, all tests are selected.

This function also supports "test collections": specifically, "linalg"
 refers to collections of tests in the correspondingly-named
directories.

Upon return:
  - `tests` is a vector of fully-expanded test names,
  - `net_on` is true if networking is available (required for some tests),
  - `exit_on_error` is true if an error in one test should cancel
    remaining tests to be run (otherwise, all tests are run unconditionally),
  - `seed` is a seed which will be used to initialize the global RNG for each
    test to be run.

Three options can be passed to `choosetests` by including a special token
in the `choices` argument:
   - "--skip", which makes all tests coming after be skipped,
   - "--exit-on-error" which sets the value of `exit_on_error`,
   - "--seed=SEED", which sets the value of `seed` to `SEED`
     (parsed as an `UInt128`); `seed` is otherwise initialized randomly.
     This option can be used to reproduce failed tests.
"""
function choosetests(choices = [])
    testnames = [
        "subarray", "core", "compiler", "worlds",
        "keywordargs", "numbers", "subtype",
        "char", "strings", "triplequote", "unicode", "intrinsics",
        "dict", "hashing", "iobuffer", "staged", "offsetarray",
        "arrayops", "tuple", "reduce", "reducedim", "abstractarray",
        "intfuncs", "simdloop", "vecelement", "rational",
        "bitarray", "copy", "math", "fastmath", "functional", "iterators",
        "operators", "ordering", "path", "ccall", "parse", "loading", "gmp",
        "sorting", "spawn", "backtrace", "exceptions",
        "file", "read", "version", "namedtuple",
        "mpfr", "broadcast", "complex",
        "floatapprox", "stdlib", "reflection", "regex", "float16",
        "combinatorics", "sysinfo", "env", "rounding", "ranges", "mod2pi",
        "euler", "show", "client",
        "errorshow", "sets", "goto", "llvmcall", "llvmcall2", "ryu",
        "some", "meta", "stacktraces", "docs",
        "misc", "threads", "stress",
        "enums", "cmdlineargs", "int", "interpreter",
        "checked", "bitset", "floatfuncs", "precompile",
        "boundscheck", "error", "ambiguous", "cartesian", "osutils",
        "channels", "iostream", "secretbuffer", "specificity",
        "reinterpretarray", "syntax", "logging", "missing", "asyncmap", "atexit",
        "collections",
    ]

    tests = []
    skip_tests = []
    exit_on_error = false
    use_revise = false
    seed = rand(RandomDevice(), UInt128)

    for (i, t) in enumerate(choices)
        if t == "--skip"
            skip_tests = choices[i + 1:end]
            break
        elseif t == "--exit-on-error"
            exit_on_error = true
        elseif t == "--revise"
            use_revise = true
        elseif startswith(t, "--seed=")
            seed = parse(UInt128, t[8:end])
        else
            push!(tests, t)
        end
    end

    if tests == ["all"] || isempty(tests)
        tests = testnames
    end

    function filtertests!(tests, name, files=[name])
       flt = x -> (x != name && !(x in files))
       if name in skip_tests
           filter!(flt, tests)
       elseif name in tests
           filter!(flt, tests)
           prepend!(tests, files)
       end
    end

    filtertests!(tests, "unicode", ["unicode/utf8"])
    filtertests!(tests, "strings", ["strings/basic", "strings/search", "strings/util",
                   "strings/io", "strings/types"])
    # do subarray before sparse but after linalg
    filtertests!(tests, "subarray")
    filtertests!(tests, "compiler", ["compiler/inference", "compiler/validation",
        "compiler/ssair", "compiler/irpasses", "compiler/codegen",
        "compiler/inline", "compiler/contextual"])
    filtertests!(tests, "stdlib", STDLIBS)
    # do ambiguous first to avoid failing if ambiguities are introduced by other tests
    filtertests!(tests, "ambiguous")

    if startswith(string(Sys.ARCH), "arm")
        # Remove profile from default tests on ARM since it currently segfaults
        # Allow explicitly adding it for testing
        @warn "Skipping Profile tests"
        filter!(x -> (x != "Profile"), tests)
    end

    net_required_for = ["Sockets", "LibGit2"]
    net_on = true
    try
        ipa = getipaddr()
    catch
        @warn "Networking unavailable: Skipping tests [" * join(net_required_for, ", ") * "]"
        net_on = false
    end

    if !net_on
        filter!(!in(net_required_for), tests)
    end

    if ccall(:jl_running_on_valgrind,Cint,()) != 0 && "rounding" in tests
        @warn "Running under valgrind: Skipping rounding tests"
        filter!(x -> x != "rounding", tests)
    end

    filter!(!in(skip_tests), tests)

    explicit_pkg3    =  "Pkg/pkg"       in tests
    explicit_libgit2 =  "LibGit2/online" in tests
    new_tests = String[]
    for test in tests
        if test in STDLIBS
            testfile = joinpath(STDLIB_DIR, test, "test", "testgroups")
            if isfile(testfile)
                testgroups = readlines(testfile)
                length(testgroups) == 0 && error("no testgroups defined for $test")
                prepend!(new_tests, (test * "/") .* testgroups)
            else
                push!(new_tests, test)
            end
        end
    end
    filter!(x -> (x != "stdlib" && !(x in STDLIBS)) , tests)
    append!(tests, new_tests)
    explicit_pkg3    || filter!(x -> x != "Pkg/pkg",       tests)
    explicit_libgit2 || filter!(x -> x != "LibGit2/online", tests)

    # Filter out tests from the test groups in the stdlibs
    filter!(!in(skip_tests), tests)

    tests, net_on, exit_on_error, use_revise, seed
end
