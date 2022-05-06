# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random, Sockets

const STDLIB_DIR = Sys.STDLIB
const STDLIBS = filter!(x -> isfile(joinpath(STDLIB_DIR, x, "src", "$(x).jl")), readdir(STDLIB_DIR))

const TESTNAMES = [
        "subarray", "core", "compiler", "worlds", "atomics",
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
        "misc", "threads", "stress", "binaryplatforms", "atexit",
        "enums", "cmdlineargs", "int", "interpreter",
        "checked", "bitset", "floatfuncs", "precompile",
        "boundscheck", "error", "ambiguous", "cartesian", "osutils",
        "channels", "iostream", "secretbuffer", "specificity",
        "reinterpretarray", "syntax", "corelogging", "missing", "asyncmap",
        "smallarrayshrink", "opaque_closure", "filesystem", "download",
]

"""
`(; tests, net_on, exit_on_error, seed) = choosetests(choices)` selects a set of tests to be
run. `choices` should be a vector of test names; if empty or set to
`["all"]`, all tests are selected.

This function also supports "test collections": specifically, "linalg"
 refers to collections of tests in the correspondingly-named
directories.

The function returns a named tuple with the following elements:
  - `tests` is a vector of fully-expanded test names,
  - `net_on` is true if networking is available (required for some tests),
  - `exit_on_error` is true if an error in one test should cancel
    remaining tests to be run (otherwise, all tests are run unconditionally),
  - `seed` is a seed which will be used to initialize the global RNG for each
    test to be run.

Several options can be passed to `choosetests` by including a special token
in the `choices` argument:
   - "--skip", which makes all tests coming after be skipped,
   - "--exit-on-error" which sets the value of `exit_on_error`,
   - "--seed=SEED", which sets the value of `seed` to `SEED`
     (parsed as an `UInt128`); `seed` is otherwise initialized randomly.
     This option can be used to reproduce failed tests.
   - "--help", which prints a help message and then skips all tests.
   - "--help-list", which prints the options computed without running them.
"""
function choosetests(choices = [])
    tests = []
    skip_tests = Set()
    exit_on_error = false
    use_revise = false
    seed = rand(RandomDevice(), UInt128)
    ci_option_passed = false
    dryrun = false

    for (i, t) in enumerate(choices)
        if t == "--skip"
            union!(skip_tests, choices[i + 1:end])
            break
        elseif t == "--exit-on-error"
            exit_on_error = true
        elseif t == "--revise"
            use_revise = true
        elseif startswith(t, "--seed=")
            seed = parse(UInt128, t[(length("--seed=") + 1):end])
        elseif t == "--ci"
            ci_option_passed = true
        elseif t == "--help-list"
            dryrun = true
        elseif t == "--help"
            println("""
                USAGE: ./julia runtests.jl [options] [tests]
                OPTIONS:
                  --exit-on-error      : stop tests immediately when a test group fails
                  --help               : prints this help message
                  --help-list          : prints the options computed without running them
                  --revise             : load Revise
                  --seed=<SEED>        : set the initial seed for all testgroups (parsed as a UInt128)
                  --skip <NAMES>...    : skip test or collection tagged with <NAMES>
                TESTS:
                  Can be special tokens, such as "all", "unicode", "stdlib", the names of stdlib \
                  modules, or the names of any file in the TESTNAMES array (defaults to "all").

                  Or prefix a name with `-` (such as `-core`) to skip a particular test.
                """)
            return (; tests = [],
                      net_on = false,
                      exit_on_error = false,
                      use_revise = false,
                      seed = UInt128(0))
        elseif startswith(t, "--")
            error("unknown option: $t")
        elseif startswith(t, "-")
            push!(skip_tests, t[2:end])
        else
            push!(tests, t)
        end
    end

    unhandled = copy(skip_tests)

    requested_all     = "all"     in tests
    requested_default = "default" in tests
    if isempty(tests) || requested_all || requested_default
        append!(tests, TESTNAMES)
    end
    filter!(x -> x != "all",     tests)
    filter!(x -> x != "default", tests)

    function filtertests!(tests, name, files=[name])
       flt = x -> (x != name && !(x in files))
       if name in skip_tests
           filter!(flt, tests)
           pop!(unhandled, name)
       elseif name in tests
           filter!(flt, tests)
           prepend!(tests, files)
       end
    end

    explicit_pkg            = "Pkg"            in tests
    explicit_libgit2_online = "LibGit2/online" in tests

    filtertests!(tests, "unicode", ["unicode/utf8"])
    filtertests!(tests, "strings", ["strings/basic", "strings/search", "strings/util",
                   "strings/io", "strings/types"])
    # do subarray before sparse but after linalg
    filtertests!(tests, "subarray")
    filtertests!(tests, "compiler", ["compiler/inference", "compiler/validation",
        "compiler/ssair", "compiler/irpasses", "compiler/codegen",
        "compiler/inline", "compiler/contextual", "compiler/AbstractInterpreter",
        "compiler/EscapeAnalysis/local", "compiler/EscapeAnalysis/interprocedural"])
    filtertests!(tests, "compiler/EscapeAnalysis", [
        "compiler/EscapeAnalysis/local", "compiler/EscapeAnalysis/interprocedural"])
    filtertests!(tests, "stdlib", STDLIBS)
    # do ambiguous first to avoid failing if ambiguities are introduced by other tests
    filtertests!(tests, "ambiguous")

    if startswith(string(Sys.ARCH), "arm")
        # Remove profile from default tests on ARM since it currently segfaults
        # Allow explicitly adding it for testing
        @warn "Skipping Profile tests because the architecture is ARM"
        filter!(x -> (x != "Profile"), tests)
    end

    net_required_for = [
        "Artifacts",
        "Downloads",
        "LazyArtifacts",
        "LibCURL",
        "LibGit2",
        "Sockets",
        "download",
    ]
    net_on = true
    JULIA_TEST_NETWORKING_AVAILABLE = get(ENV, "JULIA_TEST_NETWORKING_AVAILABLE", "") |>
                                      strip |>
                                      lowercase |>
                                      s -> tryparse(Bool, s) |>
                                      x -> x === true
    # If the `JULIA_TEST_NETWORKING_AVAILABLE` environment variable is set to `true`, we
    # always set `net_on` to `true`.
    # Otherwise, we set `net_on` to true if and only if networking is actually available.
    if !JULIA_TEST_NETWORKING_AVAILABLE
        try
            ipa = getipaddr()
        catch
            if ci_option_passed
                @error("Networking unavailable, but `--ci` was passed")
                rethrow()
            end
            net_on = false
            @warn "Networking unavailable: Skipping tests [" * join(net_required_for, ", ") * "]"
            filter!(!in(net_required_for), tests)
        end
    end

    if ccall(:jl_running_on_valgrind,Cint,()) != 0 && "rounding" in tests
        @warn "Running under valgrind: Skipping rounding tests"
        filter!(x -> x != "rounding", tests)
    end

    filter!(!in(tests), unhandled)
    filter!(!in(skip_tests), tests)

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

    requested_all || explicit_pkg            || filter!(x -> x != "Pkg",            tests)
    requested_all || explicit_libgit2_online || filter!(x -> x != "LibGit2/online", tests)

    # Filter out tests from the test groups in the stdlibs
    filter!(!in(tests), unhandled)
    filter!(!in(skip_tests), tests)

    if !isempty(unhandled)
        @warn "Not skipping tests: $(join(unhandled, ", "))"
    end

    if dryrun
        print("Tests enabled to run:")
        foreach(t -> print("\n  ", t), tests)
        if !isempty(skip_tests)
            print("\n\nTests skipped:")
            foreach(t -> print("\n  ", t), skip_tests)
        end
        print("\n")
        exit_on_error && (print("\nwith option "); printstyled("exit_on_error", bold=true))
        use_revise && (print("\nwith option "); printstyled("use_revise", bold=true); print(" (Revise.jl)"))
        print("\n\n")
        empty!(tests)
    end

    return (; tests, net_on, exit_on_error, use_revise, seed)
end
