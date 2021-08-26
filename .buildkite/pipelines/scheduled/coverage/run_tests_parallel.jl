# Important note: even if one or more tests fail, we will still exit with status code 0.

# The reason for this is that we always want to upload code coverage, even if some of the
# tests fail. Therefore, even if the `coverage_linux64` builder passes, you should not
# assume that all of the tests passed. If you want to know if all of the tests are passing,
# please look at the status of the `tester_*` builders (e.g. `tester_linux64`).

# When running this file, make sure to set all of the following command-line flags:
# 1. `--code-coverage=all`
# 2. `--sysimage-native-code=no`

empty!(Base.DEPOT_PATH)
push!(Base.DEPOT_PATH, mktempdir(; cleanup = true))

const tests = "all"
const ncores = Sys.CPU_THREADS

@info "" Sys.CPU_THREADS
@info "" tests ncores

try
    Base.runtests(tests; ncores)
catch ex
    @error "" exception=(ex, catch_backtrace())
end
