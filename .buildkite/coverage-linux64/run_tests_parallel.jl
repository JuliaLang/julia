# When running this file, make sure to set the `--code-coverage=all` command-line flag.

# Important note: even if one or more tests fail, we will still exit with status code 0.

# The reason for this is that we always want to upload code coverage, even if some of the
# tests fail. Therefore, even if the `coverage-linux64` pipeline passes, you should not
# assume that all of the tests passed. If you want to know if all of the tests are passing,
# please look at the status of the `tester_linux64` pipeline.

const include_tests = String[]

const exclude_tests = String[]

empty!(Base.DEPOT_PATH)
push!(Base.DEPOT_PATH, mktempdir(; cleanup = true))

module ChooseTests
    include(joinpath(dirname(dirname(@__DIR__)), "test", "choosetests.jl"))
end

const tests = ChooseTests.choosetests() |>
              first |>
              x -> setdiff(x, exclude_tests) |>
              x -> vcat(x, include_tests) |>
              unique |>
              sort

const ncores = min(Sys.CPU_THREADS, Threads.nthreads())

@info "" ncores Sys.CPU_THREADS Threads.nthreads()

try
    Base.runtests(tests; ncores)
catch ex
    @error "" exception=(ex, catch_backtrace())
end
