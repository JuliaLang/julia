Julia performance monitoring
============================

This directory contains benchmarks and related utilities to test Julia's
performance. Many of these benchmarks have been ported to the newer
[BaseBenchmarks package](https://github.com/JuliaCI/BaseBenchmarks.jl),
which contains the benchmark suite used for CI performance testing.
In general, new benchmarks should be added to that package instead
of placed here (see the BaseBenchmarks README for details).

If you'd like to test the performance of your own package, consider using
the [BenchmarkTools package](https://github.com/JuliaCI/BenchmarkTools.jl).

Running the performance tests
-----------------------------

In `test/perf` run `make`.  It will run the `perf.jl` script in all
the sub-directories and display the test name with the minimum,
maximum, mean and standard deviation of the wall-time of five repeated
test runs in micro seconds.

There is also a `perfcomp.jl` script but it may not be working with
the rest at the moment.

Code Organization
-----------------

Tests generally go into one of the following suites:

- `micro`: A set of micro-benchmarks commonly used to compare
  programming languages; these results are shown on
    [http://julialang.org/](http://julialang.org/).
- `blas`, `lapack`: Performance tests for linear algebra tasks from
  low-level operations such as matrix multiplies to higher-level
  operations like eigenvalue problems.
- `cat`: Performance tests for concatenation of vectors and matrices.
- `kernel`: Performance tests used to track real-world code examples
  that previously ran slowly.
- `shootout` Tracks the performance of tests taken from the
  [Computer Language Benchmarks Game](http://benchmarksgame.alioth.debian.org/) performance
  tests.
- `sort`: Performance tests of sorting algorithms.
- `spell` Performance tests of
  [Peter Norvig's spelling corrector](http://norvig.com/spell-correct.html).
- `sparse`: Performance tests of sparse matrix operations.

Otherwise tests live in their own subdirectories containing a `perf.jl` file.

The `perf.jl` files include the shared performance utilies via
`include("../perfutil.jl")`, and then run the performance test
functions with the `@timeit` macro. For example:
```julia
@timeit(spelltest(tests1), "spell", "Peter Norvig's spell corrector")
```
with arguments: test function call, name of the test, description,
and, optionally, a group.  `@timeit` will do a warm-up and then 5
timings, calculating min, max, average and standard deviation of
the timings.

If possible, the tests aim to take about 10-100 microseconds.

Package dependencies
--------------------
- HTTPClient
- JSON
- DataStructures
- SortingAlgorithms
