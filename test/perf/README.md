Julia performance monitoring
============================

This directory contains tests and related utilities to monitor Julia's
performance over time.  The results are presented on
[http://speed.julialang.org/](http://speed.julialang.org/).

Running the performance tests
-----------------------------

In `test/perf` run `make`.  It will run the `perf.jl` script in all
the sub-directories and display the test name with the minimum,
maximum, mean and standard deviation of the wall-time of five repeated
test runs in micro seconds.

Calling `make codespeed` is for generating the results displayed on
[http://speed.julialang.org/](http://speed.julialang.org/), probably
not what you want.

There is also a `perfcomp.jl` script but it may not be working with
the rest at the moment.

Adding tests
------------
First decide whether the new tests should go into one of the existing
suites:
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
  [Debian shootout](http://shootout.alioth.debian.org/) performance
  tests.
- `sort`: Performance tests of sorting algorithms.
- `spell` Performance tests of
  [Peter Norvig's spelling corrector](http://norvig.com/spell-correct.html).
- `sparse`: Performance tests of sparse matrix operations.

Otherwise add a subdirectory containing the file `perf.jl` and
update the `Makefile` as well.

In `perf.jl`, `include("../perfutil.jl")` and then run the
performance test functions with the `@timeit` macro.  For example:
```julia
@timeit(spelltest(tests1), "spell", "Peter Norvig's spell corrector")
```
with arguments: test function call, name of the test, description,
and, optionally, a group (only used for codespeed).  `@timeit` will do
a warm-up and then 5 timings, calculating min, max, average and standard
deviation of the timings.

If possible aim for the tests to take about 10-100 microseconds.

Using the framework for your own tests
--------------------------------------

Just include `perfutil.jl`, use `@timeit` on the functions to be
benchmarked. Alternatively have a look at the
[Benchmark package](https://github.com/johnmyleswhite/Benchmark.jl).


Package dependencies
--------------------
- HTTPClient
- JSON
- DataStructures
- SortingAlgorithms

