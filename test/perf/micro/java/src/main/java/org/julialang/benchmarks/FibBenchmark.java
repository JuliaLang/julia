package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;

public class FibBenchmark {

    @Benchmark
    public int fib() {
        return PerfPure.fib(20);
    }
}
