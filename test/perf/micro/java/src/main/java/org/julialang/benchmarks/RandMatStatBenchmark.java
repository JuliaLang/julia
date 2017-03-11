package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;

public class RandMatStatBenchmark {

    @Benchmark
    public double[] randMatStat() {
        return PerfPure.randmatstat(1000);
    }
}
