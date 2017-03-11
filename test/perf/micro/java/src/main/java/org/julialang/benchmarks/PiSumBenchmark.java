package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;

public class PiSumBenchmark {

    @Benchmark
    public double piSum() {
        return PerfPure.pisum();
    }
}
