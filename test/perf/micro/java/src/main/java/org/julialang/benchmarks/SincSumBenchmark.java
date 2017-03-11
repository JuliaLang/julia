package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;

public class SincSumBenchmark {

    @Benchmark
    public double sincSum() {
        return PerfPure.sinc_sum(1000);
    }
}
