package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;

public class RandMatMulBenchmark {

    @Benchmark
    public double randMatMul() {
        return PerfPure.randmatmul(1000);
    }
}
