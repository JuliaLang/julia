package org.julialang.benchmarks.blas;

import org.julialang.PerfBLAS;
import org.openjdk.jmh.annotations.Benchmark;

public class RandMatStatBlasBenchmark {

    @Benchmark
    public double[] randMatStat() {
        return PerfBLAS.randmatstat(1000);
    }
}
