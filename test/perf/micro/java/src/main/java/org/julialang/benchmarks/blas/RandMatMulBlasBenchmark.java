package org.julialang.benchmarks.blas;

import org.julialang.PerfBLAS;
import org.openjdk.jmh.annotations.Benchmark;

public class RandMatMulBlasBenchmark {

    @Benchmark
    public double randMatMul() {
        return PerfBLAS.randMatMulBlas(1000);
    }
}
