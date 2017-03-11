package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;

public class MandelBenchmark {

    @Benchmark
    public int mandel() {
        return PerfPure.mandelperf();
    }
}
