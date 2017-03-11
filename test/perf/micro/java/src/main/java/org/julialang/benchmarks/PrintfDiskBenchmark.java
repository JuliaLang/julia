package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;

public class PrintfDiskBenchmark {

    @Benchmark
    public void printfd() {
        PerfPure.printfd(100000);
    }
}
