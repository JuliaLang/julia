package org.julialang.benchmarks;

import org.julialang.PerfPure;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;

import java.util.Random;

@State(Scope.Benchmark)
public class QuickSortBenchmark {

    private final Random rand = new Random(0);
    private double[] doubleArray;

    @Setup(Level.Iteration)
    public void setup() {
        int j = 5000;
        doubleArray = new double[j];
        while (--j >= 0) {
            doubleArray[j] = rand.nextDouble();
        }
    }

    @Benchmark
    public double[] quickSort() {
        PerfPure.quicksort(doubleArray, 0, 5000 - 1);
        return doubleArray;
    }

}
