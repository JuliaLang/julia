package org.julialang.benchmarks;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;

import java.util.Random;

@State(Scope.Benchmark)
public class ParseBenchmark {

    private final Random rand = new Random(0);
    private String randomHex;

    @Setup(Level.Iteration)
    public void setup() {
        int randomInt = rand.nextInt(Integer.MAX_VALUE);
        randomHex = Integer.toHexString(randomInt);
    }

    @Benchmark
    public int parseInt() {
        return Integer.valueOf(randomHex, 16);
    }
}
