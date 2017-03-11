package org.julialang;

import org.ejml.simple.SimpleMatrix;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

/**
 * (Below excerpt is printed on the website and repeated here)
 * 
 * These benchmarks, while not comprehensive, do test compiler performance on a range of common code patterns, 
 * such as function calls, string parsing, sorting, numerical loops, random number generation, and array operations. 
 * It is important to note that these benchmark implementations are not written for absolute maximal performance 
 * (the fastest code to compute fib(20) is the constant literal 6765). Rather, all of the benchmarks are written 
 * to test the performance of specific algorithms, expressed in a reasonable idiom in each language. 
 * In particular, all languages use the same algorithm: the Fibonacci benchmarks are all recursive while 
 * the pi summation benchmarks are all iterative; the “algorithm” for random matrix multiplication is to 
 * call LAPACK, except where that’s not possible, such as in JavaScript. The point of these benchmarks is to 
 * compare the performance of specific algorithms across language implementations, not to compare the fastest 
 * means of computing a result, which in most high-level languages relies on calling C code. 
 *
 */
public class PerfPure {
    
    protected static Random rand = ThreadLocalRandom.current();

    public static void printfd(int n) {
        try {
            FileOutputStream f = new FileOutputStream("/dev/null");
            PrintStream ps = new PrintStream(f);
            long i = 0;
            for (i = 0; i < n; i++) {
                ps.println(i+" "+i);
            }
            ps.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }


    public static double sinc_sum(int n) {
        double total = 0;
        for (int i=0; i < n; i++) {
            double f= Math.PI*i;
            double sinc = Math.sin(f)/f;
            total+=sinc;
        }
        return total;
    }

    public static double randmatmul(int i) {
        SimpleMatrix a = SimpleMatrix.random(i, i,  -1d, +1d, rand);
        SimpleMatrix b = SimpleMatrix.random(i, i,  -1d, +1d, rand);
        return a.mult(b).get(0);
    }

    public static double[] randmatstat(int t) {
        int n=5;
        SimpleMatrix p = new SimpleMatrix(new double[n][4*n]);
        SimpleMatrix q = new SimpleMatrix(new double[2*n][2*n]);
        SimpleMatrix v = new SimpleMatrix(new double[t][1]); //zeros(t,1);
        SimpleMatrix w = new SimpleMatrix(new double[t][1]); //zeros(t,1);
        for (int i=0; i < t; i++) {
            SimpleMatrix a = SimpleMatrix.random(n, n,  -1d, +1d, rand);
            SimpleMatrix b = SimpleMatrix.random(n, n,  -1d, +1d, rand);
            SimpleMatrix c = SimpleMatrix.random(n, n,  -1d, +1d, rand);
            SimpleMatrix d = SimpleMatrix.random(n, n,  -1d, +1d, rand);

            p.combine(0, 0*n, a);
            p.combine(0, 1*n, b);
            p.combine(0, 2*n, c);
            p.combine(0, 3*n, d);

            q.combine(0, 0*n, a);
            q.combine(0, 1*n, b);
            q.combine(n, 0*n, c);
            q.combine(n, 1*n, d);

            SimpleMatrix x = p.transpose().mult(p);
            x = x.mult(x);
            x = x.mult(x);
            v.set(i, x.trace());

            x = q.transpose().mult(q);
            x = x.mult(x);
            x = x.mult(x);
            w.set(i, x.trace());

        }
        return new double[]{stdev(v)/mean(v),stdev(w)/mean(w)};
    }

    public static double stdev(List<Double> elements) {
        double m = mean(elements);
        double total = 0;
        for(Double d:elements) {
            double dif = (d-m);
            total += dif*dif;
        }
        return Math.sqrt(total/(elements.size()-1));
    }

    public static double mean(List<Double> elements) {
        double total = 0;
        for(Double d:elements) {
            total += d;
        }
        return total/elements.size();
    }

    public static double stdev(SimpleMatrix sm) {
            double m = mean(sm);
            double total = 0;

            int i = sm.getNumElements();
            while (--i>=0) {
                double dif = (sm.get(i)-m);
                total += dif*dif;
            }
            return Math.sqrt(total/(sm.getNumElements()-1));
        }

    public static double mean(SimpleMatrix sm) {
        double total = 0;
        int i = sm.getNumElements();
        while (--i>=0) {
            total += sm.get(i);
        }
        return total/sm.getNumElements();
    }

    public static void quicksort(double[] a, int lo, int hi) {
        int i = lo;
        int j = hi;
        while (i < hi) {
            double pivot = a[(lo+hi)/2];
            // Partition
            while (i <= j) {
                while (a[i] < pivot) {
                    i = i + 1;
                }
                while (a[j] > pivot) {
                    j = j - 1;
                }
                if (i <= j) {
                    double t = a[i];
                    a[i] = a[j];
                    a[j] = t;
                    i = i + 1;
                    j = j - 1;
                }
            }

            // Recursion for quicksort
            if (lo < j) {
                quicksort(a, lo, j);
            }
            lo = i;
            j = hi;
        }
    }

    public static double pisum() {
        double sum = 0.0;
        for (int j=0; j<500; ++j) {
            sum = 0.0;
            for (int k=1; k<=10000; ++k) {
                sum += 1.0/(k*k);
            }
        }
        return sum;
    }

    private static int mandel(double zReal, double zImag) {
        int n = 0;
        double cReal = zReal;
        double cImag = zImag;
        for (n=0; n<=79; ++n) {
            if (complexAbs(zReal,zImag) > 2.0) {
                n -= 1;
                break;
            }

            // z^2
            double zSquaredReal = zReal*zReal-zImag*zImag;
            double zSquaredImag = zReal*zImag+zImag*zReal;

            // +c
            zReal = zSquaredReal+cReal;
            zImag = zSquaredImag+cImag;

        }
        return n+1;
    }

    private static double complexAbs(double zReal, double zImag) {
        return Math.sqrt(zReal*zReal + zImag*zImag);
    }

    public static int mandelperf() {
        int mandel_sum = 0;
        for (double re=-2.0; re<=0.5; re+=0.1) {
            for (double im=-1.0; im<=1.0; im+=0.1) {
                int m = mandel(re,im);
                mandel_sum += m;
            }
        }
        return mandel_sum;
    }

    public static int fib(int n) {
        return n < 2 ? n : fib(n-1) + fib(n-2);
    }

}

