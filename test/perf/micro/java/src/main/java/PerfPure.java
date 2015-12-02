import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Random;

import org.ejml.simple.SimpleMatrix;

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
    
    protected final int NITER = 5;
    protected Random rand = new Random(0);

    public static void main(String[] args) {
        PerfPure p = new PerfPure();
        p.runBenchmarks();
    }
    
    void runBenchmarks() {

        long t, tmin;

        assert(fib(20) == 6765);
        int f = 0;
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            f += fib(20);
            t = (System.nanoTime())-t;
            if (t < tmin) tmin = t;
        }
        print_perf("fib", tmin);

        // parse_bin
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            for (int k=0; k<1000; ++k) {
                int n = rand.nextInt(Integer.MAX_VALUE);
                String str = Integer.toHexString(n);
                int m = Integer.valueOf(str, 16);
                assert(m == n);
            }
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        print_perf("parse_int", tmin);

        // mandel
        int mandel_sum = 0;
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            mandel_sum = mandelperf();
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        assert(mandel_sum == 14720) : "value was "+mandel_sum;
        print_perf("mandel", tmin);

        // sort
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            int j = 5000;
            double[] d = new double[j];
            while (--j>=0) {
                d[j] = rand.nextDouble();
            }
            quicksort(d, 0, 5000-1);
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        print_perf("quicksort", tmin);

        // pi sum
        double pi = 0;
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            pi = pisum();
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        assert(Math.abs(pi-1.644834071848065) < 1e-12);
        print_perf("pi_sum", tmin);

        // rand mat stat
        double[] r;
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            r = randmatstat(1000);
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        print_perf("rand_mat_stat", tmin);

        // rand mat mul
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            double d = randmatmul(1000);
            assert(0 <= d);
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        print_perf("rand_mat_mul", tmin);


        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            sinc_sum(1000);
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        print_perf("sinc_sum", tmin);

        // printfd
        tmin = Long.MAX_VALUE;
        for (int i=0; i<NITER; ++i) {
            t = System.nanoTime();
            printfd(100000);
            t = System.nanoTime()-t;
            if (t < tmin) tmin = t;
        }
        print_perf("printfd", tmin);
    }

    void printfd(int n) {
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


    protected double sinc_sum(int n) {

        double total = 0;
        for (int i=0; i < n; i++) {
            double f= Math.PI*i;
            double sinc = Math.sin(f)/f;
            total+=sinc;
        }
        return total;
    }

    private double randmatmul(int i) {
        SimpleMatrix a = SimpleMatrix.random(i, i,  -1d, +1d, rand);
        SimpleMatrix b = SimpleMatrix.random(i, i,  -1d, +1d, rand);
        return a.mult(b).get(0);
    }

    private double[] randmatstat(int t) {
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

    public double stdev(List<Double> elements) {
        double m = mean(elements);
        double total = 0;
        for(Double d:elements) {
            double dif = (d-m);
            total += dif*dif;
        }
        return Math.sqrt(total/(elements.size()-1));
    }

    public double mean(List<Double> elements) {
        double total = 0;
        for(Double d:elements) {
            total += d;
        }
        return total/elements.size();
    }

    public double stdev(SimpleMatrix sm) {
            double m = mean(sm);
            double total = 0;

            int i = sm.getNumElements();
            while (--i>=0) {
                double dif = (sm.get(i)-m);
                total += dif*dif;
            }
            return Math.sqrt(total/(sm.getNumElements()-1));
        }

    public double mean(SimpleMatrix sm) {
        double total = 0;
        int i = sm.getNumElements();
        while (--i>=0) {
            total += sm.get(i);
        }
        return total/sm.getNumElements();
    }

    protected void quicksort(double[] a, int lo, int hi) {
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

    protected double pisum() {
        double sum = 0.0;
        for (int j=0; j<500; ++j) {
            sum = 0.0;
            for (int k=1; k<=10000; ++k) {
                sum += 1.0/(k*k);
            }
        }
        return sum;
    }

    private int mandel(double zReal, double zImag) {
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

    private double complexAbs(double zReal, double zImag) {
        return Math.sqrt(zReal*zReal + zImag*zImag);
    }

    protected int mandelperf() {
        int mandel_sum = 0;
        for (double re=-2.0; re<=0.5; re+=0.1) {
            for (double im=-1.0; im<=1.0; im+=0.1) {
                int m = mandel(re,im);
                mandel_sum += m;
            }
        }
        return mandel_sum;
    }

    protected void print_perf(String name, long t) {
        System.out.printf("javaPure,%s,%.6f\n", name, t/(double)1E6);
    }

    protected int fib(int n) {
        return n < 2 ? n : fib(n-1) + fib(n-2);
    }

}

