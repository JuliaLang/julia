import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Random;

import org.jblas.DoubleMatrix;

public class PerfBLAS {
	private static final int NITER = 5;
	private static Random rand = new Random(0);

	public static void main(String[] args) {

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
		        r = randmatstat_JBLAS(1000);
		        t = System.nanoTime()-t;
		        if (t < tmin) tmin = t;
		    }
		    print_perf("rand_mat_stat", tmin);

		    tmin = Long.MAX_VALUE;
		    for (int i=0; i<NITER; ++i) {
		        t = System.nanoTime();
		        DoubleMatrix C = randmatmul_JBLAS(1000);
		        assert(0 <= C.get(0));
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

	static void printfd(int n) {
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


	private static double sinc_sum(int n) {

		double total = 0;
		for (int i=0; i < n; i++) {
			double f= Math.PI*i;
			double sinc = Math.sin(f)/f;
			total+=sinc;
		}
		return total;
	}

	private static DoubleMatrix randmatmul_JBLAS(int i) {
		DoubleMatrix a = DoubleMatrix.randn(i,i);
		DoubleMatrix b = DoubleMatrix.randn(i,i);
		return a.mmul(b);
	}

	private static double[] randmatstat_JBLAS(int t) {
	    int n=5;
	    DoubleMatrix p;
	    DoubleMatrix q;
	    DoubleMatrix v = new DoubleMatrix(new double[t][1]); //zeros(t,1);
	    DoubleMatrix w = new DoubleMatrix(new double[t][1]); //zeros(t,1);
	    for (int i=0; i < t; i++) {
	    	DoubleMatrix a = DoubleMatrix.randn(n,n);
	    	DoubleMatrix b = DoubleMatrix.randn(n,n);
	    	DoubleMatrix c = DoubleMatrix.randn(n,n);
	    	DoubleMatrix d = DoubleMatrix.randn(n,n);

	    	p = DoubleMatrix.concatHorizontally(DoubleMatrix.concatHorizontally(a, b),DoubleMatrix.concatHorizontally(c, d));
	    	q = DoubleMatrix.concatVertically(DoubleMatrix.concatHorizontally(a, b),DoubleMatrix.concatHorizontally(c, d));

	    	DoubleMatrix x = p.transpose().mmul(p);
	    	x = x.mmul(x);
	    	x = x.mmul(x);
	    	v.data[i]=x.diag().sum();

	    	x = q.transpose().mmul(q);
	    	x = x.mmul(x);
	    	x = x.mmul(x);
	    	w.data[i]=x.diag().sum();

	    }

	    List<Double> vElements = v.elementsAsList();
	    List<Double> wElements = w.elementsAsList();

	    return new double[]{stdev(vElements)/mean(vElements),stdev(wElements)/mean(wElements)};
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

	private static void quicksort(double[] a, int lo, int hi) {
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

	private static double pisum() {
	    double sum = 0.0;
	    for (int j=0; j<500; ++j) {
	        sum = 0.0;
	        for (int k=1; k<=10000; ++k) {
	            sum += 1.0/(k*k);
	        }
	    }
	    return sum;
	}

	private static int mandel(double re, double im) {
	    int n = 0;
	    Complex z = new Complex(re, im);
	    Complex c = new Complex(re, im);
	    for (n=0; n<=79; ++n) {
	        if (Complex.abs(z) > 2.0) {
	            n -= 1;
	            break;
	        }

	        // z = z*z + c
	        z = Complex.add(Complex.mul(z, z), c);
	    }
	    return n+1;
	}

	private static int mandelperf() {
	    int mandel_sum = 0;
	    for (double re=-2.0; re<=0.5; re+=0.1) {
	        for (double im=-1.0; im<=1.0; im+=0.1) {
	            int m = mandel(re,im);
	            mandel_sum += m;
	        }
	    }
	    return mandel_sum;
	}

	private static void print_perf(String name, long t) {
		System.out.printf("java,%s,%.6f\n", name, t/(double)1E6);
	}

	private static int fib(int n) {
		return n < 2 ? n : fib(n-1) + fib(n-2);
	}

}

