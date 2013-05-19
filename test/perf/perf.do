clear
clear mata
run functions.do
mata:


// 1. Test if functions function
// I. Recursive Fibonacci. Fib(20)=6765
printf("Fib(20) = %5.0f\n",fib(20))
// II. Parse Int
// Seems to make no sense in Stata
// III. Mandelbrot set: complex arithmetic and comprehensions
// Should be 14628 according to original Matlab code.
mandel(-.53+.68i)
sum(sum(mandelperf(1)))
// IV. Quicksort
qsort(runiform(5,1))
// V. Pi (Should give 1.644834071848065)
pisum(1)
pisumvec(1)
// VI. Random matrix statistics
// First one should be > .5 and second < 1 according to original Matlab code
randmatstat(100)
// Largish random number gen & matmul
randmatmul(100)
// I/O
// Can't find a way to output to /dev/null in Windows :)
// printfd(1)

// 2. Time functions
{
timeit("fib", &fib(), 20)
// timeit("parse_int", &parseintperf(), 1000)
timeit("mandel", &mandelperf(), 1)
timeit("quicksort", &sortperf(), 5000)
timeit("pi_sum",&pisum(), 1)
timeit("pi_sum_vec",&pisumvec(), 1)
timeit("rand_mat_stat", &randmatstat(), 1000)
timeit("rand_mat_mul", &randmatmul(), 1000)
// timeit("printfd", &printfd(), 100000)
}

end
