version 12
clear all

mata // start mata code

// fibonacci
real scalar fib(n) 
{
	if (n < 2) {
		return(n)
	} else {
		return(fib(n - 1) + fib(n - 2))
	}
}

// parse integer
void parse_int(real scalar t) {
	for(i = 0; i <= t; ++i) {
		r = runiform(1, 1)
		n = floor(2^31 - 1 * r[1, 1])
		s = sprintf("%21x", n)
		m = strtoreal(s)
		asserteq(m, n)
	}
}

// quick sort
real matrix qsort(real matrix a) 
{
	return(qsort_kernel(a, 1, length(a)))
}

real matrix qsort_kernel(a, lo, hi)
{
	i = lo
	j = hi
	while (i < hi) {
		pivot = a[floor((lo + hi) / 2)]
		while (i <= j) {
			while (a[i] < pivot) i = i + 1
			while (a[j] > pivot) j = j - 1
			if (i <= j) {
				t = a[i]
				a[i] = a[j]
				a[j] = t
				i = i + 1
				j = j - 1
			}
		}
		if (lo < j) a = qsort_kernel(a, lo, j)
		lo = i
		j = hi
	}
	return(a)
}

real matrix quicksort(n)
{
	v = runiform(n, 1)
	return(qsort(v))
}


// mandelbrot
real scalar mandelarg(z) 
{
	c = z
	maxiter = 80
	for(n = 1; n <= maxiter; ++n) {
		if (abs(z) > 2) return(n - 1)
		z = z^2 + c
	}
	return(maxiter)
}

real matrix mandel()
{
	rr = range(-2.0, 0.5, 0.1)
	ri = range(-1.0, 1.0, 0.1)
	nr = length(rr)
	ni = length(ri)
	M = J(nr, ni, 0)
	for(r = 1;r <= nr; ++r) {
		for(i = 1;i <= ni; ++i) {
			M[r, i] = mandelarg(C(rr[r], ri[i]))
		}
	}
	return(M)
}


// pi series
real scalar pi_sum() 
{
    sum = 0.0
    for(j = 1; j <= 500; ++j) {
        sum = 0.0
        for(k = 1; k <= 10000; ++k) {
            sum = sum + 1.0 / (k * k)
        }
    }
    //sum - 1.644834071848065
    return(sum)
}


// random matrix statistics
real matrix rand_mat_stat(t)
{ 
	n = 5
	m = J(n, 1, 0)
	s = J(n, 1, 1)
	v = J(t, 1, 0)
	w = J(t, 1, 0)
	for(i = 1; i <= t; ++i) {
		a = rnormal(n, n, m, s)
		b = rnormal(n, n, m, s)
		c = rnormal(n, n, m, s)
		d = rnormal(n, n, m, s)
		P = (a, b , c, d)
		Q = (a, b \ c, d)
		v[i] = trace(matpowersym(cross(P, 0, P, 0), 4))
		w[i] = trace(matpowersym(cross(Q, 0, Q, 0), 4))
	}
	s1 = sqrt(variance(v)) / mean(v)
	s2 = sqrt(variance(w)) / mean(w)
	return((s1, s2))
}


// largish random number gen & matmul
real matrix rand_mat_mul(real scalar n)
{
    return(runiform(n, n) * runiform(n, n))
}

end // end mata


// arguments 
local fib 20
local parse_int 1000
local quicksort 5000
local rand_mat_stat 1000
local rand_mat_mul 1000

set processors 1
set seed 200897813

tempname results
file open `results' using `1', write append

qui foreach test in fib parse_int quicksort mandel pi_sum rand_mat_stat rand_mat_mul {
	timer clear 1
	timer on 1
	mata : `test'(``test'')
	timer off 1
	timer list 1
	file write `results' "stata,`test',`=r(t1)'" _n
}

file close `results'

