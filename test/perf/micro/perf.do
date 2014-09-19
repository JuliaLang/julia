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
	real scalar sum
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



program fib, rclass
args n
	if (`n' < 2) {
		return scalar fib = 2
	}
	else {
		fib `= `n' - 1'
		local x = r(fib)
		fib `= `n' - 2'
		return scalar fib = `x' + r(fib)
	}
end


program parse_int
args t
	tempname r n m
	forvalues i = 0 / `t' {
		scalar `r' = runiform()
		scalar `n' = floor(2^31 - `r')
		local s = string(`n', "%21x")
		scalar `m' = real("`s'")
		assert `m'==`n'
	}
end



// quick sort
program qsort_kernel
args a lo hi
	tempname t pivot
	local i = `lo'
	local j = `hi'
	quietly while (`i' < `hi') {
		local idx = floor((`lo' + `hi') / 2)
		scalar `pivot' = `a'[`idx']
		while (`i' <= `j') {
			while (`a'[`i'] < `pivot') {
				local ++i
			}
			while (`a'[`j'] > `pivot') {
				local --j
			}
			if (`i' <= `j') {
				scalar `t' = `a'[`i']
				replace `a' = `a'[`j'] in `i'
				replace `a' = `t' in `j'
				local ++i
				local --j
			}
		}
		if (`lo' < `j') qsort_kernel `a' `lo' `j'
		local lo = `i'
		local j = `hi'
	}
end

program quicksort
args n
	drop _all
	set obs `n'
	tempvar x
	g `x' = runiform()
	qsort_kernel `x' 1 `n'
end


// mandelbrot
program mandel
	tempname vr vi ri
	forvalues r = -20/5 {
		forvalues i = -10/10 {
			scalar `vr' = `r' / 10
			scalar `vi' = `i' / 10
			local j 0
			forvalues n = 1 / 80 {
				if (sqrt(`vr'^2 + `vi'^2) > 2) continue, break
				scalar `ri' = `vr' * `vi'
				scalar `vr' = `vr'^2 - `vi'^2 + `r' / 10
				scalar `vi' = 2 * `ri' + `i' / 10
				local j `n'
			}
			di %4.0g `j' _c
		}
		di
	}
end

// pi
program pi_sum
	tempname sum
    scalar `sum' = 0
    forvalues j = 1/500 {
        scalar `sum' = 0
        forvalues k = 1/10000 {
            scalar `sum' = `sum' + 1 / (`k' * `k')
        }
    }
    di `sum'
end




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

// RUN MATA TESTS
qui foreach test in fib parse_int quicksort mandel pi_sum rand_mat_stat rand_mat_mul {
	timer clear 1
	timer on 1
	mata : `test'(``test'')
	timer off 1
	timer list 1
	file write `results' "stata,`test',`=r(t1) * 1000'" _n
}

// RUN STATA TESTS
qui foreach test in fib parse_int quicksort mandel pi_sum {
	timer clear 1
	timer on 1
	`test' ``test''
	timer off 1
	timer list 1
	file write `results' "stata,`test'_stata,`=r(t1) * 1000'" _n
}



file close `results'

