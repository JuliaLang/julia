library(R.utils)

assert = function(bool) {
	if (!bool) stop('Assertion failed')
}

timeit = function(name, f, ...) {
	tmin = Inf
	for (t in 1:5) {
		t = proc.time()
		f(...)
		t = (proc.time()-t)["elapsed"]
		if (t < tmin) tmin = t
	}
	cat(sprintf("r,%s,%.8f\n", name, tmin*1000))
}

fib = function(n) {
	if (n < 2) {
		return(n)
	} else {
		return(fib(n-1) + fib(n-2))
	}
}

assert(fib(20) == 6765)
timeit("fib", fib, 20)

parseintperf = function(t) {
	for (i in 1:t) {
		// R doesn't support uint32 values
		n = floor(2^31-1*runif(1))
		s = intToHex(n)
		m = as.numeric(paste("0x",s,sep=""))
		assert(m == n)
	}
}

timeit("parse_int", parseintperf, 1000)

mandel = function(z) {
    c = z
	maxiter = 80
    for (n in 1:maxiter) {
        if (Mod(z) > 2) return(n-1)
        z = z^2+c
	}
	return(maxiter)
}

mandelperf = function() {
	re = seq(-2,0.5,.1)
	im = seq(-1,1,.1)
	M = matrix(0.0,nrow=length(re),ncol=length(im))
	count = 1
	for (r in re) {
		for (i in im) {
			M[count] = mandel(complex(real=r,imag=i))
			count = count + 1
		}
	}
	return(M)
}

assert(sum(mandelperf()) == 14660)
timeit("mandel", mandelperf)
