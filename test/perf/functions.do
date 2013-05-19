clear mata
mata:

// Timer function
void timeit(name, pointer(real scalar function) scalar func,arg) {
	lang = "Mata (Stata)"
    nexpt = 5
    times = J(nexpt, 1, 0)

	timer_clear()
    for (i=1;i<=nexpt;i++) {
		timer_on(i)
			a = (*func)(arg)
		timer_off(i)
		t = timer_value(i)
		times[i] = t[1]
    }

    times = sort(times,1)
    printf ("%s,%s,%4.0f\n", lang, name, 1000*times[1])
}
mata mosave timeit(), replace

// Recursive Fibonacci
real scalar fib(n) {
	if (n < 2) {
			return(n)
	} else {
			return(fib(n-1) + fib(n-2))
	}
}
mata mosave fib(), replace

// Mandelbrot
scalar mandel(z) {
	n = 0
    c = z
    while (abs(z)<=2 & n<=79) {
        z = z^2+c
		n++
    }
	return(n)
}
mata mosave mandel(), replace

matrix mandelperf(ignore) {
	a = range(-2,.5,.1)
	b = range(-1,1,.1)
	M = J(rows(a),rows(b),0)
	count = 1
	for (r=1;r<=rows(a);r++) {
		for (i=1;i<=rows(b);i++) {
		  M[r,i] = mandel(a[r] + b[i]*1i);
		  count = count + 1;
		}
	}
	return(M)
}
mata mosave mandelperf(), replace

// Numeric vector quicksort

matrix qsort(a) {
	return(qsort_kernel(a, 1, length(a)))
}
mata mosave qsort(), replace

matrix qsort_kernel(a, lo, hi) {
	i = lo
	j = hi
	while (i < hi) {
		pivot = a[floor((lo+hi)/2)]
		while (i <= j) {
			while (a[i] < pivot) i++
			while (a[j] > pivot) j--
			if (i <= j) {
				t = a[i]
				a[i] = a[j]
				a[j] = t
				i = i + 1
				j = j - 1
			}
		}
		if (lo < j) a=qsort_kernel(a, lo, j)
		lo = i
		j = hi
	}
	return(a)
}
mata mosave qsort_kernel(), replace

matrix sortperf(n) {
	v = runiform(n,1)
    v = qsort(v)
}
mata mosave sortperf(), replace

// Slow pi series
scalar pisum(ignore) {
	sum = 0
	for (j=1;j<=500;j++) {
		sum = 0
		for (k=1;k<=10000;k++) {
			sum = sum + 1/(k*k);
		}
	}
	return(sum)
}
mata mosave pisum(), replace

// Slow pi series, vectorized
scalar pisumvec(ignore) {
	a = range(1,10000,1)
    for (j=1;j<=500;j++) {
        s = sum(1:/(a:^2))
    }
	return(s)
}
mata mosave pisumvec(), replace

// Random matrix statistics
matrix randmatstat(t) {
	n=5;
    v = w = J(t,1,0)
    for (i=1;i<=t;i++) {
        a = runiform(n, n)
        b = runiform(n, n)
        c = runiform(n, n)
        d = runiform(n, n)
        P = a,b,c,d
        Q = (a,b)\(c,d)
		PP = P'P
		QQ = Q'Q
        v[i] = trace(PP*PP*PP*PP)
		w[i] = trace(QQ*QQ*QQ*QQ)
    }
    return((sqrt(variance(v))/mean(v),sqrt(variance(w))/mean(w)))
}
mata mosave randmatstat(), replace

// Largish random number gen & matmul
matrix randmatmul(n) {
	return(runiform(n,n)*runiform(n,n))
}
mata mosave randmatmul(), replace

// printf
// On windows /dev/null should become NUL but doesn't work
void printfd(n) {	
	filename = "test.txt"
	u = _unlink(filename)
	f = fopen(filename, "w")
	for (i=1;i<=n;i++) {
		fput(f,sprintf("%d %d\n", i, i))
	}
    fclose(f)
}
mata mosave printfd(), replace

end

exit


// Parse Int
real scalar parseintperf(t) {
	for (i=1;i<=t;i++) {
		n = int(runiform(1,1)*(2^32))
        s = dec2hex(n);
        m = hex2dec(s);
    }
	return(n)
}
