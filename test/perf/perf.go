package main

import (
	"fmt"
	matrix "github.com/skelterjohn/go.matrix"
	"math"
	"math/cmplx"
	"math/rand"
	"strconv"
	"time"
)

// fibonacci

func fib(n int) int {
	if n < 2 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

// quicksort

func qsort_kernel(a []float64, lo, hi int) []float64 {
	i := lo
	j := hi
	for i < hi {
		pivot := a[(lo+hi)/2]
		for i <= j {
			for a[i] < pivot {
				i += 1
			}
			for a[j] > pivot {
				j -= 1
			}
			if i <= j {
				a[i], a[j] = a[j], a[i]
				i += 1
				j -= 1
			}
		}
		if lo < j {
			qsort_kernel(a, lo, j)
		}
		lo = i
		j = hi
	}
	return a
}

// randmatstat

// Not implemented

// randmatmul

func randmatmul(n int) matrix.MatrixRO {
	a := matrix.Zeros(n, n)
	b := matrix.Zeros(n, n)
	for i := 0; i < n; i++ {
		for k := 0; k < n; k++ {
			a.Set(i, k, rand.Float64())
			b.Set(i, k, rand.Float64())
		}
	}
	return matrix.Product(a, b)
}

// mandelbrot 

func mandel(z complex128) int {
	c := z
	var n int
	for n = 0; n < 79; n++ {
		if cmplx.Abs(z) > 2 {
			n -= 1
			break
		}
		z = z*z + c
	}
	return n + 1
}

func mandelperf() int {
	mandel_sum := 0
	for re := -2.0; re <= 0.5; re += 0.1 {
		for im := -1.0; im <= 1.0; im += 0.1 {
			m := mandel(complex(re, im))
			mandel_sum += m
		}
	}
	return mandel_sum
}

// pisum

func pisum() float64 {
	var sum float64
	for i := 0; i < 500; i++ {
		sum = 0.0
		for k := 1; k <= 10000; k++ {
			sum += 1.0 / float64(k*k)
		}
	}
	return sum
}

func print_perf(name string, time float64) {
	fmt.Printf("go,%v,%v\n", name, time*1000)
}

// run tests

func assert(b bool) {
	if b != true {
		panic("assert failed")
	}
}

func main() {
	assert(fib(20) == 6765)
	tmin := float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		t := time.Now()
		_ = fib(20)
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("fib", tmin)

	tmin = float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		t := time.Now()
		for k := 0; k < 1000; k++ {
			n := rand.Uint32()
			s := fmt.Sprintf("%x", n)
			m, _ := strconv.ParseUint(s, 16, 32)
			assert(uint32(m) == n)
		}
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("parse_int", tmin)

	// fmt.Println(mandelperf())
	// FIXME: assert(mandelperf() == 14791)
	tmin = float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		t := time.Now()
		_ = mandelperf()
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("mandel", tmin)

	tmin = float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		lst := make([]float64, 5000)
		for k := 0; k < len(lst); k++ {
			lst[k] = rand.Float64()
		}
		t := time.Now()
		qsort_kernel(lst, 0, len(lst)-1)
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("quicksort", tmin)

	assert(math.Abs(pisum()-1.644834071848065) < 1e-6)
	tmin = float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		t := time.Now()
		pisum()
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("pi_sum", tmin)

	// randmatstat not implemented

	tmin = float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		t := time.Now()
		c := randmatmul(1000)
		assert(c.Get(0, 0) >= 0)
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("rand_mat_mul", tmin)
}
