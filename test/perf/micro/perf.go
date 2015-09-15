// Implementation of the Julia benchmark suite in Go.
//
// Three gonum packages must be installed, and then an additional environmental
// variable must be set to use the BLAS installation.
// To install the gonum packages, run:
// 		go get github.com/gonum/blas
//		go get github.com/gonum/matrix/mat64
//		go get github.com/gonum/stat
// The cgo ldflags must then be set to use the BLAS implementation. As an example,
// download OpenBLAS to ~/software
//		git clone https://github.com/xianyi/OpenBLAS
// 		cd OpenBLAS
//		make
// Then edit the enivorment variable to have
// 		export CGO_LDFLAGS="-L/$HOME/software/OpenBLAS -lopenblas"
package main

import (
	"fmt"
	"math"
	"math/cmplx"
	"math/rand"
	"strconv"
	"time"

	"github.com/gonum/blas/blas64"
	"github.com/gonum/blas/cgo"
	"github.com/gonum/matrix/mat64"
	"github.com/gonum/stat"
)

func init() {
	// Use the BLAS implementation specified in CGO_LDFLAGS. This line can be
	// commented out to use the native Go BLAS implementation found in
	// github.com/gonum/blas/native.
	blas64.Use(cgo.Implementation{})

	// These are here so that toggling the BLAS implementation does not make imports unused
	_ = cgo.Implementation{}
	_ = blas64.General{}
}

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

func randmatstat(t int) (float64, float64) {
	n := 5
	v := make([]float64, t)
	w := make([]float64, t)
	ad := make([]float64, n*n)
	bd := make([]float64, n*n)
	cd := make([]float64, n*n)
	dd := make([]float64, n*n)
	P := mat64.NewDense(n, 4*n, nil)
	Q := mat64.NewDense(2*n, 2*n, nil)
	pTmp := mat64.NewDense(4*n, 4*n, nil)
	qTmp := mat64.NewDense(2*n, 2*n, nil)
	for i := 0; i < t; i++ {
		for i := range ad {
			ad[i] = rand.NormFloat64()
			bd[i] = rand.NormFloat64()
			cd[i] = rand.NormFloat64()
			dd[i] = rand.NormFloat64()
		}
		a := mat64.NewDense(n, n, ad)
		b := mat64.NewDense(n, n, bd)
		c := mat64.NewDense(n, n, cd)
		d := mat64.NewDense(n, n, dd)
		P.Copy(a)
		P.View(0, n, n, n).(*mat64.Dense).Copy(b)
		P.View(0, 2*n, n, n).(*mat64.Dense).Copy(c)
		P.View(0, 3*n, n, n).(*mat64.Dense).Copy(d)

		Q.Copy(a)
		Q.View(0, n, n, n).(*mat64.Dense).Copy(b)
		Q.View(n, 0, n, n).(*mat64.Dense).Copy(c)
		Q.View(n, n, n, n).(*mat64.Dense).Copy(d)

		pTmp.Mul(P.T(), P)
		pTmp.Pow(pTmp, 4)

		qTmp.Mul(Q.T(), Q)
		qTmp.Pow(qTmp, 4)

		v[i] = mat64.Trace(pTmp)
		w[i] = mat64.Trace(qTmp)
	}
	mv, stdv := stat.MeanStdDev(v, nil)
	mw, stdw := stat.MeanStdDev(v, nil)
	return stdv / mv, stdw / mw
}

// randmatmul

func randmatmul(n int) *mat64.Dense {
	aData := make([]float64, n*n)
	for i := range aData {
		aData[i] = rand.Float64()
	}
	a := mat64.NewDense(n, n, aData)

	bData := make([]float64, n*n)
	for i := range bData {
		bData[i] = rand.Float64()
	}
	b := mat64.NewDense(n, n, bData)
	var c mat64.Dense
	c.Mul(a, b)
	return &c
}

// mandelbrot

func mandel(z complex128) int {
	maxiter := 80
	c := z
	for n := 0; n < maxiter; n++ {
		if cmplx.Abs(z) > 2 {
			return n
		}
		z = z*z + c
	}
	return maxiter
}

// mandelperf

func mandelperf() int {
	mandel_sum := 0
	// These loops are constructed as such because mandel is very sensitive to
	// its input and this avoids very small floating point issues.
	for re := -20.0; re <= 5; re += 1 {
		for im := -10.0; im <= 10; im += 1 {
			m := mandel(complex(re/10, im/10))
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
		for k := 1.0; k <= 10000; k += 1 {
			sum += 1.0 / (k * k)
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
		var m uint64
		var n uint32
		for k := 0; k < 1000; k++ {
			n = rand.Uint32()
			s := fmt.Sprintf("%x", n)
			m, _ = strconv.ParseUint(s, 16, 32)
		}
		assert(uint32(m) == n)
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("parse_int", tmin)

	assert(mandelperf() == 14791)
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

	tmin = float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		t := time.Now()
		c1, c2 := randmatstat(1000)
		assert(0.5 < c1)
		assert(c1 < 1.0)
		assert(0.5 < c2)
		assert(c2 < 1.0)
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("rand_mat_stat", tmin)

	tmin = float64(math.MaxFloat64)
	for i := 0; i < 5; i++ {
		t := time.Now()
		c := randmatmul(1000)
		assert(c.At(0, 0) >= 0)
		d := float64(time.Since(t).Seconds())
		if d < tmin {
			tmin = d
		}
	}
	print_perf("rand_mat_mul", tmin)
}
