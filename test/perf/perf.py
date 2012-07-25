from numpy import *
from numpy.linalg import *
import sys
import time
import random
import numpy

if sys.version_info < (3,):
    range = xrange

## fibonacci ##

def fib(n):
    if n<2:
        return n
    return fib(n-1)+fib(n-2)

## quicksort ##

def qsort_kernel(a, lo, hi):
    i = lo
    j = hi
    while i < hi:
        pivot = a[(lo+hi) // 2]
        while i <= j:
            while a[i] < pivot:
                i += 1
            while a[j] > pivot:
                j -= 1
            if i <= j:
                a[i], a[j] = a[j], a[i]
                i += 1
                j -= 1
        if lo < j:
            qsort_kernel(a, lo, j)
        lo = i
        j = hi
    return a

## randmatstat ##

def randmatstat(t):
    n = 5
    v = zeros(t)
    w = zeros(t)
    for i in range(1,t):
        a = numpy.random.randn(n, n)
        b = numpy.random.randn(n, n)
        c = numpy.random.randn(n, n)
        d = numpy.random.randn(n, n)
        P = matrix(hstack((a, b, c, d)))
        Q = matrix(vstack((hstack((a, b)), hstack((c, d)))))
        v[i] = trace(matrix_power(transpose(P)*P, 4))
        w[i] = trace(matrix_power(transpose(Q)*Q, 4))
    return (std(v)/mean(v), std(w)/mean(w))

## randmatmul ##

def randmatmul(n):
    A = numpy.random.rand(n,n)
    B = numpy.random.rand(n,n)
    return numpy.dot(A,B)

## mandelbrot ##

def mandel(z):
    c = z
    for n in range(0,79):
        if abs(z) > 2:
            n -= 1
            break
        z = z*z + c
    return n + 1

def mandelperf():
    r1 = numpy.arange(-2.0, 0.5, 0.1)
    r2 = numpy.arange(-1.0, 1.0, 0.1)
    return [mandel(complex(r, i)) for r in r1 for i in r2]

def pisum():
    sum = 0.0
    for j in range(1, 500):
        sum = 0.0
        for k in range(1, 10000):
            sum += 1.0/(k*k)
    return sum

def print_perf(name, time):
    print("python," + name + "," + str(time*1000))

## run tests ##

if __name__=="__main__":
    assert fib(20) == 6765
    tmin = float('inf')
    for i in range(5):
        t = time.time()
        f = fib(20)
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf("fib", tmin)

    tmin = float('inf')
    for i in range(5):
        t = time.time()
        for i in range(1,1000):
            n = random.randint(0,2**32-1)
            s = hex(n)
            if s[-1]=='L':
                s = s[0:-1]
            m = int(s,16)
            assert m == n
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("parse_int", tmin)

    assert sum(mandelperf()) == 14304
    tmin = float('inf')
    for i in range(5):
        t = time.time()
        mandelperf()
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("mandel", tmin)

    tmin = float('inf')
    for i in range(5):
        lst = [ random.random() for i in range(1,5000) ]
        t = time.time()
        qsort_kernel(lst, 0, len(lst)-1)
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("quicksort", tmin)

    pi = pisum()
    assert abs(pisum()-1.644834071848065) < 1e-6
    tmin = float('inf')
    for i in range(5):
        t = time.time()
        pisum()
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("pi_sum", tmin)

    (s1, s2) = randmatstat(1000)
    assert s1 > 0.5 and s1 < 1.0
    tmin = float('inf')
    for i in range(5):
        t = time.time()
        randmatstat(1000)
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("rand_mat_stat", tmin)

    tmin = float('inf')
    for i in range(5):
        t = time.time()
        C = randmatmul(1000)
        assert C[0,0] >= 0
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("rand_mat_mul", tmin)
