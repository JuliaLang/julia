from numpy import *
import sys
import time
import random
import numpy


## fibonacci ##

def fib(n):
    if n<2:
        return n
    return fib(n-1)+fib(n-2)


## quicksort ##

def partition(list, start, end):
    pivot = list[end]                          # Partition around the last value
    bottom = start-1                           # Start outside the area to be partitioned
    top = end                                  # Ditto

    done = 0
    while not done:                            # Until all elements are partitioned...

        while not done:                        # Until we find an out of place element...
            bottom = bottom+1                  # ... move the bottom up.

            if bottom == top:                  # If we hit the top...
                done = 1                       # ... we are done.
                break

            if list[bottom] > pivot:           # Is the bottom out of place?
                list[top] = list[bottom]       # Then put it at the top...
                break                          # ... and start searching from the top.

        while not done:                        # Until we find an out of place element...
            top = top-1                        # ... move the top down.

            if top == bottom:                  # If we hit the bottom...
                done = 1                       # ... we are done.
                break

            if list[top] < pivot:              # Is the top out of place?
                list[bottom] = list[top]       # Then put it at the bottom...
                break                          # ...and start searching from the bottom.

    list[top] = pivot                          # Put the pivot in its place.
    return top                                 # Return the split point


def quicksort(list, start, end):
    if start < end:                            # If there are two or more elements...
        split = partition(list, start, end)    # ... partition the sublist...
        quicksort(list, start, split-1)        # ... and sort both halves.
        quicksort(list, split+1, end)
    else:
        return

## randmatstat ##

def randmatstat(t):
    n = 5
    v = zeros(t)
    w = zeros(t)
    for i in xrange(1,t):
        a = numpy.random.randn(n, n)
        b = numpy.random.randn(n, n)
        c = numpy.random.randn(n, n)
        d = numpy.random.randn(n, n)
        P = matrix(hstack((a, b, c, d)))
        Q = matrix(vstack((hstack((a, b)), hstack((c, d)))))
        v[i] = trace(pow(transpose(P)*P, 4))
        w[i] = trace(pow(transpose(Q)*Q, 4))
    return (std(v)/mean(v), std(w)/mean(w))

## randmatmul ##

def randmatmul(n):
    A = matrix(numpy.random.rand(n,n))
    B = matrix(numpy.random.rand(n,n))
    return A*B

## mandelbrot ##

def mandel(z):
    n = 0
    c = z
    for n in xrange(0,79):
        if abs(z) > 2:
            n -= 1
            break
        z = z**2 + c
    return n + 1

def mandelperf():
    r1 = numpy.arange(-2.0, 0.5, 0.1)
    r2 = numpy.arange(-1.0, 1.0, 0.1)
    M = numpy.zeros((len(r1)*len(r2)))
    count = 0
    for r in r1:
        for i in r2:
            M[count] = mandel(complex(r,i))
            count += 1
    return M

def pisum():
    sum = 0.0
    for j in xrange(1, 500):
        sum = 0.0
        for k in xrange(1, 10000):
            sum += 1.0/(k*k)
    return sum

def print_perf(name, time):
    print "python," + name + "," + str(time*1000)

## run tests ##

if __name__=="__main__":
    assert fib(20) == 6765
    tmin = float('inf')
    for i in xrange(5):
        t = time.time()
        f = fib(20)
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf("fib", tmin)

    tmin = float('inf')
    for i in xrange(5):
        t = time.time()
        for i in xrange(1,1000):
            n = random.randint(0,2**32-1)
            s = hex(n)
            m = int(s,16)
            assert m == n
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("parse_int", tmin)

    assert sum(mandelperf()) == 14304
    tmin = float('inf')
    for i in xrange(5):
        t = time.time()
        mandelperf()
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("mandel", tmin)

    tmin = float('inf')
    for i in xrange(5):
        lst = [ random.random() for i in xrange(1,5000) ]
        t = time.time()
        quicksort(lst, 0, len(lst)-1)
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("quicksort", tmin)

    pi = pisum()
    assert abs(pisum()-1.644834071848065) < 1e-6
    tmin = float('inf')
    for i in xrange(5):
        t = time.time()
        pisum()
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("pi_sum", tmin)

    (s1, s2) = randmatstat(1000)
    assert s1 > 0.5 and s1 < 1.0
    tmin = float('inf')
    for i in xrange(5):
        t = time.time()
        randmatstat(1000)
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("rand_mat_stat", tmin)

    tmin = float('inf')
    for i in xrange(5):
        t = time.time()
        C = randmatmul(1000)
        assert C[0,0] >= 0
        t = time.time()-t
        if t < tmin: tmin = t
    print_perf ("rand_mat_mul", tmin)
