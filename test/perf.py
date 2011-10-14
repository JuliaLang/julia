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


## run tests ##

if __name__=="__main__":
    t = time.time()
    f = fib(20)
    print "fib: ", time.time()-t

    t = time.time()
    for i in xrange(1,1000):
        int("1111000011110000111100001111",2)
    print "parse_int: ", time.time()-t

    lst = [ random.random() for i in xrange(1,5000) ]
    t = time.time()
    quicksort(lst, 0, len(lst)-1)
    print "sort: ", time.time()-t

    assert sum(mandelperf()) == 14304
    t = time.time()
    mandelperf()
    print "mandelbrot: ", time.time()-t
