from numpy import *
import sys
import time
import random

def fib(n):
    if n<2:
        return n
    return fib(n-1)+fib(n-2)

def timeparseint():
    t1=time.time()
    for i in xrange(1,1000):
        int('1111000011110000111100001111',2)
    print "parse_int: ", time.time()-t1


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

if __name__=="__main__":                       # If this script is run as a program:
    t1=time.time()
    f = fib(20)
    print "fib: ", time.time()-t1
    
    timeparseint()
    
    lst = [ random.random() for i in xrange(1,5000) ]
    t1=time.time()
    quicksort(lst, 0, len(lst)-1)
    print "sort: ", time.time()-t1
