#!/home/rajeev/bin/sage/sage

import os
import sys
import time

intel  = False
cilk   = False
octave = False

N = 150
Niter = 2**10
dx = 0.1
dy = 0.1
dx2 = dx*dx
dy2 = dy*dy

par_str = '#define size_mat %d\n' %N
if open('parameter.h').read() != par_str:
    f = open('parameter.h', 'w')
    f.write(par_str)
    f.close()

os.system('make')
if intel:
    os.system('make intel')

import for_laplace
if intel:
    import intel_for_laplace

from _laplace import cy_update, cy_c_update, cy_pointer, cy_update_parallel, cy_vectorized
from scipy import weave
#import numexpr as ne
import numpy as np


def num_update(u, dx2, dy2):
    u[1:-1,1:-1] = ((u[2:,1:-1]+u[:-2,1:-1])*dy2 + 
                    (u[1:-1,2:] + u[1:-1,:-2])*dx2) * (1./ (2*(dx2+dy2)))

def expr_update(u, dx2, dy2):
    bottom = u[2:,1:-1]
    top = u[:-2,1:-1]
    left = u[1:-1,2:]
    right = u[1:-1,:-2]
    u[1:-1,1:-1] = ne.evaluate("((bottom + top)*dy2 + "\
                    "(left + right)*dx2) / (2*(dx2+dy2))")
    
def weave_update(u, dx2, dy2):
    code = """
    int i, j;
    for (i=1; i<Nu[0]-1; i++) {
       for (j=1; j<Nu[1]-1; j++) {
           U2(i,j) = ((U2(i+1, j) + U2(i-1, j))*dy2 + \
                       (U2(i, j+1) + U2(i, j-1))*dx2) * (1.0 / (2*(dx2+dy2)));
       }
    }
    """
    weave.inline(code, ['u', 'dx2', 'dy2'])


#def calc(func=num_update, args=()):
def calc_for(N, Niter=100, func=num_update, args=()):
    if intel:
        order = 'C' if func not in [for_laplace.for_update1, for_laplace.for_update2, intel_for_laplace.for_update1, intel_for_laplace.for_update2] else 'F'
    else:
        order = 'C' if func not in [for_laplace.for_update1, for_laplace.for_update2] else 'F'

    u = np.zeros([N, N], order=order)
    u[0] = 1
    for i in xrange(Niter):
        func(u,*args)
    return u

def calc(N, Niter=100, func=num_update, args=()):
    u = np.zeros([N, N])
    u[0] = 1
    func(u,*args)
    return u



def main():
    i = -1 # variable tracking methods

#             ['Numexpr', expr_update, (dx2, dy2)],
#             ['Cython with parallel update', cy_update_parallel, (dx2, dy2, Niter)],
#             ['Weave', weave_update, (dx2, dy2)],
    modes_for = [['Vectorized Fortran (f2py)', for_laplace.for_update2, (dx2, dy2, N, N)],
                 ['NumPy', num_update, (dx2, dy2)],
                 ['Cython', cy_update, (dx2, dy2)],
                 ['Cython with C', cy_c_update, (dx2, dy2)],
                 ['Cython with Pointers', cy_pointer, (dx2, dy2)],
                 ['Looped Fortran (f2py)', for_laplace.for_update1, (dx2, dy2, N, N)],
                ]

    modes = [('Cython with parallel update', cy_update_parallel, (dx2, dy2, Niter)),
             ('Cython vectorized', cy_vectorized, (dx2, dy2, Niter)),
            ]

    modes_ext = [('laplace_for_update2.out', 'data_Vectorized Fortran (pure)', 'Vectorized Fortran (pure)'),
                 ('c_laplace_parallel_update.out', 'data_C (pure, parallel update)', 'Vectorized C (pure)'),
                 ('c_laplace_parallel_update_pointer.out', 'data_C (pure, parallel update)', 'Vectorized C (pure, dyanamic array)')
                ]
    
    modes_ext_looped = [('laplace_for_update1.out', 'data_Looped Fortran (pure)', 'Looped Fortran (pure)'),
                        ('c_laplace.out', 'data_C (pure)', 'C (pure)'),
                       ]
    if intel:
        modes_for.insert(1, ['Vectorized Fortran (f2py, intel)', intel_for_laplace.for_update2, (dx2, dy2, N, N)])
        modes_for.append(['Looped Fortran (f2py, intel)', intel_for_laplace.for_update1, (dx2, dy2, N, N)])

        if cilk: 
            modes_ext.insert(0, ('cilk_laplace.out', 'data_Cilk__pure', 'Cilk Plus'))

        modes_ext.extend( [('intel_laplace_for_update2.out', 'data_Vectorized Fortran (pure)', 'Vectorized Fortran (pure, intel)'),
                           ('intel_c_laplace_parallel_update.out', 'data_C (pure, parallel update)', 'Vectorized C (pure, intel)'),
                           ('intel_c_laplace_parallel_update_pointer.out', 'data_C (pure, parallel update)', 'Vectorized C (pure, dyanamic array, intel)'),
                          ] )

        modes_ext_looped.extend( [ ('intel_laplace_for_update1.out', 'data_Looped Fortran (pure)', 'Looped Fortran (pure, intel)'),
                                   ('intel_c_laplace.out', 'data_C (pure)', 'C (pure, intel)'),
                                 ] )

    # a 3-d array to save data
    data = np.empty((len(modes)+len(modes_for)+len(modes_ext)+len(modes_ext_looped)+1, N, N))
    # octave
    if octave:
        start = time.time()
        os.system('octave --silent laplace.m')
        elapsed = time.time() - start
        i += 1
        data[i,:,:] = np.loadtxt('data_octave')
        print '% 50s (%d): % 8.4f seconds' %('Octave', i, elapsed)

    for command, fname, mode in modes_ext:
        start = time.time()
        os.system('echo %d | ./%s' %(Niter, command))
        elapsed = time.time() - start
        i += 1
        data[i,:,:] = np.transpose( np.loadtxt(fname).reshape((N,N)) )
        print "% 3d : % 8.4f seconds  %s " % (i, elapsed, mode, )
#        print "% 50s (% 3d ): % 8.4f seconds" % (mode, i, elapsed)

    for mode, update, args in modes:
        start = time.time()
        i += 1
        data[i,:,:] = calc(N, Niter, func=update, args=args)
        elapsed = time.time() - start
        print "% 3d : % 8.4f seconds  %s " % (i, elapsed, mode, )

    for mode, update, args in modes_for:
        start = time.time()
        i += 1
        data[i,:,:] = calc_for(N, Niter, func=update, args=args)
        elapsed = time.time() - start
        print "% 3d : % 8.4f seconds  %s " % (i, elapsed, mode, )

    for command, fname, mode in modes_ext_looped:
        start = time.time()
        os.system('echo %d | ./%s' %(Niter, command))
        elapsed = time.time() - start
        i += 1
        data[i,:,:] = np.transpose( np.loadtxt(fname).reshape((N,N)) )
        print "% 3d : % 8.4f seconds  %s " % (i, elapsed, mode, )























#    # cilk
#    if intel:
#        start = time.time()
#        os.system('echo %d | ./cilk_laplace.out' %Niter)
#        elapsed = time.time() - start
#        i += 1
#        data[i,:,:] = np.transpose( np.loadtxt('data_Cilk__pure').reshape((N,N)) )
#        print '% 50s (% 3d ): % 8.4f seconds' %('Cilk Plus', i, elapsed)
#
#    # vectorized fortran (pure)
#    start = time.time()
#    os.system('echo %d | ./laplace_for_update2.out' %Niter)
#    elapsed = time.time() - start
#    i += 1
#    data[i,:,:] = np.transpose( np.loadtxt('data_Vectorized Fortran (pure)').reshape((N,N)) )
#    print '% 50s (% 3d ): % 8.4f seconds' %('Vectorized Fortran (pure)', i, elapsed)
#
#    # vectorized fortran (pure intel)
#    if intel:
#        start = time.time()
#        os.system('echo %d | ./intel_laplace_for_update2.out' %Niter)
#        elapsed = time.time() - start
#        i += 1
#        data[i,:,:] = np.transpose( np.loadtxt('data_Vectorized Fortran (pure)').reshape((N,N)) )
#        print '% 50s (% 3d ): % 8.4f seconds' %('Vectorized Fortran (pure, intel)', i, elapsed)
#
#    # c version parallel update
#    start = time.time()
#    os.system('echo %d | ./c_laplace_parallel_update.out' %Niter)
#    elapsed = time.time() - start
#    i += 1
#    data[i,:,:] = np.transpose( np.loadtxt('data_C (pure, parallel update)').reshape((N,N)) )
#    print '% 50s (% 3d ): % 8.4f seconds' %('Vectorized C (pure)', i, elapsed)
#
#    # c version parallel update (intel)
#    if intel:
#        start = time.time()
#        os.system('echo %d | ./intel_c_laplace_parallel_update.out' %Niter)
#        elapsed = time.time() - start
#        i += 1
#        data[i,:,:] = np.transpose( np.loadtxt('data_C (pure, parallel update)').reshape((N,N)) )
#        print '% 50s (% 3d ): % 8.4f seconds' %('Vectorized C (pure, intel)', i, elapsed)
#
#    # c version parallel update
#    start = time.time()
#    os.system('echo %d | ./c_laplace_parallel_update_pointer.out' %Niter)
#    elapsed = time.time() - start
#    i += 1
#    data[i,:,:] = np.transpose( np.loadtxt('data_C (pure, parallel update)').reshape((N,N)) )
#    print '% 50s (% 3d ): % 8.4f seconds' %('Vectorized C (pure, dyanamic array)', i, elapsed)
#
#    # c version parallel update (intel)
#    if intel:
#        start = time.time()
#        os.system('echo %d | ./intel_c_laplace_parallel_update_pointer.out'
#                %Niter)
#        elapsed = time.time() - start
#        i += 1
#        data[i,:,:] = np.transpose( np.loadtxt('data_C (pure, parallel update)').reshape((N,N)) )
#        print '% 50s (% 3d ): % 8.4f seconds' %('Vectorized C (pure, dyanamic array, intel)', i, elapsed)
#
#    # Cython with parallel update
#    u = np.zeros([N, N])
#    u[0] = 1
#    start = time.time()
#    cy_update_parallel(u, dx2, dy2, Niter)
#    elapsed = time.time() - start
#    i += 1
#    data[i,:,:] = u
#    print '% 50s (% 3d ): % 8.4f seconds' %('Cython with parallel update', i, elapsed)
#
#    # Cython vectorized
#    u = np.zeros([N, N])
#    u[0] = 1
#    start = time.time()
#    cy_vectorized(u, dx2, dy2, Niter)
#    elapsed = time.time() - start
#    i += 1
#    data[i,:,:,] = u
##    data[i,:,:] = np.transpose( np.loadtxt('data_cython_vectorized').reshape((N,N)) )
#    print '% 50s (% 3d ): % 8.4f seconds' %('Cython vectorized', i, elapsed)
#
#    #pure fortran versions
#    start = time.time()
#    os.system('echo %d | ./laplace_for_update1.out' %Niter)
#    elapsed = time.time() - start
#    i += 1
#    data[i,:,:] = np.transpose( np.loadtxt('data_Looped Fortran (pure)').reshape((N,N)) )
#    print '% 50s (% 3d ): % 8.4f seconds' %('Looped Fortran (pure)', i, elapsed)
#
#    #pure fortran versions (intel)
#    if intel:
#        start = time.time()
#        os.system('echo %d | ./intel_laplace_for_update1.out' %Niter)
#        elapsed = time.time() - start
#        i += 1
#        data[i,:,:] = np.transpose( np.loadtxt('data_Looped Fortran (pure)').reshape((N,N)) )
#        print '% 50s (% 3d ): % 8.4f seconds' %('Looped Fortran (pure, intel)', i, elapsed)
#
#    start = time.time()
#    os.system('echo %d | ./c_laplace.out' %Niter)
#    elapsed = time.time() - start
#    i += 1
#    data[i,:,:] = np.transpose( np.loadtxt('data_C (pure)').reshape((N,N)))
#    print '% 50s (% 3d ): % 8.4f seconds' %('C (pure)', i, elapsed)
#
#    if intel:
#        start = time.time()
#        os.system('echo %d | ./intel_c_laplace.out' %Niter)
#        elapsed = time.time() - start
#        i += 1
#        data[i,:,:] = np.transpose( np.loadtxt('data_C (pure)').reshape((N,N)))
#        print '% 50s (% 3d ): % 8.4f seconds' %('C (pure, intel)', i, elapsed)

    #pypy laplace2.py
    no_tests = i+1

    print '\n  ' + ''.join(['% 8d' %i for i in range(no_tests-1)]),
    for i in range(no_tests):
        print "\n% 3d:" % i,
        for j in range(i):
            print "% 6.4f" % (np.max( np.abs( data[i,:,:]- data[j,:,:] ) )),
    print


if __name__ == '__main__':
    main()
    
