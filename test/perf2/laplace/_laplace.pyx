
include "interrupt.pxi"  # ctrl-c interrupt block support
include "stdsage.pxi"  # ctrl-c interrupt block support

include "cdefs.pxi"

include "interrupt.pxi"  # ctrl-c interrupt block support
include "stdsage.pxi"  # ctrl-c interrupt block support

include "cdefs.pxi"

include "interrupt.pxi"  # ctrl-c interrupt block support
include "stdsage.pxi"  # ctrl-c interrupt block support

include "cdefs.pxi"

include "interrupt.pxi"  # ctrl-c interrupt block support
include "stdsage.pxi"  # ctrl-c interrupt block support

include "cdefs.pxi"
#cython: boundscheck=False
#cython: wraparound=False
cimport numpy as np
from libc.stdlib cimport malloc, free

# defining the constant
cdef extern from "parameter.h":
    enum: size_mat

# Define the external C functions
cdef extern from "__laplace.c":
    cdef void c_update(double **u, unsigned long rows, unsigned long cols, double dx2, double dy2)

# vectorized cython with fixed array size
def cy_vectorized(np.ndarray[double, ndim=2] uu, double dx2, double dy2, unsigned long Niter):
    cdef double u[size_mat][size_mat], u1[size_mat][size_mat]
    cdef int i, j, k
#    f = open("data_cython_vectorized", "w")

    for i in xrange(size_mat):
        for j in xrange(size_mat):
            u[i][j] = uu[i,j]

    for i in xrange(Niter):
        for j in xrange(1,size_mat-1):
            for k in xrange(1,size_mat-1):
                u1[j][k] = ((u[j+1][k] + u[j-1][k]) * dy2 + 
                   (u[j][k+1] + u[j][k-1]) * dx2) * (1.0 / (2*(dx2+dy2)))

        for j in xrange(1,size_mat-1):
            for k in xrange(1,size_mat-1):
                u[j][k] = u1[j][k]

    for i in xrange(size_mat):
        for j in xrange(size_mat):
            uu[i,j] = u[i][j]
#    for i in xrange(size_mat):
#        for j in xrange(size_mat):
#            f.write("%f " %u[i][j])
#
#    f.close()


def cy_c_update(np.ndarray[double, ndim=2] u, double dx2, double dy2):
    # Get the size of the matrix.
    cdef unsigned int rows = <unsigned int>u.shape[0]
    cdef unsigned int cols = <unsigned int>u.shape[1]
    cdef double **data
    data = <double **>malloc(rows*sizeof(double *))

    # Go through the rows and pull out each one as an array of floats which can
    # be stored in our C-level 'matrix'.
    cdef unsigned int i
    for i in xrange(rows):
        data[i] = &u[i, 0]

    # Call the C function to calculate the result.
    c_update(data, rows, cols, dx2, dy2)

    # Free the memory we allocated for the C-level 'matrix', and we are done.
    free(data)

def cy_update(np.ndarray[double, ndim=2] u, double dx2, double dy2):
    # original cython code
    cdef unsigned int i, j
    for i in xrange(1,u.shape[0]-1):
        for j in xrange(1, u.shape[1]-1):
            u[i,j] = ((u[i+1, j] + u[i-1, j]) * dy2 +
                      (u[i, j+1] + u[i, j-1]) * dx2) * (1.0 / (2*(dx2+dy2)))


def cy_update_parallel(np.ndarray[double, ndim=2] u, double dx2, double dy2, unsigned int Niter):
    cdef unsigned int rows = <unsigned int>u.shape[0]
    cdef unsigned int cols = <unsigned int>u.shape[1]
    cdef double **u1, **data
    cdef unsigned int i, j, k
    cdef double multiplying_factor = (1.0 / (2*(dx2+dy2)))

    u1 = <double **>malloc(rows*sizeof(double *))
    for i in xrange(rows):
        u1[i] = <double *>malloc(cols*sizeof(double))

    data = <double **>malloc(rows*sizeof(double *))

    # Go through the rows and pull out each one as an array of floats which can
    # be stored in our C-level 'matrix'.
    for i in xrange(rows):
        data[i] = &(<double *>u.data)[i * cols]

    for k in xrange(Niter):
        for i in xrange(1,u.shape[0]-1):
            for j in xrange(1, u.shape[1]-1):
                u1[i][j] = ((data[i+1][j] + data[i-1][j]) * dy2 +
                          (data[i][j+1] + data[i][j-1]) * dx2) * multiplying_factor

        for i in xrange(1,u.shape[0]-1):
            for j in xrange(1, u.shape[1]-1):
                data[i][j] = u1[i][j]

    free(data)
    for i in xrange(rows):
        free(u1[i])
    free(u1)


def cy_pointer(np.ndarray[double, ndim=2] u, double dx2, double dy2):
#    if chr(u.descr.type) <> "d":
#        raise TypeError("Double array required")
#    if u.nd <> 2:
#        raise ValueError("2 dimensional array required")
    cdef double dnr_inv
    cdef double *elem

    cdef unsigned int nx = <unsigned int>u.shape[0]
    cdef unsigned int ny = <unsigned int>u.shape[1]
    dnr_inv = 0.5/(dx2 + dy2)
    elem = (<double *>u.data)

    cdef int i, j
    cdef double *uc, *uu, *ud, *ul, *ur
    for i from 1 <= i < nx-1:
        uc = elem + i*ny + 1
        ur = elem + i*ny + 2
        ul = elem + i*ny
        uu = elem + (i+1)*ny + 1
        ud = elem + (i-1)*ny + 1

        for j from 1 <= j < ny-1:
            uc[0] = ((ul[0] + ur[0])*dy2 + (uu[0] + ud[0])*dx2)*dnr_inv
            uc = uc + 1; ur = ur + 1;  ul = ul + 1
            uu = uu + 1; ud = ud + 1
