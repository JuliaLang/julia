#include <string.h>
#include <cholmod.h>

extern void
jl_cholmod_common(void **cm)
{
    cholmod_common *c = (cholmod_common *) malloc (sizeof(cholmod_common));
    *cm = c;
}

extern void
jl_cholmod_dense( void **cd,        /* Store return value in here */
                  size_t nrow,      /* the matrix is nrow-by-ncol */
                  size_t ncol,
                  size_t nzmax,     /* maximum number of entries in the matrix */
                  size_t d,         /* leading dimension (d >= nrow must hold) */
                  void *x,          /* size nzmax or 2*nzmax, if present */
                  void *z,          /* size nzmax, if present */
                  int xtype,        /* pattern, real, complex, or zomplex */
                  int dtype         /* x and z double or float */
                  )
{
    cholmod_dense *mat = (cholmod_dense *) malloc (sizeof(cholmod_dense));
    mat->nrow = nrow;
    mat->ncol = ncol;
    mat->nzmax = nzmax;
    mat->d = d;
    mat->x = x;
    mat->z = z;
    mat->xtype = xtype;
    mat->dtype = dtype;

    *cd = mat;
    return;
}

extern void
jl_cholmod_dense_copy_out(cholmod_dense *cd,
                          void *p
                          )
{
    int elsize = sizeof(double);
    if (cd->dtype == CHOLMOD_DOUBLE) {
        if (cd->xtype == CHOLMOD_REAL) { elsize = sizeof(double); }
        else if (cd->xtype == CHOLMOD_COMPLEX) { elsize = 2*sizeof(double); }
    } else if (cd->xtype == CHOLMOD_SINGLE) {
                if (cd->xtype == CHOLMOD_REAL) { elsize = sizeof(float); }
        else if (cd->xtype == CHOLMOD_COMPLEX) { elsize = 2*sizeof(float); }
    }

    memcpy(p, cd->x, cd->nzmax*elsize);
}

extern void
jl_cholmod_sparse( void **cs,    /* Store return value in here */
                   size_t nrow,  /* # of rows of A */
                   size_t ncol,  /* # of columns of A */
                   size_t nzmax, /* max # of nonzeros of A */ 
                   void *p,      /* p [0..ncol], the column pointers */
                   void *i,      /* i [0..nzmax-1], the row indices */
                   void *nz,     /* nz [0..ncol-1], the # of nonzeros in each col if unpacked */
                   void *x,      /* size nzmax or 2*nzmax, if present */
                   void *z,      /* size nzmax, if present */
                   int stype,    /*  0: matrix is unsymmetric and possibly rectangular
                                    >0: matrix is square and upper triangular
                                    <0: matrix is square and lower triangular
                                 */
                   int itype,    /* CHOLMOD_INT:     p, i, and nz are int.
                                  * CHOLMOD_INTLONG: p is UF_long, i and nz are int.
                                  * CHOLMOD_LONG:    p, i, and nz are UF_long.  */
                   int xtype,    /* pattern, real, complex, or zomplex */
                   int dtype,    /* x and z are double or float */
                   int sorted,   /* TRUE if columns are sorted, FALSE otherwise */
                   int packed    /* TRUE if packed (nz ignored), FALSE if unpacked
                                  * (nz is required) */
)
{
    cholmod_sparse *s = (cholmod_sparse *) malloc (sizeof(cholmod_sparse));
    s->nrow = nrow;
    s->ncol = ncol;
    s->nzmax = nzmax;
    s->p = p;
    s->i = i;
    s->nz = nz;
    s->x = x;
    s->z = z;
    s->stype = stype;
    s->itype = itype;
    s->xtype = xtype;
    s->dtype = dtype;
    s->sorted = sorted;
    s->packed = packed;

    *cs = s;
    return;
}
