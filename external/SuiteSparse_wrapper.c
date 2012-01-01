#include <cholmod.h>

extern cholmod_sparse *
jl_cholmod_sparse( size_t nrow, /* # of rows of A */
                   size_t ncol, /* # of columns of A */
                   void *p,     /* p [0..ncol], the column pointers */
                   void *i,     /* i [0..nzmax-1], the row indices */
                   void *nz,    /* nz [0..ncol-1], the # of nonzeros in each col if unpacked */
                   void *x,     /* size nzmax or 2*nzmax, if present */
                   void *z,     /* size nzmax, if present */
                   int itype,   /* CHOLMOD_INT:     p, i, and nz are int.
                                 * CHOLMOD_INTLONG: p is UF_long, i and nz are int.
                                 * CHOLMOD_LONG:    p, i, and nz are UF_long.  */
                   int xtype,   /* pattern, real, complex, or zomplex */
                   int dtype,   /* x and z are double or float */
                   int sorted,  /* TRUE if columns are sorted, FALSE otherwise */
                   int packed   /* TRUE if packed (nz ignored), FALSE if unpacked
                                 * (nz is required) */
)
{
    cholmod_sparse *s = (cholmod_sparse *) malloc (sizeof(cholmod_sparse));
    s->nrow = nrow;
    s->ncol = ncol;
    s->p = p;
    s->i = i;
    s->nz = nz;
    s->x = x;
    s->z = z;
    s->itype = itype;
    s->xtype = xtype;
    s->dtype = dtype;
    s->sorted = sorted;
    s->packed = packed;

    return s;
}

extern void
jl_cholmod_sparse_free(cholmod_sparse *s) {
    free(s);
}
