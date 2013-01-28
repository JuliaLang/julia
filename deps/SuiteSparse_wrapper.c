#include <string.h>
#include <cholmod.h>

extern size_t jl_cholmod_common_size(size_t x) {
    return sizeof(cholmod_common);
}

extern void jl_cholmod_common_offsets(size_t *vv) {
    vv[0] = offsetof(cholmod_common, dbound);
    vv[1] = offsetof(cholmod_common, maxrank);
    vv[2] = offsetof(cholmod_common, supernodal_switch);
    vv[3] = offsetof(cholmod_common, supernodal);
    vv[4] = offsetof(cholmod_common, final_asis);
    vv[5] = offsetof(cholmod_common, final_super);
    vv[6] = offsetof(cholmod_common, final_ll);
    vv[7] = offsetof(cholmod_common, final_pack);
    vv[8] = offsetof(cholmod_common, final_monotonic);
    vv[9] = offsetof(cholmod_common, final_resymbol);
    vv[10] = offsetof(cholmod_common, prefer_zomplex);
    vv[11] = offsetof(cholmod_common, prefer_upper);
    vv[12] = offsetof(cholmod_common, print);
    vv[13] = offsetof(cholmod_common, precise);
    vv[14] = offsetof(cholmod_common, nmethods);
    vv[15] = offsetof(cholmod_common, selected);
    vv[16] = offsetof(cholmod_common, postorder);
    vv[17] = offsetof(cholmod_common, itype);
    vv[18] = offsetof(cholmod_common, dtype);
}

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
}

extern void
jl_cholmod_dense_copy_out(cholmod_dense *cd,
                          void *p
                          )
{
    size_t elsize = (cd->xtype == CHOLMOD_COMPLEX ? 2 : 1) *
        (cd->dtype == CHOLMOD_DOUBLE ? sizeof(double) : sizeof(float));

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

extern int
jl_cholmod_sparse_copy_out(cholmod_sparse *cs,
                           void *cp,  /* column pointers */
                           void *ri,  /* row indices */
                           void *nzp,
                           cholmod_common *cm) /* non-zero values */
{
                                /* error return if cs is not packed */
    if (!cs->packed) return 1;  /* FIXME: If non-packed becomes a problem, write code to do packing */
    if (!cs->sorted)            /* sort it */
        if (!cholmod_sort(cs, cm)) return 2;

    size_t isize;
    switch(cs->itype) {
    case CHOLMOD_INT:
    case CHOLMOD_INTLONG:
        isize = sizeof(int); break;
    case CHOLMOD_LONG:
        isize =  sizeof(SuiteSparse_long); break;
    default:
        return 3;
    }
    size_t elsize = (cs->xtype == CHOLMOD_COMPLEX ? 2 : 1) *
        (cs->dtype == CHOLMOD_DOUBLE ? sizeof(double) : sizeof(float));

    if (cs->itype == CHOLMOD_INTLONG) {
        int i, *dpt = (int *) cp;
        SuiteSparse_long *spt = (SuiteSparse_long *) cs->p;
        for (i = 0; i <= cs->ncol; ++i) dpt[i] = spt[i];
    } else {
        memcpy(cp, cs->p, (cs->ncol + 1) * isize);
    }

    memcpy(ri, cs->i, cs->nzmax * isize);
    memcpy(nzp, cs->x, cs->nzmax * elsize);
    return 0;
}
