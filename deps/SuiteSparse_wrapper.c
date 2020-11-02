/*
  SuiteSparse_wrapper.c: Changes made to this file in the Julia repo
  in deps/SuiteSparse_wrapper.c should be also made in
  Yggdrasil/S/SuiteSparse and vice versa.
*/

#include <string.h>
#include <cholmod.h>

extern size_t jl_cholmod_common_size(void) {
    return sizeof(cholmod_common);
}

extern size_t jl_cholmod_sizeof_long(void) {
    return sizeof(SuiteSparse_long);
}

extern int jl_cholmod_version(int *ver) {
    if (ver != (int*) NULL) {
        ver[0] = CHOLMOD_MAIN_VERSION;
        ver[1] = CHOLMOD_SUB_VERSION;
        ver[2] = CHOLMOD_SUBSUB_VERSION;
    }
    return CHOLMOD_VERSION;
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
