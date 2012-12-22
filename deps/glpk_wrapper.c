#include <stdio.h>
#include <stdlib.h>
#include <glpk.h>

#define generate_accessors(str, field, type) \
    extern void _jl_glpkw__ ## str ## _set_ ## field(void * v, type val) \
    { \
        glp_ ## str * s = (glp_ ## str *) v; \
        s->field = val; \
    } \
    \
    extern type _jl_glpkw__ ## str ## _get_ ## field(void * v) \
    { \
        glp_ ## str * s = (glp_ ## str *) v; \
        return s->field; \
    }

extern void* _jl_glpkw__smcp_init()
{
    glp_smcp * smcp = calloc(1, sizeof(glp_smcp));
    glp_init_smcp(smcp);
    return (void*) smcp;
}

extern void _jl_glpkw__smcp_delete(void* smcp)
{
    free((glp_smcp*) smcp);
}

generate_accessors(smcp, msg_lev, int);
generate_accessors(smcp, meth, int);
generate_accessors(smcp, pricing, int);
generate_accessors(smcp, r_test, int);
generate_accessors(smcp, tol_bnd, double);
generate_accessors(smcp, tol_dj, double);
generate_accessors(smcp, tol_piv, double);
generate_accessors(smcp, obj_ll, double);
generate_accessors(smcp, obj_ul, double);
generate_accessors(smcp, it_lim, int);
generate_accessors(smcp, tm_lim, int);
generate_accessors(smcp, out_frq, int);
generate_accessors(smcp, out_dly, int);
generate_accessors(smcp, presolve, int);



extern void* _jl_glpkw__iptcp_init()
{
    glp_iptcp * iptcp = calloc(1, sizeof(glp_iptcp));
    glp_init_iptcp(iptcp);
    return (void*) iptcp;
}

extern void _jl_glpkw__iptcp_delete(void* iptcp)
{
    free((glp_iptcp*) iptcp);
}

generate_accessors(iptcp, msg_lev, int);
generate_accessors(iptcp, ord_alg, int);



extern void* _jl_glpkw__iocp_init()
{
    glp_iocp * iocp = calloc(1, sizeof(glp_iocp));
    glp_init_iocp(iocp);
    return (void*) iocp;
}

extern void _jl_glpkw__iocp_delete(void* iocp)
{
    free((glp_iocp*) iocp);
}

generate_accessors(iocp, msg_lev, int);
generate_accessors(iocp, br_tech, int);
generate_accessors(iocp, bt_tech, int);
generate_accessors(iocp, pp_tech, int);
generate_accessors(iocp, fp_heur, int);
generate_accessors(iocp, gmi_cuts, int);
generate_accessors(iocp, mir_cuts, int);
generate_accessors(iocp, cov_cuts, int);
generate_accessors(iocp, clq_cuts, int);
generate_accessors(iocp, tol_int, double);
generate_accessors(iocp, tol_obj, double);
generate_accessors(iocp, mip_gap, double);
generate_accessors(iocp, tm_lim, int);
generate_accessors(iocp, out_frq, int);
generate_accessors(iocp, out_dly, int);
generate_accessors(iocp, cb_func, void*);
generate_accessors(iocp, cb_info, void*);
generate_accessors(iocp, cb_size, int);
generate_accessors(iocp, presolve, int);
generate_accessors(iocp, binarize, int);



extern void* _jl_glpkw__bfcp_init()
{
    glp_bfcp * bfcp = calloc(1, sizeof(glp_bfcp));
    return (void*) bfcp;
}

extern void _jl_glpkw__bfcp_delete(void* bfcp)
{
    free((glp_bfcp*) bfcp);
}

generate_accessors(bfcp, type, int);
generate_accessors(bfcp, lu_size, int);
generate_accessors(bfcp, piv_tol, double);
generate_accessors(bfcp, piv_lim, int);
generate_accessors(bfcp, suhl, int);
generate_accessors(bfcp, eps_tol, double);
generate_accessors(bfcp, max_gro, double);
generate_accessors(bfcp, nfs_max, int);
generate_accessors(bfcp, upd_tol, double);
generate_accessors(bfcp, nrs_max, int);
generate_accessors(bfcp, rs_size, int);
