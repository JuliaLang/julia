// This file is a part of Julia. License is MIT: http://julialang.org/license

#define INTRINSICS \
    /*  wrap and unwrap */ \
    ALIAS(box, reinterpret) \
    ALIAS(unbox, reinterpret) \
    /*  arithmetic */ \
    ADD_I(neg_int, 1) \
    ADD_I(add_int, 2) \
    ADD_I(sub_int, 2) \
    ADD_I(mul_int, 2) \
    ADD_I(sdiv_int, 2) \
    ADD_I(udiv_int, 2) \
    ADD_I(srem_int, 2) \
    ADD_I(urem_int, 2) \
    ADD_I(neg_float, 1) \
    ADD_I(add_float, 2) \
    ADD_I(sub_float, 2) \
    ADD_I(mul_float, 2) \
    ADD_I(div_float, 2) \
    ADD_I(rem_float, 2) \
    ADD_I(fma_float, 3) \
    ADD_I(muladd_float, 3) \
    /*  fast arithmetic */ \
    ALIAS(neg_float_fast, neg_float) \
    ALIAS(add_float_fast, add_float) \
    ALIAS(sub_float_fast, sub_float) \
    ALIAS(mul_float_fast, mul_float) \
    ALIAS(div_float_fast, div_float) \
    ALIAS(rem_float_fast, rem_float) \
    /*  same-type comparisons */ \
    ADD_I(eq_int, 2) \
    ADD_I(ne_int, 2) \
    ADD_I(slt_int, 2) \
    ADD_I(ult_int, 2) \
    ADD_I(sle_int, 2) \
    ADD_I(ule_int, 2) \
    ADD_I(eq_float, 2) \
    ADD_I(ne_float, 2) \
    ADD_I(lt_float, 2) \
    ADD_I(le_float, 2) \
    ALIAS(eq_float_fast, eq_float) \
    ALIAS(ne_float_fast, ne_float) \
    ALIAS(lt_float_fast, lt_float) \
    ALIAS(le_float_fast, le_float) \
    ADD_I(fpiseq, 2) \
    ADD_I(fpislt, 2) \
    /*  bitwise operators */ \
    ADD_I(and_int, 2) \
    ADD_I(or_int, 2) \
    ADD_I(xor_int, 2) \
    ADD_I(not_int, 1) \
    ADD_I(shl_int, 2) \
    ADD_I(lshr_int, 2) \
    ADD_I(ashr_int, 2) \
    ADD_I(bswap_int, 1) \
    ADD_I(ctpop_int, 1) \
    ADD_I(ctlz_int, 1) \
    ADD_I(cttz_int, 1) \
    /*  conversion */ \
    ADD_I(sext_int, 2) \
    ADD_I(zext_int, 2) \
    ADD_I(trunc_int, 2) \
    ADD_I(fptoui, 2) \
    ADD_I(fptosi, 2) \
    ADD_I(uitofp, 2) \
    ADD_I(sitofp, 2) \
    ADD_I(fptrunc, 2) \
    ADD_I(fpext, 2) \
    /*  checked conversion */ \
    ADD_I(checked_trunc_sint, 2) \
    ADD_I(checked_trunc_uint, 2) \
    ADD_I(check_top_bit, 1) \
    /*  checked arithmetic */ \
    ADD_I(checked_sadd_int, 2) \
    ADD_I(checked_uadd_int, 2) \
    ADD_I(checked_ssub_int, 2) \
    ADD_I(checked_usub_int, 2) \
    ADD_I(checked_smul_int, 2) \
    ADD_I(checked_umul_int, 2) \
    ADD_I(checked_sdiv_int, 2) \
    ADD_I(checked_udiv_int, 2) \
    ADD_I(checked_srem_int, 2) \
    ADD_I(checked_urem_int, 2) \
    /*  functions */ \
    ADD_I(abs_float, 1) \
    ADD_I(copysign_float, 2) \
    ADD_I(flipsign_int, 2) \
    ADD_I(select_value, 3) \
    ADD_I(ceil_llvm, 1) \
    ADD_I(floor_llvm, 1) \
    ADD_I(trunc_llvm, 1) \
    ADD_I(rint_llvm, 1) \
    ADD_I(sqrt_llvm, 1) \
    ADD_I(powi_llvm, 2) \
    ALIAS(sqrt_llvm_fast, sqrt_llvm) \
    /*  pointer access */ \
    ADD_I(pointerref, 3) \
    ADD_I(pointerset, 4) \
    /* c interface */ \
    ALIAS(ccall, ccall) \
    ADD_I(cglobal, 2) \
    ALIAS(llvmcall, llvmcall) \
    /* object access */ \
    ADD_I(arraylen, 1) \
    /*  hidden intrinsics */ \
    ADD_HIDDEN(fptoui_auto, 1) \
    ADD_HIDDEN(fptosi_auto, 1) \
    ADD_HIDDEN(cglobal_auto, 1)

enum intrinsic {
#define ADD_I(func, nargs) func,
#define ADD_HIDDEN ADD_I
#define ALIAS ADD_I
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
    num_intrinsics,
    reinterpret = box
};

#ifdef __cplusplus
extern "C"
#endif
JL_DLLEXPORT const char *jl_intrinsic_name(int f)
{
    switch ((enum intrinsic)f) {
    default: return "invalid";
#define ADD_I(func, nargs) case func: return #func;
#define ADD_HIDDEN ADD_I
#define ALIAS ADD_I
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
    }
}

static void (*runtime_fp[num_intrinsics])(void);
static unsigned intrinsic_nargs[num_intrinsics];

typedef jl_value_t *(*intrinsic_call_1_arg)(jl_value_t*);
typedef jl_value_t *(*intrinsic_call_2_arg)(jl_value_t*, jl_value_t*);
typedef jl_value_t *(*intrinsic_call_3_arg)(jl_value_t*, jl_value_t*, jl_value_t*);
#define jl_is_intrinsic(v)       jl_typeis(v,jl_intrinsic_type)

#ifdef __cplusplus
extern "C"
#endif
JL_CALLABLE(jl_f_intrinsic_call)
{
    JL_NARGSV(intrinsic_call, 1);
    JL_TYPECHK(intrinsic_call, intrinsic, F);
    enum intrinsic f = (enum intrinsic)*(uint32_t*)jl_data_ptr(F);
    if (f == fptoui && nargs == 1)
        f = fptoui_auto;
    if (f == fptosi && nargs == 1)
        f = fptosi_auto;
    if (f == cglobal && nargs == 1)
        f = cglobal_auto;
    unsigned fargs = intrinsic_nargs[f];
    if (!fargs)
        jl_error("this intrinsic must be compiled to be called");
    JL_NARGS(intrinsic_call, fargs, fargs);
    switch (fargs) {
        case 1:
            return ((intrinsic_call_1_arg)runtime_fp[f])(args[0]);
        case 2:
            return ((intrinsic_call_2_arg)runtime_fp[f])(args[0], args[1]);
        case 3:
            return ((intrinsic_call_3_arg)runtime_fp[f])(args[0], args[1], args[2]);
        default:
            assert(0 && "unexpected number of arguments to an intrinsic function");
    }
    gc_debug_critical_error();
    abort();
}

static void add_intrinsic_properties(enum intrinsic f, unsigned nargs, void (*pfunc)(void))
{
    intrinsic_nargs[f] = nargs;
    runtime_fp[f] = pfunc;
}

static void add_intrinsic(jl_module_t *inm, const char *name, enum intrinsic f)
{
    jl_value_t *i = jl_box32(jl_intrinsic_type, (int32_t)f);
    jl_sym_t *sym = jl_symbol(name);
    jl_set_const(inm, sym, i);
    jl_module_export(inm, sym);
}

#ifdef __cplusplus
extern "C"
#endif
void jl_init_intrinsic_properties(void)
{
#define ADD_I(name, nargs) add_intrinsic_properties(name, nargs, (void(*)(void))&jl_##name);
#define ADD_HIDDEN ADD_I
#define ALIAS(alias, base) add_intrinsic_properties(alias, intrinsic_nargs[base], runtime_fp[base]);
    ADD_HIDDEN(reinterpret, 2);
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS

}

#ifdef __cplusplus
extern "C"
#endif
void jl_init_intrinsic_functions(void)
{
    jl_module_t *inm = jl_new_module(jl_symbol("Intrinsics"));
    inm->parent = jl_core_module;
    jl_set_const(jl_core_module, jl_symbol("Intrinsics"), (jl_value_t*)inm);
    jl_mk_builtin_func(jl_intrinsic_type, "IntrinsicFunction", jl_f_intrinsic_call);

#define ADD_I(name, nargs) add_intrinsic(inm, #name, name);
#define ADD_HIDDEN(name, nargs)
#define ALIAS ADD_I
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
}
