// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_BUILTIN_PROTO_H
#define JL_BUILTIN_PROTO_H

#ifdef __cplusplus
extern "C" {
#endif

// declarations for julia-callable builtin functions

#ifdef DEFINE_BUILTIN_GLOBALS
#define DECLARE_BUILTIN(name) \
    JL_CALLABLE(jl_f_##name); \
    JL_DLLEXPORT jl_value_t *jl_builtin_##name; \
    JL_DLLEXPORT jl_fptr_args_t jl_f_##name##_addr = &jl_f_##name
#else
#define DECLARE_BUILTIN(name) \
    JL_CALLABLE(jl_f_##name); \
    JL_DLLEXPORT extern jl_value_t *jl_builtin_##name; \
    JL_DLLEXPORT extern jl_fptr_args_t jl_f_##name##_addr
#endif

DECLARE_BUILTIN(applicable);
DECLARE_BUILTIN(_apply_iterate);
DECLARE_BUILTIN(_apply_pure);
DECLARE_BUILTIN(apply_type);
DECLARE_BUILTIN(arrayref);
DECLARE_BUILTIN(arrayset);
DECLARE_BUILTIN(arraysize);
DECLARE_BUILTIN(_call_in_world);
DECLARE_BUILTIN(_call_in_world_total);
DECLARE_BUILTIN(_call_latest);
DECLARE_BUILTIN(replacefield);
DECLARE_BUILTIN(const_arrayref);
DECLARE_BUILTIN(_expr);
DECLARE_BUILTIN(fieldtype);
DECLARE_BUILTIN(getfield);
DECLARE_BUILTIN(ifelse);
DECLARE_BUILTIN(invoke);
DECLARE_BUILTIN(is);
DECLARE_BUILTIN(isa);
DECLARE_BUILTIN(isdefined);
DECLARE_BUILTIN(issubtype);
DECLARE_BUILTIN(modifyfield);
DECLARE_BUILTIN(nfields);
DECLARE_BUILTIN(setfield);
DECLARE_BUILTIN(sizeof);
DECLARE_BUILTIN(svec);
DECLARE_BUILTIN(swapfield);
DECLARE_BUILTIN(throw);
DECLARE_BUILTIN(tuple);
DECLARE_BUILTIN(typeassert);
DECLARE_BUILTIN(_typebody);
DECLARE_BUILTIN(typeof);
DECLARE_BUILTIN(_typevar);
DECLARE_BUILTIN(donotdelete);
DECLARE_BUILTIN(getglobal);
DECLARE_BUILTIN(setglobal);

JL_CALLABLE(jl_f_invoke_kwsorter);
#ifdef DEFINE_BUILTIN_GLOBALS
JL_DLLEXPORT jl_fptr_args_t jl_f_invoke_kwsorter_addr = &jl_f_invoke_kwsorter;
#else
JL_DLLEXPORT extern jl_fptr_args_t jl_f_invoke_kwsorter_addr;
#endif
JL_CALLABLE(jl_f__structtype);
JL_CALLABLE(jl_f__abstracttype);
JL_CALLABLE(jl_f__primitivetype);
JL_CALLABLE(jl_f__setsuper);
JL_CALLABLE(jl_f__equiv_typedef);
JL_CALLABLE(jl_f_get_binding_type);
JL_CALLABLE(jl_f_set_binding_type);
JL_CALLABLE(jl_f_donotdelete);
JL_CALLABLE(jl_f_setglobal);

#ifdef __cplusplus
}
#endif

#endif
