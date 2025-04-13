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

DECLARE_BUILTIN(_apply_iterate);
DECLARE_BUILTIN(invoke_in_world);
DECLARE_BUILTIN(_call_in_world_total);
DECLARE_BUILTIN(invokelatest);
DECLARE_BUILTIN(_compute_sparams);
DECLARE_BUILTIN(_expr);
DECLARE_BUILTIN(_svec_ref);
DECLARE_BUILTIN(_typebody);
DECLARE_BUILTIN(_typevar);
DECLARE_BUILTIN(applicable);
DECLARE_BUILTIN(apply_type);
DECLARE_BUILTIN(compilerbarrier);
DECLARE_BUILTIN(current_scope);
DECLARE_BUILTIN(donotdelete);
DECLARE_BUILTIN(fieldtype);
DECLARE_BUILTIN(finalizer);
DECLARE_BUILTIN(getfield);
DECLARE_BUILTIN(getglobal);
DECLARE_BUILTIN(ifelse);
DECLARE_BUILTIN(invoke);
DECLARE_BUILTIN(is);
DECLARE_BUILTIN(isa);
DECLARE_BUILTIN(isdefined);
DECLARE_BUILTIN(isdefinedglobal);
DECLARE_BUILTIN(issubtype);
DECLARE_BUILTIN(memorynew);
DECLARE_BUILTIN(memoryref);
DECLARE_BUILTIN(memoryref_isassigned);
DECLARE_BUILTIN(memoryrefget);
DECLARE_BUILTIN(memoryrefmodify);
DECLARE_BUILTIN(memoryrefoffset);
DECLARE_BUILTIN(memoryrefreplace);
DECLARE_BUILTIN(memoryrefset);
DECLARE_BUILTIN(memoryrefsetonce);
DECLARE_BUILTIN(memoryrefswap);
DECLARE_BUILTIN(modifyfield);
DECLARE_BUILTIN(modifyglobal);
DECLARE_BUILTIN(nfields);
DECLARE_BUILTIN(replacefield);
DECLARE_BUILTIN(replaceglobal);
DECLARE_BUILTIN(setfield);
DECLARE_BUILTIN(setfieldonce);
DECLARE_BUILTIN(setglobal);
DECLARE_BUILTIN(setglobalonce);
DECLARE_BUILTIN(sizeof);
DECLARE_BUILTIN(svec);
DECLARE_BUILTIN(swapfield);
DECLARE_BUILTIN(swapglobal);
DECLARE_BUILTIN(throw);
DECLARE_BUILTIN(throw_methoderror);
DECLARE_BUILTIN(tuple);
DECLARE_BUILTIN(typeassert);
DECLARE_BUILTIN(typeof);

JL_CALLABLE(jl_f__structtype);
JL_CALLABLE(jl_f__abstracttype);
JL_CALLABLE(jl_f__primitivetype);
JL_CALLABLE(jl_f__setsuper);
JL_CALLABLE(jl_f__defaultctors);
JL_CALLABLE(jl_f__equiv_typedef);
JL_CALLABLE(jl_f_get_binding_type);
JL_CALLABLE(jl_f__compute_sparams);
JL_CALLABLE(jl_f__svec_ref);
#ifdef __cplusplus
}
#endif

#endif
