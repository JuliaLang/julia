// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef BUILTIN_PROTO_H
#define BUILTIN_PROTO_H

#ifdef __cplusplus
extern "C" {
#endif

// declarations for julia-callable builtin functions

#ifdef DEFINE_BUILTIN_GLOBALS
#define DECLARE_BUILTIN(name) \
    JL_CALLABLE(jl_f_##name); \
    jl_value_t *jl_builtin_##name
#else
#define DECLARE_BUILTIN(name) \
    JL_CALLABLE(jl_f_##name); \
    extern jl_value_t *jl_builtin_##name
#endif

DECLARE_BUILTIN(throw);      DECLARE_BUILTIN(is);
DECLARE_BUILTIN(typeof);     DECLARE_BUILTIN(sizeof);
DECLARE_BUILTIN(issubtype);  DECLARE_BUILTIN(isa);
DECLARE_BUILTIN(_apply);     DECLARE_BUILTIN(_apply_pure);
DECLARE_BUILTIN(isdefined);  DECLARE_BUILTIN(nfields);
DECLARE_BUILTIN(tuple);      DECLARE_BUILTIN(svec);
DECLARE_BUILTIN(getfield);   DECLARE_BUILTIN(setfield);
DECLARE_BUILTIN(fieldtype);  DECLARE_BUILTIN(arrayref);
DECLARE_BUILTIN(arrayset);   DECLARE_BUILTIN(arraysize);
DECLARE_BUILTIN(apply_type); DECLARE_BUILTIN(applicable);
DECLARE_BUILTIN(invoke);     DECLARE_BUILTIN(_expr);
DECLARE_BUILTIN(typeassert);

JL_CALLABLE(jl_f_invoke_kwsorter);

#ifdef __cplusplus
}
#endif

#endif
