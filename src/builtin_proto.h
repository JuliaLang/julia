// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_BUILTIN_PROTO_H
#define JL_BUILTIN_PROTO_H

#ifdef __cplusplus
extern "C" {
#endif

// declarations for julia-callable builtin functions
#define JL_BUILTIN_FUNCTIONS(XX) \
    XX(_abstracttype,"_abstracttype") \
    XX(_apply_iterate,"_apply_iterate") \
    XX(_call_in_world_total,"_call_in_world_total") \
    XX(_compute_sparams,"_compute_sparams") \
    XX(_defaultctors,"_defaultctors") \
    XX(_equiv_typedef,"_equiv_typedef") \
    XX(_expr,"_expr") \
    XX(_import, "_import") \
    XX(_primitivetype,"_primitivetype") \
    XX(_setsuper,"_setsuper!") \
    XX(_structtype,"_structtype") \
    XX(_svec_ref,"_svec_ref") \
    XX(_typebody,"_typebody!") \
    XX(_typevar,"_typevar") \
    XX(_using, "_using") \
    XX(applicable,"applicable") \
    XX(apply_type,"apply_type") \
    XX(compilerbarrier,"compilerbarrier") \
    XX(current_scope,"current_scope") \
    XX(donotdelete,"donotdelete") \
    XX(fieldtype,"fieldtype") \
    XX(finalizer,"finalizer") \
    XX(get_binding_type,"get_binding_type") \
    XX(getfield,"getfield") \
    XX(getglobal,"getglobal") \
    XX(ifelse,"ifelse") \
    XX(intrinsic_call,"intrinsic_call") \
    XX(invoke,"invoke") \
    XX(invoke_in_world,"invoke_in_world") \
    XX(invokelatest,"invokelatest") \
    XX(is,"===") \
    XX(isa,"isa") \
    XX(isdefined,"isdefined") \
    XX(isdefinedglobal,"isdefinedglobal") \
    XX(issubtype,"<:") \
    XX(memorynew,"memorynew") \
    XX(memoryrefnew,"memoryrefnew") \
    XX(memoryref_isassigned,"memoryref_isassigned") \
    XX(memoryrefget,"memoryrefget") \
    XX(memoryrefmodify,"memoryrefmodify!") \
    XX(memoryrefoffset,"memoryrefoffset") \
    XX(memoryrefreplace,"memoryrefreplace!") \
    XX(memoryrefset,"memoryrefset!") \
    XX(memoryrefsetonce,"memoryrefsetonce!") \
    XX(memoryrefswap,"memoryrefswap!") \
    XX(modifyfield,"modifyfield!") \
    XX(modifyglobal,"modifyglobal!") \
    XX(nfields,"nfields") \
    XX(opaque_closure_call,"opaque_closure_call") \
    XX(replacefield,"replacefield!") \
    XX(replaceglobal,"replaceglobal!") \
    XX(setfield,"setfield!") \
    XX(setfieldonce,"setfieldonce!") \
    XX(setglobal,"setglobal!") \
    XX(setglobalonce,"setglobalonce!") \
    XX(sizeof,"sizeof") \
    XX(svec,"svec") \
    XX(swapfield,"swapfield!") \
    XX(swapglobal,"swapglobal!") \
    XX(throw,"throw") \
    XX(throw_methoderror,"throw_methoderror") \
    XX(tuple,"tuple") \
    XX(typeassert,"typeassert") \
    XX(typeof,"typeof") \

#define DECLARE_BUILTIN(cname,jlname) \
    JL_CALLABLE(jl_f_##cname);
JL_BUILTIN_FUNCTIONS(DECLARE_BUILTIN)
#undef DECLARE_BUILTIN

#define BUILTIN(cname) (jl_builtin_instances[jl_builtin_id_##cname])

enum jl_builtin_ids {
#define BUILTIN_IDS(cname,jlname) jl_builtin_id_##cname,
JL_BUILTIN_FUNCTIONS(BUILTIN_IDS)
#undef BUILTIN_IDS
    jl_n_builtins
};

JL_DLLEXPORT extern jl_fptr_args_t const jl_builtin_f_addrs[];
JL_DLLEXPORT extern const char *const jl_builtin_f_names[];
JL_DLLEXPORT extern jl_value_t *jl_builtin_instances[];

#ifdef __cplusplus
}
#endif

#endif
