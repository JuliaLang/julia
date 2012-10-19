#ifndef BUILTIN_PROTO_H
#define BUILTIN_PROTO_H

// declarations for julia-callable builtin functions

JL_CALLABLE(jl_f_new_expr);
JL_CALLABLE(jl_f_new_box);
JL_CALLABLE(jl_f_throw);
JL_CALLABLE(jl_f_is);
JL_CALLABLE(jl_f_no_function);
JL_CALLABLE(jl_f_typeof);
JL_CALLABLE(jl_f_subtype);
JL_CALLABLE(jl_f_isa);
JL_CALLABLE(jl_f_typeassert);
JL_CALLABLE(jl_f_apply);
JL_CALLABLE(jl_f_top_eval);
JL_CALLABLE(jl_f_isdefined);
JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_tupleref);
JL_CALLABLE(jl_f_tuplelen);
JL_CALLABLE(jl_f_get_field);
JL_CALLABLE(jl_f_set_field);
JL_CALLABLE(jl_f_field_type);
JL_CALLABLE(jl_f_arraylen);
JL_CALLABLE(jl_f_arrayref);
JL_CALLABLE(jl_f_arrayset);
JL_CALLABLE(jl_f_arraysize);
JL_CALLABLE(jl_f_instantiate_type);
JL_CALLABLE(jl_f_convert_default);
JL_CALLABLE(jl_f_convert_tuple);
JL_CALLABLE(jl_trampoline);
JL_CALLABLE(jl_f_new_type_constructor);
JL_CALLABLE(jl_f_typevar);
JL_CALLABLE(jl_f_union);
JL_CALLABLE(jl_f_methodexists);
JL_CALLABLE(jl_f_applicable);
JL_CALLABLE(jl_f_invoke);
JL_CALLABLE(jl_apply_generic);
JL_CALLABLE(jl_unprotect_stack);
JL_CALLABLE(jl_f_task);
JL_CALLABLE(jl_f_yieldto);
JL_CALLABLE(jl_f_ctor_trampoline);

#endif
