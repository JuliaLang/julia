#ifndef _BUILTIN_PROTO_H_
#define _BUILTIN_PROTO_H_

// declarations for julia-callable builtin functions

JL_CALLABLE(jl_new_struct_internal);
JL_CALLABLE(jl_generic_ctor);
JL_CALLABLE(jl_constructor_factory_trampoline);
JL_CALLABLE(jl_weakref_ctor);
JL_CALLABLE(jl_new_array_internal);
JL_CALLABLE(jl_f_throw);
JL_CALLABLE(jl_f_is);
JL_CALLABLE(jl_f_no_function);
JL_CALLABLE(jl_f_typeof);
JL_CALLABLE(jl_f_subtype);
JL_CALLABLE(jl_f_isa);
JL_CALLABLE(jl_f_typeassert);
JL_CALLABLE(jl_f_apply);
JL_CALLABLE(jl_f_top_eval);
JL_CALLABLE(jl_f_isbound);
JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_tupleref);
JL_CALLABLE(jl_f_tuplelen);
JL_CALLABLE(jl_f_get_field);
JL_CALLABLE(jl_f_set_field);
JL_CALLABLE(jl_f_arraylen);
JL_CALLABLE(jl_f_arrayref);
JL_CALLABLE(jl_f_arrayset);
JL_CALLABLE(jl_f_arraysize);
JL_CALLABLE(jl_f_instantiate_type);
JL_CALLABLE(jl_f_convert);
JL_CALLABLE(jl_f_convert_to_ptr);
JL_CALLABLE(jl_f_convert_tuple);
JL_CALLABLE(jl_f_print_array_uint8);
JL_CALLABLE(jl_f_show_bool);
JL_CALLABLE(jl_f_show_char);
JL_CALLABLE(jl_f_show_float32);
JL_CALLABLE(jl_f_show_float64);
JL_CALLABLE(jl_f_show_int8);
JL_CALLABLE(jl_f_show_uint8);
JL_CALLABLE(jl_f_show_int16);
JL_CALLABLE(jl_f_show_uint16);
JL_CALLABLE(jl_f_show_int32);
JL_CALLABLE(jl_f_show_uint32);
JL_CALLABLE(jl_f_show_int64);
JL_CALLABLE(jl_f_show_uint64);
JL_CALLABLE(jl_f_show_pointer);
JL_CALLABLE(jl_f_show_typevar);
JL_CALLABLE(jl_f_show_linfo);
JL_CALLABLE(jl_f_show_any);
JL_CALLABLE(jl_f_print_symbol);
JL_CALLABLE(jl_trampoline);
JL_CALLABLE(jl_f_new_struct_type);
JL_CALLABLE(jl_f_new_struct_fields);
JL_CALLABLE(jl_f_new_type_constructor);
JL_CALLABLE(jl_f_new_tag_type);
JL_CALLABLE(jl_f_new_tag_type_super);
JL_CALLABLE(jl_f_new_bits_type);
JL_CALLABLE(jl_f_typevar);
JL_CALLABLE(jl_f_union);
JL_CALLABLE(jl_f_methodexists);
JL_CALLABLE(jl_f_applicable);
JL_CALLABLE(jl_f_invoke);
JL_CALLABLE(jl_apply_generic);
JL_CALLABLE(jl_unprotect_stack);
JL_CALLABLE(jl_f_task);
JL_CALLABLE(jl_f_yieldto);
JL_CALLABLE(jl_f_current_task);
JL_CALLABLE(jl_f_taskdone);

jl_value_t *jl_new_closure_internal(jl_lambda_info_t *li, jl_value_t *env);

#endif
