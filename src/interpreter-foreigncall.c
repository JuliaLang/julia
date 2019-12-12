// This file was auto-generated. Do not edit.
#include "julia.h"
#include "julia_internal.h"
#include "julia_gcext.h"
#include "gc.h"

extern void srand(unsigned int);
extern char * getenv(const char *);
extern int setenv(const char *, const char *, int);
extern void * memcpy(void *restrict, const void *restrict, size_t);
extern void * memmove(void *, const void *, size_t);
extern void * memset(void *, int, size_t);
extern int memcmp(const void *, const void *, size_t);
extern void * memchr(const void *, int, size_t);
extern unsigned long strlen(const char *);
typedef unsigned long size_t;
typedef unsigned int uint32_t;
extern char * uint2str(char *, size_t, uint64_t, uint32_t);
extern size_t u8_offset(const char *, size_t);
extern size_t u8_charnum(const char *, size_t);
extern size_t u8_strwidth(const char *);
extern int u8_isvalid(const char *, size_t);
extern size_t ios_read(ios_t *, char *, size_t);
extern size_t ios_readall(ios_t *, char *, size_t);
extern size_t ios_write(ios_t *, const char *, size_t);
extern int64_t ios_seek(ios_t *, int64_t);
extern int64_t ios_seek_end(ios_t *);
extern int64_t ios_skip(ios_t *, int64_t);
extern int64_t ios_pos(ios_t *);
extern int ios_trunc(ios_t *, size_t);
extern int ios_eof(ios_t *);
extern int ios_eof_blocking(ios_t *);
extern int ios_flush(ios_t *);
extern void ios_close(ios_t *);
extern int ios_isopen(ios_t *);
extern char * ios_take_buffer(ios_t *, size_t *);
extern int ios_setbuf(ios_t *, char *, size_t, int);
extern int ios_bufmode(ios_t *, bufmode_t);
extern int ios_get_readable(ios_t *);
extern int ios_get_writable(ios_t *);
extern void ios_set_readonly(ios_t *);
extern size_t ios_copy(ios_t *, ios_t *, size_t);
extern size_t ios_copyall(ios_t *, ios_t *);
extern size_t ios_copyuntil(ios_t *, ios_t *, char);
extern size_t ios_nchomp(ios_t *, size_t);
extern size_t ios_readprep(ios_t *, size_t);
extern ios_t * ios_file(ios_t *, const char *, int, int, int, int);
extern ios_t * ios_mkstemp(ios_t *, char *);
extern ios_t * ios_mem(ios_t *, size_t);
extern ios_t * ios_fd(ios_t *, long, int, int);
extern int ios_pututf8(ios_t *, uint32_t);
extern int ios_printf(ios_t *, const char *, ...);
extern int ios_getutf8(ios_t *, uint32_t *);
extern int ios_peekutf8(ios_t *, uint32_t *);
extern char * ios_readline(ios_t *);
extern void ios_purge(ios_t *);
extern int ios_putc(int, ios_t *);
extern int ios_getc(ios_t *);
extern int ios_peekc(ios_t *);
struct jl_timeval;
extern int jl_gettimeofday(struct jl_timeval *);
extern double jl_clock_now(void);
extern uint32_t int32hash(uint32_t);
extern uint64_t int64hash(uint64_t);
extern uint32_t int64to32hash(uint64_t);
extern uint64_t memhash(const char *, size_t);
extern uint64_t memhash_seed(const char *, size_t, uint32_t);
extern uint32_t memhash32(const char *, size_t);
extern uint32_t memhash32_seed(const char *, size_t, uint32_t);
extern uint32_t * bitvector_new(uint64_t, int);
extern uint32_t * bitvector_resize(uint32_t *, uint64_t, uint64_t, int);
extern void bitvector_set(uint32_t *, uint64_t, uint32_t);
extern uint32_t bitvector_get(uint32_t *, uint64_t);
extern double jl_strtod_c(const char *, char **);
extern float jl_strtof_c(const char *, char **);
extern void libsupport_init(void);
typedef short int16_t;
extern int16_t jl_threadid(void);
extern void jl_threading_profile(void);
#undef jl_get_ptls_states
extern jl_ptls_t jl_get_ptls_states(void);
#undef jl_cpu_pause
extern void jl_cpu_pause(void);
#undef jl_cpu_wake
extern void jl_cpu_wake(void);
#undef jl_gc_safepoint
extern void jl_gc_safepoint(void);
extern void jl_gc_enable_finalizers(jl_ptls_t, int);
extern void jl_wakeup_thread(int16_t);
extern int jl_gc_enable(int);
extern int jl_gc_is_enabled(void);
extern void jl_gc_collect(jl_gc_collection_t);
extern void jl_gc_add_finalizer(jl_value_t *, jl_function_t *);
extern void jl_finalize(jl_value_t *);
extern jl_weakref_t * jl_gc_new_weakref(jl_value_t *);
extern jl_value_t * jl_gc_alloc_0w(void);
extern jl_value_t * jl_gc_alloc_1w(void);
extern jl_value_t * jl_gc_alloc_2w(void);
extern jl_value_t * jl_gc_alloc_3w(void);
extern jl_value_t * jl_gc_allocobj(size_t);
struct _jl_task_t;
extern void * jl_malloc_stack(size_t *, struct _jl_task_t *);
extern void jl_free_stack(void *, size_t);
extern void jl_gc_use(jl_value_t *);
extern void jl_clear_malloc_data(void);
extern void jl_gc_queue_root(jl_value_t *);
extern void * jl_gc_managed_malloc(size_t);
extern void * jl_gc_managed_realloc(void *, size_t, size_t, int, jl_value_t *);
extern char * jl_array_typetagdata(jl_array_t *);
extern jl_svec_t * jl_compute_fieldtypes(jl_datatype_t *);
extern int jl_subtype(jl_value_t *, jl_value_t *);
extern int jl_egal(jl_value_t *, jl_value_t *);
typedef unsigned long uintptr_t;
extern uintptr_t jl_object_id(jl_value_t *);
extern int jl_has_free_typevars(jl_value_t *);
extern int jl_has_typevar(jl_value_t *, jl_tvar_t *);
extern int jl_has_typevar_from_unionall(jl_value_t *, jl_unionall_t *);
extern int jl_subtype_env_size(jl_value_t *);
extern int jl_subtype_env(jl_value_t *, jl_value_t *, jl_value_t **, int);
extern int jl_isa(jl_value_t *, jl_value_t *);
extern int jl_types_equal(jl_value_t *, jl_value_t *);
extern int jl_is_not_broken_subtype(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_type_union(jl_value_t **, size_t);
extern jl_value_t * jl_type_intersection(jl_value_t *, jl_value_t *);
extern int jl_has_empty_intersection(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_type_unionall(jl_tvar_t *, jl_value_t *);
extern const char * jl_typename_str(jl_value_t *);
extern const char * jl_typeof_str(jl_value_t *);
extern int jl_type_morespecific(jl_value_t *, jl_value_t *);
extern jl_typename_t * jl_new_typename_in(jl_sym_t *, jl_module_t *);
extern jl_tvar_t * jl_new_typevar(jl_sym_t *, jl_value_t *, jl_value_t *);
extern jl_value_t * jl_instantiate_unionall(jl_unionall_t *, jl_value_t *);
extern jl_value_t * jl_apply_type(jl_value_t *, jl_value_t **, size_t);
extern jl_value_t * jl_apply_type1(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_apply_type2(jl_value_t *, jl_value_t *, jl_value_t *);
extern jl_tupletype_t * jl_apply_tuple_type(jl_svec_t *);
extern jl_tupletype_t * jl_apply_tuple_type_v(jl_value_t **, size_t);
extern jl_datatype_t * jl_new_datatype(jl_sym_t *, jl_module_t *, jl_datatype_t *, jl_svec_t *, jl_svec_t *, jl_svec_t *, int, int, int);
extern jl_datatype_t * jl_new_primitivetype(jl_value_t *, jl_module_t *, jl_datatype_t *, jl_svec_t *, size_t);
extern jl_value_t * jl_new_bits(jl_value_t *, void *);
extern jl_value_t * jl_new_struct(jl_datatype_t *, ...);
extern jl_value_t * jl_new_structv(jl_datatype_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_new_structt(jl_datatype_t *, jl_value_t *);
extern jl_value_t * jl_new_struct_uninit(jl_datatype_t *);
extern jl_method_instance_t * jl_new_method_instance_uninit(void);
extern jl_svec_t * (jl_svec)(size_t, ...);
extern jl_svec_t * jl_svec1(void *);
extern jl_svec_t * jl_svec2(void *, void *);
extern jl_svec_t * jl_alloc_svec(size_t);
extern jl_svec_t * jl_alloc_svec_uninit(size_t);
extern jl_svec_t * jl_svec_copy(jl_svec_t *);
extern jl_svec_t * jl_svec_fill(size_t, jl_value_t *);
extern jl_value_t * jl_tupletype_fill(size_t, jl_value_t *);
extern jl_sym_t * jl_symbol(const char *);
extern jl_sym_t * jl_symbol_lookup(const char *);
extern jl_sym_t * jl_symbol_n(const char *, size_t);
extern jl_sym_t * jl_gensym(void);
typedef int int32_t;
extern jl_sym_t * jl_tagged_gensym(const char *, int32_t);
extern jl_sym_t * jl_get_root_symbol(void);
extern jl_value_t * jl_generic_function_def(jl_sym_t *, jl_module_t *, jl_value_t **, jl_value_t *, jl_binding_t *);
extern void jl_method_def(jl_svec_t *, jl_code_info_t *, jl_module_t *);
extern jl_code_info_t * jl_code_for_staged(jl_method_instance_t *);
extern jl_code_info_t * jl_copy_code_info(jl_code_info_t *);
extern size_t jl_get_world_counter(void);
extern jl_function_t * jl_get_kwsorter(jl_value_t *);
typedef signed char int8_t;
extern jl_value_t * jl_box_bool(int8_t);
extern jl_value_t * jl_box_int8(int8_t);
typedef unsigned char uint8_t;
extern jl_value_t * jl_box_uint8(uint8_t);
extern jl_value_t * jl_box_int16(int16_t);
typedef unsigned short uint16_t;
extern jl_value_t * jl_box_uint16(uint16_t);
extern jl_value_t * jl_box_int32(int32_t);
extern jl_value_t * jl_box_uint32(uint32_t);
extern jl_value_t * jl_box_char(uint32_t);
extern jl_value_t * jl_box_int64(int64_t);
extern jl_value_t * jl_box_uint64(uint64_t);
extern jl_value_t * jl_box_float32(float);
extern jl_value_t * jl_box_float64(double);
extern jl_value_t * jl_box_voidpointer(void *);
extern jl_value_t * jl_box_ssavalue(size_t);
extern jl_value_t * jl_box_slotnumber(size_t);
extern int8_t jl_unbox_bool(jl_value_t *);
extern int8_t jl_unbox_int8(jl_value_t *);
extern uint8_t jl_unbox_uint8(jl_value_t *);
extern int16_t jl_unbox_int16(jl_value_t *);
extern uint16_t jl_unbox_uint16(jl_value_t *);
extern int32_t jl_unbox_int32(jl_value_t *);
extern uint32_t jl_unbox_uint32(jl_value_t *);
extern int64_t jl_unbox_int64(jl_value_t *);
extern uint64_t jl_unbox_uint64(jl_value_t *);
extern float jl_unbox_float32(jl_value_t *);
extern double jl_unbox_float64(jl_value_t *);
extern void * jl_unbox_voidpointer(jl_value_t *);
extern int jl_get_size(jl_value_t *, size_t *);
extern int jl_field_index(jl_datatype_t *, jl_sym_t *, int);
extern jl_value_t * jl_get_nth_field(jl_value_t *, size_t);
extern jl_value_t * jl_get_nth_field_noalloc(jl_value_t *, size_t);
extern jl_value_t * jl_get_nth_field_checked(jl_value_t *, size_t);
extern void jl_set_nth_field(jl_value_t *, size_t, jl_value_t *);
extern int jl_field_isdefined(jl_value_t *, size_t);
extern jl_value_t * jl_get_field(jl_value_t *, const char *);
extern jl_value_t * jl_value_ptr(jl_value_t *);
extern int jl_islayout_inline(jl_value_t *, size_t *, size_t *);
extern jl_array_t * jl_new_array(jl_value_t *, jl_value_t *);
extern jl_array_t * jl_reshape_array(jl_value_t *, jl_array_t *, jl_value_t *);
extern jl_array_t * jl_ptr_to_array_1d(jl_value_t *, void *, size_t, int);
extern jl_array_t * jl_ptr_to_array(jl_value_t *, void *, jl_value_t *, int);
extern jl_array_t * jl_alloc_array_1d(jl_value_t *, size_t);
extern jl_array_t * jl_alloc_array_2d(jl_value_t *, size_t, size_t);
extern jl_array_t * jl_alloc_array_3d(jl_value_t *, size_t, size_t, size_t);
extern jl_array_t * jl_pchar_to_array(const char *, size_t);
extern jl_value_t * jl_pchar_to_string(const char *, size_t);
extern jl_value_t * jl_cstr_to_string(const char *);
extern jl_value_t * jl_alloc_string(size_t);
extern jl_value_t * jl_array_to_string(jl_array_t *);
extern jl_array_t * jl_alloc_vec_any(size_t);
extern jl_value_t * jl_arrayref(jl_array_t *, size_t);
extern jl_value_t * jl_ptrarrayref(jl_array_t *, size_t);
extern void jl_arrayset(jl_array_t *, jl_value_t *, size_t);
extern void jl_arrayunset(jl_array_t *, size_t);
extern int jl_array_isassigned(jl_array_t *, size_t);
extern void jl_array_grow_end(jl_array_t *, size_t);
extern void jl_array_del_end(jl_array_t *, size_t);
extern void jl_array_grow_beg(jl_array_t *, size_t);
extern void jl_array_del_beg(jl_array_t *, size_t);
extern void jl_array_sizehint(jl_array_t *, size_t);
extern void jl_array_ptr_1d_push(jl_array_t *, jl_value_t *);
extern void jl_array_ptr_1d_append(jl_array_t *, jl_array_t *);
extern jl_value_t * jl_apply_array_type(jl_value_t *, size_t);
extern void * jl_array_ptr(jl_array_t *);
extern void * jl_array_eltype(jl_value_t *);
extern int jl_array_rank(jl_value_t *);
extern size_t jl_array_size(jl_value_t *, int);
extern const char * jl_string_ptr(jl_value_t *);
extern jl_module_t * jl_new_module(jl_sym_t *);
extern void jl_set_module_nospecialize(jl_module_t *, int);
extern jl_binding_t * jl_get_binding(jl_module_t *, jl_sym_t *);
extern jl_binding_t * jl_get_binding_or_error(jl_module_t *, jl_sym_t *);
extern jl_value_t * jl_module_globalref(jl_module_t *, jl_sym_t *);
extern jl_binding_t * jl_get_binding_wr(jl_module_t *, jl_sym_t *, int);
extern jl_binding_t * jl_get_binding_for_method_def(jl_module_t *, jl_sym_t *);
extern int jl_boundp(jl_module_t *, jl_sym_t *);
extern int jl_defines_or_exports_p(jl_module_t *, jl_sym_t *);
extern int jl_binding_resolved_p(jl_module_t *, jl_sym_t *);
extern int jl_is_const(jl_module_t *, jl_sym_t *);
extern jl_value_t * jl_get_global(jl_module_t *, jl_sym_t *);
extern void jl_set_global(jl_module_t *, jl_sym_t *, jl_value_t *);
extern void jl_set_const(jl_module_t *, jl_sym_t *, jl_value_t *);
extern void jl_checked_assignment(jl_binding_t *, jl_value_t *);
extern void jl_declare_constant(jl_binding_t *);
extern void jl_module_using(jl_module_t *, jl_module_t *);
extern void jl_module_use(jl_module_t *, jl_module_t *, jl_sym_t *);
extern void jl_module_import(jl_module_t *, jl_module_t *, jl_sym_t *);
extern void jl_module_export(jl_module_t *, jl_sym_t *);
extern int jl_is_imported(jl_module_t *, jl_sym_t *);
extern int jl_module_exports_p(jl_module_t *, jl_sym_t *);
extern void jl_add_standard_imports(jl_module_t *);
extern jl_array_t * jl_eqtable_put(jl_array_t *, jl_value_t *, jl_value_t *, int *);
extern jl_value_t * jl_eqtable_get(jl_array_t *, jl_value_t *, jl_value_t *);
extern int jl_errno(void);
extern void jl_set_errno(int);
extern int32_t jl_stat(const char *, char *);
extern int jl_cpu_threads(void);
extern long jl_getpagesize(void);
extern long jl_getallocationgranularity(void);
extern int jl_is_debugbuild(void);
extern jl_sym_t * jl_get_UNAME(void);
extern jl_sym_t * jl_get_ARCH(void);
extern jl_value_t * jl_environ(int);
extern void jl_error(const char *);
extern void jl_errorf(const char *, ...);
extern void jl_exceptionf(jl_datatype_t *, const char *, ...);
extern void jl_too_few_args(const char *, int);
extern void jl_too_many_args(const char *, int);
extern void jl_type_error(const char *, jl_value_t *, jl_value_t *);
extern void jl_type_error_rt(const char *, const char *, jl_value_t *, jl_value_t *);
extern void jl_undefined_var_error(jl_sym_t *);
extern void jl_bounds_error(jl_value_t *, jl_value_t *);
extern void jl_bounds_error_v(jl_value_t *, jl_value_t **, size_t);
extern void jl_bounds_error_int(jl_value_t *, size_t);
extern void jl_bounds_error_tuple_int(jl_value_t **, size_t, size_t);
extern void jl_bounds_error_unboxed_int(void *, jl_value_t *, size_t);
extern void jl_bounds_error_ints(jl_value_t *, size_t *, size_t);
extern void jl_eof_error(void);
extern jl_value_t * jl_current_exception(void);
extern jl_value_t * jl_exception_occurred(void);
extern void jl_exception_clear(void);
extern void julia_init(JL_IMAGE_SEARCH);
extern void jl_init(void);
extern void jl_init_with_image(const char *, const char *);
extern const char * jl_get_default_sysimg_path(void);
extern int jl_is_initialized(void);
extern void jl_atexit_hook(int);
extern void jl_exit(int);
extern const char * jl_pathname_for_handle(void *);
extern int jl_deserialize_verify_header(ios_t *);
extern void jl_preload_sysimg_so(const char *);
extern void jl_set_sysimg_so(void *);
extern ios_t * jl_create_system_image(void);
extern void jl_save_system_image(const char *);
extern void jl_restore_system_image(const char *);
extern void jl_restore_system_image_data(const char *, size_t);
extern int jl_save_incremental(const char *, jl_array_t *);
extern jl_value_t * jl_restore_incremental(const char *, jl_array_t *);
extern jl_value_t * jl_restore_incremental_from_buf(const char *, size_t, jl_array_t *);
extern jl_value_t * jl_parse_all(const char *, size_t, const char *, size_t);
extern jl_value_t * jl_parse_input_line(const char *, size_t, const char *, size_t);
extern jl_value_t * jl_parse_string(const char *, size_t, int, int);
extern jl_value_t * jl_load_file_string(const char *, size_t, char *, jl_module_t *);
extern jl_value_t * jl_expand(jl_value_t *, jl_module_t *);
extern jl_value_t * jl_expand_with_loc(jl_value_t *, jl_module_t *, const char *, int);
extern jl_value_t * jl_expand_stmt(jl_value_t *, jl_module_t *);
extern jl_value_t * jl_expand_stmt_with_loc(jl_value_t *, jl_module_t *, const char *, int);
extern jl_value_t * jl_eval_string(const char *);
extern jl_uv_libhandle jl_load_dynamic_library(const char *, unsigned int, int);
extern jl_uv_libhandle jl_dlopen(const char *, unsigned int);
extern int jl_dlclose(jl_uv_libhandle);
extern int jl_dlsym(jl_uv_libhandle, const char *, void **, int);
extern jl_value_t * jl_toplevel_eval(jl_module_t *, jl_value_t *);
extern jl_value_t * jl_toplevel_eval_in(jl_module_t *, jl_value_t *);
extern jl_value_t * jl_load(jl_module_t *, const char *);
extern jl_module_t * jl_base_relative_to(jl_module_t *);
extern void jl_register_newmeth_tracer(void (*)(jl_method_t *));
extern jl_value_t * jl_copy_ast(jl_value_t *);
extern jl_array_t * jl_compress_ast(jl_method_t *, jl_code_info_t *);
extern jl_code_info_t * jl_uncompress_ast(jl_method_t *, jl_code_instance_t *, jl_array_t *);
extern uint8_t jl_ast_flag_inferred(jl_array_t *);
extern uint8_t jl_ast_flag_inlineable(jl_array_t *);
extern uint8_t jl_ast_flag_pure(jl_array_t *);
typedef long ssize_t;
extern ssize_t jl_ast_nslots(jl_array_t *);
extern uint8_t jl_ast_slotflag(jl_array_t *, size_t);
extern jl_value_t * jl_compress_argnames(jl_array_t *);
extern jl_array_t * jl_uncompress_argnames(jl_value_t *);
extern jl_value_t * jl_uncompress_argname_n(jl_value_t *, size_t);
extern int jl_is_operator(char *);
extern int jl_is_unary_operator(char *);
extern int jl_is_unary_and_binary_operator(char *);
extern int jl_operator_precedence(char *);
extern jl_value_t * jl_apply_generic(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_invoke(jl_value_t *, jl_value_t **, uint32_t, jl_method_instance_t *);
extern int32_t jl_invoke_api(jl_code_instance_t *);
extern jl_value_t * jl_call(jl_function_t *, jl_value_t **, int32_t);
extern jl_value_t * jl_call0(jl_function_t *);
extern jl_value_t * jl_call1(jl_function_t *, jl_value_t *);
extern jl_value_t * jl_call2(jl_function_t *, jl_value_t *, jl_value_t *);
extern jl_value_t * jl_call3(jl_function_t *, jl_value_t *, jl_value_t *, jl_value_t *);
extern void jl_yield(void);
extern void jl_install_sigint_handler(void);
extern void jl_sigatomic_begin(void);
extern void jl_sigatomic_end(void);
extern jl_task_t * jl_new_task(jl_function_t *, jl_value_t *, size_t);
extern void jl_switchto(jl_value_t *);
extern void jl_throw(jl_value_t *);
extern void jl_rethrow(void);
extern void jl_sig_throw(void);
extern void jl_rethrow_other(jl_value_t *);
extern void jl_no_exc_handler(jl_value_t *);
extern void jl_enter_handler(jl_handler_t *);
extern void jl_eh_restore_state(jl_handler_t *);
extern void jl_pop_handler(int);
extern size_t jl_excstack_state(void);
extern void jl_restore_excstack(size_t);
extern jl_array_t * jl_take_buffer(ios_t *);
extern int jl_sizeof_ios_t(void);
extern void jl_uv_puts(ios_t *, const char *, size_t);
extern int jl_printf(ios_t *, const char *, ...);
extern void jl_safe_printf(const char *, ...);
extern ios_t * jl_stdout_stream(void);
extern ios_t * jl_stdin_stream(void);
extern ios_t * jl_stderr_stream(void);
extern void jl_flush_cstdio(void);
extern jl_value_t * jl_stdout_obj(void);
extern jl_value_t * jl_stderr_obj(void);
extern size_t jl_static_show(ios_t *, jl_value_t *);
extern size_t jl_static_show_func_sig(ios_t *, jl_value_t *);
extern void jlbacktrace(void);
extern void jl_(jl_value_t *);
extern ssize_t jl_sizeof_jl_options(void);
extern void jl_parse_opts(int *, char ***);
extern char * jl_format_filename(const char *);
extern void jl_set_ARGS(int, char **);
extern int jl_generating_output(void);
extern int jl_ver_major(void);
extern int jl_ver_minor(void);
extern int jl_ver_patch(void);
extern int jl_ver_is_release(void);
extern const char * jl_ver_string(void);
extern const char * jl_git_branch(void);
extern const char * jl_git_commit(void);
extern jl_value_t * jl_gc_pool_alloc(jl_ptls_t, int, int);
extern jl_value_t * jl_gc_big_alloc(jl_ptls_t, size_t);
extern int jl_alignment(size_t);
extern void * jl_gc_counted_malloc(size_t);
extern void jl_throw_out_of_memory_error(void);
extern int64_t jl_gc_diff_total_bytes(void);
extern jl_code_instance_t * jl_get_method_inferred(jl_method_instance_t *, jl_value_t *, size_t, size_t);
extern int jl_compile_hint(jl_tupletype_t *);
extern void jl_foreigncall_get_syms(jl_value_t *, jl_sym_t **, jl_sym_t **);
extern int jl_foreigncall_interpretable(jl_sym_t *, jl_sym_t *);
extern jl_code_info_t * jl_new_code_info_uninit(void);
extern jl_value_t * jl_apply_2va(jl_value_t *, jl_value_t **, uint32_t);
extern void jl_typeassert(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_f_tuple(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_intrinsic_call(jl_value_t *, jl_value_t **, uint32_t);
extern void jl_method_table_insert(jl_methtable_t *, jl_method_t *, jl_tupletype_t *);
extern int jl_type_morespecific_no_subtype(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_instantiate_type_in_env(jl_value_t *, jl_unionall_t *, jl_value_t **);
extern jl_value_t * jl_matching_methods(jl_tupletype_t *, int, int, size_t, size_t *, size_t *);
extern jl_datatype_t * jl_first_argument_datatype(jl_value_t *);
extern jl_value_t * jl_argument_datatype(jl_value_t *);
extern jl_methtable_t * jl_method_table_for(jl_value_t *);
extern jl_value_t * jl_dump_fptr_asm(uint64_t, int, const char *, const char *);
extern jl_array_t * jl_idtable_rehash(jl_array_t *, size_t);
extern jl_methtable_t * jl_new_method_table(jl_sym_t *, jl_module_t *);
extern jl_value_t * jl_rettype_inferred(jl_method_instance_t *, size_t, size_t);
extern jl_value_t * jl_methtable_lookup(jl_methtable_t *, jl_value_t *, size_t);
extern jl_method_instance_t * jl_specializations_get_linfo(jl_method_t *, jl_value_t *, jl_svec_t *);
extern void jl_method_instance_add_backedge(jl_method_instance_t *, jl_method_instance_t *);
extern void jl_method_table_add_backedge(jl_methtable_t *, jl_value_t *, jl_value_t *);
extern jl_value_t * jl_get_backtrace(void);
extern void jl_raise_debugger(void);
extern void jl_gdblookup(void *);
extern int jl_is_interpreter_frame(uintptr_t);
extern int jl_is_enter_interpreter_frame(uintptr_t);
extern size_t jl_capture_interp_frame(jl_bt_element_t *, uintptr_t, uintptr_t, size_t);
extern uint64_t jl_hrtime(void);
extern void * jl_load_and_lookup(const char *, const char *, void **);
extern jl_value_t * jl_get_cfunction_trampoline(jl_value_t *, jl_datatype_t *, htable_t *, jl_svec_t *, void *(*)(void *, void **), jl_unionall_t *, jl_value_t **);
extern jl_value_t * jl_get_JIT(void);
extern int jl_fs_rename(const char *, const char *);
extern int jl_cwd(char *, size_t *);
extern int jl_is_file(char *);
extern const char * jl_intrinsic_name(int);
extern jl_value_t * jl_bitcast(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_pointerref(jl_value_t *, jl_value_t *, jl_value_t *);
extern jl_value_t * jl_pointerset(jl_value_t *, jl_value_t *, jl_value_t *, jl_value_t *);
extern jl_value_t * jl_cglobal(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_cglobal_auto(jl_value_t *);
extern jl_value_t * jl_neg_int(jl_value_t *);
extern jl_value_t * jl_add_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_sub_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_mul_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_sdiv_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_udiv_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_srem_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_urem_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_add_ptr(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_sub_ptr(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_neg_float(jl_value_t *);
extern jl_value_t * jl_add_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_sub_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_mul_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_div_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_rem_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_fma_float(jl_value_t *, jl_value_t *, jl_value_t *);
extern jl_value_t * jl_muladd_float(jl_value_t *, jl_value_t *, jl_value_t *);
extern jl_value_t * jl_eq_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_ne_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_slt_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_ult_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_sle_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_ule_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_eq_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_ne_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_lt_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_le_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_fpiseq(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_fpislt(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_not_int(jl_value_t *);
extern jl_value_t * jl_and_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_or_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_xor_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_shl_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_lshr_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_ashr_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_bswap_int(jl_value_t *);
extern jl_value_t * jl_ctpop_int(jl_value_t *);
extern jl_value_t * jl_ctlz_int(jl_value_t *);
extern jl_value_t * jl_cttz_int(jl_value_t *);
extern jl_value_t * jl_sext_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_zext_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_trunc_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_sitofp(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_uitofp(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_fptoui(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_fptosi(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_fptrunc(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_fpext(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_sadd_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_uadd_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_ssub_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_usub_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_smul_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_umul_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_sdiv_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_udiv_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_srem_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_checked_urem_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_ceil_llvm(jl_value_t *);
extern jl_value_t * jl_floor_llvm(jl_value_t *);
extern jl_value_t * jl_trunc_llvm(jl_value_t *);
extern jl_value_t * jl_rint_llvm(jl_value_t *);
extern jl_value_t * jl_sqrt_llvm(jl_value_t *);
extern jl_value_t * jl_sqrt_llvm_fast(jl_value_t *);
extern jl_value_t * jl_abs_float(jl_value_t *);
extern jl_value_t * jl_copysign_float(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_flipsign_int(jl_value_t *, jl_value_t *);
extern jl_value_t * jl_arraylen(jl_value_t *);
#undef jl_array_data_owner
extern jl_value_t * jl_array_data_owner(jl_array_t *);
extern uintptr_t jl_object_id_(jl_value_t *, jl_value_t *);
extern jl_array_t * jl_array_cconvert_cstring(jl_array_t *);
extern void jl_extern_c(jl_function_t *, jl_value_t *, jl_value_t *, char *);
extern void * jl_function_ptr(jl_function_t *, jl_value_t *, jl_value_t *);
extern const jl_value_t * jl_dump_function_asm(void *, int, const char *, const char *);
extern const jl_value_t * jl_dump_function_ir(void *, uint8_t, uint8_t, const char *);
extern void * jl_LLVMCreateDisasm(const char *, void *, int, void *, void *);
extern size_t jl_LLVMDisasmInstruction(void *, uint8_t *, uint64_t, uint64_t, char *, size_t);
extern uint32_t jl_get_LLVM_VERSION(void);
extern uint32_t jl_getutf8(ios_t *);
extern int jl_sizeof_off_t(void);
extern int jl_sizeof_mode_t(void);
extern int jl_ftruncate(int, int64_t);
extern int64_t jl_lseek(int, int64_t, int);
extern ssize_t jl_pwrite(int, const void *, size_t, int64_t);
extern void * jl_mmap(void *, size_t, int, int, int, int64_t);
extern long jl_ios_fd(ios_t *);
extern int32_t jl_nb_available(ios_t *);
extern jl_value_t * jl_readuntil(ios_t *, uint8_t, uint8_t, uint8_t);
extern int jl_ios_buffer_n(ios_t *, const size_t);
extern uint64_t jl_ios_get_nbyte_int(ios_t *, const size_t);
extern void jl_native_alignment(uint_t *, uint_t *, uint_t *, uint_t *, uint_t *, uint_t *);
extern jl_value_t * jl_is_char_signed(void);
extern long jl_SC_CLK_TCK(void);
extern size_t jl_maxrss(void);
extern int jl_threading_enabled(void);
extern jl_value_t * jl_f_throw(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_is(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_typeof(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_sizeof(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_issubtype(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_isa(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f__apply(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f__apply_pure(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f__apply_latest(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f__apply_iterate(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_isdefined(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_nfields(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_svec(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_getfield(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_setfield(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_fieldtype(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_arrayref(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_const_arrayref(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_arrayset(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_arraysize(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_apply_type(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_applicable(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_invoke(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f__expr(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_typeassert(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_ifelse(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f__typevar(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_f_invoke_kwsorter(jl_value_t *, jl_value_t **, uint32_t);
extern jl_value_t * jl_get_cpu_name(void);
extern void jl_dump_host_cpu(void);
extern int jl_running_on_valgrind(void);
extern size_t ios_write_direct(ios_t *, ios_t *);
extern int8_t jl_is_memdebug(void);
extern jl_value_t * jl_get_julia_bindir(void);
extern jl_value_t * jl_get_julia_bin(void);
extern jl_value_t * jl_get_image_file(void);
#undef jl_astaggedvalue
extern jl_taggedvalue_t * jl_astaggedvalue(jl_value_t *);
#undef jl_valueof
extern jl_value_t * jl_valueof(jl_taggedvalue_t *);
#undef jl_typeof
extern jl_value_t * jl_typeof(jl_value_t *);
#undef jl_get_fieldtypes
extern jl_value_t * jl_get_fieldtypes(jl_value_t *);
#undef jl_gc_unsafe_enter
extern int8_t jl_gc_unsafe_enter(void);
#undef jl_gc_unsafe_leave
extern void jl_gc_unsafe_leave(int8_t);
#undef jl_gc_safe_enter
extern int8_t jl_gc_safe_enter(void);
#undef jl_gc_safe_leave
extern void jl_gc_safe_leave(int8_t);
extern void jl_get_fenv_consts(int *);
extern jl_array_t * jl_find_free_typevars(jl_value_t *);
extern void jl_set_jsfunction_type(jl_value_t *);
extern jl_value_t * jl_eqtable_pop(jl_array_t *, jl_value_t *, jl_value_t *, int *);
extern size_t jl_eqtable_nextind(jl_array_t *, size_t);
extern jl_value_t * jl_get_keyword_sorter(jl_value_t *);
extern int jl_array_store_unboxed(jl_value_t *);
extern jl_array_t * jl_string_to_array(jl_value_t *);
extern void jl_array_grow_at(jl_array_t *, ssize_t, size_t);
extern void jl_array_del_at(jl_array_t *, ssize_t, size_t);
extern jl_array_t * jl_array_copy(jl_array_t *);
extern void jl_array_ptr_copy(jl_array_t *, void **, jl_array_t *, void **, ssize_t);
extern jl_value_t * jl_f_new_module(jl_sym_t *, uint8_t);
extern void jl_set_istopmod(jl_module_t *, uint8_t);
extern uint8_t jl_istopmod(jl_module_t *);
extern jl_module_t * jl_get_module_of_binding(jl_module_t *, jl_sym_t *);
extern jl_value_t * jl_binding_owner(jl_module_t *, jl_sym_t *);
extern jl_binding_t * jl_get_module_binding(jl_module_t *, jl_sym_t *);
extern void jl_deprecate_binding(jl_module_t *, jl_sym_t *, int);
extern int jl_is_binding_deprecated(jl_module_t *, jl_sym_t *);
extern jl_value_t * jl_module_usings(jl_module_t *);
extern jl_value_t * jl_module_names(jl_module_t *, int, int);
extern jl_sym_t * jl_module_name(jl_module_t *);
extern jl_module_t * jl_module_parent(jl_module_t *);
extern uint64_t jl_module_build_id(jl_module_t *);
extern jl_uuid_t jl_module_uuid(jl_module_t *);
extern void jl_set_module_uuid(jl_module_t *, jl_uuid_t);
extern void jl_gc_set_cb_root_scanner(jl_gc_cb_root_scanner_t, int);
extern void jl_gc_set_cb_task_scanner(jl_gc_cb_task_scanner_t, int);
extern void jl_gc_set_cb_pre_gc(jl_gc_cb_pre_gc_t, int);
extern void jl_gc_set_cb_post_gc(jl_gc_cb_post_gc_t, int);
extern void jl_gc_set_cb_notify_external_alloc(jl_gc_cb_notify_external_alloc_t, int);
extern void jl_gc_set_cb_notify_external_free(jl_gc_cb_notify_external_free_t, int);
extern jl_datatype_t * jl_new_foreign_type(jl_sym_t *, jl_module_t *, jl_datatype_t *, jl_markfunc_t, jl_sweepfunc_t, int, int);
extern size_t jl_gc_max_internal_obj_size(void);
extern size_t jl_gc_external_obj_hdr_size(void);
extern void * jl_gc_alloc_typed(jl_ptls_t, size_t, void *);
extern int jl_gc_mark_queue_obj(jl_ptls_t, jl_value_t *);
extern void jl_gc_mark_queue_objarray(jl_ptls_t, jl_value_t *, jl_value_t **, size_t);
extern void jl_gc_schedule_foreign_sweepfunc(jl_ptls_t, jl_value_t *);
extern int jl_gc_enable_conservative_gc_support(void);
extern int jl_gc_conservative_gc_support_enabled(void);
extern jl_value_t * jl_gc_internal_obj_base_ptr(void *);
extern void * jl_task_stack_buffer(jl_task_t *, size_t *, int *);
extern void jl_gc_add_ptr_finalizer(jl_ptls_t, jl_value_t *, void *);
extern void jl_gc_add_finalizer_th(jl_ptls_t, jl_value_t *, jl_function_t *);
extern void jl_finalize_th(jl_ptls_t, jl_value_t *);
extern jl_weakref_t * jl_gc_new_weakref_th(jl_ptls_t, jl_value_t *);
extern void jl_gc_get_total_bytes(int64_t *);
extern uint64_t jl_gc_total_hrtime(void);
extern jl_gc_num_t jl_gc_num(void);
extern void * jl_gc_counted_calloc(size_t, size_t);
extern void jl_gc_counted_free_with_size(void *, size_t);
extern void * jl_gc_counted_realloc_with_old_size(void *, size_t, size_t);
extern void * jl_malloc(size_t);
extern void * jl_calloc(size_t, size_t);
extern void jl_free(void *);
extern void * jl_realloc(void *, size_t);
extern void jl_profile_stop_timer(void);
extern int jl_profile_start_timer(void);
extern void jl_exit_on_sigint(int);
extern int jl_repl_raise_sigtstp(void);
extern int jl_profile_init(size_t, uint64_t);
extern uint8_t * jl_profile_get_data(void);
extern size_t jl_profile_len_data(void);
extern size_t jl_profile_maxlen_data(void);
extern uint64_t jl_profile_delay_nsec(void);
extern void jl_profile_clear_data(void);
extern int jl_profile_is_running(void);
#undef jl_symbol_name
extern void * jl_symbol_name(jl_sym_t *);
extern jl_nullable_float64_t jl_try_substrtod(char *, size_t, size_t);
extern int jl_substrtod(char *, size_t, size_t, double *);
extern jl_nullable_float32_t jl_try_substrtof(char *, size_t, size_t);
extern int jl_substrtof(char *, int, size_t, float *);
extern int jl_id_start_char(uint32_t);
extern int jl_id_char(uint32_t);
extern int jl_is_identifier(char *);
extern void jl_breakpoint(jl_value_t *);
extern void jl_uv_flush(ios_t *);
extern void jl_uv_putb(ios_t *, uint8_t);
extern int jl_sizeof_stat(void);
extern int32_t jl_lstat(const char *, char *);
extern int32_t jl_fstat(int, char *);
extern unsigned int jl_stat_dev(char *);
extern unsigned int jl_stat_ino(char *);
extern unsigned int jl_stat_mode(char *);
extern unsigned int jl_stat_nlink(char *);
extern unsigned int jl_stat_uid(char *);
extern unsigned int jl_stat_gid(char *);
extern unsigned int jl_stat_rdev(char *);
extern uint64_t jl_stat_size(char *);
extern uint64_t jl_stat_blksize(char *);
extern uint64_t jl_stat_blocks(char *);
extern double jl_stat_mtime(char *);
extern double jl_stat_ctime(char *);
extern void jl_wake_libuv(void);
extern size_t jl_get_tls_world_age(void);
extern int8_t jl_is_in_pure_context(void);
extern jl_value_t * jl_specializations_lookup(jl_method_t *, jl_value_t *);
extern jl_method_t * jl_new_method_uninit(jl_module_t *);
extern jl_code_instance_t * jl_set_method_inferred(jl_method_instance_t *, jl_value_t *, jl_value_t *, jl_value_t *, int32_t, size_t, size_t);
extern jl_value_t * jl_call_in_typeinf_world(jl_value_t **, int);
extern void jl_set_typeinf_func(jl_value_t *);
extern int jl_isa_compileable_sig(jl_tupletype_t *, jl_method_t *);
extern void jl_method_table_disable(jl_methtable_t *, jl_method_t *);
extern jl_value_t * jl_get_spec_lambda(jl_tupletype_t *, size_t, size_t *, size_t *);
extern jl_value_t * jl_gf_invoke_lookup(jl_value_t *, size_t);
extern jl_value_t * jl_get_invoke_lambda(jl_typemap_entry_t *, jl_value_t *);
extern void jl_typeinf_begin(void);
extern void jl_typeinf_end(void);
extern jl_value_t * jl_get_current_task(void);
extern int jl_is_task_started(jl_task_t *);
extern int16_t jl_get_task_tid(jl_task_t *);
extern void jl_set_task_tid(jl_task_t *, int);
extern jl_task_t * jl_task_get_next(jl_value_t *, jl_value_t *);
extern int pcre2_config_8(uint32_t, void *);
struct pcre2_real_general_context_8;
typedef struct pcre2_real_general_context_8 pcre2_general_context_8;
extern pcre2_general_context_8 * pcre2_general_context_copy_8(pcre2_general_context_8 *);
extern pcre2_general_context_8 * pcre2_general_context_create_8(void *(*)(size_t, void *), void (*)(void *, void *), void *);
extern void pcre2_general_context_free_8(pcre2_general_context_8 *);
struct pcre2_real_compile_context_8;
typedef struct pcre2_real_compile_context_8 pcre2_compile_context_8;
extern pcre2_compile_context_8 * pcre2_compile_context_copy_8(pcre2_compile_context_8 *);
extern pcre2_compile_context_8 * pcre2_compile_context_create_8(pcre2_general_context_8 *);
extern void pcre2_compile_context_free_8(pcre2_compile_context_8 *);
extern int pcre2_set_bsr_8(pcre2_compile_context_8 *, uint32_t);
extern int pcre2_set_character_tables_8(pcre2_compile_context_8 *, const unsigned char *);
extern int pcre2_set_compile_extra_options_8(pcre2_compile_context_8 *, uint32_t);
extern int pcre2_set_max_pattern_length_8(pcre2_compile_context_8 *, size_t);
extern int pcre2_set_newline_8(pcre2_compile_context_8 *, uint32_t);
extern int pcre2_set_parens_nest_limit_8(pcre2_compile_context_8 *, uint32_t);
extern int pcre2_set_compile_recursion_guard_8(pcre2_compile_context_8 *, int (*)(uint32_t, void *), void *);
struct pcre2_real_convert_context_8;
typedef struct pcre2_real_convert_context_8 pcre2_convert_context_8;
extern pcre2_convert_context_8 * pcre2_convert_context_copy_8(pcre2_convert_context_8 *);
extern pcre2_convert_context_8 * pcre2_convert_context_create_8(pcre2_general_context_8 *);
extern void pcre2_convert_context_free_8(pcre2_convert_context_8 *);
extern int pcre2_set_glob_escape_8(pcre2_convert_context_8 *, uint32_t);
extern int pcre2_set_glob_separator_8(pcre2_convert_context_8 *, uint32_t);
typedef unsigned char PCRE2_UCHAR8;
typedef const PCRE2_UCHAR8 * PCRE2_SPTR8;
extern int pcre2_pattern_convert_8(PCRE2_SPTR8, size_t, uint32_t, PCRE2_UCHAR8 **, size_t *, pcre2_convert_context_8 *);
extern void pcre2_converted_pattern_free_8(PCRE2_UCHAR8 *);
struct pcre2_real_match_context_8;
typedef struct pcre2_real_match_context_8 pcre2_match_context_8;
extern pcre2_match_context_8 * pcre2_match_context_copy_8(pcre2_match_context_8 *);
extern pcre2_match_context_8 * pcre2_match_context_create_8(pcre2_general_context_8 *);
extern void pcre2_match_context_free_8(pcre2_match_context_8 *);
extern int pcre2_set_depth_limit_8(pcre2_match_context_8 *, uint32_t);
extern int pcre2_set_heap_limit_8(pcre2_match_context_8 *, uint32_t);
extern int pcre2_set_match_limit_8(pcre2_match_context_8 *, uint32_t);
extern int pcre2_set_offset_limit_8(pcre2_match_context_8 *, size_t);
extern int pcre2_set_recursion_limit_8(pcre2_match_context_8 *, uint32_t);
extern int pcre2_set_recursion_memory_management_8(pcre2_match_context_8 *, void *(*)(size_t, void *), void (*)(void *, void *), void *);
struct pcre2_real_code_8;
typedef struct pcre2_real_code_8 pcre2_code_8;
extern pcre2_code_8 * pcre2_compile_8(PCRE2_SPTR8, size_t, uint32_t, int *, size_t *, pcre2_compile_context_8 *);
extern void pcre2_code_free_8(pcre2_code_8 *);
extern pcre2_code_8 * pcre2_code_copy_8(const pcre2_code_8 *);
extern pcre2_code_8 * pcre2_code_copy_with_tables_8(const pcre2_code_8 *);
extern int pcre2_pattern_info_8(const pcre2_code_8 *, uint32_t, void *);
struct pcre2_real_match_data_8;
typedef struct pcre2_real_match_data_8 pcre2_match_data_8;
extern pcre2_match_data_8 * pcre2_match_data_create_8(uint32_t, pcre2_general_context_8 *);
extern pcre2_match_data_8 * pcre2_match_data_create_from_pattern_8(const pcre2_code_8 *, pcre2_general_context_8 *);
extern int pcre2_dfa_match_8(const pcre2_code_8 *, PCRE2_SPTR8, size_t, size_t, uint32_t, pcre2_match_data_8 *, pcre2_match_context_8 *, int *, size_t);
extern int pcre2_match_8(const pcre2_code_8 *, PCRE2_SPTR8, size_t, size_t, uint32_t, pcre2_match_data_8 *, pcre2_match_context_8 *);
extern void pcre2_match_data_free_8(pcre2_match_data_8 *);
extern PCRE2_SPTR8 pcre2_get_mark_8(pcre2_match_data_8 *);
extern uint32_t pcre2_get_ovector_count_8(pcre2_match_data_8 *);
extern size_t * pcre2_get_ovector_pointer_8(pcre2_match_data_8 *);
extern size_t pcre2_get_startchar_8(pcre2_match_data_8 *);
extern int pcre2_substring_copy_byname_8(pcre2_match_data_8 *, PCRE2_SPTR8, PCRE2_UCHAR8 *, size_t *);
extern int pcre2_substring_copy_bynumber_8(pcre2_match_data_8 *, uint32_t, PCRE2_UCHAR8 *, size_t *);
extern void pcre2_substring_free_8(PCRE2_UCHAR8 *);
extern int pcre2_substring_get_byname_8(pcre2_match_data_8 *, PCRE2_SPTR8, PCRE2_UCHAR8 **, size_t *);
extern int pcre2_substring_get_bynumber_8(pcre2_match_data_8 *, uint32_t, PCRE2_UCHAR8 **, size_t *);
extern int pcre2_substring_length_byname_8(pcre2_match_data_8 *, PCRE2_SPTR8, size_t *);
extern int pcre2_substring_length_bynumber_8(pcre2_match_data_8 *, uint32_t, size_t *);
extern int pcre2_substring_nametable_scan_8(const pcre2_code_8 *, PCRE2_SPTR8, PCRE2_SPTR8 *, PCRE2_SPTR8 *);
extern int pcre2_substring_number_from_name_8(const pcre2_code_8 *, PCRE2_SPTR8);
extern void pcre2_substring_list_free_8(PCRE2_SPTR8 *);
extern int pcre2_substring_list_get_8(pcre2_match_data_8 *, PCRE2_UCHAR8 ***, size_t **);
extern int32_t pcre2_serialize_encode_8(const pcre2_code_8 **, int32_t, uint8_t **, size_t *, pcre2_general_context_8 *);
extern int32_t pcre2_serialize_decode_8(pcre2_code_8 **, int32_t, const uint8_t *, pcre2_general_context_8 *);
extern int32_t pcre2_serialize_get_number_of_codes_8(const uint8_t *);
extern void pcre2_serialize_free_8(uint8_t *);
extern int pcre2_substitute_8(const pcre2_code_8 *, PCRE2_SPTR8, size_t, size_t, uint32_t, pcre2_match_data_8 *, pcre2_match_context_8 *, PCRE2_SPTR8, size_t, PCRE2_UCHAR8 *, size_t *);
extern int pcre2_jit_compile_8(pcre2_code_8 *, uint32_t);
extern int pcre2_jit_match_8(const pcre2_code_8 *, PCRE2_SPTR8, size_t, size_t, uint32_t, pcre2_match_data_8 *, pcre2_match_context_8 *);
extern void pcre2_jit_free_unused_memory_8(pcre2_general_context_8 *);
struct pcre2_real_jit_stack_8;
typedef struct pcre2_real_jit_stack_8 pcre2_jit_stack_8;
extern pcre2_jit_stack_8 * pcre2_jit_stack_create_8(size_t, size_t, pcre2_general_context_8 *);
typedef void *pcre2_jit_callback_8;
extern void pcre2_jit_stack_assign_8(pcre2_match_context_8 *, pcre2_jit_callback_8, void *);
extern void pcre2_jit_stack_free_8(pcre2_jit_stack_8 *);
extern int pcre2_get_error_message_8(int, PCRE2_UCHAR8 *, size_t);
extern const uint8_t * pcre2_maketables_8(pcre2_general_context_8 *);
extern int pcre2_config_16(uint32_t, void *);
struct pcre2_real_general_context_16;
typedef struct pcre2_real_general_context_16 pcre2_general_context_16;
extern pcre2_general_context_16 * pcre2_general_context_copy_16(pcre2_general_context_16 *);
extern pcre2_general_context_16 * pcre2_general_context_create_16(void *(*)(size_t, void *), void (*)(void *, void *), void *);
extern void pcre2_general_context_free_16(pcre2_general_context_16 *);
struct pcre2_real_compile_context_16;
typedef struct pcre2_real_compile_context_16 pcre2_compile_context_16;
extern pcre2_compile_context_16 * pcre2_compile_context_copy_16(pcre2_compile_context_16 *);
extern pcre2_compile_context_16 * pcre2_compile_context_create_16(pcre2_general_context_16 *);
extern void pcre2_compile_context_free_16(pcre2_compile_context_16 *);
extern int pcre2_set_bsr_16(pcre2_compile_context_16 *, uint32_t);
extern int pcre2_set_character_tables_16(pcre2_compile_context_16 *, const unsigned char *);
extern int pcre2_set_compile_extra_options_16(pcre2_compile_context_16 *, uint32_t);
extern int pcre2_set_max_pattern_length_16(pcre2_compile_context_16 *, size_t);
extern int pcre2_set_newline_16(pcre2_compile_context_16 *, uint32_t);
extern int pcre2_set_parens_nest_limit_16(pcre2_compile_context_16 *, uint32_t);
extern int pcre2_set_compile_recursion_guard_16(pcre2_compile_context_16 *, int (*)(uint32_t, void *), void *);
struct pcre2_real_convert_context_16;
typedef struct pcre2_real_convert_context_16 pcre2_convert_context_16;
extern pcre2_convert_context_16 * pcre2_convert_context_copy_16(pcre2_convert_context_16 *);
extern pcre2_convert_context_16 * pcre2_convert_context_create_16(pcre2_general_context_16 *);
extern void pcre2_convert_context_free_16(pcre2_convert_context_16 *);
extern int pcre2_set_glob_escape_16(pcre2_convert_context_16 *, uint32_t);
extern int pcre2_set_glob_separator_16(pcre2_convert_context_16 *, uint32_t);
typedef unsigned short PCRE2_UCHAR16;
typedef const PCRE2_UCHAR16 * PCRE2_SPTR16;
extern int pcre2_pattern_convert_16(PCRE2_SPTR16, size_t, uint32_t, PCRE2_UCHAR16 **, size_t *, pcre2_convert_context_16 *);
extern void pcre2_converted_pattern_free_16(PCRE2_UCHAR16 *);
struct pcre2_real_match_context_16;
typedef struct pcre2_real_match_context_16 pcre2_match_context_16;
extern pcre2_match_context_16 * pcre2_match_context_copy_16(pcre2_match_context_16 *);
extern pcre2_match_context_16 * pcre2_match_context_create_16(pcre2_general_context_16 *);
extern void pcre2_match_context_free_16(pcre2_match_context_16 *);
extern int pcre2_set_depth_limit_16(pcre2_match_context_16 *, uint32_t);
extern int pcre2_set_heap_limit_16(pcre2_match_context_16 *, uint32_t);
extern int pcre2_set_match_limit_16(pcre2_match_context_16 *, uint32_t);
extern int pcre2_set_offset_limit_16(pcre2_match_context_16 *, size_t);
extern int pcre2_set_recursion_limit_16(pcre2_match_context_16 *, uint32_t);
extern int pcre2_set_recursion_memory_management_16(pcre2_match_context_16 *, void *(*)(size_t, void *), void (*)(void *, void *), void *);
struct pcre2_real_code_16;
typedef struct pcre2_real_code_16 pcre2_code_16;
extern pcre2_code_16 * pcre2_compile_16(PCRE2_SPTR16, size_t, uint32_t, int *, size_t *, pcre2_compile_context_16 *);
extern void pcre2_code_free_16(pcre2_code_16 *);
extern pcre2_code_16 * pcre2_code_copy_16(const pcre2_code_16 *);
extern pcre2_code_16 * pcre2_code_copy_with_tables_16(const pcre2_code_16 *);
extern int pcre2_pattern_info_16(const pcre2_code_16 *, uint32_t, void *);
struct pcre2_real_match_data_16;
typedef struct pcre2_real_match_data_16 pcre2_match_data_16;
extern pcre2_match_data_16 * pcre2_match_data_create_16(uint32_t, pcre2_general_context_16 *);
extern pcre2_match_data_16 * pcre2_match_data_create_from_pattern_16(const pcre2_code_16 *, pcre2_general_context_16 *);
extern int pcre2_dfa_match_16(const pcre2_code_16 *, PCRE2_SPTR16, size_t, size_t, uint32_t, pcre2_match_data_16 *, pcre2_match_context_16 *, int *, size_t);
extern int pcre2_match_16(const pcre2_code_16 *, PCRE2_SPTR16, size_t, size_t, uint32_t, pcre2_match_data_16 *, pcre2_match_context_16 *);
extern void pcre2_match_data_free_16(pcre2_match_data_16 *);
extern PCRE2_SPTR16 pcre2_get_mark_16(pcre2_match_data_16 *);
extern uint32_t pcre2_get_ovector_count_16(pcre2_match_data_16 *);
extern size_t * pcre2_get_ovector_pointer_16(pcre2_match_data_16 *);
extern size_t pcre2_get_startchar_16(pcre2_match_data_16 *);
extern int pcre2_substring_copy_byname_16(pcre2_match_data_16 *, PCRE2_SPTR16, PCRE2_UCHAR16 *, size_t *);
extern int pcre2_substring_copy_bynumber_16(pcre2_match_data_16 *, uint32_t, PCRE2_UCHAR16 *, size_t *);
extern void pcre2_substring_free_16(PCRE2_UCHAR16 *);
extern int pcre2_substring_get_byname_16(pcre2_match_data_16 *, PCRE2_SPTR16, PCRE2_UCHAR16 **, size_t *);
extern int pcre2_substring_get_bynumber_16(pcre2_match_data_16 *, uint32_t, PCRE2_UCHAR16 **, size_t *);
extern int pcre2_substring_length_byname_16(pcre2_match_data_16 *, PCRE2_SPTR16, size_t *);
extern int pcre2_substring_length_bynumber_16(pcre2_match_data_16 *, uint32_t, size_t *);
extern int pcre2_substring_nametable_scan_16(const pcre2_code_16 *, PCRE2_SPTR16, PCRE2_SPTR16 *, PCRE2_SPTR16 *);
extern int pcre2_substring_number_from_name_16(const pcre2_code_16 *, PCRE2_SPTR16);
extern void pcre2_substring_list_free_16(PCRE2_SPTR16 *);
extern int pcre2_substring_list_get_16(pcre2_match_data_16 *, PCRE2_UCHAR16 ***, size_t **);
extern int32_t pcre2_serialize_encode_16(const pcre2_code_16 **, int32_t, uint8_t **, size_t *, pcre2_general_context_16 *);
extern int32_t pcre2_serialize_decode_16(pcre2_code_16 **, int32_t, const uint8_t *, pcre2_general_context_16 *);
extern int32_t pcre2_serialize_get_number_of_codes_16(const uint8_t *);
extern void pcre2_serialize_free_16(uint8_t *);
extern int pcre2_substitute_16(const pcre2_code_16 *, PCRE2_SPTR16, size_t, size_t, uint32_t, pcre2_match_data_16 *, pcre2_match_context_16 *, PCRE2_SPTR16, size_t, PCRE2_UCHAR16 *, size_t *);
extern int pcre2_jit_compile_16(pcre2_code_16 *, uint32_t);
extern int pcre2_jit_match_16(const pcre2_code_16 *, PCRE2_SPTR16, size_t, size_t, uint32_t, pcre2_match_data_16 *, pcre2_match_context_16 *);
extern void pcre2_jit_free_unused_memory_16(pcre2_general_context_16 *);
struct pcre2_real_jit_stack_16;
typedef struct pcre2_real_jit_stack_16 pcre2_jit_stack_16;
extern pcre2_jit_stack_16 * pcre2_jit_stack_create_16(size_t, size_t, pcre2_general_context_16 *);
typedef void *pcre2_jit_callback_16;
extern void pcre2_jit_stack_assign_16(pcre2_match_context_16 *, pcre2_jit_callback_16, void *);
extern void pcre2_jit_stack_free_16(pcre2_jit_stack_16 *);
extern int pcre2_get_error_message_16(int, PCRE2_UCHAR16 *, size_t);
extern const uint8_t * pcre2_maketables_16(pcre2_general_context_16 *);
extern int pcre2_config_32(uint32_t, void *);
struct pcre2_real_general_context_32;
typedef struct pcre2_real_general_context_32 pcre2_general_context_32;
extern pcre2_general_context_32 * pcre2_general_context_copy_32(pcre2_general_context_32 *);
extern pcre2_general_context_32 * pcre2_general_context_create_32(void *(*)(size_t, void *), void (*)(void *, void *), void *);
extern void pcre2_general_context_free_32(pcre2_general_context_32 *);
struct pcre2_real_compile_context_32;
typedef struct pcre2_real_compile_context_32 pcre2_compile_context_32;
extern pcre2_compile_context_32 * pcre2_compile_context_copy_32(pcre2_compile_context_32 *);
extern pcre2_compile_context_32 * pcre2_compile_context_create_32(pcre2_general_context_32 *);
extern void pcre2_compile_context_free_32(pcre2_compile_context_32 *);
extern int pcre2_set_bsr_32(pcre2_compile_context_32 *, uint32_t);
extern int pcre2_set_character_tables_32(pcre2_compile_context_32 *, const unsigned char *);
extern int pcre2_set_compile_extra_options_32(pcre2_compile_context_32 *, uint32_t);
extern int pcre2_set_max_pattern_length_32(pcre2_compile_context_32 *, size_t);
extern int pcre2_set_newline_32(pcre2_compile_context_32 *, uint32_t);
extern int pcre2_set_parens_nest_limit_32(pcre2_compile_context_32 *, uint32_t);
extern int pcre2_set_compile_recursion_guard_32(pcre2_compile_context_32 *, int (*)(uint32_t, void *), void *);
struct pcre2_real_convert_context_32;
typedef struct pcre2_real_convert_context_32 pcre2_convert_context_32;
extern pcre2_convert_context_32 * pcre2_convert_context_copy_32(pcre2_convert_context_32 *);
extern pcre2_convert_context_32 * pcre2_convert_context_create_32(pcre2_general_context_32 *);
extern void pcre2_convert_context_free_32(pcre2_convert_context_32 *);
extern int pcre2_set_glob_escape_32(pcre2_convert_context_32 *, uint32_t);
extern int pcre2_set_glob_separator_32(pcre2_convert_context_32 *, uint32_t);
typedef unsigned int PCRE2_UCHAR32;
typedef const PCRE2_UCHAR32 * PCRE2_SPTR32;
extern int pcre2_pattern_convert_32(PCRE2_SPTR32, size_t, uint32_t, PCRE2_UCHAR32 **, size_t *, pcre2_convert_context_32 *);
extern void pcre2_converted_pattern_free_32(PCRE2_UCHAR32 *);
struct pcre2_real_match_context_32;
typedef struct pcre2_real_match_context_32 pcre2_match_context_32;
extern pcre2_match_context_32 * pcre2_match_context_copy_32(pcre2_match_context_32 *);
extern pcre2_match_context_32 * pcre2_match_context_create_32(pcre2_general_context_32 *);
extern void pcre2_match_context_free_32(pcre2_match_context_32 *);
extern int pcre2_set_depth_limit_32(pcre2_match_context_32 *, uint32_t);
extern int pcre2_set_heap_limit_32(pcre2_match_context_32 *, uint32_t);
extern int pcre2_set_match_limit_32(pcre2_match_context_32 *, uint32_t);
extern int pcre2_set_offset_limit_32(pcre2_match_context_32 *, size_t);
extern int pcre2_set_recursion_limit_32(pcre2_match_context_32 *, uint32_t);
extern int pcre2_set_recursion_memory_management_32(pcre2_match_context_32 *, void *(*)(size_t, void *), void (*)(void *, void *), void *);
struct pcre2_real_code_32;
typedef struct pcre2_real_code_32 pcre2_code_32;
extern pcre2_code_32 * pcre2_compile_32(PCRE2_SPTR32, size_t, uint32_t, int *, size_t *, pcre2_compile_context_32 *);
extern void pcre2_code_free_32(pcre2_code_32 *);
extern pcre2_code_32 * pcre2_code_copy_32(const pcre2_code_32 *);
extern pcre2_code_32 * pcre2_code_copy_with_tables_32(const pcre2_code_32 *);
extern int pcre2_pattern_info_32(const pcre2_code_32 *, uint32_t, void *);
struct pcre2_real_match_data_32;
typedef struct pcre2_real_match_data_32 pcre2_match_data_32;
extern pcre2_match_data_32 * pcre2_match_data_create_32(uint32_t, pcre2_general_context_32 *);
extern pcre2_match_data_32 * pcre2_match_data_create_from_pattern_32(const pcre2_code_32 *, pcre2_general_context_32 *);
extern int pcre2_dfa_match_32(const pcre2_code_32 *, PCRE2_SPTR32, size_t, size_t, uint32_t, pcre2_match_data_32 *, pcre2_match_context_32 *, int *, size_t);
extern int pcre2_match_32(const pcre2_code_32 *, PCRE2_SPTR32, size_t, size_t, uint32_t, pcre2_match_data_32 *, pcre2_match_context_32 *);
extern void pcre2_match_data_free_32(pcre2_match_data_32 *);
extern PCRE2_SPTR32 pcre2_get_mark_32(pcre2_match_data_32 *);
extern uint32_t pcre2_get_ovector_count_32(pcre2_match_data_32 *);
extern size_t * pcre2_get_ovector_pointer_32(pcre2_match_data_32 *);
extern size_t pcre2_get_startchar_32(pcre2_match_data_32 *);
extern int pcre2_substring_copy_byname_32(pcre2_match_data_32 *, PCRE2_SPTR32, PCRE2_UCHAR32 *, size_t *);
extern int pcre2_substring_copy_bynumber_32(pcre2_match_data_32 *, uint32_t, PCRE2_UCHAR32 *, size_t *);
extern void pcre2_substring_free_32(PCRE2_UCHAR32 *);
extern int pcre2_substring_get_byname_32(pcre2_match_data_32 *, PCRE2_SPTR32, PCRE2_UCHAR32 **, size_t *);
extern int pcre2_substring_get_bynumber_32(pcre2_match_data_32 *, uint32_t, PCRE2_UCHAR32 **, size_t *);
extern int pcre2_substring_length_byname_32(pcre2_match_data_32 *, PCRE2_SPTR32, size_t *);
extern int pcre2_substring_length_bynumber_32(pcre2_match_data_32 *, uint32_t, size_t *);
extern int pcre2_substring_nametable_scan_32(const pcre2_code_32 *, PCRE2_SPTR32, PCRE2_SPTR32 *, PCRE2_SPTR32 *);
extern int pcre2_substring_number_from_name_32(const pcre2_code_32 *, PCRE2_SPTR32);
extern void pcre2_substring_list_free_32(PCRE2_SPTR32 *);
extern int pcre2_substring_list_get_32(pcre2_match_data_32 *, PCRE2_UCHAR32 ***, size_t **);
extern int32_t pcre2_serialize_encode_32(const pcre2_code_32 **, int32_t, uint8_t **, size_t *, pcre2_general_context_32 *);
extern int32_t pcre2_serialize_decode_32(pcre2_code_32 **, int32_t, const uint8_t *, pcre2_general_context_32 *);
extern int32_t pcre2_serialize_get_number_of_codes_32(const uint8_t *);
extern void pcre2_serialize_free_32(uint8_t *);
extern int pcre2_substitute_32(const pcre2_code_32 *, PCRE2_SPTR32, size_t, size_t, uint32_t, pcre2_match_data_32 *, pcre2_match_context_32 *, PCRE2_SPTR32, size_t, PCRE2_UCHAR32 *, size_t *);
extern int pcre2_jit_compile_32(pcre2_code_32 *, uint32_t);
extern int pcre2_jit_match_32(const pcre2_code_32 *, PCRE2_SPTR32, size_t, size_t, uint32_t, pcre2_match_data_32 *, pcre2_match_context_32 *);
extern void pcre2_jit_free_unused_memory_32(pcre2_general_context_32 *);
struct pcre2_real_jit_stack_32;
typedef struct pcre2_real_jit_stack_32 pcre2_jit_stack_32;
extern pcre2_jit_stack_32 * pcre2_jit_stack_create_32(size_t, size_t, pcre2_general_context_32 *);
typedef void *pcre2_jit_callback_32;
extern void pcre2_jit_stack_assign_32(pcre2_match_context_32 *, pcre2_jit_callback_32, void *);
extern void pcre2_jit_stack_free_32(pcre2_jit_stack_32 *);
extern int pcre2_get_error_message_32(int, PCRE2_UCHAR32 *, size_t);
extern const uint8_t * pcre2_maketables_32(pcre2_general_context_32 *);
extern const char * utf8proc_version(void);
extern const char * utf8proc_unicode_version(void);
typedef long utf8proc_ssize_t;
extern const char * utf8proc_errmsg(utf8proc_ssize_t);
typedef unsigned char utf8proc_uint8_t;
typedef int utf8proc_int32_t;
extern utf8proc_ssize_t utf8proc_iterate(const utf8proc_uint8_t *, utf8proc_ssize_t, utf8proc_int32_t *);
typedef _Bool utf8proc_bool;
extern utf8proc_bool utf8proc_codepoint_valid(utf8proc_int32_t);
extern utf8proc_ssize_t utf8proc_encode_char(utf8proc_int32_t, utf8proc_uint8_t *);
struct utf8proc_property_struct;
typedef struct utf8proc_property_struct utf8proc_property_t;
extern const utf8proc_property_t * utf8proc_get_property(utf8proc_int32_t);
typedef int utf8proc_option_t;
extern utf8proc_ssize_t utf8proc_decompose_char(utf8proc_int32_t, utf8proc_int32_t *, utf8proc_ssize_t, utf8proc_option_t, int *);
typedef int utf8proc_option_t;
extern utf8proc_ssize_t utf8proc_decompose(const utf8proc_uint8_t *, utf8proc_ssize_t, utf8proc_int32_t *, utf8proc_ssize_t, utf8proc_option_t);
typedef int utf8proc_option_t;
typedef void *utf8proc_custom_func;
extern utf8proc_ssize_t utf8proc_decompose_custom(const utf8proc_uint8_t *, utf8proc_ssize_t, utf8proc_int32_t *, utf8proc_ssize_t, utf8proc_option_t, utf8proc_custom_func, void *);
typedef int utf8proc_option_t;
extern utf8proc_ssize_t utf8proc_normalize_utf32(utf8proc_int32_t *, utf8proc_ssize_t, utf8proc_option_t);
typedef int utf8proc_option_t;
extern utf8proc_ssize_t utf8proc_reencode(utf8proc_int32_t *, utf8proc_ssize_t, utf8proc_option_t);
extern utf8proc_bool utf8proc_grapheme_break_stateful(utf8proc_int32_t, utf8proc_int32_t, utf8proc_int32_t *);
extern utf8proc_bool utf8proc_grapheme_break(utf8proc_int32_t, utf8proc_int32_t);
extern utf8proc_int32_t utf8proc_tolower(utf8proc_int32_t);
extern utf8proc_int32_t utf8proc_toupper(utf8proc_int32_t);
extern utf8proc_int32_t utf8proc_totitle(utf8proc_int32_t);
extern int utf8proc_charwidth(utf8proc_int32_t);
typedef int utf8proc_category_t;
extern utf8proc_category_t utf8proc_category(utf8proc_int32_t);
extern const char * utf8proc_category_string(utf8proc_int32_t);
typedef int utf8proc_option_t;
extern utf8proc_ssize_t utf8proc_map(const utf8proc_uint8_t *, utf8proc_ssize_t, utf8proc_uint8_t **, utf8proc_option_t);
typedef int utf8proc_option_t;
typedef void *utf8proc_custom_func;
extern utf8proc_ssize_t utf8proc_map_custom(const utf8proc_uint8_t *, utf8proc_ssize_t, utf8proc_uint8_t **, utf8proc_option_t, utf8proc_custom_func, void *);
extern utf8proc_uint8_t * utf8proc_NFD(const utf8proc_uint8_t *);
extern utf8proc_uint8_t * utf8proc_NFC(const utf8proc_uint8_t *);
extern utf8proc_uint8_t * utf8proc_NFKD(const utf8proc_uint8_t *);
extern utf8proc_uint8_t * utf8proc_NFKC(const utf8proc_uint8_t *);
extern utf8proc_uint8_t * utf8proc_NFKC_Casefold(const utf8proc_uint8_t *);
extern void __gmp_set_memory_functions(void *(*)(size_t), void *(*)(void *, size_t, size_t), void (*)(void *, size_t));
extern void __gmp_get_memory_functions(void *(**)(size_t), void *(**)(void *, size_t, size_t), void (**)(void *, size_t));
extern void __gmp_set_alloc_overflow_function(void (*)(void));
extern void __gmp_get_alloc_overflow_function(void (**)(void));
struct __gmp_randstate_struct;
typedef struct __gmp_randstate_struct __gmp_randstate_struct;
typedef void *gmp_randstate_t;
typedef int gmp_randalg_t;
extern void __gmp_randinit(__gmp_randstate_struct *, gmp_randalg_t, ...);
typedef void *gmp_randstate_t;
extern void __gmp_randinit_default(__gmp_randstate_struct *);
typedef void *gmp_randstate_t;
struct __mpz_struct;
typedef struct __mpz_struct __mpz_struct;
typedef const __mpz_struct * mpz_srcptr;
typedef unsigned long mp_bitcnt_t;
extern void __gmp_randinit_lc_2exp(__gmp_randstate_struct *, mpz_srcptr, unsigned long, mp_bitcnt_t);
typedef void *gmp_randstate_t;
extern int __gmp_randinit_lc_2exp_size(__gmp_randstate_struct *, mp_bitcnt_t);
typedef void *gmp_randstate_t;
extern void __gmp_randinit_mt(__gmp_randstate_struct *);
typedef void *gmp_randstate_t;
extern void __gmp_randinit_set(__gmp_randstate_struct *, const __gmp_randstate_struct *);
typedef void *gmp_randstate_t;
extern void __gmp_randseed(__gmp_randstate_struct *, mpz_srcptr);
typedef void *gmp_randstate_t;
extern void __gmp_randseed_ui(__gmp_randstate_struct *, unsigned long);
typedef void *gmp_randstate_t;
extern void __gmp_randclear(__gmp_randstate_struct *);
typedef void *gmp_randstate_t;
extern unsigned long __gmp_urandomb_ui(__gmp_randstate_struct *, unsigned long);
typedef void *gmp_randstate_t;
extern unsigned long __gmp_urandomm_ui(__gmp_randstate_struct *, unsigned long);
extern int __gmp_asprintf(char **, const char *, ...);
extern int __gmp_printf(const char *, ...);
extern int __gmp_snprintf(char *, size_t, const char *, ...);
extern int __gmp_sprintf(char *, const char *, ...);
extern int __gmp_scanf(const char *, ...);
extern int __gmp_sscanf(const char *, const char *, ...);
typedef __mpz_struct * mpz_ptr;
typedef long mp_size_t;
extern void * __gmpz_realloc(mpz_ptr, mp_size_t);
extern void __gmpz_abs(mpz_ptr, mpz_srcptr);
extern void __gmpz_add(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_add_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_addmul(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_addmul_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_and(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_array_init(mpz_ptr, mp_size_t, mp_size_t);
extern void __gmpz_bin_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_bin_uiui(mpz_ptr, unsigned long, unsigned long);
extern void __gmpz_cdiv_q(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_cdiv_q_2exp(mpz_ptr, mpz_srcptr, mp_bitcnt_t);
extern unsigned long __gmpz_cdiv_q_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_cdiv_qr(mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);
extern unsigned long __gmpz_cdiv_qr_ui(mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_cdiv_r(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_cdiv_r_2exp(mpz_ptr, mpz_srcptr, mp_bitcnt_t);
extern unsigned long __gmpz_cdiv_r_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern unsigned long __gmpz_cdiv_ui(mpz_srcptr, unsigned long);
extern void __gmpz_clear(mpz_ptr);
extern void __gmpz_clears(mpz_ptr, ...);
extern void __gmpz_clrbit(mpz_ptr, mp_bitcnt_t);
extern int __gmpz_cmp(mpz_srcptr, mpz_srcptr);
extern int __gmpz_cmp_d(mpz_srcptr, double);
extern int __gmpz_cmp_si(mpz_srcptr, long);
extern int __gmpz_cmp_ui(mpz_srcptr, unsigned long);
extern int __gmpz_cmpabs(mpz_srcptr, mpz_srcptr);
extern int __gmpz_cmpabs_d(mpz_srcptr, double);
extern int __gmpz_cmpabs_ui(mpz_srcptr, unsigned long);
extern void __gmpz_com(mpz_ptr, mpz_srcptr);
extern void __gmpz_combit(mpz_ptr, mp_bitcnt_t);
extern int __gmpz_congruent_p(mpz_srcptr, mpz_srcptr, mpz_srcptr);
extern int __gmpz_congruent_2exp_p(mpz_srcptr, mpz_srcptr, mp_bitcnt_t);
extern int __gmpz_congruent_ui_p(mpz_srcptr, unsigned long, unsigned long);
extern void __gmpz_divexact(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_divexact_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern int __gmpz_divisible_p(mpz_srcptr, mpz_srcptr);
extern int __gmpz_divisible_ui_p(mpz_srcptr, unsigned long);
extern int __gmpz_divisible_2exp_p(mpz_srcptr, mp_bitcnt_t);
extern void __gmpz_dump(mpz_srcptr);
extern void * __gmpz_export(void *, size_t *, int, size_t, int, size_t, mpz_srcptr);
extern void __gmpz_fac_ui(mpz_ptr, unsigned long);
extern void __gmpz_2fac_ui(mpz_ptr, unsigned long);
extern void __gmpz_mfac_uiui(mpz_ptr, unsigned long, unsigned long);
extern void __gmpz_primorial_ui(mpz_ptr, unsigned long);
extern void __gmpz_fdiv_q(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_fdiv_q_2exp(mpz_ptr, mpz_srcptr, mp_bitcnt_t);
extern unsigned long __gmpz_fdiv_q_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_fdiv_qr(mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);
extern unsigned long __gmpz_fdiv_qr_ui(mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_fdiv_r(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_fdiv_r_2exp(mpz_ptr, mpz_srcptr, mp_bitcnt_t);
extern unsigned long __gmpz_fdiv_r_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern unsigned long __gmpz_fdiv_ui(mpz_srcptr, unsigned long);
extern void __gmpz_fib_ui(mpz_ptr, unsigned long);
extern void __gmpz_fib2_ui(mpz_ptr, mpz_ptr, unsigned long);
extern int __gmpz_fits_sint_p(mpz_srcptr);
extern int __gmpz_fits_slong_p(mpz_srcptr);
extern int __gmpz_fits_sshort_p(mpz_srcptr);
extern int __gmpz_fits_uint_p(mpz_srcptr);
extern int __gmpz_fits_ulong_p(mpz_srcptr);
extern int __gmpz_fits_ushort_p(mpz_srcptr);
extern void __gmpz_gcd(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern unsigned long __gmpz_gcd_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_gcdext(mpz_ptr, mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);
extern double __gmpz_get_d(mpz_srcptr);
extern double __gmpz_get_d_2exp(long *, mpz_srcptr);
extern long __gmpz_get_si(mpz_srcptr);
extern char * __gmpz_get_str(char *, int, mpz_srcptr);
extern unsigned long __gmpz_get_ui(mpz_srcptr);
typedef unsigned long mp_limb_t;
extern mp_limb_t __gmpz_getlimbn(mpz_srcptr, mp_size_t);
extern mp_bitcnt_t __gmpz_hamdist(mpz_srcptr, mpz_srcptr);
extern void __gmpz_import(mpz_ptr, size_t, int, size_t, int, size_t, const void *);
extern void __gmpz_init(mpz_ptr);
extern void __gmpz_init2(mpz_ptr, mp_bitcnt_t);
extern void __gmpz_inits(mpz_ptr, ...);
extern void __gmpz_init_set(mpz_ptr, mpz_srcptr);
extern void __gmpz_init_set_d(mpz_ptr, double);
extern void __gmpz_init_set_si(mpz_ptr, long);
extern int __gmpz_init_set_str(mpz_ptr, const char *, int);
extern void __gmpz_init_set_ui(mpz_ptr, unsigned long);
extern int __gmpz_invert(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_ior(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern int __gmpz_jacobi(mpz_srcptr, mpz_srcptr);
extern int __gmpz_kronecker_si(mpz_srcptr, long);
extern int __gmpz_kronecker_ui(mpz_srcptr, unsigned long);
extern int __gmpz_si_kronecker(long, mpz_srcptr);
extern int __gmpz_ui_kronecker(unsigned long, mpz_srcptr);
extern void __gmpz_lcm(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_lcm_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_lucnum_ui(mpz_ptr, unsigned long);
extern void __gmpz_lucnum2_ui(mpz_ptr, mpz_ptr, unsigned long);
extern int __gmpz_millerrabin(mpz_srcptr, int);
extern void __gmpz_mod(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_mul(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_mul_2exp(mpz_ptr, mpz_srcptr, mp_bitcnt_t);
extern void __gmpz_mul_si(mpz_ptr, mpz_srcptr, long);
extern void __gmpz_mul_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_neg(mpz_ptr, mpz_srcptr);
extern void __gmpz_nextprime(mpz_ptr, mpz_srcptr);
extern int __gmpz_perfect_power_p(mpz_srcptr);
extern int __gmpz_perfect_square_p(mpz_srcptr);
extern mp_bitcnt_t __gmpz_popcount(mpz_srcptr);
extern void __gmpz_pow_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_powm(mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_powm_sec(mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_powm_ui(mpz_ptr, mpz_srcptr, unsigned long, mpz_srcptr);
extern int __gmpz_probab_prime_p(mpz_srcptr, int);
extern void __gmpz_random(mpz_ptr, mp_size_t);
extern void __gmpz_random2(mpz_ptr, mp_size_t);
extern void __gmpz_realloc2(mpz_ptr, mp_bitcnt_t);
extern mp_bitcnt_t __gmpz_remove(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern int __gmpz_root(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_rootrem(mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long);
typedef void *gmp_randstate_t;
extern void __gmpz_rrandomb(mpz_ptr, __gmp_randstate_struct *, mp_bitcnt_t);
extern mp_bitcnt_t __gmpz_scan0(mpz_srcptr, mp_bitcnt_t);
extern mp_bitcnt_t __gmpz_scan1(mpz_srcptr, mp_bitcnt_t);
extern void __gmpz_set(mpz_ptr, mpz_srcptr);
extern void __gmpz_set_d(mpz_ptr, double);
struct __mpf_struct;
typedef struct __mpf_struct __mpf_struct;
typedef const __mpf_struct * mpf_srcptr;
extern void __gmpz_set_f(mpz_ptr, mpf_srcptr);
struct __mpq_struct;
typedef struct __mpq_struct __mpq_struct;
typedef const __mpq_struct * mpq_srcptr;
extern void __gmpz_set_q(mpz_ptr, mpq_srcptr);
extern void __gmpz_set_si(mpz_ptr, long);
extern int __gmpz_set_str(mpz_ptr, const char *, int);
extern void __gmpz_set_ui(mpz_ptr, unsigned long);
extern void __gmpz_setbit(mpz_ptr, mp_bitcnt_t);
extern size_t __gmpz_size(mpz_srcptr);
extern size_t __gmpz_sizeinbase(mpz_srcptr, int);
extern void __gmpz_sqrt(mpz_ptr, mpz_srcptr);
extern void __gmpz_sqrtrem(mpz_ptr, mpz_ptr, mpz_srcptr);
extern void __gmpz_sub(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_sub_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_ui_sub(mpz_ptr, unsigned long, mpz_srcptr);
extern void __gmpz_submul(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_submul_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_swap(mpz_ptr, mpz_ptr);
extern unsigned long __gmpz_tdiv_ui(mpz_srcptr, unsigned long);
extern void __gmpz_tdiv_q(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_tdiv_q_2exp(mpz_ptr, mpz_srcptr, mp_bitcnt_t);
extern unsigned long __gmpz_tdiv_q_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_tdiv_qr(mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);
extern unsigned long __gmpz_tdiv_qr_ui(mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long);
extern void __gmpz_tdiv_r(mpz_ptr, mpz_srcptr, mpz_srcptr);
extern void __gmpz_tdiv_r_2exp(mpz_ptr, mpz_srcptr, mp_bitcnt_t);
extern unsigned long __gmpz_tdiv_r_ui(mpz_ptr, mpz_srcptr, unsigned long);
extern int __gmpz_tstbit(mpz_srcptr, mp_bitcnt_t);
extern void __gmpz_ui_pow_ui(mpz_ptr, unsigned long, unsigned long);
typedef void *gmp_randstate_t;
extern void __gmpz_urandomb(mpz_ptr, __gmp_randstate_struct *, mp_bitcnt_t);
typedef void *gmp_randstate_t;
extern void __gmpz_urandomm(mpz_ptr, __gmp_randstate_struct *, mpz_srcptr);
extern void __gmpz_xor(mpz_ptr, mpz_srcptr, mpz_srcptr);
typedef const mp_limb_t * mp_srcptr;
extern mp_srcptr __gmpz_limbs_read(mpz_srcptr);
typedef mp_limb_t * mp_ptr;
extern mp_ptr __gmpz_limbs_write(mpz_ptr, mp_size_t);
extern mp_ptr __gmpz_limbs_modify(mpz_ptr, mp_size_t);
extern void __gmpz_limbs_finish(mpz_ptr, mp_size_t);
extern mpz_srcptr __gmpz_roinit_n(mpz_ptr, mp_srcptr, mp_size_t);
typedef __mpq_struct * mpq_ptr;
extern void __gmpq_abs(mpq_ptr, mpq_srcptr);
extern void __gmpq_add(mpq_ptr, mpq_srcptr, mpq_srcptr);
extern void __gmpq_canonicalize(mpq_ptr);
extern void __gmpq_clear(mpq_ptr);
extern void __gmpq_clears(mpq_ptr, ...);
extern int __gmpq_cmp(mpq_srcptr, mpq_srcptr);
extern int __gmpq_cmp_si(mpq_srcptr, long, unsigned long);
extern int __gmpq_cmp_ui(mpq_srcptr, unsigned long, unsigned long);
extern int __gmpq_cmp_z(mpq_srcptr, mpz_srcptr);
extern void __gmpq_div(mpq_ptr, mpq_srcptr, mpq_srcptr);
extern void __gmpq_div_2exp(mpq_ptr, mpq_srcptr, mp_bitcnt_t);
extern int __gmpq_equal(mpq_srcptr, mpq_srcptr);
extern void __gmpq_get_num(mpz_ptr, mpq_srcptr);
extern void __gmpq_get_den(mpz_ptr, mpq_srcptr);
extern double __gmpq_get_d(mpq_srcptr);
extern char * __gmpq_get_str(char *, int, mpq_srcptr);
extern void __gmpq_init(mpq_ptr);
extern void __gmpq_inits(mpq_ptr, ...);
extern void __gmpq_inv(mpq_ptr, mpq_srcptr);
extern void __gmpq_mul(mpq_ptr, mpq_srcptr, mpq_srcptr);
extern void __gmpq_mul_2exp(mpq_ptr, mpq_srcptr, mp_bitcnt_t);
extern void __gmpq_neg(mpq_ptr, mpq_srcptr);
extern void __gmpq_set(mpq_ptr, mpq_srcptr);
extern void __gmpq_set_d(mpq_ptr, double);
extern void __gmpq_set_den(mpq_ptr, mpz_srcptr);
extern void __gmpq_set_f(mpq_ptr, mpf_srcptr);
extern void __gmpq_set_num(mpq_ptr, mpz_srcptr);
extern void __gmpq_set_si(mpq_ptr, long, unsigned long);
extern int __gmpq_set_str(mpq_ptr, const char *, int);
extern void __gmpq_set_ui(mpq_ptr, unsigned long, unsigned long);
extern void __gmpq_set_z(mpq_ptr, mpz_srcptr);
extern void __gmpq_sub(mpq_ptr, mpq_srcptr, mpq_srcptr);
extern void __gmpq_swap(mpq_ptr, mpq_ptr);
typedef __mpf_struct * mpf_ptr;
extern void __gmpf_abs(mpf_ptr, mpf_srcptr);
extern void __gmpf_add(mpf_ptr, mpf_srcptr, mpf_srcptr);
extern void __gmpf_add_ui(mpf_ptr, mpf_srcptr, unsigned long);
extern void __gmpf_ceil(mpf_ptr, mpf_srcptr);
extern void __gmpf_clear(mpf_ptr);
extern void __gmpf_clears(mpf_ptr, ...);
extern int __gmpf_cmp(mpf_srcptr, mpf_srcptr);
extern int __gmpf_cmp_z(mpf_srcptr, mpz_srcptr);
extern int __gmpf_cmp_d(mpf_srcptr, double);
extern int __gmpf_cmp_si(mpf_srcptr, long);
extern int __gmpf_cmp_ui(mpf_srcptr, unsigned long);
extern void __gmpf_div(mpf_ptr, mpf_srcptr, mpf_srcptr);
extern void __gmpf_div_2exp(mpf_ptr, mpf_srcptr, mp_bitcnt_t);
extern void __gmpf_div_ui(mpf_ptr, mpf_srcptr, unsigned long);
extern void __gmpf_dump(mpf_srcptr);
extern int __gmpf_eq(mpf_srcptr, mpf_srcptr, mp_bitcnt_t);
extern int __gmpf_fits_sint_p(mpf_srcptr);
extern int __gmpf_fits_slong_p(mpf_srcptr);
extern int __gmpf_fits_sshort_p(mpf_srcptr);
extern int __gmpf_fits_uint_p(mpf_srcptr);
extern int __gmpf_fits_ulong_p(mpf_srcptr);
extern int __gmpf_fits_ushort_p(mpf_srcptr);
extern void __gmpf_floor(mpf_ptr, mpf_srcptr);
extern double __gmpf_get_d(mpf_srcptr);
extern double __gmpf_get_d_2exp(long *, mpf_srcptr);
extern mp_bitcnt_t __gmpf_get_default_prec(void);
extern mp_bitcnt_t __gmpf_get_prec(mpf_srcptr);
extern long __gmpf_get_si(mpf_srcptr);
typedef long mp_exp_t;
extern char * __gmpf_get_str(char *, mp_exp_t *, int, size_t, mpf_srcptr);
extern unsigned long __gmpf_get_ui(mpf_srcptr);
extern void __gmpf_init(mpf_ptr);
extern void __gmpf_init2(mpf_ptr, mp_bitcnt_t);
extern void __gmpf_inits(mpf_ptr, ...);
extern void __gmpf_init_set(mpf_ptr, mpf_srcptr);
extern void __gmpf_init_set_d(mpf_ptr, double);
extern void __gmpf_init_set_si(mpf_ptr, long);
extern int __gmpf_init_set_str(mpf_ptr, const char *, int);
extern void __gmpf_init_set_ui(mpf_ptr, unsigned long);
extern int __gmpf_integer_p(mpf_srcptr);
extern void __gmpf_mul(mpf_ptr, mpf_srcptr, mpf_srcptr);
extern void __gmpf_mul_2exp(mpf_ptr, mpf_srcptr, mp_bitcnt_t);
extern void __gmpf_mul_ui(mpf_ptr, mpf_srcptr, unsigned long);
extern void __gmpf_neg(mpf_ptr, mpf_srcptr);
extern void __gmpf_pow_ui(mpf_ptr, mpf_srcptr, unsigned long);
extern void __gmpf_random2(mpf_ptr, mp_size_t, mp_exp_t);
extern void __gmpf_reldiff(mpf_ptr, mpf_srcptr, mpf_srcptr);
extern void __gmpf_set(mpf_ptr, mpf_srcptr);
extern void __gmpf_set_d(mpf_ptr, double);
extern void __gmpf_set_default_prec(mp_bitcnt_t);
extern void __gmpf_set_prec(mpf_ptr, mp_bitcnt_t);
extern void __gmpf_set_prec_raw(mpf_ptr, mp_bitcnt_t);
extern void __gmpf_set_q(mpf_ptr, mpq_srcptr);
extern void __gmpf_set_si(mpf_ptr, long);
extern int __gmpf_set_str(mpf_ptr, const char *, int);
extern void __gmpf_set_ui(mpf_ptr, unsigned long);
extern void __gmpf_set_z(mpf_ptr, mpz_srcptr);
extern size_t __gmpf_size(mpf_srcptr);
extern void __gmpf_sqrt(mpf_ptr, mpf_srcptr);
extern void __gmpf_sqrt_ui(mpf_ptr, unsigned long);
extern void __gmpf_sub(mpf_ptr, mpf_srcptr, mpf_srcptr);
extern void __gmpf_sub_ui(mpf_ptr, mpf_srcptr, unsigned long);
extern void __gmpf_swap(mpf_ptr, mpf_ptr);
extern void __gmpf_trunc(mpf_ptr, mpf_srcptr);
extern void __gmpf_ui_div(mpf_ptr, unsigned long, mpf_srcptr);
extern void __gmpf_ui_sub(mpf_ptr, unsigned long, mpf_srcptr);
typedef void *mpf_t;
typedef void *gmp_randstate_t;
extern void __gmpf_urandomb(__mpf_struct *, __gmp_randstate_struct *, mp_bitcnt_t);
extern mp_limb_t __gmpn_add(mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_add_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
extern mp_limb_t __gmpn_add_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_addmul_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
extern int __gmpn_cmp(mp_srcptr, mp_srcptr, mp_size_t);
extern int __gmpn_zero_p(mp_srcptr, mp_size_t);
extern void __gmpn_divexact_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
extern mp_limb_t __gmpn_divexact_by3c(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
extern mp_limb_t __gmpn_divrem(mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_divrem_1(mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t);
extern mp_limb_t __gmpn_divrem_2(mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr);
extern mp_limb_t __gmpn_div_qr_1(mp_ptr, mp_limb_t *, mp_srcptr, mp_size_t, mp_limb_t);
extern mp_limb_t __gmpn_div_qr_2(mp_ptr, mp_ptr, mp_srcptr, mp_size_t, mp_srcptr);
extern mp_size_t __gmpn_gcd(mp_ptr, mp_ptr, mp_size_t, mp_ptr, mp_size_t);
extern mp_limb_t __gmpn_gcd_1(mp_srcptr, mp_size_t, mp_limb_t);
typedef long mp_limb_signed_t;
extern mp_limb_t __gmpn_gcdext_1(mp_limb_signed_t *, mp_limb_signed_t *, mp_limb_t, mp_limb_t);
extern mp_size_t __gmpn_gcdext(mp_ptr, mp_ptr, mp_size_t *, mp_ptr, mp_size_t, mp_ptr, mp_size_t);
extern size_t __gmpn_get_str(unsigned char *, int, mp_ptr, mp_size_t);
extern mp_bitcnt_t __gmpn_hamdist(mp_srcptr, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_lshift(mp_ptr, mp_srcptr, mp_size_t, unsigned int);
extern mp_limb_t __gmpn_mod_1(mp_srcptr, mp_size_t, mp_limb_t);
extern mp_limb_t __gmpn_mul(mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_mul_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
extern void __gmpn_mul_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_sqr(mp_ptr, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_neg(mp_ptr, mp_srcptr, mp_size_t);
extern void __gmpn_com(mp_ptr, mp_srcptr, mp_size_t);
extern int __gmpn_perfect_square_p(mp_srcptr, mp_size_t);
extern int __gmpn_perfect_power_p(mp_srcptr, mp_size_t);
extern mp_bitcnt_t __gmpn_popcount(mp_srcptr, mp_size_t);
extern mp_size_t __gmpn_pow_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_ptr);
extern mp_limb_t __gmpn_preinv_mod_1(mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t);
extern void __gmpn_random(mp_ptr, mp_size_t);
extern void __gmpn_random2(mp_ptr, mp_size_t);
extern mp_limb_t __gmpn_rshift(mp_ptr, mp_srcptr, mp_size_t, unsigned int);
extern mp_bitcnt_t __gmpn_scan0(mp_srcptr, mp_bitcnt_t);
extern mp_bitcnt_t __gmpn_scan1(mp_srcptr, mp_bitcnt_t);
extern mp_size_t __gmpn_set_str(mp_ptr, const unsigned char *, size_t, int);
extern size_t __gmpn_sizeinbase(mp_srcptr, mp_size_t, int);
extern mp_size_t __gmpn_sqrtrem(mp_ptr, mp_ptr, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_sub(mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_sub_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
extern mp_limb_t __gmpn_sub_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_submul_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);
extern void __gmpn_tdiv_qr(mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);
extern void __gmpn_and_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_andn_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_nand_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_ior_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_iorn_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_nior_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_xor_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_xnor_n(mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern void __gmpn_copyi(mp_ptr, mp_srcptr, mp_size_t);
extern void __gmpn_copyd(mp_ptr, mp_srcptr, mp_size_t);
extern void __gmpn_zero(mp_ptr, mp_size_t);
extern mp_limb_t __gmpn_cnd_add_n(mp_limb_t, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_cnd_sub_n(mp_limb_t, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);
extern mp_limb_t __gmpn_sec_add_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_ptr);
extern mp_size_t __gmpn_sec_add_1_itch(mp_size_t);
extern mp_limb_t __gmpn_sec_sub_1(mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_ptr);
extern mp_size_t __gmpn_sec_sub_1_itch(mp_size_t);
extern void __gmpn_cnd_swap(mp_limb_t, volatile mp_limb_t *, volatile mp_limb_t *, mp_size_t);
extern void __gmpn_sec_mul(mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t, mp_ptr);
extern mp_size_t __gmpn_sec_mul_itch(mp_size_t, mp_size_t);
extern void __gmpn_sec_sqr(mp_ptr, mp_srcptr, mp_size_t, mp_ptr);
extern mp_size_t __gmpn_sec_sqr_itch(mp_size_t);
extern void __gmpn_sec_powm(mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_bitcnt_t, mp_srcptr, mp_size_t, mp_ptr);
extern mp_size_t __gmpn_sec_powm_itch(mp_size_t, mp_bitcnt_t, mp_size_t);
extern void __gmpn_sec_tabselect(volatile mp_limb_t *, const volatile mp_limb_t *, mp_size_t, mp_size_t, mp_size_t);
extern mp_limb_t __gmpn_sec_div_qr(mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_ptr);
extern mp_size_t __gmpn_sec_div_qr_itch(mp_size_t, mp_size_t);
extern void __gmpn_sec_div_r(mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_ptr);
extern mp_size_t __gmpn_sec_div_r_itch(mp_size_t, mp_size_t);
extern int __gmpn_sec_invert(mp_ptr, mp_ptr, mp_srcptr, mp_size_t, mp_bitcnt_t, mp_ptr);
extern mp_size_t __gmpn_sec_invert_itch(mp_size_t);
extern const char * mpfr_get_version(void);
extern const char * mpfr_get_patches(void);
extern int mpfr_buildopt_tls_p(void);
extern int mpfr_buildopt_float128_p(void);
extern int mpfr_buildopt_decimal_p(void);
extern int mpfr_buildopt_gmpinternals_p(void);
extern int mpfr_buildopt_sharedcache_p(void);
extern const char * mpfr_buildopt_tune_case(void);
typedef long mpfr_exp_t;
extern mpfr_exp_t mpfr_get_emin(void);
extern int mpfr_set_emin(mpfr_exp_t);
extern mpfr_exp_t mpfr_get_emin_min(void);
extern mpfr_exp_t mpfr_get_emin_max(void);
extern mpfr_exp_t mpfr_get_emax(void);
extern int mpfr_set_emax(mpfr_exp_t);
extern mpfr_exp_t mpfr_get_emax_min(void);
extern mpfr_exp_t mpfr_get_emax_max(void);
typedef int mpfr_rnd_t;
extern void mpfr_set_default_rounding_mode(mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern mpfr_rnd_t mpfr_get_default_rounding_mode(void);
typedef int mpfr_rnd_t;
extern const char * mpfr_print_rnd_mode(mpfr_rnd_t);
extern void mpfr_clear_flags(void);
extern void mpfr_clear_underflow(void);
extern void mpfr_clear_overflow(void);
extern void mpfr_clear_divby0(void);
extern void mpfr_clear_nanflag(void);
extern void mpfr_clear_inexflag(void);
extern void mpfr_clear_erangeflag(void);
extern void mpfr_set_underflow(void);
extern void mpfr_set_overflow(void);
extern void mpfr_set_divby0(void);
extern void mpfr_set_nanflag(void);
extern void mpfr_set_inexflag(void);
extern void mpfr_set_erangeflag(void);
extern int mpfr_underflow_p(void);
extern int mpfr_overflow_p(void);
extern int mpfr_divby0_p(void);
extern int mpfr_nanflag_p(void);
extern int mpfr_inexflag_p(void);
extern int mpfr_erangeflag_p(void);
typedef unsigned int mpfr_flags_t;
extern void mpfr_flags_clear(mpfr_flags_t);
extern void mpfr_flags_set(mpfr_flags_t);
extern mpfr_flags_t mpfr_flags_test(mpfr_flags_t);
extern mpfr_flags_t mpfr_flags_save(void);
extern void mpfr_flags_restore(mpfr_flags_t, mpfr_flags_t);
struct __mpfr_struct;
typedef struct __mpfr_struct __mpfr_struct;
typedef __mpfr_struct * mpfr_ptr;
typedef int mpfr_rnd_t;
extern int mpfr_check_range(mpfr_ptr, int, mpfr_rnd_t);
typedef long mpfr_prec_t;
extern void mpfr_init2(mpfr_ptr, mpfr_prec_t);
extern void mpfr_init(mpfr_ptr);
extern void mpfr_clear(mpfr_ptr);
extern void mpfr_inits2(mpfr_prec_t, mpfr_ptr, ...);
extern void mpfr_inits(mpfr_ptr, ...);
extern void mpfr_clears(mpfr_ptr, ...);
typedef int mpfr_rnd_t;
extern int mpfr_prec_round(mpfr_ptr, mpfr_prec_t, mpfr_rnd_t);
typedef const __mpfr_struct * mpfr_srcptr;
typedef int mpfr_rnd_t;
typedef int mpfr_rnd_t;
extern int mpfr_can_round(mpfr_srcptr, mpfr_exp_t, mpfr_rnd_t, mpfr_rnd_t, mpfr_prec_t);
extern mpfr_prec_t mpfr_min_prec(mpfr_srcptr);
extern mpfr_exp_t mpfr_get_exp(mpfr_srcptr);
extern int mpfr_set_exp(mpfr_ptr, mpfr_exp_t);
extern mpfr_prec_t mpfr_get_prec(mpfr_srcptr);
extern void mpfr_set_prec(mpfr_ptr, mpfr_prec_t);
extern void mpfr_set_prec_raw(mpfr_ptr, mpfr_prec_t);
extern void mpfr_set_default_prec(mpfr_prec_t);
extern mpfr_prec_t mpfr_get_default_prec(void);
typedef int mpfr_rnd_t;
extern int mpfr_set_d(mpfr_ptr, double, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_flt(mpfr_ptr, float, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_ld(mpfr_ptr, long double, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_z(mpfr_ptr, mpz_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_z_2exp(mpfr_ptr, mpz_srcptr, mpfr_exp_t, mpfr_rnd_t);
extern void mpfr_set_nan(mpfr_ptr);
extern void mpfr_set_inf(mpfr_ptr, int);
extern void mpfr_set_zero(mpfr_ptr, int);
typedef int mpfr_rnd_t;
extern int mpfr_set_f(mpfr_ptr, mpf_srcptr, mpfr_rnd_t);
extern int mpfr_cmp_f(mpfr_srcptr, mpf_srcptr);
typedef int mpfr_rnd_t;
extern int mpfr_get_f(mpf_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_si(mpfr_ptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_ui(mpfr_ptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_si_2exp(mpfr_ptr, long, mpfr_exp_t, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_ui_2exp(mpfr_ptr, unsigned long, mpfr_exp_t, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set_q(mpfr_ptr, mpq_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul_q(mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_q(mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_add_q(mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sub_q(mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
extern int mpfr_cmp_q(mpfr_srcptr, mpq_srcptr);
extern void mpfr_get_q(mpq_ptr, mpfr_srcptr);
typedef int mpfr_rnd_t;
extern int mpfr_set_str(mpfr_ptr, const char *, int, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_init_set_str(mpfr_ptr, const char *, int, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set4(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t, int);
typedef int mpfr_rnd_t;
extern int mpfr_abs(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_set(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_neg(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
extern int mpfr_signbit(mpfr_srcptr);
typedef int mpfr_rnd_t;
extern int mpfr_setsign(mpfr_ptr, mpfr_srcptr, int, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_copysign(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
extern mpfr_exp_t mpfr_get_z_2exp(mpz_ptr, mpfr_srcptr);
typedef int mpfr_rnd_t;
extern float mpfr_get_flt(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern double mpfr_get_d(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern long double mpfr_get_ld(mpfr_srcptr, mpfr_rnd_t);
extern double mpfr_get_d1(mpfr_srcptr);
typedef int mpfr_rnd_t;
extern double mpfr_get_d_2exp(long *, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern long double mpfr_get_ld_2exp(long *, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_frexp(mpfr_exp_t *, mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern long mpfr_get_si(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern unsigned long mpfr_get_ui(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern char * mpfr_get_str(char *, mpfr_exp_t *, int, size_t, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_get_z(mpz_ptr, mpfr_srcptr, mpfr_rnd_t);
extern void mpfr_free_str(char *);
typedef void *gmp_randstate_t;
typedef int mpfr_rnd_t;
extern int mpfr_urandom(mpfr_ptr, __gmp_randstate_struct *, mpfr_rnd_t);
typedef void *gmp_randstate_t;
typedef int mpfr_rnd_t;
extern int mpfr_grandom(mpfr_ptr, mpfr_ptr, __gmp_randstate_struct *, mpfr_rnd_t);
typedef void *gmp_randstate_t;
typedef int mpfr_rnd_t;
extern int mpfr_nrandom(mpfr_ptr, __gmp_randstate_struct *, mpfr_rnd_t);
typedef void *gmp_randstate_t;
typedef int mpfr_rnd_t;
extern int mpfr_erandom(mpfr_ptr, __gmp_randstate_struct *, mpfr_rnd_t);
typedef void *gmp_randstate_t;
extern int mpfr_urandomb(mpfr_ptr, __gmp_randstate_struct *);
extern void mpfr_nextabove(mpfr_ptr);
extern void mpfr_nextbelow(mpfr_ptr);
extern void mpfr_nexttoward(mpfr_ptr, mpfr_srcptr);
extern int mpfr_printf(const char *, ...);
extern int mpfr_asprintf(char **, const char *, ...);
extern int mpfr_sprintf(char *, const char *, ...);
extern int mpfr_snprintf(char *, size_t, const char *, ...);
typedef int mpfr_rnd_t;
extern int mpfr_pow(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_pow_si(mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_pow_ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_ui_pow_ui(mpfr_ptr, unsigned long, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_ui_pow(mpfr_ptr, unsigned long, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_pow_z(mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sqrt(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sqrt_ui(mpfr_ptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_rec_sqrt(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_add(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sub(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_add_ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sub_ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_ui_sub(mpfr_ptr, unsigned long, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul_ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_ui_div(mpfr_ptr, unsigned long, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_add_si(mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sub_si(mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_si_sub(mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul_si(mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_si(mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_si_div(mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_add_d(mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sub_d(mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_d_sub(mpfr_ptr, double, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul_d(mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_d(mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_d_div(mpfr_ptr, double, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sqr(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_const_pi(mpfr_ptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_const_log2(mpfr_ptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_const_euler(mpfr_ptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_const_catalan(mpfr_ptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_agm(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_log(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_log2(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_log10(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_log1p(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_log_ui(mpfr_ptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_exp(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_exp2(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_exp10(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_expm1(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_eint(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_li2(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
extern int mpfr_cmp(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_cmp3(mpfr_srcptr, mpfr_srcptr, int);
extern int mpfr_cmp_d(mpfr_srcptr, double);
extern int mpfr_cmp_ld(mpfr_srcptr, long double);
extern int mpfr_cmpabs(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_cmp_ui(mpfr_srcptr, unsigned long);
extern int mpfr_cmp_si(mpfr_srcptr, long);
extern int mpfr_cmp_ui_2exp(mpfr_srcptr, unsigned long, mpfr_exp_t);
extern int mpfr_cmp_si_2exp(mpfr_srcptr, long, mpfr_exp_t);
typedef int mpfr_rnd_t;
extern void mpfr_reldiff(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
extern int mpfr_eq(mpfr_srcptr, mpfr_srcptr, unsigned long);
extern int mpfr_sgn(mpfr_srcptr);
typedef int mpfr_rnd_t;
extern int mpfr_mul_2exp(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_2exp(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul_2ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_2ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul_2si(mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_2si(mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_rint(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
extern int mpfr_roundeven(mpfr_ptr, mpfr_srcptr);
extern int mpfr_round(mpfr_ptr, mpfr_srcptr);
extern int mpfr_trunc(mpfr_ptr, mpfr_srcptr);
extern int mpfr_ceil(mpfr_ptr, mpfr_srcptr);
extern int mpfr_floor(mpfr_ptr, mpfr_srcptr);
typedef int mpfr_rnd_t;
extern int mpfr_rint_roundeven(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_rint_round(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_rint_trunc(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_rint_ceil(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_rint_floor(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_frac(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_modf(mpfr_ptr, mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_remquo(mpfr_ptr, long *, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_remainder(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fmod(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fmodquo(mpfr_ptr, long *, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_ulong_p(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_slong_p(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_uint_p(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_sint_p(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_ushort_p(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_sshort_p(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_uintmax_p(mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fits_intmax_p(mpfr_srcptr, mpfr_rnd_t);
extern void mpfr_extract(mpz_ptr, mpfr_srcptr, unsigned int);
extern void mpfr_swap(mpfr_ptr, mpfr_ptr);
extern void mpfr_dump(mpfr_srcptr);
extern int mpfr_nan_p(mpfr_srcptr);
extern int mpfr_inf_p(mpfr_srcptr);
extern int mpfr_number_p(mpfr_srcptr);
extern int mpfr_integer_p(mpfr_srcptr);
extern int mpfr_zero_p(mpfr_srcptr);
extern int mpfr_regular_p(mpfr_srcptr);
extern int mpfr_greater_p(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_greaterequal_p(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_less_p(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_lessequal_p(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_lessgreater_p(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_equal_p(mpfr_srcptr, mpfr_srcptr);
extern int mpfr_unordered_p(mpfr_srcptr, mpfr_srcptr);
typedef int mpfr_rnd_t;
extern int mpfr_atanh(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_acosh(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_asinh(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_cosh(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sinh(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_tanh(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sinh_cosh(mpfr_ptr, mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sech(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_csch(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_coth(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_acos(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_asin(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_atan(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sin(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sin_cos(mpfr_ptr, mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_cos(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_tan(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_atan2(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sec(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_csc(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_cot(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_hypot(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_erf(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_erfc(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_cbrt(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_root(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_rootn_ui(mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_gamma(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_gamma_inc(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_beta(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_lngamma(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_lgamma(mpfr_ptr, int *, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_digamma(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_zeta(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_zeta_ui(mpfr_ptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fac_ui(mpfr_ptr, unsigned long, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_j0(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_j1(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_jn(mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_y0(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_y1(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_yn(mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_ai(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_min(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_max(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_dim(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_mul_z(mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_div_z(mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_add_z(mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sub_z(mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_z_sub(mpfr_ptr, mpz_srcptr, mpfr_srcptr, mpfr_rnd_t);
extern int mpfr_cmp_z(mpfr_srcptr, mpz_srcptr);
typedef int mpfr_rnd_t;
extern int mpfr_fma(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fms(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fmma(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_fmms(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_sum(mpfr_ptr, const mpfr_ptr *, unsigned long, mpfr_rnd_t);
extern void mpfr_free_cache(void);
typedef int mpfr_free_cache_t;
extern void mpfr_free_cache2(mpfr_free_cache_t);
extern void mpfr_free_pool(void);
extern int mpfr_mp_memory_cleanup(void);
typedef int mpfr_rnd_t;
extern int mpfr_subnormalize(mpfr_ptr, int, mpfr_rnd_t);
typedef int mpfr_rnd_t;
extern int mpfr_strtofr(mpfr_ptr, const char *, char **, int, mpfr_rnd_t);
typedef void *mpfr_t;
extern void mpfr_round_nearest_away_begin(__mpfr_struct *);
typedef void *mpfr_t;
extern int mpfr_round_nearest_away_end(__mpfr_struct *, int);
extern size_t mpfr_custom_get_size(mpfr_prec_t);
extern void mpfr_custom_init(void *, mpfr_prec_t);
extern void * mpfr_custom_get_significand(mpfr_srcptr);
extern mpfr_exp_t mpfr_custom_get_exp(mpfr_srcptr);
extern void mpfr_custom_move(mpfr_ptr, void *);
extern void mpfr_custom_init_set(mpfr_ptr, int, mpfr_exp_t, mpfr_prec_t, void *);
extern int mpfr_custom_get_kind(mpfr_srcptr);
struct DSFMT_T;
typedef struct DSFMT_T dsfmt_t;
extern void dsfmt_gen_rand_all(dsfmt_t *);
typedef long ptrdiff_t;
extern void dsfmt_fill_array_open_close(dsfmt_t *, double *, ptrdiff_t);
extern void dsfmt_fill_array_close_open(dsfmt_t *, double *, ptrdiff_t);
extern void dsfmt_fill_array_open_open(dsfmt_t *, double *, ptrdiff_t);
extern void dsfmt_fill_array_close1_open2(dsfmt_t *, double *, ptrdiff_t);
extern void dsfmt_chk_init_gen_rand(dsfmt_t *, uint32_t, int);
extern void dsfmt_chk_init_by_array(dsfmt_t *, uint32_t *, int, int);
extern const char * dsfmt_get_idstring(void);
extern int dsfmt_get_min_array_size(void);
extern uint32_t dsfmt_genrand_uint32(dsfmt_t *);
extern double dsfmt_genrand_close1_open2(dsfmt_t *);
extern double dsfmt_genrand_close_open(dsfmt_t *);
extern double dsfmt_genrand_open_close(dsfmt_t *);
extern double dsfmt_genrand_open_open(dsfmt_t *);
extern uint32_t dsfmt_gv_genrand_uint32(void);
extern double dsfmt_gv_genrand_close1_open2(void);
extern double dsfmt_gv_genrand_close_open(void);
extern double dsfmt_gv_genrand_open_close(void);
extern double dsfmt_gv_genrand_open_open(void);
extern void dsfmt_gv_fill_array_open_close(double *, ptrdiff_t);
extern void dsfmt_gv_fill_array_close_open(double *, ptrdiff_t);
extern void dsfmt_gv_fill_array_open_open(double *, ptrdiff_t);
extern void dsfmt_gv_fill_array_close1_open2(double *, ptrdiff_t);
extern void dsfmt_gv_init_gen_rand(uint32_t);
extern void dsfmt_gv_init_by_array(uint32_t *, int);
extern void dsfmt_init_gen_rand(dsfmt_t *, uint32_t);
extern void dsfmt_init_by_array(dsfmt_t *, uint32_t *, int);
extern const char * get_idstring(void);
extern int get_min_array_size(void);
extern void init_gen_rand(uint32_t);
extern void init_by_array(uint32_t *, int);
extern double genrand_close1_open2(void);
extern double genrand_close_open(void);
extern double genrand_open_close(void);
extern double genrand_open_open(void);
extern void fill_array_open_close(double *, ptrdiff_t);
extern void fill_array_close_open(double *, ptrdiff_t);
extern void fill_array_open_open(double *, ptrdiff_t);
extern void fill_array_close1_open2(double *, ptrdiff_t);
extern void emscripten_run_script(const char *);
extern int emscripten_run_script_int(const char *);
extern char * emscripten_run_script_string(const char *);
extern void emscripten_async_run_script(const char *, int);
typedef void *em_callback_func;
typedef void *em_callback_func;
extern void emscripten_async_load_script(const char *, em_callback_func, em_callback_func);
typedef void *em_callback_func;
extern void emscripten_set_main_loop(em_callback_func, int, int);
extern int emscripten_set_main_loop_timing(int, int);
extern void emscripten_get_main_loop_timing(int *, int *);
typedef void *em_arg_callback_func;
extern void emscripten_set_main_loop_arg(em_arg_callback_func, void *, int, int);
extern void emscripten_pause_main_loop(void);
extern void emscripten_resume_main_loop(void);
extern void emscripten_cancel_main_loop(void);
typedef void *em_socket_error_callback;
extern void emscripten_set_socket_error_callback(void *, em_socket_error_callback);
typedef void *em_socket_callback;
extern void emscripten_set_socket_open_callback(void *, em_socket_callback);
typedef void *em_socket_callback;
extern void emscripten_set_socket_listen_callback(void *, em_socket_callback);
typedef void *em_socket_callback;
extern void emscripten_set_socket_connection_callback(void *, em_socket_callback);
typedef void *em_socket_callback;
extern void emscripten_set_socket_message_callback(void *, em_socket_callback);
typedef void *em_socket_callback;
extern void emscripten_set_socket_close_callback(void *, em_socket_callback);
typedef void *em_arg_callback_func;
extern void _emscripten_push_main_loop_blocker(em_arg_callback_func, void *, const char *);
typedef void *em_arg_callback_func;
extern void _emscripten_push_uncounted_main_loop_blocker(em_arg_callback_func, void *, const char *);
extern void emscripten_set_main_loop_expected_blockers(int);
typedef void *em_arg_callback_func;
extern void emscripten_async_call(em_arg_callback_func, void *, int);
extern void emscripten_exit_with_live_runtime(void);
extern void emscripten_force_exit(int);
extern double emscripten_get_device_pixel_ratio(void);
extern void emscripten_hide_mouse(void);
extern void emscripten_set_canvas_size(int, int);
extern void emscripten_get_canvas_size(int *, int *, int *);
extern double emscripten_get_now(void);
extern float emscripten_random(void);
typedef void *em_str_callback_func;
typedef void *em_str_callback_func;
extern void emscripten_async_wget(const char *, const char *, em_str_callback_func, em_str_callback_func);
typedef void *em_async_wget_onload_func;
typedef void *em_arg_callback_func;
extern void emscripten_async_wget_data(const char *, void *, em_async_wget_onload_func, em_arg_callback_func);
typedef void *em_async_wget2_onload_func;
typedef void *em_async_wget2_onstatus_func;
typedef void *em_async_wget2_onstatus_func;
extern int emscripten_async_wget2(const char *, const char *, const char *, const char *, void *, em_async_wget2_onload_func, em_async_wget2_onstatus_func, em_async_wget2_onstatus_func);
typedef void *em_async_wget2_data_onload_func;
typedef void *em_async_wget2_data_onerror_func;
typedef void *em_async_wget2_data_onprogress_func;
extern int emscripten_async_wget2_data(const char *, const char *, const char *, void *, int, em_async_wget2_data_onload_func, em_async_wget2_data_onerror_func, em_async_wget2_data_onprogress_func);
extern void emscripten_async_wget2_abort(int);
extern void emscripten_wget(const char *, const char *);
extern void emscripten_wget_data(const char *, void **, int *, int *);
typedef void *em_async_wget_onload_func;
typedef void *em_arg_callback_func;
extern void emscripten_idb_async_load(const char *, const char *, void *, em_async_wget_onload_func, em_arg_callback_func);
typedef void *em_arg_callback_func;
typedef void *em_arg_callback_func;
extern void emscripten_idb_async_store(const char *, const char *, void *, int, void *, em_arg_callback_func, em_arg_callback_func);
typedef void *em_arg_callback_func;
typedef void *em_arg_callback_func;
extern void emscripten_idb_async_delete(const char *, const char *, void *, em_arg_callback_func, em_arg_callback_func);
typedef void *em_idb_exists_func;
typedef void *em_arg_callback_func;
extern void emscripten_idb_async_exists(const char *, const char *, void *, em_idb_exists_func, em_arg_callback_func);
extern void emscripten_idb_load(const char *, const char *, void **, int *, int *);
extern void emscripten_idb_store(const char *, const char *, void *, int, int *);
extern void emscripten_idb_delete(const char *, const char *, int *);
extern void emscripten_idb_exists(const char *, const char *, int *, int *);
extern void emscripten_idb_load_blob(const char *, const char *, int *, int *);
extern void emscripten_idb_store_blob(const char *, const char *, void *, int, int *);
extern void emscripten_idb_read_from_blob(int, int, int, void *);
extern void emscripten_idb_free_blob(int);
typedef void *em_str_callback_func;
typedef void *em_str_callback_func;
extern int emscripten_run_preload_plugins(const char *, em_str_callback_func, em_str_callback_func);
typedef void *em_run_preload_plugins_data_onload_func;
typedef void *em_arg_callback_func;
extern void emscripten_run_preload_plugins_data(char *, int, const char *, void *, em_run_preload_plugins_data_onload_func, em_arg_callback_func);
extern void emscripten_lazy_load_code(void);
typedef int worker_handle;
extern worker_handle emscripten_create_worker(const char *);
extern void emscripten_destroy_worker(worker_handle);
typedef void *em_worker_callback_func;
extern void emscripten_call_worker(worker_handle, const char *, char *, int, em_worker_callback_func, void *);
extern void emscripten_worker_respond(char *, int);
extern void emscripten_worker_respond_provisionally(char *, int);
extern int emscripten_get_worker_queue_size(worker_handle);
extern int emscripten_get_compiler_setting(const char *);
extern void emscripten_debugger(void);
extern char * emscripten_get_preloaded_image_data(const char *, int *, int *);
struct _IO_FILE;
typedef struct _IO_FILE FILE;
extern char * emscripten_get_preloaded_image_data_from_FILE(FILE *, int *, int *);
extern void emscripten_log(int, ...);
extern int emscripten_get_callstack(int, char *, int);
extern int emscripten_print_double(double, char *, int);
typedef void *em_scan_func;
extern void emscripten_scan_registers(em_scan_func);
typedef void *em_scan_func;
extern void emscripten_scan_stack(em_scan_func);
extern void emscripten_sleep(unsigned int);
extern void emscripten_sleep_with_yield(unsigned int);
typedef void * emscripten_coroutine;
typedef void *em_arg_callback_func;
extern emscripten_coroutine emscripten_coroutine_create(em_arg_callback_func, void *, int);
extern int emscripten_coroutine_next(emscripten_coroutine);
extern void emscripten_yield(void);


struct interpreter_state;
typedef struct interpreter_state interpreter_state;
extern jl_value_t *eval_value(jl_value_t *e, interpreter_state *s);
extern jl_datatype_t *instantiate_foreigncall_rt(jl_datatype_t *, interpreter_state *s);
jl_value_t *eval_foreigncall(jl_sym_t *fname, jl_sym_t *libname, interpreter_state *s, jl_value_t **args, size_t nargs)
{
const char *target = jl_symbol_name(fname);
// jl_value_ptr is special
if (strcmp(target, "jl_value_ptr") == 0) {
    if (eval_value(args[1], s) == (jl_value_t*)jl_any_type) {
        return (jl_value_t*)jl_unbox_voidpointer(eval_value(args[5], s));
    } else {
        jl_ptls_t ptls = jl_get_ptls_states();
        jl_value_t *ret = (void*)eval_value(args[5], s);
        jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), eval_value(args[1], s));
        *(void**)jl_data_ptr(v) = ret;
        return v;
    }
} else if (strcmp(target, "jl_get_ptls_states") == 0) {
    return jl_box_voidpointer((void*)jl_get_ptls_states());
} else if (strcmp(target, "jl_symbol_name") == 0) {
    jl_ptls_t ptls = jl_get_ptls_states();
    const char *name = jl_symbol_name(eval_value(args[5], s));
    jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), eval_value(args[1], s));
    *(void**)jl_data_ptr(v) = name;
    return v;
} else if (strcmp(target, "srand") == 0) {
	srand(
			(unsigned int) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "getenv") == 0) {
	char * result = getenv(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "setenv") == 0) {
	int result = setenv(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "memcpy") == 0) {
	void * result = memcpy(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "memmove") == 0) {
	void * result = memmove(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "memset") == 0) {
	void * result = memset(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "memcmp") == 0) {
	int result = memcmp(
			(const void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "memchr") == 0) {
	void * result = memchr(
			(const void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "strlen") == 0) {
	unsigned long result = strlen(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "uint2str") == 0) {
	char * result = uint2str(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[7], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "u8_offset") == 0) {
	size_t result = u8_offset(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "u8_charnum") == 0) {
	size_t result = u8_charnum(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "u8_strwidth") == 0) {
	size_t result = u8_strwidth(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "u8_isvalid") == 0) {
	int result = u8_isvalid(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_read") == 0) {
	size_t result = ios_read(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_readall") == 0) {
	size_t result = ios_readall(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_write") == 0) {
	size_t result = ios_write(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_seek") == 0) {
	int64_t result = ios_seek(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int64_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "ios_seek_end") == 0) {
	int64_t result = ios_seek_end(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "ios_skip") == 0) {
	int64_t result = ios_skip(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int64_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "ios_pos") == 0) {
	int64_t result = ios_pos(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "ios_trunc") == 0) {
	int result = ios_trunc(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_eof") == 0) {
	int result = ios_eof(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_eof_blocking") == 0) {
	int result = ios_eof_blocking(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_flush") == 0) {
	int result = ios_flush(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_close") == 0) {
	ios_close(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "ios_isopen") == 0) {
	int result = ios_isopen(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_take_buffer") == 0) {
	char * result = ios_take_buffer(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "ios_setbuf") == 0) {
	int result = ios_setbuf(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_bufmode") == 0) {
	int result = ios_bufmode(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(bufmode_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_get_readable") == 0) {
	int result = ios_get_readable(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_get_writable") == 0) {
	int result = ios_get_writable(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_set_readonly") == 0) {
	ios_set_readonly(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "ios_copy") == 0) {
	size_t result = ios_copy(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ios_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_copyall") == 0) {
	size_t result = ios_copyall(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ios_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_copyuntil") == 0) {
	size_t result = ios_copyuntil(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ios_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(char) jl_unbox_uint8(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_nchomp") == 0) {
	size_t result = ios_nchomp(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_readprep") == 0) {
	size_t result = ios_readprep(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "ios_file") == 0) {
	ios_t * result = ios_file(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(int) jl_unbox_int32(eval_value(args[9], s)),
			(int) jl_unbox_int32(eval_value(args[10], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "ios_mkstemp") == 0) {
	ios_t * result = ios_mkstemp(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "ios_mem") == 0) {
	ios_t * result = ios_mem(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "ios_fd") == 0) {
	ios_t * result = ios_fd(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "ios_pututf8") == 0) {
	int result = ios_pututf8(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_printf") == 0) {
	int result = ios_printf(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_getutf8") == 0) {
	int result = ios_getutf8(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_peekutf8") == 0) {
	int result = ios_peekutf8(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_readline") == 0) {
	char * result = ios_readline(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "ios_purge") == 0) {
	ios_purge(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "ios_putc") == 0) {
	int result = ios_putc(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(ios_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_getc") == 0) {
	int result = ios_getc(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "ios_peekc") == 0) {
	int result = ios_peekc(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_gettimeofday") == 0) {
	int result = jl_gettimeofday(
			(struct jl_timeval *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_clock_now") == 0) {
	double result = jl_clock_now();
	return jl_box_float64(result);
} else if (strcmp(target, "int32hash") == 0) {
	uint32_t result = int32hash(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "int64hash") == 0) {
	uint64_t result = int64hash(
			(uint64_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "int64to32hash") == 0) {
	uint32_t result = int64to32hash(
			(uint64_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "memhash") == 0) {
	uint64_t result = memhash(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "memhash_seed") == 0) {
	uint64_t result = memhash_seed(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "memhash32") == 0) {
	uint32_t result = memhash32(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "memhash32_seed") == 0) {
	uint32_t result = memhash32_seed(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "bitvector_new") == 0) {
	uint32_t * result = bitvector_new(
			(uint64_t) jl_unbox_uint32(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "bitvector_resize") == 0) {
	uint32_t * result = bitvector_resize(
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "bitvector_set") == 0) {
	bitvector_set(
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "bitvector_get") == 0) {
	uint32_t result = bitvector_get(
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_strtod_c") == 0) {
	double result = jl_strtod_c(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char **) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "jl_strtof_c") == 0) {
	float result = jl_strtof_c(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char **) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_float32(result);
} else if (strcmp(target, "libsupport_init") == 0) {
	libsupport_init();
	return jl_nothing;
} else if (strcmp(target, "jl_threadid") == 0) {
	int16_t result = jl_threadid();
	return jl_box_int16(result);
} else if (strcmp(target, "jl_threading_profile") == 0) {
	jl_threading_profile();
	return jl_nothing;
} else if (strcmp(target, "jl_get_ptls_states") == 0) {
	jl_ptls_t result = jl_get_ptls_states();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_cpu_pause") == 0) {
	jl_cpu_pause();
	return jl_nothing;
} else if (strcmp(target, "jl_cpu_wake") == 0) {
	jl_cpu_wake();
	return jl_nothing;
} else if (strcmp(target, "jl_gc_safepoint") == 0) {
	jl_gc_safepoint();
	return jl_nothing;
} else if (strcmp(target, "jl_gc_enable_finalizers") == 0) {
	jl_gc_enable_finalizers(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_wakeup_thread") == 0) {
	jl_wakeup_thread(
			(int16_t) jl_unbox_int16(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_enable") == 0) {
	int result = jl_gc_enable(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_gc_is_enabled") == 0) {
	int result = jl_gc_is_enabled();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_gc_collect") == 0) {
	jl_gc_collect(
			(jl_gc_collection_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_add_finalizer") == 0) {
	jl_gc_add_finalizer(
			(jl_value_t *) eval_value(args[5], s),
			(jl_function_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_finalize") == 0) {
	jl_finalize(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_new_weakref") == 0) {
	jl_weakref_t * result = jl_gc_new_weakref(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_gc_alloc_0w") == 0) {
	jl_value_t * result = jl_gc_alloc_0w();
	return result;
} else if (strcmp(target, "jl_gc_alloc_1w") == 0) {
	jl_value_t * result = jl_gc_alloc_1w();
	return result;
} else if (strcmp(target, "jl_gc_alloc_2w") == 0) {
	jl_value_t * result = jl_gc_alloc_2w();
	return result;
} else if (strcmp(target, "jl_gc_alloc_3w") == 0) {
	jl_value_t * result = jl_gc_alloc_3w();
	return result;
} else if (strcmp(target, "jl_gc_allocobj") == 0) {
	jl_value_t * result = jl_gc_allocobj(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_malloc_stack") == 0) {
	void * result = jl_malloc_stack(
			(size_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(struct _jl_task_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_free_stack") == 0) {
	jl_free_stack(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_use") == 0) {
	jl_gc_use(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_clear_malloc_data") == 0) {
	jl_clear_malloc_data();
	return jl_nothing;
} else if (strcmp(target, "jl_gc_queue_root") == 0) {
	jl_gc_queue_root(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_managed_malloc") == 0) {
	void * result = jl_gc_managed_malloc(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_gc_managed_realloc") == 0) {
	void * result = jl_gc_managed_realloc(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(jl_value_t *) eval_value(args[9], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_array_typetagdata") == 0) {
	char * result = jl_array_typetagdata(
			(jl_array_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_compute_fieldtypes") == 0) {
	jl_svec_t * result = jl_compute_fieldtypes(
			(jl_datatype_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_subtype") == 0) {
	int result = jl_subtype(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_egal") == 0) {
	int result = jl_egal(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_object_id") == 0) {
	uintptr_t result = jl_object_id(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_has_free_typevars") == 0) {
	int result = jl_has_free_typevars(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_has_typevar") == 0) {
	int result = jl_has_typevar(
			(jl_value_t *) eval_value(args[5], s),
			(jl_tvar_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_has_typevar_from_unionall") == 0) {
	int result = jl_has_typevar_from_unionall(
			(jl_value_t *) eval_value(args[5], s),
			(jl_unionall_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_subtype_env_size") == 0) {
	int result = jl_subtype_env_size(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_subtype_env") == 0) {
	int result = jl_subtype_env(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_isa") == 0) {
	int result = jl_isa(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_types_equal") == 0) {
	int result = jl_types_equal(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_is_not_broken_subtype") == 0) {
	int result = jl_is_not_broken_subtype(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_type_union") == 0) {
	jl_value_t * result = jl_type_union(
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_type_intersection") == 0) {
	jl_value_t * result = jl_type_intersection(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_has_empty_intersection") == 0) {
	int result = jl_has_empty_intersection(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_type_unionall") == 0) {
	jl_value_t * result = jl_type_unionall(
			(jl_tvar_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_typename_str") == 0) {
	const char * result = jl_typename_str(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_typeof_str") == 0) {
	const char * result = jl_typeof_str(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_type_morespecific") == 0) {
	int result = jl_type_morespecific(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_new_typename_in") == 0) {
	jl_typename_t * result = jl_new_typename_in(
			(jl_sym_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_new_typevar") == 0) {
	jl_tvar_t * result = jl_new_typevar(
			(jl_sym_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_instantiate_unionall") == 0) {
	jl_value_t * result = jl_instantiate_unionall(
			(jl_unionall_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_apply_type") == 0) {
	jl_value_t * result = jl_apply_type(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_apply_type1") == 0) {
	jl_value_t * result = jl_apply_type1(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_apply_type2") == 0) {
	jl_value_t * result = jl_apply_type2(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_apply_tuple_type") == 0) {
	jl_tupletype_t * result = jl_apply_tuple_type(
			(jl_svec_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_apply_tuple_type_v") == 0) {
	jl_tupletype_t * result = jl_apply_tuple_type_v(
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_new_datatype") == 0) {
	jl_datatype_t * result = jl_new_datatype(
			(jl_sym_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(jl_datatype_t *) eval_value(args[7], s),
			(jl_svec_t *) eval_value(args[8], s),
			(jl_svec_t *) eval_value(args[9], s),
			(jl_svec_t *) eval_value(args[10], s),
			(int) jl_unbox_int32(eval_value(args[11], s)),
			(int) jl_unbox_int32(eval_value(args[12], s)),
			(int) jl_unbox_int32(eval_value(args[13], s))
		);
	return result;
} else if (strcmp(target, "jl_new_primitivetype") == 0) {
	jl_datatype_t * result = jl_new_primitivetype(
			(jl_value_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(jl_datatype_t *) eval_value(args[7], s),
			(jl_svec_t *) eval_value(args[8], s),
			(size_t) jl_unbox_uint32(eval_value(args[9], s))
		);
	return result;
} else if (strcmp(target, "jl_new_bits") == 0) {
	jl_value_t * result = jl_new_bits(
			(jl_value_t *) eval_value(args[5], s),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_new_struct") == 0) {
	jl_value_t * result = jl_new_struct(
			(jl_datatype_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_new_structv") == 0) {
	jl_value_t * result = jl_new_structv(
			(jl_datatype_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_new_structt") == 0) {
	jl_value_t * result = jl_new_structt(
			(jl_datatype_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_new_struct_uninit") == 0) {
	jl_value_t * result = jl_new_struct_uninit(
			(jl_datatype_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_new_method_instance_uninit") == 0) {
	jl_method_instance_t * result = jl_new_method_instance_uninit();
	return result;
} else if (strcmp(target, "jl_svec") == 0) {
	jl_svec_t * result = (jl_svec)(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_svec1") == 0) {
	jl_svec_t * result = jl_svec1(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_svec2") == 0) {
	jl_svec_t * result = jl_svec2(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_alloc_svec") == 0) {
	jl_svec_t * result = jl_alloc_svec(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_alloc_svec_uninit") == 0) {
	jl_svec_t * result = jl_alloc_svec_uninit(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_svec_copy") == 0) {
	jl_svec_t * result = jl_svec_copy(
			(jl_svec_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_svec_fill") == 0) {
	jl_svec_t * result = jl_svec_fill(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_tupletype_fill") == 0) {
	jl_value_t * result = jl_tupletype_fill(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_symbol") == 0) {
	jl_sym_t * result = jl_symbol(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_symbol_lookup") == 0) {
	jl_sym_t * result = jl_symbol_lookup(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_symbol_n") == 0) {
	jl_sym_t * result = jl_symbol_n(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_gensym") == 0) {
	jl_sym_t * result = jl_gensym();
	return result;
} else if (strcmp(target, "jl_tagged_gensym") == 0) {
	jl_sym_t * result = jl_tagged_gensym(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int32_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_get_root_symbol") == 0) {
	jl_sym_t * result = jl_get_root_symbol();
	return result;
} else if (strcmp(target, "jl_generic_function_def") == 0) {
	jl_value_t * result = jl_generic_function_def(
			(jl_sym_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(jl_value_t *) eval_value(args[8], s),
			(jl_binding_t *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return result;
} else if (strcmp(target, "jl_method_def") == 0) {
	jl_method_def(
			(jl_svec_t *) eval_value(args[5], s),
			(jl_code_info_t *) eval_value(args[6], s),
			(jl_module_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_code_for_staged") == 0) {
	jl_code_info_t * result = jl_code_for_staged(
			(jl_method_instance_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_copy_code_info") == 0) {
	jl_code_info_t * result = jl_copy_code_info(
			(jl_code_info_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_get_world_counter") == 0) {
	size_t result = jl_get_world_counter();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_get_kwsorter") == 0) {
	jl_function_t * result = jl_get_kwsorter(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_box_bool") == 0) {
	jl_value_t * result = jl_box_bool(
			(int8_t) jl_unbox_uint8(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_int8") == 0) {
	jl_value_t * result = jl_box_int8(
			(int8_t) jl_unbox_uint8(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_uint8") == 0) {
	jl_value_t * result = jl_box_uint8(
			(uint8_t) jl_unbox_uint8(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_int16") == 0) {
	jl_value_t * result = jl_box_int16(
			(int16_t) jl_unbox_int16(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_uint16") == 0) {
	jl_value_t * result = jl_box_uint16(
			(uint16_t) jl_unbox_uint16(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_int32") == 0) {
	jl_value_t * result = jl_box_int32(
			(int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_uint32") == 0) {
	jl_value_t * result = jl_box_uint32(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_char") == 0) {
	jl_value_t * result = jl_box_char(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_int64") == 0) {
	jl_value_t * result = jl_box_int64(
			(int64_t) jl_unbox_long(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_uint64") == 0) {
	jl_value_t * result = jl_box_uint64(
			(uint64_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_float32") == 0) {
	jl_value_t * result = jl_box_float32(
			(float) jl_unbox_float32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_float64") == 0) {
	jl_value_t * result = jl_box_float64(
			(double) jl_unbox_float64(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_voidpointer") == 0) {
	jl_value_t * result = jl_box_voidpointer(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_ssavalue") == 0) {
	jl_value_t * result = jl_box_ssavalue(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_box_slotnumber") == 0) {
	jl_value_t * result = jl_box_slotnumber(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_unbox_bool") == 0) {
	int8_t result = jl_unbox_bool(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_unbox_int8") == 0) {
	int8_t result = jl_unbox_int8(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_unbox_uint8") == 0) {
	uint8_t result = jl_unbox_uint8(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_unbox_int16") == 0) {
	int16_t result = jl_unbox_int16(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_int16(result);
} else if (strcmp(target, "jl_unbox_uint16") == 0) {
	uint16_t result = jl_unbox_uint16(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_uint16(result);
} else if (strcmp(target, "jl_unbox_int32") == 0) {
	int32_t result = jl_unbox_int32(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_unbox_uint32") == 0) {
	uint32_t result = jl_unbox_uint32(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_unbox_int64") == 0) {
	int64_t result = jl_unbox_int64(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_long(result);
} else if (strcmp(target, "jl_unbox_uint64") == 0) {
	uint64_t result = jl_unbox_uint64(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_unbox_float32") == 0) {
	float result = jl_unbox_float32(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_float32(result);
} else if (strcmp(target, "jl_unbox_float64") == 0) {
	double result = jl_unbox_float64(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_float64(result);
} else if (strcmp(target, "jl_unbox_voidpointer") == 0) {
	void * result = jl_unbox_voidpointer(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_get_size") == 0) {
	int result = jl_get_size(
			(jl_value_t *) eval_value(args[5], s),
			(size_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_field_index") == 0) {
	int result = jl_field_index(
			(jl_datatype_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_get_nth_field") == 0) {
	jl_value_t * result = jl_get_nth_field(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_get_nth_field_noalloc") == 0) {
	jl_value_t * result = jl_get_nth_field_noalloc(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_get_nth_field_checked") == 0) {
	jl_value_t * result = jl_get_nth_field_checked(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_set_nth_field") == 0) {
	jl_set_nth_field(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(jl_value_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_field_isdefined") == 0) {
	int result = jl_field_isdefined(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_get_field") == 0) {
	jl_value_t * result = jl_get_field(
			(jl_value_t *) eval_value(args[5], s),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_value_ptr") == 0) {
	jl_value_t * result = jl_value_ptr(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_islayout_inline") == 0) {
	int result = jl_islayout_inline(
			(jl_value_t *) eval_value(args[5], s),
			(size_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_new_array") == 0) {
	jl_array_t * result = jl_new_array(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_reshape_array") == 0) {
	jl_array_t * result = jl_reshape_array(
			(jl_value_t *) eval_value(args[5], s),
			(jl_array_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_ptr_to_array_1d") == 0) {
	jl_array_t * result = jl_ptr_to_array_1d(
			(jl_value_t *) eval_value(args[5], s),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_ptr_to_array") == 0) {
	jl_array_t * result = jl_ptr_to_array(
			(jl_value_t *) eval_value(args[5], s),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(jl_value_t *) eval_value(args[7], s),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_alloc_array_1d") == 0) {
	jl_array_t * result = jl_alloc_array_1d(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_alloc_array_2d") == 0) {
	jl_array_t * result = jl_alloc_array_2d(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_alloc_array_3d") == 0) {
	jl_array_t * result = jl_alloc_array_3d(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_pchar_to_array") == 0) {
	jl_array_t * result = jl_pchar_to_array(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_pchar_to_string") == 0) {
	jl_value_t * result = jl_pchar_to_string(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_cstr_to_string") == 0) {
	jl_value_t * result = jl_cstr_to_string(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_alloc_string") == 0) {
	jl_value_t * result = jl_alloc_string(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_array_to_string") == 0) {
	jl_value_t * result = jl_array_to_string(
			(jl_array_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_alloc_vec_any") == 0) {
	jl_array_t * result = jl_alloc_vec_any(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_arrayref") == 0) {
	jl_value_t * result = jl_arrayref(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_ptrarrayref") == 0) {
	jl_value_t * result = jl_ptrarrayref(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_arrayset") == 0) {
	jl_arrayset(
			(jl_array_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_arrayunset") == 0) {
	jl_arrayunset(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_isassigned") == 0) {
	int result = jl_array_isassigned(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_array_grow_end") == 0) {
	jl_array_grow_end(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_del_end") == 0) {
	jl_array_del_end(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_grow_beg") == 0) {
	jl_array_grow_beg(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_del_beg") == 0) {
	jl_array_del_beg(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_sizehint") == 0) {
	jl_array_sizehint(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_ptr_1d_push") == 0) {
	jl_array_ptr_1d_push(
			(jl_array_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_ptr_1d_append") == 0) {
	jl_array_ptr_1d_append(
			(jl_array_t *) eval_value(args[5], s),
			(jl_array_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_apply_array_type") == 0) {
	jl_value_t * result = jl_apply_array_type(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_array_ptr") == 0) {
	void * result = jl_array_ptr(
			(jl_array_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_array_eltype") == 0) {
	void * result = jl_array_eltype(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_array_rank") == 0) {
	int result = jl_array_rank(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_array_size") == 0) {
	size_t result = jl_array_size(
			(jl_value_t *) eval_value(args[5], s),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_string_ptr") == 0) {
	const char * result = jl_string_ptr(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_new_module") == 0) {
	jl_module_t * result = jl_new_module(
			(jl_sym_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_set_module_nospecialize") == 0) {
	jl_set_module_nospecialize(
			(jl_module_t *) eval_value(args[5], s),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_get_binding") == 0) {
	jl_binding_t * result = jl_get_binding(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_get_binding_or_error") == 0) {
	jl_binding_t * result = jl_get_binding_or_error(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_module_globalref") == 0) {
	jl_value_t * result = jl_module_globalref(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_get_binding_wr") == 0) {
	jl_binding_t * result = jl_get_binding_wr(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_get_binding_for_method_def") == 0) {
	jl_binding_t * result = jl_get_binding_for_method_def(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_boundp") == 0) {
	int result = jl_boundp(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_defines_or_exports_p") == 0) {
	int result = jl_defines_or_exports_p(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_binding_resolved_p") == 0) {
	int result = jl_binding_resolved_p(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_is_const") == 0) {
	int result = jl_is_const(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_get_global") == 0) {
	jl_value_t * result = jl_get_global(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_set_global") == 0) {
	jl_set_global(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_set_const") == 0) {
	jl_set_const(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_checked_assignment") == 0) {
	jl_checked_assignment(
			(jl_binding_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_declare_constant") == 0) {
	jl_declare_constant(
			(jl_binding_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_module_using") == 0) {
	jl_module_using(
			(jl_module_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_module_use") == 0) {
	jl_module_use(
			(jl_module_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(jl_sym_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_module_import") == 0) {
	jl_module_import(
			(jl_module_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(jl_sym_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_module_export") == 0) {
	jl_module_export(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_is_imported") == 0) {
	int result = jl_is_imported(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_module_exports_p") == 0) {
	int result = jl_module_exports_p(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_add_standard_imports") == 0) {
	jl_add_standard_imports(
			(jl_module_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_eqtable_put") == 0) {
	jl_array_t * result = jl_eqtable_put(
			(jl_array_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_eqtable_get") == 0) {
	jl_value_t * result = jl_eqtable_get(
			(jl_array_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_errno") == 0) {
	int result = jl_errno();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_set_errno") == 0) {
	jl_set_errno(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_stat") == 0) {
	int32_t result = jl_stat(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_cpu_threads") == 0) {
	int result = jl_cpu_threads();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_getpagesize") == 0) {
	long result = jl_getpagesize();
	return jl_box_long(result);
} else if (strcmp(target, "jl_getallocationgranularity") == 0) {
	long result = jl_getallocationgranularity();
	return jl_box_long(result);
} else if (strcmp(target, "jl_is_debugbuild") == 0) {
	int result = jl_is_debugbuild();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_get_UNAME") == 0) {
	jl_sym_t * result = jl_get_UNAME();
	return result;
} else if (strcmp(target, "jl_get_ARCH") == 0) {
	jl_sym_t * result = jl_get_ARCH();
	return result;
} else if (strcmp(target, "jl_environ") == 0) {
	jl_value_t * result = jl_environ(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_error") == 0) {
	jl_error(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_errorf") == 0) {
	jl_errorf(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_exceptionf") == 0) {
	jl_exceptionf(
			(jl_datatype_t *) eval_value(args[5], s),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_too_few_args") == 0) {
	jl_too_few_args(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_too_many_args") == 0) {
	jl_too_many_args(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_type_error") == 0) {
	jl_type_error(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_type_error_rt") == 0) {
	jl_type_error_rt(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(jl_value_t *) eval_value(args[7], s),
			(jl_value_t *) eval_value(args[8], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_undefined_var_error") == 0) {
	jl_undefined_var_error(
			(jl_sym_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_bounds_error") == 0) {
	jl_bounds_error(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_bounds_error_v") == 0) {
	jl_bounds_error_v(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_bounds_error_int") == 0) {
	jl_bounds_error_int(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_bounds_error_tuple_int") == 0) {
	jl_bounds_error_tuple_int(
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_bounds_error_unboxed_int") == 0) {
	jl_bounds_error_unboxed_int(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_bounds_error_ints") == 0) {
	jl_bounds_error_ints(
			(jl_value_t *) eval_value(args[5], s),
			(size_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_eof_error") == 0) {
	jl_eof_error();
	return jl_nothing;
} else if (strcmp(target, "jl_current_exception") == 0) {
	jl_value_t * result = jl_current_exception();
	return result;
} else if (strcmp(target, "jl_exception_occurred") == 0) {
	jl_value_t * result = jl_exception_occurred();
	return result;
} else if (strcmp(target, "jl_exception_clear") == 0) {
	jl_exception_clear();
	return jl_nothing;
} else if (strcmp(target, "julia_init") == 0) {
	julia_init(
			(JL_IMAGE_SEARCH) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_init") == 0) {
	jl_init();
	return jl_nothing;
} else if (strcmp(target, "jl_init_with_image") == 0) {
	jl_init_with_image(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_get_default_sysimg_path") == 0) {
	const char * result = jl_get_default_sysimg_path();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_is_initialized") == 0) {
	int result = jl_is_initialized();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_atexit_hook") == 0) {
	jl_atexit_hook(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_exit") == 0) {
	jl_exit(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_pathname_for_handle") == 0) {
	const char * result = jl_pathname_for_handle(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_deserialize_verify_header") == 0) {
	int result = jl_deserialize_verify_header(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_preload_sysimg_so") == 0) {
	jl_preload_sysimg_so(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_set_sysimg_so") == 0) {
	jl_set_sysimg_so(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_create_system_image") == 0) {
	ios_t * result = jl_create_system_image();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_save_system_image") == 0) {
	jl_save_system_image(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_restore_system_image") == 0) {
	jl_restore_system_image(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_restore_system_image_data") == 0) {
	jl_restore_system_image_data(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_save_incremental") == 0) {
	int result = jl_save_incremental(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_array_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_restore_incremental") == 0) {
	jl_value_t * result = jl_restore_incremental(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_array_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_restore_incremental_from_buf") == 0) {
	jl_value_t * result = jl_restore_incremental_from_buf(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(jl_array_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_parse_all") == 0) {
	jl_value_t * result = jl_parse_all(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_parse_input_line") == 0) {
	jl_value_t * result = jl_parse_input_line(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_parse_string") == 0) {
	jl_value_t * result = jl_parse_string(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_load_file_string") == 0) {
	jl_value_t * result = jl_load_file_string(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(jl_module_t *) eval_value(args[8], s)
		);
	return result;
} else if (strcmp(target, "jl_expand") == 0) {
	jl_value_t * result = jl_expand(
			(jl_value_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_expand_with_loc") == 0) {
	jl_value_t * result = jl_expand_with_loc(
			(jl_value_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_expand_stmt") == 0) {
	jl_value_t * result = jl_expand_stmt(
			(jl_value_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_expand_stmt_with_loc") == 0) {
	jl_value_t * result = jl_expand_stmt_with_loc(
			(jl_value_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_eval_string") == 0) {
	jl_value_t * result = jl_eval_string(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_load_dynamic_library") == 0) {
	jl_uv_libhandle result = jl_load_dynamic_library(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned int) jl_unbox_uint32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_dlopen") == 0) {
	jl_uv_libhandle result = jl_dlopen(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned int) jl_unbox_uint32(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_dlclose") == 0) {
	int result = jl_dlclose(
			(jl_uv_libhandle) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_dlsym") == 0) {
	int result = jl_dlsym(
			(jl_uv_libhandle) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_toplevel_eval") == 0) {
	jl_value_t * result = jl_toplevel_eval(
			(jl_module_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_toplevel_eval_in") == 0) {
	jl_value_t * result = jl_toplevel_eval_in(
			(jl_module_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_load") == 0) {
	jl_value_t * result = jl_load(
			(jl_module_t *) eval_value(args[5], s),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_base_relative_to") == 0) {
	jl_module_t * result = jl_base_relative_to(
			(jl_module_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_register_newmeth_tracer") == 0) {
	jl_register_newmeth_tracer(
			(void (*)(jl_method_t *)) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_copy_ast") == 0) {
	jl_value_t * result = jl_copy_ast(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_compress_ast") == 0) {
	jl_array_t * result = jl_compress_ast(
			(jl_method_t *) eval_value(args[5], s),
			(jl_code_info_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_uncompress_ast") == 0) {
	jl_code_info_t * result = jl_uncompress_ast(
			(jl_method_t *) eval_value(args[5], s),
			(jl_code_instance_t *) eval_value(args[6], s),
			(jl_array_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_ast_flag_inferred") == 0) {
	uint8_t result = jl_ast_flag_inferred(
			(jl_array_t *) eval_value(args[5], s)
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_ast_flag_inlineable") == 0) {
	uint8_t result = jl_ast_flag_inlineable(
			(jl_array_t *) eval_value(args[5], s)
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_ast_flag_pure") == 0) {
	uint8_t result = jl_ast_flag_pure(
			(jl_array_t *) eval_value(args[5], s)
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_ast_nslots") == 0) {
	ssize_t result = jl_ast_nslots(
			(jl_array_t *) eval_value(args[5], s)
		);
	return jl_box_long(result);
} else if (strcmp(target, "jl_ast_slotflag") == 0) {
	uint8_t result = jl_ast_slotflag(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_compress_argnames") == 0) {
	jl_value_t * result = jl_compress_argnames(
			(jl_array_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_uncompress_argnames") == 0) {
	jl_array_t * result = jl_uncompress_argnames(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_uncompress_argname_n") == 0) {
	jl_value_t * result = jl_uncompress_argname_n(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_is_operator") == 0) {
	int result = jl_is_operator(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_is_unary_operator") == 0) {
	int result = jl_is_unary_operator(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_is_unary_and_binary_operator") == 0) {
	int result = jl_is_unary_and_binary_operator(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_operator_precedence") == 0) {
	int result = jl_operator_precedence(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_apply_generic") == 0) {
	jl_value_t * result = jl_apply_generic(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_invoke") == 0) {
	jl_value_t * result = jl_invoke(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s)),
			(jl_method_instance_t *) eval_value(args[8], s)
		);
	return result;
} else if (strcmp(target, "jl_invoke_api") == 0) {
	int32_t result = jl_invoke_api(
			(jl_code_instance_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_call") == 0) {
	jl_value_t * result = jl_call(
			(jl_function_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int32_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_call0") == 0) {
	jl_value_t * result = jl_call0(
			(jl_function_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_call1") == 0) {
	jl_value_t * result = jl_call1(
			(jl_function_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_call2") == 0) {
	jl_value_t * result = jl_call2(
			(jl_function_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_call3") == 0) {
	jl_value_t * result = jl_call3(
			(jl_function_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s),
			(jl_value_t *) eval_value(args[8], s)
		);
	return result;
} else if (strcmp(target, "jl_yield") == 0) {
	jl_yield();
	return jl_nothing;
} else if (strcmp(target, "jl_install_sigint_handler") == 0) {
	jl_install_sigint_handler();
	return jl_nothing;
} else if (strcmp(target, "jl_sigatomic_begin") == 0) {
	jl_sigatomic_begin();
	return jl_nothing;
} else if (strcmp(target, "jl_sigatomic_end") == 0) {
	jl_sigatomic_end();
	return jl_nothing;
} else if (strcmp(target, "jl_new_task") == 0) {
	jl_task_t * result = jl_new_task(
			(jl_function_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_switchto") == 0) {
	jl_switchto(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_throw") == 0) {
	jl_throw(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_rethrow") == 0) {
	jl_rethrow();
	return jl_nothing;
} else if (strcmp(target, "jl_sig_throw") == 0) {
	jl_sig_throw();
	return jl_nothing;
} else if (strcmp(target, "jl_rethrow_other") == 0) {
	jl_rethrow_other(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_no_exc_handler") == 0) {
	jl_no_exc_handler(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_enter_handler") == 0) {
	jl_enter_handler(
			(jl_handler_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_eh_restore_state") == 0) {
	jl_eh_restore_state(
			(jl_handler_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_pop_handler") == 0) {
	jl_pop_handler(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_excstack_state") == 0) {
	size_t result = jl_excstack_state();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_restore_excstack") == 0) {
	jl_restore_excstack(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_take_buffer") == 0) {
	jl_array_t * result = jl_take_buffer(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_sizeof_ios_t") == 0) {
	int result = jl_sizeof_ios_t();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_uv_puts") == 0) {
	jl_uv_puts(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_printf") == 0) {
	int result = jl_printf(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_safe_printf") == 0) {
	jl_safe_printf(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_stdout_stream") == 0) {
	ios_t * result = jl_stdout_stream();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_stdin_stream") == 0) {
	ios_t * result = jl_stdin_stream();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_stderr_stream") == 0) {
	ios_t * result = jl_stderr_stream();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_flush_cstdio") == 0) {
	jl_flush_cstdio();
	return jl_nothing;
} else if (strcmp(target, "jl_stdout_obj") == 0) {
	jl_value_t * result = jl_stdout_obj();
	return result;
} else if (strcmp(target, "jl_stderr_obj") == 0) {
	jl_value_t * result = jl_stderr_obj();
	return result;
} else if (strcmp(target, "jl_static_show") == 0) {
	size_t result = jl_static_show(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_static_show_func_sig") == 0) {
	size_t result = jl_static_show_func_sig(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jlbacktrace") == 0) {
	jlbacktrace();
	return jl_nothing;
} else if (strcmp(target, "jl_") == 0) {
	jl_(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_sizeof_jl_options") == 0) {
	ssize_t result = jl_sizeof_jl_options();
	return jl_box_long(result);
} else if (strcmp(target, "jl_parse_opts") == 0) {
	jl_parse_opts(
			(int *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char ***) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_format_filename") == 0) {
	char * result = jl_format_filename(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_set_ARGS") == 0) {
	jl_set_ARGS(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(char **) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_generating_output") == 0) {
	int result = jl_generating_output();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_ver_major") == 0) {
	int result = jl_ver_major();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_ver_minor") == 0) {
	int result = jl_ver_minor();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_ver_patch") == 0) {
	int result = jl_ver_patch();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_ver_is_release") == 0) {
	int result = jl_ver_is_release();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_ver_string") == 0) {
	const char * result = jl_ver_string();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_git_branch") == 0) {
	const char * result = jl_git_branch();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_git_commit") == 0) {
	const char * result = jl_git_commit();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_gc_pool_alloc") == 0) {
	jl_value_t * result = jl_gc_pool_alloc(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_gc_big_alloc") == 0) {
	jl_value_t * result = jl_gc_big_alloc(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_alignment") == 0) {
	int result = jl_alignment(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_gc_counted_malloc") == 0) {
	void * result = jl_gc_counted_malloc(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_throw_out_of_memory_error") == 0) {
	jl_throw_out_of_memory_error();
	return jl_nothing;
} else if (strcmp(target, "jl_gc_diff_total_bytes") == 0) {
	int64_t result = jl_gc_diff_total_bytes();
	return jl_box_long(result);
} else if (strcmp(target, "jl_get_method_inferred") == 0) {
	jl_code_instance_t * result = jl_get_method_inferred(
			(jl_method_instance_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_compile_hint") == 0) {
	int result = jl_compile_hint(
			(jl_tupletype_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_foreigncall_get_syms") == 0) {
	jl_foreigncall_get_syms(
			(jl_value_t *) eval_value(args[5], s),
			(jl_sym_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(jl_sym_t **) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_foreigncall_interpretable") == 0) {
	int result = jl_foreigncall_interpretable(
			(jl_sym_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_new_code_info_uninit") == 0) {
	jl_code_info_t * result = jl_new_code_info_uninit();
	return result;
} else if (strcmp(target, "jl_apply_2va") == 0) {
	jl_value_t * result = jl_apply_2va(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_typeassert") == 0) {
	jl_typeassert(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_f_tuple") == 0) {
	jl_value_t * result = jl_f_tuple(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_intrinsic_call") == 0) {
	jl_value_t * result = jl_f_intrinsic_call(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_method_table_insert") == 0) {
	jl_method_table_insert(
			(jl_methtable_t *) eval_value(args[5], s),
			(jl_method_t *) eval_value(args[6], s),
			(jl_tupletype_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_type_morespecific_no_subtype") == 0) {
	int result = jl_type_morespecific_no_subtype(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_instantiate_type_in_env") == 0) {
	jl_value_t * result = jl_instantiate_type_in_env(
			(jl_value_t *) eval_value(args[5], s),
			(jl_unionall_t *) eval_value(args[6], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_matching_methods") == 0) {
	jl_value_t * result = jl_matching_methods(
			(jl_tupletype_t *) eval_value(args[5], s),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return result;
} else if (strcmp(target, "jl_first_argument_datatype") == 0) {
	jl_datatype_t * result = jl_first_argument_datatype(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_argument_datatype") == 0) {
	jl_value_t * result = jl_argument_datatype(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_method_table_for") == 0) {
	jl_methtable_t * result = jl_method_table_for(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_dump_fptr_asm") == 0) {
	jl_value_t * result = jl_dump_fptr_asm(
			(uint64_t) jl_unbox_uint32(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_idtable_rehash") == 0) {
	jl_array_t * result = jl_idtable_rehash(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_new_method_table") == 0) {
	jl_methtable_t * result = jl_new_method_table(
			(jl_sym_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_rettype_inferred") == 0) {
	jl_value_t * result = jl_rettype_inferred(
			(jl_method_instance_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_methtable_lookup") == 0) {
	jl_value_t * result = jl_methtable_lookup(
			(jl_methtable_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_specializations_get_linfo") == 0) {
	jl_method_instance_t * result = jl_specializations_get_linfo(
			(jl_method_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_svec_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_method_instance_add_backedge") == 0) {
	jl_method_instance_add_backedge(
			(jl_method_instance_t *) eval_value(args[5], s),
			(jl_method_instance_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_method_table_add_backedge") == 0) {
	jl_method_table_add_backedge(
			(jl_methtable_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_get_backtrace") == 0) {
	jl_value_t * result = jl_get_backtrace();
	return result;
} else if (strcmp(target, "jl_raise_debugger") == 0) {
	jl_raise_debugger();
	return jl_nothing;
} else if (strcmp(target, "jl_gdblookup") == 0) {
	jl_gdblookup(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_is_interpreter_frame") == 0) {
	int result = jl_is_interpreter_frame(
			(uintptr_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_is_enter_interpreter_frame") == 0) {
	int result = jl_is_enter_interpreter_frame(
			(uintptr_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_capture_interp_frame") == 0) {
	size_t result = jl_capture_interp_frame(
			(jl_bt_element_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uintptr_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uintptr_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_hrtime") == 0) {
	uint64_t result = jl_hrtime();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_load_and_lookup") == 0) {
	void * result = jl_load_and_lookup(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void **) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_get_cfunction_trampoline") == 0) {
	jl_value_t * result = jl_get_cfunction_trampoline(
			(jl_value_t *) eval_value(args[5], s),
			(jl_datatype_t *) eval_value(args[6], s),
			(htable_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(jl_svec_t *) eval_value(args[8], s),
			(void *(*)(void *, void **)) jl_unbox_voidpointer(eval_value(args[9], s)),
			(jl_unionall_t *) eval_value(args[10], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return result;
} else if (strcmp(target, "jl_get_JIT") == 0) {
	jl_value_t * result = jl_get_JIT();
	return result;
} else if (strcmp(target, "jl_fs_rename") == 0) {
	int result = jl_fs_rename(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_cwd") == 0) {
	int result = jl_cwd(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_is_file") == 0) {
	int result = jl_is_file(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_intrinsic_name") == 0) {
	const char * result = jl_intrinsic_name(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_bitcast") == 0) {
	jl_value_t * result = jl_bitcast(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_pointerref") == 0) {
	jl_value_t * result = jl_pointerref(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_pointerset") == 0) {
	jl_value_t * result = jl_pointerset(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s),
			(jl_value_t *) eval_value(args[8], s)
		);
	return result;
} else if (strcmp(target, "jl_cglobal") == 0) {
	jl_value_t * result = jl_cglobal(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_cglobal_auto") == 0) {
	jl_value_t * result = jl_cglobal_auto(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_neg_int") == 0) {
	jl_value_t * result = jl_neg_int(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_add_int") == 0) {
	jl_value_t * result = jl_add_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_sub_int") == 0) {
	jl_value_t * result = jl_sub_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_mul_int") == 0) {
	jl_value_t * result = jl_mul_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_sdiv_int") == 0) {
	jl_value_t * result = jl_sdiv_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_udiv_int") == 0) {
	jl_value_t * result = jl_udiv_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_srem_int") == 0) {
	jl_value_t * result = jl_srem_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_urem_int") == 0) {
	jl_value_t * result = jl_urem_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_add_ptr") == 0) {
	jl_value_t * result = jl_add_ptr(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_sub_ptr") == 0) {
	jl_value_t * result = jl_sub_ptr(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_neg_float") == 0) {
	jl_value_t * result = jl_neg_float(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_add_float") == 0) {
	jl_value_t * result = jl_add_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_sub_float") == 0) {
	jl_value_t * result = jl_sub_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_mul_float") == 0) {
	jl_value_t * result = jl_mul_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_div_float") == 0) {
	jl_value_t * result = jl_div_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_rem_float") == 0) {
	jl_value_t * result = jl_rem_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_fma_float") == 0) {
	jl_value_t * result = jl_fma_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_muladd_float") == 0) {
	jl_value_t * result = jl_muladd_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	return result;
} else if (strcmp(target, "jl_eq_int") == 0) {
	jl_value_t * result = jl_eq_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_ne_int") == 0) {
	jl_value_t * result = jl_ne_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_slt_int") == 0) {
	jl_value_t * result = jl_slt_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_ult_int") == 0) {
	jl_value_t * result = jl_ult_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_sle_int") == 0) {
	jl_value_t * result = jl_sle_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_ule_int") == 0) {
	jl_value_t * result = jl_ule_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_eq_float") == 0) {
	jl_value_t * result = jl_eq_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_ne_float") == 0) {
	jl_value_t * result = jl_ne_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_lt_float") == 0) {
	jl_value_t * result = jl_lt_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_le_float") == 0) {
	jl_value_t * result = jl_le_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_fpiseq") == 0) {
	jl_value_t * result = jl_fpiseq(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_fpislt") == 0) {
	jl_value_t * result = jl_fpislt(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_not_int") == 0) {
	jl_value_t * result = jl_not_int(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_and_int") == 0) {
	jl_value_t * result = jl_and_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_or_int") == 0) {
	jl_value_t * result = jl_or_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_xor_int") == 0) {
	jl_value_t * result = jl_xor_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_shl_int") == 0) {
	jl_value_t * result = jl_shl_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_lshr_int") == 0) {
	jl_value_t * result = jl_lshr_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_ashr_int") == 0) {
	jl_value_t * result = jl_ashr_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_bswap_int") == 0) {
	jl_value_t * result = jl_bswap_int(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_ctpop_int") == 0) {
	jl_value_t * result = jl_ctpop_int(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_ctlz_int") == 0) {
	jl_value_t * result = jl_ctlz_int(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_cttz_int") == 0) {
	jl_value_t * result = jl_cttz_int(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_sext_int") == 0) {
	jl_value_t * result = jl_sext_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_zext_int") == 0) {
	jl_value_t * result = jl_zext_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_trunc_int") == 0) {
	jl_value_t * result = jl_trunc_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_sitofp") == 0) {
	jl_value_t * result = jl_sitofp(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_uitofp") == 0) {
	jl_value_t * result = jl_uitofp(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_fptoui") == 0) {
	jl_value_t * result = jl_fptoui(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_fptosi") == 0) {
	jl_value_t * result = jl_fptosi(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_fptrunc") == 0) {
	jl_value_t * result = jl_fptrunc(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_fpext") == 0) {
	jl_value_t * result = jl_fpext(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_sadd_int") == 0) {
	jl_value_t * result = jl_checked_sadd_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_uadd_int") == 0) {
	jl_value_t * result = jl_checked_uadd_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_ssub_int") == 0) {
	jl_value_t * result = jl_checked_ssub_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_usub_int") == 0) {
	jl_value_t * result = jl_checked_usub_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_smul_int") == 0) {
	jl_value_t * result = jl_checked_smul_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_umul_int") == 0) {
	jl_value_t * result = jl_checked_umul_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_sdiv_int") == 0) {
	jl_value_t * result = jl_checked_sdiv_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_udiv_int") == 0) {
	jl_value_t * result = jl_checked_udiv_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_srem_int") == 0) {
	jl_value_t * result = jl_checked_srem_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_checked_urem_int") == 0) {
	jl_value_t * result = jl_checked_urem_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_ceil_llvm") == 0) {
	jl_value_t * result = jl_ceil_llvm(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_floor_llvm") == 0) {
	jl_value_t * result = jl_floor_llvm(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_trunc_llvm") == 0) {
	jl_value_t * result = jl_trunc_llvm(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_rint_llvm") == 0) {
	jl_value_t * result = jl_rint_llvm(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_sqrt_llvm") == 0) {
	jl_value_t * result = jl_sqrt_llvm(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_sqrt_llvm_fast") == 0) {
	jl_value_t * result = jl_sqrt_llvm_fast(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_abs_float") == 0) {
	jl_value_t * result = jl_abs_float(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_copysign_float") == 0) {
	jl_value_t * result = jl_copysign_float(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_flipsign_int") == 0) {
	jl_value_t * result = jl_flipsign_int(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_arraylen") == 0) {
	jl_value_t * result = jl_arraylen(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_array_data_owner") == 0) {
	jl_value_t * result = jl_array_data_owner(
			(jl_array_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_object_id_") == 0) {
	uintptr_t result = jl_object_id_(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_array_cconvert_cstring") == 0) {
	jl_array_t * result = jl_array_cconvert_cstring(
			(jl_array_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_extern_c") == 0) {
	jl_extern_c(
			(jl_function_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s),
			(char *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_function_ptr") == 0) {
	void * result = jl_function_ptr(
			(jl_function_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_dump_function_asm") == 0) {
	const jl_value_t * result = jl_dump_function_asm(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_dump_function_ir") == 0) {
	const jl_value_t * result = jl_dump_function_ir(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint8_t) jl_unbox_uint8(eval_value(args[6], s)),
			(uint8_t) jl_unbox_uint8(eval_value(args[7], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_LLVMCreateDisasm") == 0) {
	void * result = jl_LLVMCreateDisasm(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_LLVMDisasmInstruction") == 0) {
	size_t result = jl_LLVMDisasmInstruction(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint8_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[7], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[8], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(size_t) jl_unbox_uint32(eval_value(args[10], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_get_LLVM_VERSION") == 0) {
	uint32_t result = jl_get_LLVM_VERSION();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_getutf8") == 0) {
	uint32_t result = jl_getutf8(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_sizeof_off_t") == 0) {
	int result = jl_sizeof_off_t();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_sizeof_mode_t") == 0) {
	int result = jl_sizeof_mode_t();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_ftruncate") == 0) {
	int result = jl_ftruncate(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(int64_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_lseek") == 0) {
	int64_t result = jl_lseek(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(int64_t) jl_unbox_long(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "jl_pwrite") == 0) {
	ssize_t result = jl_pwrite(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(const void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int64_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "jl_mmap") == 0) {
	void * result = jl_mmap(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(int) jl_unbox_int32(eval_value(args[9], s)),
			(int64_t) jl_unbox_long(eval_value(args[10], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_ios_fd") == 0) {
	long result = jl_ios_fd(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "jl_nb_available") == 0) {
	int32_t result = jl_nb_available(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_readuntil") == 0) {
	jl_value_t * result = jl_readuntil(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint8_t) jl_unbox_uint8(eval_value(args[6], s)),
			(uint8_t) jl_unbox_uint8(eval_value(args[7], s)),
			(uint8_t) jl_unbox_uint8(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_ios_buffer_n") == 0) {
	int result = jl_ios_buffer_n(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_ios_get_nbyte_int") == 0) {
	uint64_t result = jl_ios_get_nbyte_int(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_native_alignment") == 0) {
	jl_native_alignment(
			(uint_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(uint_t *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(uint_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(uint_t *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_is_char_signed") == 0) {
	jl_value_t * result = jl_is_char_signed();
	return result;
} else if (strcmp(target, "jl_SC_CLK_TCK") == 0) {
	long result = jl_SC_CLK_TCK();
	return jl_box_long(result);
} else if (strcmp(target, "jl_maxrss") == 0) {
	size_t result = jl_maxrss();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_threading_enabled") == 0) {
	int result = jl_threading_enabled();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_f_throw") == 0) {
	jl_value_t * result = jl_f_throw(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_is") == 0) {
	jl_value_t * result = jl_f_is(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_typeof") == 0) {
	jl_value_t * result = jl_f_typeof(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_sizeof") == 0) {
	jl_value_t * result = jl_f_sizeof(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_issubtype") == 0) {
	jl_value_t * result = jl_f_issubtype(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_isa") == 0) {
	jl_value_t * result = jl_f_isa(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f__apply") == 0) {
	jl_value_t * result = jl_f__apply(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f__apply_pure") == 0) {
	jl_value_t * result = jl_f__apply_pure(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f__apply_latest") == 0) {
	jl_value_t * result = jl_f__apply_latest(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f__apply_iterate") == 0) {
	jl_value_t * result = jl_f__apply_iterate(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_isdefined") == 0) {
	jl_value_t * result = jl_f_isdefined(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_nfields") == 0) {
	jl_value_t * result = jl_f_nfields(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_svec") == 0) {
	jl_value_t * result = jl_f_svec(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_getfield") == 0) {
	jl_value_t * result = jl_f_getfield(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_setfield") == 0) {
	jl_value_t * result = jl_f_setfield(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_fieldtype") == 0) {
	jl_value_t * result = jl_f_fieldtype(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_arrayref") == 0) {
	jl_value_t * result = jl_f_arrayref(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_const_arrayref") == 0) {
	jl_value_t * result = jl_f_const_arrayref(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_arrayset") == 0) {
	jl_value_t * result = jl_f_arrayset(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_arraysize") == 0) {
	jl_value_t * result = jl_f_arraysize(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_apply_type") == 0) {
	jl_value_t * result = jl_f_apply_type(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_applicable") == 0) {
	jl_value_t * result = jl_f_applicable(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_invoke") == 0) {
	jl_value_t * result = jl_f_invoke(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f__expr") == 0) {
	jl_value_t * result = jl_f__expr(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_typeassert") == 0) {
	jl_value_t * result = jl_f_typeassert(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_ifelse") == 0) {
	jl_value_t * result = jl_f_ifelse(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f__typevar") == 0) {
	jl_value_t * result = jl_f__typevar(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_f_invoke_kwsorter") == 0) {
	jl_value_t * result = jl_f_invoke_kwsorter(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_get_cpu_name") == 0) {
	jl_value_t * result = jl_get_cpu_name();
	return result;
} else if (strcmp(target, "jl_dump_host_cpu") == 0) {
	jl_dump_host_cpu();
	return jl_nothing;
} else if (strcmp(target, "jl_running_on_valgrind") == 0) {
	int result = jl_running_on_valgrind();
	return jl_box_int32(result);
} else if (strcmp(target, "ios_write_direct") == 0) {
	size_t result = ios_write_direct(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ios_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_is_memdebug") == 0) {
	int8_t result = jl_is_memdebug();
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_get_julia_bindir") == 0) {
	jl_value_t * result = jl_get_julia_bindir();
	return result;
} else if (strcmp(target, "jl_get_julia_bin") == 0) {
	jl_value_t * result = jl_get_julia_bin();
	return result;
} else if (strcmp(target, "jl_get_image_file") == 0) {
	jl_value_t * result = jl_get_image_file();
	return result;
} else if (strcmp(target, "jl_astaggedvalue") == 0) {
	jl_taggedvalue_t * result = jl_astaggedvalue(
			(jl_value_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_valueof") == 0) {
	jl_value_t * result = jl_valueof(
			(jl_taggedvalue_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_typeof") == 0) {
	jl_value_t * result = jl_typeof(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_get_fieldtypes") == 0) {
	jl_value_t * result = jl_get_fieldtypes(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_gc_unsafe_enter") == 0) {
	int8_t result = jl_gc_unsafe_enter();
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_gc_unsafe_leave") == 0) {
	jl_gc_unsafe_leave(
			(int8_t) jl_unbox_uint8(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_safe_enter") == 0) {
	int8_t result = jl_gc_safe_enter();
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_gc_safe_leave") == 0) {
	jl_gc_safe_leave(
			(int8_t) jl_unbox_uint8(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_get_fenv_consts") == 0) {
	jl_get_fenv_consts(
			(int *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_find_free_typevars") == 0) {
	jl_array_t * result = jl_find_free_typevars(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_set_jsfunction_type") == 0) {
	jl_set_jsfunction_type(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_eqtable_pop") == 0) {
	jl_value_t * result = jl_eqtable_pop(
			(jl_array_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_eqtable_nextind") == 0) {
	size_t result = jl_eqtable_nextind(
			(jl_array_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_get_keyword_sorter") == 0) {
	jl_value_t * result = jl_get_keyword_sorter(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_array_store_unboxed") == 0) {
	int result = jl_array_store_unboxed(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_string_to_array") == 0) {
	jl_array_t * result = jl_string_to_array(
			(jl_value_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_array_grow_at") == 0) {
	jl_array_grow_at(
			(jl_array_t *) eval_value(args[5], s),
			(ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_del_at") == 0) {
	jl_array_del_at(
			(jl_array_t *) eval_value(args[5], s),
			(ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_array_copy") == 0) {
	jl_array_t * result = jl_array_copy(
			(jl_array_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_array_ptr_copy") == 0) {
	jl_array_ptr_copy(
			(jl_array_t *) eval_value(args[5], s),
			(void **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(jl_array_t *) eval_value(args[7], s),
			(void **) jl_unbox_voidpointer(eval_value(args[8], s)),
			(ssize_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_f_new_module") == 0) {
	jl_value_t * result = jl_f_new_module(
			(jl_sym_t *) eval_value(args[5], s),
			(uint8_t) jl_unbox_uint8(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_set_istopmod") == 0) {
	jl_set_istopmod(
			(jl_module_t *) eval_value(args[5], s),
			(uint8_t) jl_unbox_uint8(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_istopmod") == 0) {
	uint8_t result = jl_istopmod(
			(jl_module_t *) eval_value(args[5], s)
		);
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_get_module_of_binding") == 0) {
	jl_module_t * result = jl_get_module_of_binding(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_binding_owner") == 0) {
	jl_value_t * result = jl_binding_owner(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_get_module_binding") == 0) {
	jl_binding_t * result = jl_get_module_binding(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_deprecate_binding") == 0) {
	jl_deprecate_binding(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_is_binding_deprecated") == 0) {
	int result = jl_is_binding_deprecated(
			(jl_module_t *) eval_value(args[5], s),
			(jl_sym_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_module_usings") == 0) {
	jl_value_t * result = jl_module_usings(
			(jl_module_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_module_names") == 0) {
	jl_value_t * result = jl_module_names(
			(jl_module_t *) eval_value(args[5], s),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return result;
} else if (strcmp(target, "jl_module_name") == 0) {
	jl_sym_t * result = jl_module_name(
			(jl_module_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_module_parent") == 0) {
	jl_module_t * result = jl_module_parent(
			(jl_module_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_module_build_id") == 0) {
	uint64_t result = jl_module_build_id(
			(jl_module_t *) eval_value(args[5], s)
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_module_uuid") == 0) {
	jl_uuid_t result = jl_module_uuid(
			(jl_module_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(jl_uuid_t), rt);
	memcpy(jl_data_ptr(v), &result, sizeof(jl_uuid_t));
	return v;

} else if (strcmp(target, "jl_set_module_uuid") == 0) {
	jl_uuid_t arg2;
	memcpy(&arg2, jl_data_ptr(eval_value(args[6], s)), sizeof(jl_uuid_t));
	jl_set_module_uuid(
			(jl_module_t *) eval_value(args[5], s),
			(jl_uuid_t) arg2
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_set_cb_root_scanner") == 0) {
	jl_gc_set_cb_root_scanner(
			(jl_gc_cb_root_scanner_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_set_cb_task_scanner") == 0) {
	jl_gc_set_cb_task_scanner(
			(jl_gc_cb_task_scanner_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_set_cb_pre_gc") == 0) {
	jl_gc_set_cb_pre_gc(
			(jl_gc_cb_pre_gc_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_set_cb_post_gc") == 0) {
	jl_gc_set_cb_post_gc(
			(jl_gc_cb_post_gc_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_set_cb_notify_external_alloc") == 0) {
	jl_gc_set_cb_notify_external_alloc(
			(jl_gc_cb_notify_external_alloc_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_set_cb_notify_external_free") == 0) {
	jl_gc_set_cb_notify_external_free(
			(jl_gc_cb_notify_external_free_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_new_foreign_type") == 0) {
	jl_datatype_t * result = jl_new_foreign_type(
			(jl_sym_t *) eval_value(args[5], s),
			(jl_module_t *) eval_value(args[6], s),
			(jl_datatype_t *) eval_value(args[7], s),
			(jl_markfunc_t) jl_unbox_voidpointer(eval_value(args[8], s)),
			(jl_sweepfunc_t) jl_unbox_voidpointer(eval_value(args[9], s)),
			(int) jl_unbox_int32(eval_value(args[10], s)),
			(int) jl_unbox_int32(eval_value(args[11], s))
		);
	return result;
} else if (strcmp(target, "jl_gc_max_internal_obj_size") == 0) {
	size_t result = jl_gc_max_internal_obj_size();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_gc_external_obj_hdr_size") == 0) {
	size_t result = jl_gc_external_obj_hdr_size();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_gc_alloc_typed") == 0) {
	void * result = jl_gc_alloc_typed(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_gc_mark_queue_obj") == 0) {
	int result = jl_gc_mark_queue_obj(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_gc_mark_queue_objarray") == 0) {
	jl_gc_mark_queue_objarray(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_schedule_foreign_sweepfunc") == 0) {
	jl_gc_schedule_foreign_sweepfunc(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_enable_conservative_gc_support") == 0) {
	int result = jl_gc_enable_conservative_gc_support();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_gc_conservative_gc_support_enabled") == 0) {
	int result = jl_gc_conservative_gc_support_enabled();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_gc_internal_obj_base_ptr") == 0) {
	jl_value_t * result = jl_gc_internal_obj_base_ptr(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return result;
} else if (strcmp(target, "jl_task_stack_buffer") == 0) {
	void * result = jl_task_stack_buffer(
			(jl_task_t *) eval_value(args[5], s),
			(size_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_gc_add_ptr_finalizer") == 0) {
	jl_gc_add_ptr_finalizer(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_add_finalizer_th") == 0) {
	jl_gc_add_finalizer_th(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s),
			(jl_function_t *) eval_value(args[7], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_finalize_th") == 0) {
	jl_finalize_th(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_new_weakref_th") == 0) {
	jl_weakref_t * result = jl_gc_new_weakref_th(
			(jl_ptls_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_gc_get_total_bytes") == 0) {
	jl_gc_get_total_bytes(
			(int64_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_total_hrtime") == 0) {
	uint64_t result = jl_gc_total_hrtime();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_gc_num") == 0) {
	jl_gc_num_t result = jl_gc_num();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(jl_gc_num_t), rt);
	memcpy(jl_data_ptr(v), &result, sizeof(jl_gc_num_t));
	return v;

} else if (strcmp(target, "jl_gc_counted_calloc") == 0) {
	void * result = jl_gc_counted_calloc(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_gc_counted_free_with_size") == 0) {
	jl_gc_counted_free_with_size(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_gc_counted_realloc_with_old_size") == 0) {
	void * result = jl_gc_counted_realloc_with_old_size(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_malloc") == 0) {
	void * result = jl_malloc(
			(size_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_calloc") == 0) {
	void * result = jl_calloc(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_free") == 0) {
	jl_free(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_realloc") == 0) {
	void * result = jl_realloc(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_profile_stop_timer") == 0) {
	jl_profile_stop_timer();
	return jl_nothing;
} else if (strcmp(target, "jl_profile_start_timer") == 0) {
	int result = jl_profile_start_timer();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_exit_on_sigint") == 0) {
	jl_exit_on_sigint(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_repl_raise_sigtstp") == 0) {
	int result = jl_repl_raise_sigtstp();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_profile_init") == 0) {
	int result = jl_profile_init(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(uint64_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_profile_get_data") == 0) {
	uint8_t * result = jl_profile_get_data();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_profile_len_data") == 0) {
	size_t result = jl_profile_len_data();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_profile_maxlen_data") == 0) {
	size_t result = jl_profile_maxlen_data();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_profile_delay_nsec") == 0) {
	uint64_t result = jl_profile_delay_nsec();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_profile_clear_data") == 0) {
	jl_profile_clear_data();
	return jl_nothing;
} else if (strcmp(target, "jl_profile_is_running") == 0) {
	int result = jl_profile_is_running();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_symbol_name") == 0) {
	void * result = jl_symbol_name(
			(jl_sym_t *) eval_value(args[5], s)
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "jl_try_substrtod") == 0) {
	jl_nullable_float64_t result = jl_try_substrtod(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(jl_nullable_float64_t), rt);
	memcpy(jl_data_ptr(v), &result, sizeof(jl_nullable_float64_t));
	return v;

} else if (strcmp(target, "jl_substrtod") == 0) {
	int result = jl_substrtod(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(double *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_try_substrtof") == 0) {
	jl_nullable_float32_t result = jl_try_substrtof(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(jl_nullable_float32_t), rt);
	memcpy(jl_data_ptr(v), &result, sizeof(jl_nullable_float32_t));
	return v;

} else if (strcmp(target, "jl_substrtof") == 0) {
	int result = jl_substrtof(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(float *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_id_start_char") == 0) {
	int result = jl_id_start_char(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_id_char") == 0) {
	int result = jl_id_char(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_is_identifier") == 0) {
	int result = jl_is_identifier(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_breakpoint") == 0) {
	jl_breakpoint(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_uv_flush") == 0) {
	jl_uv_flush(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_uv_putb") == 0) {
	jl_uv_putb(
			(ios_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint8_t) jl_unbox_uint8(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_sizeof_stat") == 0) {
	int result = jl_sizeof_stat();
	return jl_box_int32(result);
} else if (strcmp(target, "jl_lstat") == 0) {
	int32_t result = jl_lstat(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_fstat") == 0) {
	int32_t result = jl_fstat(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_stat_dev") == 0) {
	unsigned int result = jl_stat_dev(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_ino") == 0) {
	unsigned int result = jl_stat_ino(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_mode") == 0) {
	unsigned int result = jl_stat_mode(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_nlink") == 0) {
	unsigned int result = jl_stat_nlink(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_uid") == 0) {
	unsigned int result = jl_stat_uid(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_gid") == 0) {
	unsigned int result = jl_stat_gid(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_rdev") == 0) {
	unsigned int result = jl_stat_rdev(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_size") == 0) {
	uint64_t result = jl_stat_size(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_blksize") == 0) {
	uint64_t result = jl_stat_blksize(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_blocks") == 0) {
	uint64_t result = jl_stat_blocks(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_stat_mtime") == 0) {
	double result = jl_stat_mtime(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "jl_stat_ctime") == 0) {
	double result = jl_stat_ctime(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "jl_wake_libuv") == 0) {
	jl_wake_libuv();
	return jl_nothing;
} else if (strcmp(target, "jl_get_tls_world_age") == 0) {
	size_t result = jl_get_tls_world_age();
	return jl_box_uint32(result);
} else if (strcmp(target, "jl_is_in_pure_context") == 0) {
	int8_t result = jl_is_in_pure_context();
	jl_datatype_t *rt = (jl_datatype_t*)eval_value(args[1], s);
rt = instantiate_foreigncall_rt(rt, s);
	return (rt == jl_bool_type) ? jl_box_bool(result) : jl_box_uint8(result);

} else if (strcmp(target, "jl_specializations_lookup") == 0) {
	jl_value_t * result = jl_specializations_lookup(
			(jl_method_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_new_method_uninit") == 0) {
	jl_method_t * result = jl_new_method_uninit(
			(jl_module_t *) eval_value(args[5], s)
		);
	return result;
} else if (strcmp(target, "jl_set_method_inferred") == 0) {
	jl_code_instance_t * result = jl_set_method_inferred(
			(jl_method_instance_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s),
			(jl_value_t *) eval_value(args[7], s),
			(jl_value_t *) eval_value(args[8], s),
			(int32_t) jl_unbox_int32(eval_value(args[9], s)),
			(size_t) jl_unbox_uint32(eval_value(args[10], s)),
			(size_t) jl_unbox_uint32(eval_value(args[11], s))
		);
	return result;
} else if (strcmp(target, "jl_call_in_typeinf_world") == 0) {
	jl_value_t * result = jl_call_in_typeinf_world(
			(jl_value_t **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_set_typeinf_func") == 0) {
	jl_set_typeinf_func(
			(jl_value_t *) eval_value(args[5], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_isa_compileable_sig") == 0) {
	int result = jl_isa_compileable_sig(
			(jl_tupletype_t *) eval_value(args[5], s),
			(jl_method_t *) eval_value(args[6], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_method_table_disable") == 0) {
	jl_method_table_disable(
			(jl_methtable_t *) eval_value(args[5], s),
			(jl_method_t *) eval_value(args[6], s)
		);
	return jl_nothing;
} else if (strcmp(target, "jl_get_spec_lambda") == 0) {
	jl_value_t * result = jl_get_spec_lambda(
			(jl_tupletype_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return result;
} else if (strcmp(target, "jl_gf_invoke_lookup") == 0) {
	jl_value_t * result = jl_gf_invoke_lookup(
			(jl_value_t *) eval_value(args[5], s),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return result;
} else if (strcmp(target, "jl_get_invoke_lambda") == 0) {
	jl_value_t * result = jl_get_invoke_lambda(
			(jl_typemap_entry_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "jl_typeinf_begin") == 0) {
	jl_typeinf_begin();
	return jl_nothing;
} else if (strcmp(target, "jl_typeinf_end") == 0) {
	jl_typeinf_end();
	return jl_nothing;
} else if (strcmp(target, "jl_get_current_task") == 0) {
	jl_value_t * result = jl_get_current_task();
	return result;
} else if (strcmp(target, "jl_is_task_started") == 0) {
	int result = jl_is_task_started(
			(jl_task_t *) eval_value(args[5], s)
		);
	return jl_box_int32(result);
} else if (strcmp(target, "jl_get_task_tid") == 0) {
	int16_t result = jl_get_task_tid(
			(jl_task_t *) eval_value(args[5], s)
		);
	return jl_box_int16(result);
} else if (strcmp(target, "jl_set_task_tid") == 0) {
	jl_set_task_tid(
			(jl_task_t *) eval_value(args[5], s),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "jl_task_get_next") == 0) {
	jl_task_t * result = jl_task_get_next(
			(jl_value_t *) eval_value(args[5], s),
			(jl_value_t *) eval_value(args[6], s)
		);
	return result;
} else if (strcmp(target, "pcre2_config_8") == 0) {
	int result = pcre2_config_8(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_general_context_copy_8") == 0) {
	pcre2_general_context_8 * result = pcre2_general_context_copy_8(
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_general_context_create_8") == 0) {
	pcre2_general_context_8 * result = pcre2_general_context_create_8(
			(void *(*)(size_t, void *)) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void (*)(void *, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_general_context_free_8") == 0) {
	pcre2_general_context_free_8(
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_compile_context_copy_8") == 0) {
	pcre2_compile_context_8 * result = pcre2_compile_context_copy_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_compile_context_create_8") == 0) {
	pcre2_compile_context_8 * result = pcre2_compile_context_create_8(
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_compile_context_free_8") == 0) {
	pcre2_compile_context_free_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_bsr_8") == 0) {
	int result = pcre2_set_bsr_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_character_tables_8") == 0) {
	int result = pcre2_set_character_tables_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const unsigned char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_compile_extra_options_8") == 0) {
	int result = pcre2_set_compile_extra_options_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_max_pattern_length_8") == 0) {
	int result = pcre2_set_max_pattern_length_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_newline_8") == 0) {
	int result = pcre2_set_newline_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_parens_nest_limit_8") == 0) {
	int result = pcre2_set_parens_nest_limit_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_compile_recursion_guard_8") == 0) {
	int result = pcre2_set_compile_recursion_guard_8(
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int (*)(uint32_t, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_convert_context_copy_8") == 0) {
	pcre2_convert_context_8 * result = pcre2_convert_context_copy_8(
			(pcre2_convert_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_convert_context_create_8") == 0) {
	pcre2_convert_context_8 * result = pcre2_convert_context_create_8(
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_convert_context_free_8") == 0) {
	pcre2_convert_context_free_8(
			(pcre2_convert_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_glob_escape_8") == 0) {
	int result = pcre2_set_glob_escape_8(
			(pcre2_convert_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_glob_separator_8") == 0) {
	int result = pcre2_set_glob_separator_8(
			(pcre2_convert_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_pattern_convert_8") == 0) {
	int result = pcre2_pattern_convert_8(
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s)),
			(PCRE2_UCHAR8 **) jl_unbox_voidpointer(eval_value(args[8], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(pcre2_convert_context_8 *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_converted_pattern_free_8") == 0) {
	pcre2_converted_pattern_free_8(
			(PCRE2_UCHAR8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_match_context_copy_8") == 0) {
	pcre2_match_context_8 * result = pcre2_match_context_copy_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_context_create_8") == 0) {
	pcre2_match_context_8 * result = pcre2_match_context_create_8(
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_context_free_8") == 0) {
	pcre2_match_context_free_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_depth_limit_8") == 0) {
	int result = pcre2_set_depth_limit_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_heap_limit_8") == 0) {
	int result = pcre2_set_heap_limit_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_match_limit_8") == 0) {
	int result = pcre2_set_match_limit_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_offset_limit_8") == 0) {
	int result = pcre2_set_offset_limit_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_recursion_limit_8") == 0) {
	int result = pcre2_set_recursion_limit_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_recursion_memory_management_8") == 0) {
	int result = pcre2_set_recursion_memory_management_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *(*)(size_t, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void (*)(void *, void *)) jl_unbox_voidpointer(eval_value(args[7], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_compile_8") == 0) {
	pcre2_code_8 * result = pcre2_compile_8(
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(pcre2_compile_context_8 *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_code_free_8") == 0) {
	pcre2_code_free_8(
			(pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_code_copy_8") == 0) {
	pcre2_code_8 * result = pcre2_code_copy_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_code_copy_with_tables_8") == 0) {
	pcre2_code_8 * result = pcre2_code_copy_with_tables_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_pattern_info_8") == 0) {
	int result = pcre2_pattern_info_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_data_create_8") == 0) {
	pcre2_match_data_8 * result = pcre2_match_data_create_8(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s)),
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_data_create_from_pattern_8") == 0) {
	pcre2_match_data_8 * result = pcre2_match_data_create_from_pattern_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_dfa_match_8") == 0) {
	int result = pcre2_dfa_match_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[11], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[12], s)),
			(size_t) jl_unbox_uint32(eval_value(args[13], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_8") == 0) {
	int result = pcre2_match_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_data_free_8") == 0) {
	pcre2_match_data_free_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_get_mark_8") == 0) {
	PCRE2_SPTR8 result = pcre2_get_mark_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_get_ovector_count_8") == 0) {
	uint32_t result = pcre2_get_ovector_count_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "pcre2_get_ovector_pointer_8") == 0) {
	size_t * result = pcre2_get_ovector_pointer_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_get_startchar_8") == 0) {
	size_t result = pcre2_get_startchar_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "pcre2_substring_copy_byname_8") == 0) {
	int result = pcre2_substring_copy_byname_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_UCHAR8 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_copy_bynumber_8") == 0) {
	int result = pcre2_substring_copy_bynumber_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(PCRE2_UCHAR8 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_free_8") == 0) {
	pcre2_substring_free_8(
			(PCRE2_UCHAR8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substring_get_byname_8") == 0) {
	int result = pcre2_substring_get_byname_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_UCHAR8 **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_get_bynumber_8") == 0) {
	int result = pcre2_substring_get_bynumber_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(PCRE2_UCHAR8 **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_length_byname_8") == 0) {
	int result = pcre2_substring_length_byname_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_length_bynumber_8") == 0) {
	int result = pcre2_substring_length_bynumber_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_nametable_scan_8") == 0) {
	int result = pcre2_substring_nametable_scan_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_SPTR8 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(PCRE2_SPTR8 *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_number_from_name_8") == 0) {
	int result = pcre2_substring_number_from_name_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_list_free_8") == 0) {
	pcre2_substring_list_free_8(
			(PCRE2_SPTR8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substring_list_get_8") == 0) {
	int result = pcre2_substring_list_get_8(
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_UCHAR8 ***) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t **) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_encode_8") == 0) {
	int32_t result = pcre2_serialize_encode_8(
			(const pcre2_code_8 **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int32_t) jl_unbox_int32(eval_value(args[6], s)),
			(uint8_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_decode_8") == 0) {
	int32_t result = pcre2_serialize_decode_8(
			(pcre2_code_8 **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int32_t) jl_unbox_int32(eval_value(args[6], s)),
			(const uint8_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_get_number_of_codes_8") == 0) {
	int32_t result = pcre2_serialize_get_number_of_codes_8(
			(const uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_free_8") == 0) {
	pcre2_serialize_free_8(
			(uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substitute_8") == 0) {
	int result = pcre2_substitute_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[11], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[12], s)),
			(size_t) jl_unbox_uint32(eval_value(args[13], s)),
			(PCRE2_UCHAR8 *) jl_unbox_voidpointer(eval_value(args[14], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[15], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_compile_8") == 0) {
	int result = pcre2_jit_compile_8(
			(pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_match_8") == 0) {
	int result = pcre2_jit_match_8(
			(const pcre2_code_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_8 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_free_unused_memory_8") == 0) {
	pcre2_jit_free_unused_memory_8(
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_jit_stack_create_8") == 0) {
	pcre2_jit_stack_8 * result = pcre2_jit_stack_create_8(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_jit_stack_assign_8") == 0) {
	pcre2_jit_stack_assign_8(
			(pcre2_match_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(pcre2_jit_callback_8) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_jit_stack_free_8") == 0) {
	pcre2_jit_stack_free_8(
			(pcre2_jit_stack_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_get_error_message_8") == 0) {
	int result = pcre2_get_error_message_8(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(PCRE2_UCHAR8 *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_maketables_8") == 0) {
	const uint8_t * result = pcre2_maketables_8(
			(pcre2_general_context_8 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_config_16") == 0) {
	int result = pcre2_config_16(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_general_context_copy_16") == 0) {
	pcre2_general_context_16 * result = pcre2_general_context_copy_16(
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_general_context_create_16") == 0) {
	pcre2_general_context_16 * result = pcre2_general_context_create_16(
			(void *(*)(size_t, void *)) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void (*)(void *, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_general_context_free_16") == 0) {
	pcre2_general_context_free_16(
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_compile_context_copy_16") == 0) {
	pcre2_compile_context_16 * result = pcre2_compile_context_copy_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_compile_context_create_16") == 0) {
	pcre2_compile_context_16 * result = pcre2_compile_context_create_16(
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_compile_context_free_16") == 0) {
	pcre2_compile_context_free_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_bsr_16") == 0) {
	int result = pcre2_set_bsr_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_character_tables_16") == 0) {
	int result = pcre2_set_character_tables_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const unsigned char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_compile_extra_options_16") == 0) {
	int result = pcre2_set_compile_extra_options_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_max_pattern_length_16") == 0) {
	int result = pcre2_set_max_pattern_length_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_newline_16") == 0) {
	int result = pcre2_set_newline_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_parens_nest_limit_16") == 0) {
	int result = pcre2_set_parens_nest_limit_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_compile_recursion_guard_16") == 0) {
	int result = pcre2_set_compile_recursion_guard_16(
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int (*)(uint32_t, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_convert_context_copy_16") == 0) {
	pcre2_convert_context_16 * result = pcre2_convert_context_copy_16(
			(pcre2_convert_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_convert_context_create_16") == 0) {
	pcre2_convert_context_16 * result = pcre2_convert_context_create_16(
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_convert_context_free_16") == 0) {
	pcre2_convert_context_free_16(
			(pcre2_convert_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_glob_escape_16") == 0) {
	int result = pcre2_set_glob_escape_16(
			(pcre2_convert_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_glob_separator_16") == 0) {
	int result = pcre2_set_glob_separator_16(
			(pcre2_convert_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_pattern_convert_16") == 0) {
	int result = pcre2_pattern_convert_16(
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s)),
			(PCRE2_UCHAR16 **) jl_unbox_voidpointer(eval_value(args[8], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(pcre2_convert_context_16 *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_converted_pattern_free_16") == 0) {
	pcre2_converted_pattern_free_16(
			(PCRE2_UCHAR16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_match_context_copy_16") == 0) {
	pcre2_match_context_16 * result = pcre2_match_context_copy_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_context_create_16") == 0) {
	pcre2_match_context_16 * result = pcre2_match_context_create_16(
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_context_free_16") == 0) {
	pcre2_match_context_free_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_depth_limit_16") == 0) {
	int result = pcre2_set_depth_limit_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_heap_limit_16") == 0) {
	int result = pcre2_set_heap_limit_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_match_limit_16") == 0) {
	int result = pcre2_set_match_limit_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_offset_limit_16") == 0) {
	int result = pcre2_set_offset_limit_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_recursion_limit_16") == 0) {
	int result = pcre2_set_recursion_limit_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_recursion_memory_management_16") == 0) {
	int result = pcre2_set_recursion_memory_management_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *(*)(size_t, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void (*)(void *, void *)) jl_unbox_voidpointer(eval_value(args[7], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_compile_16") == 0) {
	pcre2_code_16 * result = pcre2_compile_16(
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(pcre2_compile_context_16 *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_code_free_16") == 0) {
	pcre2_code_free_16(
			(pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_code_copy_16") == 0) {
	pcre2_code_16 * result = pcre2_code_copy_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_code_copy_with_tables_16") == 0) {
	pcre2_code_16 * result = pcre2_code_copy_with_tables_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_pattern_info_16") == 0) {
	int result = pcre2_pattern_info_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_data_create_16") == 0) {
	pcre2_match_data_16 * result = pcre2_match_data_create_16(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s)),
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_data_create_from_pattern_16") == 0) {
	pcre2_match_data_16 * result = pcre2_match_data_create_from_pattern_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_dfa_match_16") == 0) {
	int result = pcre2_dfa_match_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[11], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[12], s)),
			(size_t) jl_unbox_uint32(eval_value(args[13], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_16") == 0) {
	int result = pcre2_match_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_data_free_16") == 0) {
	pcre2_match_data_free_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_get_mark_16") == 0) {
	PCRE2_SPTR16 result = pcre2_get_mark_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_get_ovector_count_16") == 0) {
	uint32_t result = pcre2_get_ovector_count_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "pcre2_get_ovector_pointer_16") == 0) {
	size_t * result = pcre2_get_ovector_pointer_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_get_startchar_16") == 0) {
	size_t result = pcre2_get_startchar_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "pcre2_substring_copy_byname_16") == 0) {
	int result = pcre2_substring_copy_byname_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_UCHAR16 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_copy_bynumber_16") == 0) {
	int result = pcre2_substring_copy_bynumber_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(PCRE2_UCHAR16 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_free_16") == 0) {
	pcre2_substring_free_16(
			(PCRE2_UCHAR16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substring_get_byname_16") == 0) {
	int result = pcre2_substring_get_byname_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_UCHAR16 **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_get_bynumber_16") == 0) {
	int result = pcre2_substring_get_bynumber_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(PCRE2_UCHAR16 **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_length_byname_16") == 0) {
	int result = pcre2_substring_length_byname_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_length_bynumber_16") == 0) {
	int result = pcre2_substring_length_bynumber_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_nametable_scan_16") == 0) {
	int result = pcre2_substring_nametable_scan_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_SPTR16 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(PCRE2_SPTR16 *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_number_from_name_16") == 0) {
	int result = pcre2_substring_number_from_name_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_list_free_16") == 0) {
	pcre2_substring_list_free_16(
			(PCRE2_SPTR16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substring_list_get_16") == 0) {
	int result = pcre2_substring_list_get_16(
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_UCHAR16 ***) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t **) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_encode_16") == 0) {
	int32_t result = pcre2_serialize_encode_16(
			(const pcre2_code_16 **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int32_t) jl_unbox_int32(eval_value(args[6], s)),
			(uint8_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_decode_16") == 0) {
	int32_t result = pcre2_serialize_decode_16(
			(pcre2_code_16 **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int32_t) jl_unbox_int32(eval_value(args[6], s)),
			(const uint8_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_get_number_of_codes_16") == 0) {
	int32_t result = pcre2_serialize_get_number_of_codes_16(
			(const uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_free_16") == 0) {
	pcre2_serialize_free_16(
			(uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substitute_16") == 0) {
	int result = pcre2_substitute_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[11], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[12], s)),
			(size_t) jl_unbox_uint32(eval_value(args[13], s)),
			(PCRE2_UCHAR16 *) jl_unbox_voidpointer(eval_value(args[14], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[15], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_compile_16") == 0) {
	int result = pcre2_jit_compile_16(
			(pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_match_16") == 0) {
	int result = pcre2_jit_match_16(
			(const pcre2_code_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_16 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_free_unused_memory_16") == 0) {
	pcre2_jit_free_unused_memory_16(
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_jit_stack_create_16") == 0) {
	pcre2_jit_stack_16 * result = pcre2_jit_stack_create_16(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_jit_stack_assign_16") == 0) {
	pcre2_jit_stack_assign_16(
			(pcre2_match_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(pcre2_jit_callback_16) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_jit_stack_free_16") == 0) {
	pcre2_jit_stack_free_16(
			(pcre2_jit_stack_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_get_error_message_16") == 0) {
	int result = pcre2_get_error_message_16(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(PCRE2_UCHAR16 *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_maketables_16") == 0) {
	const uint8_t * result = pcre2_maketables_16(
			(pcre2_general_context_16 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_config_32") == 0) {
	int result = pcre2_config_32(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_general_context_copy_32") == 0) {
	pcre2_general_context_32 * result = pcre2_general_context_copy_32(
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_general_context_create_32") == 0) {
	pcre2_general_context_32 * result = pcre2_general_context_create_32(
			(void *(*)(size_t, void *)) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void (*)(void *, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_general_context_free_32") == 0) {
	pcre2_general_context_free_32(
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_compile_context_copy_32") == 0) {
	pcre2_compile_context_32 * result = pcre2_compile_context_copy_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_compile_context_create_32") == 0) {
	pcre2_compile_context_32 * result = pcre2_compile_context_create_32(
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_compile_context_free_32") == 0) {
	pcre2_compile_context_free_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_bsr_32") == 0) {
	int result = pcre2_set_bsr_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_character_tables_32") == 0) {
	int result = pcre2_set_character_tables_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const unsigned char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_compile_extra_options_32") == 0) {
	int result = pcre2_set_compile_extra_options_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_max_pattern_length_32") == 0) {
	int result = pcre2_set_max_pattern_length_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_newline_32") == 0) {
	int result = pcre2_set_newline_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_parens_nest_limit_32") == 0) {
	int result = pcre2_set_parens_nest_limit_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_compile_recursion_guard_32") == 0) {
	int result = pcre2_set_compile_recursion_guard_32(
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int (*)(uint32_t, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_convert_context_copy_32") == 0) {
	pcre2_convert_context_32 * result = pcre2_convert_context_copy_32(
			(pcre2_convert_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_convert_context_create_32") == 0) {
	pcre2_convert_context_32 * result = pcre2_convert_context_create_32(
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_convert_context_free_32") == 0) {
	pcre2_convert_context_free_32(
			(pcre2_convert_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_glob_escape_32") == 0) {
	int result = pcre2_set_glob_escape_32(
			(pcre2_convert_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_glob_separator_32") == 0) {
	int result = pcre2_set_glob_separator_32(
			(pcre2_convert_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_pattern_convert_32") == 0) {
	int result = pcre2_pattern_convert_32(
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s)),
			(PCRE2_UCHAR32 **) jl_unbox_voidpointer(eval_value(args[8], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(pcre2_convert_context_32 *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_converted_pattern_free_32") == 0) {
	pcre2_converted_pattern_free_32(
			(PCRE2_UCHAR32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_match_context_copy_32") == 0) {
	pcre2_match_context_32 * result = pcre2_match_context_copy_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_context_create_32") == 0) {
	pcre2_match_context_32 * result = pcre2_match_context_create_32(
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_context_free_32") == 0) {
	pcre2_match_context_free_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_set_depth_limit_32") == 0) {
	int result = pcre2_set_depth_limit_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_heap_limit_32") == 0) {
	int result = pcre2_set_heap_limit_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_match_limit_32") == 0) {
	int result = pcre2_set_match_limit_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_offset_limit_32") == 0) {
	int result = pcre2_set_offset_limit_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_recursion_limit_32") == 0) {
	int result = pcre2_set_recursion_limit_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_set_recursion_memory_management_32") == 0) {
	int result = pcre2_set_recursion_memory_management_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *(*)(size_t, void *)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void (*)(void *, void *)) jl_unbox_voidpointer(eval_value(args[7], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_compile_32") == 0) {
	pcre2_code_32 * result = pcre2_compile_32(
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(pcre2_compile_context_32 *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_code_free_32") == 0) {
	pcre2_code_free_32(
			(pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_code_copy_32") == 0) {
	pcre2_code_32 * result = pcre2_code_copy_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_code_copy_with_tables_32") == 0) {
	pcre2_code_32 * result = pcre2_code_copy_with_tables_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_pattern_info_32") == 0) {
	int result = pcre2_pattern_info_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_data_create_32") == 0) {
	pcre2_match_data_32 * result = pcre2_match_data_create_32(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s)),
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_match_data_create_from_pattern_32") == 0) {
	pcre2_match_data_32 * result = pcre2_match_data_create_from_pattern_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_dfa_match_32") == 0) {
	int result = pcre2_dfa_match_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[11], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[12], s)),
			(size_t) jl_unbox_uint32(eval_value(args[13], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_32") == 0) {
	int result = pcre2_match_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_match_data_free_32") == 0) {
	pcre2_match_data_free_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_get_mark_32") == 0) {
	PCRE2_SPTR32 result = pcre2_get_mark_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_get_ovector_count_32") == 0) {
	uint32_t result = pcre2_get_ovector_count_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "pcre2_get_ovector_pointer_32") == 0) {
	size_t * result = pcre2_get_ovector_pointer_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_get_startchar_32") == 0) {
	size_t result = pcre2_get_startchar_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "pcre2_substring_copy_byname_32") == 0) {
	int result = pcre2_substring_copy_byname_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_UCHAR32 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_copy_bynumber_32") == 0) {
	int result = pcre2_substring_copy_bynumber_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(PCRE2_UCHAR32 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_free_32") == 0) {
	pcre2_substring_free_32(
			(PCRE2_UCHAR32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substring_get_byname_32") == 0) {
	int result = pcre2_substring_get_byname_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_UCHAR32 **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_get_bynumber_32") == 0) {
	int result = pcre2_substring_get_bynumber_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(PCRE2_UCHAR32 **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_length_byname_32") == 0) {
	int result = pcre2_substring_length_byname_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_length_bynumber_32") == 0) {
	int result = pcre2_substring_length_bynumber_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_nametable_scan_32") == 0) {
	int result = pcre2_substring_nametable_scan_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(PCRE2_SPTR32 *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(PCRE2_SPTR32 *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_number_from_name_32") == 0) {
	int result = pcre2_substring_number_from_name_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_substring_list_free_32") == 0) {
	pcre2_substring_list_free_32(
			(PCRE2_SPTR32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substring_list_get_32") == 0) {
	int result = pcre2_substring_list_get_32(
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_UCHAR32 ***) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t **) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_encode_32") == 0) {
	int32_t result = pcre2_serialize_encode_32(
			(const pcre2_code_32 **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int32_t) jl_unbox_int32(eval_value(args[6], s)),
			(uint8_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_decode_32") == 0) {
	int32_t result = pcre2_serialize_decode_32(
			(pcre2_code_32 **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int32_t) jl_unbox_int32(eval_value(args[6], s)),
			(const uint8_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_get_number_of_codes_32") == 0) {
	int32_t result = pcre2_serialize_get_number_of_codes_32(
			(const uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_serialize_free_32") == 0) {
	pcre2_serialize_free_32(
			(uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_substitute_32") == 0) {
	int result = pcre2_substitute_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[11], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[12], s)),
			(size_t) jl_unbox_uint32(eval_value(args[13], s)),
			(PCRE2_UCHAR32 *) jl_unbox_voidpointer(eval_value(args[14], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[15], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_compile_32") == 0) {
	int result = pcre2_jit_compile_32(
			(pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_match_32") == 0) {
	int result = pcre2_jit_match_32(
			(const pcre2_code_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(PCRE2_SPTR32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[9], s)),
			(pcre2_match_data_32 *) jl_unbox_voidpointer(eval_value(args[10], s)),
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_jit_free_unused_memory_32") == 0) {
	pcre2_jit_free_unused_memory_32(
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_jit_stack_create_32") == 0) {
	pcre2_jit_stack_32 * result = pcre2_jit_stack_create_32(
			(size_t) jl_unbox_uint32(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "pcre2_jit_stack_assign_32") == 0) {
	pcre2_jit_stack_assign_32(
			(pcre2_match_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(pcre2_jit_callback_32) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_jit_stack_free_32") == 0) {
	pcre2_jit_stack_free_32(
			(pcre2_jit_stack_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "pcre2_get_error_message_32") == 0) {
	int result = pcre2_get_error_message_32(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(PCRE2_UCHAR32 *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "pcre2_maketables_32") == 0) {
	const uint8_t * result = pcre2_maketables_32(
			(pcre2_general_context_32 *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_version") == 0) {
	const char * result = utf8proc_version();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_unicode_version") == 0) {
	const char * result = utf8proc_unicode_version();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_errmsg") == 0) {
	const char * result = utf8proc_errmsg(
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_iterate") == 0) {
	utf8proc_ssize_t result = utf8proc_iterate(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(utf8proc_int32_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_codepoint_valid") == 0) {
	utf8proc_bool result = utf8proc_codepoint_valid(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int8(result);
} else if (strcmp(target, "utf8proc_encode_char") == 0) {
	utf8proc_ssize_t result = utf8proc_encode_char(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s)),
			(utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_get_property") == 0) {
	const utf8proc_property_t * result = utf8proc_get_property(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_decompose_char") == 0) {
	utf8proc_ssize_t result = utf8proc_decompose_char(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s)),
			(utf8proc_int32_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[7], s)),
			(utf8proc_option_t) jl_unbox_int32(eval_value(args[8], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_decompose") == 0) {
	utf8proc_ssize_t result = utf8proc_decompose(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(utf8proc_int32_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[8], s)),
			(utf8proc_option_t) jl_unbox_int32(eval_value(args[9], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_decompose_custom") == 0) {
	utf8proc_ssize_t result = utf8proc_decompose_custom(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(utf8proc_int32_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[8], s)),
			(utf8proc_option_t) jl_unbox_int32(eval_value(args[9], s)),
			(utf8proc_custom_func) jl_unbox_voidpointer(eval_value(args[10], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_normalize_utf32") == 0) {
	utf8proc_ssize_t result = utf8proc_normalize_utf32(
			(utf8proc_int32_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(utf8proc_option_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_reencode") == 0) {
	utf8proc_ssize_t result = utf8proc_reencode(
			(utf8proc_int32_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(utf8proc_option_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_grapheme_break_stateful") == 0) {
	utf8proc_bool result = utf8proc_grapheme_break_stateful(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s)),
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[6], s)),
			(utf8proc_int32_t *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int8(result);
} else if (strcmp(target, "utf8proc_grapheme_break") == 0) {
	utf8proc_bool result = utf8proc_grapheme_break(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s)),
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int8(result);
} else if (strcmp(target, "utf8proc_tolower") == 0) {
	utf8proc_int32_t result = utf8proc_tolower(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "utf8proc_toupper") == 0) {
	utf8proc_int32_t result = utf8proc_toupper(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "utf8proc_totitle") == 0) {
	utf8proc_int32_t result = utf8proc_totitle(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "utf8proc_charwidth") == 0) {
	int result = utf8proc_charwidth(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "utf8proc_category") == 0) {
	utf8proc_category_t result = utf8proc_category(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "utf8proc_category_string") == 0) {
	const char * result = utf8proc_category_string(
			(utf8proc_int32_t) jl_unbox_int32(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_map") == 0) {
	utf8proc_ssize_t result = utf8proc_map(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(utf8proc_uint8_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(utf8proc_option_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_map_custom") == 0) {
	utf8proc_ssize_t result = utf8proc_map_custom(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(utf8proc_ssize_t) jl_unbox_long(eval_value(args[6], s)),
			(utf8proc_uint8_t **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(utf8proc_option_t) jl_unbox_int32(eval_value(args[8], s)),
			(utf8proc_custom_func) jl_unbox_voidpointer(eval_value(args[9], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "utf8proc_NFD") == 0) {
	utf8proc_uint8_t * result = utf8proc_NFD(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_NFC") == 0) {
	utf8proc_uint8_t * result = utf8proc_NFC(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_NFKD") == 0) {
	utf8proc_uint8_t * result = utf8proc_NFKD(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_NFKC") == 0) {
	utf8proc_uint8_t * result = utf8proc_NFKC(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "utf8proc_NFKC_Casefold") == 0) {
	utf8proc_uint8_t * result = utf8proc_NFKC_Casefold(
			(const utf8proc_uint8_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmp_set_memory_functions") == 0) {
	__gmp_set_memory_functions(
			(void *(*)(size_t)) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *(*)(void *, size_t, size_t)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void (*)(void *, size_t)) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_get_memory_functions") == 0) {
	__gmp_get_memory_functions(
			(void *(**)(size_t)) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *(**)(void *, size_t, size_t)) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void (**)(void *, size_t)) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_set_alloc_overflow_function") == 0) {
	__gmp_set_alloc_overflow_function(
			(void (*)(void)) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_get_alloc_overflow_function") == 0) {
	__gmp_get_alloc_overflow_function(
			(void (**)(void)) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randinit") == 0) {
	__gmp_randinit(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randalg_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randinit_default") == 0) {
	__gmp_randinit_default(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randinit_lc_2exp") == 0) {
	__gmp_randinit_lc_2exp(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randinit_lc_2exp_size") == 0) {
	int result = __gmp_randinit_lc_2exp_size(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmp_randinit_mt") == 0) {
	__gmp_randinit_mt(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randinit_set") == 0) {
	__gmp_randinit_set(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const __gmp_randstate_struct *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randseed") == 0) {
	__gmp_randseed(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randseed_ui") == 0) {
	__gmp_randseed_ui(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_randclear") == 0) {
	__gmp_randclear(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmp_urandomb_ui") == 0) {
	unsigned long result = __gmp_urandomb_ui(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmp_urandomm_ui") == 0) {
	unsigned long result = __gmp_urandomm_ui(
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmp_asprintf") == 0) {
	int result = __gmp_asprintf(
			(char **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmp_printf") == 0) {
	int result = __gmp_printf(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmp_snprintf") == 0) {
	int result = __gmp_snprintf(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmp_sprintf") == 0) {
	int result = __gmp_sprintf(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmp_scanf") == 0) {
	int result = __gmp_scanf(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmp_sscanf") == 0) {
	int result = __gmp_sscanf(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_realloc") == 0) {
	void * result = __gmpz_realloc(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpz_abs") == 0) {
	__gmpz_abs(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_add") == 0) {
	__gmpz_add(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_add_ui") == 0) {
	__gmpz_add_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_addmul") == 0) {
	__gmpz_addmul(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_addmul_ui") == 0) {
	__gmpz_addmul_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_and") == 0) {
	__gmpz_and(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_array_init") == 0) {
	__gmpz_array_init(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_bin_ui") == 0) {
	__gmpz_bin_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_bin_uiui") == 0) {
	__gmpz_bin_uiui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_cdiv_q") == 0) {
	__gmpz_cdiv_q(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_cdiv_q_2exp") == 0) {
	__gmpz_cdiv_q_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_cdiv_q_ui") == 0) {
	unsigned long result = __gmpz_cdiv_q_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_cdiv_qr") == 0) {
	__gmpz_cdiv_qr(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_cdiv_qr_ui") == 0) {
	unsigned long result = __gmpz_cdiv_qr_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_cdiv_r") == 0) {
	__gmpz_cdiv_r(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_cdiv_r_2exp") == 0) {
	__gmpz_cdiv_r_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_cdiv_r_ui") == 0) {
	unsigned long result = __gmpz_cdiv_r_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_cdiv_ui") == 0) {
	unsigned long result = __gmpz_cdiv_ui(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_clear") == 0) {
	__gmpz_clear(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_clears") == 0) {
	__gmpz_clears(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_clrbit") == 0) {
	__gmpz_clrbit(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_cmp") == 0) {
	int result = __gmpz_cmp(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_cmp_d") == 0) {
	int result = __gmpz_cmp_d(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_cmp_si") == 0) {
	int result = __gmpz_cmp_si(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_cmp_ui") == 0) {
	int result = __gmpz_cmp_ui(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_cmpabs") == 0) {
	int result = __gmpz_cmpabs(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_cmpabs_d") == 0) {
	int result = __gmpz_cmpabs_d(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_cmpabs_ui") == 0) {
	int result = __gmpz_cmpabs_ui(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_com") == 0) {
	__gmpz_com(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_combit") == 0) {
	__gmpz_combit(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_congruent_p") == 0) {
	int result = __gmpz_congruent_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_congruent_2exp_p") == 0) {
	int result = __gmpz_congruent_2exp_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_congruent_ui_p") == 0) {
	int result = __gmpz_congruent_ui_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_divexact") == 0) {
	__gmpz_divexact(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_divexact_ui") == 0) {
	__gmpz_divexact_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_divisible_p") == 0) {
	int result = __gmpz_divisible_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_divisible_ui_p") == 0) {
	int result = __gmpz_divisible_ui_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_divisible_2exp_p") == 0) {
	int result = __gmpz_divisible_2exp_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_dump") == 0) {
	__gmpz_dump(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_export") == 0) {
	void * result = __gmpz_export(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(int) jl_unbox_int32(eval_value(args[9], s)),
			(size_t) jl_unbox_uint32(eval_value(args[10], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpz_fac_ui") == 0) {
	__gmpz_fac_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_2fac_ui") == 0) {
	__gmpz_2fac_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_mfac_uiui") == 0) {
	__gmpz_mfac_uiui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_primorial_ui") == 0) {
	__gmpz_primorial_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fdiv_q") == 0) {
	__gmpz_fdiv_q(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fdiv_q_2exp") == 0) {
	__gmpz_fdiv_q_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fdiv_q_ui") == 0) {
	unsigned long result = __gmpz_fdiv_q_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_fdiv_qr") == 0) {
	__gmpz_fdiv_qr(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fdiv_qr_ui") == 0) {
	unsigned long result = __gmpz_fdiv_qr_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_fdiv_r") == 0) {
	__gmpz_fdiv_r(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fdiv_r_2exp") == 0) {
	__gmpz_fdiv_r_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fdiv_r_ui") == 0) {
	unsigned long result = __gmpz_fdiv_r_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_fdiv_ui") == 0) {
	unsigned long result = __gmpz_fdiv_ui(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_fib_ui") == 0) {
	__gmpz_fib_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fib2_ui") == 0) {
	__gmpz_fib2_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_fits_sint_p") == 0) {
	int result = __gmpz_fits_sint_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_fits_slong_p") == 0) {
	int result = __gmpz_fits_slong_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_fits_sshort_p") == 0) {
	int result = __gmpz_fits_sshort_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_fits_uint_p") == 0) {
	int result = __gmpz_fits_uint_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_fits_ulong_p") == 0) {
	int result = __gmpz_fits_ulong_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_fits_ushort_p") == 0) {
	int result = __gmpz_fits_ushort_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_gcd") == 0) {
	__gmpz_gcd(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_gcd_ui") == 0) {
	unsigned long result = __gmpz_gcd_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_gcdext") == 0) {
	__gmpz_gcdext(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_get_d") == 0) {
	double result = __gmpz_get_d(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "__gmpz_get_d_2exp") == 0) {
	double result = __gmpz_get_d_2exp(
			(long *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "__gmpz_get_si") == 0) {
	long result = __gmpz_get_si(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpz_get_str") == 0) {
	char * result = __gmpz_get_str(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpz_get_ui") == 0) {
	unsigned long result = __gmpz_get_ui(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_getlimbn") == 0) {
	mp_limb_t result = __gmpz_getlimbn(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_hamdist") == 0) {
	mp_bitcnt_t result = __gmpz_hamdist(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_import") == 0) {
	__gmpz_import(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(int) jl_unbox_int32(eval_value(args[9], s)),
			(size_t) jl_unbox_uint32(eval_value(args[10], s)),
			(const void *) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_init") == 0) {
	__gmpz_init(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_init2") == 0) {
	__gmpz_init2(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_inits") == 0) {
	__gmpz_inits(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_init_set") == 0) {
	__gmpz_init_set(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_init_set_d") == 0) {
	__gmpz_init_set_d(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_init_set_si") == 0) {
	__gmpz_init_set_si(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_init_set_str") == 0) {
	int result = __gmpz_init_set_str(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_init_set_ui") == 0) {
	__gmpz_init_set_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_invert") == 0) {
	int result = __gmpz_invert(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_ior") == 0) {
	__gmpz_ior(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_jacobi") == 0) {
	int result = __gmpz_jacobi(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_kronecker_si") == 0) {
	int result = __gmpz_kronecker_si(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_kronecker_ui") == 0) {
	int result = __gmpz_kronecker_ui(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_si_kronecker") == 0) {
	int result = __gmpz_si_kronecker(
			(long) jl_unbox_long(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_ui_kronecker") == 0) {
	int result = __gmpz_ui_kronecker(
			(unsigned long) jl_unbox_uint32(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_lcm") == 0) {
	__gmpz_lcm(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_lcm_ui") == 0) {
	__gmpz_lcm_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_lucnum_ui") == 0) {
	__gmpz_lucnum_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_lucnum2_ui") == 0) {
	__gmpz_lucnum2_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_millerrabin") == 0) {
	int result = __gmpz_millerrabin(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_mod") == 0) {
	__gmpz_mod(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_mul") == 0) {
	__gmpz_mul(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_mul_2exp") == 0) {
	__gmpz_mul_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_mul_si") == 0) {
	__gmpz_mul_si(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_mul_ui") == 0) {
	__gmpz_mul_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_neg") == 0) {
	__gmpz_neg(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_nextprime") == 0) {
	__gmpz_nextprime(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_perfect_power_p") == 0) {
	int result = __gmpz_perfect_power_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_perfect_square_p") == 0) {
	int result = __gmpz_perfect_square_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_popcount") == 0) {
	mp_bitcnt_t result = __gmpz_popcount(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_pow_ui") == 0) {
	__gmpz_pow_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_powm") == 0) {
	__gmpz_powm(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_powm_sec") == 0) {
	__gmpz_powm_sec(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_powm_ui") == 0) {
	__gmpz_powm_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_probab_prime_p") == 0) {
	int result = __gmpz_probab_prime_p(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_random") == 0) {
	__gmpz_random(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_random2") == 0) {
	__gmpz_random2(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_realloc2") == 0) {
	__gmpz_realloc2(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_remove") == 0) {
	mp_bitcnt_t result = __gmpz_remove(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_root") == 0) {
	int result = __gmpz_root(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_rootrem") == 0) {
	__gmpz_rootrem(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_rrandomb") == 0) {
	__gmpz_rrandomb(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_scan0") == 0) {
	mp_bitcnt_t result = __gmpz_scan0(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_scan1") == 0) {
	mp_bitcnt_t result = __gmpz_scan1(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_set") == 0) {
	__gmpz_set(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_set_d") == 0) {
	__gmpz_set_d(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_set_f") == 0) {
	__gmpz_set_f(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_set_q") == 0) {
	__gmpz_set_q(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_set_si") == 0) {
	__gmpz_set_si(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_set_str") == 0) {
	int result = __gmpz_set_str(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_set_ui") == 0) {
	__gmpz_set_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_setbit") == 0) {
	__gmpz_setbit(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_size") == 0) {
	size_t result = __gmpz_size(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_sizeinbase") == 0) {
	size_t result = __gmpz_sizeinbase(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_sqrt") == 0) {
	__gmpz_sqrt(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_sqrtrem") == 0) {
	__gmpz_sqrtrem(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_sub") == 0) {
	__gmpz_sub(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_sub_ui") == 0) {
	__gmpz_sub_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_ui_sub") == 0) {
	__gmpz_ui_sub(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_submul") == 0) {
	__gmpz_submul(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_submul_ui") == 0) {
	__gmpz_submul_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_swap") == 0) {
	__gmpz_swap(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_tdiv_ui") == 0) {
	unsigned long result = __gmpz_tdiv_ui(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_tdiv_q") == 0) {
	__gmpz_tdiv_q(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_tdiv_q_2exp") == 0) {
	__gmpz_tdiv_q_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_tdiv_q_ui") == 0) {
	unsigned long result = __gmpz_tdiv_q_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_tdiv_qr") == 0) {
	__gmpz_tdiv_qr(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_tdiv_qr_ui") == 0) {
	unsigned long result = __gmpz_tdiv_qr_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_tdiv_r") == 0) {
	__gmpz_tdiv_r(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_tdiv_r_2exp") == 0) {
	__gmpz_tdiv_r_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_tdiv_r_ui") == 0) {
	unsigned long result = __gmpz_tdiv_r_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpz_tstbit") == 0) {
	int result = __gmpz_tstbit(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpz_ui_pow_ui") == 0) {
	__gmpz_ui_pow_ui(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_urandomb") == 0) {
	__gmpz_urandomb(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_urandomm") == 0) {
	__gmpz_urandomm(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_xor") == 0) {
	__gmpz_xor(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_limbs_read") == 0) {
	mp_srcptr result = __gmpz_limbs_read(
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpz_limbs_write") == 0) {
	mp_ptr result = __gmpz_limbs_write(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpz_limbs_modify") == 0) {
	mp_ptr result = __gmpz_limbs_modify(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpz_limbs_finish") == 0) {
	__gmpz_limbs_finish(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpz_roinit_n") == 0) {
	mpz_srcptr result = __gmpz_roinit_n(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpq_abs") == 0) {
	__gmpq_abs(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_add") == 0) {
	__gmpq_add(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_canonicalize") == 0) {
	__gmpq_canonicalize(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_clear") == 0) {
	__gmpq_clear(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_clears") == 0) {
	__gmpq_clears(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_cmp") == 0) {
	int result = __gmpq_cmp(
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpq_cmp_si") == 0) {
	int result = __gmpq_cmp_si(
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpq_cmp_ui") == 0) {
	int result = __gmpq_cmp_ui(
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpq_cmp_z") == 0) {
	int result = __gmpq_cmp_z(
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpq_div") == 0) {
	__gmpq_div(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_div_2exp") == 0) {
	__gmpq_div_2exp(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_equal") == 0) {
	int result = __gmpq_equal(
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpq_get_num") == 0) {
	__gmpq_get_num(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_get_den") == 0) {
	__gmpq_get_den(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_get_d") == 0) {
	double result = __gmpq_get_d(
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "__gmpq_get_str") == 0) {
	char * result = __gmpq_get_str(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpq_init") == 0) {
	__gmpq_init(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_inits") == 0) {
	__gmpq_inits(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_inv") == 0) {
	__gmpq_inv(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_mul") == 0) {
	__gmpq_mul(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_mul_2exp") == 0) {
	__gmpq_mul_2exp(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_neg") == 0) {
	__gmpq_neg(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set") == 0) {
	__gmpq_set(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set_d") == 0) {
	__gmpq_set_d(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set_den") == 0) {
	__gmpq_set_den(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set_f") == 0) {
	__gmpq_set_f(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set_num") == 0) {
	__gmpq_set_num(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set_si") == 0) {
	__gmpq_set_si(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set_str") == 0) {
	int result = __gmpq_set_str(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpq_set_ui") == 0) {
	__gmpq_set_ui(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_set_z") == 0) {
	__gmpq_set_z(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_sub") == 0) {
	__gmpq_sub(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpq_swap") == 0) {
	__gmpq_swap(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_abs") == 0) {
	__gmpf_abs(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_add") == 0) {
	__gmpf_add(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_add_ui") == 0) {
	__gmpf_add_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_ceil") == 0) {
	__gmpf_ceil(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_clear") == 0) {
	__gmpf_clear(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_clears") == 0) {
	__gmpf_clears(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_cmp") == 0) {
	int result = __gmpf_cmp(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_cmp_z") == 0) {
	int result = __gmpf_cmp_z(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_cmp_d") == 0) {
	int result = __gmpf_cmp_d(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_cmp_si") == 0) {
	int result = __gmpf_cmp_si(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_cmp_ui") == 0) {
	int result = __gmpf_cmp_ui(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_div") == 0) {
	__gmpf_div(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_div_2exp") == 0) {
	__gmpf_div_2exp(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_div_ui") == 0) {
	__gmpf_div_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_dump") == 0) {
	__gmpf_dump(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_eq") == 0) {
	int result = __gmpf_eq(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_fits_sint_p") == 0) {
	int result = __gmpf_fits_sint_p(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_fits_slong_p") == 0) {
	int result = __gmpf_fits_slong_p(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_fits_sshort_p") == 0) {
	int result = __gmpf_fits_sshort_p(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_fits_uint_p") == 0) {
	int result = __gmpf_fits_uint_p(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_fits_ulong_p") == 0) {
	int result = __gmpf_fits_ulong_p(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_fits_ushort_p") == 0) {
	int result = __gmpf_fits_ushort_p(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_floor") == 0) {
	__gmpf_floor(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_get_d") == 0) {
	double result = __gmpf_get_d(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "__gmpf_get_d_2exp") == 0) {
	double result = __gmpf_get_d_2exp(
			(long *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "__gmpf_get_default_prec") == 0) {
	mp_bitcnt_t result = __gmpf_get_default_prec();
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpf_get_prec") == 0) {
	mp_bitcnt_t result = __gmpf_get_prec(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpf_get_si") == 0) {
	long result = __gmpf_get_si(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpf_get_str") == 0) {
	char * result = __gmpf_get_str(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_exp_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "__gmpf_get_ui") == 0) {
	unsigned long result = __gmpf_get_ui(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpf_init") == 0) {
	__gmpf_init(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_init2") == 0) {
	__gmpf_init2(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_inits") == 0) {
	__gmpf_inits(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_init_set") == 0) {
	__gmpf_init_set(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_init_set_d") == 0) {
	__gmpf_init_set_d(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_init_set_si") == 0) {
	__gmpf_init_set_si(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_init_set_str") == 0) {
	int result = __gmpf_init_set_str(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_init_set_ui") == 0) {
	__gmpf_init_set_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_integer_p") == 0) {
	int result = __gmpf_integer_p(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_mul") == 0) {
	__gmpf_mul(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_mul_2exp") == 0) {
	__gmpf_mul_2exp(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_mul_ui") == 0) {
	__gmpf_mul_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_neg") == 0) {
	__gmpf_neg(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_pow_ui") == 0) {
	__gmpf_pow_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_random2") == 0) {
	__gmpf_random2(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_exp_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_reldiff") == 0) {
	__gmpf_reldiff(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set") == 0) {
	__gmpf_set(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_d") == 0) {
	__gmpf_set_d(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_default_prec") == 0) {
	__gmpf_set_default_prec(
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_prec") == 0) {
	__gmpf_set_prec(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_prec_raw") == 0) {
	__gmpf_set_prec_raw(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_q") == 0) {
	__gmpf_set_q(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_si") == 0) {
	__gmpf_set_si(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_str") == 0) {
	int result = __gmpf_set_str(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpf_set_ui") == 0) {
	__gmpf_set_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_set_z") == 0) {
	__gmpf_set_z(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_size") == 0) {
	size_t result = __gmpf_size(
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpf_sqrt") == 0) {
	__gmpf_sqrt(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_sqrt_ui") == 0) {
	__gmpf_sqrt_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_sub") == 0) {
	__gmpf_sub(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_sub_ui") == 0) {
	__gmpf_sub_ui(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_swap") == 0) {
	__gmpf_swap(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_trunc") == 0) {
	__gmpf_trunc(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_ui_div") == 0) {
	__gmpf_ui_div(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_ui_sub") == 0) {
	__gmpf_ui_sub(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpf_urandomb") == 0) {
	__gmpf_urandomb(
			(mpf_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_add") == 0) {
	mp_limb_t result = __gmpn_add(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_add_1") == 0) {
	mp_limb_t result = __gmpn_add_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_add_n") == 0) {
	mp_limb_t result = __gmpn_add_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_addmul_1") == 0) {
	mp_limb_t result = __gmpn_addmul_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_cmp") == 0) {
	int result = __gmpn_cmp(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpn_zero_p") == 0) {
	int result = __gmpn_zero_p(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpn_divexact_1") == 0) {
	__gmpn_divexact_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_divexact_by3c") == 0) {
	mp_limb_t result = __gmpn_divexact_by3c(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_divrem") == 0) {
	mp_limb_t result = __gmpn_divrem(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[9], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[10], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_divrem_1") == 0) {
	mp_limb_t result = __gmpn_divrem_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_divrem_2") == 0) {
	mp_limb_t result = __gmpn_divrem_2(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_div_qr_1") == 0) {
	mp_limb_t result = __gmpn_div_qr_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_limb_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_div_qr_2") == 0) {
	mp_limb_t result = __gmpn_div_qr_2(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_gcd") == 0) {
	mp_size_t result = __gmpn_gcd(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_gcd_1") == 0) {
	mp_limb_t result = __gmpn_gcd_1(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_gcdext_1") == 0) {
	mp_limb_t result = __gmpn_gcdext_1(
			(mp_limb_signed_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_limb_signed_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_gcdext") == 0) {
	mp_size_t result = __gmpn_gcdext(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[10], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[11], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_get_str") == 0) {
	size_t result = __gmpn_get_str(
			(unsigned char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_hamdist") == 0) {
	mp_bitcnt_t result = __gmpn_hamdist(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_lshift") == 0) {
	mp_limb_t result = __gmpn_lshift(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(unsigned int) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_mod_1") == 0) {
	mp_limb_t result = __gmpn_mod_1(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_mul") == 0) {
	mp_limb_t result = __gmpn_mul(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_mul_1") == 0) {
	mp_limb_t result = __gmpn_mul_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_mul_n") == 0) {
	__gmpn_mul_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_sqr") == 0) {
	__gmpn_sqr(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_neg") == 0) {
	mp_limb_t result = __gmpn_neg(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_com") == 0) {
	__gmpn_com(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_perfect_square_p") == 0) {
	int result = __gmpn_perfect_square_p(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpn_perfect_power_p") == 0) {
	int result = __gmpn_perfect_power_p(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpn_popcount") == 0) {
	mp_bitcnt_t result = __gmpn_popcount(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_pow_1") == 0) {
	mp_size_t result = __gmpn_pow_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_preinv_mod_1") == 0) {
	mp_limb_t result = __gmpn_preinv_mod_1(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_random") == 0) {
	__gmpn_random(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_random2") == 0) {
	__gmpn_random2(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_rshift") == 0) {
	mp_limb_t result = __gmpn_rshift(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(unsigned int) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_scan0") == 0) {
	mp_bitcnt_t result = __gmpn_scan0(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_scan1") == 0) {
	mp_bitcnt_t result = __gmpn_scan1(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_set_str") == 0) {
	mp_size_t result = __gmpn_set_str(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const unsigned char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(size_t) jl_unbox_uint32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sizeinbase") == 0) {
	size_t result = __gmpn_sizeinbase(
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_sqrtrem") == 0) {
	mp_size_t result = __gmpn_sqrtrem(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sub") == 0) {
	mp_limb_t result = __gmpn_sub(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_sub_1") == 0) {
	mp_limb_t result = __gmpn_sub_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_sub_n") == 0) {
	mp_limb_t result = __gmpn_sub_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_submul_1") == 0) {
	mp_limb_t result = __gmpn_submul_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_tdiv_qr") == 0) {
	__gmpn_tdiv_qr(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[10], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[11], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_and_n") == 0) {
	__gmpn_and_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_andn_n") == 0) {
	__gmpn_andn_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_nand_n") == 0) {
	__gmpn_nand_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_ior_n") == 0) {
	__gmpn_ior_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_iorn_n") == 0) {
	__gmpn_iorn_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_nior_n") == 0) {
	__gmpn_nior_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_xor_n") == 0) {
	__gmpn_xor_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_xnor_n") == 0) {
	__gmpn_xnor_n(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_copyi") == 0) {
	__gmpn_copyi(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_copyd") == 0) {
	__gmpn_copyd(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_zero") == 0) {
	__gmpn_zero(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_cnd_add_n") == 0) {
	mp_limb_t result = __gmpn_cnd_add_n(
			(mp_limb_t) jl_unbox_uint32(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_cnd_sub_n") == 0) {
	mp_limb_t result = __gmpn_cnd_sub_n(
			(mp_limb_t) jl_unbox_uint32(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_sec_add_1") == 0) {
	mp_limb_t result = __gmpn_sec_add_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_sec_add_1_itch") == 0) {
	mp_size_t result = __gmpn_sec_add_1_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sec_sub_1") == 0) {
	mp_limb_t result = __gmpn_sec_sub_1(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_limb_t) jl_unbox_uint32(eval_value(args[8], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_sec_sub_1_itch") == 0) {
	mp_size_t result = __gmpn_sec_sub_1_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_cnd_swap") == 0) {
	__gmpn_cnd_swap(
			(mp_limb_t) jl_unbox_uint32(eval_value(args[5], s)),
			(volatile mp_limb_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(volatile mp_limb_t *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_sec_mul") == 0) {
	__gmpn_sec_mul(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_sec_mul_itch") == 0) {
	mp_size_t result = __gmpn_sec_mul_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sec_sqr") == 0) {
	__gmpn_sec_sqr(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_sec_sqr_itch") == 0) {
	mp_size_t result = __gmpn_sec_sqr_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sec_powm") == 0) {
	__gmpn_sec_powm(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[9], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[10], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[11], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[12], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_sec_powm_itch") == 0) {
	mp_size_t result = __gmpn_sec_powm_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sec_tabselect") == 0) {
	__gmpn_sec_tabselect(
			(volatile mp_limb_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const volatile mp_limb_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_sec_div_qr") == 0) {
	mp_limb_t result = __gmpn_sec_div_qr(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[7], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[9], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "__gmpn_sec_div_qr_itch") == 0) {
	mp_size_t result = __gmpn_sec_div_qr_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sec_div_r") == 0) {
	__gmpn_sec_div_r(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "__gmpn_sec_div_r_itch") == 0) {
	mp_size_t result = __gmpn_sec_div_r_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "__gmpn_sec_invert") == 0) {
	int result = __gmpn_sec_invert(
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mp_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mp_size_t) jl_unbox_long(eval_value(args[8], s)),
			(mp_bitcnt_t) jl_unbox_uint32(eval_value(args[9], s)),
			(mp_ptr) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "__gmpn_sec_invert_itch") == 0) {
	mp_size_t result = __gmpn_sec_invert_itch(
			(mp_size_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_get_version") == 0) {
	const char * result = mpfr_get_version();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "mpfr_get_patches") == 0) {
	const char * result = mpfr_get_patches();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "mpfr_buildopt_tls_p") == 0) {
	int result = mpfr_buildopt_tls_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_buildopt_float128_p") == 0) {
	int result = mpfr_buildopt_float128_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_buildopt_decimal_p") == 0) {
	int result = mpfr_buildopt_decimal_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_buildopt_gmpinternals_p") == 0) {
	int result = mpfr_buildopt_gmpinternals_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_buildopt_sharedcache_p") == 0) {
	int result = mpfr_buildopt_sharedcache_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_buildopt_tune_case") == 0) {
	const char * result = mpfr_buildopt_tune_case();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "mpfr_get_emin") == 0) {
	mpfr_exp_t result = mpfr_get_emin();
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_set_emin") == 0) {
	int result = mpfr_set_emin(
			(mpfr_exp_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_get_emin_min") == 0) {
	mpfr_exp_t result = mpfr_get_emin_min();
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_get_emin_max") == 0) {
	mpfr_exp_t result = mpfr_get_emin_max();
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_get_emax") == 0) {
	mpfr_exp_t result = mpfr_get_emax();
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_set_emax") == 0) {
	int result = mpfr_set_emax(
			(mpfr_exp_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_get_emax_min") == 0) {
	mpfr_exp_t result = mpfr_get_emax_min();
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_get_emax_max") == 0) {
	mpfr_exp_t result = mpfr_get_emax_max();
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_set_default_rounding_mode") == 0) {
	mpfr_set_default_rounding_mode(
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_get_default_rounding_mode") == 0) {
	mpfr_rnd_t result = mpfr_get_default_rounding_mode();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_print_rnd_mode") == 0) {
	const char * result = mpfr_print_rnd_mode(
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "mpfr_clear_flags") == 0) {
	mpfr_clear_flags();
	return jl_nothing;
} else if (strcmp(target, "mpfr_clear_underflow") == 0) {
	mpfr_clear_underflow();
	return jl_nothing;
} else if (strcmp(target, "mpfr_clear_overflow") == 0) {
	mpfr_clear_overflow();
	return jl_nothing;
} else if (strcmp(target, "mpfr_clear_divby0") == 0) {
	mpfr_clear_divby0();
	return jl_nothing;
} else if (strcmp(target, "mpfr_clear_nanflag") == 0) {
	mpfr_clear_nanflag();
	return jl_nothing;
} else if (strcmp(target, "mpfr_clear_inexflag") == 0) {
	mpfr_clear_inexflag();
	return jl_nothing;
} else if (strcmp(target, "mpfr_clear_erangeflag") == 0) {
	mpfr_clear_erangeflag();
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_underflow") == 0) {
	mpfr_set_underflow();
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_overflow") == 0) {
	mpfr_set_overflow();
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_divby0") == 0) {
	mpfr_set_divby0();
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_nanflag") == 0) {
	mpfr_set_nanflag();
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_inexflag") == 0) {
	mpfr_set_inexflag();
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_erangeflag") == 0) {
	mpfr_set_erangeflag();
	return jl_nothing;
} else if (strcmp(target, "mpfr_underflow_p") == 0) {
	int result = mpfr_underflow_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_overflow_p") == 0) {
	int result = mpfr_overflow_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_divby0_p") == 0) {
	int result = mpfr_divby0_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_nanflag_p") == 0) {
	int result = mpfr_nanflag_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_inexflag_p") == 0) {
	int result = mpfr_inexflag_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_erangeflag_p") == 0) {
	int result = mpfr_erangeflag_p();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_flags_clear") == 0) {
	mpfr_flags_clear(
			(mpfr_flags_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_flags_set") == 0) {
	mpfr_flags_set(
			(mpfr_flags_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_flags_test") == 0) {
	mpfr_flags_t result = mpfr_flags_test(
			(mpfr_flags_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "mpfr_flags_save") == 0) {
	mpfr_flags_t result = mpfr_flags_save();
	return jl_box_uint32(result);
} else if (strcmp(target, "mpfr_flags_restore") == 0) {
	mpfr_flags_restore(
			(mpfr_flags_t) jl_unbox_uint32(eval_value(args[5], s)),
			(mpfr_flags_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_check_range") == 0) {
	int result = mpfr_check_range(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_init2") == 0) {
	mpfr_init2(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_prec_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_init") == 0) {
	mpfr_init(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_clear") == 0) {
	mpfr_clear(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_inits2") == 0) {
	mpfr_inits2(
			(mpfr_prec_t) jl_unbox_long(eval_value(args[5], s)),
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_inits") == 0) {
	mpfr_inits(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_clears") == 0) {
	mpfr_clears(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_prec_round") == 0) {
	int result = mpfr_prec_round(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_prec_t) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_can_round") == 0) {
	int result = mpfr_can_round(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s)),
			(mpfr_prec_t) jl_unbox_long(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_min_prec") == 0) {
	mpfr_prec_t result = mpfr_min_prec(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_get_exp") == 0) {
	mpfr_exp_t result = mpfr_get_exp(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_set_exp") == 0) {
	int result = mpfr_set_exp(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_get_prec") == 0) {
	mpfr_prec_t result = mpfr_get_prec(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_set_prec") == 0) {
	mpfr_set_prec(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_prec_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_prec_raw") == 0) {
	mpfr_set_prec_raw(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_prec_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_default_prec") == 0) {
	mpfr_set_default_prec(
			(mpfr_prec_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_get_default_prec") == 0) {
	mpfr_prec_t result = mpfr_get_default_prec();
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_set_d") == 0) {
	int result = mpfr_set_d(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_flt") == 0) {
	int result = mpfr_set_flt(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(float) jl_unbox_float32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_ld") == 0) {
	int result = mpfr_set_ld(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long double) jl_unbox_float64(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_z") == 0) {
	int result = mpfr_set_z(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_z_2exp") == 0) {
	int result = mpfr_set_z_2exp(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_nan") == 0) {
	mpfr_set_nan(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_inf") == 0) {
	mpfr_set_inf(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_zero") == 0) {
	mpfr_set_zero(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_f") == 0) {
	int result = mpfr_set_f(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_f") == 0) {
	int result = mpfr_cmp_f(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpf_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_get_f") == 0) {
	int result = mpfr_get_f(
			(mpf_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_si") == 0) {
	int result = mpfr_set_si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_ui") == 0) {
	int result = mpfr_set_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_si_2exp") == 0) {
	int result = mpfr_set_si_2exp(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_ui_2exp") == 0) {
	int result = mpfr_set_ui_2exp(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set_q") == 0) {
	int result = mpfr_set_q(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_q") == 0) {
	int result = mpfr_mul_q(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_q") == 0) {
	int result = mpfr_div_q(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_add_q") == 0) {
	int result = mpfr_add_q(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sub_q") == 0) {
	int result = mpfr_sub_q(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_q") == 0) {
	int result = mpfr_cmp_q(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpq_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_get_q") == 0) {
	mpfr_get_q(
			(mpq_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_set_str") == 0) {
	int result = mpfr_set_str(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_init_set_str") == 0) {
	int result = mpfr_init_set_str(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set4") == 0) {
	int result = mpfr_set4(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_abs") == 0) {
	int result = mpfr_abs(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_set") == 0) {
	int result = mpfr_set(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_neg") == 0) {
	int result = mpfr_neg(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_signbit") == 0) {
	int result = mpfr_signbit(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_setsign") == 0) {
	int result = mpfr_setsign(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_copysign") == 0) {
	int result = mpfr_copysign(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_get_z_2exp") == 0) {
	mpfr_exp_t result = mpfr_get_z_2exp(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_get_flt") == 0) {
	float result = mpfr_get_flt(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_float32(result);
} else if (strcmp(target, "mpfr_get_d") == 0) {
	double result = mpfr_get_d(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "mpfr_get_ld") == 0) {
	long double result = mpfr_get_ld(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "mpfr_get_d1") == 0) {
	double result = mpfr_get_d1(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "mpfr_get_d_2exp") == 0) {
	double result = mpfr_get_d_2exp(
			(long *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "mpfr_get_ld_2exp") == 0) {
	long double result = mpfr_get_ld_2exp(
			(long *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "mpfr_frexp") == 0) {
	int result = mpfr_frexp(
			(mpfr_exp_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_get_si") == 0) {
	long result = mpfr_get_si(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_get_ui") == 0) {
	unsigned long result = mpfr_get_ui(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "mpfr_get_str") == 0) {
	char * result = mpfr_get_str(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_exp_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(size_t) jl_unbox_uint32(eval_value(args[8], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[9], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[10], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "mpfr_get_z") == 0) {
	int result = mpfr_get_z(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_free_str") == 0) {
	mpfr_free_str(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_urandom") == 0) {
	int result = mpfr_urandom(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_grandom") == 0) {
	int result = mpfr_grandom(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_nrandom") == 0) {
	int result = mpfr_nrandom(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_erandom") == 0) {
	int result = mpfr_erandom(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_urandomb") == 0) {
	int result = mpfr_urandomb(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(gmp_randstate_t) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_nextabove") == 0) {
	mpfr_nextabove(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_nextbelow") == 0) {
	mpfr_nextbelow(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_nexttoward") == 0) {
	mpfr_nexttoward(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_printf") == 0) {
	int result = mpfr_printf(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_asprintf") == 0) {
	int result = mpfr_asprintf(
			(char **) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sprintf") == 0) {
	int result = mpfr_sprintf(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_snprintf") == 0) {
	int result = mpfr_snprintf(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(size_t) jl_unbox_uint32(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_pow") == 0) {
	int result = mpfr_pow(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_pow_si") == 0) {
	int result = mpfr_pow_si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_pow_ui") == 0) {
	int result = mpfr_pow_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_ui_pow_ui") == 0) {
	int result = mpfr_ui_pow_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_ui_pow") == 0) {
	int result = mpfr_ui_pow(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_pow_z") == 0) {
	int result = mpfr_pow_z(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sqrt") == 0) {
	int result = mpfr_sqrt(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sqrt_ui") == 0) {
	int result = mpfr_sqrt_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rec_sqrt") == 0) {
	int result = mpfr_rec_sqrt(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_add") == 0) {
	int result = mpfr_add(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sub") == 0) {
	int result = mpfr_sub(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul") == 0) {
	int result = mpfr_mul(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div") == 0) {
	int result = mpfr_div(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_add_ui") == 0) {
	int result = mpfr_add_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sub_ui") == 0) {
	int result = mpfr_sub_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_ui_sub") == 0) {
	int result = mpfr_ui_sub(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_ui") == 0) {
	int result = mpfr_mul_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_ui") == 0) {
	int result = mpfr_div_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_ui_div") == 0) {
	int result = mpfr_ui_div(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_add_si") == 0) {
	int result = mpfr_add_si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sub_si") == 0) {
	int result = mpfr_sub_si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_si_sub") == 0) {
	int result = mpfr_si_sub(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_si") == 0) {
	int result = mpfr_mul_si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_si") == 0) {
	int result = mpfr_div_si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_si_div") == 0) {
	int result = mpfr_si_div(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_add_d") == 0) {
	int result = mpfr_add_d(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(double) jl_unbox_float64(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sub_d") == 0) {
	int result = mpfr_sub_d(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(double) jl_unbox_float64(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_d_sub") == 0) {
	int result = mpfr_d_sub(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_d") == 0) {
	int result = mpfr_mul_d(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(double) jl_unbox_float64(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_d") == 0) {
	int result = mpfr_div_d(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(double) jl_unbox_float64(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_d_div") == 0) {
	int result = mpfr_d_div(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sqr") == 0) {
	int result = mpfr_sqr(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_const_pi") == 0) {
	int result = mpfr_const_pi(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_const_log2") == 0) {
	int result = mpfr_const_log2(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_const_euler") == 0) {
	int result = mpfr_const_euler(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_const_catalan") == 0) {
	int result = mpfr_const_catalan(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_agm") == 0) {
	int result = mpfr_agm(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_log") == 0) {
	int result = mpfr_log(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_log2") == 0) {
	int result = mpfr_log2(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_log10") == 0) {
	int result = mpfr_log10(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_log1p") == 0) {
	int result = mpfr_log1p(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_log_ui") == 0) {
	int result = mpfr_log_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_exp") == 0) {
	int result = mpfr_exp(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_exp2") == 0) {
	int result = mpfr_exp2(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_exp10") == 0) {
	int result = mpfr_exp10(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_expm1") == 0) {
	int result = mpfr_expm1(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_eint") == 0) {
	int result = mpfr_eint(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_li2") == 0) {
	int result = mpfr_li2(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp") == 0) {
	int result = mpfr_cmp(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp3") == 0) {
	int result = mpfr_cmp3(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_d") == 0) {
	int result = mpfr_cmp_d(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_ld") == 0) {
	int result = mpfr_cmp_ld(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long double) jl_unbox_float64(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmpabs") == 0) {
	int result = mpfr_cmpabs(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_ui") == 0) {
	int result = mpfr_cmp_ui(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_si") == 0) {
	int result = mpfr_cmp_si(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_ui_2exp") == 0) {
	int result = mpfr_cmp_ui_2exp(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_si_2exp") == 0) {
	int result = mpfr_cmp_si_2exp(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_reldiff") == 0) {
	mpfr_reldiff(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_eq") == 0) {
	int result = mpfr_eq(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sgn") == 0) {
	int result = mpfr_sgn(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_2exp") == 0) {
	int result = mpfr_mul_2exp(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_2exp") == 0) {
	int result = mpfr_div_2exp(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_2ui") == 0) {
	int result = mpfr_mul_2ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_2ui") == 0) {
	int result = mpfr_div_2ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_2si") == 0) {
	int result = mpfr_mul_2si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_2si") == 0) {
	int result = mpfr_div_2si(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(long) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rint") == 0) {
	int result = mpfr_rint(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_roundeven") == 0) {
	int result = mpfr_roundeven(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_round") == 0) {
	int result = mpfr_round(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_trunc") == 0) {
	int result = mpfr_trunc(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_ceil") == 0) {
	int result = mpfr_ceil(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_floor") == 0) {
	int result = mpfr_floor(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rint_roundeven") == 0) {
	int result = mpfr_rint_roundeven(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rint_round") == 0) {
	int result = mpfr_rint_round(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rint_trunc") == 0) {
	int result = mpfr_rint_trunc(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rint_ceil") == 0) {
	int result = mpfr_rint_ceil(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rint_floor") == 0) {
	int result = mpfr_rint_floor(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_frac") == 0) {
	int result = mpfr_frac(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_modf") == 0) {
	int result = mpfr_modf(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_remquo") == 0) {
	int result = mpfr_remquo(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_remainder") == 0) {
	int result = mpfr_remainder(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fmod") == 0) {
	int result = mpfr_fmod(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fmodquo") == 0) {
	int result = mpfr_fmodquo(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_ulong_p") == 0) {
	int result = mpfr_fits_ulong_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_slong_p") == 0) {
	int result = mpfr_fits_slong_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_uint_p") == 0) {
	int result = mpfr_fits_uint_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_sint_p") == 0) {
	int result = mpfr_fits_sint_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_ushort_p") == 0) {
	int result = mpfr_fits_ushort_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_sshort_p") == 0) {
	int result = mpfr_fits_sshort_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_uintmax_p") == 0) {
	int result = mpfr_fits_uintmax_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fits_intmax_p") == 0) {
	int result = mpfr_fits_intmax_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_extract") == 0) {
	mpfr_extract(
			(mpz_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned int) jl_unbox_uint32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_swap") == 0) {
	mpfr_swap(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_dump") == 0) {
	mpfr_dump(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_nan_p") == 0) {
	int result = mpfr_nan_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_inf_p") == 0) {
	int result = mpfr_inf_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_number_p") == 0) {
	int result = mpfr_number_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_integer_p") == 0) {
	int result = mpfr_integer_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_zero_p") == 0) {
	int result = mpfr_zero_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_regular_p") == 0) {
	int result = mpfr_regular_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_greater_p") == 0) {
	int result = mpfr_greater_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_greaterequal_p") == 0) {
	int result = mpfr_greaterequal_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_less_p") == 0) {
	int result = mpfr_less_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_lessequal_p") == 0) {
	int result = mpfr_lessequal_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_lessgreater_p") == 0) {
	int result = mpfr_lessgreater_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_equal_p") == 0) {
	int result = mpfr_equal_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_unordered_p") == 0) {
	int result = mpfr_unordered_p(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_atanh") == 0) {
	int result = mpfr_atanh(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_acosh") == 0) {
	int result = mpfr_acosh(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_asinh") == 0) {
	int result = mpfr_asinh(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cosh") == 0) {
	int result = mpfr_cosh(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sinh") == 0) {
	int result = mpfr_sinh(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_tanh") == 0) {
	int result = mpfr_tanh(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sinh_cosh") == 0) {
	int result = mpfr_sinh_cosh(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sech") == 0) {
	int result = mpfr_sech(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_csch") == 0) {
	int result = mpfr_csch(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_coth") == 0) {
	int result = mpfr_coth(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_acos") == 0) {
	int result = mpfr_acos(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_asin") == 0) {
	int result = mpfr_asin(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_atan") == 0) {
	int result = mpfr_atan(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sin") == 0) {
	int result = mpfr_sin(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sin_cos") == 0) {
	int result = mpfr_sin_cos(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cos") == 0) {
	int result = mpfr_cos(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_tan") == 0) {
	int result = mpfr_tan(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_atan2") == 0) {
	int result = mpfr_atan2(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sec") == 0) {
	int result = mpfr_sec(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_csc") == 0) {
	int result = mpfr_csc(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cot") == 0) {
	int result = mpfr_cot(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_hypot") == 0) {
	int result = mpfr_hypot(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_erf") == 0) {
	int result = mpfr_erf(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_erfc") == 0) {
	int result = mpfr_erfc(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cbrt") == 0) {
	int result = mpfr_cbrt(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_root") == 0) {
	int result = mpfr_root(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_rootn_ui") == 0) {
	int result = mpfr_rootn_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_gamma") == 0) {
	int result = mpfr_gamma(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_gamma_inc") == 0) {
	int result = mpfr_gamma_inc(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_beta") == 0) {
	int result = mpfr_beta(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_lngamma") == 0) {
	int result = mpfr_lngamma(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_lgamma") == 0) {
	int result = mpfr_lgamma(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_digamma") == 0) {
	int result = mpfr_digamma(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_zeta") == 0) {
	int result = mpfr_zeta(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_zeta_ui") == 0) {
	int result = mpfr_zeta_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fac_ui") == 0) {
	int result = mpfr_fac_ui(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_j0") == 0) {
	int result = mpfr_j0(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_j1") == 0) {
	int result = mpfr_j1(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_jn") == 0) {
	int result = mpfr_jn(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_y0") == 0) {
	int result = mpfr_y0(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_y1") == 0) {
	int result = mpfr_y1(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_yn") == 0) {
	int result = mpfr_yn(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(long) jl_unbox_long(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_ai") == 0) {
	int result = mpfr_ai(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_min") == 0) {
	int result = mpfr_min(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_max") == 0) {
	int result = mpfr_max(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_dim") == 0) {
	int result = mpfr_dim(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_mul_z") == 0) {
	int result = mpfr_mul_z(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_div_z") == 0) {
	int result = mpfr_div_z(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_add_z") == 0) {
	int result = mpfr_add_z(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sub_z") == 0) {
	int result = mpfr_sub_z(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_z_sub") == 0) {
	int result = mpfr_z_sub(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_cmp_z") == 0) {
	int result = mpfr_cmp_z(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpz_srcptr) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fma") == 0) {
	int result = mpfr_fma(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fms") == 0) {
	int result = mpfr_fms(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fmma") == 0) {
	int result = mpfr_fmma(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[9], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[10], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_fmms") == 0) {
	int result = mpfr_fmms(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[6], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[7], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[8], s)),
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[9], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[10], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_sum") == 0) {
	int result = mpfr_sum(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const mpfr_ptr *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(unsigned long) jl_unbox_uint32(eval_value(args[7], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_free_cache") == 0) {
	mpfr_free_cache();
	return jl_nothing;
} else if (strcmp(target, "mpfr_free_cache2") == 0) {
	mpfr_free_cache2(
			(mpfr_free_cache_t) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_free_pool") == 0) {
	mpfr_free_pool();
	return jl_nothing;
} else if (strcmp(target, "mpfr_mp_memory_cleanup") == 0) {
	int result = mpfr_mp_memory_cleanup();
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_subnormalize") == 0) {
	int result = mpfr_subnormalize(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_strtofr") == 0) {
	int result = mpfr_strtofr(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(char **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(mpfr_rnd_t) jl_unbox_int32(eval_value(args[9], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_round_nearest_away_begin") == 0) {
	mpfr_round_nearest_away_begin(
			(mpfr_t) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_round_nearest_away_end") == 0) {
	int result = mpfr_round_nearest_away_end(
			(mpfr_t) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "mpfr_custom_get_size") == 0) {
	size_t result = mpfr_custom_get_size(
			(mpfr_prec_t) jl_unbox_long(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "mpfr_custom_init") == 0) {
	mpfr_custom_init(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(mpfr_prec_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_custom_get_significand") == 0) {
	void * result = mpfr_custom_get_significand(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "mpfr_custom_get_exp") == 0) {
	mpfr_exp_t result = mpfr_custom_get_exp(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_long(result);
} else if (strcmp(target, "mpfr_custom_move") == 0) {
	mpfr_custom_move(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_custom_init_set") == 0) {
	mpfr_custom_init_set(
			(mpfr_ptr) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(mpfr_exp_t) jl_unbox_long(eval_value(args[7], s)),
			(mpfr_prec_t) jl_unbox_long(eval_value(args[8], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "mpfr_custom_get_kind") == 0) {
	int result = mpfr_custom_get_kind(
			(mpfr_srcptr) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "dsfmt_gen_rand_all") == 0) {
	dsfmt_gen_rand_all(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_fill_array_open_close") == 0) {
	dsfmt_fill_array_open_close(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_fill_array_close_open") == 0) {
	dsfmt_fill_array_close_open(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_fill_array_open_open") == 0) {
	dsfmt_fill_array_open_open(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_fill_array_close1_open2") == 0) {
	dsfmt_fill_array_close1_open2(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(double *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_chk_init_gen_rand") == 0) {
	dsfmt_chk_init_gen_rand(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_chk_init_by_array") == 0) {
	dsfmt_chk_init_by_array(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_get_idstring") == 0) {
	const char * result = dsfmt_get_idstring();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "dsfmt_get_min_array_size") == 0) {
	int result = dsfmt_get_min_array_size();
	return jl_box_int32(result);
} else if (strcmp(target, "dsfmt_genrand_uint32") == 0) {
	uint32_t result = dsfmt_genrand_uint32(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_uint32(result);
} else if (strcmp(target, "dsfmt_genrand_close1_open2") == 0) {
	double result = dsfmt_genrand_close1_open2(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_genrand_close_open") == 0) {
	double result = dsfmt_genrand_close_open(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_genrand_open_close") == 0) {
	double result = dsfmt_genrand_open_close(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_genrand_open_open") == 0) {
	double result = dsfmt_genrand_open_open(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_gv_genrand_uint32") == 0) {
	uint32_t result = dsfmt_gv_genrand_uint32();
	return jl_box_uint32(result);
} else if (strcmp(target, "dsfmt_gv_genrand_close1_open2") == 0) {
	double result = dsfmt_gv_genrand_close1_open2();
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_gv_genrand_close_open") == 0) {
	double result = dsfmt_gv_genrand_close_open();
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_gv_genrand_open_close") == 0) {
	double result = dsfmt_gv_genrand_open_close();
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_gv_genrand_open_open") == 0) {
	double result = dsfmt_gv_genrand_open_open();
	return jl_box_float64(result);
} else if (strcmp(target, "dsfmt_gv_fill_array_open_close") == 0) {
	dsfmt_gv_fill_array_open_close(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_gv_fill_array_close_open") == 0) {
	dsfmt_gv_fill_array_close_open(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_gv_fill_array_open_open") == 0) {
	dsfmt_gv_fill_array_open_open(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_gv_fill_array_close1_open2") == 0) {
	dsfmt_gv_fill_array_close1_open2(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_gv_init_gen_rand") == 0) {
	dsfmt_gv_init_gen_rand(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_gv_init_by_array") == 0) {
	dsfmt_gv_init_by_array(
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_init_gen_rand") == 0) {
	dsfmt_init_gen_rand(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t) jl_unbox_uint32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "dsfmt_init_by_array") == 0) {
	dsfmt_init_by_array(
			(dsfmt_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "get_idstring") == 0) {
	const char * result = get_idstring();
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "get_min_array_size") == 0) {
	int result = get_min_array_size();
	return jl_box_int32(result);
} else if (strcmp(target, "init_gen_rand") == 0) {
	init_gen_rand(
			(uint32_t) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "init_by_array") == 0) {
	init_by_array(
			(uint32_t *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "genrand_close1_open2") == 0) {
	double result = genrand_close1_open2();
	return jl_box_float64(result);
} else if (strcmp(target, "genrand_close_open") == 0) {
	double result = genrand_close_open();
	return jl_box_float64(result);
} else if (strcmp(target, "genrand_open_close") == 0) {
	double result = genrand_open_close();
	return jl_box_float64(result);
} else if (strcmp(target, "genrand_open_open") == 0) {
	double result = genrand_open_open();
	return jl_box_float64(result);
} else if (strcmp(target, "fill_array_open_close") == 0) {
	fill_array_open_close(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "fill_array_close_open") == 0) {
	fill_array_close_open(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "fill_array_open_open") == 0) {
	fill_array_open_open(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "fill_array_close1_open2") == 0) {
	fill_array_close1_open2(
			(double *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(ptrdiff_t) jl_unbox_long(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_run_script") == 0) {
	emscripten_run_script(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_run_script_int") == 0) {
	int result = emscripten_run_script_int(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_run_script_string") == 0) {
	char * result = emscripten_run_script_string(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "emscripten_async_run_script") == 0) {
	emscripten_async_run_script(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_async_load_script") == 0) {
	emscripten_async_load_script(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_callback_func) jl_unbox_voidpointer(eval_value(args[6], s)),
			(em_callback_func) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_main_loop") == 0) {
	emscripten_set_main_loop(
			(em_callback_func) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_main_loop_timing") == 0) {
	int result = emscripten_set_main_loop_timing(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_get_main_loop_timing") == 0) {
	emscripten_get_main_loop_timing(
			(int *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_main_loop_arg") == 0) {
	emscripten_set_main_loop_arg(
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_pause_main_loop") == 0) {
	emscripten_pause_main_loop();
	return jl_nothing;
} else if (strcmp(target, "emscripten_resume_main_loop") == 0) {
	emscripten_resume_main_loop();
	return jl_nothing;
} else if (strcmp(target, "emscripten_cancel_main_loop") == 0) {
	emscripten_cancel_main_loop();
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_socket_error_callback") == 0) {
	emscripten_set_socket_error_callback(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_socket_error_callback) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_socket_open_callback") == 0) {
	emscripten_set_socket_open_callback(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_socket_callback) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_socket_listen_callback") == 0) {
	emscripten_set_socket_listen_callback(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_socket_callback) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_socket_connection_callback") == 0) {
	emscripten_set_socket_connection_callback(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_socket_callback) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_socket_message_callback") == 0) {
	emscripten_set_socket_message_callback(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_socket_callback) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_socket_close_callback") == 0) {
	emscripten_set_socket_close_callback(
			(void *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_socket_callback) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "_emscripten_push_main_loop_blocker") == 0) {
	_emscripten_push_main_loop_blocker(
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "_emscripten_push_uncounted_main_loop_blocker") == 0) {
	_emscripten_push_uncounted_main_loop_blocker(
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_main_loop_expected_blockers") == 0) {
	emscripten_set_main_loop_expected_blockers(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_async_call") == 0) {
	emscripten_async_call(
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_exit_with_live_runtime") == 0) {
	emscripten_exit_with_live_runtime();
	return jl_nothing;
} else if (strcmp(target, "emscripten_force_exit") == 0) {
	emscripten_force_exit(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_get_device_pixel_ratio") == 0) {
	double result = emscripten_get_device_pixel_ratio();
	return jl_box_float64(result);
} else if (strcmp(target, "emscripten_hide_mouse") == 0) {
	emscripten_hide_mouse();
	return jl_nothing;
} else if (strcmp(target, "emscripten_set_canvas_size") == 0) {
	emscripten_set_canvas_size(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_get_canvas_size") == 0) {
	emscripten_get_canvas_size(
			(int *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_get_now") == 0) {
	double result = emscripten_get_now();
	return jl_box_float64(result);
} else if (strcmp(target, "emscripten_random") == 0) {
	float result = emscripten_random();
	return jl_box_float32(result);
} else if (strcmp(target, "emscripten_async_wget") == 0) {
	emscripten_async_wget(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(em_str_callback_func) jl_unbox_voidpointer(eval_value(args[7], s)),
			(em_str_callback_func) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_async_wget_data") == 0) {
	emscripten_async_wget_data(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(em_async_wget_onload_func) jl_unbox_voidpointer(eval_value(args[7], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_async_wget2") == 0) {
	int result = emscripten_async_wget2(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(em_async_wget2_onload_func) jl_unbox_voidpointer(eval_value(args[10], s)),
			(em_async_wget2_onstatus_func) jl_unbox_voidpointer(eval_value(args[11], s)),
			(em_async_wget2_onstatus_func) jl_unbox_voidpointer(eval_value(args[12], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_async_wget2_data") == 0) {
	int result = emscripten_async_wget2_data(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(int) jl_unbox_int32(eval_value(args[9], s)),
			(em_async_wget2_data_onload_func) jl_unbox_voidpointer(eval_value(args[10], s)),
			(em_async_wget2_data_onerror_func) jl_unbox_voidpointer(eval_value(args[11], s)),
			(em_async_wget2_data_onprogress_func) jl_unbox_voidpointer(eval_value(args[12], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_async_wget2_abort") == 0) {
	emscripten_async_wget2_abort(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_wget") == 0) {
	emscripten_wget(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_wget_data") == 0) {
	emscripten_wget_data(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void **) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_async_load") == 0) {
	emscripten_idb_async_load(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(em_async_wget_onload_func) jl_unbox_voidpointer(eval_value(args[8], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_async_store") == 0) {
	emscripten_idb_async_store(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[9], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[10], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[11], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_async_delete") == 0) {
	emscripten_idb_async_delete(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[8], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_async_exists") == 0) {
	emscripten_idb_async_exists(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(em_idb_exists_func) jl_unbox_voidpointer(eval_value(args[8], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_load") == 0) {
	emscripten_idb_load(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void **) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_store") == 0) {
	emscripten_idb_store(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_delete") == 0) {
	emscripten_idb_delete(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_exists") == 0) {
	emscripten_idb_exists(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_load_blob") == 0) {
	emscripten_idb_load_blob(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_store_blob") == 0) {
	emscripten_idb_store_blob(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[9], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_read_from_blob") == 0) {
	emscripten_idb_read_from_blob(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[8], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_idb_free_blob") == 0) {
	emscripten_idb_free_blob(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_run_preload_plugins") == 0) {
	int result = emscripten_run_preload_plugins(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(em_str_callback_func) jl_unbox_voidpointer(eval_value(args[6], s)),
			(em_str_callback_func) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_run_preload_plugins_data") == 0) {
	emscripten_run_preload_plugins_data(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[8], s)),
			(em_run_preload_plugins_data_onload_func) jl_unbox_voidpointer(eval_value(args[9], s)),
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_lazy_load_code") == 0) {
	emscripten_lazy_load_code();
	return jl_nothing;
} else if (strcmp(target, "emscripten_create_worker") == 0) {
	worker_handle result = emscripten_create_worker(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_destroy_worker") == 0) {
	emscripten_destroy_worker(
			(worker_handle) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_call_worker") == 0) {
	emscripten_call_worker(
			(worker_handle) jl_unbox_int32(eval_value(args[5], s)),
			(const char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[7], s)),
			(int) jl_unbox_int32(eval_value(args[8], s)),
			(em_worker_callback_func) jl_unbox_voidpointer(eval_value(args[9], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[10], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_worker_respond") == 0) {
	emscripten_worker_respond(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_worker_respond_provisionally") == 0) {
	emscripten_worker_respond_provisionally(
			(char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int) jl_unbox_int32(eval_value(args[6], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_get_worker_queue_size") == 0) {
	int result = emscripten_get_worker_queue_size(
			(worker_handle) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_get_compiler_setting") == 0) {
	int result = emscripten_get_compiler_setting(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_debugger") == 0) {
	emscripten_debugger();
	return jl_nothing;
} else if (strcmp(target, "emscripten_get_preloaded_image_data") == 0) {
	char * result = emscripten_get_preloaded_image_data(
			(const char *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "emscripten_get_preloaded_image_data_from_FILE") == 0) {
	char * result = emscripten_get_preloaded_image_data_from_FILE(
			(FILE *) jl_unbox_voidpointer(eval_value(args[5], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int *) jl_unbox_voidpointer(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "emscripten_log") == 0) {
	emscripten_log(
			(int) jl_unbox_int32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_get_callstack") == 0) {
	int result = emscripten_get_callstack(
			(int) jl_unbox_int32(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_print_double") == 0) {
	int result = emscripten_print_double(
			(double) jl_unbox_float64(eval_value(args[5], s)),
			(char *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_scan_registers") == 0) {
	emscripten_scan_registers(
			(em_scan_func) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_scan_stack") == 0) {
	emscripten_scan_stack(
			(em_scan_func) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_sleep") == 0) {
	emscripten_sleep(
			(unsigned int) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_sleep_with_yield") == 0) {
	emscripten_sleep_with_yield(
			(unsigned int) jl_unbox_uint32(eval_value(args[5], s))
		);
	return jl_nothing;
} else if (strcmp(target, "emscripten_coroutine_create") == 0) {
	emscripten_coroutine result = emscripten_coroutine_create(
			(em_arg_callback_func) jl_unbox_voidpointer(eval_value(args[5], s)),
			(void *) jl_unbox_voidpointer(eval_value(args[6], s)),
			(int) jl_unbox_int32(eval_value(args[7], s))
		);
	jl_ptls_t ptls = jl_get_ptls_states();
	jl_datatype_t *rt = eval_value(args[1], s);
	rt = instantiate_foreigncall_rt(rt, s);
	jl_value_t *v = jl_gc_alloc(ptls, sizeof(void*), rt);
	*(void**)jl_data_ptr(v) = (void*)result;
	return v;

} else if (strcmp(target, "emscripten_coroutine_next") == 0) {
	int result = emscripten_coroutine_next(
			(emscripten_coroutine) jl_unbox_voidpointer(eval_value(args[5], s))
		);
	return jl_box_int32(result);
} else if (strcmp(target, "emscripten_yield") == 0) {
	emscripten_yield();
	return jl_nothing;
} 

return NULL;
}

# define WRAP(symname) \
else if (strcmp(target, #symname) == 0) { \
    return (void*)&symname; \
}

extern uint32_t __gmp_version;
extern int32_t __gmp_bits_per_limb;
#include <stdio.h>

void *get_foreigncall_fptr(const char *target, const char *libname) {
if (0) {
} else if (strcmp(target, "srand") == 0) {
return (void*)&srand; } 
else if (strcmp(target, "getenv") == 0) {
return (void*)&getenv; } 
else if (strcmp(target, "setenv") == 0) {
return (void*)&setenv; } 
else if (strcmp(target, "memcpy") == 0) {
return (void*)&memcpy; } 
else if (strcmp(target, "memmove") == 0) {
return (void*)&memmove; } 
else if (strcmp(target, "memset") == 0) {
return (void*)&memset; } 
else if (strcmp(target, "memcmp") == 0) {
return (void*)&memcmp; } 
else if (strcmp(target, "memchr") == 0) {
return (void*)&memchr; } 
else if (strcmp(target, "strlen") == 0) {
return (void*)&strlen; } 
else if (strcmp(target, "uint2str") == 0) {
return (void*)&uint2str; } 
else if (strcmp(target, "u8_offset") == 0) {
return (void*)&u8_offset; } 
else if (strcmp(target, "u8_charnum") == 0) {
return (void*)&u8_charnum; } 
else if (strcmp(target, "u8_strwidth") == 0) {
return (void*)&u8_strwidth; } 
else if (strcmp(target, "u8_isvalid") == 0) {
return (void*)&u8_isvalid; } 
else if (strcmp(target, "ios_read") == 0) {
return (void*)&ios_read; } 
else if (strcmp(target, "ios_readall") == 0) {
return (void*)&ios_readall; } 
else if (strcmp(target, "ios_write") == 0) {
return (void*)&ios_write; } 
else if (strcmp(target, "ios_seek") == 0) {
return (void*)&ios_seek; } 
else if (strcmp(target, "ios_seek_end") == 0) {
return (void*)&ios_seek_end; } 
else if (strcmp(target, "ios_skip") == 0) {
return (void*)&ios_skip; } 
else if (strcmp(target, "ios_pos") == 0) {
return (void*)&ios_pos; } 
else if (strcmp(target, "ios_trunc") == 0) {
return (void*)&ios_trunc; } 
else if (strcmp(target, "ios_eof") == 0) {
return (void*)&ios_eof; } 
else if (strcmp(target, "ios_eof_blocking") == 0) {
return (void*)&ios_eof_blocking; } 
else if (strcmp(target, "ios_flush") == 0) {
return (void*)&ios_flush; } 
else if (strcmp(target, "ios_close") == 0) {
return (void*)&ios_close; } 
else if (strcmp(target, "ios_isopen") == 0) {
return (void*)&ios_isopen; } 
else if (strcmp(target, "ios_take_buffer") == 0) {
return (void*)&ios_take_buffer; } 
else if (strcmp(target, "ios_setbuf") == 0) {
return (void*)&ios_setbuf; } 
else if (strcmp(target, "ios_bufmode") == 0) {
return (void*)&ios_bufmode; } 
else if (strcmp(target, "ios_get_readable") == 0) {
return (void*)&ios_get_readable; } 
else if (strcmp(target, "ios_get_writable") == 0) {
return (void*)&ios_get_writable; } 
else if (strcmp(target, "ios_set_readonly") == 0) {
return (void*)&ios_set_readonly; } 
else if (strcmp(target, "ios_copy") == 0) {
return (void*)&ios_copy; } 
else if (strcmp(target, "ios_copyall") == 0) {
return (void*)&ios_copyall; } 
else if (strcmp(target, "ios_copyuntil") == 0) {
return (void*)&ios_copyuntil; } 
else if (strcmp(target, "ios_nchomp") == 0) {
return (void*)&ios_nchomp; } 
else if (strcmp(target, "ios_readprep") == 0) {
return (void*)&ios_readprep; } 
else if (strcmp(target, "ios_file") == 0) {
return (void*)&ios_file; } 
else if (strcmp(target, "ios_mkstemp") == 0) {
return (void*)&ios_mkstemp; } 
else if (strcmp(target, "ios_mem") == 0) {
return (void*)&ios_mem; } 
else if (strcmp(target, "ios_fd") == 0) {
return (void*)&ios_fd; } 
else if (strcmp(target, "ios_pututf8") == 0) {
return (void*)&ios_pututf8; } 
else if (strcmp(target, "ios_printf") == 0) {
return (void*)&ios_printf; } 
else if (strcmp(target, "ios_getutf8") == 0) {
return (void*)&ios_getutf8; } 
else if (strcmp(target, "ios_peekutf8") == 0) {
return (void*)&ios_peekutf8; } 
else if (strcmp(target, "ios_readline") == 0) {
return (void*)&ios_readline; } 
else if (strcmp(target, "ios_purge") == 0) {
return (void*)&ios_purge; } 
else if (strcmp(target, "ios_putc") == 0) {
return (void*)&ios_putc; } 
else if (strcmp(target, "ios_getc") == 0) {
return (void*)&ios_getc; } 
else if (strcmp(target, "ios_peekc") == 0) {
return (void*)&ios_peekc; } 
else if (strcmp(target, "jl_gettimeofday") == 0) {
return (void*)&jl_gettimeofday; } 
else if (strcmp(target, "jl_clock_now") == 0) {
return (void*)&jl_clock_now; } 
else if (strcmp(target, "int32hash") == 0) {
return (void*)&int32hash; } 
else if (strcmp(target, "int64hash") == 0) {
return (void*)&int64hash; } 
else if (strcmp(target, "int64to32hash") == 0) {
return (void*)&int64to32hash; } 
else if (strcmp(target, "memhash") == 0) {
return (void*)&memhash; } 
else if (strcmp(target, "memhash_seed") == 0) {
return (void*)&memhash_seed; } 
else if (strcmp(target, "memhash32") == 0) {
return (void*)&memhash32; } 
else if (strcmp(target, "memhash32_seed") == 0) {
return (void*)&memhash32_seed; } 
else if (strcmp(target, "bitvector_new") == 0) {
return (void*)&bitvector_new; } 
else if (strcmp(target, "bitvector_resize") == 0) {
return (void*)&bitvector_resize; } 
else if (strcmp(target, "bitvector_set") == 0) {
return (void*)&bitvector_set; } 
else if (strcmp(target, "bitvector_get") == 0) {
return (void*)&bitvector_get; } 
else if (strcmp(target, "jl_strtod_c") == 0) {
return (void*)&jl_strtod_c; } 
else if (strcmp(target, "jl_strtof_c") == 0) {
return (void*)&jl_strtof_c; } 
else if (strcmp(target, "libsupport_init") == 0) {
return (void*)&libsupport_init; } 
else if (strcmp(target, "jl_threadid") == 0) {
return (void*)&jl_threadid; } 
else if (strcmp(target, "jl_threading_profile") == 0) {
return (void*)&jl_threading_profile; } 
else if (strcmp(target, "jl_get_ptls_states") == 0) {
return (void*)&jl_get_ptls_states; } 
else if (strcmp(target, "jl_cpu_pause") == 0) {
return (void*)&jl_cpu_pause; } 
else if (strcmp(target, "jl_cpu_wake") == 0) {
return (void*)&jl_cpu_wake; } 
else if (strcmp(target, "jl_gc_safepoint") == 0) {
return (void*)&jl_gc_safepoint; } 
else if (strcmp(target, "jl_gc_enable_finalizers") == 0) {
return (void*)&jl_gc_enable_finalizers; } 
else if (strcmp(target, "jl_wakeup_thread") == 0) {
return (void*)&jl_wakeup_thread; } 
else if (strcmp(target, "jl_gc_enable") == 0) {
return (void*)&jl_gc_enable; } 
else if (strcmp(target, "jl_gc_is_enabled") == 0) {
return (void*)&jl_gc_is_enabled; } 
else if (strcmp(target, "jl_gc_collect") == 0) {
return (void*)&jl_gc_collect; } 
else if (strcmp(target, "jl_gc_add_finalizer") == 0) {
return (void*)&jl_gc_add_finalizer; } 
else if (strcmp(target, "jl_finalize") == 0) {
return (void*)&jl_finalize; } 
else if (strcmp(target, "jl_gc_new_weakref") == 0) {
return (void*)&jl_gc_new_weakref; } 
else if (strcmp(target, "jl_gc_alloc_0w") == 0) {
return (void*)&jl_gc_alloc_0w; } 
else if (strcmp(target, "jl_gc_alloc_1w") == 0) {
return (void*)&jl_gc_alloc_1w; } 
else if (strcmp(target, "jl_gc_alloc_2w") == 0) {
return (void*)&jl_gc_alloc_2w; } 
else if (strcmp(target, "jl_gc_alloc_3w") == 0) {
return (void*)&jl_gc_alloc_3w; } 
else if (strcmp(target, "jl_gc_allocobj") == 0) {
return (void*)&jl_gc_allocobj; } 
else if (strcmp(target, "jl_malloc_stack") == 0) {
return (void*)&jl_malloc_stack; } 
else if (strcmp(target, "jl_free_stack") == 0) {
return (void*)&jl_free_stack; } 
else if (strcmp(target, "jl_gc_use") == 0) {
return (void*)&jl_gc_use; } 
else if (strcmp(target, "jl_clear_malloc_data") == 0) {
return (void*)&jl_clear_malloc_data; } 
else if (strcmp(target, "jl_gc_queue_root") == 0) {
return (void*)&jl_gc_queue_root; } 
else if (strcmp(target, "jl_gc_managed_malloc") == 0) {
return (void*)&jl_gc_managed_malloc; } 
else if (strcmp(target, "jl_gc_managed_realloc") == 0) {
return (void*)&jl_gc_managed_realloc; } 
else if (strcmp(target, "jl_array_typetagdata") == 0) {
return (void*)&jl_array_typetagdata; } 
else if (strcmp(target, "jl_compute_fieldtypes") == 0) {
return (void*)&jl_compute_fieldtypes; } 
else if (strcmp(target, "jl_subtype") == 0) {
return (void*)&jl_subtype; } 
else if (strcmp(target, "jl_egal") == 0) {
return (void*)&jl_egal; } 
else if (strcmp(target, "jl_object_id") == 0) {
return (void*)&jl_object_id; } 
else if (strcmp(target, "jl_has_free_typevars") == 0) {
return (void*)&jl_has_free_typevars; } 
else if (strcmp(target, "jl_has_typevar") == 0) {
return (void*)&jl_has_typevar; } 
else if (strcmp(target, "jl_has_typevar_from_unionall") == 0) {
return (void*)&jl_has_typevar_from_unionall; } 
else if (strcmp(target, "jl_subtype_env_size") == 0) {
return (void*)&jl_subtype_env_size; } 
else if (strcmp(target, "jl_subtype_env") == 0) {
return (void*)&jl_subtype_env; } 
else if (strcmp(target, "jl_isa") == 0) {
return (void*)&jl_isa; } 
else if (strcmp(target, "jl_types_equal") == 0) {
return (void*)&jl_types_equal; } 
else if (strcmp(target, "jl_is_not_broken_subtype") == 0) {
return (void*)&jl_is_not_broken_subtype; } 
else if (strcmp(target, "jl_type_union") == 0) {
return (void*)&jl_type_union; } 
else if (strcmp(target, "jl_type_intersection") == 0) {
return (void*)&jl_type_intersection; } 
else if (strcmp(target, "jl_has_empty_intersection") == 0) {
return (void*)&jl_has_empty_intersection; } 
else if (strcmp(target, "jl_type_unionall") == 0) {
return (void*)&jl_type_unionall; } 
else if (strcmp(target, "jl_typename_str") == 0) {
return (void*)&jl_typename_str; } 
else if (strcmp(target, "jl_typeof_str") == 0) {
return (void*)&jl_typeof_str; } 
else if (strcmp(target, "jl_type_morespecific") == 0) {
return (void*)&jl_type_morespecific; } 
else if (strcmp(target, "jl_new_typename_in") == 0) {
return (void*)&jl_new_typename_in; } 
else if (strcmp(target, "jl_new_typevar") == 0) {
return (void*)&jl_new_typevar; } 
else if (strcmp(target, "jl_instantiate_unionall") == 0) {
return (void*)&jl_instantiate_unionall; } 
else if (strcmp(target, "jl_apply_type") == 0) {
return (void*)&jl_apply_type; } 
else if (strcmp(target, "jl_apply_type1") == 0) {
return (void*)&jl_apply_type1; } 
else if (strcmp(target, "jl_apply_type2") == 0) {
return (void*)&jl_apply_type2; } 
else if (strcmp(target, "jl_apply_tuple_type") == 0) {
return (void*)&jl_apply_tuple_type; } 
else if (strcmp(target, "jl_apply_tuple_type_v") == 0) {
return (void*)&jl_apply_tuple_type_v; } 
else if (strcmp(target, "jl_new_datatype") == 0) {
return (void*)&jl_new_datatype; } 
else if (strcmp(target, "jl_new_primitivetype") == 0) {
return (void*)&jl_new_primitivetype; } 
else if (strcmp(target, "jl_new_bits") == 0) {
return (void*)&jl_new_bits; } 
else if (strcmp(target, "jl_new_struct") == 0) {
return (void*)&jl_new_struct; } 
else if (strcmp(target, "jl_new_structv") == 0) {
return (void*)&jl_new_structv; } 
else if (strcmp(target, "jl_new_structt") == 0) {
return (void*)&jl_new_structt; } 
else if (strcmp(target, "jl_new_struct_uninit") == 0) {
return (void*)&jl_new_struct_uninit; } 
else if (strcmp(target, "jl_new_method_instance_uninit") == 0) {
return (void*)&jl_new_method_instance_uninit; } 
else if (strcmp(target, "jl_svec") == 0) {
return (void*)&jl_svec; } 
else if (strcmp(target, "jl_svec1") == 0) {
return (void*)&jl_svec1; } 
else if (strcmp(target, "jl_svec2") == 0) {
return (void*)&jl_svec2; } 
else if (strcmp(target, "jl_alloc_svec") == 0) {
return (void*)&jl_alloc_svec; } 
else if (strcmp(target, "jl_alloc_svec_uninit") == 0) {
return (void*)&jl_alloc_svec_uninit; } 
else if (strcmp(target, "jl_svec_copy") == 0) {
return (void*)&jl_svec_copy; } 
else if (strcmp(target, "jl_svec_fill") == 0) {
return (void*)&jl_svec_fill; } 
else if (strcmp(target, "jl_tupletype_fill") == 0) {
return (void*)&jl_tupletype_fill; } 
else if (strcmp(target, "jl_symbol") == 0) {
return (void*)&jl_symbol; } 
else if (strcmp(target, "jl_symbol_lookup") == 0) {
return (void*)&jl_symbol_lookup; } 
else if (strcmp(target, "jl_symbol_n") == 0) {
return (void*)&jl_symbol_n; } 
else if (strcmp(target, "jl_gensym") == 0) {
return (void*)&jl_gensym; } 
else if (strcmp(target, "jl_tagged_gensym") == 0) {
return (void*)&jl_tagged_gensym; } 
else if (strcmp(target, "jl_get_root_symbol") == 0) {
return (void*)&jl_get_root_symbol; } 
else if (strcmp(target, "jl_generic_function_def") == 0) {
return (void*)&jl_generic_function_def; } 
else if (strcmp(target, "jl_method_def") == 0) {
return (void*)&jl_method_def; } 
else if (strcmp(target, "jl_code_for_staged") == 0) {
return (void*)&jl_code_for_staged; } 
else if (strcmp(target, "jl_copy_code_info") == 0) {
return (void*)&jl_copy_code_info; } 
else if (strcmp(target, "jl_get_world_counter") == 0) {
return (void*)&jl_get_world_counter; } 
else if (strcmp(target, "jl_get_kwsorter") == 0) {
return (void*)&jl_get_kwsorter; } 
else if (strcmp(target, "jl_box_bool") == 0) {
return (void*)&jl_box_bool; } 
else if (strcmp(target, "jl_box_int8") == 0) {
return (void*)&jl_box_int8; } 
else if (strcmp(target, "jl_box_uint8") == 0) {
return (void*)&jl_box_uint8; } 
else if (strcmp(target, "jl_box_int16") == 0) {
return (void*)&jl_box_int16; } 
else if (strcmp(target, "jl_box_uint16") == 0) {
return (void*)&jl_box_uint16; } 
else if (strcmp(target, "jl_box_int32") == 0) {
return (void*)&jl_box_int32; } 
else if (strcmp(target, "jl_box_uint32") == 0) {
return (void*)&jl_box_uint32; } 
else if (strcmp(target, "jl_box_char") == 0) {
return (void*)&jl_box_char; } 
else if (strcmp(target, "jl_box_int64") == 0) {
return (void*)&jl_box_int64; } 
else if (strcmp(target, "jl_box_uint64") == 0) {
return (void*)&jl_box_uint64; } 
else if (strcmp(target, "jl_box_float32") == 0) {
return (void*)&jl_box_float32; } 
else if (strcmp(target, "jl_box_float64") == 0) {
return (void*)&jl_box_float64; } 
else if (strcmp(target, "jl_box_voidpointer") == 0) {
return (void*)&jl_box_voidpointer; } 
else if (strcmp(target, "jl_box_ssavalue") == 0) {
return (void*)&jl_box_ssavalue; } 
else if (strcmp(target, "jl_box_slotnumber") == 0) {
return (void*)&jl_box_slotnumber; } 
else if (strcmp(target, "jl_unbox_bool") == 0) {
return (void*)&jl_unbox_bool; } 
else if (strcmp(target, "jl_unbox_int8") == 0) {
return (void*)&jl_unbox_int8; } 
else if (strcmp(target, "jl_unbox_uint8") == 0) {
return (void*)&jl_unbox_uint8; } 
else if (strcmp(target, "jl_unbox_int16") == 0) {
return (void*)&jl_unbox_int16; } 
else if (strcmp(target, "jl_unbox_uint16") == 0) {
return (void*)&jl_unbox_uint16; } 
else if (strcmp(target, "jl_unbox_int32") == 0) {
return (void*)&jl_unbox_int32; } 
else if (strcmp(target, "jl_unbox_uint32") == 0) {
return (void*)&jl_unbox_uint32; } 
else if (strcmp(target, "jl_unbox_int64") == 0) {
return (void*)&jl_unbox_int64; } 
else if (strcmp(target, "jl_unbox_uint64") == 0) {
return (void*)&jl_unbox_uint64; } 
else if (strcmp(target, "jl_unbox_float32") == 0) {
return (void*)&jl_unbox_float32; } 
else if (strcmp(target, "jl_unbox_float64") == 0) {
return (void*)&jl_unbox_float64; } 
else if (strcmp(target, "jl_unbox_voidpointer") == 0) {
return (void*)&jl_unbox_voidpointer; } 
else if (strcmp(target, "jl_get_size") == 0) {
return (void*)&jl_get_size; } 
else if (strcmp(target, "jl_field_index") == 0) {
return (void*)&jl_field_index; } 
else if (strcmp(target, "jl_get_nth_field") == 0) {
return (void*)&jl_get_nth_field; } 
else if (strcmp(target, "jl_get_nth_field_noalloc") == 0) {
return (void*)&jl_get_nth_field_noalloc; } 
else if (strcmp(target, "jl_get_nth_field_checked") == 0) {
return (void*)&jl_get_nth_field_checked; } 
else if (strcmp(target, "jl_set_nth_field") == 0) {
return (void*)&jl_set_nth_field; } 
else if (strcmp(target, "jl_field_isdefined") == 0) {
return (void*)&jl_field_isdefined; } 
else if (strcmp(target, "jl_get_field") == 0) {
return (void*)&jl_get_field; } 
else if (strcmp(target, "jl_value_ptr") == 0) {
return (void*)&jl_value_ptr; } 
else if (strcmp(target, "jl_islayout_inline") == 0) {
return (void*)&jl_islayout_inline; } 
else if (strcmp(target, "jl_new_array") == 0) {
return (void*)&jl_new_array; } 
else if (strcmp(target, "jl_reshape_array") == 0) {
return (void*)&jl_reshape_array; } 
else if (strcmp(target, "jl_ptr_to_array_1d") == 0) {
return (void*)&jl_ptr_to_array_1d; } 
else if (strcmp(target, "jl_ptr_to_array") == 0) {
return (void*)&jl_ptr_to_array; } 
else if (strcmp(target, "jl_alloc_array_1d") == 0) {
return (void*)&jl_alloc_array_1d; } 
else if (strcmp(target, "jl_alloc_array_2d") == 0) {
return (void*)&jl_alloc_array_2d; } 
else if (strcmp(target, "jl_alloc_array_3d") == 0) {
return (void*)&jl_alloc_array_3d; } 
else if (strcmp(target, "jl_pchar_to_array") == 0) {
return (void*)&jl_pchar_to_array; } 
else if (strcmp(target, "jl_pchar_to_string") == 0) {
return (void*)&jl_pchar_to_string; } 
else if (strcmp(target, "jl_cstr_to_string") == 0) {
return (void*)&jl_cstr_to_string; } 
else if (strcmp(target, "jl_alloc_string") == 0) {
return (void*)&jl_alloc_string; } 
else if (strcmp(target, "jl_array_to_string") == 0) {
return (void*)&jl_array_to_string; } 
else if (strcmp(target, "jl_alloc_vec_any") == 0) {
return (void*)&jl_alloc_vec_any; } 
else if (strcmp(target, "jl_arrayref") == 0) {
return (void*)&jl_arrayref; } 
else if (strcmp(target, "jl_ptrarrayref") == 0) {
return (void*)&jl_ptrarrayref; } 
else if (strcmp(target, "jl_arrayset") == 0) {
return (void*)&jl_arrayset; } 
else if (strcmp(target, "jl_arrayunset") == 0) {
return (void*)&jl_arrayunset; } 
else if (strcmp(target, "jl_array_isassigned") == 0) {
return (void*)&jl_array_isassigned; } 
else if (strcmp(target, "jl_array_grow_end") == 0) {
return (void*)&jl_array_grow_end; } 
else if (strcmp(target, "jl_array_del_end") == 0) {
return (void*)&jl_array_del_end; } 
else if (strcmp(target, "jl_array_grow_beg") == 0) {
return (void*)&jl_array_grow_beg; } 
else if (strcmp(target, "jl_array_del_beg") == 0) {
return (void*)&jl_array_del_beg; } 
else if (strcmp(target, "jl_array_sizehint") == 0) {
return (void*)&jl_array_sizehint; } 
else if (strcmp(target, "jl_array_ptr_1d_push") == 0) {
return (void*)&jl_array_ptr_1d_push; } 
else if (strcmp(target, "jl_array_ptr_1d_append") == 0) {
return (void*)&jl_array_ptr_1d_append; } 
else if (strcmp(target, "jl_apply_array_type") == 0) {
return (void*)&jl_apply_array_type; } 
else if (strcmp(target, "jl_array_ptr") == 0) {
return (void*)&jl_array_ptr; } 
else if (strcmp(target, "jl_array_eltype") == 0) {
return (void*)&jl_array_eltype; } 
else if (strcmp(target, "jl_array_rank") == 0) {
return (void*)&jl_array_rank; } 
else if (strcmp(target, "jl_array_size") == 0) {
return (void*)&jl_array_size; } 
else if (strcmp(target, "jl_string_ptr") == 0) {
return (void*)&jl_string_ptr; } 
else if (strcmp(target, "jl_new_module") == 0) {
return (void*)&jl_new_module; } 
else if (strcmp(target, "jl_set_module_nospecialize") == 0) {
return (void*)&jl_set_module_nospecialize; } 
else if (strcmp(target, "jl_get_binding") == 0) {
return (void*)&jl_get_binding; } 
else if (strcmp(target, "jl_get_binding_or_error") == 0) {
return (void*)&jl_get_binding_or_error; } 
else if (strcmp(target, "jl_module_globalref") == 0) {
return (void*)&jl_module_globalref; } 
else if (strcmp(target, "jl_get_binding_wr") == 0) {
return (void*)&jl_get_binding_wr; } 
else if (strcmp(target, "jl_get_binding_for_method_def") == 0) {
return (void*)&jl_get_binding_for_method_def; } 
else if (strcmp(target, "jl_boundp") == 0) {
return (void*)&jl_boundp; } 
else if (strcmp(target, "jl_defines_or_exports_p") == 0) {
return (void*)&jl_defines_or_exports_p; } 
else if (strcmp(target, "jl_binding_resolved_p") == 0) {
return (void*)&jl_binding_resolved_p; } 
else if (strcmp(target, "jl_is_const") == 0) {
return (void*)&jl_is_const; } 
else if (strcmp(target, "jl_get_global") == 0) {
return (void*)&jl_get_global; } 
else if (strcmp(target, "jl_set_global") == 0) {
return (void*)&jl_set_global; } 
else if (strcmp(target, "jl_set_const") == 0) {
return (void*)&jl_set_const; } 
else if (strcmp(target, "jl_checked_assignment") == 0) {
return (void*)&jl_checked_assignment; } 
else if (strcmp(target, "jl_declare_constant") == 0) {
return (void*)&jl_declare_constant; } 
else if (strcmp(target, "jl_module_using") == 0) {
return (void*)&jl_module_using; } 
else if (strcmp(target, "jl_module_use") == 0) {
return (void*)&jl_module_use; } 
else if (strcmp(target, "jl_module_import") == 0) {
return (void*)&jl_module_import; } 
else if (strcmp(target, "jl_module_export") == 0) {
return (void*)&jl_module_export; } 
else if (strcmp(target, "jl_is_imported") == 0) {
return (void*)&jl_is_imported; } 
else if (strcmp(target, "jl_module_exports_p") == 0) {
return (void*)&jl_module_exports_p; } 
else if (strcmp(target, "jl_add_standard_imports") == 0) {
return (void*)&jl_add_standard_imports; } 
else if (strcmp(target, "jl_eqtable_put") == 0) {
return (void*)&jl_eqtable_put; } 
else if (strcmp(target, "jl_eqtable_get") == 0) {
return (void*)&jl_eqtable_get; } 
else if (strcmp(target, "jl_errno") == 0) {
return (void*)&jl_errno; } 
else if (strcmp(target, "jl_set_errno") == 0) {
return (void*)&jl_set_errno; } 
else if (strcmp(target, "jl_stat") == 0) {
return (void*)&jl_stat; } 
else if (strcmp(target, "jl_cpu_threads") == 0) {
return (void*)&jl_cpu_threads; } 
else if (strcmp(target, "jl_getpagesize") == 0) {
return (void*)&jl_getpagesize; } 
else if (strcmp(target, "jl_getallocationgranularity") == 0) {
return (void*)&jl_getallocationgranularity; } 
else if (strcmp(target, "jl_is_debugbuild") == 0) {
return (void*)&jl_is_debugbuild; } 
else if (strcmp(target, "jl_get_UNAME") == 0) {
return (void*)&jl_get_UNAME; } 
else if (strcmp(target, "jl_get_ARCH") == 0) {
return (void*)&jl_get_ARCH; } 
else if (strcmp(target, "jl_environ") == 0) {
return (void*)&jl_environ; } 
else if (strcmp(target, "jl_error") == 0) {
return (void*)&jl_error; } 
else if (strcmp(target, "jl_errorf") == 0) {
return (void*)&jl_errorf; } 
else if (strcmp(target, "jl_exceptionf") == 0) {
return (void*)&jl_exceptionf; } 
else if (strcmp(target, "jl_too_few_args") == 0) {
return (void*)&jl_too_few_args; } 
else if (strcmp(target, "jl_too_many_args") == 0) {
return (void*)&jl_too_many_args; } 
else if (strcmp(target, "jl_type_error") == 0) {
return (void*)&jl_type_error; } 
else if (strcmp(target, "jl_type_error_rt") == 0) {
return (void*)&jl_type_error_rt; } 
else if (strcmp(target, "jl_undefined_var_error") == 0) {
return (void*)&jl_undefined_var_error; } 
else if (strcmp(target, "jl_bounds_error") == 0) {
return (void*)&jl_bounds_error; } 
else if (strcmp(target, "jl_bounds_error_v") == 0) {
return (void*)&jl_bounds_error_v; } 
else if (strcmp(target, "jl_bounds_error_int") == 0) {
return (void*)&jl_bounds_error_int; } 
else if (strcmp(target, "jl_bounds_error_tuple_int") == 0) {
return (void*)&jl_bounds_error_tuple_int; } 
else if (strcmp(target, "jl_bounds_error_unboxed_int") == 0) {
return (void*)&jl_bounds_error_unboxed_int; } 
else if (strcmp(target, "jl_bounds_error_ints") == 0) {
return (void*)&jl_bounds_error_ints; } 
else if (strcmp(target, "jl_eof_error") == 0) {
return (void*)&jl_eof_error; } 
else if (strcmp(target, "jl_current_exception") == 0) {
return (void*)&jl_current_exception; } 
else if (strcmp(target, "jl_exception_occurred") == 0) {
return (void*)&jl_exception_occurred; } 
else if (strcmp(target, "jl_exception_clear") == 0) {
return (void*)&jl_exception_clear; } 
else if (strcmp(target, "julia_init") == 0) {
return (void*)&julia_init; } 
else if (strcmp(target, "jl_init") == 0) {
return (void*)&jl_init; } 
else if (strcmp(target, "jl_init_with_image") == 0) {
return (void*)&jl_init_with_image; } 
else if (strcmp(target, "jl_get_default_sysimg_path") == 0) {
return (void*)&jl_get_default_sysimg_path; } 
else if (strcmp(target, "jl_is_initialized") == 0) {
return (void*)&jl_is_initialized; } 
else if (strcmp(target, "jl_atexit_hook") == 0) {
return (void*)&jl_atexit_hook; } 
else if (strcmp(target, "jl_exit") == 0) {
return (void*)&jl_exit; } 
else if (strcmp(target, "jl_pathname_for_handle") == 0) {
return (void*)&jl_pathname_for_handle; } 
else if (strcmp(target, "jl_deserialize_verify_header") == 0) {
return (void*)&jl_deserialize_verify_header; } 
else if (strcmp(target, "jl_preload_sysimg_so") == 0) {
return (void*)&jl_preload_sysimg_so; } 
else if (strcmp(target, "jl_set_sysimg_so") == 0) {
return (void*)&jl_set_sysimg_so; } 
else if (strcmp(target, "jl_create_system_image") == 0) {
return (void*)&jl_create_system_image; } 
else if (strcmp(target, "jl_save_system_image") == 0) {
return (void*)&jl_save_system_image; } 
else if (strcmp(target, "jl_restore_system_image") == 0) {
return (void*)&jl_restore_system_image; } 
else if (strcmp(target, "jl_restore_system_image_data") == 0) {
return (void*)&jl_restore_system_image_data; } 
else if (strcmp(target, "jl_save_incremental") == 0) {
return (void*)&jl_save_incremental; } 
else if (strcmp(target, "jl_restore_incremental") == 0) {
return (void*)&jl_restore_incremental; } 
else if (strcmp(target, "jl_restore_incremental_from_buf") == 0) {
return (void*)&jl_restore_incremental_from_buf; } 
else if (strcmp(target, "jl_parse_all") == 0) {
return (void*)&jl_parse_all; } 
else if (strcmp(target, "jl_parse_input_line") == 0) {
return (void*)&jl_parse_input_line; } 
else if (strcmp(target, "jl_parse_string") == 0) {
return (void*)&jl_parse_string; } 
else if (strcmp(target, "jl_load_file_string") == 0) {
return (void*)&jl_load_file_string; } 
else if (strcmp(target, "jl_expand") == 0) {
return (void*)&jl_expand; } 
else if (strcmp(target, "jl_expand_with_loc") == 0) {
return (void*)&jl_expand_with_loc; } 
else if (strcmp(target, "jl_expand_stmt") == 0) {
return (void*)&jl_expand_stmt; } 
else if (strcmp(target, "jl_expand_stmt_with_loc") == 0) {
return (void*)&jl_expand_stmt_with_loc; } 
else if (strcmp(target, "jl_eval_string") == 0) {
return (void*)&jl_eval_string; } 
else if (strcmp(target, "jl_load_dynamic_library") == 0) {
return (void*)&jl_load_dynamic_library; } 
else if (strcmp(target, "jl_dlopen") == 0) {
return (void*)&jl_dlopen; } 
else if (strcmp(target, "jl_dlclose") == 0) {
return (void*)&jl_dlclose; } 
else if (strcmp(target, "jl_dlsym") == 0) {
return (void*)&jl_dlsym; } 
else if (strcmp(target, "jl_toplevel_eval") == 0) {
return (void*)&jl_toplevel_eval; } 
else if (strcmp(target, "jl_toplevel_eval_in") == 0) {
return (void*)&jl_toplevel_eval_in; } 
else if (strcmp(target, "jl_load") == 0) {
return (void*)&jl_load; } 
else if (strcmp(target, "jl_base_relative_to") == 0) {
return (void*)&jl_base_relative_to; } 
else if (strcmp(target, "jl_register_newmeth_tracer") == 0) {
return (void*)&jl_register_newmeth_tracer; } 
else if (strcmp(target, "jl_copy_ast") == 0) {
return (void*)&jl_copy_ast; } 
else if (strcmp(target, "jl_compress_ast") == 0) {
return (void*)&jl_compress_ast; } 
else if (strcmp(target, "jl_uncompress_ast") == 0) {
return (void*)&jl_uncompress_ast; } 
else if (strcmp(target, "jl_ast_flag_inferred") == 0) {
return (void*)&jl_ast_flag_inferred; } 
else if (strcmp(target, "jl_ast_flag_inlineable") == 0) {
return (void*)&jl_ast_flag_inlineable; } 
else if (strcmp(target, "jl_ast_flag_pure") == 0) {
return (void*)&jl_ast_flag_pure; } 
else if (strcmp(target, "jl_ast_nslots") == 0) {
return (void*)&jl_ast_nslots; } 
else if (strcmp(target, "jl_ast_slotflag") == 0) {
return (void*)&jl_ast_slotflag; } 
else if (strcmp(target, "jl_compress_argnames") == 0) {
return (void*)&jl_compress_argnames; } 
else if (strcmp(target, "jl_uncompress_argnames") == 0) {
return (void*)&jl_uncompress_argnames; } 
else if (strcmp(target, "jl_uncompress_argname_n") == 0) {
return (void*)&jl_uncompress_argname_n; } 
else if (strcmp(target, "jl_is_operator") == 0) {
return (void*)&jl_is_operator; } 
else if (strcmp(target, "jl_is_unary_operator") == 0) {
return (void*)&jl_is_unary_operator; } 
else if (strcmp(target, "jl_is_unary_and_binary_operator") == 0) {
return (void*)&jl_is_unary_and_binary_operator; } 
else if (strcmp(target, "jl_operator_precedence") == 0) {
return (void*)&jl_operator_precedence; } 
else if (strcmp(target, "jl_apply_generic") == 0) {
return (void*)&jl_apply_generic; } 
else if (strcmp(target, "jl_invoke") == 0) {
return (void*)&jl_invoke; } 
else if (strcmp(target, "jl_invoke_api") == 0) {
return (void*)&jl_invoke_api; } 
else if (strcmp(target, "jl_call") == 0) {
return (void*)&jl_call; } 
else if (strcmp(target, "jl_call0") == 0) {
return (void*)&jl_call0; } 
else if (strcmp(target, "jl_call1") == 0) {
return (void*)&jl_call1; } 
else if (strcmp(target, "jl_call2") == 0) {
return (void*)&jl_call2; } 
else if (strcmp(target, "jl_call3") == 0) {
return (void*)&jl_call3; } 
else if (strcmp(target, "jl_yield") == 0) {
return (void*)&jl_yield; } 
else if (strcmp(target, "jl_install_sigint_handler") == 0) {
return (void*)&jl_install_sigint_handler; } 
else if (strcmp(target, "jl_sigatomic_begin") == 0) {
return (void*)&jl_sigatomic_begin; } 
else if (strcmp(target, "jl_sigatomic_end") == 0) {
return (void*)&jl_sigatomic_end; } 
else if (strcmp(target, "jl_new_task") == 0) {
return (void*)&jl_new_task; } 
else if (strcmp(target, "jl_switchto") == 0) {
return (void*)&jl_switchto; } 
else if (strcmp(target, "jl_throw") == 0) {
return (void*)&jl_throw; } 
else if (strcmp(target, "jl_rethrow") == 0) {
return (void*)&jl_rethrow; } 
else if (strcmp(target, "jl_sig_throw") == 0) {
return (void*)&jl_sig_throw; } 
else if (strcmp(target, "jl_rethrow_other") == 0) {
return (void*)&jl_rethrow_other; } 
else if (strcmp(target, "jl_no_exc_handler") == 0) {
return (void*)&jl_no_exc_handler; } 
else if (strcmp(target, "jl_enter_handler") == 0) {
return (void*)&jl_enter_handler; } 
else if (strcmp(target, "jl_eh_restore_state") == 0) {
return (void*)&jl_eh_restore_state; } 
else if (strcmp(target, "jl_pop_handler") == 0) {
return (void*)&jl_pop_handler; } 
else if (strcmp(target, "jl_excstack_state") == 0) {
return (void*)&jl_excstack_state; } 
else if (strcmp(target, "jl_restore_excstack") == 0) {
return (void*)&jl_restore_excstack; } 
else if (strcmp(target, "jl_take_buffer") == 0) {
return (void*)&jl_take_buffer; } 
else if (strcmp(target, "jl_sizeof_ios_t") == 0) {
return (void*)&jl_sizeof_ios_t; } 
else if (strcmp(target, "jl_uv_puts") == 0) {
return (void*)&jl_uv_puts; } 
else if (strcmp(target, "jl_printf") == 0) {
return (void*)&jl_printf; } 
else if (strcmp(target, "jl_safe_printf") == 0) {
return (void*)&jl_safe_printf; } 
else if (strcmp(target, "jl_stdout_stream") == 0) {
return (void*)&jl_stdout_stream; } 
else if (strcmp(target, "jl_stdin_stream") == 0) {
return (void*)&jl_stdin_stream; } 
else if (strcmp(target, "jl_stderr_stream") == 0) {
return (void*)&jl_stderr_stream; } 
else if (strcmp(target, "jl_flush_cstdio") == 0) {
return (void*)&jl_flush_cstdio; } 
else if (strcmp(target, "jl_stdout_obj") == 0) {
return (void*)&jl_stdout_obj; } 
else if (strcmp(target, "jl_stderr_obj") == 0) {
return (void*)&jl_stderr_obj; } 
else if (strcmp(target, "jl_static_show") == 0) {
return (void*)&jl_static_show; } 
else if (strcmp(target, "jl_static_show_func_sig") == 0) {
return (void*)&jl_static_show_func_sig; } 
else if (strcmp(target, "jlbacktrace") == 0) {
return (void*)&jlbacktrace; } 
else if (strcmp(target, "jl_") == 0) {
return (void*)&jl_; } 
else if (strcmp(target, "jl_sizeof_jl_options") == 0) {
return (void*)&jl_sizeof_jl_options; } 
else if (strcmp(target, "jl_parse_opts") == 0) {
return (void*)&jl_parse_opts; } 
else if (strcmp(target, "jl_format_filename") == 0) {
return (void*)&jl_format_filename; } 
else if (strcmp(target, "jl_set_ARGS") == 0) {
return (void*)&jl_set_ARGS; } 
else if (strcmp(target, "jl_generating_output") == 0) {
return (void*)&jl_generating_output; } 
else if (strcmp(target, "jl_ver_major") == 0) {
return (void*)&jl_ver_major; } 
else if (strcmp(target, "jl_ver_minor") == 0) {
return (void*)&jl_ver_minor; } 
else if (strcmp(target, "jl_ver_patch") == 0) {
return (void*)&jl_ver_patch; } 
else if (strcmp(target, "jl_ver_is_release") == 0) {
return (void*)&jl_ver_is_release; } 
else if (strcmp(target, "jl_ver_string") == 0) {
return (void*)&jl_ver_string; } 
else if (strcmp(target, "jl_git_branch") == 0) {
return (void*)&jl_git_branch; } 
else if (strcmp(target, "jl_git_commit") == 0) {
return (void*)&jl_git_commit; } 
else if (strcmp(target, "jl_gc_pool_alloc") == 0) {
return (void*)&jl_gc_pool_alloc; } 
else if (strcmp(target, "jl_gc_big_alloc") == 0) {
return (void*)&jl_gc_big_alloc; } 
else if (strcmp(target, "jl_alignment") == 0) {
return (void*)&jl_alignment; } 
else if (strcmp(target, "jl_gc_counted_malloc") == 0) {
return (void*)&jl_gc_counted_malloc; } 
else if (strcmp(target, "jl_throw_out_of_memory_error") == 0) {
return (void*)&jl_throw_out_of_memory_error; } 
else if (strcmp(target, "jl_gc_diff_total_bytes") == 0) {
return (void*)&jl_gc_diff_total_bytes; } 
else if (strcmp(target, "jl_get_method_inferred") == 0) {
return (void*)&jl_get_method_inferred; } 
else if (strcmp(target, "jl_compile_hint") == 0) {
return (void*)&jl_compile_hint; } 
else if (strcmp(target, "jl_foreigncall_get_syms") == 0) {
return (void*)&jl_foreigncall_get_syms; } 
else if (strcmp(target, "jl_foreigncall_interpretable") == 0) {
return (void*)&jl_foreigncall_interpretable; } 
else if (strcmp(target, "jl_new_code_info_uninit") == 0) {
return (void*)&jl_new_code_info_uninit; } 
else if (strcmp(target, "jl_apply_2va") == 0) {
return (void*)&jl_apply_2va; } 
else if (strcmp(target, "jl_typeassert") == 0) {
return (void*)&jl_typeassert; } 
else if (strcmp(target, "jl_f_tuple") == 0) {
return (void*)&jl_f_tuple; } 
else if (strcmp(target, "jl_f_intrinsic_call") == 0) {
return (void*)&jl_f_intrinsic_call; } 
else if (strcmp(target, "jl_method_table_insert") == 0) {
return (void*)&jl_method_table_insert; } 
else if (strcmp(target, "jl_type_morespecific_no_subtype") == 0) {
return (void*)&jl_type_morespecific_no_subtype; } 
else if (strcmp(target, "jl_instantiate_type_in_env") == 0) {
return (void*)&jl_instantiate_type_in_env; } 
else if (strcmp(target, "jl_matching_methods") == 0) {
return (void*)&jl_matching_methods; } 
else if (strcmp(target, "jl_first_argument_datatype") == 0) {
return (void*)&jl_first_argument_datatype; } 
else if (strcmp(target, "jl_argument_datatype") == 0) {
return (void*)&jl_argument_datatype; } 
else if (strcmp(target, "jl_method_table_for") == 0) {
return (void*)&jl_method_table_for; } 
else if (strcmp(target, "jl_dump_fptr_asm") == 0) {
return (void*)&jl_dump_fptr_asm; } 
else if (strcmp(target, "jl_idtable_rehash") == 0) {
return (void*)&jl_idtable_rehash; } 
else if (strcmp(target, "jl_new_method_table") == 0) {
return (void*)&jl_new_method_table; } 
else if (strcmp(target, "jl_rettype_inferred") == 0) {
return (void*)&jl_rettype_inferred; } 
else if (strcmp(target, "jl_methtable_lookup") == 0) {
return (void*)&jl_methtable_lookup; } 
else if (strcmp(target, "jl_specializations_get_linfo") == 0) {
return (void*)&jl_specializations_get_linfo; } 
else if (strcmp(target, "jl_method_instance_add_backedge") == 0) {
return (void*)&jl_method_instance_add_backedge; } 
else if (strcmp(target, "jl_method_table_add_backedge") == 0) {
return (void*)&jl_method_table_add_backedge; } 
else if (strcmp(target, "jl_get_backtrace") == 0) {
return (void*)&jl_get_backtrace; } 
else if (strcmp(target, "jl_raise_debugger") == 0) {
return (void*)&jl_raise_debugger; } 
else if (strcmp(target, "jl_gdblookup") == 0) {
return (void*)&jl_gdblookup; } 
else if (strcmp(target, "jl_is_interpreter_frame") == 0) {
return (void*)&jl_is_interpreter_frame; } 
else if (strcmp(target, "jl_is_enter_interpreter_frame") == 0) {
return (void*)&jl_is_enter_interpreter_frame; } 
else if (strcmp(target, "jl_capture_interp_frame") == 0) {
return (void*)&jl_capture_interp_frame; } 
else if (strcmp(target, "jl_hrtime") == 0) {
return (void*)&jl_hrtime; } 
else if (strcmp(target, "jl_load_and_lookup") == 0) {
return (void*)&jl_load_and_lookup; } 
else if (strcmp(target, "jl_get_cfunction_trampoline") == 0) {
return (void*)&jl_get_cfunction_trampoline; } 
else if (strcmp(target, "jl_get_JIT") == 0) {
return (void*)&jl_get_JIT; } 
else if (strcmp(target, "jl_fs_rename") == 0) {
return (void*)&jl_fs_rename; } 
else if (strcmp(target, "jl_cwd") == 0) {
return (void*)&jl_cwd; } 
else if (strcmp(target, "jl_is_file") == 0) {
return (void*)&jl_is_file; } 
else if (strcmp(target, "jl_intrinsic_name") == 0) {
return (void*)&jl_intrinsic_name; } 
else if (strcmp(target, "jl_bitcast") == 0) {
return (void*)&jl_bitcast; } 
else if (strcmp(target, "jl_pointerref") == 0) {
return (void*)&jl_pointerref; } 
else if (strcmp(target, "jl_pointerset") == 0) {
return (void*)&jl_pointerset; } 
else if (strcmp(target, "jl_cglobal") == 0) {
return (void*)&jl_cglobal; } 
else if (strcmp(target, "jl_cglobal_auto") == 0) {
return (void*)&jl_cglobal_auto; } 
else if (strcmp(target, "jl_neg_int") == 0) {
return (void*)&jl_neg_int; } 
else if (strcmp(target, "jl_add_int") == 0) {
return (void*)&jl_add_int; } 
else if (strcmp(target, "jl_sub_int") == 0) {
return (void*)&jl_sub_int; } 
else if (strcmp(target, "jl_mul_int") == 0) {
return (void*)&jl_mul_int; } 
else if (strcmp(target, "jl_sdiv_int") == 0) {
return (void*)&jl_sdiv_int; } 
else if (strcmp(target, "jl_udiv_int") == 0) {
return (void*)&jl_udiv_int; } 
else if (strcmp(target, "jl_srem_int") == 0) {
return (void*)&jl_srem_int; } 
else if (strcmp(target, "jl_urem_int") == 0) {
return (void*)&jl_urem_int; } 
else if (strcmp(target, "jl_add_ptr") == 0) {
return (void*)&jl_add_ptr; } 
else if (strcmp(target, "jl_sub_ptr") == 0) {
return (void*)&jl_sub_ptr; } 
else if (strcmp(target, "jl_neg_float") == 0) {
return (void*)&jl_neg_float; } 
else if (strcmp(target, "jl_add_float") == 0) {
return (void*)&jl_add_float; } 
else if (strcmp(target, "jl_sub_float") == 0) {
return (void*)&jl_sub_float; } 
else if (strcmp(target, "jl_mul_float") == 0) {
return (void*)&jl_mul_float; } 
else if (strcmp(target, "jl_div_float") == 0) {
return (void*)&jl_div_float; } 
else if (strcmp(target, "jl_rem_float") == 0) {
return (void*)&jl_rem_float; } 
else if (strcmp(target, "jl_fma_float") == 0) {
return (void*)&jl_fma_float; } 
else if (strcmp(target, "jl_muladd_float") == 0) {
return (void*)&jl_muladd_float; } 
else if (strcmp(target, "jl_eq_int") == 0) {
return (void*)&jl_eq_int; } 
else if (strcmp(target, "jl_ne_int") == 0) {
return (void*)&jl_ne_int; } 
else if (strcmp(target, "jl_slt_int") == 0) {
return (void*)&jl_slt_int; } 
else if (strcmp(target, "jl_ult_int") == 0) {
return (void*)&jl_ult_int; } 
else if (strcmp(target, "jl_sle_int") == 0) {
return (void*)&jl_sle_int; } 
else if (strcmp(target, "jl_ule_int") == 0) {
return (void*)&jl_ule_int; } 
else if (strcmp(target, "jl_eq_float") == 0) {
return (void*)&jl_eq_float; } 
else if (strcmp(target, "jl_ne_float") == 0) {
return (void*)&jl_ne_float; } 
else if (strcmp(target, "jl_lt_float") == 0) {
return (void*)&jl_lt_float; } 
else if (strcmp(target, "jl_le_float") == 0) {
return (void*)&jl_le_float; } 
else if (strcmp(target, "jl_fpiseq") == 0) {
return (void*)&jl_fpiseq; } 
else if (strcmp(target, "jl_fpislt") == 0) {
return (void*)&jl_fpislt; } 
else if (strcmp(target, "jl_not_int") == 0) {
return (void*)&jl_not_int; } 
else if (strcmp(target, "jl_and_int") == 0) {
return (void*)&jl_and_int; } 
else if (strcmp(target, "jl_or_int") == 0) {
return (void*)&jl_or_int; } 
else if (strcmp(target, "jl_xor_int") == 0) {
return (void*)&jl_xor_int; } 
else if (strcmp(target, "jl_shl_int") == 0) {
return (void*)&jl_shl_int; } 
else if (strcmp(target, "jl_lshr_int") == 0) {
return (void*)&jl_lshr_int; } 
else if (strcmp(target, "jl_ashr_int") == 0) {
return (void*)&jl_ashr_int; } 
else if (strcmp(target, "jl_bswap_int") == 0) {
return (void*)&jl_bswap_int; } 
else if (strcmp(target, "jl_ctpop_int") == 0) {
return (void*)&jl_ctpop_int; } 
else if (strcmp(target, "jl_ctlz_int") == 0) {
return (void*)&jl_ctlz_int; } 
else if (strcmp(target, "jl_cttz_int") == 0) {
return (void*)&jl_cttz_int; } 
else if (strcmp(target, "jl_sext_int") == 0) {
return (void*)&jl_sext_int; } 
else if (strcmp(target, "jl_zext_int") == 0) {
return (void*)&jl_zext_int; } 
else if (strcmp(target, "jl_trunc_int") == 0) {
return (void*)&jl_trunc_int; } 
else if (strcmp(target, "jl_sitofp") == 0) {
return (void*)&jl_sitofp; } 
else if (strcmp(target, "jl_uitofp") == 0) {
return (void*)&jl_uitofp; } 
else if (strcmp(target, "jl_fptoui") == 0) {
return (void*)&jl_fptoui; } 
else if (strcmp(target, "jl_fptosi") == 0) {
return (void*)&jl_fptosi; } 
else if (strcmp(target, "jl_fptrunc") == 0) {
return (void*)&jl_fptrunc; } 
else if (strcmp(target, "jl_fpext") == 0) {
return (void*)&jl_fpext; } 
else if (strcmp(target, "jl_checked_sadd_int") == 0) {
return (void*)&jl_checked_sadd_int; } 
else if (strcmp(target, "jl_checked_uadd_int") == 0) {
return (void*)&jl_checked_uadd_int; } 
else if (strcmp(target, "jl_checked_ssub_int") == 0) {
return (void*)&jl_checked_ssub_int; } 
else if (strcmp(target, "jl_checked_usub_int") == 0) {
return (void*)&jl_checked_usub_int; } 
else if (strcmp(target, "jl_checked_smul_int") == 0) {
return (void*)&jl_checked_smul_int; } 
else if (strcmp(target, "jl_checked_umul_int") == 0) {
return (void*)&jl_checked_umul_int; } 
else if (strcmp(target, "jl_checked_sdiv_int") == 0) {
return (void*)&jl_checked_sdiv_int; } 
else if (strcmp(target, "jl_checked_udiv_int") == 0) {
return (void*)&jl_checked_udiv_int; } 
else if (strcmp(target, "jl_checked_srem_int") == 0) {
return (void*)&jl_checked_srem_int; } 
else if (strcmp(target, "jl_checked_urem_int") == 0) {
return (void*)&jl_checked_urem_int; } 
else if (strcmp(target, "jl_ceil_llvm") == 0) {
return (void*)&jl_ceil_llvm; } 
else if (strcmp(target, "jl_floor_llvm") == 0) {
return (void*)&jl_floor_llvm; } 
else if (strcmp(target, "jl_trunc_llvm") == 0) {
return (void*)&jl_trunc_llvm; } 
else if (strcmp(target, "jl_rint_llvm") == 0) {
return (void*)&jl_rint_llvm; } 
else if (strcmp(target, "jl_sqrt_llvm") == 0) {
return (void*)&jl_sqrt_llvm; } 
else if (strcmp(target, "jl_sqrt_llvm_fast") == 0) {
return (void*)&jl_sqrt_llvm_fast; } 
else if (strcmp(target, "jl_abs_float") == 0) {
return (void*)&jl_abs_float; } 
else if (strcmp(target, "jl_copysign_float") == 0) {
return (void*)&jl_copysign_float; } 
else if (strcmp(target, "jl_flipsign_int") == 0) {
return (void*)&jl_flipsign_int; } 
else if (strcmp(target, "jl_arraylen") == 0) {
return (void*)&jl_arraylen; } 
else if (strcmp(target, "jl_array_data_owner") == 0) {
return (void*)&jl_array_data_owner; } 
else if (strcmp(target, "jl_object_id_") == 0) {
return (void*)&jl_object_id_; } 
else if (strcmp(target, "jl_array_cconvert_cstring") == 0) {
return (void*)&jl_array_cconvert_cstring; } 
else if (strcmp(target, "jl_extern_c") == 0) {
return (void*)&jl_extern_c; } 
else if (strcmp(target, "jl_function_ptr") == 0) {
return (void*)&jl_function_ptr; } 
else if (strcmp(target, "jl_dump_function_asm") == 0) {
return (void*)&jl_dump_function_asm; } 
else if (strcmp(target, "jl_dump_function_ir") == 0) {
return (void*)&jl_dump_function_ir; } 
else if (strcmp(target, "jl_LLVMCreateDisasm") == 0) {
return (void*)&jl_LLVMCreateDisasm; } 
else if (strcmp(target, "jl_LLVMDisasmInstruction") == 0) {
return (void*)&jl_LLVMDisasmInstruction; } 
else if (strcmp(target, "jl_get_LLVM_VERSION") == 0) {
return (void*)&jl_get_LLVM_VERSION; } 
else if (strcmp(target, "jl_getutf8") == 0) {
return (void*)&jl_getutf8; } 
else if (strcmp(target, "jl_sizeof_off_t") == 0) {
return (void*)&jl_sizeof_off_t; } 
else if (strcmp(target, "jl_sizeof_mode_t") == 0) {
return (void*)&jl_sizeof_mode_t; } 
else if (strcmp(target, "jl_ftruncate") == 0) {
return (void*)&jl_ftruncate; } 
else if (strcmp(target, "jl_lseek") == 0) {
return (void*)&jl_lseek; } 
else if (strcmp(target, "jl_pwrite") == 0) {
return (void*)&jl_pwrite; } 
else if (strcmp(target, "jl_mmap") == 0) {
return (void*)&jl_mmap; } 
else if (strcmp(target, "jl_ios_fd") == 0) {
return (void*)&jl_ios_fd; } 
else if (strcmp(target, "jl_nb_available") == 0) {
return (void*)&jl_nb_available; } 
else if (strcmp(target, "jl_readuntil") == 0) {
return (void*)&jl_readuntil; } 
else if (strcmp(target, "jl_ios_buffer_n") == 0) {
return (void*)&jl_ios_buffer_n; } 
else if (strcmp(target, "jl_ios_get_nbyte_int") == 0) {
return (void*)&jl_ios_get_nbyte_int; } 
else if (strcmp(target, "jl_native_alignment") == 0) {
return (void*)&jl_native_alignment; } 
else if (strcmp(target, "jl_is_char_signed") == 0) {
return (void*)&jl_is_char_signed; } 
else if (strcmp(target, "jl_SC_CLK_TCK") == 0) {
return (void*)&jl_SC_CLK_TCK; } 
else if (strcmp(target, "jl_maxrss") == 0) {
return (void*)&jl_maxrss; } 
else if (strcmp(target, "jl_threading_enabled") == 0) {
return (void*)&jl_threading_enabled; } 
else if (strcmp(target, "jl_f_throw") == 0) {
return (void*)&jl_f_throw; } 
else if (strcmp(target, "jl_f_is") == 0) {
return (void*)&jl_f_is; } 
else if (strcmp(target, "jl_f_typeof") == 0) {
return (void*)&jl_f_typeof; } 
else if (strcmp(target, "jl_f_sizeof") == 0) {
return (void*)&jl_f_sizeof; } 
else if (strcmp(target, "jl_f_issubtype") == 0) {
return (void*)&jl_f_issubtype; } 
else if (strcmp(target, "jl_f_isa") == 0) {
return (void*)&jl_f_isa; } 
else if (strcmp(target, "jl_f__apply") == 0) {
return (void*)&jl_f__apply; } 
else if (strcmp(target, "jl_f__apply_pure") == 0) {
return (void*)&jl_f__apply_pure; } 
else if (strcmp(target, "jl_f__apply_latest") == 0) {
return (void*)&jl_f__apply_latest; } 
else if (strcmp(target, "jl_f__apply_iterate") == 0) {
return (void*)&jl_f__apply_iterate; } 
else if (strcmp(target, "jl_f_isdefined") == 0) {
return (void*)&jl_f_isdefined; } 
else if (strcmp(target, "jl_f_nfields") == 0) {
return (void*)&jl_f_nfields; } 
else if (strcmp(target, "jl_f_svec") == 0) {
return (void*)&jl_f_svec; } 
else if (strcmp(target, "jl_f_getfield") == 0) {
return (void*)&jl_f_getfield; } 
else if (strcmp(target, "jl_f_setfield") == 0) {
return (void*)&jl_f_setfield; } 
else if (strcmp(target, "jl_f_fieldtype") == 0) {
return (void*)&jl_f_fieldtype; } 
else if (strcmp(target, "jl_f_arrayref") == 0) {
return (void*)&jl_f_arrayref; } 
else if (strcmp(target, "jl_f_const_arrayref") == 0) {
return (void*)&jl_f_const_arrayref; } 
else if (strcmp(target, "jl_f_arrayset") == 0) {
return (void*)&jl_f_arrayset; } 
else if (strcmp(target, "jl_f_arraysize") == 0) {
return (void*)&jl_f_arraysize; } 
else if (strcmp(target, "jl_f_apply_type") == 0) {
return (void*)&jl_f_apply_type; } 
else if (strcmp(target, "jl_f_applicable") == 0) {
return (void*)&jl_f_applicable; } 
else if (strcmp(target, "jl_f_invoke") == 0) {
return (void*)&jl_f_invoke; } 
else if (strcmp(target, "jl_f__expr") == 0) {
return (void*)&jl_f__expr; } 
else if (strcmp(target, "jl_f_typeassert") == 0) {
return (void*)&jl_f_typeassert; } 
else if (strcmp(target, "jl_f_ifelse") == 0) {
return (void*)&jl_f_ifelse; } 
else if (strcmp(target, "jl_f__typevar") == 0) {
return (void*)&jl_f__typevar; } 
else if (strcmp(target, "jl_f_invoke_kwsorter") == 0) {
return (void*)&jl_f_invoke_kwsorter; } 
else if (strcmp(target, "jl_get_cpu_name") == 0) {
return (void*)&jl_get_cpu_name; } 
else if (strcmp(target, "jl_dump_host_cpu") == 0) {
return (void*)&jl_dump_host_cpu; } 
else if (strcmp(target, "jl_running_on_valgrind") == 0) {
return (void*)&jl_running_on_valgrind; } 
else if (strcmp(target, "ios_write_direct") == 0) {
return (void*)&ios_write_direct; } 
else if (strcmp(target, "jl_is_memdebug") == 0) {
return (void*)&jl_is_memdebug; } 
else if (strcmp(target, "jl_get_julia_bindir") == 0) {
return (void*)&jl_get_julia_bindir; } 
else if (strcmp(target, "jl_get_julia_bin") == 0) {
return (void*)&jl_get_julia_bin; } 
else if (strcmp(target, "jl_get_image_file") == 0) {
return (void*)&jl_get_image_file; } 
else if (strcmp(target, "jl_astaggedvalue") == 0) {
return (void*)&jl_astaggedvalue; } 
else if (strcmp(target, "jl_valueof") == 0) {
return (void*)&jl_valueof; } 
else if (strcmp(target, "jl_typeof") == 0) {
return (void*)&jl_typeof; } 
else if (strcmp(target, "jl_get_fieldtypes") == 0) {
return (void*)&jl_get_fieldtypes; } 
else if (strcmp(target, "jl_gc_unsafe_enter") == 0) {
return (void*)&jl_gc_unsafe_enter; } 
else if (strcmp(target, "jl_gc_unsafe_leave") == 0) {
return (void*)&jl_gc_unsafe_leave; } 
else if (strcmp(target, "jl_gc_safe_enter") == 0) {
return (void*)&jl_gc_safe_enter; } 
else if (strcmp(target, "jl_gc_safe_leave") == 0) {
return (void*)&jl_gc_safe_leave; } 
else if (strcmp(target, "jl_get_fenv_consts") == 0) {
return (void*)&jl_get_fenv_consts; } 
else if (strcmp(target, "jl_find_free_typevars") == 0) {
return (void*)&jl_find_free_typevars; } 
else if (strcmp(target, "jl_set_jsfunction_type") == 0) {
return (void*)&jl_set_jsfunction_type; } 
else if (strcmp(target, "jl_eqtable_pop") == 0) {
return (void*)&jl_eqtable_pop; } 
else if (strcmp(target, "jl_eqtable_nextind") == 0) {
return (void*)&jl_eqtable_nextind; } 
else if (strcmp(target, "jl_get_keyword_sorter") == 0) {
return (void*)&jl_get_keyword_sorter; } 
else if (strcmp(target, "jl_array_store_unboxed") == 0) {
return (void*)&jl_array_store_unboxed; } 
else if (strcmp(target, "jl_string_to_array") == 0) {
return (void*)&jl_string_to_array; } 
else if (strcmp(target, "jl_array_grow_at") == 0) {
return (void*)&jl_array_grow_at; } 
else if (strcmp(target, "jl_array_del_at") == 0) {
return (void*)&jl_array_del_at; } 
else if (strcmp(target, "jl_array_copy") == 0) {
return (void*)&jl_array_copy; } 
else if (strcmp(target, "jl_array_ptr_copy") == 0) {
return (void*)&jl_array_ptr_copy; } 
else if (strcmp(target, "jl_f_new_module") == 0) {
return (void*)&jl_f_new_module; } 
else if (strcmp(target, "jl_set_istopmod") == 0) {
return (void*)&jl_set_istopmod; } 
else if (strcmp(target, "jl_istopmod") == 0) {
return (void*)&jl_istopmod; } 
else if (strcmp(target, "jl_get_module_of_binding") == 0) {
return (void*)&jl_get_module_of_binding; } 
else if (strcmp(target, "jl_binding_owner") == 0) {
return (void*)&jl_binding_owner; } 
else if (strcmp(target, "jl_get_module_binding") == 0) {
return (void*)&jl_get_module_binding; } 
else if (strcmp(target, "jl_deprecate_binding") == 0) {
return (void*)&jl_deprecate_binding; } 
else if (strcmp(target, "jl_is_binding_deprecated") == 0) {
return (void*)&jl_is_binding_deprecated; } 
else if (strcmp(target, "jl_module_usings") == 0) {
return (void*)&jl_module_usings; } 
else if (strcmp(target, "jl_module_names") == 0) {
return (void*)&jl_module_names; } 
else if (strcmp(target, "jl_module_name") == 0) {
return (void*)&jl_module_name; } 
else if (strcmp(target, "jl_module_parent") == 0) {
return (void*)&jl_module_parent; } 
else if (strcmp(target, "jl_module_build_id") == 0) {
return (void*)&jl_module_build_id; } 
else if (strcmp(target, "jl_module_uuid") == 0) {
return (void*)&jl_module_uuid; } 
else if (strcmp(target, "jl_set_module_uuid") == 0) {
return (void*)&jl_set_module_uuid; } 
else if (strcmp(target, "jl_gc_set_cb_root_scanner") == 0) {
return (void*)&jl_gc_set_cb_root_scanner; } 
else if (strcmp(target, "jl_gc_set_cb_task_scanner") == 0) {
return (void*)&jl_gc_set_cb_task_scanner; } 
else if (strcmp(target, "jl_gc_set_cb_pre_gc") == 0) {
return (void*)&jl_gc_set_cb_pre_gc; } 
else if (strcmp(target, "jl_gc_set_cb_post_gc") == 0) {
return (void*)&jl_gc_set_cb_post_gc; } 
else if (strcmp(target, "jl_gc_set_cb_notify_external_alloc") == 0) {
return (void*)&jl_gc_set_cb_notify_external_alloc; } 
else if (strcmp(target, "jl_gc_set_cb_notify_external_free") == 0) {
return (void*)&jl_gc_set_cb_notify_external_free; } 
else if (strcmp(target, "jl_new_foreign_type") == 0) {
return (void*)&jl_new_foreign_type; } 
else if (strcmp(target, "jl_gc_max_internal_obj_size") == 0) {
return (void*)&jl_gc_max_internal_obj_size; } 
else if (strcmp(target, "jl_gc_external_obj_hdr_size") == 0) {
return (void*)&jl_gc_external_obj_hdr_size; } 
else if (strcmp(target, "jl_gc_alloc_typed") == 0) {
return (void*)&jl_gc_alloc_typed; } 
else if (strcmp(target, "jl_gc_mark_queue_obj") == 0) {
return (void*)&jl_gc_mark_queue_obj; } 
else if (strcmp(target, "jl_gc_mark_queue_objarray") == 0) {
return (void*)&jl_gc_mark_queue_objarray; } 
else if (strcmp(target, "jl_gc_schedule_foreign_sweepfunc") == 0) {
return (void*)&jl_gc_schedule_foreign_sweepfunc; } 
else if (strcmp(target, "jl_gc_enable_conservative_gc_support") == 0) {
return (void*)&jl_gc_enable_conservative_gc_support; } 
else if (strcmp(target, "jl_gc_conservative_gc_support_enabled") == 0) {
return (void*)&jl_gc_conservative_gc_support_enabled; } 
else if (strcmp(target, "jl_gc_internal_obj_base_ptr") == 0) {
return (void*)&jl_gc_internal_obj_base_ptr; } 
else if (strcmp(target, "jl_task_stack_buffer") == 0) {
return (void*)&jl_task_stack_buffer; } 
else if (strcmp(target, "jl_gc_add_ptr_finalizer") == 0) {
return (void*)&jl_gc_add_ptr_finalizer; } 
else if (strcmp(target, "jl_gc_add_finalizer_th") == 0) {
return (void*)&jl_gc_add_finalizer_th; } 
else if (strcmp(target, "jl_finalize_th") == 0) {
return (void*)&jl_finalize_th; } 
else if (strcmp(target, "jl_gc_new_weakref_th") == 0) {
return (void*)&jl_gc_new_weakref_th; } 
else if (strcmp(target, "jl_gc_get_total_bytes") == 0) {
return (void*)&jl_gc_get_total_bytes; } 
else if (strcmp(target, "jl_gc_total_hrtime") == 0) {
return (void*)&jl_gc_total_hrtime; } 
else if (strcmp(target, "jl_gc_num") == 0) {
return (void*)&jl_gc_num; } 
else if (strcmp(target, "jl_gc_counted_calloc") == 0) {
return (void*)&jl_gc_counted_calloc; } 
else if (strcmp(target, "jl_gc_counted_free_with_size") == 0) {
return (void*)&jl_gc_counted_free_with_size; } 
else if (strcmp(target, "jl_gc_counted_realloc_with_old_size") == 0) {
return (void*)&jl_gc_counted_realloc_with_old_size; } 
else if (strcmp(target, "jl_malloc") == 0) {
return (void*)&jl_malloc; } 
else if (strcmp(target, "jl_calloc") == 0) {
return (void*)&jl_calloc; } 
else if (strcmp(target, "jl_free") == 0) {
return (void*)&jl_free; } 
else if (strcmp(target, "jl_realloc") == 0) {
return (void*)&jl_realloc; } 
else if (strcmp(target, "jl_profile_stop_timer") == 0) {
return (void*)&jl_profile_stop_timer; } 
else if (strcmp(target, "jl_profile_start_timer") == 0) {
return (void*)&jl_profile_start_timer; } 
else if (strcmp(target, "jl_exit_on_sigint") == 0) {
return (void*)&jl_exit_on_sigint; } 
else if (strcmp(target, "jl_repl_raise_sigtstp") == 0) {
return (void*)&jl_repl_raise_sigtstp; } 
else if (strcmp(target, "jl_profile_init") == 0) {
return (void*)&jl_profile_init; } 
else if (strcmp(target, "jl_profile_get_data") == 0) {
return (void*)&jl_profile_get_data; } 
else if (strcmp(target, "jl_profile_len_data") == 0) {
return (void*)&jl_profile_len_data; } 
else if (strcmp(target, "jl_profile_maxlen_data") == 0) {
return (void*)&jl_profile_maxlen_data; } 
else if (strcmp(target, "jl_profile_delay_nsec") == 0) {
return (void*)&jl_profile_delay_nsec; } 
else if (strcmp(target, "jl_profile_clear_data") == 0) {
return (void*)&jl_profile_clear_data; } 
else if (strcmp(target, "jl_profile_is_running") == 0) {
return (void*)&jl_profile_is_running; } 
else if (strcmp(target, "jl_symbol_name") == 0) {
return (void*)&jl_symbol_name; } 
else if (strcmp(target, "jl_try_substrtod") == 0) {
return (void*)&jl_try_substrtod; } 
else if (strcmp(target, "jl_substrtod") == 0) {
return (void*)&jl_substrtod; } 
else if (strcmp(target, "jl_try_substrtof") == 0) {
return (void*)&jl_try_substrtof; } 
else if (strcmp(target, "jl_substrtof") == 0) {
return (void*)&jl_substrtof; } 
else if (strcmp(target, "jl_id_start_char") == 0) {
return (void*)&jl_id_start_char; } 
else if (strcmp(target, "jl_id_char") == 0) {
return (void*)&jl_id_char; } 
else if (strcmp(target, "jl_is_identifier") == 0) {
return (void*)&jl_is_identifier; } 
else if (strcmp(target, "jl_breakpoint") == 0) {
return (void*)&jl_breakpoint; } 
else if (strcmp(target, "jl_uv_flush") == 0) {
return (void*)&jl_uv_flush; } 
else if (strcmp(target, "jl_uv_putb") == 0) {
return (void*)&jl_uv_putb; } 
else if (strcmp(target, "jl_sizeof_stat") == 0) {
return (void*)&jl_sizeof_stat; } 
else if (strcmp(target, "jl_lstat") == 0) {
return (void*)&jl_lstat; } 
else if (strcmp(target, "jl_fstat") == 0) {
return (void*)&jl_fstat; } 
else if (strcmp(target, "jl_stat_dev") == 0) {
return (void*)&jl_stat_dev; } 
else if (strcmp(target, "jl_stat_ino") == 0) {
return (void*)&jl_stat_ino; } 
else if (strcmp(target, "jl_stat_mode") == 0) {
return (void*)&jl_stat_mode; } 
else if (strcmp(target, "jl_stat_nlink") == 0) {
return (void*)&jl_stat_nlink; } 
else if (strcmp(target, "jl_stat_uid") == 0) {
return (void*)&jl_stat_uid; } 
else if (strcmp(target, "jl_stat_gid") == 0) {
return (void*)&jl_stat_gid; } 
else if (strcmp(target, "jl_stat_rdev") == 0) {
return (void*)&jl_stat_rdev; } 
else if (strcmp(target, "jl_stat_size") == 0) {
return (void*)&jl_stat_size; } 
else if (strcmp(target, "jl_stat_blksize") == 0) {
return (void*)&jl_stat_blksize; } 
else if (strcmp(target, "jl_stat_blocks") == 0) {
return (void*)&jl_stat_blocks; } 
else if (strcmp(target, "jl_stat_mtime") == 0) {
return (void*)&jl_stat_mtime; } 
else if (strcmp(target, "jl_stat_ctime") == 0) {
return (void*)&jl_stat_ctime; } 
else if (strcmp(target, "jl_wake_libuv") == 0) {
return (void*)&jl_wake_libuv; } 
else if (strcmp(target, "jl_get_tls_world_age") == 0) {
return (void*)&jl_get_tls_world_age; } 
else if (strcmp(target, "jl_is_in_pure_context") == 0) {
return (void*)&jl_is_in_pure_context; } 
else if (strcmp(target, "jl_specializations_lookup") == 0) {
return (void*)&jl_specializations_lookup; } 
else if (strcmp(target, "jl_new_method_uninit") == 0) {
return (void*)&jl_new_method_uninit; } 
else if (strcmp(target, "jl_set_method_inferred") == 0) {
return (void*)&jl_set_method_inferred; } 
else if (strcmp(target, "jl_call_in_typeinf_world") == 0) {
return (void*)&jl_call_in_typeinf_world; } 
else if (strcmp(target, "jl_set_typeinf_func") == 0) {
return (void*)&jl_set_typeinf_func; } 
else if (strcmp(target, "jl_isa_compileable_sig") == 0) {
return (void*)&jl_isa_compileable_sig; } 
else if (strcmp(target, "jl_method_table_disable") == 0) {
return (void*)&jl_method_table_disable; } 
else if (strcmp(target, "jl_get_spec_lambda") == 0) {
return (void*)&jl_get_spec_lambda; } 
else if (strcmp(target, "jl_gf_invoke_lookup") == 0) {
return (void*)&jl_gf_invoke_lookup; } 
else if (strcmp(target, "jl_get_invoke_lambda") == 0) {
return (void*)&jl_get_invoke_lambda; } 
else if (strcmp(target, "jl_typeinf_begin") == 0) {
return (void*)&jl_typeinf_begin; } 
else if (strcmp(target, "jl_typeinf_end") == 0) {
return (void*)&jl_typeinf_end; } 
else if (strcmp(target, "jl_get_current_task") == 0) {
return (void*)&jl_get_current_task; } 
else if (strcmp(target, "jl_is_task_started") == 0) {
return (void*)&jl_is_task_started; } 
else if (strcmp(target, "jl_get_task_tid") == 0) {
return (void*)&jl_get_task_tid; } 
else if (strcmp(target, "jl_set_task_tid") == 0) {
return (void*)&jl_set_task_tid; } 
else if (strcmp(target, "jl_task_get_next") == 0) {
return (void*)&jl_task_get_next; } 
else if (strcmp(target, "pcre2_config_8") == 0) {
return (void*)&pcre2_config_8; } 
else if (strcmp(target, "pcre2_general_context_copy_8") == 0) {
return (void*)&pcre2_general_context_copy_8; } 
else if (strcmp(target, "pcre2_general_context_create_8") == 0) {
return (void*)&pcre2_general_context_create_8; } 
else if (strcmp(target, "pcre2_general_context_free_8") == 0) {
return (void*)&pcre2_general_context_free_8; } 
else if (strcmp(target, "pcre2_compile_context_copy_8") == 0) {
return (void*)&pcre2_compile_context_copy_8; } 
else if (strcmp(target, "pcre2_compile_context_create_8") == 0) {
return (void*)&pcre2_compile_context_create_8; } 
else if (strcmp(target, "pcre2_compile_context_free_8") == 0) {
return (void*)&pcre2_compile_context_free_8; } 
else if (strcmp(target, "pcre2_set_bsr_8") == 0) {
return (void*)&pcre2_set_bsr_8; } 
else if (strcmp(target, "pcre2_set_character_tables_8") == 0) {
return (void*)&pcre2_set_character_tables_8; } 
else if (strcmp(target, "pcre2_set_compile_extra_options_8") == 0) {
return (void*)&pcre2_set_compile_extra_options_8; } 
else if (strcmp(target, "pcre2_set_max_pattern_length_8") == 0) {
return (void*)&pcre2_set_max_pattern_length_8; } 
else if (strcmp(target, "pcre2_set_newline_8") == 0) {
return (void*)&pcre2_set_newline_8; } 
else if (strcmp(target, "pcre2_set_parens_nest_limit_8") == 0) {
return (void*)&pcre2_set_parens_nest_limit_8; } 
else if (strcmp(target, "pcre2_set_compile_recursion_guard_8") == 0) {
return (void*)&pcre2_set_compile_recursion_guard_8; } 
else if (strcmp(target, "pcre2_convert_context_copy_8") == 0) {
return (void*)&pcre2_convert_context_copy_8; } 
else if (strcmp(target, "pcre2_convert_context_create_8") == 0) {
return (void*)&pcre2_convert_context_create_8; } 
else if (strcmp(target, "pcre2_convert_context_free_8") == 0) {
return (void*)&pcre2_convert_context_free_8; } 
else if (strcmp(target, "pcre2_set_glob_escape_8") == 0) {
return (void*)&pcre2_set_glob_escape_8; } 
else if (strcmp(target, "pcre2_set_glob_separator_8") == 0) {
return (void*)&pcre2_set_glob_separator_8; } 
else if (strcmp(target, "pcre2_pattern_convert_8") == 0) {
return (void*)&pcre2_pattern_convert_8; } 
else if (strcmp(target, "pcre2_converted_pattern_free_8") == 0) {
return (void*)&pcre2_converted_pattern_free_8; } 
else if (strcmp(target, "pcre2_match_context_copy_8") == 0) {
return (void*)&pcre2_match_context_copy_8; } 
else if (strcmp(target, "pcre2_match_context_create_8") == 0) {
return (void*)&pcre2_match_context_create_8; } 
else if (strcmp(target, "pcre2_match_context_free_8") == 0) {
return (void*)&pcre2_match_context_free_8; } 
else if (strcmp(target, "pcre2_set_depth_limit_8") == 0) {
return (void*)&pcre2_set_depth_limit_8; } 
else if (strcmp(target, "pcre2_set_heap_limit_8") == 0) {
return (void*)&pcre2_set_heap_limit_8; } 
else if (strcmp(target, "pcre2_set_match_limit_8") == 0) {
return (void*)&pcre2_set_match_limit_8; } 
else if (strcmp(target, "pcre2_set_offset_limit_8") == 0) {
return (void*)&pcre2_set_offset_limit_8; } 
else if (strcmp(target, "pcre2_set_recursion_limit_8") == 0) {
return (void*)&pcre2_set_recursion_limit_8; } 
else if (strcmp(target, "pcre2_set_recursion_memory_management_8") == 0) {
return (void*)&pcre2_set_recursion_memory_management_8; } 
else if (strcmp(target, "pcre2_compile_8") == 0) {
return (void*)&pcre2_compile_8; } 
else if (strcmp(target, "pcre2_code_free_8") == 0) {
return (void*)&pcre2_code_free_8; } 
else if (strcmp(target, "pcre2_code_copy_8") == 0) {
return (void*)&pcre2_code_copy_8; } 
else if (strcmp(target, "pcre2_code_copy_with_tables_8") == 0) {
return (void*)&pcre2_code_copy_with_tables_8; } 
else if (strcmp(target, "pcre2_pattern_info_8") == 0) {
return (void*)&pcre2_pattern_info_8; } 
else if (strcmp(target, "pcre2_match_data_create_8") == 0) {
return (void*)&pcre2_match_data_create_8; } 
else if (strcmp(target, "pcre2_match_data_create_from_pattern_8") == 0) {
return (void*)&pcre2_match_data_create_from_pattern_8; } 
else if (strcmp(target, "pcre2_dfa_match_8") == 0) {
return (void*)&pcre2_dfa_match_8; } 
else if (strcmp(target, "pcre2_match_8") == 0) {
return (void*)&pcre2_match_8; } 
else if (strcmp(target, "pcre2_match_data_free_8") == 0) {
return (void*)&pcre2_match_data_free_8; } 
else if (strcmp(target, "pcre2_get_mark_8") == 0) {
return (void*)&pcre2_get_mark_8; } 
else if (strcmp(target, "pcre2_get_ovector_count_8") == 0) {
return (void*)&pcre2_get_ovector_count_8; } 
else if (strcmp(target, "pcre2_get_ovector_pointer_8") == 0) {
return (void*)&pcre2_get_ovector_pointer_8; } 
else if (strcmp(target, "pcre2_get_startchar_8") == 0) {
return (void*)&pcre2_get_startchar_8; } 
else if (strcmp(target, "pcre2_substring_copy_byname_8") == 0) {
return (void*)&pcre2_substring_copy_byname_8; } 
else if (strcmp(target, "pcre2_substring_copy_bynumber_8") == 0) {
return (void*)&pcre2_substring_copy_bynumber_8; } 
else if (strcmp(target, "pcre2_substring_free_8") == 0) {
return (void*)&pcre2_substring_free_8; } 
else if (strcmp(target, "pcre2_substring_get_byname_8") == 0) {
return (void*)&pcre2_substring_get_byname_8; } 
else if (strcmp(target, "pcre2_substring_get_bynumber_8") == 0) {
return (void*)&pcre2_substring_get_bynumber_8; } 
else if (strcmp(target, "pcre2_substring_length_byname_8") == 0) {
return (void*)&pcre2_substring_length_byname_8; } 
else if (strcmp(target, "pcre2_substring_length_bynumber_8") == 0) {
return (void*)&pcre2_substring_length_bynumber_8; } 
else if (strcmp(target, "pcre2_substring_nametable_scan_8") == 0) {
return (void*)&pcre2_substring_nametable_scan_8; } 
else if (strcmp(target, "pcre2_substring_number_from_name_8") == 0) {
return (void*)&pcre2_substring_number_from_name_8; } 
else if (strcmp(target, "pcre2_substring_list_free_8") == 0) {
return (void*)&pcre2_substring_list_free_8; } 
else if (strcmp(target, "pcre2_substring_list_get_8") == 0) {
return (void*)&pcre2_substring_list_get_8; } 
else if (strcmp(target, "pcre2_serialize_encode_8") == 0) {
return (void*)&pcre2_serialize_encode_8; } 
else if (strcmp(target, "pcre2_serialize_decode_8") == 0) {
return (void*)&pcre2_serialize_decode_8; } 
else if (strcmp(target, "pcre2_serialize_get_number_of_codes_8") == 0) {
return (void*)&pcre2_serialize_get_number_of_codes_8; } 
else if (strcmp(target, "pcre2_serialize_free_8") == 0) {
return (void*)&pcre2_serialize_free_8; } 
else if (strcmp(target, "pcre2_substitute_8") == 0) {
return (void*)&pcre2_substitute_8; } 
else if (strcmp(target, "pcre2_jit_compile_8") == 0) {
return (void*)&pcre2_jit_compile_8; } 
else if (strcmp(target, "pcre2_jit_match_8") == 0) {
return (void*)&pcre2_jit_match_8; } 
else if (strcmp(target, "pcre2_jit_free_unused_memory_8") == 0) {
return (void*)&pcre2_jit_free_unused_memory_8; } 
else if (strcmp(target, "pcre2_jit_stack_create_8") == 0) {
return (void*)&pcre2_jit_stack_create_8; } 
else if (strcmp(target, "pcre2_jit_stack_assign_8") == 0) {
return (void*)&pcre2_jit_stack_assign_8; } 
else if (strcmp(target, "pcre2_jit_stack_free_8") == 0) {
return (void*)&pcre2_jit_stack_free_8; } 
else if (strcmp(target, "pcre2_get_error_message_8") == 0) {
return (void*)&pcre2_get_error_message_8; } 
else if (strcmp(target, "pcre2_maketables_8") == 0) {
return (void*)&pcre2_maketables_8; } 
else if (strcmp(target, "pcre2_config_16") == 0) {
return (void*)&pcre2_config_16; } 
else if (strcmp(target, "pcre2_general_context_copy_16") == 0) {
return (void*)&pcre2_general_context_copy_16; } 
else if (strcmp(target, "pcre2_general_context_create_16") == 0) {
return (void*)&pcre2_general_context_create_16; } 
else if (strcmp(target, "pcre2_general_context_free_16") == 0) {
return (void*)&pcre2_general_context_free_16; } 
else if (strcmp(target, "pcre2_compile_context_copy_16") == 0) {
return (void*)&pcre2_compile_context_copy_16; } 
else if (strcmp(target, "pcre2_compile_context_create_16") == 0) {
return (void*)&pcre2_compile_context_create_16; } 
else if (strcmp(target, "pcre2_compile_context_free_16") == 0) {
return (void*)&pcre2_compile_context_free_16; } 
else if (strcmp(target, "pcre2_set_bsr_16") == 0) {
return (void*)&pcre2_set_bsr_16; } 
else if (strcmp(target, "pcre2_set_character_tables_16") == 0) {
return (void*)&pcre2_set_character_tables_16; } 
else if (strcmp(target, "pcre2_set_compile_extra_options_16") == 0) {
return (void*)&pcre2_set_compile_extra_options_16; } 
else if (strcmp(target, "pcre2_set_max_pattern_length_16") == 0) {
return (void*)&pcre2_set_max_pattern_length_16; } 
else if (strcmp(target, "pcre2_set_newline_16") == 0) {
return (void*)&pcre2_set_newline_16; } 
else if (strcmp(target, "pcre2_set_parens_nest_limit_16") == 0) {
return (void*)&pcre2_set_parens_nest_limit_16; } 
else if (strcmp(target, "pcre2_set_compile_recursion_guard_16") == 0) {
return (void*)&pcre2_set_compile_recursion_guard_16; } 
else if (strcmp(target, "pcre2_convert_context_copy_16") == 0) {
return (void*)&pcre2_convert_context_copy_16; } 
else if (strcmp(target, "pcre2_convert_context_create_16") == 0) {
return (void*)&pcre2_convert_context_create_16; } 
else if (strcmp(target, "pcre2_convert_context_free_16") == 0) {
return (void*)&pcre2_convert_context_free_16; } 
else if (strcmp(target, "pcre2_set_glob_escape_16") == 0) {
return (void*)&pcre2_set_glob_escape_16; } 
else if (strcmp(target, "pcre2_set_glob_separator_16") == 0) {
return (void*)&pcre2_set_glob_separator_16; } 
else if (strcmp(target, "pcre2_pattern_convert_16") == 0) {
return (void*)&pcre2_pattern_convert_16; } 
else if (strcmp(target, "pcre2_converted_pattern_free_16") == 0) {
return (void*)&pcre2_converted_pattern_free_16; } 
else if (strcmp(target, "pcre2_match_context_copy_16") == 0) {
return (void*)&pcre2_match_context_copy_16; } 
else if (strcmp(target, "pcre2_match_context_create_16") == 0) {
return (void*)&pcre2_match_context_create_16; } 
else if (strcmp(target, "pcre2_match_context_free_16") == 0) {
return (void*)&pcre2_match_context_free_16; } 
else if (strcmp(target, "pcre2_set_depth_limit_16") == 0) {
return (void*)&pcre2_set_depth_limit_16; } 
else if (strcmp(target, "pcre2_set_heap_limit_16") == 0) {
return (void*)&pcre2_set_heap_limit_16; } 
else if (strcmp(target, "pcre2_set_match_limit_16") == 0) {
return (void*)&pcre2_set_match_limit_16; } 
else if (strcmp(target, "pcre2_set_offset_limit_16") == 0) {
return (void*)&pcre2_set_offset_limit_16; } 
else if (strcmp(target, "pcre2_set_recursion_limit_16") == 0) {
return (void*)&pcre2_set_recursion_limit_16; } 
else if (strcmp(target, "pcre2_set_recursion_memory_management_16") == 0) {
return (void*)&pcre2_set_recursion_memory_management_16; } 
else if (strcmp(target, "pcre2_compile_16") == 0) {
return (void*)&pcre2_compile_16; } 
else if (strcmp(target, "pcre2_code_free_16") == 0) {
return (void*)&pcre2_code_free_16; } 
else if (strcmp(target, "pcre2_code_copy_16") == 0) {
return (void*)&pcre2_code_copy_16; } 
else if (strcmp(target, "pcre2_code_copy_with_tables_16") == 0) {
return (void*)&pcre2_code_copy_with_tables_16; } 
else if (strcmp(target, "pcre2_pattern_info_16") == 0) {
return (void*)&pcre2_pattern_info_16; } 
else if (strcmp(target, "pcre2_match_data_create_16") == 0) {
return (void*)&pcre2_match_data_create_16; } 
else if (strcmp(target, "pcre2_match_data_create_from_pattern_16") == 0) {
return (void*)&pcre2_match_data_create_from_pattern_16; } 
else if (strcmp(target, "pcre2_dfa_match_16") == 0) {
return (void*)&pcre2_dfa_match_16; } 
else if (strcmp(target, "pcre2_match_16") == 0) {
return (void*)&pcre2_match_16; } 
else if (strcmp(target, "pcre2_match_data_free_16") == 0) {
return (void*)&pcre2_match_data_free_16; } 
else if (strcmp(target, "pcre2_get_mark_16") == 0) {
return (void*)&pcre2_get_mark_16; } 
else if (strcmp(target, "pcre2_get_ovector_count_16") == 0) {
return (void*)&pcre2_get_ovector_count_16; } 
else if (strcmp(target, "pcre2_get_ovector_pointer_16") == 0) {
return (void*)&pcre2_get_ovector_pointer_16; } 
else if (strcmp(target, "pcre2_get_startchar_16") == 0) {
return (void*)&pcre2_get_startchar_16; } 
else if (strcmp(target, "pcre2_substring_copy_byname_16") == 0) {
return (void*)&pcre2_substring_copy_byname_16; } 
else if (strcmp(target, "pcre2_substring_copy_bynumber_16") == 0) {
return (void*)&pcre2_substring_copy_bynumber_16; } 
else if (strcmp(target, "pcre2_substring_free_16") == 0) {
return (void*)&pcre2_substring_free_16; } 
else if (strcmp(target, "pcre2_substring_get_byname_16") == 0) {
return (void*)&pcre2_substring_get_byname_16; } 
else if (strcmp(target, "pcre2_substring_get_bynumber_16") == 0) {
return (void*)&pcre2_substring_get_bynumber_16; } 
else if (strcmp(target, "pcre2_substring_length_byname_16") == 0) {
return (void*)&pcre2_substring_length_byname_16; } 
else if (strcmp(target, "pcre2_substring_length_bynumber_16") == 0) {
return (void*)&pcre2_substring_length_bynumber_16; } 
else if (strcmp(target, "pcre2_substring_nametable_scan_16") == 0) {
return (void*)&pcre2_substring_nametable_scan_16; } 
else if (strcmp(target, "pcre2_substring_number_from_name_16") == 0) {
return (void*)&pcre2_substring_number_from_name_16; } 
else if (strcmp(target, "pcre2_substring_list_free_16") == 0) {
return (void*)&pcre2_substring_list_free_16; } 
else if (strcmp(target, "pcre2_substring_list_get_16") == 0) {
return (void*)&pcre2_substring_list_get_16; } 
else if (strcmp(target, "pcre2_serialize_encode_16") == 0) {
return (void*)&pcre2_serialize_encode_16; } 
else if (strcmp(target, "pcre2_serialize_decode_16") == 0) {
return (void*)&pcre2_serialize_decode_16; } 
else if (strcmp(target, "pcre2_serialize_get_number_of_codes_16") == 0) {
return (void*)&pcre2_serialize_get_number_of_codes_16; } 
else if (strcmp(target, "pcre2_serialize_free_16") == 0) {
return (void*)&pcre2_serialize_free_16; } 
else if (strcmp(target, "pcre2_substitute_16") == 0) {
return (void*)&pcre2_substitute_16; } 
else if (strcmp(target, "pcre2_jit_compile_16") == 0) {
return (void*)&pcre2_jit_compile_16; } 
else if (strcmp(target, "pcre2_jit_match_16") == 0) {
return (void*)&pcre2_jit_match_16; } 
else if (strcmp(target, "pcre2_jit_free_unused_memory_16") == 0) {
return (void*)&pcre2_jit_free_unused_memory_16; } 
else if (strcmp(target, "pcre2_jit_stack_create_16") == 0) {
return (void*)&pcre2_jit_stack_create_16; } 
else if (strcmp(target, "pcre2_jit_stack_assign_16") == 0) {
return (void*)&pcre2_jit_stack_assign_16; } 
else if (strcmp(target, "pcre2_jit_stack_free_16") == 0) {
return (void*)&pcre2_jit_stack_free_16; } 
else if (strcmp(target, "pcre2_get_error_message_16") == 0) {
return (void*)&pcre2_get_error_message_16; } 
else if (strcmp(target, "pcre2_maketables_16") == 0) {
return (void*)&pcre2_maketables_16; } 
else if (strcmp(target, "pcre2_config_32") == 0) {
return (void*)&pcre2_config_32; } 
else if (strcmp(target, "pcre2_general_context_copy_32") == 0) {
return (void*)&pcre2_general_context_copy_32; } 
else if (strcmp(target, "pcre2_general_context_create_32") == 0) {
return (void*)&pcre2_general_context_create_32; } 
else if (strcmp(target, "pcre2_general_context_free_32") == 0) {
return (void*)&pcre2_general_context_free_32; } 
else if (strcmp(target, "pcre2_compile_context_copy_32") == 0) {
return (void*)&pcre2_compile_context_copy_32; } 
else if (strcmp(target, "pcre2_compile_context_create_32") == 0) {
return (void*)&pcre2_compile_context_create_32; } 
else if (strcmp(target, "pcre2_compile_context_free_32") == 0) {
return (void*)&pcre2_compile_context_free_32; } 
else if (strcmp(target, "pcre2_set_bsr_32") == 0) {
return (void*)&pcre2_set_bsr_32; } 
else if (strcmp(target, "pcre2_set_character_tables_32") == 0) {
return (void*)&pcre2_set_character_tables_32; } 
else if (strcmp(target, "pcre2_set_compile_extra_options_32") == 0) {
return (void*)&pcre2_set_compile_extra_options_32; } 
else if (strcmp(target, "pcre2_set_max_pattern_length_32") == 0) {
return (void*)&pcre2_set_max_pattern_length_32; } 
else if (strcmp(target, "pcre2_set_newline_32") == 0) {
return (void*)&pcre2_set_newline_32; } 
else if (strcmp(target, "pcre2_set_parens_nest_limit_32") == 0) {
return (void*)&pcre2_set_parens_nest_limit_32; } 
else if (strcmp(target, "pcre2_set_compile_recursion_guard_32") == 0) {
return (void*)&pcre2_set_compile_recursion_guard_32; } 
else if (strcmp(target, "pcre2_convert_context_copy_32") == 0) {
return (void*)&pcre2_convert_context_copy_32; } 
else if (strcmp(target, "pcre2_convert_context_create_32") == 0) {
return (void*)&pcre2_convert_context_create_32; } 
else if (strcmp(target, "pcre2_convert_context_free_32") == 0) {
return (void*)&pcre2_convert_context_free_32; } 
else if (strcmp(target, "pcre2_set_glob_escape_32") == 0) {
return (void*)&pcre2_set_glob_escape_32; } 
else if (strcmp(target, "pcre2_set_glob_separator_32") == 0) {
return (void*)&pcre2_set_glob_separator_32; } 
else if (strcmp(target, "pcre2_pattern_convert_32") == 0) {
return (void*)&pcre2_pattern_convert_32; } 
else if (strcmp(target, "pcre2_converted_pattern_free_32") == 0) {
return (void*)&pcre2_converted_pattern_free_32; } 
else if (strcmp(target, "pcre2_match_context_copy_32") == 0) {
return (void*)&pcre2_match_context_copy_32; } 
else if (strcmp(target, "pcre2_match_context_create_32") == 0) {
return (void*)&pcre2_match_context_create_32; } 
else if (strcmp(target, "pcre2_match_context_free_32") == 0) {
return (void*)&pcre2_match_context_free_32; } 
else if (strcmp(target, "pcre2_set_depth_limit_32") == 0) {
return (void*)&pcre2_set_depth_limit_32; } 
else if (strcmp(target, "pcre2_set_heap_limit_32") == 0) {
return (void*)&pcre2_set_heap_limit_32; } 
else if (strcmp(target, "pcre2_set_match_limit_32") == 0) {
return (void*)&pcre2_set_match_limit_32; } 
else if (strcmp(target, "pcre2_set_offset_limit_32") == 0) {
return (void*)&pcre2_set_offset_limit_32; } 
else if (strcmp(target, "pcre2_set_recursion_limit_32") == 0) {
return (void*)&pcre2_set_recursion_limit_32; } 
else if (strcmp(target, "pcre2_set_recursion_memory_management_32") == 0) {
return (void*)&pcre2_set_recursion_memory_management_32; } 
else if (strcmp(target, "pcre2_compile_32") == 0) {
return (void*)&pcre2_compile_32; } 
else if (strcmp(target, "pcre2_code_free_32") == 0) {
return (void*)&pcre2_code_free_32; } 
else if (strcmp(target, "pcre2_code_copy_32") == 0) {
return (void*)&pcre2_code_copy_32; } 
else if (strcmp(target, "pcre2_code_copy_with_tables_32") == 0) {
return (void*)&pcre2_code_copy_with_tables_32; } 
else if (strcmp(target, "pcre2_pattern_info_32") == 0) {
return (void*)&pcre2_pattern_info_32; } 
else if (strcmp(target, "pcre2_match_data_create_32") == 0) {
return (void*)&pcre2_match_data_create_32; } 
else if (strcmp(target, "pcre2_match_data_create_from_pattern_32") == 0) {
return (void*)&pcre2_match_data_create_from_pattern_32; } 
else if (strcmp(target, "pcre2_dfa_match_32") == 0) {
return (void*)&pcre2_dfa_match_32; } 
else if (strcmp(target, "pcre2_match_32") == 0) {
return (void*)&pcre2_match_32; } 
else if (strcmp(target, "pcre2_match_data_free_32") == 0) {
return (void*)&pcre2_match_data_free_32; } 
else if (strcmp(target, "pcre2_get_mark_32") == 0) {
return (void*)&pcre2_get_mark_32; } 
else if (strcmp(target, "pcre2_get_ovector_count_32") == 0) {
return (void*)&pcre2_get_ovector_count_32; } 
else if (strcmp(target, "pcre2_get_ovector_pointer_32") == 0) {
return (void*)&pcre2_get_ovector_pointer_32; } 
else if (strcmp(target, "pcre2_get_startchar_32") == 0) {
return (void*)&pcre2_get_startchar_32; } 
else if (strcmp(target, "pcre2_substring_copy_byname_32") == 0) {
return (void*)&pcre2_substring_copy_byname_32; } 
else if (strcmp(target, "pcre2_substring_copy_bynumber_32") == 0) {
return (void*)&pcre2_substring_copy_bynumber_32; } 
else if (strcmp(target, "pcre2_substring_free_32") == 0) {
return (void*)&pcre2_substring_free_32; } 
else if (strcmp(target, "pcre2_substring_get_byname_32") == 0) {
return (void*)&pcre2_substring_get_byname_32; } 
else if (strcmp(target, "pcre2_substring_get_bynumber_32") == 0) {
return (void*)&pcre2_substring_get_bynumber_32; } 
else if (strcmp(target, "pcre2_substring_length_byname_32") == 0) {
return (void*)&pcre2_substring_length_byname_32; } 
else if (strcmp(target, "pcre2_substring_length_bynumber_32") == 0) {
return (void*)&pcre2_substring_length_bynumber_32; } 
else if (strcmp(target, "pcre2_substring_nametable_scan_32") == 0) {
return (void*)&pcre2_substring_nametable_scan_32; } 
else if (strcmp(target, "pcre2_substring_number_from_name_32") == 0) {
return (void*)&pcre2_substring_number_from_name_32; } 
else if (strcmp(target, "pcre2_substring_list_free_32") == 0) {
return (void*)&pcre2_substring_list_free_32; } 
else if (strcmp(target, "pcre2_substring_list_get_32") == 0) {
return (void*)&pcre2_substring_list_get_32; } 
else if (strcmp(target, "pcre2_serialize_encode_32") == 0) {
return (void*)&pcre2_serialize_encode_32; } 
else if (strcmp(target, "pcre2_serialize_decode_32") == 0) {
return (void*)&pcre2_serialize_decode_32; } 
else if (strcmp(target, "pcre2_serialize_get_number_of_codes_32") == 0) {
return (void*)&pcre2_serialize_get_number_of_codes_32; } 
else if (strcmp(target, "pcre2_serialize_free_32") == 0) {
return (void*)&pcre2_serialize_free_32; } 
else if (strcmp(target, "pcre2_substitute_32") == 0) {
return (void*)&pcre2_substitute_32; } 
else if (strcmp(target, "pcre2_jit_compile_32") == 0) {
return (void*)&pcre2_jit_compile_32; } 
else if (strcmp(target, "pcre2_jit_match_32") == 0) {
return (void*)&pcre2_jit_match_32; } 
else if (strcmp(target, "pcre2_jit_free_unused_memory_32") == 0) {
return (void*)&pcre2_jit_free_unused_memory_32; } 
else if (strcmp(target, "pcre2_jit_stack_create_32") == 0) {
return (void*)&pcre2_jit_stack_create_32; } 
else if (strcmp(target, "pcre2_jit_stack_assign_32") == 0) {
return (void*)&pcre2_jit_stack_assign_32; } 
else if (strcmp(target, "pcre2_jit_stack_free_32") == 0) {
return (void*)&pcre2_jit_stack_free_32; } 
else if (strcmp(target, "pcre2_get_error_message_32") == 0) {
return (void*)&pcre2_get_error_message_32; } 
else if (strcmp(target, "pcre2_maketables_32") == 0) {
return (void*)&pcre2_maketables_32; } 
else if (strcmp(target, "utf8proc_version") == 0) {
return (void*)&utf8proc_version; } 
else if (strcmp(target, "utf8proc_unicode_version") == 0) {
return (void*)&utf8proc_unicode_version; } 
else if (strcmp(target, "utf8proc_errmsg") == 0) {
return (void*)&utf8proc_errmsg; } 
else if (strcmp(target, "utf8proc_iterate") == 0) {
return (void*)&utf8proc_iterate; } 
else if (strcmp(target, "utf8proc_codepoint_valid") == 0) {
return (void*)&utf8proc_codepoint_valid; } 
else if (strcmp(target, "utf8proc_encode_char") == 0) {
return (void*)&utf8proc_encode_char; } 
else if (strcmp(target, "utf8proc_get_property") == 0) {
return (void*)&utf8proc_get_property; } 
else if (strcmp(target, "utf8proc_decompose_char") == 0) {
return (void*)&utf8proc_decompose_char; } 
else if (strcmp(target, "utf8proc_decompose") == 0) {
return (void*)&utf8proc_decompose; } 
else if (strcmp(target, "utf8proc_decompose_custom") == 0) {
return (void*)&utf8proc_decompose_custom; } 
else if (strcmp(target, "utf8proc_normalize_utf32") == 0) {
return (void*)&utf8proc_normalize_utf32; } 
else if (strcmp(target, "utf8proc_reencode") == 0) {
return (void*)&utf8proc_reencode; } 
else if (strcmp(target, "utf8proc_grapheme_break_stateful") == 0) {
return (void*)&utf8proc_grapheme_break_stateful; } 
else if (strcmp(target, "utf8proc_grapheme_break") == 0) {
return (void*)&utf8proc_grapheme_break; } 
else if (strcmp(target, "utf8proc_tolower") == 0) {
return (void*)&utf8proc_tolower; } 
else if (strcmp(target, "utf8proc_toupper") == 0) {
return (void*)&utf8proc_toupper; } 
else if (strcmp(target, "utf8proc_totitle") == 0) {
return (void*)&utf8proc_totitle; } 
else if (strcmp(target, "utf8proc_charwidth") == 0) {
return (void*)&utf8proc_charwidth; } 
else if (strcmp(target, "utf8proc_category") == 0) {
return (void*)&utf8proc_category; } 
else if (strcmp(target, "utf8proc_category_string") == 0) {
return (void*)&utf8proc_category_string; } 
else if (strcmp(target, "utf8proc_map") == 0) {
return (void*)&utf8proc_map; } 
else if (strcmp(target, "utf8proc_map_custom") == 0) {
return (void*)&utf8proc_map_custom; } 
else if (strcmp(target, "utf8proc_NFD") == 0) {
return (void*)&utf8proc_NFD; } 
else if (strcmp(target, "utf8proc_NFC") == 0) {
return (void*)&utf8proc_NFC; } 
else if (strcmp(target, "utf8proc_NFKD") == 0) {
return (void*)&utf8proc_NFKD; } 
else if (strcmp(target, "utf8proc_NFKC") == 0) {
return (void*)&utf8proc_NFKC; } 
else if (strcmp(target, "utf8proc_NFKC_Casefold") == 0) {
return (void*)&utf8proc_NFKC_Casefold; } 
else if (strcmp(target, "__gmp_set_memory_functions") == 0) {
return (void*)&__gmp_set_memory_functions; } 
else if (strcmp(target, "__gmp_get_memory_functions") == 0) {
return (void*)&__gmp_get_memory_functions; } 
else if (strcmp(target, "__gmp_set_alloc_overflow_function") == 0) {
return (void*)&__gmp_set_alloc_overflow_function; } 
else if (strcmp(target, "__gmp_get_alloc_overflow_function") == 0) {
return (void*)&__gmp_get_alloc_overflow_function; } 
else if (strcmp(target, "__gmp_randinit") == 0) {
return (void*)&__gmp_randinit; } 
else if (strcmp(target, "__gmp_randinit_default") == 0) {
return (void*)&__gmp_randinit_default; } 
else if (strcmp(target, "__gmp_randinit_lc_2exp") == 0) {
return (void*)&__gmp_randinit_lc_2exp; } 
else if (strcmp(target, "__gmp_randinit_lc_2exp_size") == 0) {
return (void*)&__gmp_randinit_lc_2exp_size; } 
else if (strcmp(target, "__gmp_randinit_mt") == 0) {
return (void*)&__gmp_randinit_mt; } 
else if (strcmp(target, "__gmp_randinit_set") == 0) {
return (void*)&__gmp_randinit_set; } 
else if (strcmp(target, "__gmp_randseed") == 0) {
return (void*)&__gmp_randseed; } 
else if (strcmp(target, "__gmp_randseed_ui") == 0) {
return (void*)&__gmp_randseed_ui; } 
else if (strcmp(target, "__gmp_randclear") == 0) {
return (void*)&__gmp_randclear; } 
else if (strcmp(target, "__gmp_urandomb_ui") == 0) {
return (void*)&__gmp_urandomb_ui; } 
else if (strcmp(target, "__gmp_urandomm_ui") == 0) {
return (void*)&__gmp_urandomm_ui; } 
else if (strcmp(target, "__gmp_asprintf") == 0) {
return (void*)&__gmp_asprintf; } 
else if (strcmp(target, "__gmp_printf") == 0) {
return (void*)&__gmp_printf; } 
else if (strcmp(target, "__gmp_snprintf") == 0) {
return (void*)&__gmp_snprintf; } 
else if (strcmp(target, "__gmp_sprintf") == 0) {
return (void*)&__gmp_sprintf; } 
else if (strcmp(target, "__gmp_scanf") == 0) {
return (void*)&__gmp_scanf; } 
else if (strcmp(target, "__gmp_sscanf") == 0) {
return (void*)&__gmp_sscanf; } 
else if (strcmp(target, "__gmpz_realloc") == 0) {
return (void*)&__gmpz_realloc; } 
else if (strcmp(target, "__gmpz_abs") == 0) {
return (void*)&__gmpz_abs; } 
else if (strcmp(target, "__gmpz_add") == 0) {
return (void*)&__gmpz_add; } 
else if (strcmp(target, "__gmpz_add_ui") == 0) {
return (void*)&__gmpz_add_ui; } 
else if (strcmp(target, "__gmpz_addmul") == 0) {
return (void*)&__gmpz_addmul; } 
else if (strcmp(target, "__gmpz_addmul_ui") == 0) {
return (void*)&__gmpz_addmul_ui; } 
else if (strcmp(target, "__gmpz_and") == 0) {
return (void*)&__gmpz_and; } 
else if (strcmp(target, "__gmpz_array_init") == 0) {
return (void*)&__gmpz_array_init; } 
else if (strcmp(target, "__gmpz_bin_ui") == 0) {
return (void*)&__gmpz_bin_ui; } 
else if (strcmp(target, "__gmpz_bin_uiui") == 0) {
return (void*)&__gmpz_bin_uiui; } 
else if (strcmp(target, "__gmpz_cdiv_q") == 0) {
return (void*)&__gmpz_cdiv_q; } 
else if (strcmp(target, "__gmpz_cdiv_q_2exp") == 0) {
return (void*)&__gmpz_cdiv_q_2exp; } 
else if (strcmp(target, "__gmpz_cdiv_q_ui") == 0) {
return (void*)&__gmpz_cdiv_q_ui; } 
else if (strcmp(target, "__gmpz_cdiv_qr") == 0) {
return (void*)&__gmpz_cdiv_qr; } 
else if (strcmp(target, "__gmpz_cdiv_qr_ui") == 0) {
return (void*)&__gmpz_cdiv_qr_ui; } 
else if (strcmp(target, "__gmpz_cdiv_r") == 0) {
return (void*)&__gmpz_cdiv_r; } 
else if (strcmp(target, "__gmpz_cdiv_r_2exp") == 0) {
return (void*)&__gmpz_cdiv_r_2exp; } 
else if (strcmp(target, "__gmpz_cdiv_r_ui") == 0) {
return (void*)&__gmpz_cdiv_r_ui; } 
else if (strcmp(target, "__gmpz_cdiv_ui") == 0) {
return (void*)&__gmpz_cdiv_ui; } 
else if (strcmp(target, "__gmpz_clear") == 0) {
return (void*)&__gmpz_clear; } 
else if (strcmp(target, "__gmpz_clears") == 0) {
return (void*)&__gmpz_clears; } 
else if (strcmp(target, "__gmpz_clrbit") == 0) {
return (void*)&__gmpz_clrbit; } 
else if (strcmp(target, "__gmpz_cmp") == 0) {
return (void*)&__gmpz_cmp; } 
else if (strcmp(target, "__gmpz_cmp_d") == 0) {
return (void*)&__gmpz_cmp_d; } 
else if (strcmp(target, "__gmpz_cmp_si") == 0) {
return (void*)&__gmpz_cmp_si; } 
else if (strcmp(target, "__gmpz_cmp_ui") == 0) {
return (void*)&__gmpz_cmp_ui; } 
else if (strcmp(target, "__gmpz_cmpabs") == 0) {
return (void*)&__gmpz_cmpabs; } 
else if (strcmp(target, "__gmpz_cmpabs_d") == 0) {
return (void*)&__gmpz_cmpabs_d; } 
else if (strcmp(target, "__gmpz_cmpabs_ui") == 0) {
return (void*)&__gmpz_cmpabs_ui; } 
else if (strcmp(target, "__gmpz_com") == 0) {
return (void*)&__gmpz_com; } 
else if (strcmp(target, "__gmpz_combit") == 0) {
return (void*)&__gmpz_combit; } 
else if (strcmp(target, "__gmpz_congruent_p") == 0) {
return (void*)&__gmpz_congruent_p; } 
else if (strcmp(target, "__gmpz_congruent_2exp_p") == 0) {
return (void*)&__gmpz_congruent_2exp_p; } 
else if (strcmp(target, "__gmpz_congruent_ui_p") == 0) {
return (void*)&__gmpz_congruent_ui_p; } 
else if (strcmp(target, "__gmpz_divexact") == 0) {
return (void*)&__gmpz_divexact; } 
else if (strcmp(target, "__gmpz_divexact_ui") == 0) {
return (void*)&__gmpz_divexact_ui; } 
else if (strcmp(target, "__gmpz_divisible_p") == 0) {
return (void*)&__gmpz_divisible_p; } 
else if (strcmp(target, "__gmpz_divisible_ui_p") == 0) {
return (void*)&__gmpz_divisible_ui_p; } 
else if (strcmp(target, "__gmpz_divisible_2exp_p") == 0) {
return (void*)&__gmpz_divisible_2exp_p; } 
else if (strcmp(target, "__gmpz_dump") == 0) {
return (void*)&__gmpz_dump; } 
else if (strcmp(target, "__gmpz_export") == 0) {
return (void*)&__gmpz_export; } 
else if (strcmp(target, "__gmpz_fac_ui") == 0) {
return (void*)&__gmpz_fac_ui; } 
else if (strcmp(target, "__gmpz_2fac_ui") == 0) {
return (void*)&__gmpz_2fac_ui; } 
else if (strcmp(target, "__gmpz_mfac_uiui") == 0) {
return (void*)&__gmpz_mfac_uiui; } 
else if (strcmp(target, "__gmpz_primorial_ui") == 0) {
return (void*)&__gmpz_primorial_ui; } 
else if (strcmp(target, "__gmpz_fdiv_q") == 0) {
return (void*)&__gmpz_fdiv_q; } 
else if (strcmp(target, "__gmpz_fdiv_q_2exp") == 0) {
return (void*)&__gmpz_fdiv_q_2exp; } 
else if (strcmp(target, "__gmpz_fdiv_q_ui") == 0) {
return (void*)&__gmpz_fdiv_q_ui; } 
else if (strcmp(target, "__gmpz_fdiv_qr") == 0) {
return (void*)&__gmpz_fdiv_qr; } 
else if (strcmp(target, "__gmpz_fdiv_qr_ui") == 0) {
return (void*)&__gmpz_fdiv_qr_ui; } 
else if (strcmp(target, "__gmpz_fdiv_r") == 0) {
return (void*)&__gmpz_fdiv_r; } 
else if (strcmp(target, "__gmpz_fdiv_r_2exp") == 0) {
return (void*)&__gmpz_fdiv_r_2exp; } 
else if (strcmp(target, "__gmpz_fdiv_r_ui") == 0) {
return (void*)&__gmpz_fdiv_r_ui; } 
else if (strcmp(target, "__gmpz_fdiv_ui") == 0) {
return (void*)&__gmpz_fdiv_ui; } 
else if (strcmp(target, "__gmpz_fib_ui") == 0) {
return (void*)&__gmpz_fib_ui; } 
else if (strcmp(target, "__gmpz_fib2_ui") == 0) {
return (void*)&__gmpz_fib2_ui; } 
else if (strcmp(target, "__gmpz_fits_sint_p") == 0) {
return (void*)&__gmpz_fits_sint_p; } 
else if (strcmp(target, "__gmpz_fits_slong_p") == 0) {
return (void*)&__gmpz_fits_slong_p; } 
else if (strcmp(target, "__gmpz_fits_sshort_p") == 0) {
return (void*)&__gmpz_fits_sshort_p; } 
else if (strcmp(target, "__gmpz_fits_uint_p") == 0) {
return (void*)&__gmpz_fits_uint_p; } 
else if (strcmp(target, "__gmpz_fits_ulong_p") == 0) {
return (void*)&__gmpz_fits_ulong_p; } 
else if (strcmp(target, "__gmpz_fits_ushort_p") == 0) {
return (void*)&__gmpz_fits_ushort_p; } 
else if (strcmp(target, "__gmpz_gcd") == 0) {
return (void*)&__gmpz_gcd; } 
else if (strcmp(target, "__gmpz_gcd_ui") == 0) {
return (void*)&__gmpz_gcd_ui; } 
else if (strcmp(target, "__gmpz_gcdext") == 0) {
return (void*)&__gmpz_gcdext; } 
else if (strcmp(target, "__gmpz_get_d") == 0) {
return (void*)&__gmpz_get_d; } 
else if (strcmp(target, "__gmpz_get_d_2exp") == 0) {
return (void*)&__gmpz_get_d_2exp; } 
else if (strcmp(target, "__gmpz_get_si") == 0) {
return (void*)&__gmpz_get_si; } 
else if (strcmp(target, "__gmpz_get_str") == 0) {
return (void*)&__gmpz_get_str; } 
else if (strcmp(target, "__gmpz_get_ui") == 0) {
return (void*)&__gmpz_get_ui; } 
else if (strcmp(target, "__gmpz_getlimbn") == 0) {
return (void*)&__gmpz_getlimbn; } 
else if (strcmp(target, "__gmpz_hamdist") == 0) {
return (void*)&__gmpz_hamdist; } 
else if (strcmp(target, "__gmpz_import") == 0) {
return (void*)&__gmpz_import; } 
else if (strcmp(target, "__gmpz_init") == 0) {
return (void*)&__gmpz_init; } 
else if (strcmp(target, "__gmpz_init2") == 0) {
return (void*)&__gmpz_init2; } 
else if (strcmp(target, "__gmpz_inits") == 0) {
return (void*)&__gmpz_inits; } 
else if (strcmp(target, "__gmpz_init_set") == 0) {
return (void*)&__gmpz_init_set; } 
else if (strcmp(target, "__gmpz_init_set_d") == 0) {
return (void*)&__gmpz_init_set_d; } 
else if (strcmp(target, "__gmpz_init_set_si") == 0) {
return (void*)&__gmpz_init_set_si; } 
else if (strcmp(target, "__gmpz_init_set_str") == 0) {
return (void*)&__gmpz_init_set_str; } 
else if (strcmp(target, "__gmpz_init_set_ui") == 0) {
return (void*)&__gmpz_init_set_ui; } 
else if (strcmp(target, "__gmpz_invert") == 0) {
return (void*)&__gmpz_invert; } 
else if (strcmp(target, "__gmpz_ior") == 0) {
return (void*)&__gmpz_ior; } 
else if (strcmp(target, "__gmpz_jacobi") == 0) {
return (void*)&__gmpz_jacobi; } 
else if (strcmp(target, "__gmpz_kronecker_si") == 0) {
return (void*)&__gmpz_kronecker_si; } 
else if (strcmp(target, "__gmpz_kronecker_ui") == 0) {
return (void*)&__gmpz_kronecker_ui; } 
else if (strcmp(target, "__gmpz_si_kronecker") == 0) {
return (void*)&__gmpz_si_kronecker; } 
else if (strcmp(target, "__gmpz_ui_kronecker") == 0) {
return (void*)&__gmpz_ui_kronecker; } 
else if (strcmp(target, "__gmpz_lcm") == 0) {
return (void*)&__gmpz_lcm; } 
else if (strcmp(target, "__gmpz_lcm_ui") == 0) {
return (void*)&__gmpz_lcm_ui; } 
else if (strcmp(target, "__gmpz_lucnum_ui") == 0) {
return (void*)&__gmpz_lucnum_ui; } 
else if (strcmp(target, "__gmpz_lucnum2_ui") == 0) {
return (void*)&__gmpz_lucnum2_ui; } 
else if (strcmp(target, "__gmpz_millerrabin") == 0) {
return (void*)&__gmpz_millerrabin; } 
else if (strcmp(target, "__gmpz_mod") == 0) {
return (void*)&__gmpz_mod; } 
else if (strcmp(target, "__gmpz_mul") == 0) {
return (void*)&__gmpz_mul; } 
else if (strcmp(target, "__gmpz_mul_2exp") == 0) {
return (void*)&__gmpz_mul_2exp; } 
else if (strcmp(target, "__gmpz_mul_si") == 0) {
return (void*)&__gmpz_mul_si; } 
else if (strcmp(target, "__gmpz_mul_ui") == 0) {
return (void*)&__gmpz_mul_ui; } 
else if (strcmp(target, "__gmpz_neg") == 0) {
return (void*)&__gmpz_neg; } 
else if (strcmp(target, "__gmpz_nextprime") == 0) {
return (void*)&__gmpz_nextprime; } 
else if (strcmp(target, "__gmpz_perfect_power_p") == 0) {
return (void*)&__gmpz_perfect_power_p; } 
else if (strcmp(target, "__gmpz_perfect_square_p") == 0) {
return (void*)&__gmpz_perfect_square_p; } 
else if (strcmp(target, "__gmpz_popcount") == 0) {
return (void*)&__gmpz_popcount; } 
else if (strcmp(target, "__gmpz_pow_ui") == 0) {
return (void*)&__gmpz_pow_ui; } 
else if (strcmp(target, "__gmpz_powm") == 0) {
return (void*)&__gmpz_powm; } 
else if (strcmp(target, "__gmpz_powm_sec") == 0) {
return (void*)&__gmpz_powm_sec; } 
else if (strcmp(target, "__gmpz_powm_ui") == 0) {
return (void*)&__gmpz_powm_ui; } 
else if (strcmp(target, "__gmpz_probab_prime_p") == 0) {
return (void*)&__gmpz_probab_prime_p; } 
else if (strcmp(target, "__gmpz_random") == 0) {
return (void*)&__gmpz_random; } 
else if (strcmp(target, "__gmpz_random2") == 0) {
return (void*)&__gmpz_random2; } 
else if (strcmp(target, "__gmpz_realloc2") == 0) {
return (void*)&__gmpz_realloc2; } 
else if (strcmp(target, "__gmpz_remove") == 0) {
return (void*)&__gmpz_remove; } 
else if (strcmp(target, "__gmpz_root") == 0) {
return (void*)&__gmpz_root; } 
else if (strcmp(target, "__gmpz_rootrem") == 0) {
return (void*)&__gmpz_rootrem; } 
else if (strcmp(target, "__gmpz_rrandomb") == 0) {
return (void*)&__gmpz_rrandomb; } 
else if (strcmp(target, "__gmpz_scan0") == 0) {
return (void*)&__gmpz_scan0; } 
else if (strcmp(target, "__gmpz_scan1") == 0) {
return (void*)&__gmpz_scan1; } 
else if (strcmp(target, "__gmpz_set") == 0) {
return (void*)&__gmpz_set; } 
else if (strcmp(target, "__gmpz_set_d") == 0) {
return (void*)&__gmpz_set_d; } 
else if (strcmp(target, "__gmpz_set_f") == 0) {
return (void*)&__gmpz_set_f; } 
else if (strcmp(target, "__gmpz_set_q") == 0) {
return (void*)&__gmpz_set_q; } 
else if (strcmp(target, "__gmpz_set_si") == 0) {
return (void*)&__gmpz_set_si; } 
else if (strcmp(target, "__gmpz_set_str") == 0) {
return (void*)&__gmpz_set_str; } 
else if (strcmp(target, "__gmpz_set_ui") == 0) {
return (void*)&__gmpz_set_ui; } 
else if (strcmp(target, "__gmpz_setbit") == 0) {
return (void*)&__gmpz_setbit; } 
else if (strcmp(target, "__gmpz_size") == 0) {
return (void*)&__gmpz_size; } 
else if (strcmp(target, "__gmpz_sizeinbase") == 0) {
return (void*)&__gmpz_sizeinbase; } 
else if (strcmp(target, "__gmpz_sqrt") == 0) {
return (void*)&__gmpz_sqrt; } 
else if (strcmp(target, "__gmpz_sqrtrem") == 0) {
return (void*)&__gmpz_sqrtrem; } 
else if (strcmp(target, "__gmpz_sub") == 0) {
return (void*)&__gmpz_sub; } 
else if (strcmp(target, "__gmpz_sub_ui") == 0) {
return (void*)&__gmpz_sub_ui; } 
else if (strcmp(target, "__gmpz_ui_sub") == 0) {
return (void*)&__gmpz_ui_sub; } 
else if (strcmp(target, "__gmpz_submul") == 0) {
return (void*)&__gmpz_submul; } 
else if (strcmp(target, "__gmpz_submul_ui") == 0) {
return (void*)&__gmpz_submul_ui; } 
else if (strcmp(target, "__gmpz_swap") == 0) {
return (void*)&__gmpz_swap; } 
else if (strcmp(target, "__gmpz_tdiv_ui") == 0) {
return (void*)&__gmpz_tdiv_ui; } 
else if (strcmp(target, "__gmpz_tdiv_q") == 0) {
return (void*)&__gmpz_tdiv_q; } 
else if (strcmp(target, "__gmpz_tdiv_q_2exp") == 0) {
return (void*)&__gmpz_tdiv_q_2exp; } 
else if (strcmp(target, "__gmpz_tdiv_q_ui") == 0) {
return (void*)&__gmpz_tdiv_q_ui; } 
else if (strcmp(target, "__gmpz_tdiv_qr") == 0) {
return (void*)&__gmpz_tdiv_qr; } 
else if (strcmp(target, "__gmpz_tdiv_qr_ui") == 0) {
return (void*)&__gmpz_tdiv_qr_ui; } 
else if (strcmp(target, "__gmpz_tdiv_r") == 0) {
return (void*)&__gmpz_tdiv_r; } 
else if (strcmp(target, "__gmpz_tdiv_r_2exp") == 0) {
return (void*)&__gmpz_tdiv_r_2exp; } 
else if (strcmp(target, "__gmpz_tdiv_r_ui") == 0) {
return (void*)&__gmpz_tdiv_r_ui; } 
else if (strcmp(target, "__gmpz_tstbit") == 0) {
return (void*)&__gmpz_tstbit; } 
else if (strcmp(target, "__gmpz_ui_pow_ui") == 0) {
return (void*)&__gmpz_ui_pow_ui; } 
else if (strcmp(target, "__gmpz_urandomb") == 0) {
return (void*)&__gmpz_urandomb; } 
else if (strcmp(target, "__gmpz_urandomm") == 0) {
return (void*)&__gmpz_urandomm; } 
else if (strcmp(target, "__gmpz_xor") == 0) {
return (void*)&__gmpz_xor; } 
else if (strcmp(target, "__gmpz_limbs_read") == 0) {
return (void*)&__gmpz_limbs_read; } 
else if (strcmp(target, "__gmpz_limbs_write") == 0) {
return (void*)&__gmpz_limbs_write; } 
else if (strcmp(target, "__gmpz_limbs_modify") == 0) {
return (void*)&__gmpz_limbs_modify; } 
else if (strcmp(target, "__gmpz_limbs_finish") == 0) {
return (void*)&__gmpz_limbs_finish; } 
else if (strcmp(target, "__gmpz_roinit_n") == 0) {
return (void*)&__gmpz_roinit_n; } 
else if (strcmp(target, "__gmpq_abs") == 0) {
return (void*)&__gmpq_abs; } 
else if (strcmp(target, "__gmpq_add") == 0) {
return (void*)&__gmpq_add; } 
else if (strcmp(target, "__gmpq_canonicalize") == 0) {
return (void*)&__gmpq_canonicalize; } 
else if (strcmp(target, "__gmpq_clear") == 0) {
return (void*)&__gmpq_clear; } 
else if (strcmp(target, "__gmpq_clears") == 0) {
return (void*)&__gmpq_clears; } 
else if (strcmp(target, "__gmpq_cmp") == 0) {
return (void*)&__gmpq_cmp; } 
else if (strcmp(target, "__gmpq_cmp_si") == 0) {
return (void*)&__gmpq_cmp_si; } 
else if (strcmp(target, "__gmpq_cmp_ui") == 0) {
return (void*)&__gmpq_cmp_ui; } 
else if (strcmp(target, "__gmpq_cmp_z") == 0) {
return (void*)&__gmpq_cmp_z; } 
else if (strcmp(target, "__gmpq_div") == 0) {
return (void*)&__gmpq_div; } 
else if (strcmp(target, "__gmpq_div_2exp") == 0) {
return (void*)&__gmpq_div_2exp; } 
else if (strcmp(target, "__gmpq_equal") == 0) {
return (void*)&__gmpq_equal; } 
else if (strcmp(target, "__gmpq_get_num") == 0) {
return (void*)&__gmpq_get_num; } 
else if (strcmp(target, "__gmpq_get_den") == 0) {
return (void*)&__gmpq_get_den; } 
else if (strcmp(target, "__gmpq_get_d") == 0) {
return (void*)&__gmpq_get_d; } 
else if (strcmp(target, "__gmpq_get_str") == 0) {
return (void*)&__gmpq_get_str; } 
else if (strcmp(target, "__gmpq_init") == 0) {
return (void*)&__gmpq_init; } 
else if (strcmp(target, "__gmpq_inits") == 0) {
return (void*)&__gmpq_inits; } 
else if (strcmp(target, "__gmpq_inv") == 0) {
return (void*)&__gmpq_inv; } 
else if (strcmp(target, "__gmpq_mul") == 0) {
return (void*)&__gmpq_mul; } 
else if (strcmp(target, "__gmpq_mul_2exp") == 0) {
return (void*)&__gmpq_mul_2exp; } 
else if (strcmp(target, "__gmpq_neg") == 0) {
return (void*)&__gmpq_neg; } 
else if (strcmp(target, "__gmpq_set") == 0) {
return (void*)&__gmpq_set; } 
else if (strcmp(target, "__gmpq_set_d") == 0) {
return (void*)&__gmpq_set_d; } 
else if (strcmp(target, "__gmpq_set_den") == 0) {
return (void*)&__gmpq_set_den; } 
else if (strcmp(target, "__gmpq_set_f") == 0) {
return (void*)&__gmpq_set_f; } 
else if (strcmp(target, "__gmpq_set_num") == 0) {
return (void*)&__gmpq_set_num; } 
else if (strcmp(target, "__gmpq_set_si") == 0) {
return (void*)&__gmpq_set_si; } 
else if (strcmp(target, "__gmpq_set_str") == 0) {
return (void*)&__gmpq_set_str; } 
else if (strcmp(target, "__gmpq_set_ui") == 0) {
return (void*)&__gmpq_set_ui; } 
else if (strcmp(target, "__gmpq_set_z") == 0) {
return (void*)&__gmpq_set_z; } 
else if (strcmp(target, "__gmpq_sub") == 0) {
return (void*)&__gmpq_sub; } 
else if (strcmp(target, "__gmpq_swap") == 0) {
return (void*)&__gmpq_swap; } 
else if (strcmp(target, "__gmpf_abs") == 0) {
return (void*)&__gmpf_abs; } 
else if (strcmp(target, "__gmpf_add") == 0) {
return (void*)&__gmpf_add; } 
else if (strcmp(target, "__gmpf_add_ui") == 0) {
return (void*)&__gmpf_add_ui; } 
else if (strcmp(target, "__gmpf_ceil") == 0) {
return (void*)&__gmpf_ceil; } 
else if (strcmp(target, "__gmpf_clear") == 0) {
return (void*)&__gmpf_clear; } 
else if (strcmp(target, "__gmpf_clears") == 0) {
return (void*)&__gmpf_clears; } 
else if (strcmp(target, "__gmpf_cmp") == 0) {
return (void*)&__gmpf_cmp; } 
else if (strcmp(target, "__gmpf_cmp_z") == 0) {
return (void*)&__gmpf_cmp_z; } 
else if (strcmp(target, "__gmpf_cmp_d") == 0) {
return (void*)&__gmpf_cmp_d; } 
else if (strcmp(target, "__gmpf_cmp_si") == 0) {
return (void*)&__gmpf_cmp_si; } 
else if (strcmp(target, "__gmpf_cmp_ui") == 0) {
return (void*)&__gmpf_cmp_ui; } 
else if (strcmp(target, "__gmpf_div") == 0) {
return (void*)&__gmpf_div; } 
else if (strcmp(target, "__gmpf_div_2exp") == 0) {
return (void*)&__gmpf_div_2exp; } 
else if (strcmp(target, "__gmpf_div_ui") == 0) {
return (void*)&__gmpf_div_ui; } 
else if (strcmp(target, "__gmpf_dump") == 0) {
return (void*)&__gmpf_dump; } 
else if (strcmp(target, "__gmpf_eq") == 0) {
return (void*)&__gmpf_eq; } 
else if (strcmp(target, "__gmpf_fits_sint_p") == 0) {
return (void*)&__gmpf_fits_sint_p; } 
else if (strcmp(target, "__gmpf_fits_slong_p") == 0) {
return (void*)&__gmpf_fits_slong_p; } 
else if (strcmp(target, "__gmpf_fits_sshort_p") == 0) {
return (void*)&__gmpf_fits_sshort_p; } 
else if (strcmp(target, "__gmpf_fits_uint_p") == 0) {
return (void*)&__gmpf_fits_uint_p; } 
else if (strcmp(target, "__gmpf_fits_ulong_p") == 0) {
return (void*)&__gmpf_fits_ulong_p; } 
else if (strcmp(target, "__gmpf_fits_ushort_p") == 0) {
return (void*)&__gmpf_fits_ushort_p; } 
else if (strcmp(target, "__gmpf_floor") == 0) {
return (void*)&__gmpf_floor; } 
else if (strcmp(target, "__gmpf_get_d") == 0) {
return (void*)&__gmpf_get_d; } 
else if (strcmp(target, "__gmpf_get_d_2exp") == 0) {
return (void*)&__gmpf_get_d_2exp; } 
else if (strcmp(target, "__gmpf_get_default_prec") == 0) {
return (void*)&__gmpf_get_default_prec; } 
else if (strcmp(target, "__gmpf_get_prec") == 0) {
return (void*)&__gmpf_get_prec; } 
else if (strcmp(target, "__gmpf_get_si") == 0) {
return (void*)&__gmpf_get_si; } 
else if (strcmp(target, "__gmpf_get_str") == 0) {
return (void*)&__gmpf_get_str; } 
else if (strcmp(target, "__gmpf_get_ui") == 0) {
return (void*)&__gmpf_get_ui; } 
else if (strcmp(target, "__gmpf_init") == 0) {
return (void*)&__gmpf_init; } 
else if (strcmp(target, "__gmpf_init2") == 0) {
return (void*)&__gmpf_init2; } 
else if (strcmp(target, "__gmpf_inits") == 0) {
return (void*)&__gmpf_inits; } 
else if (strcmp(target, "__gmpf_init_set") == 0) {
return (void*)&__gmpf_init_set; } 
else if (strcmp(target, "__gmpf_init_set_d") == 0) {
return (void*)&__gmpf_init_set_d; } 
else if (strcmp(target, "__gmpf_init_set_si") == 0) {
return (void*)&__gmpf_init_set_si; } 
else if (strcmp(target, "__gmpf_init_set_str") == 0) {
return (void*)&__gmpf_init_set_str; } 
else if (strcmp(target, "__gmpf_init_set_ui") == 0) {
return (void*)&__gmpf_init_set_ui; } 
else if (strcmp(target, "__gmpf_integer_p") == 0) {
return (void*)&__gmpf_integer_p; } 
else if (strcmp(target, "__gmpf_mul") == 0) {
return (void*)&__gmpf_mul; } 
else if (strcmp(target, "__gmpf_mul_2exp") == 0) {
return (void*)&__gmpf_mul_2exp; } 
else if (strcmp(target, "__gmpf_mul_ui") == 0) {
return (void*)&__gmpf_mul_ui; } 
else if (strcmp(target, "__gmpf_neg") == 0) {
return (void*)&__gmpf_neg; } 
else if (strcmp(target, "__gmpf_pow_ui") == 0) {
return (void*)&__gmpf_pow_ui; } 
else if (strcmp(target, "__gmpf_random2") == 0) {
return (void*)&__gmpf_random2; } 
else if (strcmp(target, "__gmpf_reldiff") == 0) {
return (void*)&__gmpf_reldiff; } 
else if (strcmp(target, "__gmpf_set") == 0) {
return (void*)&__gmpf_set; } 
else if (strcmp(target, "__gmpf_set_d") == 0) {
return (void*)&__gmpf_set_d; } 
else if (strcmp(target, "__gmpf_set_default_prec") == 0) {
return (void*)&__gmpf_set_default_prec; } 
else if (strcmp(target, "__gmpf_set_prec") == 0) {
return (void*)&__gmpf_set_prec; } 
else if (strcmp(target, "__gmpf_set_prec_raw") == 0) {
return (void*)&__gmpf_set_prec_raw; } 
else if (strcmp(target, "__gmpf_set_q") == 0) {
return (void*)&__gmpf_set_q; } 
else if (strcmp(target, "__gmpf_set_si") == 0) {
return (void*)&__gmpf_set_si; } 
else if (strcmp(target, "__gmpf_set_str") == 0) {
return (void*)&__gmpf_set_str; } 
else if (strcmp(target, "__gmpf_set_ui") == 0) {
return (void*)&__gmpf_set_ui; } 
else if (strcmp(target, "__gmpf_set_z") == 0) {
return (void*)&__gmpf_set_z; } 
else if (strcmp(target, "__gmpf_size") == 0) {
return (void*)&__gmpf_size; } 
else if (strcmp(target, "__gmpf_sqrt") == 0) {
return (void*)&__gmpf_sqrt; } 
else if (strcmp(target, "__gmpf_sqrt_ui") == 0) {
return (void*)&__gmpf_sqrt_ui; } 
else if (strcmp(target, "__gmpf_sub") == 0) {
return (void*)&__gmpf_sub; } 
else if (strcmp(target, "__gmpf_sub_ui") == 0) {
return (void*)&__gmpf_sub_ui; } 
else if (strcmp(target, "__gmpf_swap") == 0) {
return (void*)&__gmpf_swap; } 
else if (strcmp(target, "__gmpf_trunc") == 0) {
return (void*)&__gmpf_trunc; } 
else if (strcmp(target, "__gmpf_ui_div") == 0) {
return (void*)&__gmpf_ui_div; } 
else if (strcmp(target, "__gmpf_ui_sub") == 0) {
return (void*)&__gmpf_ui_sub; } 
else if (strcmp(target, "__gmpf_urandomb") == 0) {
return (void*)&__gmpf_urandomb; } 
else if (strcmp(target, "__gmpn_add") == 0) {
return (void*)&__gmpn_add; } 
else if (strcmp(target, "__gmpn_add_1") == 0) {
return (void*)&__gmpn_add_1; } 
else if (strcmp(target, "__gmpn_add_n") == 0) {
return (void*)&__gmpn_add_n; } 
else if (strcmp(target, "__gmpn_addmul_1") == 0) {
return (void*)&__gmpn_addmul_1; } 
else if (strcmp(target, "__gmpn_cmp") == 0) {
return (void*)&__gmpn_cmp; } 
else if (strcmp(target, "__gmpn_zero_p") == 0) {
return (void*)&__gmpn_zero_p; } 
else if (strcmp(target, "__gmpn_divexact_1") == 0) {
return (void*)&__gmpn_divexact_1; } 
else if (strcmp(target, "__gmpn_divexact_by3c") == 0) {
return (void*)&__gmpn_divexact_by3c; } 
else if (strcmp(target, "__gmpn_divrem") == 0) {
return (void*)&__gmpn_divrem; } 
else if (strcmp(target, "__gmpn_divrem_1") == 0) {
return (void*)&__gmpn_divrem_1; } 
else if (strcmp(target, "__gmpn_divrem_2") == 0) {
return (void*)&__gmpn_divrem_2; } 
else if (strcmp(target, "__gmpn_div_qr_1") == 0) {
return (void*)&__gmpn_div_qr_1; } 
else if (strcmp(target, "__gmpn_div_qr_2") == 0) {
return (void*)&__gmpn_div_qr_2; } 
else if (strcmp(target, "__gmpn_gcd") == 0) {
return (void*)&__gmpn_gcd; } 
else if (strcmp(target, "__gmpn_gcd_1") == 0) {
return (void*)&__gmpn_gcd_1; } 
else if (strcmp(target, "__gmpn_gcdext_1") == 0) {
return (void*)&__gmpn_gcdext_1; } 
else if (strcmp(target, "__gmpn_gcdext") == 0) {
return (void*)&__gmpn_gcdext; } 
else if (strcmp(target, "__gmpn_get_str") == 0) {
return (void*)&__gmpn_get_str; } 
else if (strcmp(target, "__gmpn_hamdist") == 0) {
return (void*)&__gmpn_hamdist; } 
else if (strcmp(target, "__gmpn_lshift") == 0) {
return (void*)&__gmpn_lshift; } 
else if (strcmp(target, "__gmpn_mod_1") == 0) {
return (void*)&__gmpn_mod_1; } 
else if (strcmp(target, "__gmpn_mul") == 0) {
return (void*)&__gmpn_mul; } 
else if (strcmp(target, "__gmpn_mul_1") == 0) {
return (void*)&__gmpn_mul_1; } 
else if (strcmp(target, "__gmpn_mul_n") == 0) {
return (void*)&__gmpn_mul_n; } 
else if (strcmp(target, "__gmpn_sqr") == 0) {
return (void*)&__gmpn_sqr; } 
else if (strcmp(target, "__gmpn_neg") == 0) {
return (void*)&__gmpn_neg; } 
else if (strcmp(target, "__gmpn_com") == 0) {
return (void*)&__gmpn_com; } 
else if (strcmp(target, "__gmpn_perfect_square_p") == 0) {
return (void*)&__gmpn_perfect_square_p; } 
else if (strcmp(target, "__gmpn_perfect_power_p") == 0) {
return (void*)&__gmpn_perfect_power_p; } 
else if (strcmp(target, "__gmpn_popcount") == 0) {
return (void*)&__gmpn_popcount; } 
else if (strcmp(target, "__gmpn_pow_1") == 0) {
return (void*)&__gmpn_pow_1; } 
else if (strcmp(target, "__gmpn_preinv_mod_1") == 0) {
return (void*)&__gmpn_preinv_mod_1; } 
else if (strcmp(target, "__gmpn_random") == 0) {
return (void*)&__gmpn_random; } 
else if (strcmp(target, "__gmpn_random2") == 0) {
return (void*)&__gmpn_random2; } 
else if (strcmp(target, "__gmpn_rshift") == 0) {
return (void*)&__gmpn_rshift; } 
else if (strcmp(target, "__gmpn_scan0") == 0) {
return (void*)&__gmpn_scan0; } 
else if (strcmp(target, "__gmpn_scan1") == 0) {
return (void*)&__gmpn_scan1; } 
else if (strcmp(target, "__gmpn_set_str") == 0) {
return (void*)&__gmpn_set_str; } 
else if (strcmp(target, "__gmpn_sizeinbase") == 0) {
return (void*)&__gmpn_sizeinbase; } 
else if (strcmp(target, "__gmpn_sqrtrem") == 0) {
return (void*)&__gmpn_sqrtrem; } 
else if (strcmp(target, "__gmpn_sub") == 0) {
return (void*)&__gmpn_sub; } 
else if (strcmp(target, "__gmpn_sub_1") == 0) {
return (void*)&__gmpn_sub_1; } 
else if (strcmp(target, "__gmpn_sub_n") == 0) {
return (void*)&__gmpn_sub_n; } 
else if (strcmp(target, "__gmpn_submul_1") == 0) {
return (void*)&__gmpn_submul_1; } 
else if (strcmp(target, "__gmpn_tdiv_qr") == 0) {
return (void*)&__gmpn_tdiv_qr; } 
else if (strcmp(target, "__gmpn_and_n") == 0) {
return (void*)&__gmpn_and_n; } 
else if (strcmp(target, "__gmpn_andn_n") == 0) {
return (void*)&__gmpn_andn_n; } 
else if (strcmp(target, "__gmpn_nand_n") == 0) {
return (void*)&__gmpn_nand_n; } 
else if (strcmp(target, "__gmpn_ior_n") == 0) {
return (void*)&__gmpn_ior_n; } 
else if (strcmp(target, "__gmpn_iorn_n") == 0) {
return (void*)&__gmpn_iorn_n; } 
else if (strcmp(target, "__gmpn_nior_n") == 0) {
return (void*)&__gmpn_nior_n; } 
else if (strcmp(target, "__gmpn_xor_n") == 0) {
return (void*)&__gmpn_xor_n; } 
else if (strcmp(target, "__gmpn_xnor_n") == 0) {
return (void*)&__gmpn_xnor_n; } 
else if (strcmp(target, "__gmpn_copyi") == 0) {
return (void*)&__gmpn_copyi; } 
else if (strcmp(target, "__gmpn_copyd") == 0) {
return (void*)&__gmpn_copyd; } 
else if (strcmp(target, "__gmpn_zero") == 0) {
return (void*)&__gmpn_zero; } 
else if (strcmp(target, "__gmpn_cnd_add_n") == 0) {
return (void*)&__gmpn_cnd_add_n; } 
else if (strcmp(target, "__gmpn_cnd_sub_n") == 0) {
return (void*)&__gmpn_cnd_sub_n; } 
else if (strcmp(target, "__gmpn_sec_add_1") == 0) {
return (void*)&__gmpn_sec_add_1; } 
else if (strcmp(target, "__gmpn_sec_add_1_itch") == 0) {
return (void*)&__gmpn_sec_add_1_itch; } 
else if (strcmp(target, "__gmpn_sec_sub_1") == 0) {
return (void*)&__gmpn_sec_sub_1; } 
else if (strcmp(target, "__gmpn_sec_sub_1_itch") == 0) {
return (void*)&__gmpn_sec_sub_1_itch; } 
else if (strcmp(target, "__gmpn_cnd_swap") == 0) {
return (void*)&__gmpn_cnd_swap; } 
else if (strcmp(target, "__gmpn_sec_mul") == 0) {
return (void*)&__gmpn_sec_mul; } 
else if (strcmp(target, "__gmpn_sec_mul_itch") == 0) {
return (void*)&__gmpn_sec_mul_itch; } 
else if (strcmp(target, "__gmpn_sec_sqr") == 0) {
return (void*)&__gmpn_sec_sqr; } 
else if (strcmp(target, "__gmpn_sec_sqr_itch") == 0) {
return (void*)&__gmpn_sec_sqr_itch; } 
else if (strcmp(target, "__gmpn_sec_powm") == 0) {
return (void*)&__gmpn_sec_powm; } 
else if (strcmp(target, "__gmpn_sec_powm_itch") == 0) {
return (void*)&__gmpn_sec_powm_itch; } 
else if (strcmp(target, "__gmpn_sec_tabselect") == 0) {
return (void*)&__gmpn_sec_tabselect; } 
else if (strcmp(target, "__gmpn_sec_div_qr") == 0) {
return (void*)&__gmpn_sec_div_qr; } 
else if (strcmp(target, "__gmpn_sec_div_qr_itch") == 0) {
return (void*)&__gmpn_sec_div_qr_itch; } 
else if (strcmp(target, "__gmpn_sec_div_r") == 0) {
return (void*)&__gmpn_sec_div_r; } 
else if (strcmp(target, "__gmpn_sec_div_r_itch") == 0) {
return (void*)&__gmpn_sec_div_r_itch; } 
else if (strcmp(target, "__gmpn_sec_invert") == 0) {
return (void*)&__gmpn_sec_invert; } 
else if (strcmp(target, "__gmpn_sec_invert_itch") == 0) {
return (void*)&__gmpn_sec_invert_itch; } 
else if (strcmp(target, "mpfr_get_version") == 0) {
return (void*)&mpfr_get_version; } 
else if (strcmp(target, "mpfr_get_patches") == 0) {
return (void*)&mpfr_get_patches; } 
else if (strcmp(target, "mpfr_buildopt_tls_p") == 0) {
return (void*)&mpfr_buildopt_tls_p; } 
else if (strcmp(target, "mpfr_buildopt_float128_p") == 0) {
return (void*)&mpfr_buildopt_float128_p; } 
else if (strcmp(target, "mpfr_buildopt_decimal_p") == 0) {
return (void*)&mpfr_buildopt_decimal_p; } 
else if (strcmp(target, "mpfr_buildopt_gmpinternals_p") == 0) {
return (void*)&mpfr_buildopt_gmpinternals_p; } 
else if (strcmp(target, "mpfr_buildopt_sharedcache_p") == 0) {
return (void*)&mpfr_buildopt_sharedcache_p; } 
else if (strcmp(target, "mpfr_buildopt_tune_case") == 0) {
return (void*)&mpfr_buildopt_tune_case; } 
else if (strcmp(target, "mpfr_get_emin") == 0) {
return (void*)&mpfr_get_emin; } 
else if (strcmp(target, "mpfr_set_emin") == 0) {
return (void*)&mpfr_set_emin; } 
else if (strcmp(target, "mpfr_get_emin_min") == 0) {
return (void*)&mpfr_get_emin_min; } 
else if (strcmp(target, "mpfr_get_emin_max") == 0) {
return (void*)&mpfr_get_emin_max; } 
else if (strcmp(target, "mpfr_get_emax") == 0) {
return (void*)&mpfr_get_emax; } 
else if (strcmp(target, "mpfr_set_emax") == 0) {
return (void*)&mpfr_set_emax; } 
else if (strcmp(target, "mpfr_get_emax_min") == 0) {
return (void*)&mpfr_get_emax_min; } 
else if (strcmp(target, "mpfr_get_emax_max") == 0) {
return (void*)&mpfr_get_emax_max; } 
else if (strcmp(target, "mpfr_set_default_rounding_mode") == 0) {
return (void*)&mpfr_set_default_rounding_mode; } 
else if (strcmp(target, "mpfr_get_default_rounding_mode") == 0) {
return (void*)&mpfr_get_default_rounding_mode; } 
else if (strcmp(target, "mpfr_print_rnd_mode") == 0) {
return (void*)&mpfr_print_rnd_mode; } 
else if (strcmp(target, "mpfr_clear_flags") == 0) {
return (void*)&mpfr_clear_flags; } 
else if (strcmp(target, "mpfr_clear_underflow") == 0) {
return (void*)&mpfr_clear_underflow; } 
else if (strcmp(target, "mpfr_clear_overflow") == 0) {
return (void*)&mpfr_clear_overflow; } 
else if (strcmp(target, "mpfr_clear_divby0") == 0) {
return (void*)&mpfr_clear_divby0; } 
else if (strcmp(target, "mpfr_clear_nanflag") == 0) {
return (void*)&mpfr_clear_nanflag; } 
else if (strcmp(target, "mpfr_clear_inexflag") == 0) {
return (void*)&mpfr_clear_inexflag; } 
else if (strcmp(target, "mpfr_clear_erangeflag") == 0) {
return (void*)&mpfr_clear_erangeflag; } 
else if (strcmp(target, "mpfr_set_underflow") == 0) {
return (void*)&mpfr_set_underflow; } 
else if (strcmp(target, "mpfr_set_overflow") == 0) {
return (void*)&mpfr_set_overflow; } 
else if (strcmp(target, "mpfr_set_divby0") == 0) {
return (void*)&mpfr_set_divby0; } 
else if (strcmp(target, "mpfr_set_nanflag") == 0) {
return (void*)&mpfr_set_nanflag; } 
else if (strcmp(target, "mpfr_set_inexflag") == 0) {
return (void*)&mpfr_set_inexflag; } 
else if (strcmp(target, "mpfr_set_erangeflag") == 0) {
return (void*)&mpfr_set_erangeflag; } 
else if (strcmp(target, "mpfr_underflow_p") == 0) {
return (void*)&mpfr_underflow_p; } 
else if (strcmp(target, "mpfr_overflow_p") == 0) {
return (void*)&mpfr_overflow_p; } 
else if (strcmp(target, "mpfr_divby0_p") == 0) {
return (void*)&mpfr_divby0_p; } 
else if (strcmp(target, "mpfr_nanflag_p") == 0) {
return (void*)&mpfr_nanflag_p; } 
else if (strcmp(target, "mpfr_inexflag_p") == 0) {
return (void*)&mpfr_inexflag_p; } 
else if (strcmp(target, "mpfr_erangeflag_p") == 0) {
return (void*)&mpfr_erangeflag_p; } 
else if (strcmp(target, "mpfr_flags_clear") == 0) {
return (void*)&mpfr_flags_clear; } 
else if (strcmp(target, "mpfr_flags_set") == 0) {
return (void*)&mpfr_flags_set; } 
else if (strcmp(target, "mpfr_flags_test") == 0) {
return (void*)&mpfr_flags_test; } 
else if (strcmp(target, "mpfr_flags_save") == 0) {
return (void*)&mpfr_flags_save; } 
else if (strcmp(target, "mpfr_flags_restore") == 0) {
return (void*)&mpfr_flags_restore; } 
else if (strcmp(target, "mpfr_check_range") == 0) {
return (void*)&mpfr_check_range; } 
else if (strcmp(target, "mpfr_init2") == 0) {
return (void*)&mpfr_init2; } 
else if (strcmp(target, "mpfr_init") == 0) {
return (void*)&mpfr_init; } 
else if (strcmp(target, "mpfr_clear") == 0) {
return (void*)&mpfr_clear; } 
else if (strcmp(target, "mpfr_inits2") == 0) {
return (void*)&mpfr_inits2; } 
else if (strcmp(target, "mpfr_inits") == 0) {
return (void*)&mpfr_inits; } 
else if (strcmp(target, "mpfr_clears") == 0) {
return (void*)&mpfr_clears; } 
else if (strcmp(target, "mpfr_prec_round") == 0) {
return (void*)&mpfr_prec_round; } 
else if (strcmp(target, "mpfr_can_round") == 0) {
return (void*)&mpfr_can_round; } 
else if (strcmp(target, "mpfr_min_prec") == 0) {
return (void*)&mpfr_min_prec; } 
else if (strcmp(target, "mpfr_get_exp") == 0) {
return (void*)&mpfr_get_exp; } 
else if (strcmp(target, "mpfr_set_exp") == 0) {
return (void*)&mpfr_set_exp; } 
else if (strcmp(target, "mpfr_get_prec") == 0) {
return (void*)&mpfr_get_prec; } 
else if (strcmp(target, "mpfr_set_prec") == 0) {
return (void*)&mpfr_set_prec; } 
else if (strcmp(target, "mpfr_set_prec_raw") == 0) {
return (void*)&mpfr_set_prec_raw; } 
else if (strcmp(target, "mpfr_set_default_prec") == 0) {
return (void*)&mpfr_set_default_prec; } 
else if (strcmp(target, "mpfr_get_default_prec") == 0) {
return (void*)&mpfr_get_default_prec; } 
else if (strcmp(target, "mpfr_set_d") == 0) {
return (void*)&mpfr_set_d; } 
else if (strcmp(target, "mpfr_set_flt") == 0) {
return (void*)&mpfr_set_flt; } 
else if (strcmp(target, "mpfr_set_ld") == 0) {
return (void*)&mpfr_set_ld; } 
else if (strcmp(target, "mpfr_set_z") == 0) {
return (void*)&mpfr_set_z; } 
else if (strcmp(target, "mpfr_set_z_2exp") == 0) {
return (void*)&mpfr_set_z_2exp; } 
else if (strcmp(target, "mpfr_set_nan") == 0) {
return (void*)&mpfr_set_nan; } 
else if (strcmp(target, "mpfr_set_inf") == 0) {
return (void*)&mpfr_set_inf; } 
else if (strcmp(target, "mpfr_set_zero") == 0) {
return (void*)&mpfr_set_zero; } 
else if (strcmp(target, "mpfr_set_f") == 0) {
return (void*)&mpfr_set_f; } 
else if (strcmp(target, "mpfr_cmp_f") == 0) {
return (void*)&mpfr_cmp_f; } 
else if (strcmp(target, "mpfr_get_f") == 0) {
return (void*)&mpfr_get_f; } 
else if (strcmp(target, "mpfr_set_si") == 0) {
return (void*)&mpfr_set_si; } 
else if (strcmp(target, "mpfr_set_ui") == 0) {
return (void*)&mpfr_set_ui; } 
else if (strcmp(target, "mpfr_set_si_2exp") == 0) {
return (void*)&mpfr_set_si_2exp; } 
else if (strcmp(target, "mpfr_set_ui_2exp") == 0) {
return (void*)&mpfr_set_ui_2exp; } 
else if (strcmp(target, "mpfr_set_q") == 0) {
return (void*)&mpfr_set_q; } 
else if (strcmp(target, "mpfr_mul_q") == 0) {
return (void*)&mpfr_mul_q; } 
else if (strcmp(target, "mpfr_div_q") == 0) {
return (void*)&mpfr_div_q; } 
else if (strcmp(target, "mpfr_add_q") == 0) {
return (void*)&mpfr_add_q; } 
else if (strcmp(target, "mpfr_sub_q") == 0) {
return (void*)&mpfr_sub_q; } 
else if (strcmp(target, "mpfr_cmp_q") == 0) {
return (void*)&mpfr_cmp_q; } 
else if (strcmp(target, "mpfr_get_q") == 0) {
return (void*)&mpfr_get_q; } 
else if (strcmp(target, "mpfr_set_str") == 0) {
return (void*)&mpfr_set_str; } 
else if (strcmp(target, "mpfr_init_set_str") == 0) {
return (void*)&mpfr_init_set_str; } 
else if (strcmp(target, "mpfr_set4") == 0) {
return (void*)&mpfr_set4; } 
else if (strcmp(target, "mpfr_abs") == 0) {
return (void*)&mpfr_abs; } 
else if (strcmp(target, "mpfr_set") == 0) {
return (void*)&mpfr_set; } 
else if (strcmp(target, "mpfr_neg") == 0) {
return (void*)&mpfr_neg; } 
else if (strcmp(target, "mpfr_signbit") == 0) {
return (void*)&mpfr_signbit; } 
else if (strcmp(target, "mpfr_setsign") == 0) {
return (void*)&mpfr_setsign; } 
else if (strcmp(target, "mpfr_copysign") == 0) {
return (void*)&mpfr_copysign; } 
else if (strcmp(target, "mpfr_get_z_2exp") == 0) {
return (void*)&mpfr_get_z_2exp; } 
else if (strcmp(target, "mpfr_get_flt") == 0) {
return (void*)&mpfr_get_flt; } 
else if (strcmp(target, "mpfr_get_d") == 0) {
return (void*)&mpfr_get_d; } 
else if (strcmp(target, "mpfr_get_ld") == 0) {
return (void*)&mpfr_get_ld; } 
else if (strcmp(target, "mpfr_get_d1") == 0) {
return (void*)&mpfr_get_d1; } 
else if (strcmp(target, "mpfr_get_d_2exp") == 0) {
return (void*)&mpfr_get_d_2exp; } 
else if (strcmp(target, "mpfr_get_ld_2exp") == 0) {
return (void*)&mpfr_get_ld_2exp; } 
else if (strcmp(target, "mpfr_frexp") == 0) {
return (void*)&mpfr_frexp; } 
else if (strcmp(target, "mpfr_get_si") == 0) {
return (void*)&mpfr_get_si; } 
else if (strcmp(target, "mpfr_get_ui") == 0) {
return (void*)&mpfr_get_ui; } 
else if (strcmp(target, "mpfr_get_str") == 0) {
return (void*)&mpfr_get_str; } 
else if (strcmp(target, "mpfr_get_z") == 0) {
return (void*)&mpfr_get_z; } 
else if (strcmp(target, "mpfr_free_str") == 0) {
return (void*)&mpfr_free_str; } 
else if (strcmp(target, "mpfr_urandom") == 0) {
return (void*)&mpfr_urandom; } 
else if (strcmp(target, "mpfr_grandom") == 0) {
return (void*)&mpfr_grandom; } 
else if (strcmp(target, "mpfr_nrandom") == 0) {
return (void*)&mpfr_nrandom; } 
else if (strcmp(target, "mpfr_erandom") == 0) {
return (void*)&mpfr_erandom; } 
else if (strcmp(target, "mpfr_urandomb") == 0) {
return (void*)&mpfr_urandomb; } 
else if (strcmp(target, "mpfr_nextabove") == 0) {
return (void*)&mpfr_nextabove; } 
else if (strcmp(target, "mpfr_nextbelow") == 0) {
return (void*)&mpfr_nextbelow; } 
else if (strcmp(target, "mpfr_nexttoward") == 0) {
return (void*)&mpfr_nexttoward; } 
else if (strcmp(target, "mpfr_printf") == 0) {
return (void*)&mpfr_printf; } 
else if (strcmp(target, "mpfr_asprintf") == 0) {
return (void*)&mpfr_asprintf; } 
else if (strcmp(target, "mpfr_sprintf") == 0) {
return (void*)&mpfr_sprintf; } 
else if (strcmp(target, "mpfr_snprintf") == 0) {
return (void*)&mpfr_snprintf; } 
else if (strcmp(target, "mpfr_pow") == 0) {
return (void*)&mpfr_pow; } 
else if (strcmp(target, "mpfr_pow_si") == 0) {
return (void*)&mpfr_pow_si; } 
else if (strcmp(target, "mpfr_pow_ui") == 0) {
return (void*)&mpfr_pow_ui; } 
else if (strcmp(target, "mpfr_ui_pow_ui") == 0) {
return (void*)&mpfr_ui_pow_ui; } 
else if (strcmp(target, "mpfr_ui_pow") == 0) {
return (void*)&mpfr_ui_pow; } 
else if (strcmp(target, "mpfr_pow_z") == 0) {
return (void*)&mpfr_pow_z; } 
else if (strcmp(target, "mpfr_sqrt") == 0) {
return (void*)&mpfr_sqrt; } 
else if (strcmp(target, "mpfr_sqrt_ui") == 0) {
return (void*)&mpfr_sqrt_ui; } 
else if (strcmp(target, "mpfr_rec_sqrt") == 0) {
return (void*)&mpfr_rec_sqrt; } 
else if (strcmp(target, "mpfr_add") == 0) {
return (void*)&mpfr_add; } 
else if (strcmp(target, "mpfr_sub") == 0) {
return (void*)&mpfr_sub; } 
else if (strcmp(target, "mpfr_mul") == 0) {
return (void*)&mpfr_mul; } 
else if (strcmp(target, "mpfr_div") == 0) {
return (void*)&mpfr_div; } 
else if (strcmp(target, "mpfr_add_ui") == 0) {
return (void*)&mpfr_add_ui; } 
else if (strcmp(target, "mpfr_sub_ui") == 0) {
return (void*)&mpfr_sub_ui; } 
else if (strcmp(target, "mpfr_ui_sub") == 0) {
return (void*)&mpfr_ui_sub; } 
else if (strcmp(target, "mpfr_mul_ui") == 0) {
return (void*)&mpfr_mul_ui; } 
else if (strcmp(target, "mpfr_div_ui") == 0) {
return (void*)&mpfr_div_ui; } 
else if (strcmp(target, "mpfr_ui_div") == 0) {
return (void*)&mpfr_ui_div; } 
else if (strcmp(target, "mpfr_add_si") == 0) {
return (void*)&mpfr_add_si; } 
else if (strcmp(target, "mpfr_sub_si") == 0) {
return (void*)&mpfr_sub_si; } 
else if (strcmp(target, "mpfr_si_sub") == 0) {
return (void*)&mpfr_si_sub; } 
else if (strcmp(target, "mpfr_mul_si") == 0) {
return (void*)&mpfr_mul_si; } 
else if (strcmp(target, "mpfr_div_si") == 0) {
return (void*)&mpfr_div_si; } 
else if (strcmp(target, "mpfr_si_div") == 0) {
return (void*)&mpfr_si_div; } 
else if (strcmp(target, "mpfr_add_d") == 0) {
return (void*)&mpfr_add_d; } 
else if (strcmp(target, "mpfr_sub_d") == 0) {
return (void*)&mpfr_sub_d; } 
else if (strcmp(target, "mpfr_d_sub") == 0) {
return (void*)&mpfr_d_sub; } 
else if (strcmp(target, "mpfr_mul_d") == 0) {
return (void*)&mpfr_mul_d; } 
else if (strcmp(target, "mpfr_div_d") == 0) {
return (void*)&mpfr_div_d; } 
else if (strcmp(target, "mpfr_d_div") == 0) {
return (void*)&mpfr_d_div; } 
else if (strcmp(target, "mpfr_sqr") == 0) {
return (void*)&mpfr_sqr; } 
else if (strcmp(target, "mpfr_const_pi") == 0) {
return (void*)&mpfr_const_pi; } 
else if (strcmp(target, "mpfr_const_log2") == 0) {
return (void*)&mpfr_const_log2; } 
else if (strcmp(target, "mpfr_const_euler") == 0) {
return (void*)&mpfr_const_euler; } 
else if (strcmp(target, "mpfr_const_catalan") == 0) {
return (void*)&mpfr_const_catalan; } 
else if (strcmp(target, "mpfr_agm") == 0) {
return (void*)&mpfr_agm; } 
else if (strcmp(target, "mpfr_log") == 0) {
return (void*)&mpfr_log; } 
else if (strcmp(target, "mpfr_log2") == 0) {
return (void*)&mpfr_log2; } 
else if (strcmp(target, "mpfr_log10") == 0) {
return (void*)&mpfr_log10; } 
else if (strcmp(target, "mpfr_log1p") == 0) {
return (void*)&mpfr_log1p; } 
else if (strcmp(target, "mpfr_log_ui") == 0) {
return (void*)&mpfr_log_ui; } 
else if (strcmp(target, "mpfr_exp") == 0) {
return (void*)&mpfr_exp; } 
else if (strcmp(target, "mpfr_exp2") == 0) {
return (void*)&mpfr_exp2; } 
else if (strcmp(target, "mpfr_exp10") == 0) {
return (void*)&mpfr_exp10; } 
else if (strcmp(target, "mpfr_expm1") == 0) {
return (void*)&mpfr_expm1; } 
else if (strcmp(target, "mpfr_eint") == 0) {
return (void*)&mpfr_eint; } 
else if (strcmp(target, "mpfr_li2") == 0) {
return (void*)&mpfr_li2; } 
else if (strcmp(target, "mpfr_cmp") == 0) {
return (void*)&mpfr_cmp; } 
else if (strcmp(target, "mpfr_cmp3") == 0) {
return (void*)&mpfr_cmp3; } 
else if (strcmp(target, "mpfr_cmp_d") == 0) {
return (void*)&mpfr_cmp_d; } 
else if (strcmp(target, "mpfr_cmp_ld") == 0) {
return (void*)&mpfr_cmp_ld; } 
else if (strcmp(target, "mpfr_cmpabs") == 0) {
return (void*)&mpfr_cmpabs; } 
else if (strcmp(target, "mpfr_cmp_ui") == 0) {
return (void*)&mpfr_cmp_ui; } 
else if (strcmp(target, "mpfr_cmp_si") == 0) {
return (void*)&mpfr_cmp_si; } 
else if (strcmp(target, "mpfr_cmp_ui_2exp") == 0) {
return (void*)&mpfr_cmp_ui_2exp; } 
else if (strcmp(target, "mpfr_cmp_si_2exp") == 0) {
return (void*)&mpfr_cmp_si_2exp; } 
else if (strcmp(target, "mpfr_reldiff") == 0) {
return (void*)&mpfr_reldiff; } 
else if (strcmp(target, "mpfr_eq") == 0) {
return (void*)&mpfr_eq; } 
else if (strcmp(target, "mpfr_sgn") == 0) {
return (void*)&mpfr_sgn; } 
else if (strcmp(target, "mpfr_mul_2exp") == 0) {
return (void*)&mpfr_mul_2exp; } 
else if (strcmp(target, "mpfr_div_2exp") == 0) {
return (void*)&mpfr_div_2exp; } 
else if (strcmp(target, "mpfr_mul_2ui") == 0) {
return (void*)&mpfr_mul_2ui; } 
else if (strcmp(target, "mpfr_div_2ui") == 0) {
return (void*)&mpfr_div_2ui; } 
else if (strcmp(target, "mpfr_mul_2si") == 0) {
return (void*)&mpfr_mul_2si; } 
else if (strcmp(target, "mpfr_div_2si") == 0) {
return (void*)&mpfr_div_2si; } 
else if (strcmp(target, "mpfr_rint") == 0) {
return (void*)&mpfr_rint; } 
else if (strcmp(target, "mpfr_roundeven") == 0) {
return (void*)&mpfr_roundeven; } 
else if (strcmp(target, "mpfr_round") == 0) {
return (void*)&mpfr_round; } 
else if (strcmp(target, "mpfr_trunc") == 0) {
return (void*)&mpfr_trunc; } 
else if (strcmp(target, "mpfr_ceil") == 0) {
return (void*)&mpfr_ceil; } 
else if (strcmp(target, "mpfr_floor") == 0) {
return (void*)&mpfr_floor; } 
else if (strcmp(target, "mpfr_rint_roundeven") == 0) {
return (void*)&mpfr_rint_roundeven; } 
else if (strcmp(target, "mpfr_rint_round") == 0) {
return (void*)&mpfr_rint_round; } 
else if (strcmp(target, "mpfr_rint_trunc") == 0) {
return (void*)&mpfr_rint_trunc; } 
else if (strcmp(target, "mpfr_rint_ceil") == 0) {
return (void*)&mpfr_rint_ceil; } 
else if (strcmp(target, "mpfr_rint_floor") == 0) {
return (void*)&mpfr_rint_floor; } 
else if (strcmp(target, "mpfr_frac") == 0) {
return (void*)&mpfr_frac; } 
else if (strcmp(target, "mpfr_modf") == 0) {
return (void*)&mpfr_modf; } 
else if (strcmp(target, "mpfr_remquo") == 0) {
return (void*)&mpfr_remquo; } 
else if (strcmp(target, "mpfr_remainder") == 0) {
return (void*)&mpfr_remainder; } 
else if (strcmp(target, "mpfr_fmod") == 0) {
return (void*)&mpfr_fmod; } 
else if (strcmp(target, "mpfr_fmodquo") == 0) {
return (void*)&mpfr_fmodquo; } 
else if (strcmp(target, "mpfr_fits_ulong_p") == 0) {
return (void*)&mpfr_fits_ulong_p; } 
else if (strcmp(target, "mpfr_fits_slong_p") == 0) {
return (void*)&mpfr_fits_slong_p; } 
else if (strcmp(target, "mpfr_fits_uint_p") == 0) {
return (void*)&mpfr_fits_uint_p; } 
else if (strcmp(target, "mpfr_fits_sint_p") == 0) {
return (void*)&mpfr_fits_sint_p; } 
else if (strcmp(target, "mpfr_fits_ushort_p") == 0) {
return (void*)&mpfr_fits_ushort_p; } 
else if (strcmp(target, "mpfr_fits_sshort_p") == 0) {
return (void*)&mpfr_fits_sshort_p; } 
else if (strcmp(target, "mpfr_fits_uintmax_p") == 0) {
return (void*)&mpfr_fits_uintmax_p; } 
else if (strcmp(target, "mpfr_fits_intmax_p") == 0) {
return (void*)&mpfr_fits_intmax_p; } 
else if (strcmp(target, "mpfr_extract") == 0) {
return (void*)&mpfr_extract; } 
else if (strcmp(target, "mpfr_swap") == 0) {
return (void*)&mpfr_swap; } 
else if (strcmp(target, "mpfr_dump") == 0) {
return (void*)&mpfr_dump; } 
else if (strcmp(target, "mpfr_nan_p") == 0) {
return (void*)&mpfr_nan_p; } 
else if (strcmp(target, "mpfr_inf_p") == 0) {
return (void*)&mpfr_inf_p; } 
else if (strcmp(target, "mpfr_number_p") == 0) {
return (void*)&mpfr_number_p; } 
else if (strcmp(target, "mpfr_integer_p") == 0) {
return (void*)&mpfr_integer_p; } 
else if (strcmp(target, "mpfr_zero_p") == 0) {
return (void*)&mpfr_zero_p; } 
else if (strcmp(target, "mpfr_regular_p") == 0) {
return (void*)&mpfr_regular_p; } 
else if (strcmp(target, "mpfr_greater_p") == 0) {
return (void*)&mpfr_greater_p; } 
else if (strcmp(target, "mpfr_greaterequal_p") == 0) {
return (void*)&mpfr_greaterequal_p; } 
else if (strcmp(target, "mpfr_less_p") == 0) {
return (void*)&mpfr_less_p; } 
else if (strcmp(target, "mpfr_lessequal_p") == 0) {
return (void*)&mpfr_lessequal_p; } 
else if (strcmp(target, "mpfr_lessgreater_p") == 0) {
return (void*)&mpfr_lessgreater_p; } 
else if (strcmp(target, "mpfr_equal_p") == 0) {
return (void*)&mpfr_equal_p; } 
else if (strcmp(target, "mpfr_unordered_p") == 0) {
return (void*)&mpfr_unordered_p; } 
else if (strcmp(target, "mpfr_atanh") == 0) {
return (void*)&mpfr_atanh; } 
else if (strcmp(target, "mpfr_acosh") == 0) {
return (void*)&mpfr_acosh; } 
else if (strcmp(target, "mpfr_asinh") == 0) {
return (void*)&mpfr_asinh; } 
else if (strcmp(target, "mpfr_cosh") == 0) {
return (void*)&mpfr_cosh; } 
else if (strcmp(target, "mpfr_sinh") == 0) {
return (void*)&mpfr_sinh; } 
else if (strcmp(target, "mpfr_tanh") == 0) {
return (void*)&mpfr_tanh; } 
else if (strcmp(target, "mpfr_sinh_cosh") == 0) {
return (void*)&mpfr_sinh_cosh; } 
else if (strcmp(target, "mpfr_sech") == 0) {
return (void*)&mpfr_sech; } 
else if (strcmp(target, "mpfr_csch") == 0) {
return (void*)&mpfr_csch; } 
else if (strcmp(target, "mpfr_coth") == 0) {
return (void*)&mpfr_coth; } 
else if (strcmp(target, "mpfr_acos") == 0) {
return (void*)&mpfr_acos; } 
else if (strcmp(target, "mpfr_asin") == 0) {
return (void*)&mpfr_asin; } 
else if (strcmp(target, "mpfr_atan") == 0) {
return (void*)&mpfr_atan; } 
else if (strcmp(target, "mpfr_sin") == 0) {
return (void*)&mpfr_sin; } 
else if (strcmp(target, "mpfr_sin_cos") == 0) {
return (void*)&mpfr_sin_cos; } 
else if (strcmp(target, "mpfr_cos") == 0) {
return (void*)&mpfr_cos; } 
else if (strcmp(target, "mpfr_tan") == 0) {
return (void*)&mpfr_tan; } 
else if (strcmp(target, "mpfr_atan2") == 0) {
return (void*)&mpfr_atan2; } 
else if (strcmp(target, "mpfr_sec") == 0) {
return (void*)&mpfr_sec; } 
else if (strcmp(target, "mpfr_csc") == 0) {
return (void*)&mpfr_csc; } 
else if (strcmp(target, "mpfr_cot") == 0) {
return (void*)&mpfr_cot; } 
else if (strcmp(target, "mpfr_hypot") == 0) {
return (void*)&mpfr_hypot; } 
else if (strcmp(target, "mpfr_erf") == 0) {
return (void*)&mpfr_erf; } 
else if (strcmp(target, "mpfr_erfc") == 0) {
return (void*)&mpfr_erfc; } 
else if (strcmp(target, "mpfr_cbrt") == 0) {
return (void*)&mpfr_cbrt; } 
else if (strcmp(target, "mpfr_root") == 0) {
return (void*)&mpfr_root; } 
else if (strcmp(target, "mpfr_rootn_ui") == 0) {
return (void*)&mpfr_rootn_ui; } 
else if (strcmp(target, "mpfr_gamma") == 0) {
return (void*)&mpfr_gamma; } 
else if (strcmp(target, "mpfr_gamma_inc") == 0) {
return (void*)&mpfr_gamma_inc; } 
else if (strcmp(target, "mpfr_beta") == 0) {
return (void*)&mpfr_beta; } 
else if (strcmp(target, "mpfr_lngamma") == 0) {
return (void*)&mpfr_lngamma; } 
else if (strcmp(target, "mpfr_lgamma") == 0) {
return (void*)&mpfr_lgamma; } 
else if (strcmp(target, "mpfr_digamma") == 0) {
return (void*)&mpfr_digamma; } 
else if (strcmp(target, "mpfr_zeta") == 0) {
return (void*)&mpfr_zeta; } 
else if (strcmp(target, "mpfr_zeta_ui") == 0) {
return (void*)&mpfr_zeta_ui; } 
else if (strcmp(target, "mpfr_fac_ui") == 0) {
return (void*)&mpfr_fac_ui; } 
else if (strcmp(target, "mpfr_j0") == 0) {
return (void*)&mpfr_j0; } 
else if (strcmp(target, "mpfr_j1") == 0) {
return (void*)&mpfr_j1; } 
else if (strcmp(target, "mpfr_jn") == 0) {
return (void*)&mpfr_jn; } 
else if (strcmp(target, "mpfr_y0") == 0) {
return (void*)&mpfr_y0; } 
else if (strcmp(target, "mpfr_y1") == 0) {
return (void*)&mpfr_y1; } 
else if (strcmp(target, "mpfr_yn") == 0) {
return (void*)&mpfr_yn; } 
else if (strcmp(target, "mpfr_ai") == 0) {
return (void*)&mpfr_ai; } 
else if (strcmp(target, "mpfr_min") == 0) {
return (void*)&mpfr_min; } 
else if (strcmp(target, "mpfr_max") == 0) {
return (void*)&mpfr_max; } 
else if (strcmp(target, "mpfr_dim") == 0) {
return (void*)&mpfr_dim; } 
else if (strcmp(target, "mpfr_mul_z") == 0) {
return (void*)&mpfr_mul_z; } 
else if (strcmp(target, "mpfr_div_z") == 0) {
return (void*)&mpfr_div_z; } 
else if (strcmp(target, "mpfr_add_z") == 0) {
return (void*)&mpfr_add_z; } 
else if (strcmp(target, "mpfr_sub_z") == 0) {
return (void*)&mpfr_sub_z; } 
else if (strcmp(target, "mpfr_z_sub") == 0) {
return (void*)&mpfr_z_sub; } 
else if (strcmp(target, "mpfr_cmp_z") == 0) {
return (void*)&mpfr_cmp_z; } 
else if (strcmp(target, "mpfr_fma") == 0) {
return (void*)&mpfr_fma; } 
else if (strcmp(target, "mpfr_fms") == 0) {
return (void*)&mpfr_fms; } 
else if (strcmp(target, "mpfr_fmma") == 0) {
return (void*)&mpfr_fmma; } 
else if (strcmp(target, "mpfr_fmms") == 0) {
return (void*)&mpfr_fmms; } 
else if (strcmp(target, "mpfr_sum") == 0) {
return (void*)&mpfr_sum; } 
else if (strcmp(target, "mpfr_free_cache") == 0) {
return (void*)&mpfr_free_cache; } 
else if (strcmp(target, "mpfr_free_cache2") == 0) {
return (void*)&mpfr_free_cache2; } 
else if (strcmp(target, "mpfr_free_pool") == 0) {
return (void*)&mpfr_free_pool; } 
else if (strcmp(target, "mpfr_mp_memory_cleanup") == 0) {
return (void*)&mpfr_mp_memory_cleanup; } 
else if (strcmp(target, "mpfr_subnormalize") == 0) {
return (void*)&mpfr_subnormalize; } 
else if (strcmp(target, "mpfr_strtofr") == 0) {
return (void*)&mpfr_strtofr; } 
else if (strcmp(target, "mpfr_round_nearest_away_begin") == 0) {
return (void*)&mpfr_round_nearest_away_begin; } 
else if (strcmp(target, "mpfr_round_nearest_away_end") == 0) {
return (void*)&mpfr_round_nearest_away_end; } 
else if (strcmp(target, "mpfr_custom_get_size") == 0) {
return (void*)&mpfr_custom_get_size; } 
else if (strcmp(target, "mpfr_custom_init") == 0) {
return (void*)&mpfr_custom_init; } 
else if (strcmp(target, "mpfr_custom_get_significand") == 0) {
return (void*)&mpfr_custom_get_significand; } 
else if (strcmp(target, "mpfr_custom_get_exp") == 0) {
return (void*)&mpfr_custom_get_exp; } 
else if (strcmp(target, "mpfr_custom_move") == 0) {
return (void*)&mpfr_custom_move; } 
else if (strcmp(target, "mpfr_custom_init_set") == 0) {
return (void*)&mpfr_custom_init_set; } 
else if (strcmp(target, "mpfr_custom_get_kind") == 0) {
return (void*)&mpfr_custom_get_kind; } 
else if (strcmp(target, "dsfmt_gen_rand_all") == 0) {
return (void*)&dsfmt_gen_rand_all; } 
else if (strcmp(target, "dsfmt_fill_array_open_close") == 0) {
return (void*)&dsfmt_fill_array_open_close; } 
else if (strcmp(target, "dsfmt_fill_array_close_open") == 0) {
return (void*)&dsfmt_fill_array_close_open; } 
else if (strcmp(target, "dsfmt_fill_array_open_open") == 0) {
return (void*)&dsfmt_fill_array_open_open; } 
else if (strcmp(target, "dsfmt_fill_array_close1_open2") == 0) {
return (void*)&dsfmt_fill_array_close1_open2; } 
else if (strcmp(target, "dsfmt_chk_init_gen_rand") == 0) {
return (void*)&dsfmt_chk_init_gen_rand; } 
else if (strcmp(target, "dsfmt_chk_init_by_array") == 0) {
return (void*)&dsfmt_chk_init_by_array; } 
else if (strcmp(target, "dsfmt_get_idstring") == 0) {
return (void*)&dsfmt_get_idstring; } 
else if (strcmp(target, "dsfmt_get_min_array_size") == 0) {
return (void*)&dsfmt_get_min_array_size; } 
else if (strcmp(target, "dsfmt_genrand_uint32") == 0) {
return (void*)&dsfmt_genrand_uint32; } 
else if (strcmp(target, "dsfmt_genrand_close1_open2") == 0) {
return (void*)&dsfmt_genrand_close1_open2; } 
else if (strcmp(target, "dsfmt_genrand_close_open") == 0) {
return (void*)&dsfmt_genrand_close_open; } 
else if (strcmp(target, "dsfmt_genrand_open_close") == 0) {
return (void*)&dsfmt_genrand_open_close; } 
else if (strcmp(target, "dsfmt_genrand_open_open") == 0) {
return (void*)&dsfmt_genrand_open_open; } 
else if (strcmp(target, "dsfmt_gv_genrand_uint32") == 0) {
return (void*)&dsfmt_gv_genrand_uint32; } 
else if (strcmp(target, "dsfmt_gv_genrand_close1_open2") == 0) {
return (void*)&dsfmt_gv_genrand_close1_open2; } 
else if (strcmp(target, "dsfmt_gv_genrand_close_open") == 0) {
return (void*)&dsfmt_gv_genrand_close_open; } 
else if (strcmp(target, "dsfmt_gv_genrand_open_close") == 0) {
return (void*)&dsfmt_gv_genrand_open_close; } 
else if (strcmp(target, "dsfmt_gv_genrand_open_open") == 0) {
return (void*)&dsfmt_gv_genrand_open_open; } 
else if (strcmp(target, "dsfmt_gv_fill_array_open_close") == 0) {
return (void*)&dsfmt_gv_fill_array_open_close; } 
else if (strcmp(target, "dsfmt_gv_fill_array_close_open") == 0) {
return (void*)&dsfmt_gv_fill_array_close_open; } 
else if (strcmp(target, "dsfmt_gv_fill_array_open_open") == 0) {
return (void*)&dsfmt_gv_fill_array_open_open; } 
else if (strcmp(target, "dsfmt_gv_fill_array_close1_open2") == 0) {
return (void*)&dsfmt_gv_fill_array_close1_open2; } 
else if (strcmp(target, "dsfmt_gv_init_gen_rand") == 0) {
return (void*)&dsfmt_gv_init_gen_rand; } 
else if (strcmp(target, "dsfmt_gv_init_by_array") == 0) {
return (void*)&dsfmt_gv_init_by_array; } 
else if (strcmp(target, "dsfmt_init_gen_rand") == 0) {
return (void*)&dsfmt_init_gen_rand; } 
else if (strcmp(target, "dsfmt_init_by_array") == 0) {
return (void*)&dsfmt_init_by_array; } 
else if (strcmp(target, "get_idstring") == 0) {
return (void*)&get_idstring; } 
else if (strcmp(target, "get_min_array_size") == 0) {
return (void*)&get_min_array_size; } 
else if (strcmp(target, "init_gen_rand") == 0) {
return (void*)&init_gen_rand; } 
else if (strcmp(target, "init_by_array") == 0) {
return (void*)&init_by_array; } 
else if (strcmp(target, "genrand_close1_open2") == 0) {
return (void*)&genrand_close1_open2; } 
else if (strcmp(target, "genrand_close_open") == 0) {
return (void*)&genrand_close_open; } 
else if (strcmp(target, "genrand_open_close") == 0) {
return (void*)&genrand_open_close; } 
else if (strcmp(target, "genrand_open_open") == 0) {
return (void*)&genrand_open_open; } 
else if (strcmp(target, "fill_array_open_close") == 0) {
return (void*)&fill_array_open_close; } 
else if (strcmp(target, "fill_array_close_open") == 0) {
return (void*)&fill_array_close_open; } 
else if (strcmp(target, "fill_array_open_open") == 0) {
return (void*)&fill_array_open_open; } 
else if (strcmp(target, "fill_array_close1_open2") == 0) {
return (void*)&fill_array_close1_open2; } 
else if (strcmp(target, "emscripten_run_script") == 0) {
return (void*)&emscripten_run_script; } 
else if (strcmp(target, "emscripten_run_script_int") == 0) {
return (void*)&emscripten_run_script_int; } 
else if (strcmp(target, "emscripten_run_script_string") == 0) {
return (void*)&emscripten_run_script_string; } 
else if (strcmp(target, "emscripten_async_run_script") == 0) {
return (void*)&emscripten_async_run_script; } 
else if (strcmp(target, "emscripten_async_load_script") == 0) {
return (void*)&emscripten_async_load_script; } 
else if (strcmp(target, "emscripten_set_main_loop") == 0) {
return (void*)&emscripten_set_main_loop; } 
else if (strcmp(target, "emscripten_set_main_loop_timing") == 0) {
return (void*)&emscripten_set_main_loop_timing; } 
else if (strcmp(target, "emscripten_get_main_loop_timing") == 0) {
return (void*)&emscripten_get_main_loop_timing; } 
else if (strcmp(target, "emscripten_set_main_loop_arg") == 0) {
return (void*)&emscripten_set_main_loop_arg; } 
else if (strcmp(target, "emscripten_pause_main_loop") == 0) {
return (void*)&emscripten_pause_main_loop; } 
else if (strcmp(target, "emscripten_resume_main_loop") == 0) {
return (void*)&emscripten_resume_main_loop; } 
else if (strcmp(target, "emscripten_cancel_main_loop") == 0) {
return (void*)&emscripten_cancel_main_loop; } 
else if (strcmp(target, "emscripten_set_socket_error_callback") == 0) {
return (void*)&emscripten_set_socket_error_callback; } 
else if (strcmp(target, "emscripten_set_socket_open_callback") == 0) {
return (void*)&emscripten_set_socket_open_callback; } 
else if (strcmp(target, "emscripten_set_socket_listen_callback") == 0) {
return (void*)&emscripten_set_socket_listen_callback; } 
else if (strcmp(target, "emscripten_set_socket_connection_callback") == 0) {
return (void*)&emscripten_set_socket_connection_callback; } 
else if (strcmp(target, "emscripten_set_socket_message_callback") == 0) {
return (void*)&emscripten_set_socket_message_callback; } 
else if (strcmp(target, "emscripten_set_socket_close_callback") == 0) {
return (void*)&emscripten_set_socket_close_callback; } 
else if (strcmp(target, "_emscripten_push_main_loop_blocker") == 0) {
return (void*)&_emscripten_push_main_loop_blocker; } 
else if (strcmp(target, "_emscripten_push_uncounted_main_loop_blocker") == 0) {
return (void*)&_emscripten_push_uncounted_main_loop_blocker; } 
else if (strcmp(target, "emscripten_set_main_loop_expected_blockers") == 0) {
return (void*)&emscripten_set_main_loop_expected_blockers; } 
else if (strcmp(target, "emscripten_async_call") == 0) {
return (void*)&emscripten_async_call; } 
else if (strcmp(target, "emscripten_exit_with_live_runtime") == 0) {
return (void*)&emscripten_exit_with_live_runtime; } 
else if (strcmp(target, "emscripten_force_exit") == 0) {
return (void*)&emscripten_force_exit; } 
else if (strcmp(target, "emscripten_get_device_pixel_ratio") == 0) {
return (void*)&emscripten_get_device_pixel_ratio; } 
else if (strcmp(target, "emscripten_hide_mouse") == 0) {
return (void*)&emscripten_hide_mouse; } 
else if (strcmp(target, "emscripten_set_canvas_size") == 0) {
return (void*)&emscripten_set_canvas_size; } 
else if (strcmp(target, "emscripten_get_canvas_size") == 0) {
return (void*)&emscripten_get_canvas_size; } 
else if (strcmp(target, "emscripten_get_now") == 0) {
return (void*)&emscripten_get_now; } 
else if (strcmp(target, "emscripten_random") == 0) {
return (void*)&emscripten_random; } 
else if (strcmp(target, "emscripten_async_wget") == 0) {
return (void*)&emscripten_async_wget; } 
else if (strcmp(target, "emscripten_async_wget_data") == 0) {
return (void*)&emscripten_async_wget_data; } 
else if (strcmp(target, "emscripten_async_wget2") == 0) {
return (void*)&emscripten_async_wget2; } 
else if (strcmp(target, "emscripten_async_wget2_data") == 0) {
return (void*)&emscripten_async_wget2_data; } 
else if (strcmp(target, "emscripten_async_wget2_abort") == 0) {
return (void*)&emscripten_async_wget2_abort; } 
else if (strcmp(target, "emscripten_wget") == 0) {
return (void*)&emscripten_wget; } 
else if (strcmp(target, "emscripten_wget_data") == 0) {
return (void*)&emscripten_wget_data; } 
else if (strcmp(target, "emscripten_idb_async_load") == 0) {
return (void*)&emscripten_idb_async_load; } 
else if (strcmp(target, "emscripten_idb_async_store") == 0) {
return (void*)&emscripten_idb_async_store; } 
else if (strcmp(target, "emscripten_idb_async_delete") == 0) {
return (void*)&emscripten_idb_async_delete; } 
else if (strcmp(target, "emscripten_idb_async_exists") == 0) {
return (void*)&emscripten_idb_async_exists; } 
else if (strcmp(target, "emscripten_idb_load") == 0) {
return (void*)&emscripten_idb_load; } 
else if (strcmp(target, "emscripten_idb_store") == 0) {
return (void*)&emscripten_idb_store; } 
else if (strcmp(target, "emscripten_idb_delete") == 0) {
return (void*)&emscripten_idb_delete; } 
else if (strcmp(target, "emscripten_idb_exists") == 0) {
return (void*)&emscripten_idb_exists; } 
else if (strcmp(target, "emscripten_idb_load_blob") == 0) {
return (void*)&emscripten_idb_load_blob; } 
else if (strcmp(target, "emscripten_idb_store_blob") == 0) {
return (void*)&emscripten_idb_store_blob; } 
else if (strcmp(target, "emscripten_idb_read_from_blob") == 0) {
return (void*)&emscripten_idb_read_from_blob; } 
else if (strcmp(target, "emscripten_idb_free_blob") == 0) {
return (void*)&emscripten_idb_free_blob; } 
else if (strcmp(target, "emscripten_run_preload_plugins") == 0) {
return (void*)&emscripten_run_preload_plugins; } 
else if (strcmp(target, "emscripten_run_preload_plugins_data") == 0) {
return (void*)&emscripten_run_preload_plugins_data; } 
else if (strcmp(target, "emscripten_lazy_load_code") == 0) {
return (void*)&emscripten_lazy_load_code; } 
else if (strcmp(target, "emscripten_create_worker") == 0) {
return (void*)&emscripten_create_worker; } 
else if (strcmp(target, "emscripten_destroy_worker") == 0) {
return (void*)&emscripten_destroy_worker; } 
else if (strcmp(target, "emscripten_call_worker") == 0) {
return (void*)&emscripten_call_worker; } 
else if (strcmp(target, "emscripten_worker_respond") == 0) {
return (void*)&emscripten_worker_respond; } 
else if (strcmp(target, "emscripten_worker_respond_provisionally") == 0) {
return (void*)&emscripten_worker_respond_provisionally; } 
else if (strcmp(target, "emscripten_get_worker_queue_size") == 0) {
return (void*)&emscripten_get_worker_queue_size; } 
else if (strcmp(target, "emscripten_get_compiler_setting") == 0) {
return (void*)&emscripten_get_compiler_setting; } 
else if (strcmp(target, "emscripten_debugger") == 0) {
return (void*)&emscripten_debugger; } 
else if (strcmp(target, "emscripten_get_preloaded_image_data") == 0) {
return (void*)&emscripten_get_preloaded_image_data; } 
else if (strcmp(target, "emscripten_get_preloaded_image_data_from_FILE") == 0) {
return (void*)&emscripten_get_preloaded_image_data_from_FILE; } 
else if (strcmp(target, "emscripten_log") == 0) {
return (void*)&emscripten_log; } 
else if (strcmp(target, "emscripten_get_callstack") == 0) {
return (void*)&emscripten_get_callstack; } 
else if (strcmp(target, "emscripten_print_double") == 0) {
return (void*)&emscripten_print_double; } 
else if (strcmp(target, "emscripten_scan_registers") == 0) {
return (void*)&emscripten_scan_registers; } 
else if (strcmp(target, "emscripten_scan_stack") == 0) {
return (void*)&emscripten_scan_stack; } 
else if (strcmp(target, "emscripten_sleep") == 0) {
return (void*)&emscripten_sleep; } 
else if (strcmp(target, "emscripten_sleep_with_yield") == 0) {
return (void*)&emscripten_sleep_with_yield; } 
else if (strcmp(target, "emscripten_coroutine_create") == 0) {
return (void*)&emscripten_coroutine_create; } 
else if (strcmp(target, "emscripten_coroutine_next") == 0) {
return (void*)&emscripten_coroutine_next; } 
else if (strcmp(target, "emscripten_yield") == 0) {
return (void*)&emscripten_yield; } 

        WRAP(jl_options)
#ifdef _OS_EMSCRIPTEN_
        WRAP(__gmp_version)
        WRAP(__gmp_bits_per_limb)
        WRAP(__gmpz_clear)
#endif
        WRAP(jl_gc_counted_malloc)
        WRAP(jl_gc_counted_realloc_with_old_size)
        WRAP(jl_gc_counted_free_with_size)
        WRAP(jl_n_threads)
        WRAP(jl_throw_out_of_memory_error)
        else if (strcmp(target, "jl_uv_stdout") == 0) {
            return &JL_STDOUT;
        } else if (strcmp(target, "jl_uv_stderr") == 0) {
            return &JL_STDERR;
        }

 printf("%s", target);
 jl_error("Encountered cglobal not mapped in runtime_intrinsics. For now, you may add it to the list. (Or write a proper solution)\n");

return NULL;
}
