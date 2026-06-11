// This file is a part of Julia. License is MIT: https://julialang.org/license

// Host-facing entry points of the self-contained standalone frontend
// library (libjulia-frontend-standalone).
//
// This library contains a complete, private copy of the Julia runtime
// (libjulia-internal compiled objects, libuv, libsupport, ...) plus a
// system image with JuliaSyntax/JuliaLowering, all with their symbols
// internalized: only the jl_frontend_*_impl ABI below is exported. The
// host process runs its own, possibly different-version, Julia runtime.
//
// Consequently values may not be passed between the two runtimes: each has
// its own heap, GC and type objects. The functions here implement the
// frontend trampoline ABI in terms of the *host's* values, by converting
// between host values and this runtime's values at the boundary (the same
// role scm_to_julia/julia_to_scm played for the flisp frontend). Host
// values are only manipulated through a small table of host C API
// functions resolved by name at runtime (fe_host_api below), which is the
// version-stability boundary between the two runtimes.
//
// "TLS switching" here amounts to: both runtimes have their own thread
// local state under distinct (internalized) TLS symbols; on entry we make
// sure the calling thread is known to the private runtime (first use
// initializes it; later threads are adopted), and host TLS is simply left
// untouched while host API functions are called for conversion.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <dlfcn.h>

#include "julia.h"
#include "julia_internal.h"
#include "processor.h"

// Native entry points exported (internally) by the embedded image; these
// take values of the private runtime.
extern jl_value_t *jlfe_parse(const char *text, size_t text_len, jl_value_t *filename,
                              size_t lineno, size_t offset, jl_value_t *options);
extern jl_value_t *jlfe_lower(jl_value_t *ex, jl_module_t *mod, const char *filename,
                              int line, size_t world, int warn);
extern jl_value_t *jlfe_macroexpand(jl_value_t *ex, jl_module_t *mod, int recursive,
                                    int inplace, int expand_scope);
extern int jlfe_is_operator(const char *s);
extern int jlfe_is_unary_operator(const char *s);
extern int jlfe_is_unary_and_binary_operator(const char *s);
extern int jlfe_is_syntactic_operator(const char *s);
extern int jlfe_operator_precedence(const char *s);

// Embedded system image (provided by the statically linked image archive)
extern const jl_image_pointers_t jl_image_pointers;
extern const char jl_system_image_data[];
extern size_t jl_system_image_size;
extern uint32_t jl_system_image_checksum;

// ------------------------------------------------------------------------
// Host API. `hv` is a value of the *host* runtime; it is opaque here.

typedef void *hv;

static struct {
    hv   (*symbol)(const char *);
    void *(*symbol_name)(hv);
    const char *(*string_ptr)(hv);
    hv   (*pchar_to_string)(const char *, size_t);
    hv   (*exprn)(hv, size_t);
    hv   (*get_field)(hv, const char *);
    void (*array_ptr_1d_push)(hv, hv);
    hv   (*svec)(size_t, ...);
    hv   (*new_struct)(hv, ...);
    hv   (*box_int64)(int64_t);
    hv   (*box_int32)(int32_t);
    hv   (*box_uint64)(uint64_t);
    hv   (*box_uint32)(uint32_t);
    hv   (*box_uint16)(uint16_t);
    hv   (*box_uint8)(uint8_t);
    hv   (*box_float64)(double);
    hv   (*box_float32)(float);
    hv   (*box_bool)(int8_t);
    hv   (*box_char)(uint32_t);
    void **(*get_pgcstack)(void);
    void (*errorf)(const char *, ...);
    hv   (*get_global)(hv, hv);
    hv   (*call1)(hv, hv);
    hv   (*call2)(hv, hv, hv);
    hv   (*call)(hv, hv *, int32_t);
    hv   (*copy_ast)(hv);
    hv   (*invoke_julia_macro)(hv, hv, hv *, hv *, size_t, int);
    size_t (*get_world_counter)(void);
    // lowering support
    hv   (*typeof_)(hv);
    size_t (*expr_argcount)(hv);
    hv   (*expr_arg)(hv, size_t);
    void (*expr_setarg)(hv, size_t, hv);
    size_t (*string_size)(hv);
    size_t (*array_length)(hv);
    hv   (*module_name)(hv);
    hv   (*module_globalref)(hv, hv);
    hv   (*base_relative_to)(hv);
    hv   (*get_nth_field)(hv, size_t);
    int8_t (*unbox_bool)(hv);
    int64_t (*unbox_int64)(hv);
    int32_t (*unbox_int32)(hv);
    uint64_t (*unbox_uint64)(hv);
    uint32_t (*unbox_uint32)(hv);
    uint16_t (*unbox_uint16)(hv);
    uint8_t (*unbox_uint8)(hv);
    double (*unbox_float64)(hv);
    float (*unbox_float32)(hv);
    hv   (*box_ssavalue)(size_t);
    hv   (*box_slotnumber)(size_t);
    hv   (*new_code_info_uninit)(void);
    int  (*field_index)(hv, hv, int);
    void (*set_nth_field)(hv, size_t, hv);
    hv   (*new_structv)(hv, hv *, uint32_t);
    hv   (*alloc_array_1d)(hv, size_t);
    hv   (*alloc_vec_any)(size_t);
    hv   (*pchar_to_array)(const char *, size_t);
    void *(*array_ptr)(hv);
    // slots holding host globals (deref at use; filled by the host loader)
    hv *nothing;
    hv *true_;
    hv *linenumbernode_type;
    hv *quotenode_type;
    hv *base_module;
    hv *core_module;
    hv *main_module;
    hv *expr_type;
    hv *symbol_type;
    hv *string_type;
    hv *module_type;
    hv *globalref_type;
    hv *bool_type;
    hv *int64_type;
    hv *int32_type;
    hv *uint64_type;
    hv *uint32_type;
    hv *uint16_type;
    hv *uint8_type;
    hv *float64_type;
    hv *float32_type;
    hv *char_type;
    hv *gotonode_type;
    hv *gotoifnot_type;
    hv *returnnode_type;
    hv *enternode_type;
    hv *newvarnode_type;
    hv *argument_type;
    hv *code_info_type;
    hv *debuginfo_type;
    hv *any_type;
    hv *emptysvec;
    hv *array_symbol_type;
    hv *array_int32_type;
    hv *array_uint32_type;
    hv *ssavalue_type;
    hv *slotnumber_type;
    hv *array_any_type;
} H;

static const char *fe_missing_host_sym = NULL;

static void *host_sym(const char *name)
{
    // n.b. not RTLD_DEFAULT: from code in an RTLD_DEEPBIND-loaded library,
    // RTLD_DEFAULT searches the library's own scope first, which would
    // resolve these names to our private runtime. The main program's handle
    // searches the global scope only (this library is RTLD_LOCAL, so it is
    // not part of it), yielding the host runtime's symbols.
    static void *host_scope = NULL;
    if (host_scope == NULL)
        host_scope = dlopen(NULL, RTLD_LAZY | RTLD_NOLOAD);
    void *p = dlsym(host_scope, name);
    if (p == NULL && fe_missing_host_sym == NULL)
        fe_missing_host_sym = name;
    return p;
}

static void fe_resolve_host_api(void)
{
    H.symbol = (hv (*)(const char *))host_sym("jl_symbol");
    H.symbol_name = (void *(*)(hv))host_sym("jl_symbol_name");
    H.string_ptr = (const char *(*)(hv))host_sym("jl_string_ptr");
    H.pchar_to_string = (hv (*)(const char *, size_t))host_sym("jl_pchar_to_string");
    H.exprn = (hv (*)(hv, size_t))host_sym("jl_exprn");
    H.get_field = (hv (*)(hv, const char *))host_sym("jl_get_field");
    H.array_ptr_1d_push = (void (*)(hv, hv))host_sym("jl_array_ptr_1d_push");
    H.svec = (hv (*)(size_t, ...))host_sym("jl_svec");
    H.new_struct = (hv (*)(hv, ...))host_sym("jl_new_struct");
    H.box_int64 = (hv (*)(int64_t))host_sym("jl_box_int64");
    H.box_int32 = (hv (*)(int32_t))host_sym("jl_box_int32");
    H.box_uint64 = (hv (*)(uint64_t))host_sym("jl_box_uint64");
    H.box_uint32 = (hv (*)(uint32_t))host_sym("jl_box_uint32");
    H.box_uint16 = (hv (*)(uint16_t))host_sym("jl_box_uint16");
    H.box_uint8 = (hv (*)(uint8_t))host_sym("jl_box_uint8");
    H.box_float64 = (hv (*)(double))host_sym("jl_box_float64");
    H.box_float32 = (hv (*)(float))host_sym("jl_box_float32");
    H.box_bool = (hv (*)(int8_t))host_sym("jl_box_bool");
    H.box_char = (hv (*)(uint32_t))host_sym("jl_box_char");
    H.get_pgcstack = (void **(*)(void))host_sym("jl_get_pgcstack");
    H.errorf = (void (*)(const char *, ...))host_sym("jl_errorf");
    H.get_global = (hv (*)(hv, hv))host_sym("jl_get_global");
    H.call1 = (hv (*)(hv, hv))host_sym("jl_call1");
    H.call2 = (hv (*)(hv, hv, hv))host_sym("jl_call2");
    H.call = (hv (*)(hv, hv *, int32_t))host_sym("jl_call");
    H.copy_ast = (hv (*)(hv))host_sym("jl_copy_ast");
    H.invoke_julia_macro = (hv (*)(hv, hv, hv *, hv *, size_t, int))host_sym("jl_invoke_julia_macro");
    H.get_world_counter = (size_t (*)(void))host_sym("jl_get_world_counter");
    H.typeof_ = (hv (*)(hv))host_sym("jl_typeof");
    H.expr_argcount = (size_t (*)(hv))host_sym("jl_expr_argcount");
    H.expr_arg = (hv (*)(hv, size_t))host_sym("jl_expr_arg");
    H.expr_setarg = (void (*)(hv, size_t, hv))host_sym("jl_expr_setarg");
    H.string_size = (size_t (*)(hv))host_sym("jl_string_size");
    H.array_length = (size_t (*)(hv))host_sym("jl_array_length");
    H.module_name = (hv (*)(hv))host_sym("jl_module_name");
    H.module_globalref = (hv (*)(hv, hv))host_sym("jl_module_globalref");
    H.base_relative_to = (hv (*)(hv))host_sym("jl_base_relative_to");
    H.get_nth_field = (hv (*)(hv, size_t))host_sym("jl_get_nth_field");
    H.unbox_bool = (int8_t (*)(hv))host_sym("jl_unbox_bool");
    H.unbox_int64 = (int64_t (*)(hv))host_sym("jl_unbox_int64");
    H.unbox_int32 = (int32_t (*)(hv))host_sym("jl_unbox_int32");
    H.unbox_uint64 = (uint64_t (*)(hv))host_sym("jl_unbox_uint64");
    H.unbox_uint32 = (uint32_t (*)(hv))host_sym("jl_unbox_uint32");
    H.unbox_uint16 = (uint16_t (*)(hv))host_sym("jl_unbox_uint16");
    H.unbox_uint8 = (uint8_t (*)(hv))host_sym("jl_unbox_uint8");
    H.unbox_float64 = (double (*)(hv))host_sym("jl_unbox_float64");
    H.unbox_float32 = (float (*)(hv))host_sym("jl_unbox_float32");
    H.box_ssavalue = (hv (*)(size_t))host_sym("jl_box_ssavalue");
    H.box_slotnumber = (hv (*)(size_t))host_sym("jl_box_slotnumber");
    H.new_code_info_uninit = (hv (*)(void))host_sym("jl_new_code_info_uninit");
    H.field_index = (int (*)(hv, hv, int))host_sym("jl_field_index");
    H.set_nth_field = (void (*)(hv, size_t, hv))host_sym("jl_set_nth_field");
    H.new_structv = (hv (*)(hv, hv *, uint32_t))host_sym("jl_new_structv");
    H.alloc_array_1d = (hv (*)(hv, size_t))host_sym("jl_alloc_array_1d");
    H.alloc_vec_any = (hv (*)(size_t))host_sym("jl_alloc_vec_any");
    H.pchar_to_array = (hv (*)(const char *, size_t))host_sym("jl_pchar_to_array");
    H.array_ptr = (void *(*)(hv))host_sym("jl_array_ptr");
    H.nothing = (hv *)host_sym("jl_nothing");
    H.true_ = (hv *)host_sym("jl_true");
    H.linenumbernode_type = (hv *)host_sym("jl_linenumbernode_type");
    H.quotenode_type = (hv *)host_sym("jl_quotenode_type");
    H.base_module = (hv *)host_sym("jl_base_module");
    H.core_module = (hv *)host_sym("jl_core_module");
    H.main_module = (hv *)host_sym("jl_main_module");
    H.expr_type = (hv *)host_sym("jl_expr_type");
    H.symbol_type = (hv *)host_sym("jl_symbol_type");
    H.string_type = (hv *)host_sym("jl_string_type");
    H.module_type = (hv *)host_sym("jl_module_type");
    H.globalref_type = (hv *)host_sym("jl_globalref_type");
    H.bool_type = (hv *)host_sym("jl_bool_type");
    H.int64_type = (hv *)host_sym("jl_int64_type");
    H.int32_type = (hv *)host_sym("jl_int32_type");
    H.uint64_type = (hv *)host_sym("jl_uint64_type");
    H.uint32_type = (hv *)host_sym("jl_uint32_type");
    H.uint16_type = (hv *)host_sym("jl_uint16_type");
    H.uint8_type = (hv *)host_sym("jl_uint8_type");
    H.float64_type = (hv *)host_sym("jl_float64_type");
    H.float32_type = (hv *)host_sym("jl_float32_type");
    H.char_type = (hv *)host_sym("jl_char_type");
    H.gotonode_type = (hv *)host_sym("jl_gotonode_type");
    H.gotoifnot_type = (hv *)host_sym("jl_gotoifnot_type");
    H.returnnode_type = (hv *)host_sym("jl_returnnode_type");
    H.enternode_type = (hv *)host_sym("jl_enternode_type");
    H.newvarnode_type = (hv *)host_sym("jl_newvarnode_type");
    H.argument_type = (hv *)host_sym("jl_argument_type");
    H.code_info_type = (hv *)host_sym("jl_code_info_type");
    H.debuginfo_type = (hv *)host_sym("jl_debuginfo_type");
    H.any_type = (hv *)host_sym("jl_any_type");
    H.emptysvec = (hv *)host_sym("jl_emptysvec");
    H.array_symbol_type = (hv *)host_sym("jl_array_symbol_type");
    H.array_int32_type = (hv *)host_sym("jl_array_int32_type");
    H.array_uint32_type = (hv *)host_sym("jl_array_uint32_type");
    H.ssavalue_type = (hv *)host_sym("jl_ssavalue_type");
    H.slotnumber_type = (hv *)host_sym("jl_slotnumber_type");
    H.array_any_type = (hv *)host_sym("jl_array_any_type");
    if (fe_missing_host_sym != NULL) {
        if (H.errorf)
            H.errorf("libjulia-frontend-standalone: missing required host symbol %s "
                     "(the host julia is too old for the cross-runtime frontend API)",
                     fe_missing_host_sym);
        fprintf(stderr, "FATAL: libjulia-frontend-standalone: missing required host symbol %s\n",
                fe_missing_host_sym);
        abort();
    }
}

// ------------------------------------------------------------------------
// Rooting of host values while we build them: a chain of GC frames pushed
// onto the *host* pgcstack of the calling thread. Relies on the gcframe
// layout { size_t nroots; gcframe *prev; values... } with the
// JL_GC_ENCODE_PUSHARGS encoding (nroots = count << 2, values stored
// inline), which is a de-facto stable part of the runtime ABI.

typedef struct {
    size_t n;
    void *prev;
    hv slots[3];
} hframe_t;

static inline void hpush(void **pg, hframe_t *f)
{
    f->n = 0;
    f->prev = *pg;
    *pg = f;
}

static inline void hroot(hframe_t *f, int idx, hv v)
{
    f->slots[idx] = v;
    if (((size_t)(idx + 1) << 2) > f->n)
        f->n = (size_t)(idx + 1) << 2;
}

static inline void hpop(void **pg, hframe_t *f)
{
    *pg = f->prev;
}

// ------------------------------------------------------------------------
// Conversion of this runtime's values to host values, for the value kinds
// that can occur in parser output.

static hv fe2host_codeinfo(jl_value_t *v, void **pg);
static void *fe_shadow_host_ptr(jl_value_t *m);
static void *fe_foreign_ptr_of(jl_value_t *v);
// host module the current jl_frontend_lower invocation lowers into, for
// resolving the guest's `(top X)` (Base-relative) references host-side
static hv fe_cur_host_inmodule = NULL;

static jl_value_t *fe_wrap_foreign(hv p);

static hv fe2host(jl_value_t *v, void **pg)
{
    if (v == jl_nothing)
        return *H.nothing;
    if (jl_is_symbol(v))
        return H.symbol((const char *)jl_symbol_name((jl_sym_t *)v));
    if (jl_is_string(v))
        return H.pchar_to_string(jl_string_data(v), jl_string_len(v));
    jl_value_t *t = jl_typeof(v); // n.b. normalizes small type tags
    if (t == (jl_value_t *)jl_bool_type)
        return H.box_bool(jl_unbox_bool(v));
    if (t == (jl_value_t *)jl_int64_type)
        return H.box_int64(jl_unbox_int64(v));
    if (t == (jl_value_t *)jl_int32_type)
        return H.box_int32(jl_unbox_int32(v));
    if (t == (jl_value_t *)jl_uint64_type)
        return H.box_uint64(jl_unbox_uint64(v));
    if (t == (jl_value_t *)jl_uint32_type)
        return H.box_uint32(jl_unbox_uint32(v));
    if (t == (jl_value_t *)jl_uint16_type)
        return H.box_uint16(jl_unbox_uint16(v));
    if (t == (jl_value_t *)jl_uint8_type)
        return H.box_uint8(jl_unbox_uint8(v));
    if (t == (jl_value_t *)jl_float64_type)
        return H.box_float64(jl_unbox_float64(v));
    if (t == (jl_value_t *)jl_float32_type)
        return H.box_float32(jl_unbox_float32(v));
    if (t == (jl_value_t *)jl_char_type)
        return H.box_char(*(uint32_t *)jl_data_ptr(v));
    if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t *)v;
        size_t i, n = jl_expr_nargs(e);
        hv hhead = H.symbol((const char *)jl_symbol_name(e->head));
        hframe_t f;
        hpush(pg, &f);
        hv he = H.exprn(hhead, 0);
        hroot(&f, 0, he);
        hv hargs = H.get_field(he, "args");
        for (i = 0; i < n; i++) {
            hv c = fe2host(jl_exprarg(e, i), pg);
            hroot(&f, 1, c);
            H.array_ptr_1d_push(hargs, c);
        }
        hpop(pg, &f);
        return he;
    }
    if (t == (jl_value_t *)jl_linenumbernode_type) {
        hframe_t f;
        hpush(pg, &f);
        hv line = fe2host(jl_fieldref(v, 0), pg);
        hroot(&f, 0, line);
        hv file = fe2host(jl_fieldref(v, 1), pg);
        hroot(&f, 1, file);
        hv r = H.new_struct(*H.linenumbernode_type, line, file);
        hpop(pg, &f);
        return r;
    }
    if (t == (jl_value_t *)jl_quotenode_type) {
        hframe_t f;
        hpush(pg, &f);
        hv val = fe2host(jl_quotenode_value(v), pg);
        hroot(&f, 0, val);
        hv r = H.new_struct(*H.quotenode_type, val);
        hpop(pg, &f);
        return r;
    }
    if (t == (jl_value_t *)jl_module_type) {
        // lowering-inserted references to this runtime's Core/Base/Main map
        // to the host's; other modules must be shadows of host modules
        if (v == (jl_value_t *)jl_core_module)
            return *H.core_module;
        if (v == (jl_value_t *)jl_base_module) {
            // `(top X)` references: flisp resolves these relative to the
            // module being lowered into, falling back to Core during
            // bootstrap when Base does not exist yet
            hv relto = fe_cur_host_inmodule != NULL ? fe_cur_host_inmodule
                                                    : *H.main_module;
            return H.base_relative_to(relto);
        }
        if (v == (jl_value_t *)jl_main_module)
            return *H.main_module;
        void *hmod = fe_shadow_host_ptr(v);
        if (hmod == NULL)
            H.errorf("libjulia-frontend-standalone: module %s is not a host module shadow",
                     jl_symbol_name(((jl_module_t *)v)->name));
        return (hv)hmod;
    }
    if (t == (jl_value_t *)jl_globalref_type) {
        hv hmod = fe2host((jl_value_t *)jl_globalref_mod(v), pg);
        return H.module_globalref(hmod, H.symbol(jl_symbol_name(jl_globalref_name(v))));
    }
    if (t == (jl_value_t *)jl_ssavalue_type)
        return H.box_ssavalue(((jl_ssavalue_t *)v)->id);
    if (t == (jl_value_t *)jl_slotnumber_type)
        return H.box_slotnumber(jl_slot_number(v));
    if (t == (jl_value_t *)jl_argument_type) {
        hv arg = H.box_int64(jl_unbox_long(jl_fieldref(v, 0)));
        return H.new_structv(*H.argument_type, &arg, 1);
    }
    if (t == (jl_value_t *)jl_gotonode_type) {
        hv arg = H.box_int64(jl_gotonode_label(v));
        return H.new_structv(*H.gotonode_type, &arg, 1);
    }
    if (t == (jl_value_t *)jl_gotoifnot_type) {
        hframe_t f;
        hpush(pg, &f);
        hv args[2];
        args[0] = fe2host(jl_gotoifnot_cond(v), pg);
        hroot(&f, 0, args[0]);
        args[1] = H.box_int64(jl_gotoifnot_label(v));
        hroot(&f, 1, args[1]);
        hv r = H.new_structv(*H.gotoifnot_type, args, 2);
        hpop(pg, &f);
        return r;
    }
    if (t == (jl_value_t *)jl_returnnode_type) {
        if (!jl_field_isdefined(v, 0))
            return H.new_structv(*H.returnnode_type, NULL, 0); // unreachable terminator
        hframe_t f;
        hpush(pg, &f);
        hv arg = fe2host(jl_returnnode_value(v), pg);
        hroot(&f, 0, arg);
        hv r = H.new_structv(*H.returnnode_type, &arg, 1);
        hpop(pg, &f);
        return r;
    }
    if (t == (jl_value_t *)jl_enternode_type) {
        hframe_t f;
        hpush(pg, &f);
        hv args[2];
        uint32_t nargs = 1;
        args[0] = H.box_int64(jl_enternode_catch_dest(v));
        hroot(&f, 0, args[0]);
        if (jl_field_isdefined(v, 1) && jl_enternode_scope(v) != NULL) {
            args[1] = fe2host(jl_enternode_scope(v), pg);
            hroot(&f, 1, args[1]);
            nargs = 2;
        }
        hv r = H.new_structv(*H.enternode_type, args, nargs);
        hpop(pg, &f);
        return r;
    }
    if (t == (jl_value_t *)jl_newvarnode_type) {
        hframe_t f;
        hpush(pg, &f);
        hv arg = fe2host(jl_fieldref(v, 0), pg);
        hroot(&f, 0, arg);
        hv r = H.new_structv(*H.newvarnode_type, &arg, 1);
        hpop(pg, &f);
        return r;
    }
    if (t == (jl_value_t *)jl_code_info_type)
        return fe2host_codeinfo(v, pg);
    if (jl_is_svec(v)) {
        if (jl_svec_len(v) == 0)
            return *H.emptysvec;
        H.errorf("libjulia-frontend-standalone: cannot convert non-empty "
                 "SimpleVector across the runtime boundary yet");
    }
    if (jl_is_tuple(v)) {
        // e.g. the (:ccall, effects) calling-convention tuple in lowered
        // foreigncall expressions
        size_t i, n = jl_nfields(v);
        hframe_t f;
        hpush(pg, &f);
        hv ha = H.alloc_vec_any(0);
        hroot(&f, 0, ha);
        for (i = 0; i < n; i++) {
            hv c = fe2host(jl_get_nth_field(v, i), pg);
            hroot(&f, 1, c);
            H.array_ptr_1d_push(ha, c);
        }
        hv tuple_f = H.get_global(*H.core_module, H.symbol("tuple"));
        hv r = H.call(tuple_f, (hv *)H.array_ptr(ha), (int32_t)n);
        hpop(pg, &f);
        if (r == NULL)
            H.errorf("libjulia-frontend-standalone: host tuple construction failed");
        return r;
    }
    if (t == (jl_value_t *)jl_datatype_type) {
        if (v == (jl_value_t *)jl_any_type)
            return *H.any_type;
        H.errorf("libjulia-frontend-standalone: cannot convert type %s across the "
                 "runtime boundary", jl_symbol_name(((jl_datatype_t *)v)->name->name));
    }
    {
        void *fp = fe_foreign_ptr_of(v);
        if (fp != NULL)
            return (hv)fp; // host value passing back through unchanged
    }
    // Core.MacroSource carries macro provenance (location + syntax version)
    // in macro expansions; construct the host's equivalent, or degrade to
    // the plain location if the host runtime predates the type.
    jl_value_t *msty = jl_get_global(jl_core_module, jl_symbol("MacroSource"));
    if (msty != NULL && t == msty) {
        hv hty = H.get_global(*H.core_module, H.symbol("MacroSource"));
        if (hty == NULL)
            return fe2host(jl_fieldref(v, 0), pg);
        hframe_t f;
        hpush(pg, &f);
        hv args[2];
        args[0] = fe2host(jl_fieldref(v, 0), pg);
        hroot(&f, 0, args[0]);
        args[1] = fe2host(jl_fieldref(v, 1), pg);
        hroot(&f, 1, args[1]);
        hv r = H.new_structv(hty, args, 2);
        hpop(pg, &f);
        return r;
    }
    // VersionNumber appears in parser output as the per-module syntax
    // version marker; round-trip it through its string form.
    jl_value_t *vnty = jl_get_global(jl_base_module, jl_symbol("VersionNumber"));
    if (vnty != NULL && t == vnty) {
        // n.b. the host's Base must be found through jl_base_relative_to:
        // the jl_base_module CLI data slot is a snapshot taken before
        // bootstrap defines Base. Pre-Base there is no host VersionNumber
        // type at all; degrade to `nothing` (this can only be reached for
        // the syntax-version field of a MacroSource during bootstrap).
        hv hbase = H.base_relative_to(*H.main_module);
        hv hctor = hbase != NULL ? H.get_global(hbase, H.symbol("VersionNumber")) : NULL;
        if (hctor == NULL)
            return *H.nothing;
        jl_value_t *str = jl_call1(jl_get_function(jl_base_module, "string"), v);
        if (str == NULL || !jl_is_string(str))
            H.errorf("libjulia-frontend-standalone: failed to stringify VersionNumber");
        hframe_t f;
        hpush(pg, &f);
        hv hstr = H.pchar_to_string(jl_string_data(str), jl_string_len(str));
        hroot(&f, 0, hstr);
        hv r = H.call1(hctor, hstr);
        hpop(pg, &f);
        if (r == NULL)
            H.errorf("libjulia-frontend-standalone: host VersionNumber construction failed");
        return r;
    }
    H.errorf("libjulia-frontend-standalone: cannot convert value of type %s "
             "across the runtime boundary", jl_typeof_str(v));
    return NULL; // unreachable
}

// ------------------------------------------------------------------------
// Conversion of host values to this runtime's values, for surface ASTs
// passed to lowering. Host modules are mirrored by shadow modules; host
// values with no syntactic meaning are wrapped opaquely and flow through
// lowering unchanged (the host tree stays rooted by our caller for the
// duration of the entry call, so the raw pointers remain valid).

// Registry of shadow modules mirroring host modules, and the wrapper type
// carrying opaque host values through lowering. Kept entirely on the C side
// (using this runtime's C API directly) so no guest Julia code runs for it.
// The shadow modules array doubles as the GC root for the shadows (it is
// bound as a constant in the guest's Main).
static jl_array_t *fe_shadow_mods = NULL;
static void **fe_shadow_hostptrs = NULL;
static size_t fe_nshadows = 0, fe_shadow_cap = 0;
static jl_value_t *fe_foreign_type = NULL;

static void fe_cross_runtime_init(void)
{
    fe_shadow_mods = jl_alloc_vec_any(0);
    jl_set_const(jl_main_module, jl_symbol("__fe_cross_runtime_shadow_modules"),
                 (jl_value_t *)fe_shadow_mods);
    {
        // Salt closure-type names with a host-unique token: name
        // uniquification scans shadow modules, which start empty in every
        // process, so without it separate sessions whose lowered output is
        // serialized into the same image chain (bootstrap stages) would
        // reuse names. The host world counter is monotonic across such
        // chains.
        char saltbuf[128];
        snprintf(saltbuf, sizeof(saltbuf),
                 "JuliaFrontend.JuliaLowering._module_binding_salt[] = \"w%zu#\"",
                 (size_t)H.get_world_counter());
        jl_eval_string(saltbuf);
        if (jl_exception_occurred()) {
            jl_exception_clear();
            fprintf(stderr, "libjulia-frontend-standalone: warning: could not "
                            "set the binding name salt\n");
        }
    }
    fe_foreign_type = jl_eval_string(
        "struct __FeHostValue ptr::Ptr{Cvoid} end; __FeHostValue");
    if (fe_foreign_type == NULL || !jl_is_datatype(fe_foreign_type)) {
        fprintf(stderr, "FATAL: libjulia-frontend-standalone: could not define "
                        "the host value wrapper type\n");
        abort();
    }
}

static jl_value_t *fe_shadow_module(hv hmod)
{
    for (size_t i = 0; i < fe_nshadows; i++)
        if (fe_shadow_hostptrs[i] == (void *)hmod)
            return jl_array_ptr_ref(fe_shadow_mods, i);
    hv hname = H.module_name(hmod);
    jl_module_t *m = jl_new_module(jl_symbol((const char *)H.symbol_name(hname)), jl_main_module);
    JL_GC_PUSH1(&m);
    jl_array_ptr_1d_push(fe_shadow_mods, (jl_value_t *)m);
    JL_GC_POP();
    if (fe_nshadows == fe_shadow_cap) {
        fe_shadow_cap = fe_shadow_cap ? 2 * fe_shadow_cap : 16;
        fe_shadow_hostptrs = (void **)realloc(fe_shadow_hostptrs,
                                              fe_shadow_cap * sizeof(void *));
    }
    fe_shadow_hostptrs[fe_nshadows++] = (void *)hmod;
    return jl_array_ptr_ref(fe_shadow_mods, fe_nshadows - 1);
}

static void *fe_shadow_host_ptr(jl_value_t *m)
{
    for (size_t i = 0; i < fe_nshadows; i++)
        if (jl_array_ptr_ref(fe_shadow_mods, i) == m)
            return fe_shadow_hostptrs[i];
    return NULL;
}

static jl_value_t *fe_wrap_foreign(hv p)
{
    jl_value_t *boxed = jl_box_voidpointer((void *)p);
    JL_GC_PUSH1(&boxed);
    jl_value_t *r = jl_new_struct((jl_datatype_t *)fe_foreign_type, boxed);
    JL_GC_POP();
    return r;
}

static void *fe_foreign_ptr_of(jl_value_t *v)
{
    // n.b. the Ptr{Cvoid} field is stored inline (isbits), so it must be
    // read as raw data; jl_fieldref_noalloc is only valid for fields that
    // hold references.
    if (fe_foreign_type != NULL && jl_typeof(v) == fe_foreign_type)
        return *(void **)jl_data_ptr(v);
    return NULL;
}

static jl_value_t *host2fe(hv v)
{
    if (v == *H.nothing)
        return jl_nothing;
    hv ht = H.typeof_(v);
    if (ht == *H.symbol_type)
        return (jl_value_t *)jl_symbol((const char *)H.symbol_name(v));
    if (ht == *H.string_type)
        return jl_pchar_to_string(H.string_ptr(v), H.string_size(v));
    if (ht == *H.bool_type)
        return H.unbox_bool(v) ? jl_true : jl_false;
    if (ht == *H.int64_type)
        return jl_box_int64(H.unbox_int64(v));
    if (ht == *H.int32_type)
        return jl_box_int32(H.unbox_int32(v));
    if (ht == *H.uint64_type)
        return jl_box_uint64(H.unbox_uint64(v));
    if (ht == *H.uint32_type)
        return jl_box_uint32(H.unbox_uint32(v));
    if (ht == *H.uint16_type)
        return jl_box_uint16(H.unbox_uint16(v));
    if (ht == *H.uint8_type)
        return jl_box_uint8(H.unbox_uint8(v));
    if (ht == *H.float64_type)
        return jl_box_float64(H.unbox_float64(v));
    if (ht == *H.float32_type)
        return jl_box_float32(H.unbox_float32(v));
    if (ht == *H.char_type)
        return jl_box_char(H.unbox_uint32(v));
    if (ht == *H.expr_type) {
        hv hhead = H.get_field(v, "head");
        size_t i, n = H.expr_argcount(v);
        jl_expr_t *e = jl_exprn(jl_symbol((const char *)H.symbol_name(hhead)), n);
        JL_GC_PUSH1(&e);
        for (i = 0; i < n; i++) {
            jl_value_t *c = host2fe(H.expr_arg(v, i));
            jl_exprargset(e, i, c);
        }
        JL_GC_POP();
        return (jl_value_t *)e;
    }
    if (ht == *H.linenumbernode_type) {
        jl_value_t *line = NULL, *file = NULL, *r = NULL;
        JL_GC_PUSH2(&line, &file);
        line = host2fe(H.get_field(v, "line"));
        file = host2fe(H.get_field(v, "file"));
        r = jl_new_struct(jl_linenumbernode_type, line, file);
        JL_GC_POP();
        return r;
    }
    if (ht == *H.quotenode_type) {
        jl_value_t *val = host2fe(H.get_field(v, "value"));
        JL_GC_PUSH1(&val);
        jl_value_t *r = jl_new_struct(jl_quotenode_type, val);
        JL_GC_POP();
        return r;
    }
    if (ht == *H.globalref_type) {
        jl_value_t *m = fe_shadow_module(H.get_field(v, "mod"));
        jl_sym_t *name = jl_symbol((const char *)H.symbol_name(H.get_field(v, "name")));
        return jl_module_globalref((jl_module_t *)m, name);
    }
    if (ht == *H.module_type)
        return fe_shadow_module(v);
    {
        // host Tuples (e.g. the (:ccall, effects) calling-convention tuple
        // produced by ccall macro expansion) convert structurally
        hv isa_f = H.get_global(*H.core_module, H.symbol("isa"));
        hv tuple_ty = H.get_global(*H.core_module, H.symbol("Tuple"));
        hv isa_r = (isa_f != NULL && tuple_ty != NULL) ? H.call2(isa_f, v, tuple_ty) : NULL;
        if (isa_r != NULL && isa_r == *H.true_) {
            hv nfields_f = H.get_global(*H.core_module, H.symbol("nfields"));
            hv getfield_f = H.get_global(*H.core_module, H.symbol("getfield"));
            size_t i, n = (size_t)H.unbox_int64(H.call1(nfields_f, v));
            jl_value_t **fe_args;
            JL_GC_PUSHARGS(fe_args, n);
            for (i = 0; i < n; i++) {
                hv elt = H.call2(getfield_f, v, H.box_int64((int64_t)(i + 1)));
                fe_args[i] = host2fe(elt);
            }
            jl_value_t *r = jl_f_tuple(NULL, fe_args, (uint32_t)n);
            JL_GC_POP();
            return r;
        }
    }
    if (ht == *H.array_any_type) {
        // e.g. the argnames Vector{Any} in a generated function stub's
        // Expr(:lambda, ...). Vector{Any} data is contiguous jl_value_t*
        // (and the host GC is non-moving), so reading through the data
        // pointer is safe while the array stays rooted by the input tree.
        size_t i, n = H.array_length(v);
        hv *data = (hv *)H.array_ptr(v);
        jl_array_t *a = jl_alloc_vec_any(n);
        JL_GC_PUSH1(&a);
        for (i = 0; i < n; i++) {
            jl_value_t *c = host2fe(data[i]);
            jl_array_ptr_set(a, i, c);
        }
        JL_GC_POP();
        return (jl_value_t *)a;
    }
    return fe_wrap_foreign(v);
}

// ------------------------------------------------------------------------
// Conversion of lowered output to host values: in addition to the parser
// output kinds handled by fe2host, lowered code contains modules,
// GlobalRefs, the linear IR node types, and CodeInfo itself. CodeInfo is
// constructed host-side field-by-field by *name* (jl_field_index /
// jl_set_nth_field), so the two runtimes only need compatible field
// meanings, not identical layouts.

static hv fe2host_vec_any(jl_value_t *a, void **pg)
{
    size_t i, n = jl_array_nrows((jl_array_t *)a);
    hframe_t f;
    hpush(pg, &f);
    hv ha = H.alloc_vec_any(0);
    hroot(&f, 0, ha);
    for (i = 0; i < n; i++) {
        hv c = fe2host(jl_array_ptr_ref((jl_array_t *)a, i), pg);
        hroot(&f, 1, c);
        H.array_ptr_1d_push(ha, c);
    }
    hpop(pg, &f);
    return ha;
}

static hv fe2host_symbol_vec(jl_value_t *a, void **pg)
{
    size_t i, n = jl_array_nrows((jl_array_t *)a);
    hframe_t f;
    hpush(pg, &f);
    hv ha = H.alloc_array_1d(*H.array_symbol_type, 0);
    hroot(&f, 0, ha);
    for (i = 0; i < n; i++) {
        jl_value_t *sym = jl_array_ptr_ref((jl_array_t *)a, i);
        H.array_ptr_1d_push(ha, H.symbol((const char *)jl_symbol_name((jl_sym_t *)sym)));
    }
    hpop(pg, &f);
    return ha;
}

static hv fe2host_bits_vec(jl_value_t *a, hv htype, size_t elsz)
{
    size_t n = jl_array_nrows((jl_array_t *)a);
    hv ha = H.alloc_array_1d(htype, n);
    memcpy(H.array_ptr(ha), jl_array_data((jl_array_t *)a, char), n * elsz);
    return ha;
}

static hv fe2host_debuginfo(jl_value_t *di, void **pg)
{
    // DebugInfo(def::Union{Symbol,Method,MethodInstance}, linetable, edges::SimpleVector, codelocs::String)
    jl_value_t *def = jl_fieldref_noalloc(di, 0);
    jl_value_t *linetable = jl_fieldref_noalloc(di, 1);
    jl_value_t *edges = jl_fieldref_noalloc(di, 2);
    jl_value_t *codelocs = jl_fieldref_noalloc(di, 3);
    if (!jl_is_symbol(def))
        H.errorf("libjulia-frontend-standalone: cannot convert DebugInfo.def of type %s",
                 jl_typeof_str(def));
    if (jl_svec_len(edges) != 0)
        H.errorf("libjulia-frontend-standalone: cannot convert DebugInfo with edges yet");
    hframe_t f;
    hpush(pg, &f);
    hv args[4];
    args[0] = H.symbol((const char *)jl_symbol_name((jl_sym_t *)def));
    args[1] = linetable == jl_nothing ? *H.nothing : fe2host_debuginfo(linetable, pg);
    hroot(&f, 0, args[1]);
    args[2] = *H.emptysvec;
    args[3] = H.pchar_to_string(jl_string_data(codelocs), jl_string_len(codelocs));
    hroot(&f, 1, args[3]);
    hv r = H.new_structv(*H.debuginfo_type, args, 4);
    hpop(pg, &f);
    return r;
}

static hv fe2host_codeinfo(jl_value_t *v, void **pg)
{
    jl_datatype_t *gt = (jl_datatype_t *)jl_typeof(v);
    size_t i, nf = jl_datatype_nfields(gt);
    hframe_t f;
    hpush(pg, &f);
    hv hci = H.new_code_info_uninit();
    hroot(&f, 0, hci);
    for (i = 0; i < nf; i++) {
        jl_sym_t *fname = (jl_sym_t *)jl_svecref(jl_field_names(gt), i);
        if (!jl_field_isdefined(v, i))
            continue;
        int hidx = H.field_index(*H.code_info_type, H.symbol((const char *)jl_symbol_name(fname)), 0);
        if (hidx < 0)
            continue; // field unknown to the host's CodeInfo; skip
        jl_value_t *fv = jl_fieldref(v, i);
        hv hfv;
        jl_value_t *ft = jl_typeof(fv);
        if (ft == (jl_value_t *)jl_array_any_type)
            hfv = fe2host_vec_any(fv, pg);
        else if (ft == (jl_value_t *)jl_array_symbol_type)
            hfv = fe2host_symbol_vec(fv, pg);
        else if (ft == (jl_value_t *)jl_array_uint8_type)
            hfv = H.pchar_to_array((const char *)jl_array_data((jl_array_t *)fv, uint8_t),
                                   jl_array_nrows((jl_array_t *)fv));
        else if (ft == (jl_value_t *)jl_array_int32_type)
            hfv = fe2host_bits_vec(fv, *H.array_int32_type, sizeof(int32_t));
        else if (ft == (jl_value_t *)jl_array_uint32_type)
            hfv = fe2host_bits_vec(fv, *H.array_uint32_type, sizeof(uint32_t));
        else if (ft == (jl_value_t *)jl_debuginfo_type)
            hfv = fe2host_debuginfo(fv, pg);
        else
            hfv = fe2host(fv, pg);
        hroot(&f, 1, hfv);
        H.set_nth_field(hci, (size_t)hidx, hfv);
    }
    hpop(pg, &f);
    return hci;
}

// ------------------------------------------------------------------------
// Private runtime initialization and per-thread entry

static pthread_once_t fe_init_once = PTHREAD_ONCE_INIT;

static void fe_runtime_init(void)
{
    fe_resolve_host_api();

    jl_init_options();
    // This runtime is a guest in the host's process: leave signal handling
    // to the host, and run single-threaded.
    jl_options.handle_signals = JL_OPTIONS_HANDLE_SIGNALS_OFF;
    static const int16_t nthreads_per_pool[2] = {1, 0};
    jl_options.nthreads = 1;
    jl_options.nthreadpools = 1;
    jl_options.nthreads_per_pool = nthreads_per_pool;
    jl_options.nmarkthreads = 0;
    jl_options.nsweepthreads = 0;
    jl_options.startupfile = JL_OPTIONS_STARTUPFILE_OFF;

    static char selfpath[4096];
    Dl_info info = {0};
    if (dladdr((void *)&fe_runtime_init, &info) && info.dli_fname &&
        strlen(info.dli_fname) < sizeof(selfpath)) {
        strcpy(selfpath, info.dli_fname);
        jl_options.image_file = selfpath;
    }
    // Sys.__init__ requires julia_bindir to be non-NULL (the equivalent of
    // jlapi.c's jl_resolve_sysimg_location, which is static there)
    static char bindir[4096];
    if (jl_options.image_file) {
        strcpy(bindir, jl_options.image_file);
        char *slash = strrchr(bindir, '/');
        if (slash)
            *slash = '\0';
        jl_options.julia_bindir = bindir;
    }

    // The image is statically linked into this library; construct the
    // image buffer directly from our own symbols.
    jl_image_buf_t image = {
        .kind = JL_IMAGE_KIND_SO,
        .pointers = &jl_image_pointers,
        .data = jl_system_image_data,
        .size = jl_system_image_size,
        .checksum = jl_system_image_checksum,
        .base = (intptr_t)info.dli_fbase,
    };

    jl_init_(image);
    jl_exception_clear();
    fe_cross_runtime_init();
}

static void fe_enter(void)
{
    pthread_once(&fe_init_once, fe_runtime_init);
    // Adopt threads that this runtime has not seen before. (The thread
    // that ran fe_runtime_init is already the runtime's thread 0.)
    if (jl_get_pgcstack() == NULL)
        jl_adopt_thread();
}

// ------------------------------------------------------------------------
// The frontend ABI

JL_DLLEXPORT void jl_frontend_init_impl(void)
{
    fe_enter();
}

JL_DLLEXPORT hv jl_frontend_parse_impl(const char *text, size_t text_len, hv filename,
                                       size_t lineno, size_t offset, hv options)
{
    fe_enter();
    const char *fname = H.string_ptr(filename);
    const char *opts = (const char *)H.symbol_name(options);

    jl_value_t *fe_res = NULL, *fe_fname = NULL;
    JL_GC_PUSH2(&fe_res, &fe_fname);
    fe_fname = jl_pchar_to_string(fname, strlen(fname));
    jl_value_t *fe_opts = (jl_value_t *)jl_symbol(opts);
    fe_res = jlfe_parse(text, text_len, fe_fname, lineno, offset, fe_opts);

    // Convert svec(expr, end_offset) to host values. The result of this
    // runtime stays rooted (fe_res) while we walk it; the host values are
    // rooted through frames pushed on the host's pgcstack.
    void **pg = (void **)H.get_pgcstack();
    hframe_t f;
    hpush(pg, &f);
    hv hex = fe2host(jl_svecref(fe_res, 0), pg);
    hroot(&f, 0, hex);
    hv hofs = H.box_int64(jl_unbox_int64(jl_svecref(fe_res, 1)));
    hroot(&f, 1, hofs);
    hv result = H.svec(2, hex, hofs);
    hpop(pg, &f);
    JL_GC_POP();
    return result;
}

// ------------------------------------------------------------------------
// Macro expansion, performed host-side on the host's syntax tree before it
// crosses into this runtime for lowering: the macro functions live in the
// host's world and are resolved and invoked there through the host's
// jl_invoke_julia_macro, so macro arguments and results never need to be
// converted. This is a port of the flisp frontend's jl_expand_macros (see
// src/flfrontend.c), driving the host runtime through the host API table;
// the resulting `hygienic-scope` wrappers are understood by JuliaLowering's
// Expr compatibility mode (with the modules inside becoming shadows).
//
// One difference from flisp: `quote` is left alone (like `inert`), since
// JuliaLowering expands quoted syntax itself during lowering, while flisp
// required backquote pre-expansion.

typedef struct h_macroctx {
    hv m;
    struct h_macroctx *parent;
} h_macroctx_t;

static int h_head_is(hv e, const char *name)
{
    hv head = H.get_field(e, "head");
    return strcmp((const char *)H.symbol_name(head), name) == 0;
}

static int h_is_expr(hv v)
{
    return v != NULL && H.typeof_(v) == *H.expr_type;
}

static int h_isa_ast_node(hv v)
{
    hv t = H.typeof_(v);
    return t == *H.expr_type || t == *H.symbol_type || t == *H.globalref_type ||
        t == *H.linenumbernode_type || t == *H.quotenode_type ||
        t == *H.newvarnode_type || t == *H.code_info_type ||
        t == *H.gotonode_type || t == *H.gotoifnot_type ||
        t == *H.returnnode_type || t == *H.enternode_type ||
        t == *H.ssavalue_type || t == *H.slotnumber_type ||
        t == *H.argument_type;
}

static int h_is_self_escaping_expr(hv e)
{
    return h_head_is(e, "inert") || h_head_is(e, "leave") || h_head_is(e, "core") ||
        h_head_is(e, "line") || h_head_is(e, "lineinfo") || h_head_is(e, "meta") ||
        h_head_is(e, "boundscheck") || h_head_is(e, "inline") || h_head_is(e, "noinline");
}

static int h_need_esc_node(hv v)
{
    hv t = H.typeof_(v);
    if (t == *H.linenumbernode_type || t == *H.ssavalue_type ||
        t == *H.slotnumber_type || t == *H.argument_type ||
        t == *H.enternode_type || t == *H.quotenode_type)
        return 0;
    if (t == *H.expr_type)
        return !h_is_self_escaping_expr(v);
    return h_isa_ast_node(v);
}

static hv h_expand_macros_in_quote(hv expr, hv inmodule, h_macroctx_t *macroctx,
                                   int onelevel, size_t world, int throw_load_error,
                                   int depth);

static hv h_expand_macros(hv expr, hv inmodule, h_macroctx_t *macroctx,
                          int onelevel, size_t world, int throw_load_error)
{
    if (expr == NULL || !h_is_expr(expr))
        return expr;
    if (h_head_is(expr, "inert") || h_head_is(expr, "module") ||
        h_head_is(expr, "toplevel") || h_head_is(expr, "meta"))
        return expr;
    if (h_head_is(expr, "quote")) {
        // quoted syntax is left for the lowering side, EXCEPT live code in
        // `$` interpolations, whose macros must be expanded here like any
        // other code (flisp does the same)
        return h_expand_macros_in_quote(expr, inmodule, macroctx, onelevel, world,
                                        throw_load_error, 0);
    }
    void **pg = (void **)H.get_pgcstack();
    if (h_head_is(expr, "hygienic-scope") && H.expr_argcount(expr) >= 2) {
        h_macroctx_t newctx;
        newctx.m = H.expr_arg(expr, 1);
        newctx.parent = macroctx;
        hv a = H.expr_arg(expr, 0);
        hframe_t f;
        hpush(pg, &f);
        hv a2 = h_expand_macros(a, inmodule, &newctx, onelevel, world, throw_load_error);
        hroot(&f, 0, a2);
        hv r = expr;
        if (h_is_expr(a2) && h_head_is(a2, "escape") && !h_need_esc_node(H.expr_arg(a2, 0)))
            r = H.expr_arg(a2, 0);
        else if (!h_need_esc_node(a2))
            r = a2;
        else if (a != a2)
            H.expr_setarg(expr, 0, a2);
        hpop(pg, &f);
        return r;
    }
    if (h_head_is(expr, "macrocall")) {
        h_macroctx_t newctx;
        newctx.m = macroctx ? macroctx->m : inmodule;
        newctx.parent = macroctx;
        hv lineinfo = NULL;
        hv margs = H.get_field(expr, "args");
        hv result = H.invoke_julia_macro(margs, inmodule, &newctx.m, &lineinfo,
                                         world, throw_load_error);
        if (!h_need_esc_node(result))
            return result;
        if (h_is_expr(result) && h_head_is(result, "toplevel")) {
            // a `toplevel` expansion is deferred and re-lowered chunk by
            // chunk by the runtime; wrap each chunk in its own
            // hygienic-scope so chunks stay self-contained
            hframe_t ft;
            hpush(pg, &ft);
            hroot(&ft, 0, result);
            hroot(&ft, 1, newctx.m);
            hroot(&ft, 2, lineinfo);
            result = H.copy_ast(result);
            hroot(&ft, 0, result);
            size_t ti, tn = H.expr_argcount(result);
            for (ti = 0; ti < tn; ti++) {
                hv c = H.expr_arg(result, ti);
                if (h_need_esc_node(c)) {
                    hv cwrap = H.exprn(H.symbol("hygienic-scope"), 3);
                    H.expr_setarg(cwrap, 0, c);
                    H.expr_setarg(cwrap, 1, newctx.m);
                    H.expr_setarg(cwrap, 2, lineinfo);
                    H.expr_setarg(result, ti, cwrap);
                }
            }
            hpop(pg, &ft);
            return result;
        }
        hframe_t f;
        hpush(pg, &f);
        hroot(&f, 0, result);
        hroot(&f, 1, newctx.m);
        hroot(&f, 2, lineinfo);
        hv wrap = NULL;
        if (h_is_expr(result) && h_head_is(result, "escape"))
            result = H.expr_arg(result, 0);
        else
            wrap = H.exprn(H.symbol("hygienic-scope"), 3);
        hroot(&f, 0, result);
        if (wrap != NULL) {
            // re-push the frame to keep both result and wrap rooted
            hframe_t f2;
            hpush(pg, &f2);
            hroot(&f2, 0, wrap);
            result = H.copy_ast(result);
            hroot(&f, 0, result);
            if (!onelevel)
                result = h_expand_macros(result, inmodule, &newctx, onelevel,
                                         world, throw_load_error);
            hroot(&f, 0, result);
            if (h_need_esc_node(result)) {
                H.expr_setarg(wrap, 0, result);
                H.expr_setarg(wrap, 1, newctx.m);
                H.expr_setarg(wrap, 2, lineinfo);
                if (h_is_expr(result) && h_head_is(result, "escape"))
                    result = H.expr_arg(result, 0);
                else
                    result = wrap;
            }
            hpop(pg, &f2);
        }
        else {
            result = H.copy_ast(result);
            hroot(&f, 0, result);
            if (!onelevel)
                result = h_expand_macros(result, inmodule, macroctx, onelevel,
                                         world, throw_load_error);
        }
        hpop(pg, &f);
        return result;
    }
    if (h_head_is(expr, "do") && H.expr_argcount(expr) == 2 &&
        h_is_expr(H.expr_arg(expr, 0)) && h_head_is(H.expr_arg(expr, 0), "macrocall")) {
        hv mc = H.expr_arg(expr, 0);
        size_t j, nm = H.expr_argcount(mc);
        hframe_t f;
        hpush(pg, &f);
        hv mc2 = H.exprn(H.symbol("macrocall"), nm + 1);
        hroot(&f, 0, mc2);
        H.expr_setarg(mc2, 0, H.expr_arg(mc, 0)); // macro name
        H.expr_setarg(mc2, 1, H.expr_arg(mc, 1)); // location
        H.expr_setarg(mc2, 2, H.expr_arg(expr, 1)); // function argument
        for (j = 2; j < nm; j++)
            H.expr_setarg(mc2, j + 1, H.expr_arg(mc, j));
        hv ret = h_expand_macros(mc2, inmodule, macroctx, onelevel, world,
                                 throw_load_error);
        hpop(pg, &f);
        return ret;
    }
    if (h_head_is(expr, "escape") && macroctx != NULL)
        macroctx = macroctx->parent;

    size_t i, n = H.expr_argcount(expr);
    for (i = 0; i < n; i++) {
        hv a = H.expr_arg(expr, i);
        hv a2 = h_expand_macros(a, inmodule, macroctx, onelevel, world,
                                throw_load_error);
        if (a != a2)
            H.expr_setarg(expr, i, a2);
    }
    return expr;
}

// Walk quoted syntax expanding macros only inside `$` interpolations,
// tracking quote nesting depth.
static hv h_expand_macros_in_quote(hv expr, hv inmodule, h_macroctx_t *macroctx,
                                   int onelevel, size_t world, int throw_load_error,
                                   int depth)
{
    if (expr == NULL || !h_is_expr(expr))
        return expr;
    if (h_head_is(expr, "inert") || h_head_is(expr, "meta"))
        return expr;
    int newdepth = depth;
    if (h_head_is(expr, "quote"))
        newdepth = depth + 1;
    else if (h_head_is(expr, "$"))
        newdepth = depth - 1;

    void **pg = (void **)H.get_pgcstack();
    hframe_t f;
    hpush(pg, &f);
    hroot(&f, 0, expr);
    size_t i, n = H.expr_argcount(expr);
    for (i = 0; i < n; i++) {
        hv a = H.expr_arg(expr, i);
        hv a2;
        if (newdepth == 0) {
            // live interpolated code
            a2 = h_expand_macros(a, inmodule, macroctx, onelevel, world,
                                 throw_load_error);
        } else {
            a2 = h_expand_macros_in_quote(a, inmodule, macroctx, onelevel, world,
                                          throw_load_error, newdepth);
        }
        if (a2 != a)
            H.expr_setarg(expr, i, a2);
    }
    hpop(pg, &f);
    return expr;
}

JL_DLLEXPORT hv jl_frontend_lower_impl(hv expr, hv inmodule, const char *filename,
                                       int line, size_t world, int warn)
{
    fe_enter();
    fe_cur_host_inmodule = inmodule;

    // Expand macros host-side first (flisp's lowering did the same); the
    // expanded tree, including its hygienic-scope wrappers, then crosses
    // into this runtime for lowering proper.
    {
        void **pg = (void **)H.get_pgcstack();
        hframe_t f;
        hpush(pg, &f);
        hv copied = H.copy_ast(expr);
        hroot(&f, 0, copied);
        expr = h_expand_macros(copied, inmodule, NULL, 0, world, 1);
        hroot(&f, 1, expr);

        jl_value_t *fe_ex0 = NULL, *fe_mod0 = NULL, *fe_res0 = NULL;
        JL_GC_PUSH3(&fe_ex0, &fe_mod0, &fe_res0);
        fe_mod0 = fe_shadow_module(inmodule);
        fe_ex0 = host2fe(expr);
        // n.b. frame f (rooting the expanded host tree) must stay pushed
        // until fe2host below is done: foreign-wrapped host values inside
        // fe_ex0 hold raw pointers into the host heap that are only kept
        // alive by the expanded tree.
        if (getenv("FE_DEBUG_LOWER") != NULL) {
            jl_printf(JL_STDERR, "FE_DEBUG_LOWER input: ");
            jl_static_show(JL_STDERR, fe_ex0);
            jl_printf(JL_STDERR, "\n");
        }
        fe_res0 = jlfe_lower(fe_ex0, (jl_module_t *)fe_mod0, filename ? filename : "none",
                             line, (size_t)-1, warn);
        jl_value_t *r0 = jl_svecref(fe_res0, 0);
        if (jl_svec_len(fe_res0) == 2 && jl_is_symbol(r0) &&
            r0 == (jl_value_t *)jl_symbol("__fe_lowering_error__")) {
            char buf[2048];
            jl_value_t *msg = jl_svecref(fe_res0, 1);
            snprintf(buf, sizeof(buf), "%.*s", (int)jl_string_len(msg), jl_string_data(msg));
            JL_GC_POP();
            hpop(pg, &f);
            H.errorf("jl_frontend_lower: %s", buf);
        }
        if (getenv("FE_DEBUG_LOWER") != NULL) {
            jl_printf(JL_STDERR, "FE_DEBUG_LOWER output: ");
            jl_static_show(JL_STDERR, r0);
            jl_printf(JL_STDERR, "\n");
        }
        hv hlowered = fe2host(r0, pg);
        hroot(&f, 2, hlowered);
        hv result = H.svec(1, hlowered);
        hpop(pg, &f);
        JL_GC_POP();
        return result;
    }
}

JL_DLLEXPORT hv jl_macroexpand_impl(hv expr, hv inmodule, int recursive, int inplace,
                                    int expand_scope)
{
    // Macro expansion runs entirely host-side; this runtime is not involved.
    // n.b. expand_scope (resolution of the hygienic-scope wrappers into the
    // legacy escape form) is not implemented; callers see the wrappers.
    (void)expand_scope;
    fe_enter();
    void **pg = (void **)H.get_pgcstack();
    hframe_t f;
    hpush(pg, &f);
    hv ex = expr;
    if (!inplace) {
        ex = H.copy_ast(expr);
    }
    hroot(&f, 0, ex);
    hv r = h_expand_macros(ex, inmodule, NULL, !recursive,
                           H.get_world_counter(), 0);
    hpop(pg, &f);
    return r;
}

JL_DLLEXPORT int jl_is_operator_impl(const char *s)
{
    fe_enter();
    return jlfe_is_operator(s);
}

JL_DLLEXPORT int jl_is_unary_operator_impl(const char *s)
{
    fe_enter();
    return jlfe_is_unary_operator(s);
}

JL_DLLEXPORT int jl_is_unary_and_binary_operator_impl(const char *s)
{
    fe_enter();
    return jlfe_is_unary_and_binary_operator(s);
}

JL_DLLEXPORT int jl_is_syntactic_operator_impl(const char *s)
{
    fe_enter();
    return jlfe_is_syntactic_operator(s);
}

JL_DLLEXPORT int jl_operator_precedence_impl(const char *s)
{
    fe_enter();
    return jlfe_operator_precedence(s);
}

JL_DLLEXPORT void jl_lisp_prompt_impl(void)
{
    fe_resolve_host_api();
    H.errorf("--lisp: this julia uses the JuliaSyntax/JuliaLowering frontend; "
             "there is no flisp REPL");
}

JL_DLLEXPORT void fl_profile_impl(const char *fname)
{
    (void)fname;
    fe_resolve_host_api();
    H.errorf("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend");
}

JL_DLLEXPORT void fl_show_profile_impl(void)
{
    fe_resolve_host_api();
    H.errorf("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend");
}

JL_DLLEXPORT void fl_clear_profile_impl(void)
{
    fe_resolve_host_api();
    H.errorf("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend");
}
