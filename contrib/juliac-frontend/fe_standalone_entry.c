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
    // slots holding host globals (deref at use; filled by the host loader)
    hv *nothing;
    hv *linenumbernode_type;
    hv *quotenode_type;
    hv *base_module;
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
    H.nothing = (hv *)host_sym("jl_nothing");
    H.linenumbernode_type = (hv *)host_sym("jl_linenumbernode_type");
    H.quotenode_type = (hv *)host_sym("jl_quotenode_type");
    H.base_module = (hv *)host_sym("jl_base_module");
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
    // VersionNumber appears in parser output as the per-module syntax
    // version marker; round-trip it through its string form.
    jl_value_t *vnty = jl_get_global(jl_base_module, jl_symbol("VersionNumber"));
    if (vnty != NULL && t == vnty) {
        jl_value_t *str = jl_call1(jl_get_function(jl_base_module, "string"), v);
        if (str == NULL || !jl_is_string(str))
            H.errorf("libjulia-frontend-standalone: failed to stringify VersionNumber");
        hframe_t f;
        hpush(pg, &f);
        hv hstr = H.pchar_to_string(jl_string_data(str), jl_string_len(str));
        hroot(&f, 0, hstr);
        hv hctor = H.get_global(*H.base_module, H.symbol("VersionNumber"));
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

JL_DLLEXPORT hv jl_frontend_lower_impl(hv expr, hv inmodule, const char *filename,
                                       int line, size_t world, int warn)
{
    (void)expr; (void)inmodule; (void)filename; (void)line; (void)world; (void)warn;
    fe_enter();
    H.errorf("jl_frontend_lower: cross-runtime lowering is not yet implemented "
             "by libjulia-frontend-standalone");
    return NULL; // unreachable
}

JL_DLLEXPORT hv jl_macroexpand_impl(hv expr, hv inmodule, int recursive, int inplace,
                                    int expand_scope)
{
    (void)expr; (void)inmodule; (void)recursive; (void)inplace; (void)expand_scope;
    fe_enter();
    H.errorf("jl_macroexpand: cross-runtime macro expansion is not yet implemented "
             "by libjulia-frontend-standalone");
    return NULL; // unreachable
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
