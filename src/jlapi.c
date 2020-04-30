// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  jlapi.c
  miscellaneous functions for users of libjulia.so, to handle initialization
  and the style of use where julia is not in control most of the time.
*/
#include "platform.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "julia.h"
#include "options.h"
#include "julia_assert.h"
#include "julia_internal.h"

#ifdef __cplusplus
#include <cfenv>
extern "C" {
#else
#include <fenv.h>
#endif

#if defined(_OS_WINDOWS_) && !defined(_COMPILER_GCC_)
JL_DLLEXPORT char * __cdecl dirname(char *);
#else
#include <libgen.h>
#endif
#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif

JL_DLLEXPORT int jl_is_initialized(void)
{
    return jl_main_module != NULL;
}

JL_DLLEXPORT void jl_set_ARGS(int argc, char **argv)
{
    if (jl_core_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
        if (args == NULL) {
            args = jl_alloc_vec_any(0);
            JL_GC_PUSH1(&args);
            jl_set_const(jl_core_module, jl_symbol("ARGS"), (jl_value_t*)args);
            JL_GC_POP();
        }
        assert(jl_array_len(args) == 0);
        jl_array_grow_end(args, argc);
        int i;
        for (i = 0; i < argc; i++) {
            jl_value_t *s = (jl_value_t*)jl_cstr_to_string(argv[i]);
            jl_arrayset(args, s, i);
        }
    }
}

// First argument is the usr/bin directory where the julia binary is, or NULL to guess.
// Second argument is the path of a system image file (*.ji) relative to the
// first argument path, or relative to the default julia home dir.
// The default is something like ../lib/julia/sys.ji
JL_DLLEXPORT void jl_init_with_image(const char *julia_bindir,
                                     const char *image_relative_path)
{
    if (jl_is_initialized())
        return;
    libsupport_init();
    jl_options.julia_bindir = julia_bindir;
    if (image_relative_path != NULL)
        jl_options.image_file = image_relative_path;
    else
        jl_options.image_file = jl_get_default_sysimg_path();
    julia_init(JL_IMAGE_JULIA_HOME);
    jl_exception_clear();
}

JL_DLLEXPORT void jl_init(void)
{
    char *libbindir = NULL;
#ifdef _OS_WINDOWS_
    void *hdl = (void*)jl_load_dynamic_library(NULL, JL_RTLD_DEFAULT, 0);
    if (hdl) {
        char *to_free = (char*)jl_pathname_for_handle(hdl);
        if (to_free) {
            libbindir = strdup(dirname(to_free));
            free(to_free);
        }
    }
#else
    Dl_info dlinfo;
    if (dladdr((void*)jl_init, &dlinfo) != 0 && dlinfo.dli_fname) {
        char *to_free = strdup(dlinfo.dli_fname);
        (void)asprintf(&libbindir, "%s" PATHSEPSTRING ".." PATHSEPSTRING "%s", dirname(to_free), "bin");
        free(to_free);
    }
#endif
    if (!libbindir) {
        printf("jl_init unable to find libjulia!\n");
        abort();
    }
    jl_init_with_image(libbindir, jl_get_default_sysimg_path());
    free(libbindir);
}

JL_DLLEXPORT jl_value_t *jl_eval_string(const char *str)
{
    jl_value_t *r;
    JL_TRY {
        const char filename[] = "none";
        jl_value_t *ast = jl_parse_all(str, strlen(str),
                filename, strlen(filename));
        JL_GC_PUSH1(&ast);
        r = jl_toplevel_eval_in(jl_main_module, ast);
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        jl_get_ptls_states()->previous_exception = jl_current_exception();
        r = NULL;
    }
    return r;
}

JL_DLLEXPORT jl_value_t *jl_current_exception(void) JL_GLOBALLY_ROOTED
{
    jl_excstack_t *s = jl_get_ptls_states()->current_task->excstack;
    return s && s->top != 0 ? jl_excstack_exception(s, s->top) : jl_nothing;
}

JL_DLLEXPORT jl_value_t *jl_exception_occurred(void)
{
    return jl_get_ptls_states()->previous_exception;
}

JL_DLLEXPORT void jl_exception_clear(void)
{
    jl_get_ptls_states()->previous_exception = NULL;
}

// get the name of a type as a string
JL_DLLEXPORT const char *jl_typename_str(jl_value_t *v)
{
    if (!jl_is_datatype(v))
        return NULL;
    return jl_symbol_name(((jl_datatype_t*)v)->name->name);
}

// get the name of typeof(v) as a string
JL_DLLEXPORT const char *jl_typeof_str(jl_value_t *v)
{
    return jl_typename_str((jl_value_t*)jl_typeof(v));
}

JL_DLLEXPORT void *jl_array_eltype(jl_value_t *a)
{
    return jl_tparam0(jl_typeof(a));
}

JL_DLLEXPORT int jl_array_rank(jl_value_t *a)
{
    return jl_array_ndims(a);
}

JL_DLLEXPORT size_t jl_array_size(jl_value_t *a, int d)
{
    return jl_array_dim(a, d);
}

JL_DLLEXPORT const char *jl_string_ptr(jl_value_t *s)
{
    return jl_string_data(s);
}

JL_DLLEXPORT jl_value_t *jl_call(jl_function_t *f, jl_value_t **args, int32_t nargs)
{
    jl_value_t *v;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, nargs+1);
        argv[0] = (jl_value_t*)f;
        for(int i=1; i<nargs+1; i++)
            argv[i] = args[i-1];
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = jl_get_world_counter();
        v = jl_apply(argv, nargs+1);
        jl_get_ptls_states()->world_age = last_age;
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        jl_get_ptls_states()->previous_exception = jl_current_exception();
        v = NULL;
    }
    return v;
}

JL_DLLEXPORT jl_value_t *jl_call0(jl_function_t *f)
{
    jl_value_t *v;
    JL_TRY {
        JL_GC_PUSH1(&f);
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = jl_get_world_counter();
        v = jl_apply(&f, 1);
        jl_get_ptls_states()->world_age = last_age;
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        jl_get_ptls_states()->previous_exception = jl_current_exception();
        v = NULL;
    }
    return v;
}

JL_DLLEXPORT jl_value_t *jl_call1(jl_function_t *f, jl_value_t *a)
{
    jl_value_t *v;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 2);
        argv[0] = f; argv[1] = a;
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = jl_get_world_counter();
        v = jl_apply(argv, 2);
        jl_get_ptls_states()->world_age = last_age;
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        jl_get_ptls_states()->previous_exception = jl_current_exception();
        v = NULL;
    }
    return v;
}

JL_DLLEXPORT jl_value_t *jl_call2(jl_function_t *f, jl_value_t *a, jl_value_t *b)
{
    jl_value_t *v;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 3);
        argv[0] = f; argv[1] = a; argv[2] = b;
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = jl_get_world_counter();
        v = jl_apply(argv, 3);
        jl_get_ptls_states()->world_age = last_age;
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        jl_get_ptls_states()->previous_exception = jl_current_exception();
        v = NULL;
    }
    return v;
}

JL_DLLEXPORT jl_value_t *jl_call3(jl_function_t *f, jl_value_t *a,
                                  jl_value_t *b, jl_value_t *c)
{
    jl_value_t *v;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 4);
        argv[0] = f; argv[1] = a; argv[2] = b; argv[3] = c;
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = jl_get_world_counter();
        v = jl_apply(argv, 4);
        jl_get_ptls_states()->world_age = last_age;
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        jl_get_ptls_states()->previous_exception = jl_current_exception();
        v = NULL;
    }
    return v;
}

JL_DLLEXPORT void jl_yield(void)
{
    static jl_function_t *yieldfunc = NULL;
    if (yieldfunc == NULL)
        yieldfunc = (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("yield"));
    if (yieldfunc != NULL)
        jl_call0(yieldfunc);
}

JL_DLLEXPORT jl_value_t *jl_get_field(jl_value_t *o, const char *fld)
{
    jl_value_t *v;
    JL_TRY {
        jl_value_t *s = (jl_value_t*)jl_symbol(fld);
        int i = jl_field_index((jl_datatype_t*)jl_typeof(o), (jl_sym_t*)s, 1);
        v = jl_get_nth_field(o, i);
        jl_exception_clear();
    }
    JL_CATCH {
        jl_get_ptls_states()->previous_exception = jl_current_exception();
        v = NULL;
    }
    return v;
}

JL_DLLEXPORT void jl_sigatomic_begin(void)
{
    JL_SIGATOMIC_BEGIN();
}

JL_DLLEXPORT void jl_sigatomic_end(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->defer_signal == 0)
        jl_error("sigatomic_end called in non-sigatomic region");
    JL_SIGATOMIC_END();
}

JL_DLLEXPORT int jl_is_debugbuild(void)
{
#ifdef JL_DEBUG_BUILD
    return 1;
#else
    return 0;
#endif
}

JL_DLLEXPORT int8_t jl_is_memdebug(void) {
#ifdef MEMDEBUG
    return 1;
#else
    return 0;
#endif
}

JL_DLLEXPORT jl_value_t *jl_get_julia_bindir(void)
{
    return jl_cstr_to_string(jl_options.julia_bindir);
}

JL_DLLEXPORT jl_value_t *jl_get_julia_bin(void)
{
    return jl_cstr_to_string(jl_options.julia_bin);
}

JL_DLLEXPORT jl_value_t *jl_get_image_file(void)
{
    return jl_cstr_to_string(jl_options.image_file);
}

JL_DLLEXPORT int jl_ver_major(void)
{
    return JULIA_VERSION_MAJOR;
}

JL_DLLEXPORT int jl_ver_minor(void)
{
    return JULIA_VERSION_MINOR;
}

JL_DLLEXPORT int jl_ver_patch(void)
{
    return JULIA_VERSION_PATCH;
}

JL_DLLEXPORT int jl_ver_is_release(void)
{
    return JULIA_VERSION_IS_RELEASE;
}

JL_DLLEXPORT const char *jl_ver_string(void)
{
   return JULIA_VERSION_STRING;
}

// return char* from String field in Base.GIT_VERSION_INFO
static const char *git_info_string(const char *fld)
{
    static jl_value_t *GIT_VERSION_INFO = NULL;
    if (!GIT_VERSION_INFO)
        GIT_VERSION_INFO = jl_get_global(jl_base_module, jl_symbol("GIT_VERSION_INFO"));
    jl_value_t *f = jl_get_field(GIT_VERSION_INFO, fld);
    assert(jl_is_string(f));
    return jl_string_data(f);
}

JL_DLLEXPORT const char *jl_git_branch(void)
{
    static const char *branch = NULL;
    if (!branch) branch = git_info_string("branch");
    return branch;
}

JL_DLLEXPORT const char *jl_git_commit(void)
{
    static const char *commit = NULL;
    if (!commit) commit = git_info_string("commit");
    return commit;
}

// Create function versions of some useful macros
JL_DLLEXPORT jl_taggedvalue_t *(jl_astaggedvalue)(jl_value_t *v)
{
    return jl_astaggedvalue(v);
}

JL_DLLEXPORT jl_value_t *(jl_valueof)(jl_taggedvalue_t *v)
{
    return jl_valueof(v);
}

JL_DLLEXPORT jl_value_t *(jl_typeof)(jl_value_t *v)
{
    return jl_typeof(v);
}

JL_DLLEXPORT jl_value_t *(jl_get_fieldtypes)(jl_value_t *v)
{
    return (jl_value_t*)jl_get_fieldtypes((jl_datatype_t*)v);
}


#ifndef __clang_analyzer__
JL_DLLEXPORT int8_t (jl_gc_unsafe_enter)(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_unsafe_enter(ptls);
}

JL_DLLEXPORT void (jl_gc_unsafe_leave)(int8_t state)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_gc_unsafe_leave(ptls, state);
}

JL_DLLEXPORT int8_t (jl_gc_safe_enter)(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_safe_enter(ptls);
}

JL_DLLEXPORT void (jl_gc_safe_leave)(int8_t state)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_gc_safe_leave(ptls, state);
}
#endif

JL_DLLEXPORT void (jl_gc_safepoint)(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_gc_safepoint_(ptls);
}

JL_DLLEXPORT void (jl_cpu_pause)(void)
{
    jl_cpu_pause();
}

JL_DLLEXPORT void (jl_cpu_wake)(void)
{
    jl_cpu_wake();
}

JL_DLLEXPORT void jl_get_fenv_consts(int *ret)
{
    ret[0] = FE_INEXACT;
    ret[1] = FE_UNDERFLOW;
    ret[2] = FE_OVERFLOW;
    ret[3] = FE_DIVBYZERO;
    ret[4] = FE_INVALID;
    ret[5] = FE_TONEAREST;
    ret[6] = FE_UPWARD;
    ret[7] = FE_DOWNWARD;
    ret[8] = FE_TOWARDZERO;
}

#ifdef __cplusplus
}
#endif
