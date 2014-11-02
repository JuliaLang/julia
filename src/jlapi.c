/*
  jlapi.c
  miscellaneous functions for users of libjulia.so, to handle initialization
  and the style of use where julia is not in control most of the time.
*/
#include "platform.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
DLLEXPORT char * __cdecl dirname(char *);
DLLEXPORT char * __cdecl basename(char *);
#else
#include <libgen.h>
#endif

DLLEXPORT char *jl_locate_sysimg(char *jlhome, char *imgpath)
{
    if (jlhome == NULL) {
        char *julia_path = (char*)malloc(512);
        size_t path_size = 512;
        uv_exepath(julia_path, &path_size);
        julia_home = strdup(dirname(julia_path));
        free(julia_path);
    }
    else {
        julia_home = jlhome;
    }
    char path[512];
    snprintf(path, sizeof(path), "%s%s%s",
             julia_home, PATHSEPSTRING, imgpath);
    return strdup(path);
}

DLLEXPORT void *jl_eval_string(char *str);

int jl_is_initialized(void) { return jl_main_module!=NULL; }

// First argument is the usr/lib directory where libjulia is, or NULL to guess.
// if that doesn't work, try the full path to the "lib" directory that
// contains lib/julia/sys.ji
// Second argument is the path of a system image file (*.ji) relative to the
// first argument path, or relative to the default julia home dir. The default
// is something like ../lib/julia/sys.ji
DLLEXPORT void jl_init_with_image(char *julia_home_dir, char *image_relative_path)
{
    if (jl_is_initialized()) return;
    libsupport_init();
    if (image_relative_path == NULL)
        image_relative_path = JL_SYSTEM_IMAGE_PATH;
    char *image_file = jl_locate_sysimg(julia_home_dir, image_relative_path);
    julia_init(image_file);
    jl_set_const(jl_core_module, jl_symbol("JULIA_HOME"),
                 jl_cstr_to_string(julia_home));
    jl_module_export(jl_core_module, jl_symbol("JULIA_HOME"));
    jl_eval_string("Base.early_init()");
    jl_eval_string("Base.init_head_sched()");
    jl_eval_string("Base.init_load_path()");
    jl_exception_clear();
}

DLLEXPORT void jl_init(char *julia_home_dir)
{
    jl_init_with_image(julia_home_dir, JL_SYSTEM_IMAGE_PATH);
}

DLLEXPORT void *jl_eval_string(char *str)
{
#ifdef COPY_STACKS
    int outside_task = (jl_root_task->stackbase == NULL);
    if (outside_task) {
        JL_SET_STACK_BASE;
    }
#endif
    jl_value_t *r;
    JL_TRY {
        jl_value_t *ast = jl_parse_input_line(str);
        JL_GC_PUSH1(&ast);
        r = jl_toplevel_eval(ast);
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        //jl_show(jl_stderr_obj(), jl_exception_in_transit);
        r = NULL;
    }
#ifdef COPY_STACKS
    if (outside_task) {
        jl_root_task->stackbase = NULL;
    }
#endif
    return r;
}

DLLEXPORT jl_value_t *jl_exception_occurred(void)
{
    return jl_is_null(jl_exception_in_transit) ? NULL : 
        jl_exception_in_transit;
}

DLLEXPORT void jl_exception_clear(void)
{
    jl_exception_in_transit = (jl_value_t*)jl_null;
}

// get the name of a type as a string
DLLEXPORT const char *jl_typename_str(jl_value_t *v)
{
    if (jl_is_tuple(v))
        return "Tuple";
    return ((jl_datatype_t*)v)->name->name->name;
}

// get the name of typeof(v) as a string
DLLEXPORT const char *jl_typeof_str(jl_value_t *v)
{
    return jl_typename_str((jl_value_t*)jl_typeof(v));
}

DLLEXPORT void *jl_array_eltype(jl_value_t *a)
{
    return jl_tparam0(jl_typeof(a));
}

DLLEXPORT int jl_array_rank(jl_value_t *a)
{
    return jl_array_ndims(a);
}

DLLEXPORT size_t jl_array_size(jl_value_t *a, int d)
{
    return jl_array_dim(a, d);
}

DLLEXPORT void *jl_array_ptr(jl_array_t *a);

DLLEXPORT const char *jl_bytestring_ptr(jl_value_t *s)
{
    return jl_string_data(s);
}

DLLEXPORT jl_value_t *jl_call(jl_function_t *f, jl_value_t **args, int32_t nargs)
{
    jl_value_t *v;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, nargs+1);
        argv[0] = (jl_value_t*) f;
        for(int i=1; i<nargs+1; i++)
            argv[i] = args[i-1];
        v = jl_apply(f, args, nargs);
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        v = NULL;
    }
    return v;
}

DLLEXPORT jl_value_t *jl_call0(jl_function_t *f)
{
    jl_value_t *v;
    JL_TRY {
        JL_GC_PUSH1(&f);
        v = jl_apply(f, NULL, 0);
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        v = NULL;
    }
    return v;
}

DLLEXPORT jl_value_t *jl_call1(jl_function_t *f, jl_value_t *a)
{
    jl_value_t *v;
    JL_TRY {
        JL_GC_PUSH2(&f,&a);
        v = jl_apply(f, &a, 1);
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        v = NULL;
    }
    return v;
}

DLLEXPORT jl_value_t *jl_call2(jl_function_t *f, jl_value_t *a, jl_value_t *b)
{
    jl_value_t *v;
    JL_TRY {
        JL_GC_PUSH3(&f,&a,&b);
        jl_value_t *args[2] = {a,b};
        v = jl_apply(f, args, 2);
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        v = NULL;
    }
    return v;
}

DLLEXPORT jl_value_t *jl_call3(jl_function_t *f, jl_value_t *a, jl_value_t *b, jl_value_t *c)
{
    jl_value_t *v;
    JL_TRY {
        JL_GC_PUSH4(&f,&a,&b,&c);
        jl_value_t *args[3] = {a,b,c};
        v = jl_apply(f, args, 3);
        JL_GC_POP();
        jl_exception_clear();
    }
    JL_CATCH {
        v = NULL;
    }
    return v;
}

DLLEXPORT void jl_yield()
{
    static jl_function_t *yieldfunc = NULL;
    if (yieldfunc == NULL)
        yieldfunc = (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("yield"));
    if (yieldfunc != NULL && jl_is_func(yieldfunc))
        jl_call0(yieldfunc);
}

DLLEXPORT jl_value_t *jl_get_field(jl_value_t *o, char *fld)
{
    jl_value_t *v;
    JL_TRY {
        jl_value_t *s = (jl_value_t*)jl_symbol(fld);
        int i = jl_field_index((jl_datatype_t*)jl_typeof(o), (jl_sym_t*)s, 1);
        v = jl_get_nth_field(o, i);
        jl_exception_clear();
    }
    JL_CATCH {
        v = NULL;
    }
    return v;
}

DLLEXPORT void jl_sigatomic_begin(void)
{
    JL_SIGATOMIC_BEGIN();
}

DLLEXPORT void jl_sigatomic_end(void)
{
    if (jl_defer_signal == 0)
        jl_error("sigatomic_end called in non-sigatomic region");
    JL_SIGATOMIC_END();
}

DLLEXPORT int jl_is_debugbuild(void)
{
#ifdef JL_DEBUG_BUILD
    return 1;
#else
    return 0;
#endif
}

#ifdef __cplusplus
}
#endif
