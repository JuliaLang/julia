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

#ifdef USE_TRACY
#include "tracy/TracyC.h"
#endif

#ifdef __cplusplus
#include <cfenv>
extern "C" {
#else
#include <fenv.h>
#endif

/**
 * @brief Check if Julia is already initialized.
 *
 * Determine if Julia has been previously initialized
 * via `jl_init` or `jl_init_with_image`.
 *
 * @return Returns 1 if Julia is initialized, 0 otherwise.
 */
JL_DLLEXPORT int jl_is_initialized(void)
{
    return jl_main_module != NULL;
}

/**
 * @brief Set Julia command line arguments.
 *
 * Allows setting the command line arguments for Julia,
 * similar to arguments passed in the main function of a C program.
 *
 * @param argc The number of command line arguments.
 * @param argv Array of command line arguments.
 */
JL_DLLEXPORT void jl_set_ARGS(int argc, char **argv)
{
    if (jl_core_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_core_module, jl_symbol("ARGS"));
        if (args == NULL) {
            jl_value_t *vecstr = jl_apply_array_type((jl_value_t*)jl_string_type, 1);
            args = jl_alloc_array_1d(vecstr, 0);
            JL_GC_PUSH1(&args);
            jl_set_const(jl_core_module, jl_symbol("ARGS"), (jl_value_t*)args);
            JL_GC_POP();
        }
        assert(jl_array_nrows(args) == 0);
        jl_array_grow_end(args, argc);
        int i;
        for (i = 0; i < argc; i++) {
            jl_value_t *s = (jl_value_t*)jl_cstr_to_string(argv[i]);
            jl_array_ptr_set(args, i, s);
        }
    }
}

/**
 * @brief Initialize Julia with a specified system image file.
 *
 * Initializes Julia by specifying the usr/bin directory where the Julia binary is
 * and the path of a system image file (*.so). If the julia_bindir is NULL, the function
 * attempts to guess the directory. The image_path is interpreted as a path to the system image
 * file. A non-absolute path for the system image is considered relative to julia_bindir, or
 * relative to the default Julia home directory. The default system image is typically
 * something like ../lib/julia/sys.so.
 *
 * @param julia_bindir The usr/bin directory where the Julia binary is located, or NULL to guess.
 * @param image_path The path of a system image file (*.so). Interpreted as relative to julia_bindir
 *                   or the default Julia home directory if not an absolute path.
 */
JL_DLLEXPORT void jl_init_with_image(const char *julia_bindir,
                                     const char *image_path)
{
    if (jl_is_initialized())
        return;
    libsupport_init();
    jl_options.julia_bindir = julia_bindir;
    if (image_path != NULL)
        jl_options.image_file = image_path;
    else
        jl_options.image_file = jl_get_default_sysimg_path();
    julia_init(JL_IMAGE_JULIA_HOME);
    jl_exception_clear();
}

/**
 * @brief Initialize the Julia runtime.
 *
 * Initializes the Julia runtime without any specific system image.
 * It must be called before any other Julia API functions.
 */
JL_DLLEXPORT void jl_init(void)
{
    char *libbindir = NULL;
#ifdef _OS_WINDOWS_
    libbindir = strdup(jl_get_libdir());
#else
    (void)asprintf(&libbindir, "%s" PATHSEPSTRING ".." PATHSEPSTRING "%s", jl_get_libdir(), "bin");
#endif
    if (!libbindir) {
        printf("jl_init unable to find libjulia!\n");
        abort();
    }
    jl_init_with_image(libbindir, jl_get_default_sysimg_path());
    free(libbindir);
}

static void _jl_exception_clear(jl_task_t *ct) JL_NOTSAFEPOINT
{
    ct->ptls->previous_exception = NULL;
}

/**
 * @brief Evaluate a Julia expression from a string.
 *
 * @param str A C string containing the Julia expression to be evaluated.
 * @return A pointer to `jl_value_t` representing the result of the evaluation.
 *         Returns `NULL` if an error occurs during parsing or evaluation.
 */
JL_DLLEXPORT jl_value_t *jl_eval_string(const char *str)
{
    jl_value_t *r;
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        const char filename[] = "none";
        jl_value_t *ast = jl_parse_all(str, strlen(str),
                filename, strlen(filename), 1);
        JL_GC_PUSH1(&ast);
        r = jl_toplevel_eval_in(jl_main_module, ast);
        JL_GC_POP();
        _jl_exception_clear(ct);
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        r = NULL;
    }
    return r;
}

/**
 * @brief Get the current exception in the Julia context.
 *
 * @return A pointer to `jl_value_t` representing the current exception.
 *         Returns `NULL` if no exception is currently thrown.
 */
JL_DLLEXPORT jl_value_t *jl_current_exception(jl_task_t *ct) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
{
    jl_excstack_t *s = ct->excstack;
    return s && s->top != 0 ? jl_excstack_exception(s, s->top) : jl_nothing;
}

/**
 * @brief Check if an exception has occurred in the Julia context.
 *
 * @return A pointer to `jl_value_t` representing the exception that occurred.
 *         Returns `NULL` if no exception has occurred.
 */
JL_DLLEXPORT jl_value_t *jl_exception_occurred(void)
{
    return jl_current_task->ptls->previous_exception;
}

/**
 * @brief Clear the current exception in the Julia context.
 *
 */
JL_DLLEXPORT void jl_exception_clear(void)
{
    _jl_exception_clear(jl_current_task);
}

/**
 * @brief Get the type name of a Julia value.
 *
 * @param v A pointer to `jl_value_t` representing the Julia value.
 * @return A C string containing the name of the type.
 */
JL_DLLEXPORT const char *jl_typename_str(jl_value_t *v)
{
    if (!jl_is_datatype(v))
        return NULL;
    return jl_symbol_name(((jl_datatype_t*)v)->name->name);
}

/**
 * @brief Get the string representation of a Julia value's type.
 *
 * @param v A pointer to `jl_value_t` representing the Julia value.
 * @return A C string describing the type of the value.
 */
JL_DLLEXPORT const char *jl_typeof_str(jl_value_t *v)
{
    return jl_typename_str((jl_value_t*)jl_typeof(v));
}

/**
 * @brief Get the element type of a Julia array.
 *
 * @param a A pointer to `jl_value_t` representing the Julia array.
 * @return A pointer to the type of the array elements.
 */
JL_DLLEXPORT void *jl_array_eltype(jl_value_t *a)
{
    return jl_tparam0(jl_typeof(a));
}

/**
 * @brief Get the number of dimensions of a Julia array.
 *
 * Returns the rank (number of dimensions) of a Julia array.
 *
 * @param a A pointer to `jl_value_t` representing the Julia array.
 * @return An integer representing the number of dimensions of the array.
 */
JL_DLLEXPORT int jl_array_rank(jl_value_t *a)
{
    return jl_array_ndims(a);
}

/**
 * @brief Get the size of a specific dimension of a Julia array.
 *
 * Returns the size (number of elements) of a specific dimension
 * of a Julia array.
 *
 * @param a A pointer to `jl_array_t` representing the Julia array.
 * @param d The dimension for which the size is requested.
 * @return The size of the specified dimension of the array.
 */
JL_DLLEXPORT size_t jl_array_size(jl_array_t *a, int d)
{
    // n.b this functions only use was to violate the vector abstraction, so we have to continue to emulate that
    if (d >= jl_array_ndims(a))
        return a->ref.mem->length;
    return jl_array_dim(a, d);
}

/**
 * @brief Get the C string pointer from a Julia string.
 *
 * @param s A pointer to `jl_value_t` representing the Julia string.
 * @return A C string pointer containing the contents of the Julia string.
 */
JL_DLLEXPORT const char *jl_string_ptr(jl_value_t *s)
{
    return jl_string_data(s);
}

/**
 * @brief Call a Julia function with a specified number of arguments.
 *
 * @param f A pointer to `jl_function_t` representing the Julia function to call.
 * @param args An array of pointers to `jl_value_t` representing the arguments.
 * @param nargs The number of arguments in the array.
 * @return A pointer to `jl_value_t` representing the result of the function call.
 */
JL_DLLEXPORT jl_value_t *jl_call(jl_function_t *f, jl_value_t **args, uint32_t nargs)
{
    jl_value_t *v;
    jl_task_t *ct = jl_current_task;
    nargs++; // add f to args
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, nargs);
        argv[0] = (jl_value_t*)f;
        for (int i = 1; i < nargs; i++)
            argv[i] = args[i - 1];
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        v = jl_apply(argv, nargs);
        ct->world_age = last_age;
        JL_GC_POP();
        _jl_exception_clear(ct);
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        v = NULL;
    }
    return v;
}

/**
 * @brief Call a Julia function with no arguments.
 *
 * A specialized case of `jl_call` for simpler scenarios.
 *
 * @param f A pointer to `jl_function_t` representing the Julia function to call.
 * @return A pointer to `jl_value_t` representing the result of the function call.
 */
JL_DLLEXPORT jl_value_t *jl_call0(jl_function_t *f)
{
    jl_value_t *v;
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        JL_GC_PUSH1(&f);
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        v = jl_apply_generic(f, NULL, 0);
        ct->world_age = last_age;
        JL_GC_POP();
        _jl_exception_clear(ct);
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        v = NULL;
    }
    return v;
}

/**
 * @brief Call a Julia function with one argument.
 *
 * A specialized case of `jl_call` for simpler scenarios.
 *
 * @param f A pointer to `jl_function_t` representing the Julia function to call.
 * @param a A pointer to `jl_value_t` representing the argument to the function.
 * @return A pointer to `jl_value_t` representing the result of the function call.
 */
JL_DLLEXPORT jl_value_t *jl_call1(jl_function_t *f, jl_value_t *a)
{
    jl_value_t *v;
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 2);
        argv[0] = f;
        argv[1] = a;
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        v = jl_apply(argv, 2);
        ct->world_age = last_age;
        JL_GC_POP();
        _jl_exception_clear(ct);
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        v = NULL;
    }
    return v;
}

/**
 * @brief Call a Julia function with two arguments.
 *
 * A specialized case of `jl_call` for simpler scenarios.
 *
 * @param f A pointer to `jl_function_t` representing the Julia function to call.
 * @param a A pointer to `jl_value_t` representing the first argument.
 * @param b A pointer to `jl_value_t` representing the second argument.
 * @return A pointer to `jl_value_t` representing the result of the function call.
 */
JL_DLLEXPORT jl_value_t *jl_call2(jl_function_t *f, jl_value_t *a, jl_value_t *b)
{
    jl_value_t *v;
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 3);
        argv[0] = f;
        argv[1] = a;
        argv[2] = b;
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        v = jl_apply(argv, 3);
        ct->world_age = last_age;
        JL_GC_POP();
        _jl_exception_clear(ct);
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        v = NULL;
    }
    return v;
}

/**
 * @brief Call a Julia function with three arguments.
 *
 * A specialized case of `jl_call` for simpler scenarios.
 *
 * @param f A pointer to `jl_function_t` representing the Julia function to call.
 * @param a A pointer to `jl_value_t` representing the first argument.
 * @param b A pointer to `jl_value_t` representing the second argument.
 * @param c A pointer to `jl_value_t` representing the third argument.
 * @return A pointer to `jl_value_t` representing the result of the function call.
 */
JL_DLLEXPORT jl_value_t *jl_call3(jl_function_t *f, jl_value_t *a,
                                  jl_value_t *b, jl_value_t *c)
{
    jl_value_t *v;
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 4);
        argv[0] = f;
        argv[1] = a;
        argv[2] = b;
        argv[3] = c;
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        v = jl_apply(argv, 4);
        ct->world_age = last_age;
        JL_GC_POP();
        _jl_exception_clear(ct);
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        v = NULL;
    }
    return v;
}

/**
 * @brief Call a Julia function with three arguments.
 *
 * A specialized case of `jl_call` for simpler scenarios.
 *
 * @param f A pointer to `jl_function_t` representing the Julia function to call.
 * @param a A pointer to `jl_value_t` representing the first argument.
 * @param b A pointer to `jl_value_t` representing the second argument.
 * @param c A pointer to `jl_value_t` representing the third argument.
 * @param d A pointer to `jl_value_t` representing the fourth argument.
 * @return A pointer to `jl_value_t` representing the result of the function call.
 */
JL_DLLEXPORT jl_value_t *jl_call4(jl_function_t *f, jl_value_t *a,
                                  jl_value_t *b, jl_value_t *c,
                                  jl_value_t *d)
{
    jl_value_t *v;
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 5);
        argv[0] = f;
        argv[1] = a;
        argv[2] = b;
        argv[3] = c;
        argv[4] = d;
        size_t last_age = ct->world_age;
        ct->world_age = jl_get_world_counter();
        v = jl_apply(argv, 5);
        ct->world_age = last_age;
        JL_GC_POP();
        _jl_exception_clear(ct);
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        v = NULL;
    }
    return v;
}

/**
 * @brief Get a field from a Julia object.
 *
 * @param o A pointer to `jl_value_t` representing the Julia object.
 * @param fld A C string representing the name of the field to retrieve.
 * @return A pointer to `jl_value_t` representing the value of the field.
 */
JL_DLLEXPORT jl_value_t *jl_get_field(jl_value_t *o, const char *fld)
{
    jl_value_t *v;
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        jl_value_t *s = (jl_value_t*)jl_symbol(fld);
        int i = jl_field_index((jl_datatype_t*)jl_typeof(o), (jl_sym_t*)s, 1);
        v = jl_get_nth_field(o, i);
        jl_exception_clear();
    }
    JL_CATCH {
        ct->ptls->previous_exception = jl_current_exception(ct);
        v = NULL;
    }
    return v;
}

/**
 * @brief Begin an atomic signal-protected region.
 *
 * Marks the start of a region of code that should be protected
 * from interruption by asynchronous signals.
 */
JL_DLLEXPORT void jl_sigatomic_begin(void)
{
    JL_SIGATOMIC_BEGIN();
}

/**
 * @brief End an atomic signal-protected region.
 *
 * Marks the end of a region of code protected from asynchronous signals.
 * It should be used in conjunction with `jl_sigatomic_begin` to define signal-protected regions.
 */
JL_DLLEXPORT void jl_sigatomic_end(void)
{
    jl_task_t *ct = jl_current_task;
    if (ct->ptls->defer_signal == 0)
        jl_error("sigatomic_end called in non-sigatomic region");
    JL_SIGATOMIC_END();
}

/**
 * @brief Check if Julia is running in debug build mode.
 *
 * @return Returns 1 if Julia is in debug build mode, 0 otherwise.
 */
JL_DLLEXPORT int jl_is_debugbuild(void) JL_NOTSAFEPOINT
{
#ifdef JL_DEBUG_BUILD
    return 1;
#else
    return 0;
#endif
}

/**
 * @brief Check if Julia has been build with assertions enabled.
 *
 * @return Returns 1 if assertions are enabled, 0 otherwise.
 */
JL_DLLEXPORT int8_t jl_is_assertsbuild(void) JL_NOTSAFEPOINT {
#ifndef JL_NDEBUG
    return 1;
#else
    return 0;
#endif
}

/**
 * @brief Check if Julia's memory debugging is enabled.
 *
 * @return Returns 1 if memory debugging is enabled, 0 otherwise.
 */
JL_DLLEXPORT int8_t jl_is_memdebug(void) JL_NOTSAFEPOINT {
#ifdef MEMDEBUG
    return 1;
#else
    return 0;
#endif
}

/**
 * @brief Get the directory path of the Julia binary.
 *
 * @return A pointer to `jl_value_t` representing the directory path as a Julia string.
 */
JL_DLLEXPORT jl_value_t *jl_get_julia_bindir(void)
{
    return jl_cstr_to_string(jl_options.julia_bindir);
}

/**
 * @brief Get the path to the Julia binary.
 *
 * @return A pointer to `jl_value_t` representing the full path as a Julia string.
 */
JL_DLLEXPORT jl_value_t *jl_get_julia_bin(void)
{
    return jl_cstr_to_string(jl_options.julia_bin);
}

/**
 * @brief Get the path to the Julia system image file.
 *
 * @return A pointer to `jl_value_t` representing the system image file path as a Julia string.
 */
JL_DLLEXPORT jl_value_t *jl_get_image_file(void)
{
    return jl_cstr_to_string(jl_options.image_file);
}

/**
 * @brief Get the major version number of Julia.
 *
 * @return The major version number as an integer.
 */
JL_DLLEXPORT int jl_ver_major(void)
{
    return JULIA_VERSION_MAJOR;
}

/**
 * @brief Get the minor version number of Julia.
 *
 * @return The minor version number as an integer.
 */
JL_DLLEXPORT int jl_ver_minor(void)
{
    return JULIA_VERSION_MINOR;
}

/**
 * @brief Get the patch version number of Julia.
 *
 * @return The patch version number as an integer.
 */
JL_DLLEXPORT int jl_ver_patch(void)
{
    return JULIA_VERSION_PATCH;
}

/**
 * @brief Check if the current Julia version is a release version.
 *
 * @return Returns 1 if it is a release version, 0 otherwise.
 */
JL_DLLEXPORT int jl_ver_is_release(void)
{
    return JULIA_VERSION_IS_RELEASE;
}

/**
 * @brief Get the Julia version as a string.
 *
 * @return A C string containing the version information.
 */
JL_DLLEXPORT const char *jl_ver_string(void)
{
   return JULIA_VERSION_STRING;
}

/**
 * @brief Convert a Julia value to a tagged value.
 *
 * Converts a Julia value into its corresponding tagged value representation.
 * Tagged values include additional metadata used internally by the Julia runtime.
 *
 * @param v A pointer to `jl_value_t` representing the Julia value.
 * @return A pointer to `jl_taggedvalue_t` representing the tagged value.
 */
JL_DLLEXPORT jl_taggedvalue_t *(jl_astaggedvalue)(jl_value_t *v)
{
    return jl_astaggedvalue(v);
}

/**
 * @brief Convert a tagged value back to a Julia value.
 *
 * Converts a tagged value back into its original Julia value.
 * It's the inverse operation of `jl_astaggedvalue`.
 *
 * @param v A pointer to `jl_taggedvalue_t` representing the tagged value.
 * @return A pointer to `jl_value_t` representing the original Julia value.
 */
JL_DLLEXPORT jl_value_t *(jl_valueof)(jl_taggedvalue_t *v)
{
    return jl_valueof(v);
}

/**
 * @brief Get the type of a Julia value.
 *
 * @param v A pointer to `jl_value_t` representing the Julia value.
 * @return A pointer to `jl_value_t` representing the type of the value.
 */
JL_DLLEXPORT jl_value_t *(jl_typeof)(jl_value_t *v)
{
    return jl_typeof(v);
}

/**
 * @brief Get the field types of a Julia value.
 *
 * @param v A pointer to `jl_value_t` representing the Julia value.
 * @return A pointer to `jl_value_t` representing the field types.
 */
JL_DLLEXPORT jl_value_t *(jl_get_fieldtypes)(jl_value_t *v)
{
    return (jl_value_t*)jl_get_fieldtypes((jl_datatype_t*)v);
}

/**
 * @brief Check equality of two Julia values.
 *
 * @param a A pointer to `jl_value_t` representing the first Julia value.
 * @param b A pointer to `jl_value_t` representing the second Julia value.
 * @return Returns 1 if the values are equal, 0 otherwise.
 */
JL_DLLEXPORT int ijl_egal(jl_value_t *a, jl_value_t *b)
{
    return jl_egal(a, b);
}


#ifndef __clang_gcanalyzer__
/**
 * @brief Enter a state where concurrent garbage collection (GC) is considered unsafe.
 *
 * Marks the beginning of a code region where garbage collection operations are unsafe.
 * Used to make it legal to access GC-managed state (almost anything)
 *
 * @return An `int8_t` state value representing the previous GC state.
 */
JL_DLLEXPORT int8_t (jl_gc_unsafe_enter)(void)
{
    jl_task_t *ct = jl_current_task;
    return jl_gc_unsafe_enter(ct->ptls);
}

/**
 * @brief Leave the state where garbage collection is considered unsafe.
 *
 * Ends a code region where garbage collection was marked as unsafe.
 * It restores the previous GC state using the state value returned by `jl_gc_unsafe_enter`.
 *
 * @param state The state value returned by `jl_gc_unsafe_enter` to restore the previous GC state.
 */
JL_DLLEXPORT void (jl_gc_unsafe_leave)(int8_t state)
{
    jl_task_t *ct = jl_current_task;
    jl_gc_unsafe_leave(ct->ptls, state);
}

/**
 * @brief Enter a state where garbage collection (GC) is considered safe.
 *
 * Marks the beginning of a code region where garbage collection operations are safe.
 * Used to enable GC in sections of code where it was previously marked as unsafe.
 *
 * @return An `int8_t` state value representing the previous GC state.
 */
JL_DLLEXPORT int8_t (jl_gc_safe_enter)(void)
{
    jl_task_t *ct = jl_current_task;
    return jl_gc_safe_enter(ct->ptls);
}

/**
 * @brief Leave the state where garbage collection is considered safe.
 *
 * Ends a code region where garbage collection was marked as safe.
 * It restores the previous GC state using the state value returned by `jl_gc_safe_enter`.
 *
 * @param state The state value returned by `jl_gc_safe_enter` to restore the previous GC state.
 */
JL_DLLEXPORT void (jl_gc_safe_leave)(int8_t state)
{
    jl_task_t *ct = jl_current_task;
    jl_gc_safe_leave(ct->ptls, state);
}
#endif

/**
 * @brief Trigger a garbage collection safepoint in a GC-unsafe region.
 *
 * Triggers a safepoint for garbage collection. Used to
 * ensure that the garbage collector can run at specific points in the code,
 * particularly in long-running operations or loops.
 */
JL_DLLEXPORT void jl_gc_safepoint(void)
{
    jl_task_t *ct = jl_current_task;
    jl_gc_safepoint_(ct->ptls);
}

/**
 * @brief Pause CPU execution for a brief moment.
 *
 * Used to pause the CPU briefly, typically to reduce power consumption
 * or manage CPU resources more effectively in a tight loop or busy wait scenario.
 */
JL_DLLEXPORT void (jl_cpu_pause)(void)
{
    jl_cpu_pause();
}

/**
 * @brief Suspend CPU execution.
 *
 * Suspends CPU execution until a specific condition or event occurs.
 */
JL_DLLEXPORT void (jl_cpu_suspend)(void)
{
    jl_cpu_suspend();
}

/**
 * @brief Wake the CPU from a suspended state.
 *
 * Used to resume CPU execution after it has been suspended using `jl_cpu_suspend`.
 */
JL_DLLEXPORT void (jl_cpu_wake)(void)
{
    jl_cpu_wake();
}

/**
 * @brief Enable cumulative compile timing.
 */
JL_DLLEXPORT void jl_cumulative_compile_timing_enable(void)
{
    // Increment the flag to allow reentrant callers to `@time`.
    jl_atomic_fetch_add(&jl_measure_compile_time_enabled, 1);
}

/**
 * @brief Disable cumulative compile timing.
 */
JL_DLLEXPORT void jl_cumulative_compile_timing_disable(void)
{
    // Decrement the flag when done measuring, allowing other callers to continue measuring.
    jl_atomic_fetch_add(&jl_measure_compile_time_enabled, -1);
}

/**
 * @brief Get the cumulative compilation time in nanoseconds.
 *
 * @return The cumulative compilation time in nanoseconds.
 */
JL_DLLEXPORT uint64_t jl_cumulative_compile_time_ns(void)
{
    return jl_atomic_load_relaxed(&jl_cumulative_compile_time);
}

/**
 * @brief Get the cumulative recompilation time in nanoseconds.
 *
 * @return The cumulative recompilation time in nanoseconds.
 */
JL_DLLEXPORT uint64_t jl_cumulative_recompile_time_ns(void)
{
    return jl_atomic_load_relaxed(&jl_cumulative_recompile_time);
}

/**
 * @brief Enable per-task timing.
 */
JL_DLLEXPORT void jl_task_metrics_enable(void)
{
    // Increment the flag to allow reentrant callers.
    jl_atomic_fetch_add(&jl_task_metrics_enabled, 1);
}

/**
 * @brief Disable per-task timing.
 */
JL_DLLEXPORT void jl_task_metrics_disable(void)
{
    // Prevent decrementing the counter below zero
    uint8_t enabled = jl_atomic_load_relaxed(&jl_task_metrics_enabled);
    while (enabled > 0) {
        if (jl_atomic_cmpswap(&jl_task_metrics_enabled, &enabled, enabled-1))
            break;
    }
}

/**
 * @brief Retrieve floating-point environment constants.
 *
 * Populates an array with constants related to the floating-point environment,
 * such as rounding modes and exception flags.
 *
 * @param ret An array of integers to be populated with floating-point environment constants.
 */
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

// TODO: Windows binaries currently load msvcrt which doesn't have these C99 functions.
//       the mingw compiler ships additional definitions, but only for use in C code.
//       remove this when we switch to ucrt, make the version in openlibm portable,
//       or figure out how to reexport the defs from libmingwex (see JuliaLang/julia#38466).
JL_DLLEXPORT int jl_get_fenv_rounding(void)
{
    return fegetround();
}

/**
 * @brief Set the floating-point rounding mode.
 *
 * @param i An integer representing the desired floating-point rounding mode.
          See also "floating-point rounding" macros in `<fenv.h>`.
 * @return An integer indicating the success or failure of setting the rounding mode.
 */
JL_DLLEXPORT int jl_set_fenv_rounding(int i)
{
    return fesetround(i);
}

static int exec_program(char *program)
{
    jl_task_t *ct = jl_current_task;
    JL_TRY {
        jl_load(jl_main_module, program);
    }
    JL_CATCH {
        // TODO: It is possible for this output to be mangled due to `jl_print_backtrace`
        //       printing directly to STDERR_FILENO.
        int shown_err = 0;
        jl_printf(JL_STDERR, "error during bootstrap:\n");
        jl_value_t *exc = jl_current_exception(ct);
        jl_value_t *showf = jl_base_module ? jl_get_function(jl_base_module, "show") : NULL;
        if (showf) {
            jl_value_t *errs = jl_stderr_obj();
            if (errs) {
                if (jl_call2(showf, errs, exc)) {
                    jl_printf(JL_STDERR, "\n");
                    shown_err = 1;
                }
            }
        }
        if (!shown_err) {
            jl_static_show((JL_STREAM*)STDERR_FILENO, exc);
            jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
        }
        jl_print_backtrace(); // written to STDERR_FILENO
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
        return 1;
    }
    return 0;
}

static NOINLINE int true_main(int argc, char *argv[])
{
    jl_set_ARGS(argc, argv);


    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_get_world_counter();

    jl_function_t *start_client = jl_base_module ?
        (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("_start")) : NULL;

    if (start_client) {
        int ret = 1;
        JL_TRY {
            jl_value_t *r = jl_apply(&start_client, 1);
            if (jl_typeof(r) != (jl_value_t*)jl_int32_type)
                jl_type_error("typeassert", (jl_value_t*)jl_int32_type, r);
            ret = jl_unbox_int32(r);
        }
        JL_CATCH {
            jl_no_exc_handler(jl_current_exception(ct), ct);
        }
        ct->world_age = last_age;
        return ret;
    }
    ct->world_age = last_age;

    // run program if specified, otherwise enter REPL
    if (argc > 0) {
        if (strcmp(argv[0], "-")) {
            return exec_program(argv[0]);
        }
    }

    jl_printf(JL_STDOUT, "WARNING: Base._start not defined, falling back to economy mode repl.\n");
    if (!jl_errorexception_type)
        jl_printf(JL_STDOUT, "WARNING: jl_errorexception_type not defined; any errors will be fatal.\n");

    while (!ios_eof(ios_stdin)) {
        char *volatile line = NULL;
        JL_TRY {
            ios_puts("\njulia> ", ios_stdout);
            ios_flush(ios_stdout);
            line = ios_readline(ios_stdin);
            jl_value_t *val = (jl_value_t*)jl_eval_string(line);
            JL_GC_PUSH1(&val);
            if (jl_exception_occurred()) {
                jl_printf(JL_STDERR, "error during run:\n");
                jl_static_show(JL_STDERR, jl_exception_occurred());
                jl_exception_clear();
            }
            else if (val) {
                jl_static_show(JL_STDOUT, val);
            }
            JL_GC_POP();
            jl_printf(JL_STDOUT, "\n");
            free(line);
            line = NULL;
            jl_process_events();
        }
        JL_CATCH {
            if (line) {
                free(line);
                line = NULL;
            }
            jl_printf((JL_STREAM*)STDERR_FILENO, "\nparser error:\n");
            jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception(ct));
            jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
            jl_print_backtrace(); // written to STDERR_FILENO
        }
    }
    return 0;
}

static void lock_low32(void)
{
#if defined(_OS_WINDOWS_) && defined(_P64) && defined(JL_DEBUG_BUILD)
    // Prevent usage of the 32-bit address space on Win64, to catch pointer cast errors.
    char *const max32addr = (char*)0xffffffffL;
    SYSTEM_INFO info;
    MEMORY_BASIC_INFORMATION meminfo;
    GetNativeSystemInfo(&info);
    memset(&meminfo, 0, sizeof(meminfo));
    meminfo.BaseAddress = info.lpMinimumApplicationAddress;
    while ((char*)meminfo.BaseAddress < max32addr) {
        size_t nbytes = VirtualQuery(meminfo.BaseAddress, &meminfo, sizeof(meminfo));
        assert(nbytes == sizeof(meminfo));
        if (meminfo.State == MEM_FREE) { // reserve all free pages in the first 4GB of memory
            char *first = (char*)meminfo.BaseAddress;
            char *last = first + meminfo.RegionSize;
            if (last > max32addr)
                last = max32addr;
            // adjust first up to the first allocation granularity boundary
            // adjust last down to the last allocation granularity boundary
            first = (char*)(((long long)first + info.dwAllocationGranularity - 1) & ~(info.dwAllocationGranularity - 1));
            last = (char*)((long long)last & ~(info.dwAllocationGranularity - 1));
            if (last != first) {
                void *p = VirtualAlloc(first, last - first, MEM_RESERVE, PAGE_NOACCESS); // reserve all memory in between
                if ((char*)p != first)
                    // Wine and Windows10 seem to have issues with reporting memory access information correctly
                    // so we sometimes end up with unexpected results - this is just ignore those and continue
                    // this is just a debugging aid to help find accidental pointer truncation anyways,
                    // so it is not critical
                    VirtualFree(p, 0, MEM_RELEASE);
            }
        }
        meminfo.BaseAddress = (void*)((char*)meminfo.BaseAddress + meminfo.RegionSize);
    }
#endif
    return;
}

// Actual definition in `ast.c`
void jl_lisp_prompt(void);

#ifdef _OS_LINUX_
static void rr_detach_teleport(void) {
#define RR_CALL_BASE 1000
#define SYS_rrcall_detach_teleport (RR_CALL_BASE + 9)
    int err = syscall(SYS_rrcall_detach_teleport, 0, 0, 0, 0, 0, 0);
    if (err < 0 || jl_running_under_rr(1)) {
        jl_error("Failed to detach from rr session");
    }
}
#endif

/**
 * @brief Entry point for the Julia REPL (Read-Eval-Print Loop).
 *
 * @param argc The number of command-line arguments.
 * @param argv Array of command-line arguments.
 * @return An integer indicating the exit status of the REPL session.
 */
JL_DLLEXPORT int jl_repl_entrypoint(int argc, char *argv[])
{
#ifdef USE_TRACY
    if (getenv("JULIA_WAIT_FOR_TRACY"))
        while (!TracyCIsConnected) jl_cpu_pause(); // Wait for connection
#endif

    // no-op on Windows, note that the caller must have already converted
    // from `wchar_t` to `UTF-8` already if we're running on Windows.
    uv_setup_args(argc, argv);

    // No-op on non-windows
    lock_low32();

    libsupport_init();
    int lisp_prompt = (argc >= 2 && strcmp((char*)argv[1],"--lisp") == 0);
    if (lisp_prompt) {
        memmove(&argv[1], &argv[2], (argc-2)*sizeof(void*));
        argc--;
    }
    char **new_argv = argv;
    jl_parse_opts(&argc, (char***)&new_argv);

    // The parent process requested that we detach from the rr session.
    // N.B.: In a perfect world, we would only do this for the portion of
    // the execution where we actually need to exclude rr (e.g. because we're
    // testing for the absence of a memory-model-dependent bug).
    if (jl_options.rr_detach && jl_running_under_rr(0)) {
#ifdef _OS_LINUX_
        rr_detach_teleport();
        execv("/proc/self/exe", argv);
#endif
        jl_error("Failed to self-execute");
    }

    julia_init(jl_options.image_file_specified ? JL_IMAGE_CWD : JL_IMAGE_JULIA_HOME);
    if (lisp_prompt) {
        jl_current_task->world_age = jl_get_world_counter();
        jl_lisp_prompt();
        return 0;
    }
    int ret = true_main(argc, (char**)new_argv);
    jl_atexit_hook(ret);
    return ret;
}

#ifdef __cplusplus
}
#endif
