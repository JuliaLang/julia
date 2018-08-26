#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Extracted from src/support/platform.h
#if defined(_WIN32) || defined(_WIN64)
#define _OS_WINDOWS_
#endif

#ifdef _OS_WINDOWS_
#include <windows.h>
#else
#include <dlfcn.h>
#endif

// Load a minimal subset of the functions from julia.h dynamically and
// masquerade the function pointers to look like the original
// functions.

// We could get the definitions of these structs from
// `#include "julia.h"`, but we only need to declare them and
// including julia.h would interfere with the masquerading trick.
typedef struct _jl_value_t jl_value_t;
typedef struct _jl_sym_t jl_sym_t;
typedef struct _jl_module_t jl_module_t;

// Declare pointers to the functions we need to load from libjulia.
// Obviously these signatures must match the corresponding functions
// in the julia.h that libjulia was built from.
static void (*p_jl_init)(void);
static int (*p_jl_is_initialized)(void);
static void (*p_jl_atexit_hook)(int);
static jl_value_t *(*p_jl_eval_string)(const char *);
static jl_sym_t *(*p_jl_symbol)(const char *);
static jl_module_t **p_jl_main_module;
static jl_value_t *(*p_jl_get_global)(jl_module_t *, jl_sym_t *);
static void *(*p_jl_unbox_voidpointer)(jl_value_t *);
static jl_value_t *(*p_jl_exception_occurred)(void);

// Helper function to extract function pointers from the dynamically
// loaded libjulia.
#ifdef _OS_WINDOWS_
static void *load_function(HMODULE libjulia, const char *name, int *success)
#else
static void *load_function(void *libjulia, const char *name, int *success)
#endif
{
#ifdef _OS_WINDOWS_
    void *p = GetProcAddress(libjulia, name);
#else
    void *p = dlsym(libjulia, name);
#endif

    // Unfortunately Julia renames jl_init to jl_init__threading if
    // Julia is compiled with threading support, so we have to check
    // which of these is available, or otherwise query Julia in some
    // other way (issue #28824).
    if (!p && strcmp(name, "jl_init") == 0) {
#ifdef _OS_WINDOWS_
        p = GetProcAddress(libjulia, "jl_init__threading");
#else
        p = dlsym(libjulia, "jl_init__threading");
#endif
    }

    if (!p) {
        fprintf(stderr, "%s not found in libjulia.\n", name);
        *success = 0;
    }

    return p;
}

// Open libjulia and extract pointers to the needed functions.
static int load_julia_functions()
{
#ifdef _OS_WINDOWS_
    // libjulia.dll needs to be in the same directory as the
    // executable or in PATH.
    const char *library_name = "libjulia.dll";
    HMODULE libjulia = LoadLibrary(library_name);
#else
    // libjulia.so needs to be in LD_LIBRARY_PATH. It could also be in
    // rpath (but that kind of defeats the purpose of dynamically
    // loading libjulia) or an absolute path could be given, computed
    // from other information.
#ifdef __APPLE__
    const char *library_name = "libjulia.dylib";
#else
    const char *library_name = "libjulia.so";
#endif
    void *libjulia = dlopen(library_name, RTLD_LAZY | RTLD_GLOBAL);
#endif

    if (!libjulia) {
        fprintf(stderr, "Failed to load libjulia.\n");
        return 0;
    }

    int success = 1;
    p_jl_init = load_function(libjulia, "jl_init", &success);
    p_jl_is_initialized = load_function(libjulia, "jl_is_initialized", &success);
    p_jl_atexit_hook = load_function(libjulia, "jl_atexit_hook", &success);
    p_jl_eval_string = load_function(libjulia, "jl_eval_string", &success);
    p_jl_symbol = load_function(libjulia, "jl_symbol", &success);
    p_jl_main_module = load_function(libjulia, "jl_main_module", &success);
    p_jl_get_global = load_function(libjulia, "jl_get_global", &success);
    p_jl_unbox_voidpointer = load_function(libjulia, "jl_unbox_voidpointer", &success);
    p_jl_exception_occurred = load_function(libjulia, "jl_exception_occurred", &success);

    return success;
}

// Masquerade the dynamically loaded function pointers as regular functions.
#define jl_init (*p_jl_init)
#define jl_is_initialized (*p_jl_is_initialized)
#define jl_atexit_hook (*p_jl_atexit_hook)
#define jl_eval_string (*p_jl_eval_string)
#define jl_symbol (*p_jl_symbol)
#define jl_main_module (*p_jl_main_module)
#define jl_get_global (*p_jl_get_global)
#define jl_unbox_voidpointer (*p_jl_unbox_voidpointer)
#define jl_exception_occurred (*p_jl_exception_occurred)

// Helper function to retrieve pointers to cfunctions on the Julia side.
static void *get_cfunction_pointer(const char *name)
{
    void *p = 0;
    jl_value_t *boxed_pointer = jl_get_global(jl_main_module, jl_symbol(name));

    if (boxed_pointer != 0) {
        p = jl_unbox_voidpointer(boxed_pointer);
    }

    if (!p) {
        fprintf(stderr, "cfunction pointer %s not available.\n", name);
    }

    return p;
}

// jl_eval_string with error checking.
static int checked_eval_string(const char *command, const char *error_message)
{
    jl_eval_string(command);

    if (jl_exception_occurred()) {
        const char *p = jl_unbox_voidpointer(jl_eval_string("pointer(sprint(showerror, ccall(:jl_exception_occurred, Any, ())))"));
        fprintf(stderr, "%s%s\n", error_message, p);
        return 0;
    }

    return 1;
}

int main()
{
    if (!load_julia_functions())
        return EXIT_FAILURE;

    jl_init();
    if (!jl_is_initialized()) {
        fprintf(stderr, "jl_init failed.");
        jl_atexit_hook(EXIT_FAILURE);
        return EXIT_FAILURE;
    }

    // `include` is not available out of the box from the embedding
    // environment, so we add it here (issue #28825).
    checked_eval_string("include(x) = Base.include(Main, x)",
                        "Failed to define include: ");

    if (!checked_eval_string("include(\"embeddingdl-cfunctions.jl\")",
                             "Failed to load cfunctions: ")) {
        jl_atexit_hook(EXIT_FAILURE);
        return EXIT_FAILURE;
    }

    double (*p_julia_sqrt)(double) = get_cfunction_pointer("julia_sqrt");
    if (!p_julia_sqrt) {
        jl_atexit_hook(EXIT_FAILURE);
        return EXIT_FAILURE;
    }

    printf("%e\n", (*p_julia_sqrt)(2.0));
    fflush(stdout);

    // Handling exceptions gracefully
    checked_eval_string("function this_function_has_no_methods end; this_function_has_no_methods()", "Intentional error: ");

    int ret = EXIT_SUCCESS;
    jl_atexit_hook(ret);
    return ret;
}
