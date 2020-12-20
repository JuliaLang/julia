// This file is a part of Julia. License is MIT: https://julialang.org/license
// This defines a bare-bones loader that opens `libjulia` and immediately invokes its `load_repl()` function.
#include "loader.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Bring in helper functions for windows without libgcc. */
#ifdef _OS_WINDOWS_
#include "loader_win_utils.c"
#endif

/* Define ptls getter, as this cannot be defined within a shared library. */
#if !defined(_OS_WINDOWS_) && !defined(_OS_DARWIN_)
JL_DLLEXPORT JL_CONST_FUNC void * jl_get_ptls_states_static(void)
{
    /* Because we can't #include <julia.h> in this file, we define a TLS state object with
     * hopefully enough room; at last check, the `jl_tls_states_t` struct was <16KB. */
    static __attribute__((tls_model("local-exec"))) __thread char tls_states[32768];
    return &tls_states;
}
#endif

#ifdef _OS_WINDOWS_
int mainCRTStartup(void)
{
    int argc;
    LPWSTR * wargv = CommandLineToArgv(GetCommandLine(), &argc);
    char ** argv = (char **)malloc(sizeof(char *)*(argc+ 1));
    setup_stdio();
#else
int main(int argc, char * argv[])
{
#endif

    // Convert Windows wchar_t values to UTF8
#ifdef _OS_WINDOWS_
    for (int i=0; i<argc; i++) {
        size_t max_arg_len = 4*wcslen(wargv[i]);
        argv[i] = (char *)malloc(max_arg_len);
        if (!wchar_to_utf8(wargv[i], argv[i], max_arg_len)) {
            jl_loader_print_stderr("Unable to convert all arguments to UTF-8!\n");
            return 1;
        }
    }
    argv[argc] = NULL;
#endif

    // Call load_repl with our initialization arguments:
    int ret = jl_load_repl(argc, argv);

    // On Windows we're running without the CRT that would do this for us
    exit(ret);
    return ret;
}

#ifdef __cplusplus
} // extern "C"
#endif
