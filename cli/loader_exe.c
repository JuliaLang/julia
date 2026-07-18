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

JULIA_DEFINE_FAST_TLS

#ifdef _OS_DARWIN_
// The macOS application bundle (contrib/mac/app) uses `julia-terminal` as its
// double-clickable entry point (CFBundleExecutable). `julia-terminal` is a hardlink
// to the julia binary sitting next to it in `bin/`. When the loader is invoked under
// that name, we don't start the REPL in-process (a Finder-launched bundle has no
// controlling terminal); instead we relaunch the sibling `julia` binary inside a new
// Terminal.app window so the user gets an interactive session.
//
// A hardlink (rather than a symlink) is what makes this work: LaunchServices resolves
// a symlinked CFBundleExecutable down to its target, discarding the `julia-terminal`
// name, whereas a hardlink keeps its own name. That distinct name is also how we avoid
// hijacking ordinary `julia` invocations (e.g. non-interactive `julia script.jl`).
static void maybe_launch_terminal(void)
{
    char exe_path[JL_PATH_MAX];
    uint32_t bufsize = sizeof(exe_path);
    if (_NSGetExecutablePath(exe_path, &bufsize) != 0)
        return;
    // Resolve any intermediate symlinks (the bundle's Contents/MacOS/julia-terminal
    // is a symlink into bin/) to land on the real `julia-terminal` hardlink.
    char self_path[JL_PATH_MAX];
    if (realpath(exe_path, self_path) == NULL)
        return;

    // Only take over when invoked as `julia-terminal`.
    char * base = strrchr(self_path, '/');
    if (base == NULL)
        return;
    base++;
    if (strcmp(base, "julia-terminal") != 0)
        return;

    // The real julia binary is the sibling `julia`. Hand it to Terminal.app, which
    // opens a window and runs the REPL there.
    strcpy(base, "julia");
    execl("/usr/bin/open", "open", "-a", "Terminal", self_path, (char *)NULL);
    jl_loader_print_stderr("ERROR: Failed to launch Terminal.app!\n");
    exit(1);
}
#endif

#ifdef _COMPILER_ASAN_ENABLED_
JL_DLLEXPORT const char* __asan_default_options(void)
{
    return "allow_user_segv_handler=1:detect_leaks=0";
    // FIXME: enable LSAN after fixing leaks & defining __lsan_default_suppressions(),
    //        or defining __lsan_default_options = exitcode=0 once publicly available
    //        (here and in flisp/flmain.c)
}
#endif

#ifdef _OS_WINDOWS_
int mainCRTStartup(void)
{
    int argc;
    LPWSTR * wargv = CommandLineToArgv(GetCommandLine(), &argc);
    char ** argv = (char **)malloc(sizeof(char*) * (argc + 1));
    setup_stdio();
#else
int main(int argc, char * argv[])
{
#endif

#if defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_TSAN_ENABLED_) || defined(_COMPILER_MSAN_ENABLED_)
    // ASAN/TSAN do not support RTLD_DEEPBIND
    // https://github.com/google/sanitizers/issues/611
    putenv("LBT_USE_RTLD_DEEPBIND=0");
#endif

#ifdef _OS_DARWIN_
    maybe_launch_terminal();
#endif

    // Convert Windows wchar_t values to UTF8
#ifdef _OS_WINDOWS_
    for (int i = 0; i < argc; i++) {
        argv[i] = wchar_to_utf8(wargv[i]);
        if (!argv[i]) {
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

#if defined(__GLIBC__) && (defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_TSAN_ENABLED_))
// fork is generally bad news, but it is better if we prevent applications from
// making it worse as openblas threadpools cause it to hang
int __register_atfork232(void (*prepare)(void), void (*parent)(void), void (*child)(void), void *dso_handle) {
    return 0;
}
__asm__ (".symver __register_atfork232, __register_atfork@@GLIBC_2.3.2");
#endif

#ifdef __cplusplus
} // extern "C"
#endif
