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
// double-clickable entry point (CFBundleExecutable): a copy of this loader placed
// directly in Contents/MacOS/ (a real Mach-O, as Apple's notary requires of a bundle's
// main executable -- a symlink, or a binary living under Contents/Resources/ where it is
// also sealed as a resource, is rejected as an invalid signature). When the loader is
// invoked under that name, we don't start the REPL in-process (a Finder-launched bundle
// has no controlling terminal); instead we relaunch the REPL binary from the bundled
// tree inside a new Terminal.app window so the user gets an interactive session. Keying
// off the `julia-terminal` name is also how we avoid hijacking ordinary `julia`
// invocations (e.g. non-interactive `julia script.jl`).
static void maybe_launch_terminal(void)
{
    char exe_path[JL_PATH_MAX];
    uint32_t bufsize = sizeof(exe_path);
    if (_NSGetExecutablePath(exe_path, &bufsize) != 0)
        return;
    // Resolve any symlinks to get this executable's canonical path.
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

    // Locate the REPL binary to hand to Terminal.app. In the app bundle the main
    // executable is Contents/MacOS/julia-terminal and the REPL lives in the bundled tree
    // at ../Resources/julia/bin/julia -- it must run from there so it finds its sysimage,
    // stdlibs and share/ via its own ../lib and ../share. Rewrite self_path to that path
    // in place, falling back to a sibling `julia` for any other (non-bundle) layout.
    base[-1] = '\0';                             // strip "/julia-terminal" -> "...MacOS"
    char * dir = strrchr(self_path, '/');
    if (dir != NULL && strcmp(dir + 1, "MacOS") == 0) {
        static const char rel[] = "/Resources/julia/bin/julia";
        if ((size_t)(dir - self_path) + sizeof(rel) > sizeof(self_path))
            return;
        strcpy(dir, rel);                        // ".../Contents" + "/Resources/julia/bin/julia"
    }
    else {
        static const char rel[] = "/julia";
        if (strlen(self_path) + sizeof(rel) > sizeof(self_path))
            return;
        strcat(self_path, rel);
    }
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
