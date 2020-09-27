#include "loader.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Bring in helper functions for windows without libgcc. */
#ifdef _OS_WINDOWS_
#include "loader_win_utils.c"
#endif

/*
 * DEP_LIBS is our list of dependent libraries that must be loaded before `libjulia`.
 * Note that order matters, as each entry will be opened in-order.  We define here a
 * dummy value just so this file compiles on its own, and also so that developers can
 * see what this value should look like.  Note that the last entry must always be
 * `libjulia`, and that all paths should be relative to this loader `.exe` path.
 */
#if !defined(DEP_LIBS)
#define DEP_LIBS "../lib/example.so:../lib/libjulia.so"
#endif
static char dep_libs[256] = DEP_LIBS;

void print_stderr(const char * msg)
{
    fputs(msg, stderr);
}
// I use three arguments a lot.
void print_stderr3(const char * msg1, const char * msg2, const char * msg3)
{
    print_stderr(msg1);
    print_stderr(msg2);
    print_stderr(msg3);
}


/* Absolute path to the path of the current executable, gets filled in by `get_exe_path()` */
static void * load_library(const char * rel_path, const char * src_dir) {
    char path[2*PATH_MAX + 1] = {0};
    strncat(path, src_dir, sizeof(path) - 1);
    strncat(path, PATHSEPSTRING, sizeof(path) - 1);
    strncat(path, rel_path, sizeof(path) - 1);

    void * handle = NULL;
#if defined(_OS_WINDOWS_)
    wchar_t wpath[2*PATH_MAX + 1] = {0};
    if (!utf8_to_wchar(path, wpath, 2*PATH_MAX)) {
        print_stderr3("ERROR: Unable to convert path ", path, " to wide string!\n");
        exit(1);
    }
    handle = (void *)LoadLibraryExW(wpath, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
#else
    handle = dlopen(path, RTLD_NOW | RTLD_GLOBAL);
#endif

    if (handle == NULL) {
        print_stderr3("ERROR: Unable to load dependent library ", path, "\n");
#if defined(_OS_WINDOWS_)
        LPWSTR wmsg = TEXT("");
        FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                       FORMAT_MESSAGE_FROM_SYSTEM |
                       FORMAT_MESSAGE_IGNORE_INSERTS |
                       FORMAT_MESSAGE_MAX_WIDTH_MASK,
                       NULL, GetLastError(),
                       MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US),
                       (LPWSTR)&wmsg, 0, NULL);
        char err[256] = {0};
        wchar_to_utf8(wmsg, err, 255);
        print_stderr3("Message:", err, "\n");
#else
        print_stderr3("Message:", dlerror(), "\n");
#endif
        exit(1);
    }
    return handle;
}

char exe_dir[PATH_MAX];
const char * get_exe_dir()
{
#if defined(_OS_WINDOWS_)
    // On Windows, we use GetModuleFileName()
    wchar_t julia_path[PATH_MAX];
    if (!GetModuleFileName(NULL, julia_path, PATH_MAX)) {
        print_stderr("ERROR: GetModuleFileName() failed\n");
        exit(1);
    }
    if (!wchar_to_utf8(julia_path, exe_dir, PATH_MAX)) {
        print_stderr("ERROR: Unable to convert julia path to UTF-8\n");
        exit(1);
    }
#elif defined(_OS_DARWIN_)
    // On MacOS, we use _NSGetExecutablePath(), followed by realpath()
    char nonreal_exe_path[PATH_MAX + 1];
    uint32_t exe_path_len = PATH_MAX;
    int ret = _NSGetExecutablePath(nonreal_exe_path, &exe_path_len);
    if (ret != 0) {
        print_stderr("ERROR: _NSGetExecutablePath() failed\n");
        exit(1);
    }

    if (realpath(nonreal_exe_path, exe_dir) == NULL) {
        print_stderr("ERROR: realpath() failed\n");
        exit(1);
    }
#elif defined(_OS_LINUX_)
    // On Linux, we read from /proc/self/exe
    int num_bytes = readlink("/proc/self/exe", exe_dir, PATH_MAX);
    if (num_bytes == -1) {
        print_stderr("ERROR: readlink(/proc/self/exe) failed\n");
        exit(1);
    }
    exe_dir[num_bytes] = '\0';
#elif defined(_OS_FREEBSD_)
    // On FreeBSD, we use the KERN_PROC_PATHNAME sysctl:
    int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1};
    unsigned long exe_dir_len = PATH_MAX - 1;
    int ret = sysctl(mib, 4, &exe_dir, &exe_dir_len, NULL, 0);
    if (ret) {
        print_stderr("ERROR: sysctl(KERN_PROC_PATHNAME) failed\n");
        exit(1);
    }
    exe_dir[exe_dir_len] = '\0';
#endif
    // Finally, convert to dirname
    const char * new_dir = dirname(exe_dir);
    if (new_dir != exe_dir) {
        // On some plateforms, dirname() mutates.  On others, it does not.
        memcpy(exe_dir, new_dir, strlen(new_dir)+1);
    }
    return exe_dir;
}

// Load libjulia and run the REPL with the given arguments (in UTF-8 format)
int load_repl(const char * exe_dir, int argc, char * argv[])
{
    // Pre-load libraries that libjulia needs.
    int deps_len = strlen(dep_libs);
    char * curr_dep = &dep_libs[0];
    while (1) {
        // try to find next colon character, if we can't, escape out.
        char * colon = strchr(curr_dep, ':');
        if (colon == NULL)
            break;

        // Chop the string at the colon, load this library.
        *colon = '\0';
        load_library(curr_dep, exe_dir);

        // Skip ahead to next dependency
        curr_dep = colon + 1;
    }

    // Last dependency is `libjulia`, so load that and we're done with `dep_libs`!
    void * libjulia = load_library(curr_dep, exe_dir);

    // Next, if we're on Linux/FreeBSD, set up fast TLS.
#if !defined(_OS_WINDOWS_) && !defined(_OS_DARWIN_)
    void (*jl_set_ptls_states_getter)(void *) = dlsym(libjulia, "jl_set_ptls_states_getter");
    if (jl_set_ptls_states_getter == NULL) {
        print_stderr("ERROR: Cannot find jl_set_ptls_states_getter() function within libjulia!\n");
        exit(1);
    }
    void * (*fptr)(void) = dlsym(NULL, "jl_get_ptls_states_static");
    if (fptr == NULL) {
        print_stderr("ERROR: Cannot find jl_get_ptls_states_static(), must define this symbol within calling executable!\n");
        exit(1);
    }
    jl_set_ptls_states_getter((void *)fptr);
#endif

    // Load the repl entrypoint symbol and jump into it!
    int (*entrypoint)(int, char **) = NULL;
    #ifdef _OS_WINDOWS_
        entrypoint = (int (*)(int, char **))GetProcAddress((HMODULE) libjulia, "repl_entrypoint");
    #else
        entrypoint = (int (*)(int, char **))dlsym(libjulia, "repl_entrypoint");
    #endif
    if (entrypoint == NULL) {
        print_stderr("ERROR: Unable to find `repl_entrypoint()` within libjulia!\n");
        exit(1);
    }
    return entrypoint(argc, (char **)argv);
}

// Empty DLL main entrypoint to silence warning
#ifdef _OS_WINDOWS_
int __stdcall DllMainCRTStartup(void* instance, unsigned reason, void* reserved) {
}
#endif

#ifdef __cplusplus
} // extern "C"
#endif
