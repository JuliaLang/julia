// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "platform.h"
#include "julia.h"
#include "julia_internal.h"
#ifdef _OS_WINDOWS_
#include <windows.h>
#include <direct.h>
#else
#include <unistd.h>
#include <dlfcn.h>
#endif
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__APPLE__)
static char const *const extensions[] = { "", ".dylib" };
#elif defined(_OS_WINDOWS_)
static char const *const extensions[] = { "", ".dll" };
extern int needsSymRefreshModuleList;
#else
static char const *const extensions[] = { "", ".so" };
#endif
#define N_EXTENSIONS (sizeof(extensions) / sizeof(char*))

static int endswith_extension(const char *path)
{
    if (!path)
        return 0;
    size_t len = strlen(path);
    // Skip the first one since it is empty
    for (size_t i = 1; i < N_EXTENSIONS; i++) {
        const char *ext = extensions[i];
        size_t extlen = strlen(ext);
        if (len < extlen)
            return 0;
        // Skip version extensions if present
        size_t j = len - 1;
        while (j > 0) {
            if (path[j] == '.' || (path[j] >= '0' && path[j] <= '9'))
                j--;
            else
                break;
        }
        if ((j == len-1 || path[j+1] == '.') && memcmp(ext, path + j - extlen + 1, extlen) == 0) {
            return 1;
        }
    }
    return 0;
}

#define PATHBUF 4096

#define JL_RTLD(flags, FLAG) (flags & JL_RTLD_ ## FLAG ? RTLD_ ## FLAG : 0)

#ifdef _OS_WINDOWS_
static void win32_formatmessage(DWORD code, char *reason, int len)
{
    DWORD res;
    LPWSTR errmsg;
    res = FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                         FORMAT_MESSAGE_FROM_SYSTEM |
                         FORMAT_MESSAGE_IGNORE_INSERTS |
                         FORMAT_MESSAGE_MAX_WIDTH_MASK,
                         NULL, code,
                         MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US),
                         (LPWSTR)&errmsg, 0, NULL);
    if (!res && (GetLastError() == ERROR_MUI_FILE_NOT_FOUND ||
                 GetLastError() == ERROR_RESOURCE_TYPE_NOT_FOUND)) {
      res = FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                           FORMAT_MESSAGE_FROM_SYSTEM |
                           FORMAT_MESSAGE_IGNORE_INSERTS |
                           FORMAT_MESSAGE_MAX_WIDTH_MASK,
                           NULL, code,
                           0, (LPWSTR)&errmsg, 0, NULL);
    }
    res = WideCharToMultiByte(CP_UTF8, 0, errmsg, -1, reason, len, NULL, NULL);
    assert(res > 0 || GetLastError() == ERROR_INSUFFICIENT_BUFFER);
    reason[len - 1] = '\0';
    LocalFree(errmsg);
}
#endif

JL_DLLEXPORT void *jl_dlopen(const char *filename, unsigned flags)
{
#if defined(_OS_WINDOWS_)
    needsSymRefreshModuleList = 1;
    size_t len = MultiByteToWideChar(CP_UTF8, 0, filename, -1, NULL, 0);
    if (!len) return NULL;
    WCHAR *wfilename = (WCHAR*)alloca(len * sizeof(WCHAR));
    if (!MultiByteToWideChar(CP_UTF8, 0, filename, -1, wfilename, len)) return NULL;
    return LoadLibraryExW(wfilename, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
#else
    dlerror(); /* Reset error status. */
    return dlopen(filename,
                  (flags & JL_RTLD_NOW ? RTLD_NOW : RTLD_LAZY)
                  | JL_RTLD(flags, LOCAL)
                  | JL_RTLD(flags, GLOBAL)
#ifdef RTLD_NODELETE
                  | JL_RTLD(flags, NODELETE)
#endif
#ifdef RTLD_NOLOAD
                  | JL_RTLD(flags, NOLOAD)
#endif
#if defined(RTLD_DEEPBIND) && !defined(JL_ASAN_ENABLED)
                  | JL_RTLD(flags, DEEPBIND)
#endif
#ifdef RTLD_FIRST
                  | JL_RTLD(flags, FIRST)
#endif
                  );
#endif
}

JL_DLLEXPORT int jl_dlclose(void *handle)
{
#ifdef _OS_WINDOWS_
    if (!handle) return -1;
    return !FreeLibrary((HMODULE) handle);
#else
    dlerror(); /* Reset error status. */
    if (!handle) return -1;
    return dlclose(handle);
#endif
}

JL_DLLEXPORT void *jl_load_dynamic_library(const char *modname, unsigned flags, int throw_err)
{
    char path[PATHBUF], relocated[PATHBUF];
    int i;
#ifdef _OS_WINDOWS_
    int err;
#endif
    uv_stat_t stbuf;
    void *handle;
    int abspath;
    // number of extensions to try — if modname already ends with the
    // standard extension, then we don't try adding additional extensions
    int n_extensions = endswith_extension(modname) ? 1 : N_EXTENSIONS;

    /*
      this branch returns handle of libjulia
    */
    if (modname == NULL) {
#ifdef _OS_WINDOWS_
        if (!GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                                (LPCWSTR)(uintptr_t)(&jl_load_dynamic_library),
                                (HMODULE*)&handle)) {
            jl_error("could not load base module");
        }
#else
        Dl_info info;
        if (!dladdr((void*)(uintptr_t)&jl_load_dynamic_library, &info) || !info.dli_fname)
            jl_error("could not load base module");
        handle = dlopen(info.dli_fname, RTLD_NOW);
#endif
        goto done;
    }

    abspath = isabspath(modname);

    /*
      this branch permutes all base paths in DL_LOAD_PATH with all extensions
      note: skip when !jl_base_module to avoid UndefVarError(:DL_LOAD_PATH),
            and also skip for absolute paths
      We also do simple string replacement here for elements starting with `@executable_path/`.
      While these exist as OS concepts on Darwin, we want to use them on other platforms
      such as Windows, so we emulate them here.
    */
    if (!abspath && jl_base_module != NULL) {
        jl_array_t *DL_LOAD_PATH = (jl_array_t*)jl_get_global(jl_base_module, jl_symbol("DL_LOAD_PATH"));
        if (DL_LOAD_PATH != NULL) {
            size_t j;
            for (j = 0; j < jl_array_len(DL_LOAD_PATH); j++) {
                char *dl_path = jl_string_data(jl_array_ptr_data(DL_LOAD_PATH)[j]);
                size_t len = strlen(dl_path);
                if (len == 0)
                    continue;

                // Is this entry supposed to be relative to the bindir?
                if (len >= 16 && strncmp(dl_path, "@executable_path", 16) == 0) {
                    snprintf(relocated, PATHBUF, "%s%s", jl_options.julia_bindir, dl_path + 16);
                    len = len - 16 + strlen(jl_options.julia_bindir);
                } else {
                    strncpy(relocated, dl_path, len);
                }
                for (i = 0; i < n_extensions; i++) {
                    const char *ext = extensions[i];
                    path[0] = '\0';
                    if (relocated[len-1] == PATHSEPSTRING[0])
                        snprintf(path, PATHBUF, "%s%s%s", relocated, modname, ext);
                    else
                        snprintf(path, PATHBUF, "%s" PATHSEPSTRING "%s%s", relocated, modname, ext);
#ifdef _OS_WINDOWS_
                    if (i == 0) { // LoadLibrary already tested the extensions, we just need to check the `stat` result
#endif
                        handle = jl_dlopen(path, flags);
                        if (handle)
                            goto done;
#ifdef _OS_WINDOWS_
                        err = GetLastError();
                    }
#endif
                    // bail out and show the error if file actually exists
                    if (jl_stat(path, (char*)&stbuf) == 0)
                        goto notfound;
                }
            }
        }
    }

    // now fall back and look in default library paths, for all extensions
    for (i = 0; i < n_extensions; i++) {
        const char *ext = extensions[i];
        path[0] = '\0';
        snprintf(path, PATHBUF, "%s%s", modname, ext);
        handle = jl_dlopen(path, flags);
        if (handle)
            goto done;
#ifdef _OS_WINDOWS_
        err = GetLastError();
        break; // LoadLibrary already tested the rest
#endif
    }

notfound:
    if (throw_err) {
#ifdef _OS_WINDOWS_
        char reason[256];
        win32_formatmessage(err, reason, sizeof(reason));
#else
        const char *reason = dlerror();
#endif
        jl_errorf("could not load library \"%s\"\n%s", modname, reason);
    }
    handle = NULL;

done:
    return handle;
}

JL_DLLEXPORT int jl_dlsym(void *handle, const char *symbol, void ** value, int throw_err)
{
    int symbol_found = 0;

    /* First, get the symbol value */
#ifdef _OS_WINDOWS_
    *value = GetProcAddress((HMODULE) handle, symbol);
#else
    dlerror(); /* Reset error status. */
    *value = dlsym(handle, symbol);
#endif

    /* Next, check for errors.  On Windows, a NULL pointer means the symbol
     * was not found.  On everything else, we can have NULL symbols, so we check
     * for non-NULL returns from dlerror().  Note that means we unconditionally
     * call dlerror() on POSIX systems.*/
#ifdef _OS_WINDOWS_
    symbol_found = *value != NULL;
#else
    const char *err = dlerror();
    symbol_found = err == NULL;
#endif

    if (!symbol_found && throw_err) {
#ifdef _OS_WINDOWS_
        char err[256];
        win32_formatmessage(GetLastError(), err, sizeof(err));
#endif
        jl_errorf("could not load symbol \"%s\":\n%s", symbol, err);
    }
    return symbol_found;
}

#ifdef _OS_WINDOWS_
//Look for symbols in win32 libraries
const char *jl_dlfind_win32(const char *f_name)
{
    void * dummy;
    if (jl_dlsym(jl_exe_handle, f_name, &dummy, 0))
        return JL_EXE_LIBNAME;
    if (jl_dlsym(jl_dl_handle, f_name, &dummy, 0))
        return JL_DL_LIBNAME;
    if (jl_dlsym(jl_kernel32_handle, f_name, &dummy, 0))
        return "kernel32";
    if (jl_dlsym(jl_ntdll_handle, f_name, &dummy, 0))
        return "ntdll";
    if (jl_dlsym(jl_crtdll_handle, f_name, &dummy, 0))
#if defined(_MSC_VER)
#if _MSC_VER == 1800
        return "msvcr120";
#else
#error This version of MSVC has not been tested.
#endif
#else
        return "msvcrt";
#endif
    if (jl_dlsym(jl_winsock_handle, f_name, &dummy, 0))
        return "ws2_32";
    // additional common libraries (libc?) could be added here, but in general,
    // it is better to specify the library explicitly in the code. This exists
    // mainly to ease compatibility with linux, and for libraries that don't
    // have a name (julia.exe and libjulia.dll)
    // We could also loop over all libraries that have been used so far, but, again,
    // explicit is preferred over implicit
    return NULL;
    // oops, we didn't find it. NULL defaults to searching jl_RTLD_DEFAULT_handle,
    // which defaults to jl_dl_handle, where we won't find it, and will throw the
    // appropriate error.
}
#endif

#ifdef __cplusplus
}
#endif
