// Loads one or both of the trimmed libraries and calls their entry points.
//
// Usage: twoimages <solo|samethread|newthread> <libA> <libB>
//   solo        load A, call it (expect 1) and exit
//   samethread  load both, call A then B on the main thread
//   newthread   load both, call A on the main thread and B on a fresh thread
//
// In the two-library modes the call into B is expected to terminate the process:
// libjulia is already initialized with A's image, so B's image can never be
// relocated. Reaching the end of `main` therefore means the check is missing.

#include <stdio.h>
#include <stdint.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
typedef HMODULE lib_handle_t;
#else
#include <dlfcn.h>
#include <pthread.h>
typedef void* lib_handle_t;
#endif

typedef int32_t (*answer_t)(void);

static lib_handle_t load_library(const char* path) {
#ifdef _WIN32
    return LoadLibraryA(path);
#else
    return dlopen(path, RTLD_NOW | RTLD_LOCAL);
#endif
}

static void* get_symbol(lib_handle_t handle, const char* name) {
#ifdef _WIN32
    return (void*)GetProcAddress(handle, name);
#else
    return dlsym(handle, name);
#endif
}

static void print_load_error(const char* context) {
#ifdef _WIN32
    fprintf(stderr, "%s failed: error code %lu\n", context, GetLastError());
#else
    fprintf(stderr, "%s failed: %s\n", context, dlerror());
#endif
}

static answer_t thread_entry;

#ifdef _WIN32
static DWORD WINAPI thread_main(LPVOID unused) {
    (void)unused;
    thread_entry();
    return 0;
}
static int call_on_new_thread(answer_t f) {
    thread_entry = f;
    HANDLE t = CreateThread(NULL, 0, thread_main, NULL, 0, NULL);
    if (t == NULL)
        return 0;
    WaitForSingleObject(t, INFINITE);
    CloseHandle(t);
    return 1;
}
#else
static void* thread_main(void* unused) {
    (void)unused;
    thread_entry();
    return NULL;
}
static int call_on_new_thread(answer_t f) {
    pthread_t t;
    thread_entry = f;
    if (pthread_create(&t, NULL, thread_main, NULL) != 0)
        return 0;
    pthread_join(t, NULL);
    return 1;
}
#endif

int main(int argc, char** argv) {
    if (argc < 4) {
        fprintf(stderr, "usage: %s <solo|samethread|newthread> <libA> <libB>\n", argv[0]);
        return 2;
    }
    const char* mode = argv[1];

    lib_handle_t ha = load_library(argv[2]);
    if (!ha) {
        print_load_error("LoadLibrary/dlopen A");
        return 3;
    }
    answer_t a = (answer_t)get_symbol(ha, "twoimages_a_answer");
    if (!a) {
        print_load_error("GetProcAddress/dlsym twoimages_a_answer");
        return 4;
    }

    if (strcmp(mode, "solo") == 0) {
        int32_t r = a();
        printf("a=%d\n", r);
        fflush(stdout);
        return r == 1 ? 0 : 5;
    }

    lib_handle_t hb = load_library(argv[3]);
    if (!hb) {
        print_load_error("LoadLibrary/dlopen B");
        return 6;
    }
    answer_t b = (answer_t)get_symbol(hb, "twoimages_b_answer");
    if (!b) {
        print_load_error("GetProcAddress/dlsym twoimages_b_answer");
        return 7;
    }

    int32_t r = a();
    printf("a=%d\n", r);
    fflush(stdout);
    if (r != 1)
        return 5;

    if (strcmp(mode, "newthread") == 0) {
        if (!call_on_new_thread(b)) {
            fprintf(stderr, "could not create a thread\n");
            return 8;
        }
    }
    else {
        b();
    }

    fprintf(stderr, "the second image was entered without being reported\n");
    return 9;
}
