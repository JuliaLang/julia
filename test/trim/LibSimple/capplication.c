#include <stdio.h>
#include <stdint.h>

#ifdef _WIN32
#include <windows.h>
typedef HMODULE lib_handle_t;
#else
#include <dlfcn.h>
typedef void* lib_handle_t;
#endif

// Cross-platform library loading functions
static lib_handle_t load_library(const char* path) {
#ifdef _WIN32
    return LoadLibraryA(path);
#else
    return dlopen(path, RTLD_NOW | RTLD_GLOBAL);
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

// Struct definitions matching LibSimple.jl
typedef struct CVector_Float32 {
    int32_t length;
    float* data;
} CVector_Float32;
typedef struct CVectorPair_Float32 {
    CVector_Float32 from;
    CVector_Float32 to;
} CVectorPair_Float32;
typedef struct MyTwoVec {
    int32_t x;
    int32_t y;
} MyTwoVec;

typedef float (*copyto_and_sum_t)(CVectorPair_Float32);
typedef int32_t (*countsame_t)(MyTwoVec*, int32_t);

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s <libpath>\n", argv[0]);
        return 2;
    }

    lib_handle_t h = load_library(argv[1]);
    if (!h) {
        print_load_error("LoadLibrary/dlopen");
        return 3;
    }

    // Test copyto_and_sum
    copyto_and_sum_t copyto_and_sum = (copyto_and_sum_t)get_symbol(h, "copyto_and_sum");
    if (!copyto_and_sum) {
        print_load_error("GetProcAddress/dlsym copyto_and_sum");
        return 4;
    }

    CVectorPair_Float32 vecPair;
    float from_data[] = {1.0f, 2.0f, 3.0f};
    float to_data[] = {4.0f, 5.0f, 6.0f};
    vecPair.from.length = 3;
    vecPair.from.data = from_data;
    vecPair.to.length = 3;
    vecPair.to.data = to_data;

    float sum = copyto_and_sum(vecPair);
    printf("Sum of copied values: %f\n", sum);
    if (sum < 5.9f || sum > 6.1f) {
        fprintf(stderr, "bad result from copyto_and_sum: %f (expected 6.0)\n", sum);
        return 5;
    }

    // Test countsame
    countsame_t countsame = (countsame_t)get_symbol(h, "countsame");
    if (!countsame) {
        print_load_error("GetProcAddress/dlsym countsame");
        return 6;
    }

    MyTwoVec list[] = {{1, 2}, {5, 5}, {3, 4}};
    int32_t count = countsame(list, 3);
    printf("Count of same vectors: %d\n", count);
    if (count != 1) {
        fprintf(stderr, "bad result from countsame: %d (expected 1)\n", count);
        return 7;
    }

    return 0;
}
