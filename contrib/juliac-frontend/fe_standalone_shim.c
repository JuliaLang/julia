// This file is a part of Julia. License is MIT: https://julialang.org/license

// Stand-ins for the symbols that the CLI loader library (libjulia) normally
// provides to libjulia-internal, for the self-contained standalone frontend
// library, which contains its own private copy of the runtime and is not
// loaded through the CLI loader. Compare cli/jl_exports.h.
//
// The codegen and frontend trampolines that the loader would provide are
// instead bound directly to their *_fallback implementations at link time
// (see the --defsym arguments in the Makefile).

#include <stddef.h>
#include <stdatomic.h>
#include <string.h>

#include <dlfcn.h>
#include <libgen.h>

#include "julia.h"

// Exported data symbols normally instantiated in the CLI library
// (JL_EXPORTED_DATA_SYMBOLS); types must match the declarations in julia.h.
JL_DLLEXPORT jl_options_t jl_options;
JL_DLLEXPORT _Atomic(int) jl_n_threads;
JL_DLLEXPORT int jl_n_gcthreads;
JL_DLLEXPORT int jl_n_threadpools;
JL_DLLEXPORT int *jl_n_threads_per_pool;
JL_DLLEXPORT int jl_task_gcstack_offset;
JL_DLLEXPORT int jl_task_ptls_offset;

// n.b. jl_small_typeof is provided by the embedded image archive

// The loader's "where is libjulia" query; point it at our own location.
JL_DLLEXPORT const char *jl_get_libdir(void)
{
    static char buf[4096];
    if (buf[0] == '\0') {
        Dl_info info;
        if (!dladdr((void *)&jl_get_libdir, &info) || !info.dli_fname ||
            strlen(info.dli_fname) >= sizeof(buf))
            return NULL;
        strcpy(buf, info.dli_fname);
        dirname(buf);
    }
    return buf;
}
