// This file is a part of Julia. License is MIT: https://julialang.org/license

// The data for looking up a foreign symbol (the target of a ccall or cglobal),
// shared by codegen and the runtime.

#ifndef JL_CSYMBOL_DATA_H
#define JL_CSYMBOL_DATA_H

#include "julia.h"
#include "plt.h"

#ifdef __cplusplus
extern "C" {
#endif

struct csymbol_data_t {
    jl_plt_entry_t plt; // the slot, and jl_bind_foreignsymbol_slot (NULL for a cglobal)
    const char *lib;    // the library, if known statically
    const char *func;   // the name, if known statically
    // Otherwise, a cell holding the svec (fn,) or (fn, lib) of the values and
    // global references to evaluate (NULL, or a cell holding NULL, if both are static)
    jl_value_t *target;
};

// Looks up a foreign symbol (without storing it into its slot).
JL_DLLEXPORT void *jl_resolve_foreignsymbol(const struct csymbol_data_t *data) JL_CANSAFEPOINT_ENTER_LEAVE;

// The resolve function of the PLT entry: look the symbol up and store it into the slot.
JL_DLLEXPORT void *jl_bind_foreignsymbol_slot(jl_plt_entry_t *entry) JL_CANSAFEPOINT_ENTER_LEAVE;

#ifdef __cplusplus
} // extern "C"
#endif

#endif // JL_CSYMBOL_DATA_H
