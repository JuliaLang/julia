// This file is a part of Julia. License is MIT: https://julialang.org/license
// Bring in the curated lists of exported data and function symbols, then
// perform C preprocessor magic upon them to generate lists of declarations and
// functions to re-export our function symbols from libjulia-internal to libjulia.
#include "../src/jl_exported_data.inc"
#include "../src/jl_exported_funcs.inc"

// Define pointer data as `const void * $(name);`
#define XX(name)    JL_DLLEXPORT const void * name;
JL_EXPORTED_DATA_POINTERS(XX)
#undef XX

// Define symbol data as `$(type) $(name);`
#define XX(name, type)    JL_DLLEXPORT type name;
JL_EXPORTED_DATA_SYMBOLS(XX)
#undef XX

// Declare list of exported functions (sans type)
#define XX(name)    JL_DLLEXPORT void name(void);
typedef void (anonfunc)(void);
JL_EXPORTED_FUNCS(XX)
#undef XX

// Define holder locations for function addresses as `const void * $(name)_addr = & $(name);`
#define XX(name)    JL_HIDDEN anonfunc * name##_addr = (anonfunc*)&name;
JL_EXPORTED_FUNCS(XX)
#undef XX

// Generate lists of function names and addresses
#define XX(name)    #name,
static const char *const jl_exported_func_names[] = {
    JL_EXPORTED_FUNCS(XX)
    NULL
};
#undef XX

#define XX(name)    &name##_addr,
static anonfunc **const jl_exported_func_addrs[] = {
    JL_EXPORTED_FUNCS(XX)
    NULL
};
#undef XX
