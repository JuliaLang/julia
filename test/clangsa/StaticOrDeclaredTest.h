// This file is a part of Julia. License is MIT: https://julialang.org/license

// Public prototypes for StaticOrDeclaredTest: a function defined in the test
// file whose prototype lives here is "declared in a header" and must not be
// flagged by the julia-static-or-declared check.

void sod_public(void);
int sod_public_val(int x);
void sod_redundant_hdr(void);
__attribute__((visibility("default"))) void sod_fwd_exported(void) {}

#ifdef __cplusplus
struct sod_HdrType {
    void method();
};

class sod_Opaque;
#endif
