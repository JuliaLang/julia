#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <setjmp.h>

#include "flisp.h"
#include "equalhash.h"

#include "htable.inc"

#define _equal_lispvalue_(x, y, ctx)                                    \
    equal_lispvalue((fl_context_t*)ctx, (value_t)(x), (value_t)(y))
#define _hash_lispvalue_(x, ctx)                        \
    hash_lispvalue((fl_context_t*)ctx, (value_t)(x))

#ifdef __cplusplus
extern "C" {
#endif

HTIMPL_R(equalhash, _hash_lispvalue_, _equal_lispvalue_)

#ifdef __cplusplus
}
#endif
