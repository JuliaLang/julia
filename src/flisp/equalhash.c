#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <setjmp.h>

#include "flisp.h"
#include "equalhash.h"

#include "htable.inc"

#define _equal_lispvalue_(x,y) equal_lispvalue((value_t)(x),(value_t)(y))

#ifdef __cplusplus
extern "C" {
#endif

HTIMPL(equalhash, hash_lispvalue, _equal_lispvalue_)

#ifdef __cplusplus
}
#endif
