#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <setjmp.h>

#include "llt.h"
#include "flisp.h"
#include "equalhash.h"

#include "htable.inc"

#define _equal_lispvalue_(x,y) equal_lispvalue((value_t)(x),(value_t)(y))

HTIMPL(equalhash, hash_lispvalue, _equal_lispvalue_)
