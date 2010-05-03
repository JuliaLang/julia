#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <setjmp.h>

#include "llt.h"
#include "flisp.h"

#include "htable.inc"

HTIMPL(equalhash, hash_lispvalue, equal_lispvalue)
