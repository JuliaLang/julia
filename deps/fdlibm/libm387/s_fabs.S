#include "bsd_asm.h"

ENTRY(fabs)
	fldl	4(%esp)
	fabs
	ret
END(fabs)
