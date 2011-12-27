#include "bsd_asm.h"


ENTRY(fabsf)
	flds	4(%esp)
	fabs
	ret
END(fabsf)
