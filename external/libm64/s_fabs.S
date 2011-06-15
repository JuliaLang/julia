#include "bsd_asm.h"

.align 8
.size S1, 8
S1:
	.long 0xffffffff
	.long 0x7fffffff
ENTRY(fabs)
	pand    S1(%rip), %xmm0
	ret
END(fabs)
