#include "bsd_asm.h"

.align 8
.size S1, 4
S1:
	.long 0x7fffffff
ENTRY(fabsf)
	pand    S1(%rip), %xmm0
	ret
END(fabsf)
