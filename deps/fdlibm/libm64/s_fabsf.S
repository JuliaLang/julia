#include "bsd_asm.h"

ENTRY(fabsf)
	movd %xmm0,%eax
	and  $0x7fffffff,%eax
	movd %eax,%xmm0
	ret
END(fabsf)
