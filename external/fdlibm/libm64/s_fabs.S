#include "bsd_asm.h"

ENTRY(fabs)
	movabs $0x7fffffffffffffff,%rdx
	movq %xmm0,%rax
	and %rdx,%rax
	movq %rax,%xmm0
	ret
END(fabs)
