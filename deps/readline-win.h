#ifndef READLINE_WIN_H
#define READLINE_WIN_H

extern void *jl_uv_stderr;
#include <stdio.h>
#define fprintf jl_printf
#undef stderr
#define stderr jl_uv_stderr
#define fflush (void)
#define putc jl_putc
#define fwrite(ptr,size,count,stream) jl_write(stream,ptr,size*count)
//#define fileno -1//

#endif // WIN_H
