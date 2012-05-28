#ifndef READLINE_WIN_H
#define READLINE_WIN_H

#include <stdio.h>
#define fprintf jl_printf
#define fflush (void)
#define putc jl_putc
#define fwrite(ptr,size,count,stream) jl_write(stream,ptr,size*count)
//#define fileno -1//

#endif // WIN_H
