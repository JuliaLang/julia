#include "julia.h" 
#include <stdio.h>

JL_DLLEXPORT int jl_printf(JL_STREAM *s, const char *format, ...)
{
    va_list args;
    int c;

    va_start(args, format);
	vprintf(format, args);
    va_end(args);
    return c;
}

JL_DLLEXPORT void JL_NORETURN jl_exit(int status)
{
    return exit(status);
}
