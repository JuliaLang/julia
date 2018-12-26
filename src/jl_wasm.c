#include "julia.h"
#include <stdio.h>

JL_DLLEXPORT int jl_printf(JL_STREAM *s, const char *format, ...)
{
    va_list args;
    int c;

    va_start(args, format);
	c = vprintf(format, args);
    va_end(args);
    return c;
}
JL_DLLEXPORT int jl_vprintf(JL_STREAM *s, const char *format, va_list args)
{
    int c;
	va_list _args;
	va_copy(_args, args);
	c = vprintf(format, _args);
    va_end(_args);
    return c;
}

JL_DLLEXPORT void jl_uv_puts(JL_STREAM *stream, const char *str, size_t n)
{
    assert(stream);
		
		int fd = -1;
		// Fallback for output during early initialisation...
    if (stream == (void*)STDOUT_FILENO) {
        fd = STDOUT_FILENO;
    }
    else if (stream == (void*)STDERR_FILENO) {
        fd = STDERR_FILENO;
    }

    if ((ssize_t)fd != -1) {
        // Write to file descriptor...
        write(fd, str, n);
				return;
    }
		
    ios_write((ios_t*)stream, str, n);
}

JL_DLLEXPORT void jl_uv_putb(JL_STREAM *stream, uint8_t b)
{
    jl_uv_puts(stream, (char*)&b, 1);
}

JL_DLLEXPORT void JL_NORETURN jl_exit(int status)
{
    return exit(status);
}
