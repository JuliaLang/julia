#include <stdlib.h>
#include <cairo/cairo.h>
#include "julia.h"

cairo_status_t
cairo_write_to_ios_callback(void *v, const unsigned char *buf, unsigned int len)
{
    ios_t *s = (ios_t *) v;
    size_t n = ios_write(s, buf, len);
    return (n == len) ? CAIRO_STATUS_SUCCESS : CAIRO_STATUS_WRITE_ERROR;
}
