#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <alloca.h>
#include "dtypes.h"
#include "utils.h"

void bswap_buffer(byte_t *data, size_t sz, size_t npts)
{
    size_t i, b;
    byte_t *el;
    byte_t temp;

    if (sz <= 1)
        return;

    switch (sz) {
    case 8:
        for(i=0; i < npts; i++) {
            ((u_int64_t*)data)[i] = bswap_64(((u_int64_t*)data)[i]);
        }
        break;
    case 4:
        for(i=0; i < npts; i++) {
            ((u_int32_t*)data)[i] = bswap_32(((u_int32_t*)data)[i]);
        }
        break;
    case 2:
        for(i=0; i < npts; i++) {
            ((u_int16_t*)data)[i] = bswap_16(((u_int16_t*)data)[i]);
        }
        break;
    default:
        for(i=0; i < sz * npts; i += sz) {
            el = data + i;
            for(b=0; b < sz/2; b++) {
                temp = el[b];
                el[b] = el[sz-b-1];
                el[sz-b-1] = temp;
            }
        }
    }
}

void bswap(byte_t *s, size_t n)
{
    unsigned int i;
    char temp;

    switch (n) {
    case 8:
        *(u_int64_t*)s = bswap_64(*(u_int64_t*)s); break;
    case 4:
        *(u_int32_t*)s = bswap_32(*(u_int32_t*)s); break;
    case 2:
        *(u_int16_t*)s = bswap_16(*(u_int16_t*)s); break;
    case 1:
        break;
    default:
        for(i=0; i < n/2; i++) {
            temp = s[i];
            s[i] = s[n-i-1];
            s[n-i-1] = temp;
        }
    }
}

void bswap_to(byte_t *dest, byte_t *src, size_t n)
{
    unsigned int i;

    switch (n) {
    case 8:
        *(u_int64_t*)dest = bswap_64(*(u_int64_t*)src); break;
    case 4:
        *(u_int32_t*)dest = bswap_32(*(u_int32_t*)src); break;
    case 2:
        *(u_int16_t*)dest = bswap_16(*(u_int16_t*)src); break;
    case 1:
        break;
    default:
        for(i=0; i < n; i++) {
            dest[i] = src[n-i-1];
        }
    }
}

