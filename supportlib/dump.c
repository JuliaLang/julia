#include <stdlib.h>
#include "dtypes.h"
#include "ios.h"
#include "utils.h"

static char hexdig[] = "0123456789abcdef";

/*
  display a given number of bytes from a buffer, with the first
  address label being startoffs
*/
void hexdump(ios_t *dest, const char *buffer, size_t len, size_t startoffs)
{
    size_t offs=0;
    size_t i, pos;
    char ch, linebuffer[16];
    char hexc[4];
    static char *spc50 = "                                                  ";

    hexc[2] = hexc[3] = ' ';
    do {
        ios_printf(dest, "%.8x  ", offs+startoffs);
        pos = 10;
        for(i=0; i < 16 && offs < len; i++, offs++) {
            ch = buffer[offs];
            linebuffer[i] = (ch<32 || ch>=0x7f) ? '.' : ch;
            hexc[0] = hexdig[((unsigned char)ch)>>4];
            hexc[1] = hexdig[ch&0x0f];
            pos += ios_write(dest, hexc, (i==7 || i==15) ? 4 : 3);
        }
        for(; i < 16; i++)
            linebuffer[i] = ' ';
        ios_write(dest, spc50, 60-pos);
        ios_putc('|', dest);
        ios_write(dest, linebuffer, 16);
        ios_write(dest, "|\n", 2);
    } while (offs < len);
}
