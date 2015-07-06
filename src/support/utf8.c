/*
  Basic UTF-8 manipulation routines
  by Jeff Bezanson
  placed in the public domain Fall 2005

  This code is designed to provide the utilities you need to manipulate
  UTF-8 as an internal string encoding. These functions do not perform the
  error checking normally needed when handling UTF-8 data, so if you happen
  to be from the Unicode Consortium you will want to flay me alive.
  I do this because error checking can be performed at the boundaries (I/O),
  with these routines reserved for higher performance on data known to be
  valid.
  A UTF-8 validation routine is included.
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <wchar.h>
#include <wctype.h>

#include "utf8proc.h"
#undef DLLEXPORT /* avoid conflicting definition */

#include "dtypes.h"

#ifdef _OS_WINDOWS_
#include <malloc.h>
#define snprintf _snprintf
#else
#ifndef __FreeBSD__
#include <alloca.h>
#endif /* __FreeBSD__ */
#endif
#include <assert.h>

#include "utf8.h"

#ifdef __cplusplus
extern "C" {
#endif

static const uint32_t offsetsFromUTF8[6] = {
    0x00000000UL, 0x00003080UL, 0x000E2080UL,
    0x03C82080UL, 0xFA082080UL, 0x82082080UL
};

static const char trailingBytesForUTF8[256] = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

/* returns length of next utf-8 sequence */
size_t u8_seqlen(const char *s)
{
    return trailingBytesForUTF8[(unsigned int)(unsigned char)s[0]] + 1;
}

/* returns the # of bytes needed to encode a certain character
   0 means the character cannot (or should not) be encoded. */
size_t u8_charlen(uint32_t ch)
{
    if (ch < 0x80)
        return 1;
    else if (ch < 0x800)
        return 2;
    else if (ch < 0x10000)
        return 3;
    else if (ch < 0x110000)
        return 4;
    return 0;
}

size_t u8_codingsize(uint32_t *wcstr, size_t n)
{
    size_t i, c=0;

    for(i=0; i < n; i++) {
        size_t cl = u8_charlen(wcstr[i]);
        if (cl == 0) cl = 3;  // invalid: encoded as replacement char
        c += cl;
    }
    return c;
}

/* conversions without error checking
   only works for valid UTF-8, i.e. no 5- or 6-byte sequences
   srcsz = source size in bytes
   sz = dest size in # of wide characters

   returns # characters converted
   if sz == srcsz+1 (i.e. 4*srcsz+4 bytes), there will always be enough space.
*/
size_t u8_toucs(uint32_t *dest, size_t sz, const char *src, size_t srcsz)
{
    uint32_t ch;
    const char *src_end = src + srcsz;
    size_t nb;
    size_t i=0;

    if (sz == 0 || srcsz == 0)
        return 0;

    while (i < sz) {
        if (!isutf(*src)) {     // invalid sequence
            dest[i++] = 0xFFFD;
            src++;
            if (src >= src_end) break;
            continue;
        }
        nb = trailingBytesForUTF8[(unsigned char)*src];
        if (src + nb >= src_end)
            break;
        ch = 0;
        switch (nb) {
            /* these fall through deliberately */
        case 5: ch += (unsigned char)*src++; ch <<= 6;
        case 4: ch += (unsigned char)*src++; ch <<= 6;
        case 3: ch += (unsigned char)*src++; ch <<= 6;
        case 2: ch += (unsigned char)*src++; ch <<= 6;
        case 1: ch += (unsigned char)*src++; ch <<= 6;
        case 0: ch += (unsigned char)*src++;
        }
        ch -= offsetsFromUTF8[nb];
        dest[i++] = ch;
    }
    return i;
}

/* srcsz = number of source characters
   sz = size of dest buffer in bytes

   returns # bytes stored in dest
   the destination string will never be bigger than the source string.
*/
size_t u8_toutf8(char *dest, size_t sz, const uint32_t *src, size_t srcsz)
{
    uint32_t ch;
    size_t i = 0;
    char *dest0 = dest;
    char *dest_end = dest + sz;

    while (i < srcsz) {
        ch = src[i];
        if (ch < 0x80) {
            if (dest >= dest_end)
                break;
            *dest++ = (char)ch;
        }
        else if (ch < 0x800) {
            if (dest >= dest_end-1)
                break;
            *dest++ = (ch>>6) | 0xC0;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        else if (ch < 0x10000) {
            if (dest >= dest_end-2)
                break;
            *dest++ = (ch>>12) | 0xE0;
            *dest++ = ((ch>>6) & 0x3F) | 0x80;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        else if (ch < 0x110000) {
            if (dest >= dest_end-3)
                break;
            *dest++ = (ch>>18) | 0xF0;
            *dest++ = ((ch>>12) & 0x3F) | 0x80;
            *dest++ = ((ch>>6) & 0x3F) | 0x80;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        else {
            if (dest >= dest_end-2)
                break;
            // invalid: use replacement char \ufffd
            *dest++ = 0xef;
            *dest++ = 0xbf;
            *dest++ = 0xbd;
        }
        i++;
    }
    return (dest-dest0);
}

size_t u8_wc_toutf8(char *dest, uint32_t ch)
{
    if (ch < 0x80) {
        dest[0] = (char)ch;
        return 1;
    }
    if (ch < 0x800) {
        dest[0] = (ch>>6) | 0xC0;
        dest[1] = (ch & 0x3F) | 0x80;
        return 2;
    }
    if (ch < 0x10000) {
        dest[0] = (ch>>12) | 0xE0;
        dest[1] = ((ch>>6) & 0x3F) | 0x80;
        dest[2] = (ch & 0x3F) | 0x80;
        return 3;
    }
    if (ch < 0x110000) {
        dest[0] = (ch>>18) | 0xF0;
        dest[1] = ((ch>>12) & 0x3F) | 0x80;
        dest[2] = ((ch>>6) & 0x3F) | 0x80;
        dest[3] = (ch & 0x3F) | 0x80;
        return 4;
    }
    dest[0] = 0xef;
    dest[1] = 0xbf;
    dest[2] = 0xbd;
    return 3;
}

/* charnum => byte offset */
size_t u8_offset(const char *s, size_t charnum)
{
    size_t i=0;

    while (charnum > 0) {
        if (s[i++] & 0x80) {
            (void)(isutf(s[++i]) || isutf(s[++i]) || ++i);
        }
        charnum--;
    }
    return i;
}

/* byte offset => charnum */
size_t u8_charnum(const char *s, size_t offset)
{
    size_t charnum = 0;
    if (offset) {
       do {
          // Simply not count continuation bytes
          // Since we are not doing validation anyway, we can just
          // assume this is a valid UTF-8 string
          charnum += ((*(unsigned char *)s++ & 0xc0) != 0x80);
       } while (--offset);
    }
    return charnum;
}

size_t u8_strwidth(const char *s)
{
    uint32_t ch;
    size_t nb, tot=0;
    signed char sc;

    while ((sc = (signed char)*s) != 0) {
        if (sc >= 0) {
            s++;
            if (sc) tot++;
        }
        else {
            if (!isutf(sc)) { tot++; s++; continue; }
            nb = trailingBytesForUTF8[(unsigned char)sc];
            ch = 0;
            switch (nb) {
                /* these fall through deliberately */
            case 5: ch += (unsigned char)*s++; ch <<= 6;
            case 4: ch += (unsigned char)*s++; ch <<= 6;
            case 3: ch += (unsigned char)*s++; ch <<= 6;
            case 2: ch += (unsigned char)*s++; ch <<= 6;
            case 1: ch += (unsigned char)*s++; ch <<= 6;
            case 0: ch += (unsigned char)*s++;
            }
            ch -= offsetsFromUTF8[nb];
            tot += utf8proc_charwidth(ch);
        }
    }
    return tot;
}

/* reads the next utf-8 sequence out of a string, updating an index */
uint32_t u8_nextchar(const char *s, size_t *i)
{
    uint32_t ch = 0;
    size_t sz, j;

    sz = u8_seqlen(&s[*i]);
    for (j = sz; j > 0; j--) {
        ch <<= 6;
        ch += (unsigned char)s[(*i)++];
    }
    ch -= offsetsFromUTF8[sz-1];

    return ch;
}

/* next character without NUL character terminator */
uint32_t u8_nextmemchar(const char *s, size_t *i)
{
    return u8_nextchar(s,i);
}

void u8_inc(const char *s, size_t *i)
{
    (void)(isutf(s[++(*i)]) || isutf(s[++(*i)]) || isutf(s[++(*i)]) || ++(*i));
}

void u8_dec(const char *s, size_t *i)
{
    (void)(isutf(s[--(*i)]) || isutf(s[--(*i)]) || isutf(s[--(*i)]) || --(*i));
}

int octal_digit(char c)
{
    return (c >= '0' && c <= '7');
}

int hex_digit(char c)
{
    return ((c >= '0' && c <= '9') ||
            (c >= 'A' && c <= 'F') ||
            (c >= 'a' && c <= 'f'));
}

char read_escape_control_char(char c)
{
    if (c == 'n')
        return '\n';
    else if (c == 't')
        return '\t';
    else if (c == 'r')
        return '\r';
    else if (c == 'e')
        return '\x1B';
    else if (c == 'b')
        return '\b';
    else if (c == 'f')
        return '\f';
    else if (c == 'v')
        return '\v';
    else if (c == 'a')
        return '\a';
    return c;
}

/* assumes that src points to the character after a backslash
   returns number of input characters processed, 0 if error */
size_t u8_read_escape_sequence(const char *str, size_t ssz, uint32_t *dest)
{
    assert(ssz > 0);
    uint32_t ch;
    char digs[10];
    int dno=0, ndig;
    size_t i=1;
    char c0 = str[0];

    if (octal_digit(c0)) {
        i = 0;
        do {
            digs[dno++] = str[i++];
        } while (i<ssz && octal_digit(str[i]) && dno<3);
        digs[dno] = '\0';
        ch = strtol(digs, NULL, 8);
    }
    else if ((c0=='x' && (ndig=2)) ||
             (c0=='u' && (ndig=4)) ||
             (c0=='U' && (ndig=8))) {
        while (i<ssz && hex_digit(str[i]) && dno<ndig) {
            digs[dno++] = str[i++];
        }
        if (dno == 0) return 0;
        digs[dno] = '\0';
        ch = strtol(digs, NULL, 16);
    }
    else {
        ch = (uint32_t)read_escape_control_char(c0);
    }
    *dest = ch;

    return i;
}

/* convert a string with literal \uxxxx or \Uxxxxxxxx characters to UTF-8
   example: u8_unescape(mybuf, 256, "hello\\u220e")
   note the double backslash is needed if called on a C string literal */
size_t u8_unescape(char *buf, size_t sz, const char *src)
{
    size_t c=0, amt;
    uint32_t ch = 0;
    char temp[4];

    while (*src && c < sz) {
        if (*src == '\\') {
            src++;
            amt = u8_read_escape_sequence(src, 1000, &ch);
        }
        else {
            ch = (uint32_t)*src;
            amt = 1;
        }
        src += amt;
        amt = u8_wc_toutf8(temp, ch);
        if (amt > sz-c)
            break;
        memcpy(&buf[c], temp, amt);
        c += amt;
    }
    if (c < sz)
        buf[c] = '\0';
    return c;
}

static inline int buf_put2c(char *buf, const char *src)
{
    buf[0] = src[0];
    buf[1] = src[1];
    buf[2] = '\0';
    return 2;
}

int u8_escape_wchar(char *buf, size_t sz, uint32_t ch)
{
    assert(sz > 2);
    if (ch == L'\n')
        return buf_put2c(buf, "\\n");
    else if (ch == L'\t')
        return buf_put2c(buf, "\\t");
    else if (ch == L'\r')
        return buf_put2c(buf, "\\r");
    else if (ch == L'\x1B')
        return buf_put2c(buf, "\\e");
    else if (ch == L'\b')
        return buf_put2c(buf, "\\b");
    else if (ch == L'\f')
        return buf_put2c(buf, "\\f");
    else if (ch == L'\v')
        return buf_put2c(buf, "\\v");
    else if (ch == L'\a')
        return buf_put2c(buf, "\\a");
    else if (ch == L'\\')
        return buf_put2c(buf, "\\\\");
    else if (ch < 32 || ch == 0x7f)
        return snprintf(buf, sz, "\\x%.2hhx", (unsigned char)ch);
    else if (ch > 0xFFFF)
        return snprintf(buf, sz, "\\U%.8x", (uint32_t)ch);
    else if (ch >= 0x80)
        return snprintf(buf, sz, "\\u%.4hx", (unsigned short)ch);

    buf[0] = (char)ch;
    buf[1] = '\0';
    return 1;
}

size_t u8_escape(char *buf, size_t sz, const char *src, size_t *pi, size_t end,
                 int escape_quotes, int ascii)
{
    size_t i = *pi, i0;
    uint32_t ch;
    char *start = buf;
    char *blim = start + sz-11;
    assert(sz > 11);

    while (i<end && buf<blim) {
        // sz-11: leaves room for longest escape sequence
        if (escape_quotes && src[i] == '"') {
            buf += buf_put2c(buf, "\\\"");
            i++;
        }
        else if (src[i] == '\\') {
            buf += buf_put2c(buf, "\\\\");
            i++;
        }
        else {
            i0 = i;
            ch = u8_nextmemchar(src, &i);
            if (ascii || !iswprint((wint_t)ch)) {
                buf += u8_escape_wchar(buf, sz - (buf-start), ch);
            }
            else {
                i = i0;
                do {
                    *buf++ = src[i++];
                } while (!isutf(src[i]));
            }
        }
    }
    *buf++ = '\0';
    *pi = i;
    return (buf-start);
}

char *u8_strchr(const char *s, uint32_t ch, size_t *charn)
{
    size_t i = 0, lasti=0;
    uint32_t c;

    *charn = 0;
    while (s[i]) {
        c = u8_nextchar(s, &i);
        if (c == ch) {
            /* it's const for us, but not necessarily the caller */
            return (char*)&s[lasti];
        }
        lasti = i;
        (*charn)++;
    }
    return NULL;
}

char *u8_memchr(const char *s, uint32_t ch, size_t sz, size_t *charn)
{
    size_t i = 0, lasti=0;
    uint32_t c;
    int csz;

    *charn = 0;
    while (i < sz) {
        c = csz = 0;
        do {
            c <<= 6;
            c += (unsigned char)s[i++];
            csz++;
        } while (i < sz && !isutf(s[i]));
        c -= offsetsFromUTF8[csz-1];

        if (c == ch) {
            return (char*)&s[lasti];
        }
        lasti = i;
        (*charn)++;
    }
    return NULL;
}

char *u8_memrchr(const char *s, uint32_t ch, size_t sz)
{
    size_t i = sz-1, tempi=0;
    uint32_t c;

    if (sz == 0) return NULL;

    while (i && !isutf(s[i])) i--;

    while (1) {
        tempi = i;
        c = u8_nextmemchar(s, &tempi);
        if (c == ch) {
            return (char*)&s[i];
        }
        if (i == 0)
            break;
        tempi = i;
        u8_dec(s, &i);
        if (i > tempi)
            break;
    }
    return NULL;
}

int u8_is_locale_utf8(const char *locale)
{
    if (locale == NULL) return 0;

    /* this code based on libutf8 */
    const char *cp = locale;

    for (; *cp != '\0' && *cp != '@' && *cp != '+' && *cp != ',' && *cp != ';'; cp++) {
        if (*cp == '.') {
            const char *encoding = ++cp;
            for (; *cp != '\0' && *cp != '@' && *cp != '+' && *cp != ',' && *cp != ';'; cp++)
                ;
            if ((cp-encoding == 5 && !strncmp(encoding, "UTF-8", 5))
                || (cp-encoding == 4 && !strncmp(encoding, "utf8", 4)))
                return 1; /* it's UTF-8 */
            break;
        }
    }
    return 0;
}

size_t u8_vprintf(const char *fmt, va_list ap)
{
    size_t cnt, sz=0, nc, needfree=0;
    char *buf;
    uint32_t *wcs;

    sz = 512;
    buf = (char*)alloca(sz);
    cnt = vsnprintf(buf, sz, fmt, ap);
    if ((intptr_t)cnt < 0)
        return 0;
    if (cnt >= sz) {
        buf = (char*)malloc(cnt + 1);
        needfree = 1;
        vsnprintf(buf, cnt+1, fmt, ap);
    }
    wcs = (uint32_t*)alloca((cnt+1) * sizeof(uint32_t));
    nc = u8_toucs(wcs, cnt+1, buf, cnt);
    wcs[nc] = 0;
    printf("%ls", (wchar_t*)wcs);
    if (needfree) free(buf);
    return nc;
}

size_t u8_printf(const char *fmt, ...)
{
    size_t cnt;
    va_list args;

    va_start(args, fmt);

    cnt = u8_vprintf(fmt, args);

    va_end(args);
    return cnt;
}

/* Rewritten completely, original code not based on anything else

   length is in bytes, since without knowing whether the string is valid
   it's hard to know how many characters there are! */
int u8_isvalid(const char *str, size_t len)
{
    const unsigned char *pnt;   // Current pointer in string
    const unsigned char *pend;  // End of string
    unsigned char       byt;    // Current byte

    // Empty strings can be considered valid ASCII
    if (!len) return 1;
    pnt = (unsigned char *)str;
    pend = (unsigned char *)str + len;
    // First scan for non-ASCII characters as fast as possible
    do {
        if (*pnt++ & 0x80) goto chkutf8;
    } while (pnt < pend);
    return 1;

    // Check validity of UTF-8 sequences
chkutf8:
    if (pnt == pend) return 0;    // Last byte can't be > 127
    byt = pnt[-1];
    // Must be between 0xc2 and 0xf4 inclusive to be valid
    if (((uint32_t)byt - 0xc2) > (0xf4-0xc2)) return 0;
    if (byt < 0xe0) {               // 2-byte sequence
        // Must have valid continuation character
        if ((*pnt++ & 0xc0) != 0x80) return 0;
    } else if (byt < 0xf0) {        // 3-byte sequence
        if ((pnt + 1 >= pend)
              || (*pnt & 0xc0) != 0x80
              || (pnt[1] & 0xc0) != 0x80)
            return 0;
        // Check for surrogate chars
        if (byt == 0xed && *pnt > 0x9f) return 0;
        pnt += 2;
    } else {                        // 4-byte sequence
        // Must have 3 valid continuation characters
        if ((pnt + 2 >= pend)
              || (*pnt & 0xc0) != 0x80
              || (pnt[1] & 0xc0) != 0x80
              || (pnt[2] & 0xc0) != 0x80)
            return 0;
        // Make sure in correct range (0x10000 - 0x10ffff)
        if (byt == 0xf0) {
            if (*pnt < 0x90) return 0;
        } else if (byt == 0xf4) {
            if (*pnt > 0x8f) return 0;
        }
        pnt += 3;
    }
    // Find next non-ASCII characters as fast as possible
    while (pnt < pend) {
        if (*pnt++ & 0x80) goto chkutf8;
    }
    return 2;   // Valid UTF-8
}

int u8_reverse(char *dest, char *src, size_t len)
{
    size_t si=0, di=len;
    unsigned char c;

    dest[di] = '\0';
    while (si < len) {
        c = (unsigned char)src[si];
        if ((~c) & 0x80) {
            di--;
            dest[di] = c;
            si++;
        }
        else {
            switch (c>>4) {
            case 0xC:
            case 0xD:
                di -= 2;
                *((int16_t*)&dest[di]) = *((int16_t*)&src[si]);
                si += 2;
                break;
            case 0xE:
                di -= 3;
                dest[di] = src[si];
                *((int16_t*)&dest[di+1]) = *((int16_t*)&src[si+1]);
                si += 3;
                break;
            case 0xF:
                di -= 4;
                *((int32_t*)&dest[di]) = *((int32_t*)&src[si]);
                si += 4;
                break;
            default:
                return 1;
            }
        }
    }
    return 0;
}

#ifdef __cplusplus
}
#endif
