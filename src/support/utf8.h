// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_UTF8_H
#define JL_UTF8_H

#ifdef __cplusplus
extern "C" {
#endif

#include "analyzer_annotations.h"

/* is c the start of a utf8 sequence? */
#define isutf(c) (((c)&0xC0)!=0x80)

#define UEOF ((uint32_t)-1)

/* convert UTF-8 data to wide character */
size_t u8_toucs(uint32_t *dest, size_t sz, const char *src, size_t srcsz);

/* the opposite conversion */
size_t u8_toutf8(char *dest, size_t sz, const uint32_t *src, size_t srcsz);

/* single character to UTF-8, returns # bytes written */
size_t u8_wc_toutf8(char *dest, uint32_t ch);

/* character number to byte offset */
JL_DLLEXPORT size_t u8_offset(const char *str, size_t charnum);

/* byte offset to character number */
JL_DLLEXPORT size_t u8_charnum(const char *str, size_t offset);

/* return next character, updating an index variable */
uint32_t u8_nextchar(const char *s, size_t *i) JL_NOTSAFEPOINT;

/* next character without NUL character terminator */
uint32_t u8_nextmemchar(const char *s, size_t *i);

/* move to next character */
void u8_inc(const char *s, size_t *i);

/* move to previous character */
void u8_dec(const char *s, size_t *i);

/* returns length of next utf-8 sequence */
size_t u8_seqlen(const char *s);

/* returns the # of bytes needed to encode a certain character */
size_t u8_charlen(uint32_t ch);

char read_escape_control_char(char c);

/* assuming src points to the character after a backslash, read an
   escape sequence, storing the result in dest and returning the number of
   input characters processed */
size_t u8_read_escape_sequence(const char *src, size_t ssz, uint32_t *dest);

/* given a wide character, convert it to an ASCII escape sequence stored in
   buf, where buf is "sz" bytes. returns the number of characters output.
   sz must be at least 3. */
int u8_escape_wchar(char *buf, size_t sz, uint32_t ch);

/* convert UTF-8 "src" to escape sequences.

   sz is buf size in bytes. must be at least 12.

   if escape_quotes is nonzero, quote characters will be escaped.

   if ascii is nonzero, the output is 7-bit ASCII, no UTF-8 survives.

   starts at src[*pi], updates *pi to point to the first unprocessed
   byte of the input.

   end is one more than the last allowable value of *pi.

   returns number of bytes placed in buf, including a NUL terminator.
*/
size_t u8_escape(char *buf, size_t sz, const char *src, size_t *pi, size_t end,
                 int escape_quotes, int ascii);

/* utility predicates used by the above */
int octal_digit(char c);
int hex_digit(char c);

/* same as the above, but searches a buffer of a given size instead of
   a NUL-terminated string. */
char *u8_memchr(const char *s, uint32_t ch, size_t sz, size_t *charn);

char *u8_memrchr(const char *s, uint32_t ch, size_t sz);

/* number of columns occupied by a string */
JL_DLLEXPORT size_t u8_strwidth(const char *s);

/* printf where the format string and arguments may be in UTF-8.
   you can avoid this function and just use ordinary printf() if the current
   locale is UTF-8. */
size_t u8_vprintf(const char *fmt, va_list ap);
size_t u8_printf(const char *fmt, ...);

/* determine whether a sequence of bytes is valid UTF-8. length is in bytes */
JL_DLLEXPORT int u8_isvalid(const char *str, size_t length);

#ifdef __cplusplus
}
#endif

#endif
