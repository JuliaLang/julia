/*
  string functions
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <wchar.h>
#include <wctype.h>
#include <sys/types.h>
#include <errno.h>

#include "flisp.h"

#if !defined(_OS_WINDOWS_)
#include <sys/time.h>
#endif /* !_OS_WINDOWS_ */

#undef JL_DLLEXPORT /* avoid conflicting definition */
#include "utf8proc.h"

#ifdef __cplusplus
extern "C" {
#endif

value_t fl_stringp(value_t *args, u_int32_t nargs)
{
    argcount("string?", nargs, 1);
    return fl_isstring(args[0]) ? FL_T : FL_F;
}

value_t fl_string_count(value_t *args, u_int32_t nargs)
{
    size_t start = 0;
    if (nargs < 1 || nargs > 3)
        argcount("string.count", nargs, 1);
    if (!fl_isstring(args[0]))
        type_error("string.count", "string", args[0]);
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t stop = len;
    if (nargs > 1) {
        start = tosize(args[1], "string.count");
        if (start > len)
            bounds_error("string.count", args[0], args[1]);
        if (nargs > 2) {
            stop = tosize(args[2], "string.count");
            if (stop > len)
                bounds_error("string.count", args[0], args[2]);
            if (stop <= start)
                return fixnum(0);
        }
    }
    char *str = (char*)cvalue_data(args[0]);
    return size_wrap(u8_charnum(str+start, stop-start));
}

extern value_t fl_buffer(value_t *args, u_int32_t nargs);
extern value_t stream_to_string(value_t *ps);

value_t fl_string(value_t *args, u_int32_t nargs)
{
    if (nargs == 1 && fl_isstring(args[0]))
        return args[0];
    value_t arg, buf = fl_buffer(NULL, 0);
    fl_gc_handle(&buf);
    ios_t *s = value2c(ios_t*,buf);
    uint32_t i;
    value_t oldpr = symbol_value(printreadablysym);
    value_t oldpp = symbol_value(printprettysym);
    set(printreadablysym, FL_F);
    set(printprettysym, FL_F);
    FOR_ARGS(i,0,arg,args) {
        fl_print(s, args[i]);
    }
    set(printreadablysym, oldpr);
    set(printprettysym, oldpp);
    value_t outp = stream_to_string(&buf);
    fl_free_gc_handles(1);
    return outp;
}

value_t fl_string_sub(value_t *args, u_int32_t nargs)
{
    if (nargs != 2)
        argcount("string.sub", nargs, 3);
    char *s = tostring(args[0], "string.sub");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i1, i2;
    i1 = tosize(args[1], "string.sub");
    if (i1 > len)
        bounds_error("string.sub", args[0], args[1]);
    if (nargs == 3) {
        i2 = tosize(args[2], "string.sub");
        if (i2 > len)
            bounds_error("string.sub", args[0], args[2]);
    }
    else {
        i2 = len;
    }
    if (i2 <= i1)
        return cvalue_string(0);
    value_t ns = cvalue_string(i2-i1);
    s = (char*)cvalue_data(args[0]); // reload after alloc
    memcpy(cv_data((cvalue_t*)ptr(ns)), &s[i1], i2-i1);
    return ns;
}

value_t fl_string_char(value_t *args, u_int32_t nargs)
{
    argcount("string.char", nargs, 2);
    char *s = tostring(args[0], "string.char");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = tosize(args[1], "string.char");
    if (i >= len)
        bounds_error("string.char", args[0], args[1]);
    size_t sl = u8_seqlen(&s[i]);
    if (sl > len || i > len-sl)
        bounds_error("string.char", args[0], args[1]);
    return mk_wchar(u8_nextchar(s, &i));
}

static value_t mem_find_byte(char *s, char c, size_t start, size_t len)
{
    char *p = (char*)memchr(s+start, c, len-start);
    if (p == NULL)
        return FL_F;
    return size_wrap((size_t)(p - s));
}

value_t fl_string_find(value_t *args, u_int32_t nargs)
{
    char cbuf[8];
    size_t start = 0;
    if (nargs == 3)
        start = tosize(args[2], "string.find");
    else
        argcount("string.find", nargs, 2);
    char *s = tostring(args[0], "string.find");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    if (start > len)
        bounds_error("string.find", args[0], args[2]);
    char *needle; size_t needlesz;

    value_t v = args[1];
    cprim_t *cp = (cprim_t*)ptr(v);
    if (iscprim(v) && cp_class(cp) == wchartype) {
        uint32_t c = *(uint32_t*)cp_data(cp);
        if (c <= 0x7f)
            return mem_find_byte(s, (char)c, start, len);
        needlesz = u8_toutf8(cbuf, sizeof(cbuf), &c, 1);
        needle = cbuf;
    }
    else if (iscprim(v) && cp_class(cp) == bytetype) {
        return mem_find_byte(s, *(char*)cp_data(cp), start, len);
    }
    else if (fl_isstring(v)) {
        cvalue_t *cv = (cvalue_t*)ptr(v);
        needlesz = cv_len(cv);
        needle = (char*)cv_data(cv);
    }
    else {
        type_error("string.find", "string", args[1]);
    }
    if (needlesz > len-start)
        return FL_F;
    else if (needlesz == 1)
        return mem_find_byte(s, needle[0], start, len);
    else if (needlesz == 0)
        return size_wrap(start);
    size_t i;
    for(i=start; i < len-needlesz+1; i++) {
        if (s[i] == needle[0]) {
            if (!memcmp(&s[i+1], needle+1, needlesz-1))
                return size_wrap(i);
        }
    }
    return FL_F;
}

value_t fl_string_inc(value_t *args, u_int32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount("string.inc", nargs, 2);
    char *s = tostring(args[0], "string.inc");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = tosize(args[1], "string.inc");
    size_t cnt = 1;
    if (nargs == 3)
        cnt = tosize(args[2], "string.inc");
    while (cnt--) {
        if (i >= len)
            bounds_error("string.inc", args[0], args[1]);
        (void)(isutf(s[++i]) || isutf(s[++i]) || isutf(s[++i]) || ++i);
    }
    return size_wrap(i);
}

value_t fl_string_dec(value_t *args, u_int32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount("string.dec", nargs, 2);
    char *s = tostring(args[0], "string.dec");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    size_t i = tosize(args[1], "string.dec");
    size_t cnt = 1;
    if (nargs == 3)
        cnt = tosize(args[2], "string.dec");
    // note: i is allowed to start at index len
    if (i > len)
        bounds_error("string.dec", args[0], args[1]);
    while (cnt--) {
        if (i == 0)
            bounds_error("string.dec", args[0], args[1]);
        (void)(isutf(s[--i]) || isutf(s[--i]) || isutf(s[--i]) || --i);
    }
    return size_wrap(i);
}

static unsigned long get_radix_arg(value_t arg, char *fname)
{
    unsigned long radix = (unsigned long)tosize(arg, fname);
    if (radix < 2 || radix > 36)
        lerrorf(ArgError, "%s: invalid radix", fname);
    return radix;
}

value_t fl_numbertostring(value_t *args, u_int32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount("number->string", nargs, 2);
    value_t n = args[0];
    int neg = 0;
    uint64_t num;
    if (isfixnum(n))      num = numval(n);
    else if (!iscprim(n)) type_error("number->string", "integer", n);
    else num = conv_to_uint64(cp_data((cprim_t*)ptr(n)),
                              cp_numtype((cprim_t*)ptr(n)));
    if (numval(fl_compare(args[0],fixnum(0))) < 0) {
        num = -num;
        neg = 1;
    }
    unsigned long radix = 10;
    if (nargs == 2)
        radix = get_radix_arg(args[1], "number->string");
    char buf[128];
    char *str = uint2str(buf, sizeof(buf), num, radix);
    if (neg && str > &buf[0])
        *(--str) = '-';
    return string_from_cstr(str);
}

value_t fl_stringtonumber(value_t *args, uint32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount("string->number", nargs, 2);
    char *str = tostring(args[0], "string->number");
    value_t n;
    unsigned long radix = 0;
    if (nargs == 2)
        radix = get_radix_arg(args[1], "string->number");
    if (!isnumtok_base(str, &n, (int)radix))
        return FL_F;
    return n;
}

value_t fl_string_isutf8(value_t *args, u_int32_t nargs)
{
    argcount("string.isutf8", nargs, 1);
    char *s = tostring(args[0], "string.isutf8");
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    return u8_isvalid(s, len) ? FL_T : FL_F;
}

static builtinspec_t stringfunc_info[] = {
    { "string", fl_string },
    { "string?", fl_stringp },
    { "string.count", fl_string_count },
    { "string.sub", fl_string_sub },
    { "string.find", fl_string_find },
    { "string.char", fl_string_char },
    { "string.inc", fl_string_inc },
    { "string.dec", fl_string_dec },
    { "string.isutf8", fl_string_isutf8 },

    { "number->string", fl_numbertostring },
    { "string->number", fl_stringtonumber },

    { NULL, NULL }
};

void stringfuncs_init(void)
{
    assign_global_builtins(stringfunc_info);
}

#ifdef __cplusplus
}
#endif
