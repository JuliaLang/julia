#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "utf8proc.h"
#undef JL_DLLEXPORT /* avoid conflicting definition */

#include "flisp.h"

#ifdef __cplusplus
extern "C" {
#endif

#define _equal_wchar_(x, y, ctx) ((x) == (y))
#define _hash_wchar_(x, ctx) inthash((uint32_t) ((uintptr_t) (x)))
#include "htable.inc"
HTIMPL_R(wcharhash, _hash_wchar_, _equal_wchar_, _HTIMPL_IDENTITY_KEYALLOC, _HTIMPL_NOOP_KEYFREE)

static int is_uws(uint32_t wc)
{
    return (wc==9 || wc==10 || wc==11 || wc==12 || wc==13 || wc==32 ||
            wc==133 || wc==160 || wc==5760 || wc==6158 || wc==8192 ||
            wc==8193 || wc==8194 || wc==8195 || wc==8196 || wc==8197 ||
            wc==8198 || wc==8199 || wc==8200 || wc==8201 || wc==8202 ||
            wc==8232 || wc==8233 || wc==8239 || wc==8287 || wc==12288);
}

static int is_bom(uint32_t wc)
{
    return wc == 0xFEFF;
}

static int safe_peekutf8(fl_context_t *fl_ctx, ios_t *s, uint32_t *pwc)
{
    int result = ios_peekutf8(s, pwc);
    if (result == 0)
        lerror(fl_ctx, fl_ctx->IOError, "invalid UTF-8 sequence");
    return result;
}

value_t fl_skipws(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "skip-ws", nargs, 2);
    ios_t *s = fl_toiostream(fl_ctx, args[0], "skip-ws");
    int newlines = (args[1]!=fl_ctx->F);
    uint32_t wc=0;
    value_t skipped = fl_ctx->F;
    while (1) {
        if (safe_peekutf8(fl_ctx, s, &wc) == IOS_EOF) {
            ios_getutf8(s, &wc);  // to set EOF flag if this is a true EOF
            if (!ios_eof(s))
                lerror(fl_ctx, symbol(fl_ctx, "error"), "incomplete character");
            return fl_ctx->FL_EOF;
        }
        if (!ios_eof(s) && (is_uws(wc) || is_bom(wc)) && (newlines || wc!=10)) {
            skipped = fl_ctx->T;
            ios_getutf8(s, &wc);
        }
        else {
            break;
        }
    }
    return skipped;
}

// chars that we will never allow to be part of a valid non-operator identifier
static int never_id_char(uint32_t wc)
{
     utf8proc_category_t cat = utf8proc_category((utf8proc_int32_t) wc);
     return (
          // spaces and control characters:
          (cat >= UTF8PROC_CATEGORY_ZS && cat <= UTF8PROC_CATEGORY_CS) ||

          // ASCII and Latin1 non-connector punctuation
          (wc < 0xff &&
           cat >= UTF8PROC_CATEGORY_PD && cat <= UTF8PROC_CATEGORY_PO) ||

          wc == '`' ||

          // mathematical brackets
          (wc >= 0x27e6 && wc <= 0x27ef) ||
          // angle, corner, and lenticular brackets
          (wc >= 0x3008 && wc <= 0x3011) ||
          // tortoise shell, square, and more lenticular brackets
          (wc >= 0x3014 && wc <= 0x301b) ||
          // fullwidth parens
          (wc == 0xff08 || wc == 0xff09) ||
          // fullwidth square brackets
          (wc == 0xff3b || wc == 0xff3d));
}

value_t fl_julia_identifier_char(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "identifier-char?", nargs, 1);
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "identifier-char?", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    return jl_id_char(wc) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_julia_identifier_start_char(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "identifier-start-char?", nargs, 1);
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "identifier-start-char?", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    return jl_id_start_char(wc) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_julia_never_identifier_char(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "never-identifier-char?", nargs, 1);
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "never-identifier-char?", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    return never_id_char(wc) ? fl_ctx->T : fl_ctx->F;
}


value_t fl_julia_op_suffix_char(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "op-suffix-char?", nargs, 1);
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "op-suffix-char?", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    return jl_op_suffix_char(wc) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_julia_strip_op_suffix(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "strip-op-suffix", nargs, 1);
    if (!issymbol(args[0]))
        type_error(fl_ctx, "strip-op-suffix", "symbol", args[0]);
    char *op = symbol_name(fl_ctx, args[0]);
    size_t i = 0;
    while (op[i]) {
        size_t j = i;
        if (jl_op_suffix_char(u8_nextchar(op, &j)))
            break;
        i = j;
    }
    if (!op[i]) return args[0]; // no suffix to strip
    if (!i) return args[0]; // only suffix chars --- might still be a valid identifier
    char *opnew = strncpy((char*)malloc(i+1), op, i);
    // TODO: if argument to opnew == NULL
    opnew[i] = 0;
    value_t opnew_symbol = symbol(fl_ctx, opnew);
    free(opnew);
    return opnew_symbol;
}

/* check whether arg is a symbol that consists solely of underscores. */
value_t fl_julia_underscore_symbolp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "underscore-symbol?", nargs, 1);
    if (!issymbol(args[0])) return fl_ctx->F;
    char *op = symbol_name(fl_ctx, args[0]);
    if (*op == '\0') return fl_ctx->F; // return false for empty symbol
    while (*op == '_') ++op;
    return *op ? fl_ctx->F : fl_ctx->T;
}

#include "julia_charmap.h"

utf8proc_int32_t jl_charmap_map(utf8proc_int32_t c, void *ctx)
{
    static htable_t jl_charmap; // XXX: requires uv_once
    if (!jl_charmap.size) { // initialize hash table
        size_t i, charmap_len = sizeof(charmap) / (2*sizeof(uint32_t));
        htable_t *h = htable_new(&jl_charmap, charmap_len);
        assert(sizeof(uint32_t) <= sizeof(void*));
        for (i = 0; i < charmap_len; ++i) {
            /* Store charmap in a hash table.  Typecasting codepoints
               directly to pointer keys works because pointers are at
               least 32 bits on all Julia-supported systems, and because
               we never map anything to U+0001 (since HT_NOTFOUND is (void*)1). */
            assert((void*)(uintptr_t)charmap[i][1] != HT_NOTFOUND);
            wcharhash_put_r(h, (void*)((uintptr_t)charmap[i][0]),
                               (void*)((uintptr_t)charmap[i][1]), NULL);
        }
    }
    void *v = wcharhash_get_r(&jl_charmap, (void*)((uintptr_t)c), NULL);
    return v == HT_NOTFOUND ? c : (utf8proc_int32_t) ((uintptr_t) v);
}

// return NFC-normalized UTF8-encoded version of s, with
// additional custom normalizations defined by jl_charmap above.
static char *normalize(fl_context_t *fl_ctx, char *s)
{
    // options equivalent to utf8proc_NFC:
    const int options = UTF8PROC_NULLTERM|UTF8PROC_STABLE|UTF8PROC_COMPOSE;
    ssize_t result;
    size_t newlen;
    result = utf8proc_decompose_custom((uint8_t*) s, 0, NULL, 0, (utf8proc_option_t)options,
                                       jl_charmap_map, NULL);
    if (result < 0) goto error;
    newlen = result * sizeof(int32_t) + 1;
    if (newlen > fl_ctx->jlbuflen) {
        fl_ctx->jlbuflen = newlen * 2;
        fl_ctx->jlbuf = realloc(fl_ctx->jlbuf, fl_ctx->jlbuflen);
        if (!fl_ctx->jlbuf) lerror(fl_ctx, fl_ctx->OutOfMemoryError, "error allocating UTF8 buffer");
    }
    result = utf8proc_decompose_custom((uint8_t*)s,0, (int32_t*)fl_ctx->jlbuf,result, (utf8proc_option_t)options,
                                       jl_charmap_map, NULL);
    if (result < 0) goto error;
    result = utf8proc_reencode((int32_t*)fl_ctx->jlbuf,result, (utf8proc_option_t)options);
    if (result < 0) goto error;
    return (char*) fl_ctx->jlbuf;
error:
    lerrorf(fl_ctx, symbol(fl_ctx, "error"), "error normalizing identifier %s: %s", s,
            utf8proc_errmsg(result));
}

value_t fl_accum_julia_symbol(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "accum-julia-symbol", nargs, 2);
    ios_t *s = fl_toiostream(fl_ctx, args[1], "accum-julia-symbol");
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "accum-julia-symbol", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0])); // peek the first character we'll read
    ios_t str;
    int allascii = 1;
    ios_mem(&str, 0);
    do {
        ios_getutf8(s, &wc);
        if (wc == '!') {
            uint32_t nwc = 0;
            ios_peekutf8(s, &nwc);
            // make sure != is always an operator
            if (nwc == '=') {
                ios_skip(s, -1);
                break;
            }
        }
        allascii &= (wc <= 0x7f);
        ios_pututf8(&str, wc);
        if (safe_peekutf8(fl_ctx, s, &wc) == IOS_EOF)
            break;
    } while (jl_id_char(wc));
    ios_pututf8(&str, 0);
    return symbol(fl_ctx, allascii ? str.buf : normalize(fl_ctx, str.buf));
}

/* convert a string to a symbol, first applying normalization */
value_t fl_string2normsymbol(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "string->normsymbol", nargs, 1);
    if (!fl_isstring(fl_ctx, args[0]))
        type_error(fl_ctx, "string->normsymbol", "string", args[0]);
    return symbol(fl_ctx, normalize(fl_ctx, (char*)cvalue_data(args[0])));
}

static uint32_t _iterate_continued(uint8_t *s, size_t n, size_t *i, uint32_t u) {
    if (u < 0xc0000000) { ++*i; return u; }
    uint8_t b;

    if (++*i >= n) return u;
    b = s[*i]; // cont byte 1
    if ((b & 0xc0) != 0x80) return u;
    u |= (uint32_t)b << 16;

    if (++*i >= n || u < 0xe0000000) return u;
    b = s[*i]; // cont byte 2
    if ((b & 0xc0) != 0x80) return u;
    u |= (uint32_t)b << 8;

    if (++*i >= n || u < 0xf0000000) return u;
    b = s[*i]; // cont byte 3
    if ((b & 0xc0) != 0x80) return u;
    u |= (uint32_t)b; ++*i;

    return u;
}

static uint32_t _string_only_julia_char(uint8_t *s, size_t n) {
    if (!(0 < n && n <= 4))
        return -1;
    size_t i = 0;
    uint8_t b = s[i];
    uint32_t u = (uint32_t)b << 24;
    if (0x80 <= b && b <= 0xf7)
        u = _iterate_continued(s, n, &i, u);
    else
        i = 1;
    if (i < n)
        return -1;
    return u;
}

value_t fl_string_only_julia_char(fl_context_t *fl_ctx, value_t *args, uint32_t nargs) {
    argcount(fl_ctx, "string.only-julia-char", nargs, 1);
    if (!fl_isstring(fl_ctx, args[0]))
        type_error(fl_ctx, "string.only-julia-char", "string", args[0]);
    uint8_t *s = (uint8_t*)cvalue_data(args[0]);
    size_t len = cv_len((cvalue_t*)ptr(args[0]));
    uint32_t u = _string_only_julia_char(s, len);
    if (u == UINT32_MAX)
        return fl_ctx->F;
    return fl_list2(fl_ctx, fl_ctx->jl_char_sym, mk_uint32(fl_ctx, u));
}

static const builtinspec_t julia_flisp_func_info[] = {
    { "skip-ws", fl_skipws },
    { "accum-julia-symbol", fl_accum_julia_symbol },
    { "identifier-char?", fl_julia_identifier_char },
    { "identifier-start-char?", fl_julia_identifier_start_char },
    { "never-identifier-char?", fl_julia_never_identifier_char },
    { "op-suffix-char?", fl_julia_op_suffix_char },
    { "strip-op-suffix", fl_julia_strip_op_suffix },
    { "underscore-symbol?", fl_julia_underscore_symbolp },
    { "string->normsymbol", fl_string2normsymbol },
    { "string.only-julia-char", fl_string_only_julia_char },
    { NULL, NULL }
};

void fl_init_julia_extensions(fl_context_t *fl_ctx)
{
    assign_global_builtins(fl_ctx, julia_flisp_func_info);
}

#ifdef __cplusplus
}
#endif
