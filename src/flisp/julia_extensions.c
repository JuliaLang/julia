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

value_t fl_skipws(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "skip-ws", nargs, 2);
    ios_t *s = fl_toiostream(fl_ctx, args[0], "skip-ws");
    int newlines = (args[1]!=fl_ctx->F);
    uint32_t wc=0;
    value_t skipped = fl_ctx->F;
    while (1) {
        if (ios_peekutf8(s, &wc) == IOS_EOF) {
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

static int is_wc_cat_id_start(uint32_t wc, utf8proc_category_t cat)
{
    return (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LL ||
            cat == UTF8PROC_CATEGORY_LT || cat == UTF8PROC_CATEGORY_LM ||
            cat == UTF8PROC_CATEGORY_LO || cat == UTF8PROC_CATEGORY_NL ||
            cat == UTF8PROC_CATEGORY_SC ||  // allow currency symbols
            cat == UTF8PROC_CATEGORY_SO ||  // other symbols

            // math symbol (category Sm) whitelist
            (wc >= 0x2140 && wc <= 0x2a1c &&
             ((wc >= 0x2140 && wc <= 0x2144) || // ⅀, ⅁, ⅂, ⅃, ⅄
              wc == 0x223f || wc == 0x22be || wc == 0x22bf || // ∿, ⊾, ⊿
              wc == 0x22a4 || wc == 0x22a5 ||   // ⊤ ⊥
              (wc >= 0x22ee && wc <= 0x22f1) || // ⋮, ⋯, ⋰, ⋱

              (wc >= 0x2202 && wc <= 0x2233 &&
               (wc == 0x2202 || wc == 0x2205 || wc == 0x2206 || // ∂, ∅, ∆
                wc == 0x2207 || wc == 0x220e || wc == 0x220f || // ∇, ∎, ∏
                wc == 0x2210 || wc == 0x2211 || // ∐, ∑
                wc == 0x221e || wc == 0x221f || // ∞, ∟
                wc >= 0x222b)) || // ∫, ∬, ∭, ∮, ∯, ∰, ∱, ∲, ∳

              (wc >= 0x22c0 && wc <= 0x22c3) ||  // N-ary big ops: ⋀, ⋁, ⋂, ⋃
              (wc >= 0x25F8 && wc <= 0x25ff) ||  // ◸, ◹, ◺, ◻, ◼, ◽, ◾, ◿

              (wc >= 0x266f &&
               (wc == 0x266f || wc == 0x27d8 || wc == 0x27d9 || // ♯, ⟘, ⟙
                (wc >= 0x27c0 && wc <= 0x27c2) ||  // ⟀, ⟁, ⟂
                (wc >= 0x29b0 && wc <= 0x29b4) ||  // ⦰, ⦱, ⦲, ⦳, ⦴
                (wc >= 0x2a00 && wc <= 0x2a06) ||  // ⨀, ⨁, ⨂, ⨃, ⨄, ⨅, ⨆
                (wc >= 0x2a09 && wc <= 0x2a16) ||  // ⨉, ⨊, ⨋, ⨌, ⨍, ⨎, ⨏, ⨐, ⨑, ⨒, ⨓, ⨔, ⨕, ⨖
                wc == 0x2a1b || wc == 0x2a1c)))) || // ⨛, ⨜

            (wc >= 0x1d6c1 && // variants of \nabla and \partial
             (wc == 0x1d6c1 || wc == 0x1d6db ||
              wc == 0x1d6fb || wc == 0x1d715 ||
              wc == 0x1d735 || wc == 0x1d74f ||
              wc == 0x1d76f || wc == 0x1d789 ||
              wc == 0x1d7a9 || wc == 0x1d7c3)) ||

            // super- and subscript +-=()
            (wc >= 0x207a && wc <= 0x207e) ||
            (wc >= 0x208a && wc <= 0x208e) ||

            // angle symbols
            (wc >= 0x2220 && wc <= 0x2222) || // ∠, ∡, ∢
            (wc >= 0x299b && wc <= 0x29af) || // ⦛, ⦜, ⦝, ⦞, ⦟, ⦠, ⦡, ⦢, ⦣, ⦤, ⦥, ⦦, ⦧, ⦨, ⦩, ⦪, ⦫, ⦬, ⦭, ⦮, ⦯

            // Other_ID_Start
            wc == 0x2118 || wc == 0x212E || // ℘, ℮
            (wc >= 0x309B && wc <= 0x309C)); // katakana-hiragana sound marks
}

JL_DLLEXPORT int jl_id_start_char(uint32_t wc)
{
    if ((wc >= 'A' && wc <= 'Z') || (wc >= 'a' && wc <= 'z') || wc == '_')
        return 1;
    if (wc < 0xA1 || wc > 0x10ffff)
        return 0;
    return is_wc_cat_id_start(wc, utf8proc_category((utf8proc_int32_t) wc));
}

JL_DLLEXPORT int jl_id_char(uint32_t wc)
{
    if ((wc >= 'A' && wc <= 'Z') || (wc >= 'a' && wc <= 'z') || wc == '_' ||
        (wc >= '0' && wc <= '9') || wc == '!')
        return 1;
    if (wc < 0xA1 || wc > 0x10ffff)
        return 0;
    utf8proc_category_t cat = utf8proc_category((utf8proc_int32_t) wc);
    if (is_wc_cat_id_start(wc, cat)) return 1;
    if (cat == UTF8PROC_CATEGORY_MN || cat == UTF8PROC_CATEGORY_MC ||
        cat == UTF8PROC_CATEGORY_ND || cat == UTF8PROC_CATEGORY_PC ||
        cat == UTF8PROC_CATEGORY_SK || cat == UTF8PROC_CATEGORY_ME ||
        cat == UTF8PROC_CATEGORY_NO ||
        // primes (single, double, triple, their reverses, and quadruple)
        (wc >= 0x2032 && wc <= 0x2037) || (wc == 0x2057) ||
        // Other_ID_Continue
        wc == 0x0387 || wc == 0x19da || (wc >= 0x1369 && wc <= 0x1371))
        return 1;
    return 0;
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

#include "julia_charmap.h"
#define _equal_wchar_(x, y, ctx) ((x) == (y))
#define _hash_wchar_(x, ctx) inthash((uint32_t) ((uintptr_t) (x)))
#include "htable.inc"
HTIMPL_R(wcharhash, _hash_wchar_, _equal_wchar_)

void jl_charmap_init(fl_context_t *fl_ctx)
{
    size_t charmap_len = sizeof(charmap) / (2*sizeof(uint32_t));
    size_t i;
    htable_t *h = htable_new(&fl_ctx->jl_charmap, charmap_len);
    assert(sizeof(uint32_t) <= sizeof(void*));
    for (i = 0; i < charmap_len; ++i) {
        /* Store charmap in a hash table.  Typecasting codepoints
           directly to pointer keys works because pointers are at
           least 32 bits on all Julia-supported systems, and because
           we never map anything to U+0001 (since HT_NOTFOUND is (void*)1). */
        assert((void*)charmap[i][1] != HT_NOTFOUND);
        wcharhash_put_r(h, (void*)((uintptr_t)charmap[i][0]),
                           (void*)((uintptr_t)charmap[i][1]), (void*)fl_ctx);
    }
}
utf8proc_int32_t jl_charmap_map(utf8proc_int32_t c, void *fl_ctx_)
{
    fl_context_t *fl_ctx = (fl_context_t *) fl_ctx_;
    htable_t *h = &fl_ctx->jl_charmap;
    void *v = wcharhash_get_r(h, (void*)((uintptr_t)c), (void*) fl_ctx);
    return v == HT_NOTFOUND ? c : (utf8proc_int32_t) ((uintptr_t) v);
}

value_t fl_julia_normalize_char(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "normalize-char", nargs, 1);
    if (fl_ctx->FL_EOF == args[0])
        return args[0];
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "normalize-char", "wchar", args[0]);
    int32_t wc = jl_charmap_map(*(int32_t*)cp_data((cprim_t*)ptr(args[0])), fl_ctx);
    return mk_wchar(fl_ctx, wc);
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
                                       jl_charmap_map, (void*) fl_ctx);
    if (result < 0) goto error;
    newlen = result * sizeof(int32_t) + 1;
    if (newlen > fl_ctx->jlbuflen) {
        fl_ctx->jlbuflen = newlen * 2;
        fl_ctx->jlbuf = realloc(fl_ctx->jlbuf, fl_ctx->jlbuflen);
        if (!fl_ctx->jlbuf) lerror(fl_ctx, fl_ctx->OutOfMemoryError, "error allocating UTF8 buffer");
    }
    result = utf8proc_decompose_custom((uint8_t*)s,0, (int32_t*)fl_ctx->jlbuf,result, (utf8proc_option_t)options,
                                       jl_charmap_map, (void*) fl_ctx);
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
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    ios_t str;
    int allascii=1;
    ios_mem(&str, 0);
    do {
        allascii &= (wc <= 0x7f);
        ios_getutf8(s, &wc);
        if (jl_charmap_map((int32_t)wc,fl_ctx) == '!') {
            uint32_t nwc;
            ios_peekutf8(s, &nwc);
            // make sure != is always an operator
            if (jl_charmap_map((int32_t)nwc,fl_ctx) == '=') {
                ios_ungetc('!', s);
                break;
            }
        }
        ios_pututf8(&str, wc);
        if (ios_peekutf8(s, &wc) == IOS_EOF)
            break;
    } while (jl_id_char(wc));
    ios_pututf8(&str, 0);
    return symbol(fl_ctx, allascii ? str.buf : normalize(fl_ctx, str.buf));
}

static const builtinspec_t julia_flisp_func_info[] = {
    { "skip-ws", fl_skipws },
    { "accum-julia-symbol", fl_accum_julia_symbol },
    { "identifier-char?", fl_julia_identifier_char },
    { "identifier-start-char?", fl_julia_identifier_start_char },
    { "normalize-char", fl_julia_normalize_char },
    { NULL, NULL }
};

void fl_init_julia_extensions(fl_context_t *fl_ctx)
{
    assign_global_builtins(fl_ctx, julia_flisp_func_info);
}

#ifdef __cplusplus
}
#endif
