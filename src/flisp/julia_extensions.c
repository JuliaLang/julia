#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "flisp.h"
#include "mojibake.h"

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

value_t fl_skipws(value_t *args, u_int32_t nargs)
{
    argcount("skip-ws", nargs, 2);
    ios_t *s = fl_toiostream(args[0], "skip-ws");
    int newlines = (args[1]!=FL_F);
    uint32_t wc=0;
    value_t skipped = FL_F;
    while (1) {
        if (ios_peekutf8(s, &wc) == IOS_EOF) {
            ios_getutf8(s, &wc);  // to set EOF flag if this is a true EOF
            if (!ios_eof(s))
                lerror(symbol("error"), "incomplete character");
            return FL_EOF;
        }
        if (!ios_eof(s) && (is_uws(wc) || is_bom(wc)) && (newlines || wc!=10)) {
            skipped = FL_T;
            ios_getutf8(s, &wc);
        }
        else {
            break;
        }
    }
    return skipped;
}

static int is_wc_cat_id_start(uint32_t wc, utf8proc_propval_t cat)
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

DLLEXPORT int jl_id_start_char(uint32_t wc)
{
    if ((wc >= 'A' && wc <= 'Z') || (wc >= 'a' && wc <= 'z') || wc == '_')
        return 1;
    if (wc < 0xA1 || wc > 0x10ffff)
        return 0;
    const utf8proc_property_t *prop = utf8proc_get_property(wc);
    return is_wc_cat_id_start(wc, prop->category);
}

DLLEXPORT int jl_id_char(uint32_t wc)
{
    if ((wc >= 'A' && wc <= 'Z') || (wc >= 'a' && wc <= 'z') || wc == '_' ||
        (wc >= '0' && wc <= '9') || wc == '!')
        return 1;
    if (wc < 0xA1 || wc > 0x10ffff)
        return 0;
    const utf8proc_property_t *prop = utf8proc_get_property(wc);
    utf8proc_propval_t cat = prop->category;
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

value_t fl_julia_identifier_char(value_t *args, u_int32_t nargs)
{
    argcount("identifier-char?", nargs, 1);
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != wchartype)
        type_error("identifier-char?", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    return jl_id_char(wc) ? FL_T : FL_F;
}

value_t fl_julia_identifier_start_char(value_t *args, u_int32_t nargs)
{
    argcount("identifier-start-char?", nargs, 1);
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != wchartype)
        type_error("identifier-start-char?", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    return jl_id_start_char(wc) ? FL_T : FL_F;
}

// return NFC-normalized UTF8-encoded version of s
static char *normalize(char *s)
{
    static size_t buflen = 0;
    static void *buf = NULL; // persistent buffer (avoid repeated malloc/free)
    // options equivalent to utf8proc_NFC:
    const int options = UTF8PROC_NULLTERM|UTF8PROC_STABLE|UTF8PROC_COMPOSE;
    ssize_t result;
    size_t newlen;
    result = utf8proc_decompose((uint8_t*) s, 0, NULL, 0, options);
    if (result < 0) goto error;
    newlen = result * sizeof(int32_t) + 1;
    if (newlen > buflen) {
        buflen = newlen * 2;
        buf = realloc(buf, buflen);
        if (!buf) lerror(MemoryError, "error allocating UTF8 buffer");
    }
    result = utf8proc_decompose((uint8_t*)s,0, (int32_t*)buf,result, options);
    if (result < 0) goto error;
    result = utf8proc_reencode((int32_t*)buf,result, options);
    if (result < 0) goto error;
    return (char*) buf;
error:
    lerrorf(symbol("error"), "error normalizing identifier %s: %s", s,
            utf8proc_errmsg(result));
}

value_t fl_accum_julia_symbol(value_t *args, u_int32_t nargs)
{
    argcount("accum-julia-symbol", nargs, 2);
    ios_t *s = fl_toiostream(args[1], "accum-julia-symbol");
    if (!iscprim(args[0]) || ((cprim_t*)ptr(args[0]))->type != wchartype)
        type_error("accum-julia-symbol", "wchar", args[0]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[0]));
    ios_t str;
    ios_mem(&str, 0);
    while (jl_id_char(wc)) {
        ios_getutf8(s, &wc);
        if (wc == '!') {
            uint32_t nwc;
            ios_peekutf8(s, &nwc);
            // make sure != is always an operator
            if (nwc == '=') {
                ios_ungetc('!', s);
                break;
            }
        }
        ios_pututf8(&str, wc);
        if (ios_peekutf8(s, &wc) == IOS_EOF)
            break;
    }
    ios_pututf8(&str, 0);
    return symbol(normalize(str.buf));
}

static builtinspec_t julia_flisp_func_info[] = {
    { "skip-ws", fl_skipws },
    { "accum-julia-symbol", fl_accum_julia_symbol },
    { "identifier-char?", fl_julia_identifier_char },
    { "identifier-start-char?", fl_julia_identifier_start_char },
    { NULL, NULL }
};

void fl_init_julia_extensions(void)
{
    assign_global_builtins(julia_flisp_func_info);
}

#ifdef __cplusplus
}
#endif
