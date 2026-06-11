// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Classification of identifier and operator-suffix characters, used both by
  the parser (via libjulia-frontend) and for printing identifiers in the
  runtime. This lives in libsupport so that it is also available to the
  build-time flisp host executable, which cannot link against the runtime.
*/

#include <stdlib.h>
#include <assert.h>

#include "utf8proc.h"
#undef JL_DLLEXPORT /* avoid conflicting definition */

#include "libsupport.h"
#include "htable.h"
#include "hashing.h"

#ifdef __cplusplus
extern "C" {
#endif

#define _equal_wchar_(x, y, ctx) ((x) == (y))
#define _hash_wchar_(x, ctx) inthash((uint32_t) ((uintptr_t) (x)))
#include "htable.inc"
HTIMPL_R(idcharhash, _hash_wchar_, _equal_wchar_, _HTIMPL_IDENTITY_KEYALLOC, _HTIMPL_NOOP_KEYFREE)

static int is_wc_cat_id_start(uint32_t wc, utf8proc_category_t cat)
{
    return (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LL ||
            cat == UTF8PROC_CATEGORY_LT || cat == UTF8PROC_CATEGORY_LM ||
            cat == UTF8PROC_CATEGORY_LO || cat == UTF8PROC_CATEGORY_NL ||
            cat == UTF8PROC_CATEGORY_SC ||  // allow currency symbols
            // other symbols, but not arrows or replacement characters
            (cat == UTF8PROC_CATEGORY_SO && !(wc >= 0x2190 && wc <= 0x21FF) &&
             wc != 0xfffc && wc != 0xfffd &&
             wc != 0x233f &&  // notslash
             wc != 0x00a6) || // broken bar

            // math symbol (category Sm) allowlist
            (wc >= 0x2140 && wc <= 0x2a1c &&
             ((wc >= 0x2140 && wc <= 0x2144) || // ⅀, ⅁, ⅂, ⅃, ⅄
              wc == 0x223f || wc == 0x22be || wc == 0x22bf || // ∿, ⊾, ⊿
              wc == 0x22a4 || wc == 0x22a5 ||   // ⊤ ⊥

              (wc >= 0x2200 && wc <= 0x2233 &&
               (wc == 0x2202 || wc == 0x2205 || wc == 0x2206 || // ∂, ∅, ∆
                wc == 0x2207 || wc == 0x220e || wc == 0x220f || // ∇, ∎, ∏
                wc == 0x2200 || wc == 0x2203 || wc == 0x2204 || // ∀, ∃, ∄
                wc == 0x2210 || wc == 0x2211 || // ∐, ∑
                wc == 0x221e || wc == 0x221f || // ∞, ∟
                wc >= 0x222b)) || // ∫, ∬, ∭, ∮, ∯, ∰, ∱, ∲, ∳

              (wc >= 0x22c0 && wc <= 0x22c3) ||  // N-ary big ops: ⋀, ⋁, ⋂, ⋃
              (wc >= 0x25F8 && wc <= 0x25ff) ||  // ◸, ◹, ◺, ◻, ◼, ◽, ◾, ◿

              (wc >= 0x266f &&
               (wc == 0x266f || wc == 0x27d8 || wc == 0x27d9 || // ♯, ⟘, ⟙
                (wc >= 0x27c0 && wc <= 0x27c1) ||  // ⟀, ⟁
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
            (wc >= 0x309B && wc <= 0x309C) || // katakana-hiragana sound marks

            // bold-digits and double-struck digits
            (wc >= 0x1D7CE && wc <= 0x1D7E1)); // 𝟎 through 𝟗 (inclusive), 𝟘 through 𝟡 (inclusive)
}

JL_DLLEXPORT int jl_id_start_char(uint32_t wc)
{
    if ((wc >= 'A' && wc <= 'Z') || (wc >= 'a' && wc <= 'z') || wc == '_')
        return 1;
    if (wc < 0xA1 || wc > 0x10ffff)
        return 0;
    // "Rightwards Arrow with Lower Hook"
    if (wc == 0x1f8b2)
    	return 1;
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
        // "Rightwards Arrow with Lower Hook"
        wc == 0x1f8b2)
        return 1;
    return 0;
}

#include "julia_opsuffs.h"

// chars that can follow an operator (e.g. +) and be parsed as part of the operator
JL_DLLEXPORT int jl_op_suffix_char(uint32_t wc)
{
    static htable_t jl_opsuffs; // XXX: requires uv_once
    if (!jl_opsuffs.size) { // initialize hash table of suffixes
        size_t i, opsuffs_len = sizeof(opsuffs) / (sizeof(uint32_t));
        htable_t *h = htable_new(&jl_opsuffs, opsuffs_len);
        assert(sizeof(uint32_t) <= sizeof(void*));
        for (i = 0; i < opsuffs_len; ++i)
            idcharhash_put_r(h, (void*)((uintptr_t)opsuffs[i]), NULL, NULL);
    }
    if (wc < 0xA1 || wc > 0x10ffff) return 0;
    utf8proc_category_t cat = utf8proc_category((utf8proc_int32_t) wc);
    if (cat == UTF8PROC_CATEGORY_MN || cat == UTF8PROC_CATEGORY_MC ||
        cat == UTF8PROC_CATEGORY_ME)
        return 1;
    // use hash table of other allowed characters: primes and sub/superscripts
    return HT_NOTFOUND != idcharhash_get_r(&jl_opsuffs, (void*)((uintptr_t)wc), NULL);
}

#ifdef __cplusplus
}
#endif
