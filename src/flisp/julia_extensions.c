#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "flisp.h"

static int is_uws(uint32_t wc)
{
    return (wc==9 || wc==10 || wc==11 || wc==12 || wc==13 || wc==32 ||
            wc==133 || wc==160 || wc==5760 || wc==6158 || wc==8192 ||
            wc==8193 || wc==8194 || wc==8195 || wc==8196 || wc==8197 ||
            wc==8198 || wc==8199 || wc==8200 || wc==8201 || wc==8202 ||
            wc==8232 || wc==8233 || wc==8239 || wc==8287 || wc==12288);
}

value_t fl_skipws(value_t *args, u_int32_t nargs)
{
    argcount("skip-ws", nargs, 2);
    ios_t *s = fl_toiostream(args[0], "skip-ws");
    int newlines = (args[1]!=FL_F);
    uint32_t wc=0;
    if (ios_peekutf8(s, &wc) == IOS_EOF)
        return FL_EOF;
    value_t skipped = FL_F;
    while (!ios_eof(s) && is_uws(wc) && (newlines || wc!=10)) {
        skipped = FL_T;
        ios_getutf8(s, &wc);
        ios_peekutf8(s, &wc);
    }
    return skipped;
}

static int jl_id_char(uint32_t wc)
{
    return ((wc >= 'A' && wc <= 'Z') || (wc >= 'a' && wc <= 'z') ||
            (wc >= '0' && wc <= '9') || (wc >= 0xA1) ||
            wc == '!' || wc == '_');
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
    return symbol(str.buf);
}

static builtinspec_t julia_flisp_func_info[] = {
    { "skip-ws", fl_skipws },
    { "accum-julia-symbol", fl_accum_julia_symbol },
    { NULL, NULL }
};

void fl_init_julia_extensions(void)
{
    assign_global_builtins(julia_flisp_func_info);
}
