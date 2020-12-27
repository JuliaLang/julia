extern void *memrchr(const void *s, int c, size_t n);

static void outc(fl_context_t *fl_ctx, char c, ios_t *f)
{
    ios_putc(c, f);
    if (c == '\n')
        fl_ctx->HPOS = 0;
    else
        fl_ctx->HPOS++;
}
static void outs(fl_context_t *fl_ctx, const char *s, ios_t *f)
{
    ios_puts(s, f);
    fl_ctx->HPOS += u8_strwidth(s);
}
static void outsn(fl_context_t *fl_ctx, const char *s, ios_t *f, size_t n)
{
    ios_write(f, s, n);
    fl_ctx->HPOS += u8_strwidth(s);
}
static int outindent(fl_context_t *fl_ctx, int n, ios_t *f)
{
    // move back to left margin if we get too indented
    if (n > fl_ctx->SCR_WIDTH-12)
        n = 2;
    int n0 = n;
    ios_putc('\n', f);
    fl_ctx->VPOS++;
    fl_ctx->HPOS = n;
    while (n >= 8) {
        ios_putc('\t', f);
        n -= 8;
    }
    while (n) {
        ios_putc(' ', f);
        n--;
    }
    return n0;
}

void fl_print_chr(fl_context_t *fl_ctx, char c, ios_t *f)
{
    outc(fl_ctx, c, f);
}

void fl_print_str(fl_context_t *fl_ctx, const char *s, ios_t *f)
{
    outs(fl_ctx, s, f);
}

void print_traverse(fl_context_t *fl_ctx, value_t v)
{
    value_t *bp;
    while (iscons(v)) {
        if (ismarked(fl_ctx, v)) {
            bp = (value_t*)ptrhash_bp(&fl_ctx->printconses, (void*)v);
            if (*bp == (value_t)HT_NOTFOUND)
                *bp = fixnum(fl_ctx->printlabel++);
            return;
        }
        mark_cons(fl_ctx, v);
        print_traverse(fl_ctx, car_(v));
        v = cdr_(v);
    }
    if (!ismanaged(fl_ctx, v) || issymbol(v))
        return;
    if (ismarked(fl_ctx, v)) {
        bp = (value_t*)ptrhash_bp(&fl_ctx->printconses, (void*)v);
        if (*bp == (value_t)HT_NOTFOUND)
            *bp = fixnum(fl_ctx->printlabel++);
        return;
    }
    if (isvector(v)) {
        if (vector_size(v) > 0)
            mark_cons(fl_ctx, v);
        unsigned int i;
        for(i=0; i < vector_size(v); i++)
            print_traverse(fl_ctx, vector_elt(v,i));
    }
    else if (iscprim(v)) {
        mark_cons(fl_ctx, v);
    }
    else if (isclosure(v)) {
        mark_cons(fl_ctx, v);
        function_t *f = (function_t*)ptr(v);
        print_traverse(fl_ctx, f->bcode);
        print_traverse(fl_ctx, f->vals);
        print_traverse(fl_ctx, f->env);
    }
    else {
        assert(iscvalue(v));
        cvalue_t *cv = (cvalue_t*)ptr(v);
        // don't consider shared references to ""
        if (!cv_isstr(fl_ctx, cv) || cv_len(cv)!=0)
            mark_cons(fl_ctx, v);
        fltype_t *t = cv_class(cv);
        if (t->vtable != NULL && t->vtable->print_traverse != NULL)
            t->vtable->print_traverse(fl_ctx, v);
    }
}

static void print_symbol_name(fl_context_t *fl_ctx, ios_t *f, char *name)
{
    int i, escape=0, charescape=0;

    if ((name[0] == '\0') ||
        (name[0] == '.' && name[1] == '\0') ||
        (name[0] == '#') ||
        isnumtok(fl_ctx, name, NULL))
        escape = 1;
    i=0;
    while (name[i]) {
        if (!symchar(name[i])) {
            escape = 1;
            if (name[i]=='|' || name[i]=='\\') {
                charescape = 1;
                break;
            }
        }
        i++;
    }
    if (escape) {
        if (charescape) {
            outc(fl_ctx, '|', f);
            i=0;
            while (name[i]) {
                if (name[i]=='|' || name[i]=='\\')
                    outc(fl_ctx, '\\', f);
                outc(fl_ctx, name[i], f);
                i++;
            }
            outc(fl_ctx, '|', f);
        }
        else {
            outc(fl_ctx, '|', f);
            outs(fl_ctx, name, f);
            outc(fl_ctx, '|', f);
        }
    }
    else {
        outs(fl_ctx, name, f);
    }
}

/*
  The following implements a simple pretty-printing algorithm. This is
  an unlimited-width approach that doesn't require an extra pass.
  It uses some heuristics to guess whether an expression is "small",
  and avoids wrapping symbols across lines. The result is high
  performance and nice output for typical code. Quality is poor for
  pathological or deeply-nested expressions, but those are difficult
  to print anyway.
*/
#define SMALL_STR_LEN 20
static inline int tinyp(fl_context_t *fl_ctx, value_t v)
{
    if (issymbol(v))
        return (u8_strwidth(symbol_name(fl_ctx, v)) < SMALL_STR_LEN);
    if (fl_isstring(fl_ctx, v))
        return (cv_len((cvalue_t*)ptr(v)) < SMALL_STR_LEN);
    return (isfixnum(v) || isbuiltin(v) || v==fl_ctx->F || v==fl_ctx->T || v==fl_ctx->NIL ||
            v == fl_ctx->FL_EOF);
}

static int smallp(fl_context_t *fl_ctx, value_t v)
{
    if (tinyp(fl_ctx, v)) return 1;
    if (fl_isnumber(fl_ctx, v)) return 1;
    if (iscons(v)) {
        if (tinyp(fl_ctx, car_(v)) && (tinyp(fl_ctx, cdr_(v)) ||
                               (iscons(cdr_(v)) && tinyp(fl_ctx, car_(cdr_(v))) &&
                                cdr_(cdr_(v))==fl_ctx->NIL)))
            return 1;
        return 0;
    }
    if (isvector(v)) {
        size_t s = vector_size(v);
        return (s == 0 || (tinyp(fl_ctx, vector_elt(v,0)) &&
                           (s == 1 || (s == 2 &&
                                       tinyp(fl_ctx, vector_elt(v,1))))));
    }
    return 0;
}

static int specialindent(fl_context_t *fl_ctx, value_t head)
{
    // indent these forms 2 spaces, not lined up with the first argument
    if (head == fl_ctx->LAMBDA || head == fl_ctx->TRYCATCH || head == fl_ctx->definesym ||
        head == fl_ctx->defmacrosym || head == fl_ctx->forsym)
        return 2;
    return -1;
}

static int lengthestimate(fl_context_t *fl_ctx, value_t v)
{
    // get the width of an expression if we can do so cheaply
    if (issymbol(v))
        return u8_strwidth(symbol_name(fl_ctx, v));
    return -1;
}

static int allsmallp(fl_context_t *fl_ctx, value_t v)
{
    int n = 1;
    while (iscons(v)) {
        if (!smallp(fl_ctx, car_(v)))
            return 0;
        v = cdr_(v);
        n++;
        if (n > 25)
            return n;
    }
    return n;
}

static int indentafter3(fl_context_t *fl_ctx, value_t head, value_t v)
{
    // for certain X always indent (X a b c) after b
    return ((head == fl_ctx->forsym) && !allsmallp(fl_ctx, cdr_(v)));
}

static int indentafter2(fl_context_t *fl_ctx, value_t head, value_t v)
{
    // for certain X always indent (X a b) after a
    return ((head == fl_ctx->definesym || head == fl_ctx->defmacrosym) &&
            !allsmallp(fl_ctx, cdr_(v)));
}

static int indentevery(fl_context_t *fl_ctx, value_t v)
{
    // indent before every subform of a special form, unless every
    // subform is "small"
    value_t c = car_(v);
    if (c == fl_ctx->LAMBDA || c == fl_ctx->setqsym)
        return 0;
    if (c == fl_ctx->IF) // TODO: others
        return !allsmallp(fl_ctx, cdr_(v));
    return 0;
}

static int blockindent(fl_context_t *fl_ctx, value_t v)
{
    // in this case we switch to block indent mode, where the head
    // is no longer considered special:
    // (a b c d e
    //  f g h i j)
    return (allsmallp(fl_ctx, v) > 9);
}

static void print_pair(fl_context_t *fl_ctx, ios_t *f, value_t v)
{
    value_t cd;
    char *op = NULL;
    if (iscons(cdr_(v)) && cdr_(cdr_(v)) == fl_ctx->NIL &&
        !ptrhash_has(&fl_ctx->printconses, (void*)cdr_(v)) &&
        (((car_(v) == fl_ctx->QUOTE)     && (op = "'"))  ||
         ((car_(v) == fl_ctx->BACKQUOTE) && (op = "`"))  ||
         ((car_(v) == fl_ctx->COMMA)     && (op = ","))  ||
         ((car_(v) == fl_ctx->COMMAAT)   && (op = ",@")) ||
         ((car_(v) == fl_ctx->COMMADOT)  && (op = ",.")))) {
        // special prefix syntax
        unmark_cons(fl_ctx, v);
        unmark_cons(fl_ctx, cdr_(v));
        outs(fl_ctx, op, f);
        fl_print_child(fl_ctx, f, car_(cdr_(v)));
        return;
    }
    int startpos = fl_ctx->HPOS;
    outc(fl_ctx, '(', f);
    int newindent=fl_ctx->HPOS, blk=blockindent(fl_ctx, v);
    int lastv, n=0, si, ind=0, est, always=0, nextsmall, thistiny;
    if (!blk) always = indentevery(fl_ctx, v);
    value_t head = car_(v);
    int after3 = indentafter3(fl_ctx, head, v);
    int after2 = indentafter2(fl_ctx, head, v);
    int n_unindented = 1;
    while (1) {
        cd = cdr_(v);
        if (fl_ctx->print_length >= 0 && n >= fl_ctx->print_length && cd!=fl_ctx->NIL) {
            outsn(fl_ctx, "...)", f, 4);
            break;
        }
        lastv = fl_ctx->VPOS;
        unmark_cons(fl_ctx, v);
        fl_print_child(fl_ctx, f, car_(v));
        if (!iscons(cd) || ptrhash_has(&fl_ctx->printconses, (void*)cd)) {
            if (cd != fl_ctx->NIL) {
                outsn(fl_ctx, " . ", f, 3);
                fl_print_child(fl_ctx, f, cd);
            }
            outc(fl_ctx, ')', f);
            break;
        }

        if (!fl_ctx->print_pretty ||
            ((head == fl_ctx->LAMBDA) && n == 0)) {
            // never break line before lambda-list
            ind = 0;
        }
        else {
            est = lengthestimate(fl_ctx, car_(cd));
            nextsmall = smallp(fl_ctx, car_(cd));
            thistiny = tinyp(fl_ctx, car_(v));
            ind = (((fl_ctx->VPOS > lastv) ||
                    (fl_ctx->HPOS>fl_ctx->SCR_WIDTH/2 && !nextsmall && !thistiny && n>0)) ||

                   (fl_ctx->HPOS > fl_ctx->SCR_WIDTH-4) ||

                   (est!=-1 && (fl_ctx->HPOS+est > fl_ctx->SCR_WIDTH-2)) ||

                   ((head == fl_ctx->LAMBDA) && !nextsmall) ||

                   (n > 0 && always) ||

                   (n == 2 && after3) ||
                   (n == 1 && after2) ||

                   (n_unindented >= 3 && !nextsmall) ||

                   (n == 0 && !smallp(fl_ctx, head)));
        }

        if (ind) {
            newindent = outindent(fl_ctx, newindent, f);
            n_unindented = 1;
        }
        else {
            n_unindented++;
            outc(fl_ctx, ' ', f);
            if (n==0) {
                // set indent level after printing head
                si = specialindent(fl_ctx, head);
                if (si != -1)
                    newindent = startpos + si;
                else if (!blk)
                    newindent = fl_ctx->HPOS;
            }
        }
        n++;
        v = cd;
    }
}

static void cvalue_print(fl_context_t *fl_ctx, ios_t *f, value_t v);

static int print_circle_prefix(fl_context_t *fl_ctx, ios_t *f, value_t v)
{
    value_t label;
    char buf[64];
    char *str;
    if ((label=(value_t)ptrhash_get(&fl_ctx->printconses, (void*)v)) !=
        (value_t)HT_NOTFOUND) {
        if (!ismarked(fl_ctx, v)) {
            //fl_ctx->HPOS+=ios_printf(f, "#%ld#", numval(label));
            outc(fl_ctx, '#', f);
            str = uint2str(buf, sizeof(buf)-1, numval(label), 10);
            outs(fl_ctx, str, f);
            outc(fl_ctx, '#', f);
            return 1;
        }
        //fl_ctx->HPOS+=ios_printf(f, "#%ld=", numval(label));
        outc(fl_ctx, '#', f);
        str = uint2str(buf, sizeof(buf)-1, numval(label), 10);
        outs(fl_ctx, str, f);
        outc(fl_ctx, '=', f);
    }
    if (ismanaged(fl_ctx, v))
        unmark_cons(fl_ctx, v);
    return 0;
}

void fl_print_child(fl_context_t *fl_ctx, ios_t *f, value_t v)
{
    char *name, *str;
    char buf[64];
    if (fl_ctx->print_level >= 0 && fl_ctx->P_LEVEL >= fl_ctx->print_level &&
        (iscons(v) || isvector(v) || isclosure(v))) {
        outc(fl_ctx, '#', f);
        return;
    }
    fl_ctx->P_LEVEL++;

    switch (tag(v)) {
    case TAG_NUM :
    case TAG_NUM1: //fl_ctx->HPOS+=ios_printf(f, "%ld", numval(v)); break;
        str = uint2str(&buf[1], sizeof(buf)-1, labs(numval(v)), 10);
        if (numval(v)<0)
            *(--str) = '-';
        outs(fl_ctx, str, f);
        break;
    case TAG_SYM:
        name = symbol_name(fl_ctx, v);
        if (fl_ctx->print_princ)
            outs(fl_ctx, name, f);
        else if (ismanaged(fl_ctx, v)) {
            outsn(fl_ctx, "#:", f, 2);
            outs(fl_ctx, name, f);
        }
        else
            print_symbol_name(fl_ctx, f, name);
        break;
    case TAG_FUNCTION:
        if (v == fl_ctx->T) {
            outsn(fl_ctx, "#t", f, 2);
        }
        else if (v == fl_ctx->F) {
            outsn(fl_ctx, "#f", f, 2);
        }
        else if (v == fl_ctx->NIL) {
            outsn(fl_ctx, "()", f, 2);
        }
        else if (v == fl_ctx->FL_EOF) {
            outsn(fl_ctx, "#<eof>", f, 6);
        }
        else if (isbuiltin(v)) {
            if (!fl_ctx->print_princ)
                outsn(fl_ctx, "#.", f, 2);
            outs(fl_ctx, builtin_names[uintval(v)], f);
        }
        else {
            assert(isclosure(v));
            if (!fl_ctx->print_princ) {
                if (print_circle_prefix(fl_ctx, f, v)) break;
                function_t *fn = (function_t*)ptr(v);
                outs(fl_ctx, "#fn(", f);
                char *data = (char*)cvalue_data(fn->bcode);
                size_t i, sz = cvalue_len(fn->bcode);
                for(i=0; i < sz; i++) data[i] += 48;
                fl_print_child(fl_ctx, f, fn->bcode);
                for(i=0; i < sz; i++) data[i] -= 48;
                outc(fl_ctx, ' ', f);
                fl_print_child(fl_ctx, f, fn->vals);
                if (fn->env != fl_ctx->NIL) {
                    outc(fl_ctx, ' ', f);
                    fl_print_child(fl_ctx, f, fn->env);
                }
                if (fn->name != fl_ctx->LAMBDA) {
                    outc(fl_ctx, ' ', f);
                    fl_print_child(fl_ctx, f, fn->name);
                }
                outc(fl_ctx, ')', f);
            }
            else {
                outs(fl_ctx, "#<function>", f);
            }
        }
        break;
    case TAG_CVALUE:
    case TAG_CPRIM:
        if (v == UNBOUND) { outs(fl_ctx, "#<undefined>", f); break; }
        JL_FALLTHROUGH;
    case TAG_VECTOR:
    case TAG_CONS:
        if (print_circle_prefix(fl_ctx, f, v)) break;
        if (isvector(v)) {
            outc(fl_ctx, '[', f);
            int newindent = fl_ctx->HPOS, est;
            int i, sz = vector_size(v);
            for(i=0; i < sz; i++) {
                if (fl_ctx->print_length >= 0 && i >= fl_ctx->print_length && i < sz-1) {
                    outsn(fl_ctx, "...", f, 3);
                    break;
                }
                fl_print_child(fl_ctx, f, vector_elt(v,i));
                if (i < sz-1) {
                    if (!fl_ctx->print_pretty) {
                        outc(fl_ctx, ' ', f);
                    }
                    else {
                        est = lengthestimate(fl_ctx, vector_elt(v,i+1));
                        if (fl_ctx->HPOS > fl_ctx->SCR_WIDTH-4 ||
                            (est!=-1 && (fl_ctx->HPOS+est > fl_ctx->SCR_WIDTH-2)) ||
                            (fl_ctx->HPOS > fl_ctx->SCR_WIDTH/2 &&
                             !smallp(fl_ctx, vector_elt(v,i+1)) &&
                             !tinyp(fl_ctx, vector_elt(v,i))))
                            newindent = outindent(fl_ctx, newindent, f);
                        else
                            outc(fl_ctx, ' ', f);
                    }
                }
            }
            outc(fl_ctx, ']', f);
            break;
        }
        if (iscvalue(v) || iscprim(v))
            cvalue_print(fl_ctx, f, v);
        else
            print_pair(fl_ctx, f, v);
        break;
    }
    fl_ctx->P_LEVEL--;
}

static void print_string(fl_context_t *fl_ctx, ios_t *f, char *str, size_t sz)
{
    char buf[512];
    size_t i = 0;
    uint8_t c;
    static const char hexdig[] = "0123456789abcdef";

    outc(fl_ctx, '"', f);
    if (!u8_isvalid(str, sz)) {
        // alternate print algorithm that preserves data if it's not UTF-8
        for(i=0; i < sz; i++) {
            c = str[i];
            if (c == '\\')
                outsn(fl_ctx, "\\\\", f, 2);
            else if (c == '"')
                outsn(fl_ctx, "\\\"", f, 2);
            else if (c >= 32 && c < 0x7f)
                outc(fl_ctx, c, f);
            else {
                outsn(fl_ctx, "\\x", f, 2);
                outc(fl_ctx, hexdig[c>>4], f);
                outc(fl_ctx, hexdig[c&0xf], f);
            }
        }
    }
    else {
        while (i < sz) {
            size_t n = u8_escape(buf, sizeof(buf), str, &i, sz, 1, 0);
            outsn(fl_ctx, buf, f, n-1);
        }
    }
    outc(fl_ctx, '"', f);
}

static numerictype_t sym_to_numtype(fl_context_t *fl_ctx, value_t type);
#ifndef _OS_WINDOWS_
#define __USE_GNU
#include <dlfcn.h>
#undef __USE_GNU
#endif

#define sign_bit(r) ((*(int64_t*)&(r)) & BIT63)
#define DFINITE(d) (((*(int64_t*)&(d))&0x7ff0000000000000LL)!=0x7ff0000000000000LL)

// 'weak' means we don't need to accurately reproduce the type, so
// for example #int32(0) can be printed as just 0. this is used
// printing in a context where a type is already implied, e.g. inside
// an array.
static void cvalue_printdata(fl_context_t *fl_ctx, ios_t *f, void *data,
                             size_t len, value_t type, int weak)
{
    if (type == fl_ctx->bytesym) {
        unsigned char ch = *(unsigned char*)data;
        if (fl_ctx->print_princ)
            outc(fl_ctx, ch, f);
        else if (weak)
            fl_ctx->HPOS+=ios_printf(f, "0x%hhx", ch);
        else
            fl_ctx->HPOS+=ios_printf(f, "#byte(0x%hhx)", ch);
    }
    else if (type == fl_ctx->wcharsym) {
        uint32_t wc = *(uint32_t*)data;
        char seq[8];
        size_t nb = u8_toutf8(seq, sizeof(seq), &wc, 1);
        seq[nb] = '\0';
        if (fl_ctx->print_princ) {
            // TODO: better multibyte handling
            outs(fl_ctx, seq, f);
        }
        else {
            outsn(fl_ctx, "#\\", f, 2);
            if      (wc == 0x00) outsn(fl_ctx, "nul", f, 3);
            else if (wc == 0x07) outsn(fl_ctx, "alarm", f, 5);
            else if (wc == 0x08) outsn(fl_ctx, "backspace", f, 9);
            else if (wc == 0x09) outsn(fl_ctx, "tab", f, 3);
            else if (wc == 0x0A) outsn(fl_ctx, "linefeed", f, 8);
            //else if (wc == 0x0A) outsn(fl_ctx, "newline", f, 7);
            else if (wc == 0x0B) outsn(fl_ctx, "vtab", f, 4);
            else if (wc == 0x0C) outsn(fl_ctx, "page", f, 4);
            else if (wc == 0x0D) outsn(fl_ctx, "return", f, 6);
            else if (wc == 0x1B) outsn(fl_ctx, "esc", f, 3);
            else if (wc == 0x20) outsn(fl_ctx, "space", f, 5);
            else if (wc == 0x7F) outsn(fl_ctx, "delete", f, 6);
            else if (iswprint(wc)) outs(fl_ctx, seq, f);
            else fl_ctx->HPOS+=ios_printf(f, "x%04x", (int)wc);
        }
    }
    else if (type == fl_ctx->floatsym || type == fl_ctx->doublesym) {
        char buf[64];
        double d;
        if (type == fl_ctx->floatsym) { d = (double)*(float*)data; }
        else { d = *(double*)data; }
        if (!DFINITE(d)) {
            char *rep;
            if (d != d)
                rep = (char*)(sign_bit(d) ? "-nan.0" : "+nan.0");
            else
                rep = (char*)(sign_bit(d) ? "-inf.0" : "+inf.0");
            if (type == fl_ctx->floatsym && !fl_ctx->print_princ && !weak)
                fl_ctx->HPOS+=ios_printf(f, "#%s(%s)", symbol_name(fl_ctx, type), rep);
            else
                outs(fl_ctx, rep, f);
        }
        else if (d == 0) {
            if (sign_bit(d))
                outsn(fl_ctx, "-0.0", f, 4);
            else
                outsn(fl_ctx, "0.0", f, 3);
            if (type == fl_ctx->floatsym && !fl_ctx->print_princ && !weak)
                outc(fl_ctx, 'f', f);
        }
        else {
            double ad = d < 0 ? -d : d;
            if ((long)d == d && ad < 1e6 && ad >= 1e-4) {
                snprintf(buf, sizeof(buf), "%g", d);
            }
            else {
                if (type == fl_ctx->floatsym)
                    snprintf(buf, sizeof(buf), "%.8g", d);
                else
                    snprintf(buf, sizeof(buf), "%.16g", d);
            }
            int hasdec = (strpbrk(buf, ".eE") != NULL);
            outs(fl_ctx, buf, f);
            if (!hasdec) outsn(fl_ctx, ".0", f, 2);
            if (type == fl_ctx->floatsym && !fl_ctx->print_princ && !weak)
                outc(fl_ctx, 'f', f);
        }
    }
    else if (type == fl_ctx->uint64sym
#ifdef _P64
             || type == fl_ctx->sizesym
#endif
             ) {
        uint64_t ui64 = *(uint64_t*)data;
        if (weak || fl_ctx->print_princ)
            fl_ctx->HPOS += ios_printf(f, "%llu", ui64);
        else
            fl_ctx->HPOS += ios_printf(f, "#%s(%llu)", symbol_name(fl_ctx, type), ui64);
    }
    else if (issymbol(type)) {
        // handle other integer prims. we know it's smaller than uint64
        // at this point, so int64 is big enough to capture everything.
        numerictype_t nt = sym_to_numtype(fl_ctx, type);
        if (nt == N_NUMTYPES) {
            // These states should be context independent.
            static size_t (*volatile jl_static_print)(ios_t*, void*) = NULL;
            static volatile int init = 0;
            // XXX: use uv_once
            if (init == 0) {
#if defined(RTLD_SELF)
                jl_static_print = (size_t (*)(ios_t*, void*))
                    (uintptr_t)dlsym(RTLD_SELF, "jl_static_show");
#elif defined(RTLD_DEFAULT)
                jl_static_print = (size_t (*)(ios_t*, void*))
                    (uintptr_t)dlsym(RTLD_DEFAULT, "jl_static_show");
#elif defined(_OS_WINDOWS_)
                HMODULE handle;
                if (GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
                                       GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                                       (LPCWSTR)(&cvalue_printdata),
                                       &handle)) {
                    jl_static_print = (size_t (*)(ios_t*, void*))
                        (uintptr_t)GetProcAddress(handle, "jl_static_show");
                }
#endif
                init = 1;
            }
            if (jl_static_print != NULL && fl_ctx->jl_sym == type) {
                fl_ctx->HPOS += ios_printf(f, "#<julia: ");
                fl_ctx->HPOS += jl_static_print(f, *(void**)data);
                fl_ctx->HPOS += ios_printf(f, ">");
            }
            else
                fl_ctx->HPOS += ios_printf(f, "#<%s>", symbol_name(fl_ctx, type));
        }
        else {
            int64_t i64 = conv_to_int64(data, nt);
            if (weak || fl_ctx->print_princ)
                fl_ctx->HPOS += ios_printf(f, "%lld", i64);
            else
                fl_ctx->HPOS += ios_printf(f, "#%s(%lld)", symbol_name(fl_ctx, type), i64);
        }
    }
    else if (iscons(type)) {
        if (car_(type) == fl_ctx->arraysym) {
            value_t eltype = car(fl_ctx, cdr_(type));
            size_t cnt, elsize;
            if (iscons(cdr_(cdr_(type)))) {
                cnt = tosize(fl_ctx, car_(cdr_(cdr_(type))), "length");
                elsize = cnt ? len/cnt : 0;
            }
            else {
                // incomplete array type
                int junk;
                elsize = ctype_sizeof(fl_ctx, eltype, &junk);
                cnt = elsize ? len/elsize : 0;
            }
            if (eltype == fl_ctx->bytesym) {
                if (fl_ctx->print_princ) {
                    ios_write(f, (char*)data, len);
                    /*
                    char *nl = memrchr(data, '\n', len);
                    if (nl)
                        fl_ctx->HPOS = u8_strwidth(nl+1);
                    else
                        fl_ctx->HPOS += u8_strwidth(data);
                    */
                }
                else {
                    print_string(fl_ctx, f, (char*)data, len);
                }
                return;
            }
            else if (eltype == fl_ctx->wcharsym) {
                // TODO wchar
            }
            else {
            }
            size_t i;
            if (!weak) {
                if (eltype == fl_ctx->uint8sym) {
                    outsn(fl_ctx, "#vu8(", f, 5);
                }
                else {
                    outsn(fl_ctx, "#array(", f, 7);
                    fl_print_child(fl_ctx, f, eltype);
                    if (cnt > 0)
                        outc(fl_ctx, ' ', f);
                }
            }
            else {
                outc(fl_ctx, '[', f);
            }
            for(i=0; i < cnt; i++) {
                if (i > 0)
                    outc(fl_ctx, ' ', f);
                cvalue_printdata(fl_ctx, f, data, elsize, eltype, 1);
                data = (char *)data + elsize;
            }
            if (!weak)
                outc(fl_ctx, ')', f);
            else
                outc(fl_ctx, ']', f);
        }
    }
}

static void cvalue_print(fl_context_t *fl_ctx, ios_t *f, value_t v)
{
    cvalue_t *cv = (cvalue_t*)ptr(v);
    void *data = cptr(v);
    value_t label;

    if (cv_class(cv) == fl_ctx->builtintype) {
        void *fptr = *(void**)data;
        label = (value_t)ptrhash_get(&fl_ctx->reverse_dlsym_lookup_table, cv);
        if (label == (value_t)HT_NOTFOUND) {
            fl_ctx->HPOS += ios_printf(f, "#<builtin @0x%08zx>", (size_t)fptr);
        }
        else {
            if (fl_ctx->print_princ) {
                outs(fl_ctx, symbol_name(fl_ctx, label), f);
            }
            else {
                outsn(fl_ctx, "#fn(", f, 4);
                outs(fl_ctx, symbol_name(fl_ctx, label), f);
                outc(fl_ctx, ')', f);
            }
        }
    }
    else if (cv_class(cv)->vtable != NULL &&
             cv_class(cv)->vtable->print != NULL) {
        cv_class(cv)->vtable->print(fl_ctx, v, f);
    }
    else {
        value_t type = cv_type(cv);
        size_t len = iscprim(v) ? cv_class(cv)->size : cv_len(cv);
        cvalue_printdata(fl_ctx, f, data, len, type, 0);
    }
}

static void set_print_width(fl_context_t *fl_ctx)
{
    value_t pw = symbol_value(fl_ctx->printwidthsym);
    if (!isfixnum(pw)) return;
    fl_ctx->SCR_WIDTH = numval(pw);
}

void fl_print(fl_context_t *fl_ctx, ios_t *f, value_t v)
{
    fl_ctx->print_pretty = (symbol_value(fl_ctx->printprettysym) != fl_ctx->F);
    if (fl_ctx->print_pretty)
        set_print_width(fl_ctx);
    fl_ctx->print_princ = (symbol_value(fl_ctx->printreadablysym) == fl_ctx->F);

    value_t pl = symbol_value(fl_ctx->printlengthsym);
    if (isfixnum(pl)) fl_ctx->print_length = numval(pl);
    else fl_ctx->print_length = -1;
    pl = symbol_value(fl_ctx->printlevelsym);
    if (isfixnum(pl)) fl_ctx->print_level = numval(pl);
    else fl_ctx->print_level = -1;
    fl_ctx->P_LEVEL = 0;

    fl_ctx->printlabel = 0;
    print_traverse(fl_ctx, v);
    fl_ctx->HPOS = fl_ctx->VPOS = 0;

    fl_print_child(fl_ctx, f, v);

    if (fl_ctx->print_level >= 0 || fl_ctx->print_length >= 0) {
        memset(fl_ctx->consflags, 0, 4*bitvector_nwords(fl_ctx->heapsize/sizeof(cons_t)));
    }

    if ((iscons(v) || isvector(v) || isfunction(v) || iscvalue(v)) &&
        !fl_isstring(fl_ctx, v) && v!=fl_ctx->T && v!=fl_ctx->F && v!=fl_ctx->NIL) {
        htable_reset(&fl_ctx->printconses, 32);
    }
}

void fl_print_init(fl_context_t *fl_ctx)
{
    htable_new(&fl_ctx->printconses, 32);
    fl_ctx->SCR_WIDTH = 80;
    fl_ctx->HPOS = 0;
}
